%%======================================================================
%%
%% LeoProject - Savanna Agent
%%
%% Copyright (c) 2014 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
-module(savanna_agent_worker).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").


%% API
-export([start_link/2,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {sync_interval = ?DEF_SYNC_INTERVAL :: integer(),
                managers = [] :: sva_managers()
               }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
-spec(start_link(integer(), sva_managers()) ->
             {ok, pid()} | ignore | {error, any()}).
start_link(SyncInterval, ManagerNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [SyncInterval, ManagerNodes], []).


stop() ->
    gen_server:call(?MODULE, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([SyncInterval, ManagerNodes]) ->
    {ok, #state{sync_interval = SyncInterval,
                managers      = ManagerNodes}, SyncInterval}.

handle_call({status}, _From, #state{sync_interval = SyncInterval} = State) ->
    {reply, {ok, []}, State, SyncInterval};

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

handle_cast(_Msg, #state{sync_interval = SyncInterval} = State) ->
    {noreply, State, SyncInterval}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(timeout, #state{sync_interval = SyncInterval,
                            managers = ManagerNodes} = State) ->
    case ManagerNodes of
        [] ->
            void;
        _ ->
            %% Check and Sync schema-table(s)
            ChecksumSchema_1 = svc_tbl_schema:checksum(),
            ChecksumSchema_2 = get_tbl_schema_checksum(ManagerNodes),
            case (ChecksumSchema_1 /= ChecksumSchema_2 andalso
                  ChecksumSchema_2 > 0) of
                true ->
                    savanna_agent:sync_schemas(ManagerNodes);
                false ->
                    ok
            end

            %% @TODO
            %% Check and Sync savannadb's member(s)
    end,
    {noreply, State, SyncInterval};
handle_info(_Info, #state{sync_interval = SyncInterval} = State) ->
    {noreply, State, SyncInterval}.


%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason,_State) ->
    ok.


%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Retrieve checksum of schema-table from savanna-manager(s)
%% @private
get_tbl_schema_checksum([]) ->
    -1;
get_tbl_schema_checksum([Node|Rest]) ->
    case leo_rpc:call(Node, svc_tbl_schema, checksum, []) of
        {badrpc,_Cause} ->
            get_tbl_schema_checksum(Rest);
        timeout ->
            get_tbl_schema_checksum(Rest);
        Checksum ->
            Checksum
    end.
