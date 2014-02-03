%%======================================================================
%%
%% LeoProject - SavannaDB Agent
%%
%% Copyright (c) 2013-2014 Rakuten, Inc.
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
-module(svdba_metrics_counter).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("svdba.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1, start_link/2,
         stop/1]).

-export([get_values/1,
         update/2,
         trim/3
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {name :: atom(),
                window = 0 :: pos_integer(),
                before = 0 :: pos_integer()
               }).

-define(DEF_WINDOW, 60).
-define(DEF_WIDTH,  16).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, ?DEF_WINDOW], []).

start_link(Name, Window) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Window], []).

stop(Name) ->
    gen_server:call(Name, stop).


%% @doc
-spec(get_values(atom()) ->
             {ok, tuple()} | {error, any()}).
get_values(Name) ->
    gen_server:call(Name, get_values).


%% @doc
-spec(update(atom(), any()) ->
             ok | {error, any()}).
update(Name, Value) ->
    gen_server:call(Name, {update, Value}).


%% @doc
-spec(trim(atom(), atom(), pos_integer()) ->
             ok | {error, any()}).
trim(Name, Tid, Window) ->
    gen_server:call(Name, {trim, Tid, Window}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Name, Window]) ->
    Spiral = #spiral{},
    Pid = svdba_sup:start_slide_server(?MODULE, Name, Spiral#spiral.tid, Window),
    ok = folsom_ets:add_handler(spiral, Name),

    case ets:insert_new(Spiral#spiral.tid,
                        [{{count, N}, 0} || N <- lists:seq(0, ?DEF_WIDTH - 1)]) of
        true ->
            case ets:insert(?SPIRAL_TABLE, {Name, Spiral#spiral{server = Pid}}) of
                true ->
                    {ok, #state{name = Name,
                                window = Window}};
                _ ->
                    {stop, ?ERROR_ETS_NOT_AVAILABLE}
            end;
        _ ->
            {stop, ?ERROR_ETS_NOT_AVAILABLE}
    end.

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};


handle_call(get_values, _From, #state{name = Name,
                                      window = Window} = State) ->
    Reply = get_values_1(Name, Window),
    {reply, Reply, State};

handle_call({update, Value}, _From, #state{name = Name} = State) ->
    #spiral{tid=Tid} = get_value(Name),
    Moment = folsom_utils:now_epoch(),
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?DEF_WIDTH - 1),
    folsom_utils:update_counter(Tid, {Moment, Rnd}, Value),
    Reply = ets:update_counter(Tid, {count, Rnd}, Value),
    {reply, Reply, State};

handle_call({trim, Tid, Window}, _From, #state{name = Name} = State) ->
    Oldest = folsom_utils:now_epoch() - Window,
    _ = ets:select_delete(Tid, [{{{'$1','_'},'_'},
                                 [{is_integer, '$1'},
                                  {'<', '$1', Oldest}],
                                 ['true']}]),

    %% @TODO - retrieve the current value
    Current = get_values_1(Name, Window),
    ?debugVal(Current),

    {reply, ok, State#state{before = Oldest}}.


handle_cast(_Msg, State) ->
    {noreply, State}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% handle_info({_Label, {_From, MRef}, get_modules}, State) ->
%%     {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


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
%% @private
get_value(Name) ->
    [{Name, Spiral}] =  ets:lookup(?SPIRAL_TABLE, Name),
    Spiral.


%% @private
get_values_1(Name, Window) ->
    Oldest = folsom_utils:now_epoch() - Window,
    #spiral{tid=Tid} = get_value(Name),

    Count = lists:sum(ets:select(Tid, [{{{count,'_'},'$1'},[],['$1']}])),
    One   = lists:sum(ets:select(Tid, [{{{'$1','_'},'$2'},
                                        [{is_integer, '$1'},
                                         {'>=', '$1', Oldest}],
                                        ['$2']}])),
    {ok, [{count, Count}, {one, One}]}.
