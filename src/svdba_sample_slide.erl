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
-module(svdba_sample_slide).

-export([start_link/1, start_link/2, start_link/3,
         stop/1]).

-export([update/2,
         get_values/1,
         get_histogram_statistics/1,
         resize/2,
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
                server     :: pid(),
                reservoir  :: pos_integer(),
                before = 0 :: pos_integer()
               }).

-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEF_WIDTH,  16).
-define(DEF_WINDOW, 60).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Name) ->
    start_link(Name, ?DEF_WINDOW, ?DEFAULT_SIZE).

start_link(Name, Window) ->
    start_link(Name, Window, ?DEFAULT_SIZE).

start_link(Name, Window, SampleSize) ->
    gen_server:start_link({local, Name}, ?MODULE,
                          [Name, Window, SampleSize, ?DEFAULT_ALPHA], []).

stop(Name) ->
    gen_server:call(Name, stop).


get_values(Name) ->
    gen_server:call(Name, get_values).


get_histogram_statistics(Name) ->
    gen_server:call(Name, get_histogram_statistics).


update(Name, Value) ->
    gen_server:call(Name, {update, Value}).


resize(Name, NewSize) ->
    gen_server:call(Name, {resize, NewSize}).


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
init([Name, Window, SampleSize, Alpha]) ->
    Sample = #slide{window = Window},
    Reservoir = Sample#slide.reservoir,

    Pid = svdba_sup:start_slide_server(?MODULE, Name, Reservoir, Window),
    ok = folsom_ets:add_handler(histogram, Name, 'slide', SampleSize, Alpha),

    {ok, #state{name = Name,
                window = Window,
                server = Pid,
                reservoir = Reservoir
               }}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_values, _From, #state{window = Window,
                                      reservoir = Tid} = State) ->
    Reply = get_values_1(Tid, Window),
    {reply, Reply, State};

handle_call(get_histogram_statistics, _From, #state{window = Window,
                                                    reservoir = Tid} = State) ->
    {ok, Values} = get_values_1(Tid, Window),
    Current = bear:get_statistics(Values),
    {reply, {ok, Current}, State};

handle_call({update, Value}, _From, #state{reservoir = Reservoir} = State) ->
    Moment = folsom_utils:now_epoch(),
    Rnd = erlang:system_info(scheduler_id) band (?DEF_WIDTH - 1),

    true = ets:insert(Reservoir, {{Moment, Rnd}, Value}),
    {reply, ok, State};

handle_call({resize, NewSize}, _From, #state{server = Pid} = State) ->
    ok = svdba_sample_slide_server:resize(Pid, NewSize),
    {reply, ok, State#state{window = NewSize}};

handle_call({trim, Tid, Window}, _From, State) ->
    Oldest = folsom_utils:now_epoch() - Window,
    _ = ets:select_delete(Tid, [{{{'$1','_'},'_'},
                                 [{'<', '$1', Oldest}],
                                 ['true']}]),

    %% @TODO - retrieve the current value
    {ok, Values} = get_values_1(Tid, Window),
    Current = bear:get_statistics(Values),
    ?debugVal(Current),

    {reply, ok, State#state{before = Oldest}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
get_values_1(Tid, Window) ->
    Oldest = folsom_utils:now_epoch() - Window,
    Ret = ets:select(Tid, [{{{'$1','_'},'$2'},[{'>=', '$1', Oldest}],['$2']}]),
    {ok, Ret}.
