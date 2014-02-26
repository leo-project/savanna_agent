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
-module(savanna_agent_notifier).
-author('Yosuke Hara').

-behaviour(svc_notify_behaviour).

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").

%% callback
-export([notify/2]).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc
-spec(notify(atom(), {atom(),any()}) ->
             ok | {error, any()}).
notify(MetricGroup, {Key, Value}) ->
    notify(MetricGroup, {Key, Value}, 1).

notify(MetricGroup, {Key, Value}, ?DEF_MAX_FAIL_COUNT) ->
    %% @TODO enqueue a fail message
    ?debugVal({MetricGroup, {Key, Value}}),
    ok;
notify(MetricGroup, {Key, Value},_Times) ->
    %% Retrieve the destination node(s)
    Node = case savanna_agent_tbl_members:find_by_state('running') of
               {ok, Members} ->
                   ?debugVal(Members),
                   Len = length(Members),
                   lists:nth(erlang:phash2(leo_date:now(), Len) + 1, Members);
               _ ->
                   []
           end,

    %% Transfer calculated statistics/metrics
    ?debugVal({MetricGroup, {Key, Value}}),
    case Node of
        [] ->
            notify(MetricGroup, {Key, Value}, ?DEF_MAX_FAIL_COUNT);
        _ ->
            %% @TODO transfer a message
            %% case leo_rpc:call(Node, savannadb, notify, [MetricGroup, {Key, Value}]) of
            %%     ok ->
            %%         ok;
            %%     _ ->
            %%         notify(MetricGroup, {Key, Value}, Times+1)
            %% end
            ok
    end.
