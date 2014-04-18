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
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% callback
-export([notify/1]).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc
-spec(notify(#sv_result{}) ->
             ok | {error, any()}).
notify(#sv_result{metric_group_name = MetricGroup,
                  adjusted_step = DateTime,
                  col_name = Key,
                  result = Value}) ->
    notify(MetricGroup, DateTime, {Key, Value}, 1).

%% @private
notify(_MetricGroup, _DateTime, _KeyAndVal, ?DEF_MAX_FAIL_COUNT) ->
    %% @TODO enqueue a fail message
    ok;
notify(MetricGroup, DateTime, {Key, Value}, Times) ->
    %% Retrieve the destination node(s)
    Node = case savanna_agent_tbl_members:find_by_state('running') of
               {ok, Members} ->
                   Len = length(Members),
                   lists:nth(erlang:phash2(leo_date:now(), Len) + 1, Members);
               _ ->
                   []
           end,

    %% Transfer calculated statistics/metrics
    case Node of
        [] ->
            notify(MetricGroup, DateTime, {Key, Value}, ?DEF_MAX_FAIL_COUNT);
        _ ->
            case notify_1(Node, MetricGroup, DateTime, Key, Value) of
                ok ->
                    ok;
                _ ->
                    notify(MetricGroup, DateTime, {Key, Value}, Times + 1)
            end
    end.

%% @private
notify_1(Node, MetricGroup, DateTime, Key, Value) ->
    case svc_tbl_metric_group:get(MetricGroup) of
        {ok, #sv_metric_group{schema_name = Schema}} ->
            case leo_rpc:call(Node, savannadb_api, notify,
                              [Schema, MetricGroup, DateTime, {Key, Value}]) of
                ok ->
                    ok;
                _ ->
                    {error, ?ERROR_COULD_NOT_GET_SCHEMA}
            end;
        _ ->
            {error, ?ERROR_COULD_NOT_TRANSFER_MSG}
    end.
