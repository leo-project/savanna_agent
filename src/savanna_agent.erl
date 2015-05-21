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
-module(savanna_agent).
-author('Yosuke Hara').

-export([start/1,
         create_metrics/3,
         create_metrics/4,
         notify/3,
         set_managers/1,
         sync_schemas/0, sync_schemas/1
        ]).

-include("savanna_agent.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API
%% ===================================================================
%% @doc Create the tables for the metrics, then launch the agent
%%
-spec(start(ram_copies|disc_copies) ->
             ok | {error, any()}).
start(MnesiaDiscType) ->
    _ = mnesia:start(),
    Nodes = [erlang:node()],
    ok = svc_tbl_schema:create_table(MnesiaDiscType, Nodes),
    ok = svc_tbl_column:create_table(MnesiaDiscType, Nodes),
    ok = svc_tbl_metric_group:create_table(MnesiaDiscType, Nodes),
    application:start(savanna_agent).


%% @doc Create a new metrics or histgram by the schema
%%
-spec(create_metrics(binary(), binary(), pos_integer()) ->
             ok | {error, any()}).
create_metrics(Schema, MetricGroup, Window) ->
    Notifier = 'savanna_agent_notifier',
    create_metrics(Schema, MetricGroup, Window, Notifier).

-spec(create_metrics(binary(), binary(), pos_integer(), atom()) ->
             ok | {error, any()}).
create_metrics(Schema, MetricGroup, Window, Notifier) ->
    Step = case Window of
               ?SV_WINDOW_10S -> ?SV_STEP_1M;
               ?SV_WINDOW_30S -> ?SV_STEP_1M;
               ?SV_WINDOW_1M  -> ?SV_STEP_1M;
               ?SV_WINDOW_5M  -> ?SV_STEP_5M;
               _ -> ?SV_STEP_5M
           end,
    create_metrics(Schema, MetricGroup, Window, Step, Notifier).

-spec(create_metrics(binary(), binary(), pos_integer(), pos_integer(), atom()) ->
             ok | {error, any()}).
create_metrics(Schema, MetricGroup, Window, Step, Notifier) ->
    savanna_commons:create_metrics_by_schema(Schema, MetricGroup, Window, Step, Notifier).


%% @doc Notify an event with a schema and a key
%%
-spec(notify(binary(), binary(), any()) ->
             ok | {error, any()}).
notify(MetricGroup, Key, Event) ->
    savanna_commons:notify(MetricGroup, {Key, Event}).


%% @doc Set savanna-manager's nodes
%%
-spec(set_managers(list(atom())) ->
             ok | {error, any()}).
set_managers(ManagerNodes) ->
    application:set_env('savanna_agent', 'managers', ManagerNodes).


%% @doc Synchronize the schemas
%%
-spec(sync_schemas() ->
             ok | {error, any()}).
sync_schemas() ->
    sync_schemas(?env_svdb_manager_nodes()).

-spec(sync_schemas(list(atom())) ->
             ok | {error, any()}).
sync_schemas(Managers) ->
    savanna_commons:sync_schemas(Managers).


%% ===================================================================
%% Inner Functions
%% ===================================================================
