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

-export([start/2,
         create_metrics/4,
         notify/3]).

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API
%% ===================================================================
%% @doc Create the tables for the metrics, then launch the agent
%%
-spec(start(ram_copies|disc_copies, list(atom())) ->
             ok | {error, any()}).
start(MnesiaDiscType, Nodes) ->
    _ = mnesia:start(),
    {atomic,ok} = svc_tbl_schema:create_table(MnesiaDiscType, Nodes),
    {atomic,ok} = svc_tbl_column:create_table(MnesiaDiscType, Nodes),
    {atomic,ok} = svc_tbl_metric_group:create_table(MnesiaDiscType, Nodes),
    application:start(savanna_agent).


%% @doc Create a new metrics or histgram by the schema
%%
-spec(create_metrics(atom(), atom(), pos_integer(), atom()) ->
             ok | {error, any()}).
create_metrics(Schema, MetricGroup, Window, Notifier) ->
    savanna_commons:create_metrics_by_schema(Schema, MetricGroup, Window, Notifier).


%% @doc Notify an event with a schema and a key
%%
-spec(notify(atom(), atom(), any()) ->
             ok | {error, any()}).
notify(MetricGroup, Key, Event) ->
    savanna_commons:notify(MetricGroup, {Key, Event}).


%% ===================================================================
%% Inner Functions
%% ===================================================================
