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
-module(savanna_agent_test).
-author('Yosuke Hara').

-include("savanna_agent.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
    {setup,
     fun () ->
             ok
     end,
     fun (_) ->
             ok
     end,
     [{"test all functions",
       {timeout, 300, fun suite/0}}
     ]}.


suite() ->
    %% Preparation
    ok = savanna_agent:start(ram_copies),
    SchemaName = << "bucket" >>,
    ok = svc_tbl_schema:update(#?SV_SCHEMA{name = SchemaName,
                                           created_at = leo_date:now()}),
    ok = svc_tbl_column:update(#?SV_COLUMN{id = {SchemaName, << "col_1" >>},
                                           schema_name = SchemaName,
                                           name = << "col_1" >>,
                                           type = ?COL_TYPE_COUNTER,
                                           constraint = [],
                                           created_at = leo_date:now()}),
    ok = svc_tbl_column:update(#?SV_COLUMN{id = {SchemaName, << "col_2" >>},
                                           schema_name = SchemaName,
                                           name = << "col_2" >>,
                                           type = ?COL_TYPE_H_UNIFORM,
                                           constraint = [{?HISTOGRAM_CONS_SAMPLE, 3000}],
                                           created_at = leo_date:now()}),
    %% Create metrics
    ok = savanna_agent:create_metrics(SchemaName, 'bucket_test', 30),
    ok = put_events(3000),
    ok.

put_events(0) ->
    ?debugVal("DONE"),
    ok;
put_events(Index) ->
    MinDelay = 10,
    Delay_1 = erlang:phash2(leo_date:now(),250),
    Delay_2 = case Delay_1 < MinDelay of
                  true  -> MinDelay;
                  false -> Delay_1
              end,

    ok = timer:sleep(Delay_2),
    ok = savanna_agent:notify(<< "bucket_test" >>, << "col_1" >>, 1),
    ok = savanna_agent:notify(<< "bucket_test" >>, << "col_2" >>, erlang:memory(total)),
    put_events(Index - 1).
