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

-export([create_tables/2]).

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API
%% ===================================================================
%% @doc Create stat's tables
%%
-spec(create_tables(disc_copies|ram_copies, list(atom())) ->
             ok).
create_tables(MnesiaDiscType, Nodes) ->
    _ = mnesia:start(),
    {atomic,ok} = svc_tbl_schema:create_table(MnesiaDiscType, Nodes),
    {atomic,ok} = svc_tbl_column:create_table(MnesiaDiscType, Nodes),
    {atomic,ok} = svc_tbl_metric_group:create_table(MnesiaDiscType, Nodes),
    ok.


%% ===================================================================
%% Inner Functions
%% ===================================================================
