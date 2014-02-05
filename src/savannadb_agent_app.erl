%%======================================================================
%%
%% LeoProject - SavannaDB Agent
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
-module(savannadb_agent_app).

-behaviour(application).

-include("savannadb_agent.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    catch mnesia:start(),
    Ret = savannadb_agent_sup:start_link(),
    after_proc(Ret).

stop(_State) ->
    ok.


%% @private
after_proc({ok,_Pid} = Ret) ->
    %% Create schema-tables
    Type = ?env_table_replica_of_type(),
    {atomic,ok} = svdbc_tbl_schema:create_table(Type, [node()]),
    {atomic,ok} = svdbc_tbl_column:create_table(Type, [node()]),
    Ret.
