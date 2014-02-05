%%======================================================================
%%
%% LeoProject - SavannaDB
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
-author('Yosuke Hara').

%% Default values
-define(DEF_SYNC_INTERVAL, 5000).

-type(svdba_managers() :: list(atom())).

%% Macro
%%
-define(env_table_replica_of_type(),
        case application:get_env('savannadb_agent', 'table_replica_of_type') of
            undefined ->
                ram_copies;
            {ok, _EnvTblReplicaOfType} ->
                case is_list(_EnvTblReplicaOfType) of
                    true  -> list_to_atom(_EnvTblReplicaOfType);
                    false -> _EnvTblReplicaOfType
                end
        end).

-define(env_table_sync_interval(),
        case application:get_env('savannadb_agent', 'table_sync_interval') of
            undefined ->
                ?DEF_SYNC_INTERVAL;
            {ok, _EnvTblSyncInterval} ->
                _EnvTblSyncInterval
        end).

-define(env_svdb_manager_nodes(),
        case application:get_env('savannadb_agent', 'managers') of
            undefined ->
                [];
            {ok, _EnvManagerNodes} ->
                _EnvManagerNodes
        end).
        
