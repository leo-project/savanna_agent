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
-author('Yosuke Hara').

%% Default values
-define(DEF_SYNC_INTERVAL, 5000).

-type(sva_managers() :: list(atom())).

-define(TBL_SVDB_MEMBERS, 'svdb_members').
-define(DEF_MAX_FAIL_COUNT, 3).
-define(ERROR_COULD_NOT_TRANSFER_MSG, "[ERROR]Could not transfer a message").


%% Record
%%
-record(member, {
          node           :: atom(),        %% actual node-name
          ip = "0.0.0.0" :: string(),      %% ip-address
          port  = 13075  :: pos_integer(), %% port-number
          state = null   :: atom()         %% current-status
         }).

%% Macro
%%
%% @doc Retrieve interval of synchronization
-define(env_table_sync_interval(),
        case application:get_env('savanna_agent', 'table_sync_interval') of
            undefined ->
                ?DEF_SYNC_INTERVAL;
            {ok, _EnvTblSyncInterval} ->
                _EnvTblSyncInterval
        end).

%% @doc Retrieve savannadb's manager nodes
-define(env_svdb_manager_nodes(),
        case application:get_env('savanna_agent', 'managers') of
            undefined ->
                [];
            {ok, _EnvManagerNodes} ->
                _EnvManagerNodes
        end).

