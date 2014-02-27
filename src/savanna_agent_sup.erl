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
-module(savanna_agent_sup).

-behaviour(supervisor).

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Children = [
                {savanna_commons_sup,
                 {savanna_commons_sup, start_link, []},
                 permanent,
                 2000,
                 supervisor,
                 [savanna_commons_sup]},

                {savanna_agent_worker,
                 {savanna_agent_worker, start_link,
                  [?env_table_sync_interval(),
                   ?env_svdb_manager_nodes()
                  ]},
                 permanent,
                 2000,
                 worker,
                 [savanna_agent_worker]}
               ],
    {ok, { {one_for_one, 5, 60}, Children}}.
