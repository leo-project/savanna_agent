%%======================================================================
%%
%% LeoProject - SavannaDB Agent
%%
%% Copyright (c) 2013-2014 Rakuten, Inc.
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
-module(svdba_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_slide_server/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_slide_server(SampleMod, ServerId, Reservoir, Window) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [SampleMod, ServerId, Reservoir, Window]),
    Pid.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok,{{simple_one_for_one, 3, 180},
         [
          {undefined, {svdba_sample_slide_server, start_link, []},
           transient, brutal_kill, worker, [svdba_sample_slide_server]}
         ]}}.
