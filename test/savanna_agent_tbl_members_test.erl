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
-module(savanna_agent_tbl_members_test).
-author('Yosuke Hara').

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
    {setup,
     fun () ->
             mnesia:start()
     end,
     fun (_) ->
             mnesia:stop()
     end,
     [{"test all functions",
       {timeout, 30, fun suite/0}}
     ]}.


suite() ->
    ok = savanna_agent_tbl_members:create_table(ram_copies, [node()]),
    not_found = savanna_agent_tbl_members:all(),
    not_found = savanna_agent_tbl_members:get(node()),

    Member_1 = #member{node  = node(),
                       ip    = "0.0.0.0",
                       port  = "13075",
                       state = 'running'
                      },
    Member_2 = #member{node  = 'node_1@0.0.0.0',
                       ip    = "0.0.0.0",
                       port  = "13075",
                       state = 'suspend'
                      },
    Member_3 = #member{node  = 'node_2@0.0.0.0',
                       ip    = "0.0.0.0",
                       port  = "13075",
                       state = 'running'
                      },
    ok = savanna_agent_tbl_members:update(Member_1),
    ok = savanna_agent_tbl_members:update(Member_2),
    ok = savanna_agent_tbl_members:update(Member_3),

    {ok, Ret_1} = savanna_agent_tbl_members:all(),
    {ok, Ret_2} = savanna_agent_tbl_members:get(node()),
    ?assertEqual(3, length(Ret_1)),
    ?assertEqual(Ret_2, Member_1),

    {ok, Ret_3} = savanna_agent_tbl_members:find_by_state('running'),
    ?assertEqual(2, length(Ret_3)),

    ok = savanna_agent_tbl_members:delete(node()),
    not_found = savanna_agent_tbl_members:get(node()),
    {ok, Ret_4} = savanna_agent_tbl_members:all(),
    ?assertEqual(2, length(Ret_4)),
    ok.
