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
-module(savanna_agent_tbl_members).
-author('Yosuke Hara').

-include("savanna_agent.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([create_table/2,
         all/0, get/1, find_by_state/1,
         update/1, delete/1,
         checksum/0, size/0
        ]).

-define(TBL_NAME, ?TBL_SVDB_MEMBERS).
-define(ERROR_MNESIA_NOT_START, "Mnesia is not available").


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Create a table of system-configutation
%%
-spec(create_table(disc_copies|ram_copies, [atom()]) ->
             ok | {error, any()}).
create_table(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_NAME,
           [{Mode, Nodes},
            {type, set},
            {record_name, member},
            {attributes, record_info(fields, member)},
            {user_properties,
             [
              {node,       atom,        primary},
              {ip,         string,      false  },
              {port,       pos_integer, false  },
              {state,      atom,        false  }
             ]}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted,{already_exists,_}} ->
            ok;
        {aborted,Error} ->
            {error, Error}
    end.


%% @doc Retrieve all records
%%
-spec(all() ->
             {ok, [#member{}]} | not_found | {error, any()}).
all() ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a member by name
%%
-spec(get(atom()) ->
             {ok, #member{}} | not_found | {error, any()}).
get(Node) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                        X#member.node == Node]),
                        qlc:e(Q)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve members by status
%%
-spec(find_by_state(atom()) ->
             {ok, [#member{}]} | not_found | {error, any()}).
find_by_state(State) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_NAME),
                                         X#member.state == State]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, Members} ->
                    {ok, Members};
                Other ->
                    Other
            end
    end.


%% @doc Modify a member
%%
-spec(update(#member{}) ->
             ok | {error, any()}).
update(Member) ->
    case catch mnesia:table_info(?TBL_NAME, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun()-> mnesia:write(?TBL_NAME, Member, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Remove a member
%%
-spec(delete(atom()) ->
             ok | {error, any()}).
delete(Node) ->
    case ?MODULE:get(Node) of
        {ok, Member} ->
            Fun = fun() ->
                          mnesia:delete_object(?TBL_NAME, Member, write)
                  end,
            leo_mnesia:delete(Fun);
        Error ->
            Error
    end.


%% @doc Retrieve the checksum of this table
%%
-spec(checksum() ->
             integer()).
checksum() ->
    case ?MODULE:all() of
        {ok, Records} ->
            erlang:phash2(Records);
        _ ->
            -1
    end.


%% @doc Retrieve the records
%%
-spec(size() ->
             pos_integer()).
size() ->
    mnesia:table_info(?TBL_NAME, size).

