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
-module(svdba).

-export([notify/2]).

-include("svdba.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% API
%% ===================================================================
%% @doc
%%
-spec(notify(svdba_schema(), svdba_keyval()) ->
             ok).
notify(Schema, {Key, Event}) ->
    Name = list_to_atom(lists:append([atom_to_list(Schema), "_", atom_to_list(Key)])),
    notify(check_type(Name), Name, Event).

%% @private
notify(counter, Name, Event) ->
    folsom_metrics:notify({Name, Event});
notify(histogram, Name, Event) ->
    svdba_sample_slide:update(Name, Event);
notify(_,_,_) ->
    {error, invalid_args}.


%% @private
check_type(Name) ->
    check_type([counter, histogram], Name).

%% @private
check_type([],_Name) ->
    not_found;
check_type([counter = Type|Rest], Name) ->
   case ets:lookup(?SPIRAL_TABLE, Name) of
       [] ->
           check_type(Rest, Name);
       [_H|_] ->
           Type
   end;
check_type([histogram = Type|Rest], Name) ->
   case ets:lookup(?HISTOGRAM_TABLE, Name) of
       [] ->
           check_type(Rest, Name);
       [_H|_] ->
           Type
   end.
