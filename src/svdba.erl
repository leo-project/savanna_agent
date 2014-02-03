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

-export([notify/2, get_metric_value/2,
         get_histogram_statistics/2]).

-include("svdba.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ===================================================================
%% API
%% ===================================================================
%% @doc Notify an event with a schema and a key
%%
-spec(notify(svdba_schema(), svdba_keyval()) ->
             ok | {error, any()}).
notify(Schema, {Key, Event}) ->
    Name = gen_name(Schema, Key),
    notify(check_type(Name), Name, Event).

%% @private
notify(?METRIC_COUNTER, Name, Event) ->
    folsom_metrics:notify({Name, Event});
notify(?METRIC_HISTOGRAM, Name, Event) ->
    svdba_sample_slide:update(Name, Event);
notify(_,_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a metric value
%%
-spec(get_metric_value(svdba_schema(), atom()) ->
             {ok, any()} | {error, any()}).
get_metric_value(Schema, Key) ->
    Name = gen_name(Schema, Key),
    get_metric_value_1(check_type(Name), Name).

%% @private
get_metric_value_1(?METRIC_COUNTER, Name) ->
    svdba_metrics_counter:get_values(Name);
get_metric_value_1(?METRIC_HISTOGRAM, Name) ->
    svdba_sample_slide:get_values(Name);
get_metric_value_1(_,_) ->
    {error, invalid_args}.


%% @doc Retrieve a historgram statistics
%%
get_histogram_statistics(Schema, Key) ->
    Name = gen_name(Schema, Key),
    case check_type(Name) of
        ?METRIC_HISTOGRAM ->
            svdba_sample_slide:get_histogram_statistics(Name);
        _ ->
            not_found
    end.


%% ===================================================================
%% Inner Functions
%% ===================================================================
%% @private
gen_name(Schema, Key) ->
    list_to_atom(lists:append([atom_to_list(Schema), "/", atom_to_list(Key)])).

%% @private
check_type(Name) ->
    check_type([?METRIC_COUNTER, ?METRIC_HISTOGRAM], Name).

%% @private
check_type([],_Name) ->
    not_found;
check_type([?METRIC_COUNTER = Type|Rest], Name) ->
   case ets:lookup(?SPIRAL_TABLE, Name) of
       [{Name,{spiral,_,_}}|_] ->
           Type;
       _ ->
           check_type(Rest, Name)
   end;
check_type([?METRIC_HISTOGRAM = Type|Rest], Name) ->
   case ets:lookup(?HISTOGRAM_TABLE, Name) of
       [{Name,{histogram,slide,{slide,_,_,_}}}|_] ->
           Type;
       _ ->
           check_type(Rest, Name)
   end.
