%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%-----------------------------------------------------------------
%% Purpose: Functions for handling statistic counters
%%-----------------------------------------------------------------
-module(megaco_stats).


%%-----------------------------------------------------------------
%% Application internal exports
%%-----------------------------------------------------------------

-export([init/1, init/2]).

-export([get_stats/1, get_stats/2, get_stats/3,
	 reset_stats/1, reset_stats/2]).


%%-----------------------------------------------------------------
%% Func: init/1, init/2
%% Description: Initiate the statistics. Creates the stats table 
%%              and the global counters.
%%-----------------------------------------------------------------
init(Name) ->
    init(Name, []).

init(Name, GlobalCounters) ->
    ets:new(Name, [public, named_table, {keypos, 1}]),
    ets:insert(Name, {global_counters, GlobalCounters}),
    create_global_snmp_counters(Name, GlobalCounters).


% create_global_snmp_counters(_Name, []) ->
%     ok;
% create_global_snmp_counters(Name, [Counter|Counters]) ->
%     ets:insert(Name, {Counter, 0}),
%     create_global_snmp_counters(Name, Counters).

create_global_snmp_counters(Name, Counters) ->
    F = fun(Counter) -> ets:insert(Name, {Counter, 0}) end,
    lists:foreach(F, Counters).


%%-----------------------------------------------------------------
%% Func: get_stats/1, get_stats/2, get_stats/3 
%% Description: Get statistics
%%-----------------------------------------------------------------
get_stats(Ets) ->
    Handles = get_handles_and_global_counters(Ets),
    (catch do_get_stats(Ets, Handles, [])).

do_get_stats(_Ets, [], Acc) ->
    {ok, lists:reverse(Acc)};
do_get_stats(Ets, [Handle|Handles], Acc) ->
    case get_stats(Ets, Handle) of
	{ok, Stats} ->
	    do_get_stats(Ets, Handles, [{Handle, Stats}|Acc]);
	{error, Reason} ->
	    throw({error, Reason})
    end.

get_stats(Ets, GlobalCounter) when atom(GlobalCounter) ->
    case (catch ets:lookup(Ets, GlobalCounter)) of
	[{GlobalCounter, Val}] ->
	    {ok, Val};
	[] ->
	    {error, {no_such_counter, GlobalCounter}}
    end;

get_stats(Ets, Handle) ->
    case (catch ets:match(Ets, {{Handle, '$1'},'$2'})) of
	CounterVals when list(CounterVals) ->
	    {ok, [{Counter, Val} || [Counter, Val] <- CounterVals]};
	Other ->
	    {error, {unexpected_result, Other}}
    end.


get_stats(Ets, Handle, Counter) when atom(Counter) ->
    Key = {Handle, Counter}, 
    case (catch ets:lookup(Ets, Key)) of
	[{Key, Val}] ->
	    {ok, Val};
	_ ->
	    {error, {undefined_counter, Counter}}
    end.


%%-----------------------------------------------------------------
%% Funcs: reset_stats/1, reset_stats/2
%% Description: Reset statistics 
%%-----------------------------------------------------------------
reset_stats(Ets) ->
    Handles = get_handles_and_global_counters(Ets),
    (catch do_reset_stats(Ets, Handles, [])).

do_reset_stats(_Ets, [], Acc) ->
    {ok, lists:reverse(Acc)};
do_reset_stats(Ets, [Handle|Handles], Acc) ->
    case reset_stats(Ets, Handle) of
	{ok, OldStats} ->
	    do_reset_stats(Ets, Handles, [{Handle, OldStats}|Acc]);
	{error, Reason} ->
	    throw({error, Reason})
    end.

reset_stats(Ets, GlobalCounter) when atom(GlobalCounter) ->
    %% First get the current value of the counter
    case (catch ets:lookup(Ets, GlobalCounter)) of
	[{GlobalCounter, Val}] ->
	    ets:insert(Ets, {GlobalCounter, 0}),
	    {ok, Val};
	[] -> %% Oooups
	    {error, {no_such_counter, GlobalCounter}}
    end;

reset_stats(Ets, Handle) ->
    case (catch ets:match(Ets, {{Handle, '$1'},'$2'})) of
	CounterVals when list(CounterVals) ->
	    CVs = [{Counter, Val} || [Counter, Val] <- CounterVals],
	    reset_stats(Ets, Handle, CVs),
	    {ok, CVs};
	Other ->
	    {error, {unexpected_result, Other}}
    end.

reset_stats(_Ets, _Handle, []) ->
    ok;
reset_stats(Ets, Handle, [{Counter, _}|CVs]) ->
    ets:insert(Ets, {{Handle, Counter}, 0}),
    reset_stats(Ets, Handle, CVs).



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
get_handles_and_global_counters(Ets) ->
    GlobalCounters = 
	case ets:lookup(Ets, global_counters) of
	    [{global_counters, GC}] ->
		GC;
	    [] ->
		[]
	end,
    L1 = ets:match(Ets, {{'$1', '_'}, '_'}),
    GlobalCounters ++ 
	lists:sort([Handle || [Handle] <- remove_duplicates(L1, [])]).
    
remove_duplicates([], L) ->
    L;
remove_duplicates([H|T], L) ->
    case lists:member(H,T) of
        true ->
            remove_duplicates(T, L);
        false ->
            remove_duplicates(T, [H|L])
    end.

