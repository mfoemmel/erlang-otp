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
-module(instrument).

-export([holes/1, mem_limits/1, memory_data/0, read_memory_data/1,
	 sort/1, store_memory_data/1, sum_blocks/1, type_string/1]).

%% (This is sizeof(mem_link) in utils.c.)
-define(INFO_SIZE, 28).

memory_data() ->
    erlang:info(allocated).

store_memory_data(File) ->
    %% The BIF returns 'true' when all goes well.
    erlang:info({allocated, File}),
    ok.

read_memory_data(File) ->
    file:consult(File).

holes([]) ->
    ok;
holes([E | L]) ->
    holes(E, L).

holes(E1, []) ->
    io:format("~n");
holes(E1, [E2 | Rest]) ->
    check_hole(E1, E2),
    holes(E2, Rest).

check_hole({_,P1,S1,_}, {_,P2,S2,_}) ->
    End = P1+S1,
    Hole = P2 - (End + ?INFO_SIZE),
    if
	Hole =< 7 ->
	    ok;
	true ->
	    io:format(" ~p", [Hole])
    end.

sum_blocks(L) ->
    lists:foldl(fun({_,P,S,_}, Sum) -> S+Sum end,
		0,
		L).

mem_limits(L) ->
    {_, P1, _, _} = hd(L),
    {_, P2, S2, _} = lists:last(L),
    {P1, P2+S2}.

sort(L) ->
    lists:keysort(2, L).

type_string(-1) ->
    "unknown";
type_string(1) ->
    "atom text";
type_string(11) ->
    "atom desc";
type_string(2) ->
    "bignum (big_to_list)";
type_string(31) ->
    "fixalloc";
type_string(32) ->
    "unknown fixalloc block";
type_string(33) ->
    "message buffer";
type_string(4) ->
    "stack";
type_string(51) ->
    "db segment";
type_string(52) ->
    "db term";
type_string(53) ->
    "db add_counter";
type_string(54) ->
    "db segment table";
type_string(55) ->
    "db table (fix)";
type_string(56) ->
    "db bindings";
type_string(57) ->
    "db counter";
type_string(61) ->
    "binary";
type_string(62) ->
    "procbin (fix)";
type_string(71) ->
    "binary (io.c)";
type_string(8) ->
    "heap";
type_string(801) ->
    "heap (1)";
type_string(802) ->
    "heap (2)";
type_string(803) ->
    "heap (3)";
type_string(804) ->
    "heap (4)";
type_string(805) ->
    "heap (5)";
type_string(91) ->
    "process table";
type_string(92) ->
    "process desc";
type_string(110) ->
    "hash buckets";
type_string(111) ->
    "hash table";
type_string(120) ->
    "index init";
type_string(121) ->
    "index table";
type_string(130) ->
    "temp buffer";
type_string(140) ->
    "timer wheel";
type_string(150) ->
    "distribution cache";
type_string(151) ->
    "dmem";
type_string(152) ->
    "distribution table";
type_string(160) ->
    "port table";
type_string(161) ->
    "driver entry";
type_string(162) ->
    "port setup";
type_string(163) ->
    "port wait";
type_string(170) ->
    "module";
type_string(171) ->
    "fundef";
type_string(180) ->
    "file table";
type_string(181) ->
    "driver table";
type_string(190) ->
    "inet driver";
type_string(200) ->
    "efile driver";
type_string(210) ->
    "gc root set".
