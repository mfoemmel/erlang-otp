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
-module(icscan).


-export([scan/2,add_keyw/0]).

-include("ic.hrl").


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

-import(lists, [reverse/1]).


scan(G, File) ->
    Kw = G#genobj.keywtab,
    PL = call_preproc(G, Kw, File),
    call_scan(G, Kw, PL).

call_preproc(G, Kw, File) ->
    case icgen:get_opt(G, use_preproc) of
	true -> 
	    icpreproc:preproc(G, File);
	false ->
	    case catch file:read_file(File) of
		{ok, Bin} ->
		    binary_to_list(Bin);
		Other ->
		    exit(Other)
	    end
    end.

call_scan(G, Kw, PL) ->
    RSL = scan(G, Kw, PL, 1, []),
    lists:reverse(RSL).


%% Guard macros used at top scan functions only
-define(is_number(X), X >= $0, X =< $9).
-define(is_upper(X), X >= $A, X =< $Z).
-define(is_lower(X), X >= $a, X =< $z).


scan(G, Kw, [X|Str], Line, Out) when ?is_upper(X) ->
    scan_name(G, Kw, Str, [X], Line, Out);
scan(G, Kw, [X|Str], Line, Out) when ?is_lower(X) ->
    scan_name(G, Kw, Str, [X], Line, Out);
scan(G, Kw, [X|Str], Line, Out) when ?is_number(X) ->
    scan_number(G, Kw, Str, [X], Line, Out);
scan(G, Kw, [9| T], Line, Out) -> scan(G, Kw, T, Line, Out);
scan(G, Kw, [32| T], Line, Out) -> scan(G, Kw, T, Line, Out);
scan(G, Kw, [$\r|Str], Line, Out) ->
    scan(G, Kw, Str, Line, Out);
scan(G, Kw, [$\n|Str], Line, Out) ->
    scan(G, Kw, Str, Line+1, Out);
scan(G, Kw, [$:, $: | Str], Line, Out) ->
    scan(G, Kw, Str, Line, [{'::', Line} | Out]);
scan(G, Kw, [$/, $/ | Str], Line, Out) ->
    Rest = skip_to_nl(Str),
    scan(G, Kw, Rest, Line, Out);
scan(G, Kw, [$/, $* | Str], Line, Out) ->
    Rest = skip_comment(Str),
    scan(G, Kw, Rest, Line, Out);
scan(G, Kw, [$"|Str], Line, Out) ->
    scan_const(G, Kw, string, Str, [], Line, Out);
scan(G, Kw, [$'|Str], Line, Out) ->
    scan_const(G, Kw, char, Str, [], Line, Out);
scan(G, Kw, [$. | Str], Line, Out) ->
    scan_frac(G, Kw, Str, [$.], Line, Out);
scan(G, Kw, [$# | Str], Line, Out) ->
    scan_preproc(G, Kw, Str, Line, Out);
scan(G, Kw, [$<, $< | Str], Line, Out) ->
    scan(G, Kw, Str, Line, [{'<<', Line} | Out]);
scan(G, Kw, [$>, $> | Str], Line, Out) ->
    scan(G, Kw, Str, Line, [{'>>', Line} | Out]);
scan(G, Kw, [C|Str], Line, Out) ->
    scan(G, Kw, Str, Line, [{list_to_atom([C]), Line} | Out]);
	    
scan(G, Kw, [], Line, Out) ->
    Out.


scan_number(G, Kw, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_number(G, Kw, Str, [X|Accum], Line, Out);
scan_number(G, Kw, [X|Str], Accum, Line, Out) when X==$. ->
    scan_frac(G, Kw, Str, [X|Accum], Line, Out);
scan_number(G, Kw, [X|Str], Accum, Line, Out) when X==$e ->
    scan_exp(G, Kw, Str, [X|Accum], Line, Out);
scan_number(G, Kw, [X|Str], Accum, Line, Out) when X==$E ->
    scan_exp(G, Kw, Str, [X|Accum], Line, Out);
scan_number(G, Kw, Str, Accum, Line, Out) ->
    scan(G, Kw, Str, Line, [{'<integer_literal>', Line,
			(lists:reverse(Accum))} | Out]).


%% Floating point number scan.
%%
%%	Non trivial scan. A float consists of an integral part, a
%%	decimal point, a fraction part, an e or E and a signed integer
%%	exponent. Either the integer part or the fraction part but not
%%	both may be missing, and either the decimal point or the
%%	exponent part but not both may be missing. The exponent part
%%	must consist of an e or E and a possibly signed exponent.
%%
%%	Analysis shows that "1." ".7" "1e2" ".5e-3" "1.7e2" "1.7e-2"
%%	is allowed and "1" ".e9" is not. The sign is only allowed just
%%	after an e or E. The scanner reads a number as an integer
%%	until it encounters a "." so the integer part only error case
%%	will not be caught in the scanner (but rather in expression
%%	evaluation)

scan_frac(G, Kw, [$e | Str], [$.], Line, Out) ->
    icgen:fatal_error(G, {illegal_float, Line});
scan_frac(G, Kw, [$E | Str], [$.], Line, Out) ->
    icgen:fatal_error(G, {illegal_float, Line});
scan_frac(G, Kw, Str, Accum, Line, Out) ->
    scan_frac2(G, Kw, Str, Accum, Line, Out).

scan_frac2(G, Kw, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_frac2(G, Kw, Str, [X|Accum], Line, Out);
scan_frac2(G, Kw, [X|Str], Accum, Line, Out) when X==$e ->
    scan_exp(G, Kw, Str, [X|Accum], Line, Out);
scan_frac2(G, Kw, [X|Str], Accum, Line, Out) when X==$E ->
    scan_exp(G, Kw, Str, [X|Accum], Line, Out);
scan_frac2(G, Kw, Str, Accum, Line, Out) ->
    scan(G, Kw, Str, Line, [{'<floating_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]).

scan_exp(G, Kw, [X|Str], Accum, Line, Out) when X==$- ->
    scan_exp2(G, Kw, Str, [X|Accum], Line, Out);
scan_exp(G, Kw, Str, Accum, Line, Out) ->
    scan_exp2(G, Kw, Str, Accum, Line, Out).

scan_exp2(G, Kw, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_exp2(G, Kw, Str, [X|Accum], Line, Out);
scan_exp2(G, Kw, Str, Accum, Line, Out) ->
    scan(G, Kw, Str, Line, [{'<floating_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]).


scan_name(G, Kw, [X|Str], Accum, Line, Out) when ?is_upper(X) ->
    scan_name(G, Kw, Str, [X|Accum], Line, Out);
scan_name(G, Kw, [X|Str], Accum, Line, Out) when ?is_lower(X) ->
    scan_name(G, Kw, Str, [X|Accum], Line, Out);
scan_name(G, Kw, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_name(G, Kw, Str, [X|Accum], Line, Out);
scan_name(G, Kw, [$_|Str], Accum, Line, Out) ->
    scan_name(G, Kw, Str, [$_|Accum], Line, Out);
scan_name(G, Kw, S, Accum, Line, Out) ->
    L = lists:reverse(Accum),
    X = case ets:lookup(Kw, L) of
	    [] -> {'<identifier>', Line, L};
	    _ -> {list_to_atom(L), Line}
	end,
    scan(G, Kw, S, Line, [X | Out]).

%% Shall scan a constant
scan_const(G, Kw, string, [$" | Rest], Accum, Line, Out) ->
    scan(G, Kw, Rest, Line, 
	 [{'<string_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, Kw, string, [], Accum, Line, Out) -> %% Bad string
    icgen:error(G, {bad_string, Line}),
    Out;
scan_const(G, Kw, char, [$' | Rest], Accum, Line, Out) ->
    scan(G, Kw, Rest, Line, 
	 [{'<character_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, Kw, Mode, [$\\, C | Rest], Accum, Line, Out) ->
    case escaped_char(C) of
	error ->
	    icgen:error(G, {bad_escape_character, Line, C}), %% Bad escape character
	    scan_const(G, Kw, Mode, Rest, [C | Accum], Line, Out);
	EC ->
	    scan_const(G, Kw, Mode, Rest, [EC | Accum], Line, Out)
    end;
scan_const(G, Kw, Mode, [C | Rest], Accum, Line, Out) ->
    scan_const(G, Kw, Mode, Rest, [C | Accum], Line, Out).


%%
%% Preprocessor output handling
%%
%%	gcc outputs a line with line number, file name (within \") and
%%	one or more integer flags. The scanner scans the line number,
%%	the id and all integers up to nl.
%%
%% NOTE: This will have to be enhanced in order to eat #pragma
%%
scan_preproc(G, Kw, Str, Line, Out) ->
    {List, Rest} = scan_to_nl2(strip(Str), []),
    NewLine = get_new_line_nr(strip(List), Line+1, []),
    case scan_number(G, Kw, List, [], Line, [{'#', Line} | Out]) of
	L when list(L) ->
	    scan(G, Kw, Rest, NewLine, [{'#', Line} | L]);
	X ->
	    scan(G, Kw, Rest, NewLine, [{'#', Line}, {'#', Line} | Out])
    end.

%%    Out2 = scan(G, Kw, List, Line, [{'#', Line} | Out]),
%%    NewLine = get_new_line_nr(strip(List), Line+1, []),
%%    scan(G, Kw, Rest, NewLine, [{'#', Line} | Out2]).

get_new_line_nr([C|R], Line, Acc) when C>=$0, C=<$9 ->
    get_new_line_nr(R, Line, [C|Acc]);
get_new_line_nr(_, Line, []) -> Line;		% No line nr found
get_new_line_nr(_, _, Acc) -> list_to_integer(reverse(Acc)).

    
%%    NewLine = list_to_integer(hd(List)),
%%    Id = delete($", delete($", hd(tl(List)))),
%%    Flags = lists:map(fun(X) -> list_to_integer(X) end, tl(tl(List))),
%%    scan(G, Kw, Rest, NewLine, [{preproc, NewLine, {Id, Flags}} | Out]).

scan_to_nl2([], Acc) -> {reverse(Acc), []};
scan_to_nl2([$\n|Str], Acc) -> {reverse(Acc), Str};
scan_to_nl2([$\r|R], Acc) -> scan_to_nl2(R, Acc);
scan_to_nl2([C|R], Acc) -> scan_to_nl2(R, [C|Acc]).

strip([$ |R]) -> strip(R);
strip(L) -> L.

scan_to_nl([32 | Str], [], Acc2) -> scan_to_nl(Str, [], Acc2);
scan_to_nl([32 | Str], Acc1, Acc2) -> 
    scan_to_nl(Str, [], [reverse(Acc1) | Acc2]);
scan_to_nl([10 | Str], Acc1, Acc2) -> 
    {reverse([reverse(Acc1) | Acc2]), Str};
scan_to_nl([X|Str], Acc1, Acc2) -> scan_to_nl(Str, [X|Acc1], Acc2).


%% Escaped character. Escaped chars are repr as two characters in the
%% input list of letters and this is translated into one char.
escaped_char($n) -> $\n;
escaped_char($t) -> $\t;
escaped_char($v) -> $\v;
escaped_char($b) -> $\b;
escaped_char($r) -> $ ;
escaped_char($f) -> $\f;
escaped_char($a) -> $\a;
escaped_char($\\) -> $\\;
escaped_char($?) -> $?;
escaped_char($') -> $';
escaped_char($") -> $";
%% Error
escaped_char(Other) -> error.

skip_to_nl([]) -> [];
skip_to_nl([$\n | Str]) ->[$\n | Str];
skip_to_nl([_|Str]) ->
    skip_to_nl(Str).

skip_comment([$\\, _ | Str]) ->
    skip_comment(Str);
skip_comment([$*, $/ | Str]) -> Str;
skip_comment([_|Str]) -> 
    skip_comment(Str).


%%----------------------------------------------------------------------
%% Shall separate keywords from identifiers and numbers
%%
%%-define(add_keyw(K, L), [{list_to_atom(K), ?line} | L]).

-define(add_k2(TAB,KEYW), ets:insert(TAB,KEYW)).


%% Fill in the ets of reserved words
add_keyw() -> 
    G = ets:new(keywtab, [public, bag]),
    ?add_k2(G,{"Object"}),
    ?add_k2(G,{"in"}),
    ?add_k2(G,{"interface"}),
    ?add_k2(G,{"case"}),
    ?add_k2(G,{"union"}),
    ?add_k2(G,{"struct"}),
    ?add_k2(G,{"any"}),
    ?add_k2(G,{"long"}),
    ?add_k2(G,{"float"}),
    ?add_k2(G,{"out"}),
    ?add_k2(G,{"enum"}),
    ?add_k2(G,{"double"}),
    ?add_k2(G,{"context"}),
    ?add_k2(G,{"oneway"}),
    ?add_k2(G,{"sequence"}),
    ?add_k2(G,{"FALSE"}),
    ?add_k2(G,{"readonly"}),
    ?add_k2(G,{"char"}),
    ?add_k2(G,{"void"}),
    ?add_k2(G,{"inout"}),
    ?add_k2(G,{"attribute"}),
    ?add_k2(G,{"octet"}),
    ?add_k2(G,{"TRUE"}),
    ?add_k2(G,{"switch"}),
    ?add_k2(G,{"unsigned"}),
    ?add_k2(G,{"typedef"}),
    ?add_k2(G,{"const"}),
    ?add_k2(G,{"raises"}),
    ?add_k2(G,{"string"}),
    ?add_k2(G,{"default"}),
    ?add_k2(G,{"short"}),
    ?add_k2(G,{"module"}),
    ?add_k2(G,{"exception"}),
    ?add_k2(G,{"boolean"}),
    G.






