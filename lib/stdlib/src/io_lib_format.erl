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
-module(io_lib_format).

%% Formatting functions of io library.

-export([fwrite/2,fwrite_g/1,indentation/2]).

%% fwrite(Format, ArgList) -> [Char].
%%  Format the arguments in ArgList after string Format. Just generate
%%  an error if there is an error in the arguments.
%%
%%  To do the printing command correctly we need to calculate the
%%  current indentation for everything before it. This may be very
%%  expensive, especially when it is not needed, so we first determine
%%  if, and for how long, we need to calculate the indentations. We do
%%  this by first collecting all the control sequences and
%%  corresponding arguments, then counting the print sequences and
%%  then building the output.  This method has some drawbacks, it does
%%  two passes over the format string and creates more temporary data,
%%  and it also splits the handling of the control characters into two
%%  parts.

fwrite(Format, Args) when is_atom(Format) ->
    fwrite(atom_to_list(Format), Args);
fwrite(Format, Args) when is_binary(Format) ->
    fwrite(binary_to_list(Format), Args);
fwrite(Format, Args) ->
    Cs = collect(Format, Args),
    Pc = pcount(Cs),
    build(Cs, Pc, 0).

collect([$~|Fmt0], Args0) ->
    {C,Fmt1,Args1} = collect_cseq(Fmt0, Args0),
    [C|collect(Fmt1, Args1)];
collect([C|Fmt], Args) ->
    [C|collect(Fmt, Args)];
collect([], []) -> [].

collect_cseq(Fmt0, Args0) ->
    {F,Ad,Fmt1,Args1} = field_width(Fmt0, Args0),
    {P,Fmt2,Args2} = precision(Fmt1, Args1),
    {Pad,Fmt3,Args3} = pad_char(Fmt2, Args2),
    {C,As,Fmt4,Args4} = collect_cc(Fmt3, Args3),
    {{C,As,F,Ad,P,Pad},Fmt4,Args4}.

field_width([$-|Fmt0], Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F,left,Fmt,Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F,right,Fmt,Args}.

precision([$.|Fmt], Args) ->
    field_value(Fmt, Args);
precision(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([$*|Fmt], [A|Args]) when is_integer(A) ->
    {A,Fmt,Args};
field_value([C|Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([C|Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10*F + (C - $0));
field_value(Fmt, Args, F) ->		%Default case
    {F,Fmt,Args}.

pad_char([$.,$*|Fmt], [Pad|Args]) -> {Pad,Fmt,Args};
pad_char([$.,Pad|Fmt], Args) -> {Pad,Fmt,Args};
pad_char(Fmt, Args) -> {$\s,Fmt,Args}.

%% collect_cc([FormatChar], [Argument]) ->
%%	{Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w|Fmt], [A|Args]) -> {$w,[A],Fmt,Args};
collect_cc([$p|Fmt], [A|Args]) -> {$p,[A],Fmt,Args};
collect_cc([$W|Fmt], [A,Depth|Args]) -> {$W,[A,Depth],Fmt,Args};
collect_cc([$P|Fmt], [A,Depth|Args]) -> {$P,[A,Depth],Fmt,Args};
collect_cc([$s|Fmt], [A|Args]) -> {$s,[A],Fmt,Args};
collect_cc([$e|Fmt], [A|Args]) -> {$e,[A],Fmt,Args};
collect_cc([$f|Fmt], [A|Args]) -> {$f,[A],Fmt,Args};
collect_cc([$g|Fmt], [A|Args]) -> {$g,[A],Fmt,Args};
collect_cc([$b|Fmt], [A|Args]) -> {$b,[A],Fmt,Args};
collect_cc([$B|Fmt], [A|Args]) -> {$B,[A],Fmt,Args};
collect_cc([$x|Fmt], [A,Prefix|Args]) -> {$x,[A,Prefix],Fmt,Args};
collect_cc([$X|Fmt], [A,Prefix|Args]) -> {$X,[A,Prefix],Fmt,Args};
collect_cc([$+|Fmt], [A|Args]) -> {$+,[A],Fmt,Args};
collect_cc([$#|Fmt], [A|Args]) -> {$#,[A],Fmt,Args};
collect_cc([$c|Fmt], [A|Args]) -> {$c,[A],Fmt,Args};
collect_cc([$~|Fmt], Args) when is_list(Args) -> {$~,[],Fmt,Args};
collect_cc([$n|Fmt], Args) when is_list(Args) -> {$n,[],Fmt,Args};
collect_cc([$i|Fmt], [A|Args]) -> {$i,[A],Fmt,Args}.

%% pcount([ControlC]) -> Count.
%%  Count the number of print requests.

pcount(Cs) -> pcount(Cs, 0).

pcount([{$p,_As,_F,_Ad,_P,_Pad}|Cs], Acc) -> pcount(Cs, Acc+1);
pcount([{$P,_As,_F,_Ad,_P,_Pad}|Cs], Acc) -> pcount(Cs, Acc+1);
pcount([_|Cs], Acc) -> pcount(Cs, Acc);
pcount([], Acc) -> Acc.

%% build([Control], Pc, Indentation) -> [Char].
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build([{C,As,F,Ad,P,Pad}|Cs], Pc0, I) ->
    S = control(C, As, F, Ad, P, Pad, I),
    Pc1 = decr_pc(C, Pc0),
    if
	Pc1 > 0 -> [S|build(Cs, Pc1, indentation(S, I))];
	true -> [S|build(Cs, Pc1, I)]
    end;
build([$\n|Cs], Pc, _I) -> [$\n|build(Cs, Pc, 0)];
build([$\t|Cs], Pc, I) -> [$\t|build(Cs, Pc, ((I + 8) div 8) * 8)];
build([C|Cs], Pc, I) -> [C|build(Cs, Pc, I+1)];
build([], _Pc, _I) -> [].

decr_pc($p, Pc) -> Pc - 1;
decr_pc($P, Pc) -> Pc - 1;
decr_pc(_, Pc) -> Pc.

%% indentation([Char], Indentation) -> Indentation.
%%  Calculate the indentation of the end of a string given its start
%%  indentation. We assume tabs at 8 cols.

indentation([$\n|Cs], _I) -> indentation(Cs, 0);
indentation([$\t|Cs], I) -> indentation(Cs, ((I + 8) div 8) * 8);
indentation([C|Cs], I) when is_integer(C) ->
    indentation(Cs, I+1);
indentation([C|Cs], I) ->
    indentation(Cs, indentation(C, I));
indentation([], I) -> I.

%% control(FormatChar, [Argument], FieldWidth, Adjust, Precision, PadChar,
%%	   Indentation) ->
%%	[Char]
%%  This is the main dispatch function for the various formatting commands.
%%  Field widths and precisions have already been calculated.

control($w, [A], F, Adj, P, Pad, _I) ->
    term(io_lib:write(A, -1), F, Adj, P, Pad);
control($p, [A], F, Adj, P, Pad, I) ->
    print(A, -1, F, Adj, P, Pad, I);
control($W, [A,Depth], F, Adj, P, Pad, _I) when is_integer(Depth) ->
    term(io_lib:write(A, Depth), F, Adj, P, Pad);
control($P, [A,Depth], F, Adj, P, Pad, I) when is_integer(Depth) ->
    print(A, Depth, F, Adj, P, Pad, I);
control($s, [A], F, Adj, P, Pad, _I) when is_atom(A) ->
    string(atom_to_list(A), F, Adj, P, Pad);
control($s, [L0], F, Adj, P, Pad, _I) ->
    L = iolist_to_chars(L0),
    string(L, F, Adj, P, Pad);
control($e, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_e(A, F, Adj, P, Pad);
control($f, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_f(A, F, Adj, P, Pad);
control($g, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_g(A, F, Adj, P, Pad);
control($b, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control($B, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control($x, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A), 
                                                 is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
control($x, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true);
control($X, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A), 
                                                 is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
control($X, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false);
control($+, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control($#, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control($c, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    char(A band 255, F, Adj, P, Pad);
control($~, [], F, Adj, P, Pad, _I) -> char($~, F, Adj, P, Pad);
control($n, [], F, Adj, P, Pad, _I) -> newline(F, Adj, P, Pad);
control($i, [_A], _F, _Adj, _P, _Pad, _I) -> [].

%% Default integer base
base(none) ->
    10;
base(B) when is_integer(B) ->
    B.

%% term(TermList, Field, Adjust, Precision, PadChar)
%%  Output the characters in a term.
%%  Adjust the characters within the field if length less than Max padding
%%  with PadChar.

term(T, none, _Adj, none, _Pad) -> T;
term(T, none, Adj, P, Pad) -> term(T, P, Adj, P, Pad);
term(T, F, Adj, P0, Pad) ->
    L = lists:flatlength(T),
    P = case P0 of none -> min(L, F); _ -> P0 end,
    if
	L > P ->
	    adjust(chars($*, P), chars(Pad, F-P), Adj);
	F >= P ->
	    adjust(T, chars(Pad, F-L), Adj)
    end.

%% print(Term, Depth, Field, Adjust, Precision, PadChar, Indentation)
%%  Print a term.

print(T, D, none, Adj, P, Pad, I) -> print(T, D, 80, Adj, P, Pad, I);
print(T, D, F, Adj, none, Pad, I) -> print(T, D, F, Adj, I+1, Pad, I);
print(T, D, F, right, P, _Pad, _I) ->
    io_lib_pretty:print(T, P, F, D).

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    term(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_e(Fl, Fd, P) when Fl < 0.0 ->		%Negative numbers
    [$-|float_e(-Fl, Fd, P)];
float_e(_Fl, {Ds,E}, P) ->
    case float_man(Ds, 1, P-1) of
	{[$0|Fs],true} -> [[$1|Fs]|float_exp(E)];
	{Fs,false} -> [Fs|float_exp(E-1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Chars],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs,C} = float_man(Ds, Dc),
    {[$.|Cs],C};
float_man([D|Ds], I, Dc) ->
    case float_man(Ds, I-1, Dc) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false};
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], I, Dc) ->				%Pad with 0's
    {string:chars($0, I, [$.|string:chars($0, Dc)]),false}.

float_man([D|_], 0) when D >= $5 -> {[],true};
float_man([_|_], 0) -> {[],false};
float_man([D|Ds], Dc) ->
    case float_man(Ds, Dc-1) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false}; 
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], Dc) -> {string:chars($0, Dc),false}.	%Pad with 0's

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e,$+|integer_to_list(E)];
float_exp(E) ->
    [$e|integer_to_list(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    term(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) when Fl < 0.0 ->
    [$-|float_f(-Fl, Fd, P)];
float_f(Fl, {Ds,E}, P) when E =< 0 ->
    float_f(Fl, {string:chars($0, -E+1, Ds),1}, P);	%Prepend enough 0's
float_f(_Fl, {Ds,E}, P) ->
    case float_man(Ds, E, P) of
	{Fs,true} -> "1" ++ Fs;			%Handle carry
	{Fs,false} -> Fs
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) ->
    float_data(float_to_list(Fl), []).

float_data([$e|E], Ds) ->
    {lists:reverse(Ds),list_to_integer(E)+1};
float_data([D|Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D|Ds]);
float_data([_|Cs], Ds) ->
    float_data(Cs, Ds).

%% fwrite_g(Float)
%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4, 
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl) ->
    fwrite_g(Fl, none, right, none, $\s).

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E = if A < 1.0e-1 -> -2;
	   A < 1.0e0  -> -1;
	   A < 1.0e1  -> 0;
	   A < 1.0e2  -> 1;
	   A < 1.0e3  -> 2;
	   A < 1.0e4  -> 3;
	   true       -> fwrite_f
	end,
    if  P =< 1, E =:= -1;
	P-1 > E, E >= -1 ->
	    fwrite_f(Fl, F, Adj, P-1-E, Pad);
	P =< 1 ->
	    fwrite_e(Fl, F, Adj, 2, Pad);
	true ->
	    fwrite_e(Fl, F, Adj, P, Pad)
    end.


%% iolist_to_chars(iolist()) -> deep_char_list()

iolist_to_chars([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    [C | iolist_to_chars(Cs)];
iolist_to_chars([I|Cs]) ->
    [iolist_to_chars(I) | iolist_to_chars(Cs)];
iolist_to_chars([]) ->
    [];
iolist_to_chars(B) when is_binary(B) ->
    binary_to_list(B).

%% string(String, Field, Adjust, Precision, PadChar)

string(S, none, _Adj, none, _Pad) -> S;
string(S, F, Adj, none, Pad) ->
    N = lists:flatlength(S),
    if N > F  -> flat_trunc(S, F);
       N =:= F -> S;
       true   -> adjust(S, chars(Pad, F-N), Adj)
    end;
string(S, none, _Adj, P, Pad) ->
    N = lists:flatlength(S),
    if N > P  -> flat_trunc(S, P);
       N =:= P -> S;
       true   -> [S|chars(Pad, P-N)]
    end;
string(S, F, Adj, F, Pad) ->
    string(S, none, Adj, F, Pad);
string(S, F, Adj, P, Pad) when F > P ->
    N = lists:flatlength(S),
    if N > F  -> flat_trunc(S, F);
       N =:= F -> S;
       N > P   -> adjust(flat_trunc(S, P), chars(Pad, F-P), Adj);
       N =:= P -> adjust(S, chars(Pad, F-P), Adj);
       true    -> adjust([S|chars(Pad, P-N)], chars(Pad, F-P), Adj)
    end.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase)
%% -> [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
	    S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
	    term([$-|S], F, Adj, none, Pad);
       true ->
	    S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
	    term(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase)
%% -> [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
	    S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
	    term([$-,Prefix|S], F, Adj, none, Pad);
       true ->
	    S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
	    term([Prefix|S], F, Adj, none, Pad)
    end.

%% char(Char, Field, Adjust, Precision, PadChar) -> [Char].

char(C, none, _Adj, none, _Pad) -> [C];
char(C, F, _Adj, none, _Pad) -> chars(C, F);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(F, right, _P, _Pad) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data,Pad];
adjust(Data, Pad, right) -> [Pad,Data].

min(L, R) when L < R -> L;
min(_L, R) -> R.

%% Flatten and truncate a deep list to at most N elements.

flat_trunc(List, N) when is_integer(N), N >= 0 ->
    flat_trunc(List, N, [], []).

flat_trunc(L, 0, _, R) when is_list(L) ->
    lists:reverse(R);
flat_trunc([H|T], N, S, R) when is_list(H) ->
    flat_trunc(H, N, [T|S], R);
flat_trunc([H|T], N, S, R) ->
    flat_trunc(T, N-1, S, [H|R]);
flat_trunc([], N, [H|S], R) ->
    flat_trunc(H, N, S, R);
flat_trunc([], _, [], R) ->
    lists:reverse(R).

%% A deep version of string:chars/2,3

chars(_C, 0) ->
    [];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C,C];
chars(C, 3) ->
    [C,C,C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C,S|S].

%chars(C, N, Tail) ->
%    [chars(C, N)|Tail].

%% Lowercase conversion

cond_lowercase(String, true) ->
    lowercase(String);
cond_lowercase(String,false) ->
    String.

lowercase([H|T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H-$A+$a)|lowercase(T)];
lowercase([H|T]) ->
    [H|lowercase(T)];
lowercase([]) ->
    [].
