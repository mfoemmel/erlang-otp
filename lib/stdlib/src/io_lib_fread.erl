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
-module(io_lib_fread).

%% Formatted input functions of io library.

-export([fread/2,fread/3]).

%% fread(Continuation, CharList, FormatString)
%%  This is the main function into the re-entrant formatted reader. It
%%  repeatedly collects lines and calls fread/2 to format the input until
%%  all the format string has been used. 

fread([], Chars, Format) ->
    fread({[],Format,0,[]}, Chars, Format);
fread({Rest,RestFormat,N,Inputs}, MoreChars, Format) ->
    %%io:format("FREAD: ~w `~s'~n", [{Rest,RestFormat,N,Inputs},MoreChars]),
    fread_collect(MoreChars, [], Rest, RestFormat, N, Inputs).

fread_collect([$\r|More], Stack, Rest, RestFormat, N, Inputs) ->
    fread(RestFormat, Rest ++ reverse(Stack), N, Inputs, More);
fread_collect([$\n|More], Stack, Rest, RestFormat, N, Inputs) ->
    fread(RestFormat, Rest ++ reverse(Stack), N, Inputs, More);
fread_collect([C|More], Stack, Rest, RestFormat, N, Inputs) ->
    fread_collect(More, [C|Stack], Rest, RestFormat, N, Inputs);
fread_collect([], Stack, Rest, RestFormat, N, Inputs) ->
    {more,{reverse(Stack, Rest),RestFormat,N,Inputs}};
fread_collect(eof, Stack, Rest, RestFormat, N, Inputs) ->
    fread(RestFormat, Rest ++ reverse(Stack) ++ eof, N, Inputs, []).

fread(Format, Line, N0, Inputs0, More) ->
    %%io:format("FREAD1: `~s' `~s'~n", [Format,Line]),
    case fread(Format, Line, N0, Inputs0) of
	{ok,Input,Rest} ->
	    {done,{ok,Input},Rest ++ More};
	{more,RestFormat,N,Inputs} ->
	    %% Don't forget the newline.
	    {more,{More,RestFormat,N+1,Inputs}};
	Other ->				%An error has occurred
	    {done,Other,More}
    end.

%% Conventions
%%   ~s 	String White terminated
%%   ~d         Integer terminated by ~[0-9]
%%   ~f         Float
%%   ~a         as ~s but converted to an atom
%%   ~c		characters without any stripping
%%   ~n		number of characters scanned
%%   WHITE      Skip white space
%%   X          Literal X

fread(Format, Line) ->
    fread(Format, Line, 0, []).

fread([$~|Format0], Line, N, Results) ->
    {Format,F,Sup} = fread_field(Format0),
    fread1(Format, F, Sup, Line, N, Results, Format0);
fread([$\s|Format], Line, N, Results) ->
    fread_skip_white(Format, Line, N, Results);
fread([C|Format], [C|Line], N, Results) ->
    fread(Format, Line, N+1, Results);
fread([F|Format], [C|Line], N, Results) ->
    fread_error(input);
fread([], Line, N, Results) ->
    {ok,reverse(Results),Line}.

fread_skip_white(Format, [$\s|Line], N, Results) ->
    fread_skip_white(Format, Line, N+1, Results);
fread_skip_white(Format, [$\t|Line], N, Results) ->
    fread_skip_white(Format, Line, N+1, Results);
fread_skip_white(Format, Line, N, Results) ->
    fread(Format, Line, N, Results).

%% fread_field(Format) 
%%  Reads the field specification paramters. Returns:
%%
%%	{RestFormat,FieldWidth,Suppress}

fread_field([$*|Format]) -> fread_field(Format, true);
fread_field(Format) -> fread_field(Format, false).

fread_field([C|Format], Sup) when C >= $0, C =< $9 ->
    fread_field(Format, C - $0, Sup);
fread_field(Format, Sup) -> {Format,none,Sup}.

fread_field([C|Format], F, Sup) when C >= $0, C =< $9 ->
    fread_field(Format, 10*F + C - $0, Sup);
fread_field(Format, F, Sup) ->
    {Format,F,Sup}.

%% fread1(Format, FieldWidth, Suppress, Line, N, Results, AllFormat)
%% fread1(Format, FieldWidth, Suppress, Line, N, Results)
%%  The main dispatch function for the formatting commands. Done in two
%%  stages so format commands that need no input can always be processed.

fread1([$l|Format], F, Sup, Line, N, Res, AllFormat) ->
    fread(Format, Line, N, fread_result(Sup, N, Res));
fread1(Format, F, Sup, [], N, Res, AllFormat) ->
    %% Need more input here.
    {more,[$~|AllFormat],N,Res};
fread1(Format, F, Sup, eof, N, [], AllFormat) ->
    %% This is at start of format string so no error.
    eof;
fread1(Format, F, Sup, eof, N, Res, AllFormat) ->
    %% This is an error as there is no more input.
    fread_error(input);
fread1(Format, F, Sup, Line, N, Res, AllFormat) ->
    fread1(Format, F, Sup, Line, N, Res).

fread1([$f|Format], none, Sup, Line0, N0, Res) ->
    {Line,N,Cs} = fread_float_cs(Line0, N0),
    fread_float(Cs, Sup, Format, Line, N, Res);
fread1([$f|Format], F, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, F),
    fread_float(Cs, Sup, Format, Line, N+F, Res);
fread1([$d|Format], none, Sup, Line0, N0, Res) ->
    {Line,N,Cs} = fread_int_cs(Line0, N0),
    fread_integer(Cs, Sup, Format, Line, N, Res);
fread1([$d|Format], F, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, F),
    fread_integer(Cs, Sup, Format, Line, N+F, Res);
fread1([$s|Format], none, Sup, Line0, N0, Res) ->
    {Line,N,Cs} = fread_string_cs(Line0, N0),
    fread_string(Cs, Sup, Format, Line, N, Res);
fread1([$s|Format], F, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, F),
    fread_string(Cs, Sup, Format, Line, N+F, Res);
fread1([$a|Format], none, Sup, Line0, N0, Res) ->
    {Line,N,Cs} = fread_string_cs(Line0, N0),
    fread_atom(Cs, Sup, Format, Line, N, Res);
fread1([$a|Format], F, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, F),
    fread_atom(Cs, Sup, Format, Line, N+F, Res);
fread1([$c|Format], none, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, 1),
    fread_chars(Cs, Sup, Format, Line, N+1, Res);
fread1([$c|Format], F, Sup, Line0, N, Res) ->
    {Line,Cs} = fread_chars(Line0, F),
    fread_chars(Cs, Sup, Format, Line, N+F, Res);
fread1([$~|Format], F, Sup, [$~|Line], N, Res) ->
    fread(Format, Line, N+1, Res);
fread1(Format, F, Sup, Line, N, Res) ->
    fread_error(format).

%% fread_float(FloatChars, Suppress, Format, Line, N, Results)

fread_float(Cs, Sup, Format, Line, N, Res) ->
    case catch list_to_float(fread_skip_white(reverse(Cs))) of
	{'EXIT',R} ->
	    fread_error(float);
	Float ->
	    fread(Format, Line, N, fread_result(Sup, Float, Res))
    end.

%% fread_integer(IntegerChars, Suppress, Format, Line, N, Results)

fread_integer(Cs, Sup, Format, Line, N, Res) ->
    case catch list_to_integer(fread_skip_white(reverse(Cs))) of
	{'EXIT',R} ->
	    fread_error(integer);
	Integer ->
	    fread(Format, Line, N, fread_result(Sup, Integer, Res))
    end.

%% fread_string(StringChars, Suppress, Format, Line, N, Results)

fread_string(error, Sup, Format, Line, N, Res) ->
    fread_error(string);
fread_string(Cs0, Sup, Format, Line, N, Res) ->
    Cs = fread_skip_white(reverse(fread_skip_white(Cs0))),
    fread(Format, Line, N, fread_result(Sup, Cs, Res)).

%% fread_atom(AtomChars, Suppress, Format, Line, N, Results)

fread_atom(error, Sup, Format, Line, N, Res) ->
    fread_error(atom);
fread_atom(Cs0, Sup, Format, Line, N, Res) ->
    Cs = fread_skip_white(reverse(fread_skip_white(Cs0))),
    fread(Format, Line, N, fread_result(Sup, list_to_atom(Cs), Res)).

%% fread_chars(Characters, Suppress, Format, Line, N, Results)

fread_chars(error, Sup, Format, Line, N, Res) ->
    fread_error(character);
fread_chars(Cs, Sup, Format, Line, N, Res) ->
    fread(Format, Line, N, fread_result(Sup, reverse(Cs), Res)).

%% fread_chars(Line, Count)

fread_chars(Line, C) ->
    fread_chars(C, Line, []).

fread_chars(0, Line, Cs) -> {Line,Cs};
fread_chars(N, [$\n|Line], Cs) -> {[$\n|Line],error};
fread_chars(N, [C|Line], Cs) ->
    fread_chars(N-1, Line, [C|Cs]);
fread_chars(N, [], Cs) -> {[],error}.

%% fread_int_cs(Line, N)

fread_int_cs(Line0, N0) ->
    {Line1,N1} = fread_skip_white(Line0, N0),
    {Line,N,Cs} = fread_sign(Line1, N1, []),
    fread_digits(Line, N, Cs).

%% fread_float_cs(Line, N)
%%  A float is "[+|-][0-9]+.[0-9]+[[E|e][+|-][09-]+]

fread_float_cs(Line0, N0) ->
    {Line1,N1} = fread_skip_white(Line0, N0),
    {Line2,N2,Cs2} = fread_sign(Line1, N1, []),
    {Line,N,Cs} = fread_digits(Line2, N2, Cs2),
    fread_float_cs_1(Line, N, Cs).

fread_float_cs_1([$.|Line0], N0, Cs0) ->
    {Line,N,Cs} = fread_digits(Line0, N0+1, [$.|Cs0]),
    fread_float_cs_2(Line, N, Cs);
fread_float_cs_1(Line, N, Cs) ->
    {Line,N,Cs}.

fread_float_cs_2([$e|Line0], N0, Cs0) ->
    {Line,N,Cs} = fread_sign(Line0, N0+1, [$e|Cs0]),
    fread_digits(Line, N, Cs);
fread_float_cs_2([$E|Line0], N0, Cs0) ->
    {Line,N,Cs} = fread_sign(Line0, N0+1, [$E|Cs0]),
    fread_digits(Line, N, Cs);
fread_float_cs_2(Line, N, Cs) ->
    {Line,N,Cs}.

%% fread_string_cs(Line, N)

fread_string_cs(Line0, N0) ->
    {Line,N} = fread_skip_white(Line0, N0),
    fread_skip_nonwhite(Line, N, []).

%% fread_skip_white(Line)
%% fread_skip_white(Line, N)
%% fread_skip_nonwhite(Line, N, Characters)
%% fread_sign(Line, N, Characters)
%% fread_digits(Line, N, Characters)
%%  Read segments of things, return "thing" characters in reverse order.

fread_skip_white([$\s|Line]) -> fread_skip_white(Line);
fread_skip_white([$\t|Line]) -> fread_skip_white(Line);
fread_skip_white([$\n|Line]) -> fread_skip_white(Line);
fread_skip_white(Line) -> Line.

fread_skip_white([$\s|Line], N) ->
    fread_skip_white(Line, N+1);
fread_skip_white([$\t|Line], N) ->
    fread_skip_white(Line, N+1);
fread_skip_white([$\n|Line], N) ->
    fread_skip_white(Line, N+1);
fread_skip_white(Line, N) -> {Line,N}.

fread_skip_nonwhite([$\s|Line], N, Cs) -> {[$\s|Line],N,Cs};
fread_skip_nonwhite([$\t|Line], N, Cs) -> {[$\t|Line],N,Cs};
fread_skip_nonwhite([$\n|Line], N, Cs) -> {[$\n|Line],N,Cs};
fread_skip_nonwhite([C|Line], N, Cs) ->
    fread_skip_nonwhite(Line, N+1, [C|Cs]);
fread_skip_nonwhite([], N, Cs) -> {[],N,Cs}.

fread_sign([$+|Line], N, Cs) -> {Line,N+1,[$+|Cs]};
fread_sign([$-|Line], N, Cs) -> {Line,N+1,[$-|Cs]};
fread_sign(Line, N, Cs) -> {Line,N,Cs}.

fread_digits([C|Line], N, Cs) when C >= $0, C =< $9 ->
    fread_digits(Line, N+1, [C|Cs]);
fread_digits(Line, N, Cs) -> {Line,N,Cs}.

%% fread_result(Suppress, Value, Results)

fread_result(true, V, Res) -> Res;
fread_result(false, V, Res) -> [V|Res].

fread_error(In) ->
    {error,{fread,In}}.

%%
%% Utilities
%%

reverse(List) ->
    reverse(List, []).

reverse([], Stack) ->
    Stack;
reverse([H|T], Stack) ->
    reverse(T, [H|Stack]).
