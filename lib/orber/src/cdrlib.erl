%%--------------------------------------------------------------------
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
%% File: cdrlib.erl
%% 
%% Description:
%%    CDR basic type encode/decode functions
%% 
%% Creation date: 970214
%%
%%-----------------------------------------------------------------
-module(cdrlib).

-include_lib("orber/include/corba.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([ %% IIOP 1.0 -
	  enc_short/2, enc_r_short/2, dec_short/2,
	  enc_unsigned_short/2, enc_r_unsigned_short/2, dec_unsigned_short/2,
	  enc_long/2, enc_r_long/2, dec_long/2,
	  enc_unsigned_long/2, enc_r_unsigned_long/2, dec_unsigned_long/2,
	  enc_bool/2, dec_bool/1,
	  enc_float/2, enc_r_float/2, dec_float/2,
	  enc_double/2, enc_r_double/2, dec_double/2,
	  enc_char/2, dec_char/1,
	  enc_octet/2, dec_octet/1,
	  enc_enum/3, enc_r_enum/3, dec_enum/3,
	  %% IIOP 1.1 - 
	  enc_longlong/2, enc_r_longlong/2, dec_longlong/2,
	  enc_unsigned_longlong/2, enc_r_unsigned_longlong/2, dec_unsigned_longlong/2
	  %%enc_longdouble/2, enc_r_longdouble/2, dec_longdouble/2
	  %%enc_fixed/4, enc_r_fixed/4, dec_fixed/2
	 ]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 10).

%%-----------------------------------------------------------------
%% short
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_short/2
%%-----------------------------------------------------------------
enc_short(X, Message) when integer(X) -> 
    [((X) bsr 8) band 16#ff, (X) band 16#ff | Message];
enc_short(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_short(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_short/2
%%-----------------------------------------------------------------
enc_r_short(X, Message) when integer(X) -> 
    [(X) band 16#ff, ((X) bsr 8) band 16#ff | Message];
enc_r_short(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_short(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_short/2
%%-----------------------------------------------------------------
dec_short([X1,X0 | R], big) ->
    X = (X1 bsl 8) + X0,
    if X1 >= 16#80 ->
	    {X - 16#10000, R};
       true  ->
	    {X, R}
    end;
dec_short([X1,X0 | R], little) ->
    X = (X0 bsl 8) + X1,
    if X0 >= 16#80 ->
	    {X - 16#10000, R};
       true  ->
	    {X, R}
    end.


%%-----------------------------------------------------------------
%% unsigned short
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_unsigned_short/2
%%-----------------------------------------------------------------
enc_unsigned_short(X, Message) when integer(X), X >= 0 -> 
    enc_short(X, Message);
enc_unsigned_short(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_unsigned_short(~p); not integer >= 0", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_unsigned_short/2
%%-----------------------------------------------------------------
enc_r_unsigned_short(X, Message) when integer(X), X >= 0 -> 
    enc_r_short(X, Message);
enc_r_unsigned_short(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_unsigned_short(~p); not integer >= 0", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_short/2
%%-----------------------------------------------------------------
dec_unsigned_short([X1,X0 | R],big) ->
    { (X1 bsl 8) + X0, R};
dec_unsigned_short([X1,X0 | R],little) ->
    { (X0 bsl 8) + X1, R}.


%%-----------------------------------------------------------------
%% long
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_long/2
%%-----------------------------------------------------------------
enc_long(X, Message) when integer(X) -> 
    [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
     ((X) bsr 8) band 16#ff, (X) band 16#ff| Message];
enc_long(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_long(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_long/2
%%-----------------------------------------------------------------
enc_r_long(X, Message) when integer(X) -> 
    [(X) band 16#ff, ((X) bsr 8) band 16#ff,
     ((X) bsr 16) band 16#ff, ((X) bsr 24) band 16#ff | Message];
enc_r_long(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_long(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_long/2
%%-----------------------------------------------------------------
dec_long([X3,X2,X1,X0 | R],big) ->
    X = (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0,
    if X3 >= 16#80 ->
	    {X - 16#100000000, R};
       true  ->

	    {X, R}
    end;
dec_long([X3,X2,X1,X0 | R],little) ->
    X = (X0 bsl 24) + (X1 bsl 16) + (X2 bsl 8) + X3,
    if X0 >= 16#80 ->
	    {X - 16#100000000, R};
       true  ->
	    {X, R}
    end.


%%-----------------------------------------------------------------
%% unsigned_long
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_unsigned_long/2
%%-----------------------------------------------------------------
enc_unsigned_long(X, Message) when integer(X), X >= 0 ->
    enc_long(X,Message);
enc_unsigned_long(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_unsigned_long(~p); not integer >=0 ", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_unsigned_long/2
%%-----------------------------------------------------------------
enc_r_unsigned_long(X, Message) when integer(X), X >= 0 ->
    enc_r_long(X,Message);
enc_r_unsigned_long(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_unsigned_long(~p); not integer >=0 ", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_long/2
%%-----------------------------------------------------------------
dec_unsigned_long([X3,X2,X1,X0 | R],big) ->
    { (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0, R};
dec_unsigned_long([X3,X2,X1,X0 | R],little) ->
    { (X0 bsl 24) + (X1 bsl 16) + (X2 bsl 8) + X3, R}.


%%-----------------------------------------------------------------
%% boolean
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_bool/2
%%-----------------------------------------------------------------
enc_bool(true, Message) -> [1| Message];
enc_bool(false, Message) -> [0| Message].

%%-----------------------------------------------------------------
%% Func: dec_bool/1
%%-----------------------------------------------------------------
dec_bool([1|R]) -> {true, R};
dec_bool([0|R]) -> {false, R}.


%%-----------------------------------------------------------------
%% Float [S=1 | E=8 | F=23]
%% X = (-1)^S * 2^(E-127) * 1.F
%%-----------------------------------------------------------------
-define(FLOAT_BASE, 16#800000).
-define(FLOAT_BIAS, 127).

%%-----------------------------------------------------------------
%% Func: enc_float/2
%%-----------------------------------------------------------------
enc_float(X, Message) when number(X) ->
    {S, E, F} = enc_ieee(X, ?FLOAT_BASE, ?FLOAT_BIAS),
    [ (S bsl 7) bor ((E bsr 1) band 16#7f),
     ((F bsr 16) band 16#7f) bor ((E band 1) bsl 7),
     (F bsr 8) band 16#ff,
     (F band 16#ff) | Message];
enc_float(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_float(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_float/2
%%-----------------------------------------------------------------
enc_r_float(X, Message) when number(X) ->
    {S, E, F} = enc_ieee(X, ?FLOAT_BASE, ?FLOAT_BIAS),
    [ (F band 16#ff),
     (F bsr 8) band 16#ff,
     ((F bsr 16) band 16#7f) bor ((E band 1) bsl 7),
     (S bsl 7) bor ((E bsr 1) band 16#7f)
     | Message];
enc_r_float(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_float(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_float/2
%%-----------------------------------------------------------------
dec_float([X3,X2,X1,X0 | R], big) ->
    E = (X3 band 16#7f) bsl 1 + (X2 bsr 7),
    F = (X2 band 16#7f) bsl 16 + (X1 bsl 8) + X0,
    if
	E == 0, F == 0 -> 
	    { 0.0, R};
	X3 >= 16#80 ->
	    { - math:pow(2, E-?FLOAT_BIAS) * (1 + F / ?FLOAT_BASE), R};
	true ->
	    { math:pow(2, E-?FLOAT_BIAS) * (1 + F / ?FLOAT_BASE), R}
    end;
dec_float([X3,X2,X1,X0 | R],little) ->
    E = (X0 band 16#7f) bsl 1 + (X1 bsr 7),
    F = (X1 band 16#7f) bsl 16 + (X2 bsl 8) + X3,
    if
	E == 0, F == 0 -> 
	    { 0.0, R};
	X0 >= 16#80 ->
	    { - math:pow(2, E-?FLOAT_BIAS) * (1 + F / ?FLOAT_BASE), R};
	true ->
	    { math:pow(2, E-?FLOAT_BIAS) * (1 + F / ?FLOAT_BASE), R}
    end.
 
%%-----------------------------------------------------------------
%% Double [S=1 | E=11 | F=52]
%% X = (-1)^S * 2^(E-1023) * 1.F
%%-----------------------------------------------------------------
-define(DOUBLE_BASE, 16#10000000000000).
-define(DOUBLE_BIAS, 1023).

%%-----------------------------------------------------------------
%% Func: enc_double/2
%%-----------------------------------------------------------------
enc_double(X, Message) when number(X) ->
    {S, E, F} = enc_ieee(X, ?DOUBLE_BASE, ?DOUBLE_BIAS),
    [ (S bsl 7) bor ((E bsr 4) band 16#7f),
     ((F bsr 48) band 16#0f) bor ((E band 16#f) bsl 4),
     (F bsr 40) band 16#ff,
     (F bsr 32) band 16#ff,
     (F bsr 24) band 16#ff,
     (F bsr 16) band 16#ff,
     (F bsr 8) band 16#ff,
     (F band 16#ff) | Message];
enc_double(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_double(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_double/2
%%-----------------------------------------------------------------
enc_r_double(X, Message) when number(X) ->
    {S, E, F} = enc_ieee(X, ?DOUBLE_BASE, ?DOUBLE_BIAS),
    [(F band 16#ff),
     (F bsr 8) band 16#ff,
     (F bsr 16) band 16#ff,
     (F bsr 24) band 16#ff,
     (F bsr 32) band 16#ff,
     (F bsr 40) band 16#ff,
     ((F bsr 48) band 16#0f) bor ((E band 16#f) bsl 4),
     (S bsl 7) bor ((E bsr 4) band 16#7f)
     | Message];
enc_r_double(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_double(~p); not a number.", [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_double/2
%%-----------------------------------------------------------------
dec_double([X7,X6,X5,X4,X3,X2,X1,X0 | R], big) ->
    E = (X7 band 16#7f) bsl 4 + (X6 bsr 4),
    F = (X6 band 16#0f) bsl 48 +
	(X5 bsl 40) + (X4 bsl 32) + (X3 bsl 24) +
	(X2 bsl 16) + (X1 bsl 8) + X0,
    if
	E == 0, F == 0 -> 
	    { 0.0, R};
	X7 >= 16#80 ->
	    { - math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R};
	true ->
	    { math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R}
    end;
dec_double([X7,X6,X5,X4,X3,X2,X1,X0 | R], little) ->
    E = (X0 band 16#7f) bsl 4 + (X1 bsr 4),
    F = (X1 band 16#0f) bsl 48 +
	(X2 bsl 40) + (X3 bsl 32) + (X4 bsl 24) +
	(X5 bsl 16) + (X6 bsl 8) + X7,
    if
	E == 0, F == 0 -> 
	    { 0.0, R};
	X0 >= 16#80 ->
	    { - math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R};
	true ->
	    { math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R}
    end.



%%-----------------------------------------------------------------
%% unscale a float return {Sign, Exponent, Fraction}
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_ieee/3
%%-----------------------------------------------------------------
enc_ieee(X, Base, Bias) when X < 0 ->
    {E,F} = unscale_float(-X),
    {1, E + Bias, trunc((F-1)*Base)};
enc_ieee(X, Base, Bias) when X > 0 ->
    {E,F} = unscale_float(X),
    {0, E + Bias, trunc((F-1)*Base)};
enc_ieee(X, _, _) ->
    {0, 0, 0}.

%%-----------------------------------------------------------------
%% Func: unscale_float/1
%%-----------------------------------------------------------------
unscale_float(X) when X == 1.0 -> {0, 0};
unscale_float(X) when X > 1.0 ->  exp_down(X, 0);
unscale_float(X) when X < 1.0 -> exp_up(X, 0);
unscale_float(X) -> {0, X}.

%%-----------------------------------------------------------------
%% Func: exp_down/2
%%-----------------------------------------------------------------
exp_down(X, E) when X >= 2 -> exp_down(X / 2, E+1);
exp_down(X, E) -> {E, X}.

%%-----------------------------------------------------------------
%% Func: exp_up/2
%%-----------------------------------------------------------------
exp_up(X, E) when X < 1 -> exp_up(X * 2, E-1);
exp_up(X, E) ->  {E, X}.



%%-----------------------------------------------------------------
%% char
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_char/2
%%-----------------------------------------------------------------
enc_char(X, Message) when integer(X) -> 
    [X | Message];

enc_char(X,_) -> 
    orber:debug_level_print("[~p] cdrlib:enc_char(~p); not an integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=102,completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_char/1
%%-----------------------------------------------------------------
dec_char([X0 | R]) when integer(X0) ->
    { X0, R};

dec_char([X0 | R]) ->
    orber:debug_level_print("[~p] cdrlib:dec_char(~p); not an integer.", 
			    [?LINE, X0], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=102,completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% octet
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_octet/2
%%-----------------------------------------------------------------
enc_octet(X, Message) -> 
    [X | Message].

%%-----------------------------------------------------------------
%% Func: dec_octet/1
%%-----------------------------------------------------------------
dec_octet([X0 | R]) ->
    { X0, R}.

%%-----------------------------------------------------------------
%% enum
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_enum/3
%%-----------------------------------------------------------------
enc_enum(Enum, ElemList, Message) ->
    Val = getEnumValue(ElemList,Enum, 0),
    enc_unsigned_long(Val, Message).

%%-----------------------------------------------------------------
%% Func: enc_r_enum/3
%%-----------------------------------------------------------------
enc_r_enum(Enum, ElemList, Message) ->
    Val = getEnumValue(ElemList,Enum, 0),
    enc_r_unsigned_long(Val, Message).

getEnumValue([],Enum, _) -> 
    orber:debug_level_print("[~p] cdrlib:enc_enum/enc_r_enum(~p); not exist.", 
			    [?LINE, Enum], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=102,completion_status=?COMPLETED_NO});
getEnumValue([Enum |List], Enum, N) ->
    N;
getEnumValue([_ |List], Enum, N) ->
    getEnumValue(List, Enum, N + 1).

%%-----------------------------------------------------------------
%% Func: dec_enum/2
%%-----------------------------------------------------------------
dec_enum(ElemList, Message, ByteOrder) ->
    {N, Rest}  = dec_unsigned_long(Message, ByteOrder),
    case catch lists:nth(N + 1, ElemList) of
	{'EXIT', _} ->
	    orber:debug_level_print("[~p] cdrlib:dec_enum(~p, ~p); not defined.", 
				    [?LINE, N, ElemList], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{minor=102,completion_status=?COMPLETED_NO});
	X ->
	    {list_to_atom(X), Rest}
    end.


%%-----------------------------------------------------------------
%% IIOP 1.1 - 
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% longlong 
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_longlong/2
%%-----------------------------------------------------------------
enc_longlong(X, Message) when integer(X) -> 
    [((X) bsr 56) band 16#ff, ((X) bsr 48) band 16#ff,
     ((X) bsr 40) band 16#ff, ((X) bsr 32) band 16#ff,
     ((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
     ((X) bsr 8) band 16#ff, (X) band 16#ff| Message];
enc_longlong(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_longlong(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_longlong/2
%%-----------------------------------------------------------------
enc_r_longlong(X, Message) when integer(X) -> 
    [(X) band 16#ff, ((X) bsr 8) band 16#ff, ((X) bsr 16) band 16#ff,
     ((X) bsr 24) band 16#ff, ((X) bsr 32) band 16#ff, ((X) bsr 40) band 16#ff,
     ((X) bsr 48) band 16#ff, ((X) bsr 56) band 16#ff | Message];
enc_r_longlong(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_longlong(~p); not integer.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_longlong/2
%%-----------------------------------------------------------------
dec_longlong([X7,X6,X5,X4,X3,X2,X1,X0 | R], big) ->
    X = (X7 bsl 56) + (X6 bsl 48) + (X5 bsl 40) + (X4 bsl 32) +
	(X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0,
    if X7 >= 16#80 ->
	    {X - 16#10000000000000000, R};
       true  ->
	    {X, R}
    end;
dec_longlong([X7,X6,X5,X4,X3,X2,X1,X0 | R], little) ->
    X = (X0 bsl 56) + (X1 bsl 48) + (X2 bsl 40) + (X3 bsl 32) +
	(X4 bsl 24) + (X5 bsl 16) + (X6 bsl 8) + X7,
    if X0 >= 16#80 ->
	    {X - 16#10000000000000000, R};
       true  ->
	    {X, R}
    end.


%%-----------------------------------------------------------------
%% Func: enc_unsigned_longlong/2
%%-----------------------------------------------------------------
enc_unsigned_longlong(X, Message) when integer(X), X >= 0 ->
    enc_longlong(X, Message);
enc_unsigned_longlong(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_unsigned_longlong(~p); not integer >= 0.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: enc_r_unsigned_longlong/2
%%-----------------------------------------------------------------
enc_r_unsigned_longlong(X, Message) when integer(X), X >= 0 ->
    enc_r_longlong(X, Message);
enc_r_unsigned_longlong(X, Message) ->
    orber:debug_level_print("[~p] cdrlib:enc_r_unsigned_longlong(~p); not integer >= 0.", 
			    [?LINE, X], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=100, completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: dec_unsigned_longlong/2
%%-----------------------------------------------------------------
dec_unsigned_longlong([X7,X6,X5,X4,X3,X2,X1,X0 | R], big) ->
    {(X7 bsl 56) + (X6 bsl 48) + (X5 bsl 40) + (X4 bsl 32) +
     (X3 bsl 24) + (X2 bsl 16) + (X1 bsl 8) + X0, R};
dec_unsigned_longlong([X7,X6,X5,X4,X3,X2,X1,X0 | R],little) ->
    {(X0 bsl 56) + (X1 bsl 48) + (X2 bsl 40) + (X3 bsl 32) +
     (X4 bsl 24) + (X5 bsl 16) + (X6 bsl 8) + X7, R}.

%%%-----------------------------------------------------------------
%%% long double [S=1 | E=15 | F=112]
%%% X = (-1)^S * 2^(E-16383) * 1.F
%%%-----------------------------------------------------------------
%-define(LONGDOUBLE_BASE, 16#10000000000000000000000000000).
%-define(LONGDOUBLE_BIAS, 16383).
%%%-----------------------------------------------------------------
%%% Func: enc_longdouble/2
%%%-----------------------------------------------------------------
%enc_longdouble(X, Message) when number(X) ->
%    {S, E, F} = enc_ieee(X, ?LONGDOUBLE_BASE, ?LONGDOUBLE_BIAS),
%    [ (S bsl 7) bor ((E bsr 8) band 16#7f),
%     E band 16#ff,
%     (F bsr 104) band 16#ff,
%     (F bsr 96) band 16#ff,
%     (F bsr 88) band 16#ff,
%     (F bsr 80) band 16#ff,
%     (F bsr 72) band 16#ff,
%     (F bsr 64) band 16#ff,
%     (F bsr 56) band 16#ff,
%     (F bsr 48) band 16#ff,
%     (F bsr 40) band 16#ff,
%     (F bsr 32) band 16#ff,
%     (F bsr 24) band 16#ff,
%     (F bsr 16) band 16#ff,
%     (F bsr 8) band 16#ff,
%     F band 16#ff | Message];
%enc_longdouble(X, Message) ->
%    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%%-----------------------------------------------------------------
%%% Func: enc_r_longdouble/2
%%%-----------------------------------------------------------------
%enc_r_longdouble(X, Message) when number(X) ->
%    {S, E, F} = enc_ieee(X, ?LONGDOUBLE_BASE, ?LONGDOUBLE_BIAS),
%    [F band 16#ff,
%     (F bsr 8) band 16#ff,
%     (F bsr 16) band 16#ff,
%     (F bsr 24) band 16#ff,
%     (F bsr 32) band 16#ff,
%     (F bsr 40) band 16#ff,
%     (F bsr 48) band 16#ff,
%     (F bsr 56) band 16#ff,
%     (F bsr 64) band 16#ff,
%     (F bsr 72) band 16#ff,
%     (F bsr 80) band 16#ff,
%     (F bsr 88) band 16#ff,
%     (F bsr 96) band 16#ff,
%     (F bsr 104) band 16#ff,
%     E band 16#ff,
%     (S bsl 7) bor ((E bsr 8) band 16#7f) | Message];
%enc_r_longdouble(X, Message) ->
%    corba:raise(#'MARSHAL'{minor=101, completion_status=?COMPLETED_NO}).

%%%-----------------------------------------------------------------
%%% Func: dec_longdouble/2
%%%-----------------------------------------------------------------
%dec_longdouble([X15,X14,X13,X12,X11,X10,X9,X8,X7,X6,X5,X4,X3,X2,X1,X0 | R], big) ->

%    E = (X15 band 16#7f) bsl 8 + X14,

%    F = (X13 bsl 104) + (X12 bsl 96) + 
%	(X11 bsl 88) + (X10 bsl 80) + (X9 bsl 72) + 
%	(X8 bsl 64) + (X7 bsl 56) + (X6 bsl 48) + 
%	(X5 bsl 40) + (X4 bsl 32) + (X3 bsl 24) + 
%	(X2 bsl 16) + (X1 bsl 8) + X0,

%    if
%	E == 0, F == 0 -> 
%	    { 0.0, R};
%	X15 >= 16#80 ->
%	    { - math:pow(2, E-?LONGDOUBLE_BIAS) * (1 + F / ?LONGDOUBLE_BASE), R};
%	true ->
%	    { math:pow(2, E-?LONGDOUBLE_BIAS) * (1 + F / ?LONGDOUBLE_BASE), R}
%    end;
%dec_longdouble([X15,X14,X13,X12,X11,X10,X9,X8,X7,X6,X5,X4,X3,X2,X1,X0  | R], little) ->

%    E = (X0 band 16#7f) bsl 8 + X1,

%    F = 
%	(X2 bsl 104) + (X3 bsl 96) + 
%	(X4 bsl 88) + (X5 bsl 80) + (X6 bsl 72) +
%	(X7 bsl 64) + (X8 bsl 56) + (X9 bsl 48) +
%	(X10 bsl 40) + (X11 bsl 32) + (X12 bsl 24) +
%	(X13 bsl 16) + (X14 bsl 8) + X15,

%    if
%	E == 0, F == 0 -> 
%	    { 0.0, R};
%	X0 >= 16#80 ->
%	    { - math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R};
%	true ->
%	    { math:pow(2, E-?DOUBLE_BIAS) * (1 + F / ?DOUBLE_BASE), R}
%    end.

