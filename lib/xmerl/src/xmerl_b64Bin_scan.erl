%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%-------------------------------------------------------------------
%%% File    : xmerl_b64Bin_scan.erl
%%% Author  : Bertil Karlsson <bertil@finrod>
%%% Description : 
%%%
%%% Created : 30 Mar 2006 by Bertil Karlsson <bertil@finrod>
%%%-------------------------------------------------------------------
-module(xmerl_b64Bin_scan).

-export([scan/1]).

-define(L, 1).

scan(Str) ->
    scan(Str,[]).

scan([],Acc) ->
    lists:reverse([{'$end', ?L, '$end'}|Acc]);
scan(Str,Acc) ->
    case scan_token(Str) of
	{Token,Rest} ->
	    scan(Rest,[Token|Acc])
    end.

scan_token([$ ,H|T]) ->
    scan_token([H|T]);
scan_token([H|T]) when H==$A;H==$Q;H==$g;H==$w ->
    {{b04,?L,H},T};
scan_token([H|T]) 
  when H==$E;H==$I;H==$M;H==$U;H==$Y;H==$c;H==$k;H==$o;H==$s;H==$0;
       H==$4;H==$8 ->
    {{b16x,?L,H},T};
scan_token([H|T]) 
  when H==$B;H==$C;H==$D;H==$F;H==$G;H==$H;H==$J;H==$K;H==$L;H==$N;
       H==$O;H==$P;H==$R;H==$S;H==$T;H==$V;H==$W;H==$X;H==$Z ->
    {{b64x,?L,H},T};
scan_token([H|T])
  when H==$a;H==$b;H==$d;H==$e;H==$f;H==$h;H==$i;H==$j;H==$l;H==$m;H==$n;H==$p;
       H==$q;H==$r;H==$t;H==$u;H==$v;H==$x;H==$y;H==$z ->
    {{b64x,?L,H},T};
scan_token([H|T])
  when H==$1;H==$2;H==$3;H==$5;H==$6;H==$7;H==$9;H==$+;H==$/ ->
    {{b64x,?L,H},T};
scan_token("="++T) ->
    {{'=',?L,"="},T};
scan_token([H|_T]) ->
    exit({error,{base64Binary_scan_illegal_char,H}}).
