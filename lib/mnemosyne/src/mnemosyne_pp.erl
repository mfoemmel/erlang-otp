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
-module(mnemosyne_pp).

%% Purpose : Pretty-printer for Amnesia Query Evaluation

-export([body/1, e/1, rule/1]).

-include("mnemosyne_internal_form.hrl").


%%%----------------------------------------------------------------
%%% 		Exports

e(X) -> lists:flatten(x(X)).

rule(R) -> rule(R," ").

rule({Head,Body}, End) ->
    lists:flatten([x(Head), " :-", End, pplist(Body,[",",End]), "."]).

body(Body) when is_record(Body,optimizer_result) ->
    if 
	Body#optimizer_result.common_bs==[] ->
	    body(Body#optimizer_result.code);
	true ->
	    body([{'#bindings',1,Body#optimizer_result.common_bs} |
		  Body#optimizer_result.code])
    end;
body(Body) when is_list(Body) ->
    B = 
	case Body of
	    [{'#bindings',BCount,Bs},C|Cs] when is_record(C,disj_alt) ->
		[{'#bindings',BCount,Bs},{'#or',1,[C|Cs]}];
	    [C|Cs] when is_record(C,disj_alt) ->
		[{'#or',1,[C|Cs]}];
	    Others ->
		Others
	end,
    lists:flatten( pplist(B) );
body(X) ->
    e(X).

%%%----------------------------------------------------------------
%%% 		Private

x(C) when is_record(C,constraint) ->
    [x(C#constraint.exprL)," ",
     op(C#constraint.op,constraint)," ",
     x(C#constraint.exprR)];
x({'#var', V}) -> v(V);
x(R) when is_record(R,rec_f) ->
    [x(R#rec_f.var), ".", atom_to_list(R#rec_f.field)];
x(R) when is_record(R,rec_c) ->
    ["#{", rfs(R#rec_c.fields), "}"];
x(R) when is_record(R,pred_sym) ->
    case R#pred_sym.args of
	[] -> functor(R);
	_  -> [functor(R), "(", args(R#pred_sym.args), ")"]
    end;
x({'#or',_,[Alt]})  -> body(Alt);
x({'#or',_,Alts})   ->  ["( ", disj(Alts), " )"];
x({'#not',C,Gs}) when is_list(Gs) -> ["~(", x(Gs), ")"];
x({'#not',C,G}) -> ["~", x(G)];
x(D) when is_record(D,disj_alt) -> da(D);
x({'#bindings',C,Bs}) -> ["<:",bs(Bs),":>"];
x({'#line',Line}) -> l(Line);
x({'#funcall',M,F,Args}) -> [x(M),":",x(F),"(",pplist(Args),")"];
x(F) when is_record(F,fn) ->[x(F#fn.alias_var),":=",x(F#fn.fndef)];
x(L) when is_list(L) ->  ["[",pplist(L),"]"];
x(T) when is_tuple(T) -> ["{",pplist(tuple_to_list(T)),"}"];
x(X) -> w(X).

v(V) when is_atom(V) -> atom_to_list(V);
v(R) when is_reference(R) -> ["Gen#",get_num(R)];
v({A,R}) when is_reference(R) -> [v(A),"#",get_num(R)];
v({A,B}) -> [v(A),"__",v(B)];
v(X) -> w(X).

l(Line) ->
    case Line of
	{File,LineNum} ->
	    io_lib:format("~s:~w", [File,LineNum]);
	LineNum when is_integer(LineNum) ->
	    io_lib:format("~w", [LineNum])
    end.


rfs([{Name,Value}|T]) -> 
    [atom_to_list(Name),"=",x(Value) |
     if  length(T)>0 -> [", " | rfs(T)];
	 true -> []
     end];
rfs([{Name,Line,Value}|T]) -> 
    [atom_to_list(Name),"=",x(Value) |
     if  length(T)>0 -> [", " | rfs(T)];
	 true -> []
     end];
rfs([]) -> 
    [].

disj([D1,D2|Ds]) -> [body(D1), " ; ", disj([D2|Ds])];
disj([D]) -> body(D);
disj([]) -> "".

pplist(L) -> pplist(L,",").

pplist([H1,H2|T],Sep) -> [x(H1),Sep|pplist([H2|T],Sep)];
pplist([H],Sep) -> x(H);
pplist([H|T],Sep) -> [x(H),"|",x(T)];
pplist([],Sep) -> [].

args(As) -> pplist(As, ", ").

functor(R) when is_record(R,pred_sym) ->
    F = R#pred_sym.functor,
    case R#pred_sym.module of
	?NO_MODULE -> atom_to_list(F);
	M ->  [atom_to_list(M),":",atom_to_list(F)]
    end.

get_num(R) ->
    Key = {ref_num,R},
    integer_to_list(
      case get(Key) of
	  N when is_integer(N) -> 
	      N;
	  undefined ->  
	      I =
		  case get(ref_num_seed) of
		      undefined -> 0;
		      N when is_integer(N) -> N+1
		  end,
	      put(ref_num_seed,I),
	      put(Key,I), I
      end
     ).

op(Op,_) when is_atom(Op) -> atom_to_list(Op).

w(X) -> io_lib:write(X).

%%%----------------------------------------------------------------
bs(Bs) -> 
    case catch [bs1(mnemosyne_unify:bindings_to_list(Bs))] of
	L when is_list(L) -> L;
	_ -> ["??bs: ", w(Bs)]
    end.

bs1([{Var,B}|Bs]) ->
    [case B of
	 {'#value',Val} -> [x({'#var',Var}), "/", x(Val)];
	 {'#bind_trigger',Triggers} -> ts(Triggers, x({'#var',Var}));
	 {'#var',Var2} -> [x({'#var',Var}), "/", x({'#var',Var2})]
     end
     | if  Bs==[] -> [];
	   true -> [", " | bs1(Bs)]
       end
    ];
bs1([]) ->
    [].


ts([{M,F,A}|Ts], V) ->
    [V,"/",w(M),":",w(F),"(",args(A),")" |
     if Ts==[] -> [];
	 true -> [", " | ts(Ts,V)]
     end
    ];
ts([],V) ->
    [];

ts(Ts,V) ->
    [V,":",w(Ts)].

%%%----------------------------------------------------------------

da(D) when is_record(D,disj_alt) ->
    L0 = da(conj, D#disj_alt.conj, []),
    L1 = da(bs, D#disj_alt.bs, L0),
    L2 = da(alias, D#disj_alt.alias, L1),
    L3 = da(constraints, D#disj_alt.constraints, L2),
    da(fns, D#disj_alt.fncalls, L3).

da(What, [], Acc) -> Acc;
da(conj, L, Acc) -> [body(L),Acc];
da(What, L, Acc) -> ["<",atom_to_list(What),fmt(What,L),"> " | Acc].

fmt(bs, L) -> ["[",bs(L),"]"];
fmt(_, L) -> x(L).
