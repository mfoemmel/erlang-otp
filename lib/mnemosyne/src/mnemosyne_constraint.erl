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
-module(mnemosyne_constraint).
-export([invert_constraint/1,
	 check/2]).

%%-define(debug,1).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%----------------------------------------------------------------
%%% 		Exports

%%% called by the unifier when a variable is bound.
%%% Returns  remove_trigger | keep_trigger | throw(fail)
%%% Process dict when_called = compile_time | undefined

check(Bindings, Constraint) ->
    C = mnemosyne_unify:instantiate(Constraint,Bindings),
    check(get(when_called), 
	  C#constraint.exprL, 
	  C#constraint.op, 
	  C#constraint.exprR).
    
invert_constraint(C) ->
    C#constraint{op=inv_op(C#constraint.op)}.

%%%----------------------------------------------------------------
%%% 		Private

check(WhenCalled, ExprL, Op, ExprR) ->
    Result = (catch eval(WhenCalled, {'#expr',
				      eval(WhenCalled, ExprL),
				      Op,
				      eval(WhenCalled, ExprR)})),
    ?debugmsg(1, "~s ~s ~s : ~w\n", [mnemosyne_pp:e(ExprL),
				     Op,
				     mnemosyne_pp:e(ExprR),
				     Result]),
    case Result of
	true ->  remove_trigger;
	cannot_evaluate -> keep_trigger;
	fail -> throw(fail)
    end.
    

%%% eval assumes that all sub expressions are evaluated. That implies
%%% that the arguments are ground and contains no function calls.

eval(WhenCalled, {'#expr',L,Op,R}) ->
    case Op of
	'+' -> L+R;
	'-' -> L-R;
	'*' -> L*R;
	'/' -> L/R;
	'<'   when L <  R -> true;
	'=<'  when L =< R -> true;
	'>'   when L >  R -> true;
	'>='  when L >= R -> true;
	'=='  when L == R -> true;
	'=/=' when L =/= R-> true;
	'='   when L == R -> true;
	'!='  when L =/= R-> true;
	_  -> throw(fail)
    end;

eval(WhenCalled, {'#expr',Op,E}) -> 
    case Op of
	'-' -> - eval(WhenCalled, E);
	'+' -> eval(WhenCalled, E)
    end;

eval(WhenCalled, {'#var',V})  -> throw(cannot_evaluate);

eval(WhenCalled, {'#value',V}) -> V;

eval(compile_time, {'#funcall',M,F,Args}) -> throw(cannot_evaluate);
eval(W, {'#funcall',M,F,Args}) -> apply(eval(W,M),
					eval(W,F),
					eval(W,Args));

eval(WhenCalled, L) when list(L) ->
    lists:map(fun(E) -> eval(WhenCalled,E) end, L);

eval(WhenCalled, T) when tuple(T) ->
    list_to_tuple(eval(WhenCalled,tuple_to_list(T)));

eval(WhenCalled, V) -> V.


%%%----------------------------------------------------------------

inv_op('<')  -> '>=' ;
inv_op('=<') -> '>'  ;
inv_op('>')  -> '=<' ;
inv_op('>=') -> '<'  ;
inv_op('==') -> '=/=';
inv_op('=/=')-> '==' ;
inv_op('=')  -> '!=' ;
inv_op('!=') -> '='  .

