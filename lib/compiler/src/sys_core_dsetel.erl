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
%% Purpose : Using dsetelement to make multiple-field record updates
%% faster.

%% The expansion of record field updates, when more than one field is
%% updated, but not a majority of the fields, will create a sequence of
%% calls to 'erlang:setelement(Index, Value, Tuple)' where Tuple in the
%% first call is the original record tuple, and in the subsequent calls
%% Tuple is the result of the previous call. Furthermore, all Index
%% values are constant positive integers, and the first call to
%% 'setelement' will have the greatest index. Thus all the following
%% calls do not actually need to test at run-time whether Tuple has type
%% tuple, nor that the index is within the tuple bounds.
%%
%% Since this introduces destructive updates in the Core Erlang code, it
%% must be done as a last stage before going to lower-level code.
%%
%% NOTE: Because there are currently no write barriers in the system,
%% this kind of optimization can only be done when we are sure that
%% garbage collection will not be triggered between the creation of the
%% tuple and the destructive updates - otherwise we might insert
%% pointers from an older generation to a newer.
%%
%% The rewriting is done as follows:
%%
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in call 'erlang':'setelement(3, X1, Value2)
%%  =>
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X1, Value2)
%%       X1
%% and
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in let X2 = call 'erlang':'setelement(3, X1, Value2)
%%    in ...
%%  =>
%%    let X2 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop 'dsetelement(3, X2, Value2)
%%       ...
%% if X1 is used exactly once.
%% Thus, we need to track variable usage.

-module(sys_core_dsetel).

-export([module/2]).

-include("core_parse.hrl").

module(M, _Options) ->
    {M1, _} = visit(dict:new(), M),
    {ok, M1}.

visit(Env0, #c_var{name=X}=R) ->
    case dict:find(X, Env0) of
	{ok, N} ->
	    {R, dict:store(X, N+1, Env0)};
	error ->
	    {R, dict:store(X, 9999, Env0)}    % free variable - unsafe
    end;
visit(Env0, #c_tuple{es=Es0}=R) ->
    {Es1,Env1} = visit_list(Env0, Es0),
    {R#c_tuple{es=Es1}, Env1};
visit(Env0, #c_cons{hd=H0,tl=T0}=R) ->
    {H1,Env1} = visit(Env0, H0),
    {T1,Env2} = visit(Env1, T0),
    {R#c_cons{hd=H1,tl=T1}, Env2};
visit(Env0, #c_values{es=Es0}=R) ->
    {Es1,Env1} = visit_list(Env0, Es0),
    {R#c_values{es=Es1}, Env1};
visit(Env, #c_values{es=[E]}) ->
    visit(Env, E);
visit(Env0, #c_def{val=V0}=R) ->
    {V1,Env1} = visit(Env0, V0),
    {R#c_def{val=V1}, Env1};
visit(Env0, #c_fun{vars=Vs, body=B0}=R) ->
    {Xs, Env1} = bind_vars(Vs, Env0),
    {B1,Env2} = visit(Env1, B0),
    {R#c_fun{body=B1}, restore_vars(Xs, Env0, Env2)};
visit(Env0, #c_let{vars=Vs, arg=A0, body=B0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {Xs,Env2} = bind_vars(Vs, Env1),
    {B1,Env3} = visit(Env2, B0),
    rewrite(R#c_let{arg=A1,body=B1}, Env3, restore_vars(Xs, Env1, Env3));
visit(Env0, #c_seq{arg=A0, body=B0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {B1,Env2} = visit(Env1, B0),
    {R#c_seq{arg=A1,body=B1}, Env2};
visit(Env0, #c_case{arg=A0,clauses=Cs0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {Cs1,Env2} = visit_list(Env1, Cs0),
    {R#c_case{arg=A1,clauses=Cs1}, Env2};
visit(Env0, #c_clause{pats=Ps,guard=G0,body=B0}=R) ->
    {Vs, Env1} = visit_pats(Ps, Env0),
    {G1,Env2} = visit(Env1, G0),
    {B1,Env3} = visit(Env2, B0),
    {R#c_clause{guard=G1,body=B1}, restore_vars(Vs, Env0, Env3)};
visit(Env0, #c_receive{clauses=Cs0,timeout=T0,action=A0}=R) ->
    {T1,Env1} = visit(Env0, T0),
    {Cs1,Env2} = visit_list(Env1, Cs0),
    {A1,Env3} = visit(Env2, A0),
    {R#c_receive{clauses=Cs1,timeout=T1,action=A1}, Env3};
visit(Env0, #c_apply{op=Op0, args=As0}=R) ->
    {Op1,Env1} = visit(Env0, Op0),
    {As1,Env2} = visit_list(Env1, As0),
    {R#c_apply{op=Op1,args=As1}, Env2};
visit(Env0, #c_call{module=M0,name=N0,args=As0}=R) ->
    {M1,Env1} = visit(Env0, M0),
    {N1,Env2} = visit(Env1, N0),
    {As1,Env3} = visit_list(Env2, As0),
    {R#c_call{module=M1,name=N1,args=As1}, Env3};
visit(Env0, #c_primop{name=N0, args=As0}=R) ->
    {N1,Env1} = visit(Env0, N0),
    {As1,Env2} = visit_list(Env1, As0),
    {R#c_primop{name=N1,args=As1}, Env2};
visit(Env0, #c_try{arg=E0, vars=Vs, body=B0, evars=Evs, handler=H0}=R) ->
    {E1,Env1} = visit(Env0, E0),
    {Xs, Env2} = bind_vars(Vs, Env1),
    {B1,Env3} = visit(Env2, B0),
    Env4 = restore_vars(Xs, Env1, Env3),
    {Ys, Env5} = bind_vars(Evs, Env4),
    {H1,Env6} = visit(Env5, H0),
    {R#c_try{arg=E1,body=B1,handler=H1}, restore_vars(Ys, Env4, Env6)};
visit(Env0, #c_catch{body=B0}=R) ->
    {B1,Env1} = visit(Env0, B0),
    {R#c_catch{body=B1}, Env1};
visit(Env0, #c_letrec{defs=Ds0,body=B0}=R) ->
    {Xs, Env1} = bind_vars([V || #c_def{name=V} <- Ds0], Env0),
    {Ds1,Env2} = visit_list(Env1, Ds0),
    {B1,Env3} = visit(Env2, B0),
    {R#c_letrec{defs=Ds1,body=B1}, restore_vars(Xs, Env0, Env3)};
visit(Env0, #c_module{defs=D0}=R) ->
    {R1,Env1} = visit(Env0, #c_letrec{defs=D0,body=#c_nil{}}),
    {R#c_module{defs=R1#c_letrec.defs}, Env1};
visit(Env, T) ->    % constants
	{T, Env}.

visit_list(Env, L) ->
    lists:mapfoldl(fun (E, A) -> visit(A, E) end, Env, L).

bind_vars(Vs, Env) ->
    bind_vars(Vs, Env, []).

bind_vars([#c_var{name=X}|Vs], Env0, Xs)->
    bind_vars(Vs, dict:store(X, 0, Env0), [X|Xs]);
bind_vars([#c_fname{id=N,arity=A}|Vs], Env0, Xs)->
    X = {N,A},
    bind_vars(Vs, dict:store(X, 0, Env0), [X|Xs]);
bind_vars([], Env,Xs) ->
    {Xs, Env}.

visit_pats(Ps, Env) ->
    visit_pats(Ps, Env, []).

visit_pats([P|Ps], Env0, Vs0) ->
    {Vs1, Env1} = visit_pat(Env0, P, Vs0),
    visit_pats(Ps, Env1, Vs1);
visit_pats([], Env, Vs) ->
    {Vs, Env}.

visit_pat(Env0, #c_var{name=V}, Vs) ->
    {[V|Vs], dict:store(V, 0, Env0)};
visit_pat(Env0, #c_tuple{es=Es}, Vs) ->
    visit_pats(Es, Env0, Vs);
visit_pat(Env0, #c_cons{hd=H,tl=T}, Vs0) ->
    {Vs1, Env1} = visit_pat(Env0, H, Vs0),
    visit_pat(Env1, T, Vs1);
visit_pat(Env0, #c_alias{pat=P,var=#c_var{name=V}}, Vs) ->
    visit_pat(dict:store(V, 0, Env0), P, [V|Vs]);
visit_pat(Env, _, Vs) ->    % constants
    {Vs, Env}.

restore_vars([V|Vs], Env0, Env1) ->
    case dict:find(V, Env0) of
	{ok, N} ->
	    restore_vars(Vs, Env0, dict:store(V, N, Env1));
	error ->
	    restore_vars(Vs, Env0, dict:erase(V, Env1))
    end;
restore_vars([], _, Env1) ->
    Env1.


%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in call 'erlang':'setelement(3, X1, Value2)
%%  =>
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X1, Value2)
%%       X1

rewrite(#c_let{vars=[#c_var{name=X}=V]=Vs,
	       arg=#c_call{module=#c_atom{val='erlang'},
			   name=#c_atom{val='setelement'},
			   args=[#c_int{val=Index1}, _Tuple, _Val1]
			  }=A,
 	       body=#c_call{anno=Banno,module=#c_atom{val='erlang'},
 			    name=#c_atom{val='setelement'},
  			    args=[#c_int{val=Index2},
				  #c_var{name=X},
				  Val2]
  			   }
	      }=R,
	_BodyEnv, FinalEnv)
  when integer(Index1), integer(Index2), Index2 > 0, Index1 > Index2 ->
    case is_safe(Val2) of
	true ->
	    {R#c_let{vars=Vs,
		     arg=A,
		     body=#c_seq{arg=#c_primop{
				   anno=Banno,
				   name=#c_atom{val='dsetelement'},
				   args=[#c_int{val=Index2},
					 V,
					 Val2]},
				 body=V}
		    },
	     FinalEnv};
	false ->
	    {R, FinalEnv}
    end;

%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in let X2 = 'erlang':'setelement(3, X1, Value2)
%%    in ...
%%  =>
%%    let X2 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X2, Value2)
%%       ...
%% if X1 is used exactly once.

rewrite(#c_let{vars=[#c_var{name=X1}],
	       arg=#c_call{module=#c_atom{val='erlang'},
			   name=#c_atom{val='setelement'},
			   args=[#c_int{val=Index1}, _Tuple, _Val1]
			  }=A,
	       body=#c_let{vars=[#c_var{}=V]=Vs,
			   arg=#c_call{anno=Banno,
				       module=#c_atom{val='erlang'},
				       name=#c_atom{val='setelement'},
				       args=[#c_int{val=Index2},
					     #c_var{name=X1},
					     Val2]},
			   body=B}
	      }=R,
	BodyEnv, FinalEnv)
  when integer(Index1), integer(Index2), Index2 > 0, Index1 > Index2 ->
    case is_single_use(X1, BodyEnv) andalso is_safe(Val2) of
	true ->
	    {R#c_let{vars=Vs,
		     arg=A,
		     body=#c_seq{arg=#c_primop{
				   anno=Banno,
				   name=#c_atom{val='dsetelement'},
				   args=[#c_int{val=Index2},
					 V,
					 Val2]},
				 body=B}
		    },
	     FinalEnv};
	false ->
	    {R, FinalEnv}
    end;

rewrite(R, _, FinalEnv) ->
    {R, FinalEnv}.

is_safe(#c_var{}) -> true;
is_safe(#c_fname{}) -> true;
is_safe(#c_int{}) -> true;
is_safe(#c_float{}) -> true;
is_safe(#c_atom{}) -> true;
is_safe(#c_char{}) -> true;
is_safe(#c_string{}) -> true;
is_safe(#c_nil{}) -> true;
is_safe(_) -> false.

is_single_use(V, Env) ->
    case dict:find(V, Env) of
	{ok, 1} ->
	    true;
	_ ->
	    false
    end.
