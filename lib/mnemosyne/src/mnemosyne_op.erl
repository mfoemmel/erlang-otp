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
-module(mnemosyne_op).
-export([bindings/3, funcall/4, erl_expr/2, split/2, table/2, negation/3,
	 negation_help/0, call_recursive_op/2,
	 split_list/2, get_from_table/3, new_end_token/1, start_op/2,
	 add_tid/5, update_endc/3
	]).

%%-define(debug,3).
-define(no_not_yet, yes).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

-record(acc, {sols=[], bss=[]}).

-ifdef(debug).
-define(trace_printout(Type,Args,Where),
	case Type of
	    negation_help when Where==1 ->
		wait_a_bit;
	    _ ->
		?debugmsg(1, "~w (~w) -> ~w. Args: ~s\n", 
			  [self(), Type, hd(Args),
			   type_dependent_args(Type, Args)])
	end).

type_dependent_args(bindings, [_,_,Bs]) ->
    [" ",mnemosyne_pp:e({'#bindings',1,Bs})];

type_dependent_args(table, [_,P]) when ?debug==2 ->
    [" line=",mnemosyne_pp:e({'#line',P#pred_sym.line}),
     " p=",mnemosyne_pp:e(P#pred_sym.pattern),
     " bp=",mnemosyne_pp:e(P#pred_sym.back_pattern),
     " idx=",io_lib:write(P#pred_sym.idx_method)
    ];

type_dependent_args(table, [_,P]) when ?debug>2 ->
    [" line=",mnemosyne_pp:e({'#line',P#pred_sym.line}),
     " goal=",mnemosyne_pp:e(P),
     " p=",mnemosyne_pp:e(P#pred_sym.pattern),
     " bp=",mnemosyne_pp:e(P#pred_sym.back_pattern),
     " idx=",io_lib:write(P#pred_sym.idx_method),
     " defvars=",io_lib:write(P#pred_sym.defvars)
    ];

type_dependent_args(negation, [_,HelpPid]) ->
    [" help=",io_lib:write(HelpPid)
    ];

type_dependent_args(negation_help, [Qpid,MainPid]) ->
    [" QueryPid=",io_lib:write(Qpid),
     " MainPid=",io_lib:write(MainPid)
    ];

type_dependent_args(Type, Args) ->
    [" ",mnemosyne_pp:e(Args)].

-else.
-define(trace_printout(Type,Args,Where),ok).
-endif.

%%%================================================================
%%% 		Exports

start_op(Type, Args) ->
    ?trace_printout(Type, Args, 1),
    %% The Type(Args) call never returns unless there is an exception:
    case catch apply(?MODULE, Type, Args) of
	{'EXIT',Cause} -> exit(Cause);
	Others -> exit({throw,Others})
    end.

new_end_token(N) ->
    {{make_ref(),erlang:now()}, N}.

%%%----------------------------------------------------------------
%%%---- Adds bindings to a set of bindings

bindings(NextPid, BCount,Bs) ->
    bindings(NextPid, BCount,Bs, Bs, {[],[],[]}).

bindings(NextPid, BCount,Bs, BsOrig, {Tid, PidL, EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} when Bss =/= [] ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),
	    case check_lastmarker (NewEndC, BCount, LastMarker) of
		true ->
		    NextPid ! {bss, bss_union(Bss,Bs), Max, EndCntrl, Stack, {true, self()}};
		_ ->
		    NextPid ! {bss, bss_union(Bss,Bs), Max, EndCntrl, Stack, false}
	    end,
	    bindings(NextPid, BCount, [], BsOrig, {Tid, PidL, NewEndC});

	{bss, _, Max, EndCntrl, Stack, LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),
	    case check_lastmarker (NewEndC, BCount, LastMarker) of
		true ->
		    NextPid ! {bss, [], Max, EndCntrl, Stack, {true, self()}};
		_ ->
		    NextPid ! {bss, [], Max, EndCntrl, Stack, false}
	    end,
	    bindings(NextPid, BCount, [], BsOrig, {Tid, PidL, NewEndC});
	
	{fail, SomePid, Cause} ->
	    NextPid ! {fail, self(), Cause},
	    bindings(NextPid, BCount, BsOrig);

	{tid, NTid, Pid} ->
	    NextPid ! {tid,Tid},
	    bindings(NextPid, BCount,Bs, BsOrig, add_tid (Tid, PidL, EndC, NTid, Pid))
    end.



%%%----------------------------------------------------------------
%%%---- Call an Erlang functi n

funcall (NextPid, LeftSide, FunCall, Count) ->
    funcall_loop (NextPid, LeftSide, FunCall, Count, {[], [], []}).

funcall_loop(NextPid, LeftSide, FunCall, Count, {Tid, PidL, EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),

	    case eval_funcall(Bss,LeftSide,FunCall,[]) of
		{fail, Cause} ->
		    NextPid ! {fail, self(), Cause};
		{ok, NewBss} ->
		    ?debugmsg(3,"~w: ~w ! ~w\n", 
			      [self(), NextPid, {bss, NewBss, Max, EndCntrl, Stack}]),
		    case check_lastmarker (NewEndC, Count, LastMarker) of
			true ->
			    NextPid ! {bss, NewBss, Max, EndCntrl, Stack, 
				       {true, self ()}};
			_ ->
			    NextPid ! {bss, NewBss, Max, EndCntrl, Stack, 
				       false}
		    end
	    end,
	    funcall_loop (NextPid, LeftSide, FunCall, Count, {Tid, PidL, NewEndC});
	
	{fail, SomePid, Cause} ->
	    NextPid ! {fail, self(), Cause},
	    funcall (NextPid, LeftSide, FunCall, Count);

	{tid, NTid, Pid} ->
	    NextPid ! {tid,NTid, self()},
	    funcall_loop(NextPid, LeftSide, FunCall, Count,  
			 add_tid (Tid, PidL, EndC, NTid, Pid))
    end.


	
eval_funcall([Bs|Bss], LeftSide, FunCall, Acc) ->
    {'#funcall',M,F,Args} = mnemosyne_unify:instantiate(FunCall,Bs),
    Ref = make_ref(),
    case catch {Ref, apply(M,F,Args)} of
	{Ref, Res} ->
	    case mnemosyne_unify:unify(LeftSide, apply(M,F,Args), Bs) of
		fail ->
		    eval_funcall(Bss, LeftSide, FunCall, Acc);
		NewBs ->
		    eval_funcall(Bss, LeftSide, FunCall, [NewBs|Acc])
	    end;
	{'EXIT', Cause} ->
	    {fail, Cause}; 
	Other ->
	    {fail, {throw, Other}}
    end;
    
eval_funcall([], LeftSide, FunCall, Acc) ->
    {ok, Acc}.
    

%%%----------------------------------------------------------------
%%%---- 

erl_expr(NextPid, Expr) ->
    erl_expr(NextPid, Expr, [], {[],[],[]}).

erl_expr(NextPid, Expr, Sols, {Tid, PidL, EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),

	    {First, Last} = 
		split_list(Max, get_values(Bss,Expr,Sols)),
	    
	    case {Last, check_lastmarker(NewEndC, Expr#erl_expr.rec_count, LastMarker)} of
		{[], true}  ->
		    NextPid ! {bss, First, Max, EndCntrl, Stack, {true, self ()}};
		_ ->
		    NextPid ! {bss, First, Max, EndCntrl, Stack,  false}
	    end,
	    erl_expr(NextPid, Expr, Last, {Tid, PidL, NewEndC});

	{fail, SomePid, Cause} ->
	    NextPid ! {fail, self(), Cause},
	    erl_expr (NextPid, Expr);

	{tid, NTid, Pid} ->
	    NextPid ! {tid, NTid, self()},
	    erl_expr(NextPid, Expr, [], add_tid (Tid, PidL, EndC, NTid, Pid))

    end.

get_values(Bss, Expr, Acc0) ->
    lists:foldl(
      fun(Bs,Acc) ->
	      ?debugmsg(2,"~w: Value = ~w\n",
			[self(),
			 mnemosyne_unify:instantiate(Expr#erl_expr.expr,Bs)]),
%	      case mnemosyne_unify:unify(
%		     mnemosyne_unify:instantiate(Expr#erl_expr.expr,Bs),
%		     Expr#erl_expr.alias_var,
%		     Bs) of
%		  fail -> Acc;
%		  NewBs -> [NewBs|Acc]
%	      end
	      unify_list(mnemosyne_unify:instantiate(Expr#erl_expr.expr,Bs),
			 Expr#erl_expr.alias_var, Bs, Acc)
      end, Acc0, Bss).

%%%----------------------------------------------------------------
%%%---- Multiplexes a message to many receivers

split (NextPids, Count) ->
    split_loop (NextPids, Count, {[],[],[]}).

split_loop(NextPids, Count, {Tid, PidL,EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),

	    NewEnd = new_end_token(length(NextPids)),
	    case check_lastmarker (NewEndC, Count, LastMarker) of
		true ->
		    hd(NextPids) ! {bss, Bss, Max, [NewEnd|EndCntrl], 
				    Stack, {true, self ()}},
		    send(tl(NextPids), {bss, Bss, Max, [NewEnd], 
					Stack, {true, self ()}});
		_ ->
		    hd(NextPids) ! {bss, Bss, Max, [NewEnd|EndCntrl], 
				    Stack, false},
		    send(tl(NextPids), {bss, Bss, Max, [NewEnd], 
					Stack, false})
	    end,
	    split_loop (NextPids, Count, {Tid, PidL, NewEndC});

	{fail, SomePid, Cause} ->
	    send(NextPids, {fail, self(), Cause}),
	    split (NextPids, Count);
	
	{tid, NTid,Pid} ->
	    send(NextPids, {tid,NTid, self ()}),
	    split_loop (NextPids, Count, add_tid (Tid, PidL, EndC, NTid, Pid))
    end.


%%%----------------------------------------------------------------
%%%---- Table lookup

table(NextPid, P) when record(P,pred_sym) ->
    table_loop(NextPid, P, #acc{}, {[], [], []}).


%%%----------------------------------------------------------------
%%%---- Negation

negation(NextPid, Count,HelpPid) ->
    negation(NextPid, Count,HelpPid, [], {[], [], []}).


negation_help() ->
    receive
	{pids, Buddie, NextPid} ->
	    ?trace_printout(negation_help, [NextPid,Buddie], 2),
	    negation_help(NextPid,Buddie,[], {[],[], []})
    end.

%%%----------------------------------------------------------------
%%---- Recursion
%% A first solution. For each binding, get ALL solutions from SLG and
%% THEN pass them on to the DestPid.

call_recursive_op(NextPid, Goal) ->
    call_recursive_op(NextPid, Goal, [], [], {[],[], []}).

call_recursive_op(NextPid, Goal, BssAcc, Acc, {Tid, PidL, EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),

	    {BssAcc1, SolutionList} =
		get_from_slg(Max, Acc, lists:append(BssAcc,Bss), Goal),
	    {First,Acc1} = split_list(Max, SolutionList),
	    case {Acc1, check_lastmarker (NewEndC, Goal#pred_sym.rec_count, LastMarker)} of
		{[], true} ->
		    NextPid ! {bss, First, Max, EndCntrl, Stack, 
			       {true, self ()}};
		_ ->
		    NextPid ! {bss, First, Max, EndCntrl, Stack, 
			       false}
	    end,
	    call_recursive_op(NextPid, Goal, BssAcc1, Acc1, 
			      {Tid, PidL, NewEndC});

	{fail, SomePid, Cause} ->
	    NextPid ! {fail, self(), Cause},
	    call_recursive_op(NextPid, Goal);

	{tid, NTid, Pid} ->
	    mnesia:put_activity_id(NTid),
	    NextPid ! {tid,NTid, self()},
	    call_recursive_op(NextPid, Goal, [], [], 
			      add_tid (Tid,PidL,EndC, NTid,Pid))
    end.

    

get_from_slg(Max, Acc, [Bs|Bss], Goal) when length(Acc)<Max ->
    GoalInst = mnemosyne_unify:instantiate(Goal,Bs),
    mnemosyne_slg:slg_init(),
    Acc1 = make_bindings(mnemosyne_slg:slg(GoalInst,none), Goal, Bs, Acc),
    get_from_slg(Max, Acc1, Bss, Goal);
get_from_slg(Max, Acc, Bss, Goal) ->
    {Bss, Acc}.


make_bindings([Answ|Answs], Goal, Bs, Acc) ->
    case mnemosyne_unify:unify(Answ,Goal,Bs) of
	fail -> make_bindings(Answs, Goal, Bs, Acc);
	NewBs -> make_bindings(Answs, Goal, Bs, [NewBs|Acc])
    end;
make_bindings([], Goal, Bs, Acc) ->
    Acc.


%%%================================================================
%%% 		Private

table_loop(NextPid, F, Acc, {Tid, PidL, EndC}) ->
    receive
	{bss, Bss, Max, EndCntrl, Stack, LastMarker} ->
?debugmsg(3,"~w got ~w\n", [self(), {bss, Bss, Max, EndCntrl, Stack}]),
	    NewEndC = update_endc (PidL, EndC, LastMarker),

	    List = lists:append(Acc#acc.bss,Bss),

	    case catch fact_produce(Max, Acc#acc{bss = List}, F) of
		{'EXIT', Cause} ->
		    NextPid ! {fail, self(), Cause},
		    table(NextPid, F);
		Acc1 ->
		    {First, Sols} = split_list(Max, Acc1#acc.sols),
		    case {Sols, check_lastmarker(NewEndC, F#pred_sym.rec_count, LastMarker)} of
			{[], true} ->
			    NextPid ! {bss, First, Max, EndCntrl, Stack, 
				       {true, self()}};
			_ ->
			    NextPid ! {bss, First, Max, EndCntrl, Stack, 
				       false}
		    end,
		    ?debugmsg(3,"~w: ~w ! ~w\n", 
			      [self(),NextPid, {bss, First, Max, EndCntrl, Stack}]),
		    table_loop(NextPid, F, Acc1#acc{sols = Sols}, 
			       {Tid, PidL, NewEndC})
	    end;
	
	{fail, SomePid, Cause} ->
	    NextPid ! {fail, self(), Cause},
	    table (NextPid, F);
	
	{tid, NTid, Pid} ->
	    mnesia:put_activity_id(NTid),
	    NextPid ! {tid,NTid, self ()},
	    table_loop(NextPid, F, #acc{}, add_tid(Tid,PidL,EndC,NTid,Pid))
    end.


%% add_tid called because a '{tid, Tid,Pid}' message arrived.
%% Tid is the new tid to use. If that Tid is a new one
%% it is a new transaction.
%% If it the same Tid as before then either we got it before
%% from another Pid (sender) --> add Pid to reference list
%% or we got it from a pid we already have it from
%% --> restart of question (using same pid)

%% Comments from dgud: The earlier version used a counter
%% to count the number of answers, due to a bug where the 
%% complicated loop failed(never returned)  when there was 
%% more than 1000 (i.e. prefetched) answers,
%% I rewrote it to use a decreasing PidList instead. So the 
%% various Count(s) variables is not used anymore.
%% This might be wrong I'm not 100% sure what I'm doing but 
%% it works much better now.

add_tid (Tid, PidL, EndC, Tid, Pid) ->
    case lists:member (Pid, PidL) of
	true ->		      % restart of trans with same tid
	    {Tid, [Pid], [Pid]};
	false ->	      % add one more pid to collect lastmarkers from
	    {Tid, [Pid | PidL], [Pid | PidL]}
    end;
add_tid (_, _, _, NTid, Pid) ->  % make new transaction
    {NTid, [Pid], [Pid]}.
    
update_endc (TidL, EndC, false) ->
    EndC;
update_endc (TidL, EndC, {true, Pid}) ->
    case lists:member(Pid, TidL) of
	true ->
	    lists:delete(Pid, EndC);
	_ ->   
	    EndC
    end.
check_lastmarker ([],BC,{true,_}) -> true;
check_lastmarker (AC,BC,Last) -> false.

%%%----------------
fact_produce(Max, Acc, F) ->
    {Sols,Bss} = fact_produce(Max, Acc#acc.sols, Acc#acc.bss, F),
    Acc#acc{bss=Bss, sols=Sols}.
    
fact_produce(Max, Sols, [Bs|Bss], F) when length(Sols)<Max ->
    fact_produce(Max, get_from_table(Bs,F,Sols), Bss, F);
fact_produce(Max, Sols, Bss, F) ->
    {Sols,Bss}.

get_from_table(Bs, F, Sols) ->
    InstPattern = mnemosyne_unify:instantiate(F#pred_sym.pattern, Bs),
    Table = F#pred_sym.functor,
    Matches = 
	case F#pred_sym.idx_method of
	    {dyn_idx,Pos} ->
		?not_yet('get_from_table: dyn_idx'),
		mnesia:match_object(Table, InstPattern, read);
	    
	    {stat_idx,Pos} ->
		mnesia:index_match_object(Table, InstPattern, Pos, read);

	    key ->
		mnesia:match_object(Table, InstPattern, read);

	    no_idx ->				% Occurs if no statistc optim
		mnesia:match_object(Table, InstPattern, read)
	end,
    NewBss = unify_list(Matches, F#pred_sym.back_pattern, Bs, Sols),
    ?debugmsg(4, "NewBss = ~s\n", 
	      [mnemosyne_pp:e(lists:map(fun(B)->{'#bindings',B} end,NewBss))]),
    NewBss.


%%%----------------------------------------------------------------
negation(NextPid, Count,HelpPid, Acc, {Tid,PidL,EndC}) ->
    receive
	%%---- From the ordinary flow
	{bss, Bss, Max, EndCntrl, Stack,LastMarker} ->
	    NewEndC = update_endc (PidL, EndC, LastMarker),
	    case check_lastmarker (NewEndC, Count, LastMarker) of
		true ->
		    HelpPid ! 
			{neg_bss_req, Bss, Max, EndCntrl, Stack, 
			 {true, self()}};
		_ ->
		    HelpPid ! 
			{neg_bss_req, Bss, Max, EndCntrl, Stack, 
			 {false, self()}}
	    end,
	    negation(NextPid, Count,HelpPid, Acc, {Tid,PidL,NewEndC});
		
	{fail, SomePid, Cause} ->
	    HelpPid ! {fail, self(), Cause},
	    NextPid ! {fail, self(), Cause},
	    negation(NextPid, Count, HelpPid);
	
	{tid, NTid,Pid} ->
	    HelpPid ! {tid_req, NTid, self()},
	    NextPid ! {tid, NTid, self()},
	    negation(NextPid, Count, HelpPid, [], add_tid(Tid,PidL,EndC,NTid,Pid));
	
	%%---- From the help process
	{neg_bss_repl, Bss, Max, EndCntrl, Stack, LastMarker} ->
	    NewLM = 
		case LastMarker of
		    {true, _} ->
			{true, self ()};
		    _ ->
			false
		end,
	    {First,Last} = split_list(Max, lists:append(Acc,Bss)),
	    NextPid ! {bss, First, Max, EndCntrl, Stack,NewLM},
	    negation(NextPid, Count, HelpPid, Last, {Tid,PidL,EndC})
	
    end.

    
negation_help(NextPid, Buddie, Acc0, {Tid,PidL, EndC}) ->
    receive
	%% Requests from the negation process ("Buddie")
	{neg_bss_req, Bss, Max, EndCntrl, Stack,LastMarker} ->
	    eval_neg_each_binding(Bss, NextPid, 
				  [{params,Max,EndCntrl}|Stack],
				  LastMarker),
	    negation_help(NextPid, Buddie, Acc0, {Tid, PidL, EndC});
	
	{tid_req,NTid,Pid} ->
%%	    mnesia:put_activity_id(Tid),
	    NextPid ! {tid,NTid,self()}, 
	    negation_help(NextPid, Buddie, [], {Tid, PidL, EndC});

	%% From the ordinary flow, that is, from the negated part
	{bss, Bss, all, [], [SendData,{params,Max,EndCntrl}|Stack],LastMarker} ->

	    NewEndC = update_endc (PidL, EndC, LastMarker),
	    case SendData of
		last when NewEndC == 0 ->
		    Buddie ! {neg_bss_repl, Acc0, Max, EndCntrl, 
			      Stack, {true, self()}},
		    negation_help(NextPid, Buddie, [], {Tid, PidL, NewEndC});
		{bs,BsOrig} ->
		    case Bss of
			[] ->
			    negation_help(NextPid,Buddie,[BsOrig|Acc0],
					 {Tid,PidL,NewEndC});
			[_|_] ->
			    negation_help(NextPid,Buddie,Acc0,
					 {Tid,PidL,NewEndC})
		    end
	    end;
	
	{fail, SomePid, Cause} ->
	    negation_help(NextPid, Buddie, [],{[],[],[]});
	
	{tid, NTid, Pid} ->
	    negation_help(NextPid, Buddie, Acc0, 
			  add_tid (Tid, PidL, EndC, NTid, Pid))
    end.
    


eval_neg_each_binding([Bs|Bss], NextPid, Stack, LastMarker) ->
    NextPid ! {bss, [Bs], all, [], [{bs,Bs}|Stack], false},
    eval_neg_each_binding(Bss, NextPid, Stack, LastMarker);
eval_neg_each_binding([], NextPid, Stack, LastMarker) ->
    NextPid ! {bss, [], all, [], [last|Stack], LastMarker}.

%%%----------------------------------------------------------------

bss_union(Bss, Bs0) ->
    bss_union(Bss, Bs0, []).

bss_union([Bs1|Bss], Bs0, Acc) ->
    case catch mnemosyne_unify:bs_union(Bs0,Bs1) of
	fail -> bss_union(Bss, Bs0, Acc);
	Bs -> bss_union(Bss, Bs0, [Bs|Acc])
    end;
bss_union([], _, Acc) ->
    Acc.

%%%----------------------------------------------------------------
send([Pid|Pids], Msg) when pid(Pid) ->	Pid ! Msg, send(Pids,Msg);
send([], _) -> ok;
send(Pid, Msg) when pid(Pid) -> Pid ! Msg.

%%%----
split_list(all, L) -> {L,[]};
split_list(N, L) when length(L)>N -> split_list(N, L, []);
split_list(_, L) -> {L,[]}.

split_list(N, [H|T], Acc) when N>0 -> split_list(N-1, T, [H|Acc]);
split_list(N, L, Acc) -> {Acc,L}.


%%%----
unify_list(Ms, Pattern, Bs, Sols) ->
    lists:foldl(
      fun (M,Acc) ->
	      case mnemosyne_unify:unify(Pattern, M, Bs) of
		  fail -> Acc; 
		  NewBs -> [NewBs|Acc]
	      end
      end, Sols, Ms).


