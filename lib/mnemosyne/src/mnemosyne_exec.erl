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
-module(mnemosyne_exec).

-export([mk_patterns/1]).

-export([setup/2]).

-export([setup_collector_and_query/1, init_query/2,
	 get_answers/3,
	 kill_processes/1]).

-export([start_simple_loop1/4, 
	 start_simple_loop2/3,
	 start_complicated_loop/3]). %% spawned
%%-export([simple_loop1/4, simple_loop2/3, complicated_loop/3]). %% spawned

%%-define(debug,42).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%================================================================
%%% 		Exports

%%% eval_collect(Nsols, Query, Fun) where
%%%	Fun = fun(Accu,P) is foldl'd over all bound patterns and the result is
%%%			  returned
%%% Ex1. fun(Accu,P) ->
%%%          [P|Accu]
%%%       end
%%%     collects all solutions.

%%% Ex2. fun(Accu,P) ->
%%%          m:f(P),
%%%          Accu
%%%       end
%%%     applies m:f on every solution but does not return any result



-record(spec, {func,
	       table = undefined,
	       pattern
	      }).

%%%----------------------------------------------------------------
%%% Cursor handling
%%% 
%% Supposed to be called as:
%%     H = setup_collector_and_query(Q),
%%         ....
%%     mnesia:transaction(
%%         fun() ->
%%	     Crsr = init_query(H [,Number_of_answers_to_prefetch]),
%%	         ....
%%	     loop over L1 = get_answers(Crsr [,Min,Max]),
%%	         ....
%%	   end),
%%         ....
%%     kill_processes(H)


setup_collector_and_query(OptQuery) when is_record(OptQuery,optimizer_result) ->
    Spec = #spec{func = fun(Accu,P) -> [P|Accu] end,
		 pattern = OptQuery#optimizer_result.pattern},
    CollectorPid =
	case OptQuery#optimizer_result.how_to_eval of
	    mnesia_match_build -> 
		%% fetch all solutions using mnesia:match_object
		[Code] = OptQuery#optimizer_result.code,
		Bs = OptQuery#optimizer_result.common_bs,
		?debugmsg(1, "Using mnesia:match_object(~w)\n",
			  [Code#pred_sym.pattern]),
		spawn_link(?MODULE, start_simple_loop1, [self(),Bs,Code,Spec]);

	    query_eval when OptQuery#optimizer_result.code==[] ->
		%% has the single answer as the pattern
		Solutions = [OptQuery#optimizer_result.pattern],
		spawn_link(?MODULE, start_simple_loop2, 
			   [self(),Solutions,Spec]);

	    query_eval -> %% Must start all processes
		spawn_link(?MODULE, start_complicated_loop, 
			   [self(),OptQuery,Spec])
	end,
    CursorOwner = self(),
    {query_collector, CollectorPid, CursorOwner}.


%%%----
init_query({query_collector,CollectorPid,CursorOwner}, N) ->
    Tid = mnesia:get_activity_id(),
    legal_pid_tid(CollectorPid, CursorOwner, Tid),
    CollectorPid ! {attention,CursorOwner},
    CollectorPid ! {init_collector,CursorOwner,Tid,N},
    {exec_cursor, CollectorPid, CursorOwner, Tid}.


%%%----
get_answers({exec_cursor,CollectorPid,CursorOwner,Tid}, Nmin, Nmax) ->
    legal_pid_tid(CollectorPid, CursorOwner, Tid),
    CollectorPid ! {get, CursorOwner, Tid, Nmin, Nmax},
    receive
	{answer,CollectorPid,Answers} ->
	    Answers;
	{fail,_,Cause} ->
	    throw({'EXIT',Cause});
	{mnesia_down, Node} ->
	    mnesia:abort({node_not_running, Node})
    end.
    

legal_pid_tid(CollectorPid, CursorOwner, Tid0) ->
    Tid = mnesia:get_activity_id(),
    CollectorProcessInfo = process_info(CollectorPid),
    if 
	CursorOwner =/= self() ->
	    throw({'EXIT',{aborted,not_cursor_owner}});

	Tid == undefined ->
	    throw({'EXIT',{aborted,no_transaction}});

	Tid =/= Tid0 ->
	    throw({'EXIT',{aborted,wrong_transaction}});

	CollectorProcessInfo == undefined ->
	    throw({'EXIT',{aborted,no_collector}});

	true ->
	    ok
    end.
    


%%%----

kill_processes({query_collector,CollectorPid,CursorOwner}) ->
    if 
	CursorOwner =/= self() ->
	    throw({'EXIT',{aborted,not_cursor_owner}});

	true ->
	    unlink(CollectorPid),
	    exit(CollectorPid, deleted)
    end.


%%%----------------

start_simple_loop1(Self,Bs,Code,Spec) ->
    simple_loop1(Self,Bs,Code,Spec).

start_simple_loop2(Self,Solutions,Spec) ->
    simple_loop2(Self,Solutions,Spec).

start_complicated_loop(Self,OptQuery,Spec) ->
    complicated_loop(Self,OptQuery,Spec).


simple_loop1(CursorOwner, Bs, Code, Spec) ->
    ?debugmsg(1,
	      "simple_loop1(CursorOwner=~w, Bs=~w, Code=~w, Spec=~w)\n",
	      [CursorOwner, Bs, Code, Spec]
	     ),
    receive
	{init_collector, CursorOwner, Tid, Nprefetch} ->
	    case Spec#spec.table of
		undefined -> true;
		_ -> ets:delete (Spec#spec.table)
	    end,
	    Spec2 = Spec#spec{table=ets:new(filter2,[set,public])},
	    mnesia:put_activity_id(Tid),
	    case catch mnemosyne_op:get_from_table(Bs,Code,[]) of
		{'EXIT', Cause} ->
		    CursorOwner ! {fail, self(), Cause},
		    simple_loop1(CursorOwner, Bs, Code, Spec);
		Solutions ->
		    simple_loop(filter_result(Spec2,Solutions),
				Tid, CursorOwner, Spec2),
		    simple_loop1(CursorOwner, Bs, Code, Spec2)
	    end;

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    ReplyTo ! {activity_ended, Activity, self()},
	    simple_loop1(CursorOwner, Bs, Code, Spec);

	{attention,CursorOwner} ->
	    simple_loop1(CursorOwner, Bs, Code, Spec)
    end.
    

simple_loop2(CursorOwner, Solutions, Spec) ->
    ?debugmsg(1,
	      "simple_loop2(CursorOwner=~w, Solutions=~w, Spec=~w)\n",
	      [CursorOwner, Solutions, Spec]
	     ),
    receive
	{init_collector, CursorOwner, Tid, Nprefetch} ->
	    case Spec#spec.table of
		undefined -> true;
		_ -> ets:delete (Spec#spec.table)
	    end,
	    Spec2 = Spec#spec{table=ets:new(filter3,[set,public])},
	    mnesia:put_activity_id(Tid),
	    simple_loop(Solutions, Tid, CursorOwner, Spec2),
	    simple_loop2(CursorOwner, Solutions, Spec2);

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    ReplyTo ! {activity_ended, Activity, self()},
	    simple_loop2(CursorOwner, Solutions, Spec);

	{attention,CursorOwner} ->
	    simple_loop2(CursorOwner, Solutions, Spec)
    end.



complicated_loop(CursorOwner, OptQuery, Spec) ->
    ?debugmsg(1,
	      "complicated_loop(CursorOwner=~w, OptQuery=~w, Spec=~w)\n",
	      [CursorOwner, OptQuery, Spec]
	     ),
    %% process_flag(trap_exit, true),
    {FirstPid,Count} = setup_query(OptQuery#optimizer_result.code, self()),
    complicated_loop_cont(FirstPid, Count,CursorOwner, OptQuery, Spec).

complicated_loop_cont(FirstPid, Count,CursorOwner, OptQuery, Spec) ->
    receive
	{init_collector, CursorOwner, Tid, Nprefetch} ->
	    case Spec#spec.table of
		undefined -> true;
		_ -> ets:delete (Spec#spec.table)
	    end,
	    Spec2 = Spec#spec{table=ets:new(filter4,[set,public])},
	    mnesia:put_activity_id(Tid),
	    FirstPid ! {tid,Tid, self()},
	    EndCounters = [mnemosyne_op:new_end_token(1)],
	    FirstPid ! {bss, 
			[OptQuery#optimizer_result.common_bs], 
			Nprefetch, EndCounters, [], {true, self()}},
	    Reply = no_pending([], Count,  FirstPid,
			       Tid, CursorOwner, Spec2, EndCounters, 
			       {[],[],[]}),
	    CursorOwner ! Reply,
	    complicated_loop_cont(FirstPid, Count,CursorOwner, 
				  OptQuery, Spec2);

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    Message = {tid, Activity},
	    FirstPid ! Message,
	    flush_until(Message),
	    ReplyTo ! {activity_ended, Activity, self()},
	    complicated_loop_cont(FirstPid, Count,CursorOwner, OptQuery, Spec);
	
	{attention,CursorOwner} ->
	    complicated_loop_cont(FirstPid, Count, CursorOwner, OptQuery, Spec)

    end.

flush_until(Message) ->
    receive
	Message -> ok;
	Other -> flush_until(Message)
    end.


simple_loop(Answers, Tid, CursorOwner, Spec) ->
    ?debugmsg(1, "simple_loop Answers=~w\n", [Answers]),
    receive
	{get, CursorOwner, Tid, Nmin, Nmax} ->
	    {First, Last} = mnemosyne_op:split_list(Nmax, Answers),
	    CursorOwner ! {answer, self(), First},
	    simple_loop(Last, Tid, CursorOwner, Spec);

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    ReplyTo ! {activity_ended, Activity, self()},
	    ok;

	{fail, SomePid, Cause} ->
	    CursorOwner ! {fail, self(), Cause},
	    ok;

	{attention,CursorOwner} ->
	    ok
    end.
	

ask_for_more(Pid, N, EndCounters) when is_pid(Pid) ->
    NewEndToken = mnemosyne_op:new_end_token(1),
     Pid ! {bss, [], N, [NewEndToken], [], {true, self()}},  
    [NewEndToken | EndCounters].    

send_answers(Pid, Acc, N) ->
    {First, Last} = mnemosyne_op:split_list(N, Acc),
    Pid ! {answer, self(), First},
    Last.
    
no_pending(Answers, Count, FirstPid, CTid, CursorOwner, Spec, 
	   EndCounters, {Tid,PidL,EndC}) ->
    ?debugmsg(1, "no_pending Answers=~w, EndCounters=~w\n",
	      [Answers, EndCounters]),
    receive
	{get, CursorOwner, CTid, Nmin, Nmax} ->
	    %% Answers requested
	    Nanswers = length(Answers),
	    if Nmin =< Nanswers ->
		    %% Has enough answers already.
		    no_pending(send_answers(CursorOwner,Answers,Nmax), Count,
			       FirstPid, CTid, CursorOwner, Spec, EndCounters,
			       {Tid, PidL, EndC});
	      Nanswers < Nmin ->
		    %% Has NOT enough answers available, ask for more
		    pending(Answers, Count, FirstPid, CTid, 
				    CursorOwner, Spec,
				    ask_for_more(FirstPid,Nmax,EndCounters),
				    Nmin, Nmax, {Tid, PidL, EndC})
	    end;

	{bss, Bss, SliceSize, EndCntrl, Stack, LastMarker} ->
	    NewEndC = mnemosyne_op:update_endc (PidL, EndC, LastMarker),
	    case NewEndC of
		[] -> 
		    simple_loop(filter_result(Spec,Bss,Answers), 
				CTid, CursorOwner, Spec);
		_ ->
		    case decrease_counters(EndCntrl, EndCounters) of
			[] -> %% No more Bss expected. But there are more
			      %% to come (lastmarker hasn't arrived yet)
			    no_pending(filter_result(Spec,Bss,Answers),
				       Count,FirstPid, CTid, CursorOwner,
				       Spec,
				       ask_for_more(FirstPid, SliceSize,[]),
				       {Tid, PidL, NewEndC});
			NewEndCounters -> %% More Bss expected
			    no_pending(filter_result(Spec,Bss,Answers),
				       Count, FirstPid, CTid, 
				       CursorOwner, Spec,
				       NewEndCounters,	
				       {Tid, PidL, NewEndC})
		    end
	    end;

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    ReplyTo ! {activity_ended, Activity, self()},
	    ok;

	{fail, SomePid, Cause} ->
	    CursorOwner ! {fail, self(), Cause},
	    ok;

	{tid, NTid,Pid} ->
	    no_pending(Answers, Count, FirstPid, CTid, CursorOwner, 
		       Spec, EndCounters,
		       mnemosyne_op:add_tid (Tid, PidL,EndC,NTid,Pid));
	
	{attention,CursorOwner} ->
	    ok;
	
	Others -> 
	    Others
    end.
	    

pending(Answers, Count, FirstPid, CTid, CursorOwner, Spec, EndCounters, 
	Nmin, Nmax, {Tid,PidL,EndC}) ->
    ?debugmsg(1, "pending Answers=~w, EndCounters=~w, Nmin=~w, Nmax=~w\n",
	      [Answers, EndCounters, Nmin, Nmax]
	     ),
    
    receive
	{bss, Bss, SliceSize, EndCntrl, Stack, LastMarker} ->
	    NewAnswers = filter_result(Spec,Bss,Answers),
	    Nnewanswers = length(NewAnswers),

	    NewEndC = mnemosyne_op:update_endc (PidL, EndC, LastMarker),

	    case NewEndC of
		[] -> %% All answers arrived. Just loop and wait for more get's
		    simple_loop(send_answers(CursorOwner, NewAnswers,Nmax),
				CTid, CursorOwner, Spec);
		_ ->
		    case decrease_counters(EndCntrl, EndCounters) of
			[] when Nnewanswers < Nmin ->
			    %% No more Bss expected, but LastMarker hasn't 
			    %% arrived yet --> ask for more.
			    pending(NewAnswers, Count, FirstPid, CTid, 
				    CursorOwner,
				    Spec,
				    ask_for_more(FirstPid, SliceSize, []),
				    Nmin, Nmax, {Tid, PidL, NewEndC});
			[] when Nmin =< Nnewanswers ->
			    %% No more Bss expected. Ask for more. Send Answers
			    no_pending(send_answers(CursorOwner,
						    NewAnswers,Nmax),
				       Count, FirstPid, CTid, CursorOwner,
				       Spec,
				       ask_for_more(FirstPid, SliceSize,[]),
				       {Tid, PidL, NewEndC});
			
			NewEndCounters when Nnewanswers < Nmin -> 
			    %% More Bss expected, just wait for them
			    pending(NewAnswers, Count, FirstPid, CTid, 
				    CursorOwner, Spec,
				    NewEndCounters, Nmin, Nmax,
				    {Tid, PidL, NewEndC});

			NewEndCounters when  Nmin =< Nnewanswers -> 
			    %% More Bss expected, send Answers, wait for 
			    %% more Bss
			    no_pending(send_answers(CursorOwner,NewAnswers,Nmax),
				       Count,
				       FirstPid, CTid, CursorOwner, Spec,
				       NewEndCounters, {Tid, PidL, NewEndC})
		    end
	    end;

	{activity_ended, Activity, ReplyTo} ->
	    mnesia:put_activity_id(Activity),
	    ReplyTo ! {activity_ended, Activity, self()},
	    ok;

	{fail, SomePid, Cause} ->
	    CursorOwner ! {fail, self(), Cause},
	    ok;

	{tid, NTid, Pid} ->
	    pending(Answers, Count, FirstPid, CTid, CursorOwner, 
		    Spec, EndCounters, Nmin, Nmax,
		    mnemosyne_op:add_tid (Tid, PidL,EndC,NTid,Pid));
	
	{attention,CursorOwner} ->
	    ok;
	
	Others -> 
	    Others
    end.
		    
%%%================================================================
%%% 		Private

-define(start_op(Type,Args),
	spawn_link(mnemosyne_op, start_op, [Type,Args])
       ).

%%%----------------

setup_query(Query, NextPid) ->
    
    {Q2,CollCount} = count_receivers (Query, 1),

    ?debugmsg(3, "~w (CollectorPid)\n", [NextPid]),
    P = setup(Q2,NextPid),
    P2 = make_split(P,1),
    {P2, CollCount}.

count_receivers ([C|Cs],Count) ->
    {C2, Count2} = count_receivers (C, 1),
    {Cs2, Count3} = count_receivers (Cs, Count2),
    {[C2 | Cs2], Count3}
;
count_receivers ([],Count) ->
    {[], Count}
;
count_receivers ({'#or', C, Alts}, Count) ->
    {Alts2, Count2} = lists:mapfoldl (fun (A, Sum) -> 
					       {A2,C2} = count_receivers (A, 1), 
					       {A2, C2+Sum} 
				       end,
				       0, Alts),
    { {'#or', Count, Alts2},  Count2}
;
count_receivers ({'#not', _, Q}, Count) ->
    {Q2, C2} = count_receivers (Q, Count),
    { {'#not', Count, Q2}, C2}
;
count_receivers (X, Count) when is_record (X, disj_alt) ->
    {X2, Count2} = count_receivers (X#disj_alt.conj, Count),
    {X#disj_alt{conj=X2}, Count2}
;
count_receivers (C, Count) ->
    {set_count (C, Count), Count}.
    
set_count (X,C) when is_record (X, pred_sym) ->
    X#pred_sym{rec_count = C};
set_count (X,C) when is_record (X,fn) ->
    X#fn{rec_count = C};
set_count (X,C) when is_record (X, erl_expr) ->
    X#erl_expr{rec_count = C};
set_count (X,C) when is_record (X, disj_alt) ->
    X#disj_alt{rec_count = C};
set_count ({'#bindings', RecC, Bs},C) ->
    { {'#bindings', C, Bs}, 1}.
			       


setup([C|Cs], NextPid) ->
    setup(C, setup(Cs,NextPid));

setup({'#or',Count, Alts}, NextPid) ->
    make_split(lists:map(fun(A) -> setup(A, NextPid) end, Alts), Count);

setup({'#bindings',RecCount, Bs}, NextPid) ->
    ?start_op(bindings, [NextPid,RecCount,Bs]);

setup(P, NextPid) when is_record(P,pred_sym) ->
    case P#pred_sym.type of
	table ->
	    Ps = mk_patterns(P),
	    ?start_op(table, [NextPid, Ps]);

	rule when P#pred_sym.recursive==recursive ->
	    ?start_op(call_recursive_op, [NextPid,P])
    end;

setup(F, NextPid) when is_record(F,fn) ->
    ?start_op(funcall, [NextPid,F#fn.alias_var,F#fn.fndef,F#fn.rec_count]);

setup(E, NextPid) when is_record(E,erl_expr) ->
    ?start_op(erl_expr, [NextPid,E]);

setup(C, NextPid) when is_record(C,disj_alt) ->
    setup(C#disj_alt.conj, NextPid);

setup({'#not',C, Q}, NextPid) ->
    HelpPid = ?start_op(negation_help, []),
    Pid = ?start_op(negation, [NextPid,C,HelpPid]),
    HelpPid ! {pids, Pid, setup(Q,HelpPid)},
    Pid;

setup([], NextPid) ->
    NextPid.




make_split([Pid], Count) when is_pid(Pid) -> Pid;
make_split(L, Count) when is_list(L),is_pid(hd(L)) -> ?start_op(split, [L, Count]);
make_split(Pid, Count) when is_pid(Pid) -> Pid.

%%%----------------
-record(mkp, {p=[],	%% The Pattern to send to trans:match (ets:match)
	      d=[],	%% Dictionary used when making p
	      evN=0	%% First unused ets variable
	     }).


mk_patterns(P) when is_record(P,pred_sym) ->
    S = mkp(P#pred_sym.args,
	    P#pred_sym.defvars,
	    P#pred_sym.singelvars,
	    #mkp{}),
    P#pred_sym{pattern=hd(S#mkp.p),%%% one arg only
	       back_pattern=hd(P#pred_sym.args)
	      }.


mkp([H|T], DefVars, SingelVars, S0) ->
    Sh = mkp1(H, DefVars, SingelVars, S0#mkp{p=[]}),
    mkp(T, DefVars, SingelVars, Sh#mkp{p=[Sh#mkp.p|S0#mkp.p]});

mkp([], DefVars, SingelVars, S) ->
    S#mkp{p=lists:reverse(S#mkp.p)}.



mkp1([H|T], DefVars, SingelVars, S0) ->
    Sh = mkp1(H, DefVars, SingelVars, S0#mkp{p=[]}),
    if
	is_list(T) ->
	    mkp1(T, DefVars, SingelVars, Sh#mkp{p=[Sh#mkp.p|S0#mkp.p]});
	true ->
	    St = mkp1(T, DefVars, SingelVars, Sh#mkp{p=[]}),
	    St#mkp{p = lists:append(lists:reverse([Sh#mkp.p|S0#mkp.p]), 
				    St#mkp.p)}
    end;

mkp1([], DefVars, SingelVars, S) ->
    S#mkp{p=lists:reverse(S#mkp.p)};

mkp1({'#var','_'}, DefVars, SingelVars, S0) ->
    S0#mkp{p='_'};

mkp1({'#var',V}, DefVars, SingelVars, S0) ->
    case ordsets:is_element(V,SingelVars) of
	true ->
	    S0#mkp{p='_'};
	false ->
	    case ordsets:is_element(V,DefVars) of
		true ->
		    S0#mkp{p={'#var',V}};
		false ->
		    case lists:keysearch(V,1,S0#mkp.d) of
			{value, {V,EtsVar}} ->
			    S0#mkp{p=EtsVar};
			false ->
			    EtsVar = ets_var(S0#mkp.evN),
			    S0#mkp{p=EtsVar, 
				   evN=S0#mkp.evN+1,
				   d=[{V,EtsVar}|S0#mkp.d]}
		    end
	    end
    end;
	
mkp1(T, DefVars, SingelVars, S0) when is_tuple(T) ->
    S1 = mkp1(tuple_to_list(T),DefVars,SingelVars,S0),
    S1#mkp{p=list_to_tuple(S1#mkp.p)};

mkp1(X, DefVars, SingelVars, S0) ->
    S0#mkp{p=X}.


ets_var(N) -> list_to_atom([$$ | integer_to_list(N)]).


%%----------------
filter_result(Spec, Bss) ->
    filter_result(Spec, Bss, []).

filter_result(Spec, Bss, Acc) ->
    lists:foldr(
      fun(Bs,Accu) ->
	      Answer = mnemosyne_unify:instantiate(Spec#spec.pattern,Bs),
	      case ets:lookup(Spec#spec.table, Answer) of
		  []  -> 
		      ets:insert(Spec#spec.table, {Answer}),
		      (Spec#spec.func)(Accu,Answer);
		  _ ->
		      Accu
	      end
      end, Acc, Bss).


%%----------------
decrease_counters([{Key,N}|T], Counters) ->
    decrease_counters(T,
		      case lists:keysearch(Key,1,Counters) of
			  {value, {Key,X}} when X>1 ->
			      lists:keyreplace(Key,1,Counters,{Key,X-1});
			  {value, {Key,X}} ->
			      lists:keydelete(Key,1,Counters);
			  false when N>1 ->
			      [{Key,N-1}|Counters];
			  false ->
			      Counters
		      end);
decrease_counters([], Counters) ->
    Counters.

%%----------------

