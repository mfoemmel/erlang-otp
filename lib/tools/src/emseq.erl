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
%%
%% Description:
%%    emseq is used to trace message passsing between processes
%%    it collects a list of messages and deliver it when 
%%    tracing is done.
%%
%%
-module(emseq).

-export([pids/2, call/3, call/4, call/5]).
-export([seq_mon/3, timer/2]).


pids(Pids, Time) ->     start([], Pids, Time).
call(M,F,A) ->          start({M,F,A}, [], infinity).
call(M,F,A,Time) ->     start({M,F,A}, [], Time).
call(M,F,A,Pids,Time) -> start({M,F,A}, Pids, Time).

%%
%% Start sequence monitor and acknoledge when ready
%%
start(MFA,Pids,Time) ->
    Pid = spawn(emseq, seq_mon, [self(), {MFA, Pids}, Time]),
    receive
	{emseq,X} -> X
    end.

%%
%% Message Sequence Monitor
%% Monitor message
%%
seq_mon(Starter, What, Time) ->
    process_flag(priority, max),
    process_flag(trap_exit, true),
    {Link,Spawn} = case What of
		       {[], Ps} ->
			   {link_all(Ps),[]};
		       {{M,F,A},Ps} ->
			   P = link_trace(spawn(M,F,A)),
			   {link_all(Ps), [P]}
		   end,
    Timer = spawn_link(emseq, timer, [self(),Time]),
    case collect([], Spawn, Link, init_call(lists:append(Spawn,Link))) of
	{timeout,Ms,NSpawn,NLink,NPids} ->
	    unlink_all(NLink),
	    unlink_all(NSpawn),
	    output(Ms, NPids);
	{exit, Ms, NLink, NPids} ->
	    exit(Timer, kill),
	    unlink_all(NLink),
	    output(Ms, NPids)
    end,
    Starter ! {emseq, ok}.
%%
%%
%%
init_call([Pid|Pids]) ->
    {_, MFA} = process_info(Pid, initial_call),
    [{Pid,MFA} | init_call(Pids)];
init_call([]) -> [].

%%
%% Link all and set trace
%%
link_all([Name|Pids]) when atom(Name) ->
    case whereis(Name) of
	undefined -> link_all(Pids);
	Pid ->
	    [ link_trace(Pid) | link_all(Pids)]
    end;
link_all([Pid|Pids]) ->
    [link_trace(Pid) | link_all(Pids)];
link_all([]) -> [].

link_trace(Pid) ->
    trace(Pid, true, [send, procs, timestamp, set_on_spawn, set_on_link]),
    link(Pid),
    Pid.

%%
%% Unlink and untrace all pids
%%
unlink_all([Pid|Pids]) ->
    unlink_trace(Pid),
    unlink_all(Pids);
unlink_all([]) -> true.

unlink_trace(Pid) ->
    unlink(Pid),
    trace(Pid, false, [send, procs, timestamp, set_on_spawn, set_on_link]),
    true.

%%
%% Message collect 
%% Ms = collection of all messages {From,To,Mess,MicroSes}
%% Pids = current pids traced
%% Ps = all pids that where traced
%%
collect(Ms, Spawn, Link, Ps) ->
    receive
	{trace, send, Mess, From, To, Stamp} ->
	    case lists:keysearch(From, 1, Ps) of 
		false -> collect(Ms,Spawn,Link,Ps);
		_ ->
		    case lists:keysearch(To, 1, Ps) of
			false -> collect(Ms,Spawn, Link,Ps);
			_ ->
			    Now = micro_secs(Stamp),
			    collect([{From,To,Mess,Now}|Ms],Spawn,Link,Ps)
		    end
	    end;
	{trace, Pid, spawn, NPid, _} ->
	    link(NPid),
	    {_, MFA} = process_info(NPid, initial_call),
	    collect(Ms, [NPid|Spawn], Link, [{NPid,MFA}|Ps]);
	{trace, Pid, link, NPid, _} ->
	    link(NPid),
	    case lists:keysearch(NPid, 1, Ps) of
		false ->
		    {_, MFA} = process_info(NPid, initial_call),
		    collect(Ms,Spawn,[NPid|Link],[{NPid,MFA}|Ps]);
		_ ->
		    collect(Ms, Spawn, [NPid|Link], Ps)
	    end;
		
	timeout ->
	    {timeout,Ms,Spawn,Link,lists:reverse(Ps)};
	{'EXIT', Pid, Reason} ->
	    case lists:delete(Pid, Spawn) of
		[] ->
		    {exit,Ms,Link,lists:reverse(Ps)};
		NSpawn ->
		    collect(Ms, NSpawn, Link, Ps)
	    end;
	_ ->
	    collect(Ms, Spawn, Link, Ps)
    end.

%%
%% Generate a timeout after Time
%%
timer(Pid, Time) ->
    receive
    after Time ->
	    Pid ! timeout
    end.

%%
%% Convert a timestamp into micro seconds
%%
micro_secs({Mega,Sec,Micro}) ->
    Mega*1000000000000 + Sec*1000000 + Micro.
%%
%%
%%
-define(default_margin, 10).
-define(default_width, 80).

%%
%% Output trace sequence input: {From,To,Message,When}
%% 1. Sort on time
%% 2. Length of pids
%%
output(Ms, Pids) ->
    Ls = lists:keysort(4, Ms),
    Regs = process_names(),
    Info = header_info(Pids, Regs),
    print("PROCESS INFO"),
    newline(),
    newline(),
    output_info(Info),
    print("MESSAGE FLOW CHART"),
    newline(),
    newline(),
    print("d(usec)"),
    output_header(Info, 8),
    output_mesg(Ls, Info, 0, 1).

%%
%% Info output
%%
%%
output_info([{Pid,Pos,Name,{M,F,A},Width,Cs} | Info]) ->
    print(Cs),
    case Name of
	'' -> ok;
	_  -> print(io_lib:format("registered as ~s ", [Name]))
    end,
    print(" spawned as "),
    print(io_lib:format("~w:~w/~w", [M,F,A])),
    newline(),
    output_info(Info);
output_info([]) -> 
    ok.

%%
%% Output a header
%% ------    Pid1       Pid2      Pid2   ...
%%
output_header([{_,Pos,_,_,Width,Cs} | Info], CPos) ->
    RPos = Pos - (Width div 2),
    blanks(RPos-CPos),
    print(Cs),
    output_header(Info, RPos+Width);
output_header([], CPos) ->
    newline(),
    CPos.

%%
%% Output Message
%%
output_mesg([{From,To,Message,When}|Ls], Info, PrevWhen, Lines) ->
    FromPos = position(From, Info),
    ToPos = position(To,  Info),
    Width = abs(FromPos-ToPos)+1,
    Delta = if
		PrevWhen == 0 -> 0;
		true -> When - PrevWhen
	    end,
    if
	FromPos < ToPos ->
	    message(Message, FromPos, Width, Delta);
	FromPos == ToPos ->
	    message(Message, FromPos, 10, Delta);
	true ->
	    message(Message, ToPos, Width, Delta)
    end,
    arrow(FromPos, ToPos),
    output_mesg(Ls, Info, When, Lines+2);
output_mesg([], _, _, Lines) -> Lines.

%%
%% Draw message arrow
%%
arrow(From, To) ->
    if
	From < To ->
	    blanks(From-1),
	    print("-", To-From),
	    print(">");
	From == To ->
	    blanks(From-1),
	    print("<->");
	true ->
	    blanks(To-1),
	    print("<"),
	    print("-", From-To)
    end,
    newline().

%%
%% Output message
%%
message(Message, Pos, Width, Delta) ->
    Cs = lists:flatten(io_lib:format("~p", [Message])),
    N = length(Cs),
    if
	N > Width ->
	    Cs1 = lists:append(lists:sublist(Cs, 1, Width-3),"..."),
	    N1 = Width;
	true ->
	    Cs1 = Cs,
	    N1 = N
    end,
    DeltaCs = integer_to_list(Delta),
    M = length(DeltaCs),
    print(DeltaCs),
    blanks(?default_margin-M),
    blanks(Pos + ((Width-N1) div 2) - 1 - ?default_margin),
    print(Cs1),
    newline().

%%
%% Build header info
%% {Pid,Pos,Name,InitCall,Width,Cs}
%%
header_info(Pids, Regs) ->
    {Width,Ws} = calc_width(Pids),
    info(Ws, Width+?default_margin,Regs).

info(Ws, Width, Regs) ->
    N = length(Ws),
    Offs = (Width - ?default_margin) div N,
    calc_info(Ws, Regs, ?default_margin, Offs).

calc_info([{Pid,_,_,MFA,Width,Cs}|Pids], Regs, Pos, Offs) ->
    NPos = Pos + (Width div 2),
    Name = case lists:keysearch(Pid, 1, Regs) of
	       false -> '';
	       {value, {_, X}} -> X
	   end,
    [{Pid,NPos,Name,MFA,Width,Cs} | calc_info(Pids, Regs, Pos+Offs, Offs)];
calc_info([], _, _, _) -> [].

%%
%% Get positions of X
%%
position(Pid, Info) ->
    {value,{_,Pos,_,_,_,_}} = lists:keysearch(Pid,1,Info),
    Pos.

calc_width(Pids) ->
    {MaxWidth,Ws} = calc_width(Pids, 0, []),
    Width = (MaxWidth+2)*length(Pids),
    if 
	Width < ?default_width -> {?default_width, Ws};
	true -> {Width, Ws}
    end.


calc_width([{Pid,MFA}|Pids], Max, Ws) ->
    Cs = lists:flatten(io_lib:format("~p", [Pid])),
    Len = length(Cs),
    if
	Len > Max -> calc_width(Pids, Len, [{Pid,0,'',MFA,Len,Cs}|Ws]);
	true -> calc_width(Pids, Max, [{Pid,0,'',MFA,Len,Cs}|Ws])
    end;
calc_width([], Max, Ws) -> {Max, lists:reverse(Ws)}.

%%
%% Build list {name, pid} for registered processes
%%
process_names() ->
    process_names(registered()).

process_names([Name | Ns]) ->
    [{whereis(Name), Name} | process_names(Ns)];
process_names([]) -> [].

%%
%%
%%
print([C|Cs], N) when N =< 0 -> print(Cs, N+1);
print([], _) -> ok;
print(C, N) -> io:put_chars(lists:duplicate(N, C)).

print(C) -> print(C, 1).

blanks(N) -> print(32, N).

newline() -> io:nl().
