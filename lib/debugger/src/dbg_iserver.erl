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
-module(dbg_iserver).

%%-compile(export_all).

-export([start1/0]).

-export([int_module/1]).

%%% --------------------------------------------------------
%%% --------------------------------------------------------
%%% The interpreter server.
%%% --------------------------------------------------------
%%% --------------------------------------------------------



start1() ->
    process_flag(trap_exit,true),
    code:stick_dir(lists:concat([code:root_dir(),
				 "/lib/interpreter/ebin"])),
    DbPid = dbg_idb:new(),
    loop(DbPid,[],[],[],[],false).

loop(DbPid,Pids,Mons,Fol,Atts,Auto) ->
    receive
	
	%% Messages from code server

	{CodeServer,{interpret,Mod}} ->
	    %% We need to check the interpretation result for 
	    %% (all?) nodes here? /olin (???)

	    mon_send({self(),interpret,Mod},lists:append(Mons, Atts)),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
	
	{_,{load,{From,Tag},Mod,File,Binary}} ->
	    case catch load_mod(Mod,File,Binary) of
		{ok, Mod} ->
		    gen_server:reply({From,Tag},{module,Mod}),  %% Simulate ack. from code_server.
		    mon_send({self(),interpret,Mod},lists:append(Mons,Atts)),
		    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
		What ->
		    gen_server:reply({From,Tag},{error,What}),  %% Simulate ack. from code_server.
		    loop(DbPid,Pids,Mons,Fol,Atts,Auto)
	    end;

	%% Handle the case that a module is no longer interpreted.
	{_,{delete,Mod}} ->
	    catch del_module(Mod),
	    mon_send({self(),no_interpret,Mod},lists:append(Mons,Atts)),

	    %% Delete all breaks for this module
	    Break = {Mod},
	    dbg_idb:delete_all_breaks(Mod),
	    mon_send({self(),no_break,Break},lists:append(Mons,Atts)),
	    pid_send({break_msg,no_break,Break},Pids),

	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Messages from pid that will be interpreted
	%% Sent from the module error_handler.
	
	{From,{interpreted,Pid}} ->
	    From ! {self(),{interpreted,member(Pid,Pids)}},
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Messages from monitor process

	{Mon_Pid, refresh} ->
	    send_pids(lists:reverse(Pids),Mon_Pid),
	    Mon_Pid ! {self(),auto_attach,Auto},
	    tell_trace([Mon_Pid]),
	    tell_stack_trace([Mon_Pid]),
	    Mons1 = new_monitor(Mons,Mon_Pid),
	    loop(DbPid,Pids,Mons1,Fol,Atts,Auto);
	{Mon_Pid, clear} ->
	    Pids1 = clear(Pids,Mons),
	    loop(DbPid,Pids1,Mons,Fol,Atts,Auto);
	{Mon_Pid,auto_attach,NewAuto} ->
	    node_mon_send({self(),auto_attach,NewAuto},Mons),
	    loop(DbPid,Pids,Mons,Fol,Atts,NewAuto);

	%% Messages from interpreted processes
	
	{Pid,am_followed,Info} ->
	    Fol1 = [{Pid,Info}|Fol],
	    loop(DbPid,Pids,Mons,Fol1,Atts,Auto);
	{Meta,{int_proc,Pid,Func}} ->
	    link(Meta),
	    Fol1 = followed(Meta,Pid,Fol,Auto),
	    PidTuple = {Pid,Meta,Func,running,{},{}},
	    mon_send({self(),new_proc,PidTuple},Mons),
	    loop(DbPid,[PidTuple|Pids],Mons,Fol1,Atts,Auto);
	{Meta,break_at,Cm,LineNo,AttP} ->
	    New_tuple = new_status(Meta,Pids,break,{Cm,LineNo}),
	    mon_send({self(),new_status,New_tuple},Mons),
	    auto_break_att(AttP,element(1,New_tuple),Meta,Auto),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{Meta,running} ->
	    New_tuple = new_status(Meta,Pids,running,{}),
	    mon_send({self(),new_status,New_tuple},Mons),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{Meta,waiting} ->
	    New_tuple = new_status(Meta,Pids,waiting,{}),
	    mon_send({self(),new_status,New_tuple},Mons),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{Meta,idle} ->
	    New_tuple = new_status(Meta,Pids,idle,{}),
	    mon_send({self(),new_status,New_tuple},Mons),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{Meta,{attached,AttPid}} ->
	    Atts1 = case lists:member(AttPid,Atts) of
			false ->
			    link(AttPid),
			    AttPid ! {self(),interpreted,dbg_idb:lookup(interpret)},
			    [AttPid|Atts];
			_ ->
			    Atts
		    end,
	    loop(DbPid,Pids,Mons,Fol,Atts1,Auto);

	%% Messages from dbg_icmd about break points,
	%% inform Mons, Atts and Pids.
	
	{From,{new_break_options,Break,Options}} ->
	    mon_send({self(),new_break_options,{Break,Options}},lists:append(Mons,Atts)),
	    pid_send({break_msg,new_break_options,{Break,Options}},Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
	    
	{From,{no_break,Break}} ->
	    mon_send({self(),no_break,Break},lists:append(Mons,Atts)),
	    pid_send({break_msg,no_break,Break},Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
	    
	{From,{delete_break,Break}} ->
	    mon_send({self(),delete_break,Break},lists:append(Mons,Atts)),
	    pid_send({break_msg,delete_break,Break},Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
	    
	{From,{new_break,Break,Options}} ->
	    mon_send({self(),new_break,{Break,Options}},lists:append(Mons,Atts)),
	    pid_send({break_msg,new_break,{Break,Options}},Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Messages from dbg_icmd about trace. Inform Pids.

	{From,{trace,on}} ->
	    dbg_idb:insert(trace,true),
	    pid_send({cmd,trace,true},Pids),
	    mon_send({self(),trace,true},Mons),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);
	{From,{trace,off}} ->
	    dbg_idb:insert(trace,false),
	    pid_send({cmd,trace,false},Pids),
	    mon_send({self(),trace,false},Mons),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Messages from dbg_icmd about stack trace. Inform Pids.

	{From,{stack_trace,Flag}} ->
	    dbg_idb:insert(stack_trace,Flag),
	    pid_send({cmd,stack_trace,Flag},Pids),
	    mon_send({self(),stack_trace,Flag},Mons),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Messages from dbg_idb about process running old code.

	{_,old_code, Module,Pid} ->
	    notify_meta_pid(Pid,Pids, Module),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Message to attach to a process using the given function.

	{From,xattach,Pid,Fnk,Node} ->
	    xattach1(From,Pid,Fnk,Pids,Node,is_alive()),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Message to take a snapshot on all processes current status.

	{From,snap} ->
	    snap(From,Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Message to find out the Meta pid for Pid.
	%% Used to make Pid continue execution.

	{From,which_meta,Pid} ->
	    which_meta(From,Pid,Pids),
	    loop(DbPid,Pids,Mons,Fol,Atts,Auto);

	%% Exit signals 

	{'EXIT',Meta,{Meta,Reason,AttP}} ->
	    New_tuple = new_status(Meta,Pids,exit,Reason),
	    mon_send({self(),new_status,New_tuple},Mons),
	    auto_exit_att(AttP,element(1,New_tuple),Reason,{},Auto),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{'EXIT',Meta,{Meta,Reason,Where,Bs,Stack,AttP}} ->
	    New_tuple = new_status(Meta,Pids,exit,Reason,{Where,Bs,Stack}),
	    mon_send({self(),new_status,New_tuple},Mons),
	    auto_exit_att(AttP,element(1,New_tuple),Reason,{Where,Bs,Stack},
			  Auto),
	    loop(DbPid,lists:keyreplace(Meta,2,Pids,New_tuple),Mons,Fol,Atts,Auto);
	{'EXIT',Pid,Reason} ->
	    case lists:member(Pid,Mons) of
		true ->
		    loop(DbPid,Pids,lists:delete(Pid,Mons),Fol,Atts,Auto);
		_ ->
		    case lists:member(Pid,Atts) of
			true ->
			    loop(DbPid,Pids,Mons,Fol,lists:delete(Pid,Atts),Auto);
			_ ->
			    New_tuple = new_status(Pid,Pids,exit,Reason),
			    mon_send({self(),new_status,New_tuple},Mons),
			    loop(DbPid,lists:keyreplace(Pid,2,Pids,New_tuple),
				 Mons,Fol,Atts,Auto)
		    end
	    end
    after timeout(Fol) ->
	    Fol1 = still_active(Fol),
	    loop(DbPid,Pids,Mons,Fol1,Atts,Auto)
    end.



which_meta(From,Pid,Pids) ->
    case lists:keysearch(Pid,1,Pids) of
	{value,Item} ->
	    Meta = element(2,Item),
	    From ! {self(),which_meta_resp,Meta};
	_ ->
	    From ! {self(),which_meta_resp,{error,not_interpreted}}
    end.

xattach1(From,Pid,{Mod,Fnk},Pids,Node,Alive) ->
    case lists:keysearch(Pid,1,Pids) of
	{value,Item} when Alive == false ->
	    case element(4,Item) of
		exit ->
		    catch spawn(Mod,Fnk,[false,Pid,element(5,Item),element(6,Item)]),
		    From ! {self(),xattach,ok};
		no_conn ->
		    From ! {self(),xattach,no_proc};
		_ ->
		    catch spawn(Mod,Fnk,[false,Pid,element(2,Item)]),
		    From ! {self(),xattach,ok}
	    end;
	{value,Item} ->
	    case element(4,Item) of
		exit ->
		    catch spawn(Node,Mod,Fnk,[false,Pid,element(5,Item),element(6,Item)]),
		    From ! {self(),xattach,ok};
		no_conn ->
		    From ! {self(),xattach,no_proc};
		_ ->
		    catch spawn(Node,Mod,Fnk,[false,Pid,element(2,Item)]),
		    From ! {self(),xattach,ok}
	    end;
	_ ->
            From ! {self(),xattach,no_proc}
    end.

snap(From,Pids) ->
    From ! {self(),snap_resp,snap(Pids)}.

snap([{Pid,_,Func,Status,Info,_}|Pids]) ->
    [{Pid,Func,Status,Info}|snap(Pids)];
snap([]) ->
    [].

new_monitor(Mons,Mon_Pid) ->
    case lists:member(Mon_Pid,Mons) of
	true ->
	    Mons;
	_ ->
	    link(Mon_Pid),
	    [Mon_Pid|Mons]
    end.

tell_trace(Pids) ->
    Tr = case dbg_idb:lookup(trace) of
	     {ok,true} ->
		 true;
	     _ ->
		 false
	 end,
    mon_send({self(),trace,Tr},Pids).

tell_stack_trace(Pids) ->
    Flag = case dbg_idb:lookup(stack_trace) of
	       {ok,Fl} ->
		   Fl;
	       _ ->
		   false
	   end,
    mon_send({self(),stack_trace,Flag},Pids).

mon_send(Msg,[Monitor|Mons]) -> 
    Monitor ! Msg,
    mon_send(Msg,Mons);
mon_send(_,[]) ->
    true.

node_mon_send(Msg,[Monitor|Mons]) when node() == node(Monitor) -> 
    Monitor ! Msg,
    node_mon_send(Msg,Mons);
node_mon_send(Msg,[_|Mons]) -> 
    node_mon_send(Msg,Mons);
node_mon_send(_,[]) ->
    true.

%% Send Msg to all Meta-processes in the PidTuple-list
%%
pid_send(Msg,[PidTuple|Pids]) ->
    Meta = element(2,PidTuple),
    Meta ! Msg,
    pid_send(Msg,Pids);
pid_send(_,[]) ->
    true.


notify_meta_pid(Pid,Pids, Module) ->
    {value,Tuple} = lists:keysearch(Pid,1,Pids),
    Meta = element(2,Tuple),
    Meta ! {old_code, Module}.


new_status(Meta,Pids,Status,Data) ->
    {value,Tuple} = lists:keysearch(Meta,2,Pids),
    T = setelement(4,Tuple,Status),
    setelement(5,T,Data).

new_status(Meta,Pids,Status,Data,ExitInfo) ->
    {value,Tuple} = lists:keysearch(Meta,2,Pids),
    T = setelement(4,Tuple,Status),
    T1 = setelement(5,T,Data),
    setelement(6,T1,ExitInfo).

followed(Meta,Pid,Fol,Auto) ->
    case lists:keysearch(Pid,1,Fol) of
	{value,{Pid,{Mod,Fnk,AttP}}} ->
	    Meta ! {interpret,your_state,init_break},
	    case node(AttP) of
		Node when Node == node() ->
		    catch spawn(Mod,Fnk,[false,Pid,Meta]),
		    lists:keydelete(Pid,1,Fol);
		Node ->
		    catch spawn(Mod,Fnk,[Node,Pid,Meta]),
		    lists:keydelete(Pid,1,Fol)
	    end;
	_ when Auto == false ->
	    Meta ! {interpret,your_state,running},
	    Fol;
	_ ->
	    {Fls,{Mod,Fnk,Node}} = Auto,
	    case lists:member(init,Fls) of
		true ->
		    Meta ! {interpret,your_state,init_break},
		    case node() of
			Node ->
			    catch spawn(Mod,Fnk,[false,Pid,Meta]),
			    Fol;
			_ ->
			    catch spawn(Mod,Fnk,[Node,Pid,Meta]),
			    Fol
		    end;
		_ ->
		    Meta ! {interpret,your_state,running},
		    Fol
	    end
    end.

auto_break_att(false,_,_,_) ->
    false;
auto_break_att(_,_,_,false) ->
    false;
auto_break_att(_,Pid,Meta,{Fls,{Mod,Fnk,Node}}) ->
    case lists:member(break,Fls) of
	true ->
	    case node() of
		Node ->
		    catch spawn(Mod,Fnk,[false,Pid,Meta]);
		_ ->
		    catch spawn(Mod,Fnk,[Node,Pid,Meta])
	    end;
	_ ->
	    false
    end.

auto_exit_att(false,_,_,_,_) ->
    false;
auto_exit_att(_,_,_,_,false) ->
    false;
auto_exit_att(_,Pid,Reason,ExitInfo,{Fls,{Mod,Fnk,Node}}) ->
    case lists:member(exit,Fls) of
	true ->
	    case node() of
		Node ->
		    catch spawn(Mod,Fnk,[false,Pid,Reason,ExitInfo]);
		_ ->
		    catch spawn(Mod,Fnk,[Node,Pid,Reason,ExitInfo])
	    end;
	_ ->
	    false
    end.

timeout([]) -> infinity;
timeout(_)  -> 1000.
	 
still_active(Fol) -> still_active(Fol,processes()).
   
still_active([{Pid,Info}|Fol],Procs) ->
    case lists:member(Pid,Procs) of
	true ->
	    [{Pid,Info}|still_active(Fol,Procs)];
	_ ->
	    still_active(Fol,Procs)
    end;
still_active([],_) ->
    [].

member(Pid,Pids) ->
    case lists:keysearch(Pid,1,Pids) of
	false ->
	    false;
	{value,PidTuple} ->
	    element(2,PidTuple)   % Meta
    end.

send_pids([PidTuple|Pids],Mon_Pid) ->
    Mon_Pid ! {self(),new_proc,PidTuple},
    send_pids(Pids,Mon_Pid);
send_pids([],_) ->
    true.

clear(Pids,Mons) ->
    node_mon_send({self(),clear},Mons),
    remove_exited(Pids).

% Check status and remove exited processes
remove_exited([PidTuple|Pids]) ->
    case element(4,PidTuple) of    
	exit ->
	    dbg_idb:rm_usage(element(1,PidTuple)), % Remove usage in DB.
	    remove_exited(Pids);
	_ ->
	    [PidTuple|remove_exited(Pids)]
    end;
remove_exited([]) ->
    [].

%%% =================================================
%%% Delete a module from the database.
%%% =================================================

del_module(Mod) ->
    dbg_idb:del_mod(Mod).

%%% =================================================
%%% Load a new module into the database.
%%% =================================================

load_mod(Mod,File,Binary) ->
    dbg_iload:load_mod(Mod, File, Binary).

int_module(Mod) ->
    ok.
