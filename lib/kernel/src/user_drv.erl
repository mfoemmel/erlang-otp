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
-module(user_drv).

%% Basic interface to a port.

-export([start/0,start/1,start/2,start/3,server/2,server/3]).

%% start()
%% start(ArgumentList)
%% start(PortName, Shell)
%% start(InPortName, OutPortName, Shell)
%%  Start the user dirver server. The arguments to start/1 are slightly
%%  strange as this may be called both at start up from the command line
%%  and explicitly from other code.

start() ->					%Default line editing shell
    spawn(user_drv, server, ['tty_sl -c -e',{shell,start,[]}]).

start([Pname]) ->
    spawn(user_drv, server, [Pname,{shell,start,[]}]);
start([Pname|Args]) ->
    spawn(user_drv, server, [Pname|Args]);
start(Pname) ->
    spawn(user_drv, server, [Pname,{shell,start,[]}]).

start(Pname, Shell) ->
    spawn(user_drv, server, [Pname,Shell]).

start(Iname, Oname, Shell) ->
    spawn(user_drv, server, [Iname,Oname,Shell]).

%% server(Pid, Shell)
%% server(Pname, Shell)
%% server(Iname, Oname, Shell)
%%  The initial calls to run the user driver. These start the port(s)
%%  then call server1/3 to set everything else up.

server(Pid, Shell) when pid(Pid) ->
    server1(Pid, Pid, Shell);

server(Pname, Shell) ->
    process_flag(trap_exit, true),
    case catch open_port({spawn,Pname}, [eof]) of
	{'EXIT', _} ->
	    %% Let's try a dumb user instead
	    user:start();
	Port ->
	    server1(Port, Port, Shell)
    end.

server(Iname, Oname, Shell) ->
    process_flag(trap_exit, true),
    case catch open_port({spawn,Iname}, [eof]) of
	{'EXIT', _} -> %% It might be a dumb terminal lets start dumb user
	    user:start();
	Iport ->
	    Oport = open_port({spawn,Oname}, [eof]),
	    server1(Iport, Oport, Shell)
    end.

server1(Iport, Oport, Shell) ->
    put(eof, false),
    %% Start user and initial shell.
    User = start_user(),
    Gr1 = gr_add_cur(gr_new(), User, {}),
    Curr = group:start(self(), Shell),
    Gr = gr_add_cur(Gr1, Curr, Shell),
    %% Print some information.
    io_request({put_chars,
		flatten(io_lib:format("~s\n",
				      [erlang:system_info(system_version)]))},
	       Iport, Oport),
    %% Enter the server loop.
    server_loop(Iport, Oport, Curr, User, Gr).

%% start_user()
%%  Start a group leader process and register it as 'user', unless,
%%  of course, a 'user' already exists.

start_user() ->
    case whereis(user) of
	undefined ->
	    User = group:start(self(), {}),
	    register(user, User),
	    User;
	User ->
	    User
    end.
   
server_loop(Iport, Oport, User, Gr) ->
    server_loop(Iport, Oport, gr_cur_pid(Gr), User, Gr).

server_loop(Iport, Oport, Curr, User, Gr) ->
    receive
	{Iport,{data,Bs}} ->
	    port_bytes(Bs, Iport, Oport, Curr, User, Gr);
	{Iport,eof} ->
	    Curr ! {self(),eof},
	    server_loop(Iport, Oport, Curr, User, Gr);
	{User,Req} ->				%Never block from user!
	    io_request(Req, Iport, Oport),
	    server_loop(Iport, Oport, Curr, User, Gr);
	{Curr,Req} ->
	    io_request(Req, Iport, Oport),
	    server_loop(Iport, Oport, Curr, User, Gr);
	{'EXIT',Iport,R} ->
	    server_loop(Iport, Oport, Curr, User, Gr);
	{'EXIT',Oport,R} ->
	    server_loop(Iport, Oport, Curr, User, Gr);
	{'EXIT',User,R} ->			%Keep 'user' alive
	    NewU = start_user(),
	    server_loop(Iport, Oport, Curr, NewU, gr_set_num(Gr, 1, NewU, {}));
	{'EXIT',Pid,R} ->
	    server_loop(Iport, Oport, Curr, User, gr_del_pid(Gr, Pid));
	X ->
	    %% Ignore unknown messages.
	    server_loop(Iport, Oport, Curr, User, Gr)
    end.

%% port_bytes(Bytes, InPort, OutPort, CurrentProcess, UserProcess, Group)
%%  Check the Bytes from the port to see if it contains a ^G which
%%  break to switch_loop else send the bytes to Curr.

port_bytes([$\^G|Bs], Iport, Oport, Curr, User, Gr) ->
    io_request({put_chars,"\nUser switch command\n"}, Iport, Oport),
    server_loop(Iport, Oport, User, switch_loop(Iport, Oport, Gr));
port_bytes([B], Iport, Oport, Curr, User, Gr) ->
    Curr ! {self(),{data,[B]}},
    server_loop(Iport, Oport, Curr, User, Gr);
port_bytes(Bs, Iport, Oport, Curr, User, Gr) ->
    case member($\^G, Bs) of
	true ->
	    io_request({put_chars,"\nUser switch command\n"}, Iport, Oport),
	    server_loop(Iport, Oport, User, switch_loop(Iport, Oport, Gr));
	false ->
	    Curr ! {self(),{data,Bs}},
	    server_loop(Iport, Oport, Curr, User, Gr)
    end.

switch_loop(Iport, Oport, Gr) ->
    Line = get_line(edlin:start(" --> "), Iport, Oport),
    switch_cmd(erl_scan:string(Line), Iport, Oport, Gr).

switch_cmd({ok,[{atom,_,c},{integer,_,I}],_}, Iport, Oport, Gr0) ->
    case gr_set_cur(Gr0, I) of
	{ok,Gr} -> Gr;
	undefined -> unknown_group(Iport, Oport, Gr0)
    end;
switch_cmd({ok,[{atom,_,c}],_}, Iport, Oport, Gr) ->
    Gr;
switch_cmd({ok,[{atom,_,i},{integer,_,I}],_}, Iport, Oport, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, interrupt),
	    switch_loop(Iport, Oport, Gr);
	undefined ->
	    unknown_group(Iport, Oport, Gr)
    end;
switch_cmd({ok,[{atom,_,i}],_}, Iport, Oport, Gr) ->
    exit(gr_cur_pid(Gr), interrupt),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{atom,_,k},{integer,_,I}],_}, Iport, Oport, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, die),
	    switch_loop(Iport, Oport, Gr);
	undefined ->
	    unknown_group(Iport, Oport, Gr)
    end;
switch_cmd({ok,[{atom,_,k}],_}, Iport, Oport, Gr) ->
    exit(gr_cur_pid(Gr), die),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{atom,_,j}],_}, Iport, Oport, Gr) ->
    io_requests(gr_list(Gr), Iport, Oport),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{atom,_,s}],_}, Iport, Oport, Gr0) ->
    Pid = group:start(self(), {shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {shell,start,[]}),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{atom,_,r}],_}, Iport, Oport, Gr0) ->
    case is_alive() of
	true ->
	    Node = pool:get_node(),
	    Pid = group:start(self(), {Node,shell,start,[]}),
	    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
	    switch_loop(Iport, Oport, Gr);
	false ->
	    io_request({put_chars,"Not alive\n"}, Iport, Oport),
	    switch_loop(Iport, Oport, Gr0)
    end;
switch_cmd({ok,[{atom,_,r},{atom,_,Node}],_}, Iport, Oport, Gr0) ->
    Pid = group:start(self(), {Node,shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{atom,_,q}],_}, Iport, Oport, Gr) ->
    halt();
switch_cmd({ok,[{atom,_,h}],_}, Iport, Oport, Gr) ->
    list_commands(Iport, Oport),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[{'?',_}],_}, Iport, Oport, Gr) ->
    list_commands(Iport, Oport),
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,[],_}, Iport, Oport, Gr) ->
    switch_loop(Iport, Oport, Gr);
switch_cmd({ok,Ts,_}, Iport, Oport, Gr) ->
    io_request({put_chars,"Unknown command\n"}, Iport, Oport),
    switch_loop(Iport, Oport, Gr);
switch_cmd(Ts, Iport, Oport, Gr) ->
    io_request({put_chars,"Illegal input\n"}, Iport, Oport),
    switch_loop(Iport, Oport, Gr).

unknown_group(Iport, Oport, Gr) ->
    io_request({put_chars,"Unknown job\n"}, Iport, Oport),
    switch_loop(Iport, Oport, Gr).

list_commands(Iport, Oport) ->
    io_requests([{put_chars,"  c [nn]   - connect to job\n"},
		 {put_chars,"  i [nn]   - interrupt job\n"},
		 {put_chars,"  k [nn]   - kill job\n"},
		 {put_chars,"  j        - list all jobs\n"},
		 {put_chars,"  s        - start local shell\n"},
		 {put_chars,"  r [node] - start remote shell\n"},
		 {put_chars,"  q        - quit erlang\n"},
		 {put_chars,"  ? | h    - this message\n"}], Iport, Oport).

get_line({done,Line,Rest,Rs}, Iport, Oport) ->
    io_requests(Rs, Iport, Oport),
    Line;
get_line({undefined,Char,Cs,Cont,Rs}, Iport, Oport) ->
    io_requests(Rs, Iport, Oport),
    io_request(beep, Iport, Oport),
    get_line(edlin:edit_line(Cs, Cont), Iport, Oport);
get_line({What,Cont0,Rs}, Iport, Oport) ->
    io_requests(Rs, Iport, Oport),
    receive
	{Iport,{data,Cs}} ->
	    get_line(edlin:edit_line(Cs, Cont0), Iport, Oport);
	{Iport,eof} ->
	    get_line(edlin:edit_line(eof, Cont0), Iport, Oport)
    after
	get_line_timeout(What) ->
	    get_line(edlin:edit_line([], Cont0), Iport, Oport)
    end.	

get_line_timeout(blink) -> 1000;
get_line_timeout(more_chars) -> infinity.

%% io_request(Request, InPort, OutPort)
%% io_requests(Requests, InPort, OutPort)

io_request({put_chars,Cs}, Iport, Oport) ->
    Oport ! {self(),{command,[0|Cs]}};
io_request({move_rel,N}, Iport, Oport) ->
    Oport ! {self(),{command,[1|put_int16(N, [])]}};
io_request({insert_chars,Cs}, Iport, Oport) ->
    Oport ! {self(),{command,[2|Cs]}};
io_request({delete_chars,N}, Iport, Oport) ->
    Oport ! {self(),{command,[3|put_int16(N, [])]}};
io_request(beep, Iport, Oport) ->
    Oport ! {self(),{command,[4]}};
io_request({requests,Rs}, Iport, Oport) ->
    io_requests(Rs, Iport, Oport);
io_request(R, Iport, Oport) ->
    ok.

io_requests([R|Rs], Iport, Oport) ->
    io_request(R, Iport, Oport),
    io_requests(Rs, Iport, Oport);
io_requests([], Iport, Oport) ->
    ok.

put_int16(N, Tail) ->
    [(N bsr 8)band 255,N band 255|Tail].

%% gr_new()
%% gr_get_num(Group, Index)
%% gr_add_cur(Group, Pid, Shell)
%% gr_set_cur(Group, Index)
%% gr_cur_pid(Group)
%% gr_del_pid(Group, Pid)
%%  Manage the group list. The group structure has the form:
%%	{NextIndex,CurrIndex,CurrPid,GroupList}
%%
%%  where each element in the group list is:
%%	{Index,GroupPid,Shell}

gr_new() ->
    {1,0,none,[]}.

gr_get_num({Next,CurI,CurP,Gs}, I) ->
    gr_get_num1(Gs, I).

gr_get_num1([{I,Pid,S}|Gs], I) ->
    {pid,Pid};
gr_get_num1([G|Gs], I) ->
    gr_get_num1(Gs, I);
gr_get_num1([], I) ->
    undefined.

gr_add_cur({Next,CurI,CurP,Gs}, Pid, Shell) ->
    {Next+1,Next,Pid,append(Gs, [{Next,Pid,Shell}])}.

gr_set_cur({Next,CurI,CurP,Gs}, I) ->
    case gr_get_num1(Gs, I) of
	{pid,Pid} -> {ok,{Next,I,Pid,Gs}};
	undefined -> undefined
    end.

gr_set_num({Next,CurI,CurP,Gs}, I, Pid, Shell) ->
    {Next,CurI,CurP,gr_set_num1(Gs, I, Pid, Shell)}.

gr_set_num1([{I,Pid,Shell}|Gs], I, NewPid, NewShell) ->
    [{I,NewPid,NewShell}|Gs];
gr_set_num1([{I,Pid,Shell}|Gs], NewI, NewPid, NewShell) when NewI < I ->
    [{I,Pid,Shell}|gr_set_num1(Gs, NewI, NewPid, NewShell)];
gr_set_num1(Gs, NewI, NewPid, NewShell) ->
    [{NewI,NewPid,NewShell}|Gs].

gr_del_pid({Next,CurI,CurP,Gs}, Pid) ->
    {Next,CurI,CurP,gr_del_pid1(Gs, Pid)}.

gr_del_pid1([{I,Pid,S}|Gs], Pid) ->
    Gs;
gr_del_pid1([G|Gs], Pid) ->
    [G|gr_del_pid1(Gs, Pid)];
gr_del_pid1([], Pid) ->
    [].

gr_cur_pid({Next,CurI,CurP,Gs}) ->
    CurP.

gr_list({Next,CurI,CurP,Gs}) ->
    gr_list(Gs, CurI).

gr_list([{Cur,Pid,Shell}|Gs], Cur) ->
    [{put_chars,flatten(io_lib:format("~4w* ~w\n", [Cur,Shell]))}|
     gr_list(Gs, Cur)];
gr_list([{I,Pid,Shell}|Gs], Cur) ->
    [{put_chars,flatten(io_lib:format("~4w  ~w\n", [I,Shell]))}|
     gr_list(Gs, Cur)];
gr_list([], Cur) ->
    [].

append([H|T], X) ->
    [H|append(T, X)];
append([], X) ->
    X.

member(X, [X|Rest]) -> true;
member(X, [H|Rest]) ->
    member(X, Rest);
member(X, []) -> false.

flatten(List) ->
    flatten(List, [], []).

flatten([H|T], Cont, Tail) when list(H) ->
    flatten(H, [T|Cont], Tail);
flatten([H|T], Cont, Tail) ->
    [H|flatten(T, Cont, Tail)];
flatten([], [H|Cont], Tail) ->
    flatten(H, Cont, Tail);
flatten([], [], Tail) ->
    Tail.
