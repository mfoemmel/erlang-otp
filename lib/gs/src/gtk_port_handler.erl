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
%% ------------------------------------------------------------
%%
%% This is a driver for the 'gtk' application modified to 
%% handle events for gs. 'gtk' is a modified standalone wish.
%% 
%% ------------------------------------------------------------

-module(gtk_port_handler).

-include("gtk.hrl").

%% ------------------------------------------------------------
%%                  DEBUG FUNCTIONS 
%% ------------------------------------------------------------
-export([exec/2,exec/1,call/2, start_link/1,bitmap_dir/0,image_dir/0,bin_dir/0,
	gs_dir/0,init/1,init_nt/1, ping/1,stop/1]).

-define(DEBUGLEVEL, 0).
-define(FRONTEND,gs).
-define(MAXBUF, 5).

dbg(DbgLvl, Format, Data) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format(lists:append("DBG: ", Format), Data);
dbg(DbgLvl, Format, Data) -> ok.

dbg_str(DbgLvl, Format, Str) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format(lists:append("DBG: ", Format), [lists:flatten(Str)]);
dbg_str(DbgLvl, Format, Data) -> ok.

%% ------------------------------------------------------------
%%                  INTERFACE FUNCTIONS 
%% ------------------------------------------------------------

start_link(Gtk) ->
    case os:type() of
	{win32,_} ->
	    {ok, spawn_link(gtk_port_handler, init_nt, [Gtk])};
	_  ->
	    Pid = spawn_link(gtk_port_handler, init, [Gtk]),
	    receive
		{Pid, ok} ->
		    %%io:format("Backend ok!~n"),
		    {ok, Pid};
		{Pid, error, Reason} ->
		    %%io:format("Error: ~p~n", [Reason]),
		    {error, Reason}
	    end
    end.


call(PortHandler, Cmd) ->
    dbg_str(1, "CALL: ~p~n", [Cmd]),
    PortHandler ! {call, ["erlcall {",Cmd,$},$\n]},
    receive
        {result, Result}         -> 
            dbg(1, "REPLY: ~p~n", [Result]),
            {result,     Result};
        {bad_result, Bad_Result} ->
            dbg(1, "BAD REPLY: ~p~n", [Bad_Result]),
            {bad_result, Bad_Result}
    end.

ping(PortHandler) ->
    PortHandler ! {ping, self()},
    receive
	{pong, From,Port} -> {ok,Port}
    end.

stop(PortHandler) ->
    PortHandler ! {stop,self()},
    receive
	{stopped,PortHandler} -> ok
    end.

%% Purpose: asyncron call to tk 
exec(PortHandler, Cmd) ->
    dbg_str(1, "EXEC: ~p~n", [Cmd]),
    PortHandler ! {exec, ["erlexec {",Cmd,"}\n"]}.

% in gtk context, but I don't want "ifndef nt40" in other 
% modules than this one.
exec(Cmd) ->
    dbg_str(1, "EXEC: ~p~n", [Cmd]),
    get(port) ! {get(port_handler),{command,["erlexec {",Cmd,"}\n"]}},
    ok.


gs_dir()->
    filename:dirname(code:which(gtk_port_handler)).

bitmap_dir() -> lists:append(gs_dir(), "/../priv/bitmap").
image_dir() -> lists:append(gs_dir(), "/../priv/image").
bin_dir() -> lists:append(gs_dir(), "/../priv/bin").

%% ------------------------------------------------------------
%% The server
%% ------------------------------------------------------------

%%----------------------------------------------------------------------
%% gtk: is the pid of the gtk process that started me. 
%%      all my input (from the port) is forwarded to it.
%%----------------------------------------------------------------------
-record(state,{port,killer,gtk}).

init(Gtk) ->
    process_flag(trap_exit,true),
    OsType = case os:getenv("OSTYPE") of
		 false -> "";
		 OsTypeValueString when list(OsTypeValueString) ->
		     OsTypeValueString --"\n"
	     end,
    BinDir = bin_dir() ++ "/",
    Cmd = choose_file(BinDir,"wrap",OsType) ++ " " ++
	choose_file(BinDir,"gtk",OsType),
    Port = open_port({spawn, Cmd},[]),
    link(Port),
    UnixPidStr = 
	receive
	    {Port, {data, Str}} -> Str
	end,
    KillerCmd = choose_file(BinDir,"killer",OsType) ++ " SIGTERM "++UnixPidStr,
    Killer = open_port({spawn, KillerCmd}, [out]),
    link(Killer),

    %% Check whether the port, i.e., the wish, still lives! If the DISPLAY
    %% variable isn't set correctly, and/or xhost doesn't allow connection,
    %% the port may be dead by now!

    Port ! {self(), {command, ["erlcall {info tclversion}\n"]}},
    receive
	{Port, {data, [2 | T]}} ->
	    Gtk ! {self(), ok};
	{Port, {data, [3 | T]}} ->
	    Gtk ! {self(),error, error_in_backend},
	    exit(normal);
	{'EXIT', Pid, Reason} ->
	    Gtk ! {self(), error, backend_died},
	    exit(normal)
    end,

    Port ! {self(), {command, ["erlexec {option read ",gs_dir(),
			       "/gs-color-xdefaults", "}\n"]}},

%    ok=io:format("~n!!! 2 idle self:~w,P1:~w,P2:~w~n",[self(),Port,Killer]),
    idle(#state{port=Port, killer=Killer, gtk=Gtk},[]).

	
%%----------------------------------------------------------------------
%% Makes it possible to install binaries for different os in the same 
%% priv/bin directory.
%% This is a temporary solution. The correct solution is to use
%% os:type/0 and os:architecture/0 (that doesn't exist yet) instead of
%% the $OSTYPE variable.
%%----------------------------------------------------------------------
choose_file(Dir,File,OsType) ->
    case file:read_file_info(F1 = (Dir++File++"-"++OsType)) of
	{ok,_FileInfo} ->
	    F1;
	NotFound ->
	    case file:read_file_info(F2 = (Dir++File)) of
		{ok,_FileInfo} ->
		    F2;
		NotFound2 -> exit({choose_file,Dir,File,OsType,NotFound2})
	    end
    end.

init_nt(Gtk) ->
    process_flag(trap_exit,true),
    {ok, BD,_} = regexp:gsub(bin_dir(), [92,92], "/"),
    Cmd =
	case os:getenv("GS_USE_PORT_PROGRAM") of
	    false ->
		["gs__drv__ ", BD, "/gtk_srv.tcl"];
	    _ ->
		[BD, "/wish_shell.exe ",BD,"/gtk_srv.tcl"]
	end,
    P = open_port({spawn, lists:flatten(Cmd)}, []),
    P ! {self(), {command, ["erlexec {source ",BD, "/gtk.tcl}\n"]}},
    idle(#state{port=P, gtk=Gtk},[]).


%% ------------------------------------------------------------
%% The main loop
%%

idle_init(State, Buffer) ->
    {false, Rest} = handle_idle_input(State,Buffer, []),
    idle(State, Rest).

idle(State, Buffer) ->
    receive

	{call, Cmd} ->
	    got_call(State, Cmd, 0, ?MAXBUF, Buffer);

	{exec, Cmd} ->
	    got_exec(State, Cmd, 0, ?MAXBUF, Buffer);
	{Port, {data, Input}} ->
	    dbg(2, "INPUT:  ~p~n", [Input]),
	    {false, Rest} = handle_idle_input(State,Input, Buffer),
	    idle(State, Buffer);

	{ping,From} -> From ! {pong,self(),State#state.port},
		       idle(State,Buffer);
	{stop,From} -> From ! {stopped,self()};
	{'EXIT',Pid,Reason} ->
	    %%io:format("Port died when in idle loop!~n"),
	    exit({port_handler,Pid,Reason});
	Other ->
	    gs:error("gtk_port_handler: got other: ~w~n",[Other]),
	    idle(State, Buffer)
    end.


handle_idle_input(State,Input,Buffer) ->
    case in_split(Input,Buffer) of
	{true, Reply, New_buffer} ->
	    input_when_idle(State#state.gtk, Reply),
	    handle_idle_input(State,New_buffer,[]);
	Other ->
	    Other
    end.

got_exec(State, Cmd, Pending, Maxbuf, Buffer) ->
    case Pending of
	Maxbuf -> 
	    output(State#state.port, Cmd),
	    idle(State, Buffer);
	_ ->
	    receive

		{call, Cmd2} ->
		    got_call(State, [Cmd, $;, Cmd2],
			     Pending+1, Maxbuf, Buffer);
		{'EXIT',Pid,Reason} -> exit({port_handler,Pid,Reason});
		{exec, Cmd2} ->
		    got_exec(State, [Cmd, $;, Cmd2], 
			     Pending+1, Maxbuf, Buffer)
	    after 0 ->
		    output(State#state.port, Cmd),
		    idle(State, Buffer)
	    end
    end.

  
got_call(State, Cmd, Pending, Maxbuf, Buffer) ->
    case Pending of
	Maxbuf -> 
	    output(State#state.port, Cmd),
	    wait_reply(State, Buffer);
	_ -> 
	    receive
		{'EXIT',Pid,Reason} -> exit({port_handler,Pid,Reason});
		{exec, Cmd2} ->
		    got_call(State, [Cmd, $;, Cmd2], 
			     Pending+1, Maxbuf, Buffer)
	    after 0 ->
		    output(State#state.port, Cmd),
		    wait_reply(State, Buffer)
	    end
    end.


wait_reply(State, Buffer) ->
    receive

	{exec, Cmd} ->
	    output(State#state.port, Cmd),
	    wait_reply(State, Buffer);
	{'EXIT',Pid,Reason} -> exit({port_handler,Pid,Reason});

	{Port, {data, Input}} ->
	    dbg(2, "INPUT2:  ~p~n", [Input]),
	    case handle_input(State,Input,Buffer) of
		{true, New_buffer} ->
		    idle_init(State, New_buffer);
		{false,New_buffer} ->
		    wait_reply(State, New_buffer)
	    end
    end.

handle_input(State,Input,Buffer) ->
    case in_split(Input,Buffer) of
	{true, Reply, New_buffer} ->
	    case input_during_call(State#state.gtk,Reply) of
		true  -> {true, New_buffer};
		false -> handle_input(State, New_buffer, [])
	    end;
	Other ->
	    Other
    end.
	

%%
%% Handle incoming data
%% 1 - Event
%% 2 - Reply from call
%% 3 - Bad reply from call
%% 4 - Error
%% 5 - End of message
%% 

input_when_idle(GtkPid, [E|Event]) ->
    case E of
	1 ->
	    handle_event(GtkPid,Event);
	2 ->
	    gs:error("~w: unexpected reply: ~s~n",[gtk, Event]);
	3 ->
	    gs:error("~w: unexpected error reply: ~s~n",[gtk, Event]);
	4 ->
	    dbg(1, "~w: error in input : ~s~n",[gtk, Event])
    end;
input_when_idle(_,[]) ->
    true.


input_during_call(GtkPid, [E|Event]) ->
    case E of
	1 ->
	    handle_event(GtkPid,Event),
	    false;
	2 ->
	    GtkPid ! {result, Event},
	    true;
	3 ->
	    GtkPid ! {bad_result, Event},
	    true;
	4 ->
	    gs:error("~w: error in input : ~s~n",[gtk, Event]),
	    false
    end.



%% all below 6 reserved for protocol, but not checked


in_split([5|T], Ack)             -> {true, lists:reverse(Ack), T};
in_split([X|T],  Ack)            -> in_split(T, [X|Ack]);
in_split([], Ack)                -> {false, Ack}.




%% ---------------------------------------------
%% output a command to the port
%% buffer several incoming execs
%%
output(Port, Cmd) ->
    %%io:format("Command: ~p~n", [Cmd]),
    Port ! {self(), {command, Cmd}}.

handle_event(GtkPid, Bytes) when list(Bytes) ->
    Event = tcl2erl:parse_event(Bytes),
    gtk:event(GtkPid, Event). %% Event is {ID, Etag, Args}
    
