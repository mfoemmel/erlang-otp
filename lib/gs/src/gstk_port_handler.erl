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
%% This is a driver for the 'gstk' application modified to 
%% handle events for gs. 'gstk' is a modified standalone wish.
%% 
%% FIXME
%% mkdir tcl ; cd tcl
%% ( cd /usr/local/pgm/tcl-8.3.3 ; tar -cf - * ) | tar -xf -
%% ( cd /usr/local/pgm/tk-8.3.3 ; tar -cf - * ) | tar -xf -
%% rm -fr include man bin/tclsh
%% cd ..
%% tar -cf tcl.tar *
%%
%% ------------------------------------------------------------

-module(gstk_port_handler).

-include("gstk.hrl").

% The executable can have many names. There is not always
% a plain "wish" program. 
% FIXME There has to be a better solution....
% FIXME Add option in app file or environmen  variable.

-define(WISHNAMES, ["wish84","wish8.4",
		    "wish83","wish8.3",
		    "wish82","wish8.2",
		    "wish"]).

%% ------------------------------------------------------------
%%                  DEBUG FUNCTIONS 
%% ------------------------------------------------------------
-export([exec/2,exec/1,call/2,
	 start_link/1,init/1,init_win32/1,ping/1,stop/1]).
-export([wait_for_connection/2]).

-define(START_TIMEOUT , 1000 * 30).
-define(ACCEPT_TIMEOUT, 1000 * 20).
-define(MAXBUF, 5).

-define(DEBUGLEVEL, 4).

-ifdef(DEBUG).

-define(DBG(DbgLvl,Format, Data),dbg(DbgLvl, Format, Data)).
-define(DBG_STR(DbgLvl, What, Str),dbg_str(DbgLvl, What, Str)).

dbg(DbgLvl, Format, Data) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format("DBG: " ++ Format, Data);
dbg(_DbgLvl, _Format, _Data) -> ok.

dbg_str(DbgLvl, What, Str) when DbgLvl =< ?DEBUGLEVEL ->
    ok = io:format("DBG: ~s ~s\n", [What,dbg_s(Str)]);
dbg_str(_DbgLvl, _What, _Data) -> ok.

dbg_s([]) ->
    [];
dbg_s([C | Str]) when list(C) ->
    [dbg_s(C) | dbg_s(Str)];
dbg_s([C | Str]) when C >= 20, C < 255 ->
    [C | dbg_s(Str)];
dbg_s([$\n | Str]) ->
    ["\\n" | dbg_s(Str)];
dbg_s([$\r | Str]) ->
    ["\\r" | dbg_s(Str)];
dbg_s([$\t | Str]) ->
    ["\\t" | dbg_s(Str)];
dbg_s([C | Str]) when integer(C) ->
    [io_lib:format("\\~.3.0w",[C]) | dbg_s(Str)].

-else.

-define(DBG(DbgLvl,Format, Data), true).
-define(DBG_STR(DbgLvl, What, Str), true).

-endif.

%% ------------------------------------------------------------
%%                  INTERFACE FUNCTIONS 
%% ------------------------------------------------------------

start_link(Gstk) ->
    InitFunc = 
	case os:type() of
	    {win32,_} ->
		init_win32;
	    _  ->
		init
	end,
    Pid = spawn_link(gstk_port_handler, InitFunc, [Gstk]),
    receive
	{Pid, ok} ->
	    {ok, Pid};
	{Pid, error, Reason} ->
	    {error, Reason}
    after ?START_TIMEOUT ->
	    {error, timeout}
    end.

call(PortHandler, Cmd) ->
    ?DBG_STR(1, "call: ~p~n", [Cmd]),
    PortHandler ! {call, ["erlcall {",Cmd,$},$\n]},
    receive
        {result, Result}         -> 
            ?DBG(1, "call reply: ~p~n", [Result]),
            {result,     Result};
        {bad_result, Bad_Result} ->
            ?DBG(1, "bad call reply: ~p~n", [Bad_Result]),
            {bad_result, Bad_Result}
    end.

ping(PortHandler) ->
    PortHandler ! {ping, self()},
    receive
	{pong,_From,PortOrSock} -> {ok,PortOrSock}
    end.

stop(PortHandler) ->
    PortHandler ! {stop,self()},
    receive
	{stopped,PortHandler} -> ok
    end.

%% Purpose: asyncron call to tk 
exec(PortHandler, Cmd) ->
    ?DBG_STR(1, "exec porthandler:", ["erlexec {",Cmd,"}"]),
    PortHandler ! {exec, ["erlexec {",Cmd,"}\n"]}.

% in gstk context, but I don't want "ifndef nt40" in other 
% modules than this one.
exec(Cmd) ->
    ?DBG_STR(1, "exec:", ["erlexec {",Cmd,"}"]),
    case get(port) of
	{socket,Sock} ->
	    gen_tcp:send(Sock, ["erlexec {",Cmd,"}\n"]);
	{port,Port} ->
	    Port ! {get(port_handler),{command,["erlexec {",Cmd,"}\n"]}}
    end,
    ok.

%% ------------------------------------------------------------
%% The server
%% ------------------------------------------------------------

%%----------------------------------------------------------------------
%% gstk: is the pid of the gstk process that started me. 
%%      all my input (from the port) is forwarded to it.
%%----------------------------------------------------------------------
-record(state,{out,gstk}).

init(Gstk) ->
    process_flag(trap_exit,true),

    % ------------------------------------------------------------
    % Set up paths
    % ------------------------------------------------------------

    PrivDir = code:priv_dir(gs),
    TclDir = filename:join(PrivDir,"tcl"),
    TclBinDir = filename:join(TclDir,"bin"),
    TclLibDir = filename:join(TclDir,"lib"),

    InitScript = filename:nativename(filename:join(PrivDir,"gstk_unix.tcl")),

    % ------------------------------------------------------------
    % Search for wish in priv and in system search path
    % ------------------------------------------------------------

    ?DBG(1, "TclBinDir :\n~p\n\n", [TclBinDir]),
    ?DBG(1, "Wish      :\n~p\n\n", [filename:join(TclBinDir,"wish*")]),

    {Wish,Options} = 
	case filelib:wildcard(filename:join(TclBinDir,"wish*")) of
	    % If more than one wish in priv we assume they are the same
	    [PrivWish | _] ->
		% ------------------------------------------------
		% We have to set TCL_LIBRARY and TK_LIBRARY because else
		% 'wish' will search in the original installation directory
		% for 'tclIndex' and this may be an incompatible version on
		% the host we run on.
		% ------------------------------------------------

		[TclLibrary] = filelib:wildcard(filename:join(PrivDir,"tcl/lib/tcl[1-9]*")),
		[TkLibrary]  = filelib:wildcard(filename:join(PrivDir,"tcl/lib/tk[1-9]*")),

		Opts = [{env,[{"TCL_LIBRARY", TclLibrary},
			      {"TK_LIBRARY", TkLibrary},
			      {"LD_LIBRARY_PATH",TclLibDir}]}],
		{PrivWish,Opts};
	    _ ->
		% We use the system wish program
		{search_wish(?WISHNAMES, Gstk),[]}
	end,

    ?DBG(1, "Wish and options:\n~p\n\n", [{Wish,Options}]),

    Cmd = Wish ++ " " ++ InitScript,
    Port = open_port({spawn, Cmd},Options),
    link(Port),

    %% Check whether the port, i.e., the wish, still lives! If the DISPLAY
    %% variable isn't set correctly, and/or xhost doesn't allow connection,
    %% the port may be dead by now!

    Port ! {self(), {command, ["erlcall {info tclversion}\n"]}},
    receive
	{Port, {data, [2 | _T]}} ->
	    Gstk ! {self(), ok};
	{Port, {data, [3 | _T]}} ->
	    Gstk ! {self(),error, error_in_backend},
	    exit(normal);
	{'EXIT', _Pid, _Reason} ->		% FIXME: Why throw away reason?!
	    Gstk ! {self(), error, backend_died},
	    exit(normal)
    end,

    State = #state{out={port,Port}, gstk=Gstk},

%    XDefaults = filename:join(code:lib_dir(gs),"ebin/gs-color-xdefaults"),
%    output(State, ["erlexec {option read ",XDefaults,"}\n"]),

    idle(State,[]).

search_wish([], Gstk) ->
    Gstk ! {self(), error, backend_died},
    exit(normal);
search_wish([WishName | WishNames], Gstk) ->
    case os:find_executable(WishName) of
	false ->
	    search_wish(WishNames, Gstk);
	Wish ->
	    Wish
    end.
	
%%----------------------------------------------------------------------
%% XXXX
%%----------------------------------------------------------------------

init_win32(Gstk) ->
    process_flag(trap_exit,true),

    % ------------------------------------------------------------
    % Set up paths
    % ------------------------------------------------------------

    PrivDir = code:priv_dir(gs),
    TclLibrary = filename:join(PrivDir,"tcl/lib/tcl8.3"),
    TkLibrary  = filename:join(PrivDir,"tcl/lib/tk8.3"),
    Wish       = filename:nativename(filename:join(PrivDir,
						   "tcl/bin/wish83.exe")),
    InitScript = filename:nativename(filename:join(PrivDir,"gstk_win32.tcl")),

    Env = [{"TCL_LIBRARY", TclLibrary},
	   {"TK_LIBRARY", TkLibrary}],

    % ------------------------------------------------------------
    % Set up a listening socket and call accept in another process
    % ------------------------------------------------------------

    Opts =
        [
         {nodelay, true},
         {packet,raw},
         {reuseaddr,true}
        ],
    {ok,ListenSocket} = gen_tcp:listen(0, Opts), % Let OS pick a number
    {ok,ListenPort} = inet:port(ListenSocket),

    % Wait in another process
    spawn_link(?MODULE,wait_for_connection,[self(),ListenSocket]),

    % ------------------------------------------------------------
    % Start the wish program with arguments
    % ------------------------------------------------------------

    Cmd = Wish ++ " " ++ InitScript ++ " -- " ++
	integer_to_list(ListenPort),

    ?DBG(1, "open port: ~s~n", [Cmd]),
    
    case open_port({spawn, Cmd}, [{env,Env}]) of
	Port when port(Port) ->
	    true;
	{error,_Reason1} ->			% FIXME: Why throw away reason?!
	    Gstk ! {self(), error, backend_died},
	    exit(normal)
    end,

    % ------------------------------------------------------------
    % Wait for a connection
    % ------------------------------------------------------------

    Sock =
	receive
	    {connected,Socket} ->
		Gstk ! {self(), ok},
		Socket;
	    {'EXIT', _Pid, _Reason2} ->		% FIXME: Why throw away reason?!
		Gstk ! {self(), error, backend_died},
		exit(normal)
	end,

    ?DBG(1,"Got socket ~p~n",[Sock]),

    % ------------------------------------------------------------
    % Call idle loop with state data
    % ------------------------------------------------------------

    State = #state{out={socket,Sock}, gstk=Gstk},

    ?DBG(1,"Calling idle ~p~n",[State]),

    idle(State,[]).


wait_for_connection(CallerPid, ListenSocket) ->
    {ok,Sock} = gen_tcp:accept(ListenSocket, ?ACCEPT_TIMEOUT),
    ?DBG(1,"Got accept ~p~p~n",[self(),Sock]),
    ok = gen_tcp:controlling_process(Sock,CallerPid),
    CallerPid ! {connected,Sock}.


%% ===========================================================================
%% The main loop
%% ===========================================================================

idle_init(State, Buffer) ->
    {false, Rest} = handle_idle_input(State, Buffer, []),
    idle(State, Rest).

idle(State, Buffer) ->
    receive

	{call, Cmd} ->
	    got_call(State, Cmd, 0, ?MAXBUF, Buffer);

	{exec, Cmd} ->
	    got_exec(State, Cmd, 0, ?MAXBUF, Buffer);

	{_Port, {data, Input}} ->
	    ?DBG(2, "INPUT:  ~p~n", [Input]),
	    {false, _Rest} = handle_idle_input(State, Input, Buffer),
	    % FIXME: What is in Rest that we skip?
	    idle(State, Buffer);

	{tcp, _Sock, Input} ->
	    ?DBG_STR(2, "idle from socket:", [Input]),
	    {false, New_Buffer} = handle_idle_input(State, Input, Buffer),
	    idle(State, New_Buffer);

	{ping,From} ->
	    From ! {pong,self(),State#state.out},
	    idle(State,Buffer);

	{stop,From} ->
	    From ! {stopped,self()};

	{'EXIT',Pid,normal} ->
	    idle(State,Buffer);

	{'EXIT',Pid,Reason} ->
	    %%io:format("Port died when in idle loop!~n"),
	    ?DBG(1,"EXIT msg ~p~n~p~n",[Pid,Reason]),
	    exit({port_handler,Pid,Reason});

	Other ->
	    gs:error("gstk_port_handler: got other: ~w~n",[Other]),
	    idle(State, Buffer)
    end.


handle_idle_input(State,Input,Buffer) ->
    case in_split(Input,Buffer) of
	{true, Reply, New_buffer} ->
	    input_when_idle(State#state.gstk, Reply),
	    handle_idle_input(State,New_buffer,[]);
	Other ->
	    Other
    end.

got_exec(State, Cmd, Pending, Maxbuf, Buffer) ->
    case Pending of
	Maxbuf -> 
	    output(State, Cmd),
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
		    output(State, Cmd),
		    idle(State, Buffer)
	    end
    end.

  
got_call(State, Cmd, Pending, Maxbuf, Buffer) ->
    case Pending of
	Maxbuf -> 
	    output(State, Cmd),
	    wait_reply(State, Buffer);
	_ -> 
	    receive
		{'EXIT',Pid,Reason} -> exit({port_handler,Pid,Reason});
		{exec, Cmd2} ->
		    got_call(State, [Cmd, $;, Cmd2], 
			     Pending+1, Maxbuf, Buffer)
	    after 0 ->
		    output(State, Cmd),
		    wait_reply(State, Buffer)
	    end
    end.


wait_reply(State, Buffer) ->
    receive

	{exec, Cmd} ->
	    output(State, Cmd),
	    wait_reply(State, Buffer);
	{'EXIT',Pid,Reason} -> exit({port_handler,Pid,Reason});

	{_Port, {data, Input}} ->
	    ?DBG_STR(2, "wait reply from port:", [Input]),
	    case handle_input(State,Input,Buffer) of
		{true, New_buffer} ->
		    idle_init(State, New_buffer);
		{false,New_buffer} ->
		    wait_reply(State, New_buffer)
	    end;

	{tcp, _Sock, Input} ->
	    ?DBG_STR(2, "wait reply from socket:", [Input]),
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
	    case input_during_call(State#state.gstk,Reply) of
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

input_when_idle(GstkPid, [E|Event]) ->
    case E of
	1 ->
	    handle_event(GstkPid,Event);
	2 ->
	    gs:error("~w: unexpected reply: ~s~n",[gstk, Event]);
	3 ->
	    gs:error("~w: unexpected error reply: ~s~n",[gstk, Event]);
	4 ->
	    ?DBG(1, "~w: error in input : ~s~n",[gstk, Event])
    end;
input_when_idle(_,[]) ->
    true.


input_during_call(GstkPid, [E|Event]) ->
    case E of
	1 ->
	    handle_event(GstkPid,Event),
	    false;
	2 ->
	    GstkPid ! {result, Event},
	    true;
	3 ->
	    GstkPid ! {bad_result, Event},
	    true;
	4 ->
	    gs:error("~w: error in input : ~s~n",[gstk, Event]),
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
output(#state{out = {socket,Sock}}, Cmd) ->
    ?DBG_STR(1, "to socket:", [Cmd]),
    ok = gen_tcp:send(Sock, Cmd);

output(#state{out = {port,Port}}, Cmd) ->
    ?DBG_STR(1, "to port:", [Cmd]),
    Port ! {self(), {command, Cmd}}.

handle_event(GstkPid, Bytes) when list(Bytes) ->
    Event = tcl2erl:parse_event(Bytes),
    gstk:event(GstkPid, Event). %% Event is {ID, Etag, Args}
