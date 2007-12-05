%%<copyright>
%% <year>2007-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_daemon).

-behaviour(gen_server).

-include("ssh.hrl").
-include("ssh_connect.hrl").

%% API
-export([listen/1, listen/2, listen/3, listen/4, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% defines
-define(DBG_IO_REQUEST, true).

%% state
-record(state, {
	  cm,
	  channel,
	  pty,
	  group,
	  buf,
	  opts
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: listen(...) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts a listening server
%% Note that the pid returned is NOT the pid of this gen_server;
%% this server is started when an SSH connection is made on the
%% listening port
%%--------------------------------------------------------------------
listen() ->
    listen(22).

listen(Port) ->
    listen(Port, []).

listen(Port, Opts) ->
    listen(any, Port, Opts).

listen(Addr, Port, Opts) ->
    ssh_cm:listen(
      fun() ->
	      {ok, Pid} =
		  gen_server:start_link(?MODULE, [Opts], []),
	      Pid
      end, Addr, Port, Opts).

%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop(Pid) ->
    ssh_cm:stop_listener(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Opts]) ->
    {ok, #state{opts = Opts}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    Result = ssh_cm:stop(State#state.cm),
    {stop, normal, Result, State};
handle_call(info, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({ssh_cm, CM, {open, Channel, _RemoteChannel, {session}}}, State) ->
    ?dbg(true, "session open: self()=~p CM=~p Channel=~p Opts=~p\n",
	 [self(), CM, Channel, Opts]),
    process_flag(trap_exit, true),
    {noreply,
     State#state{cm = CM, channel = Channel}};
handle_info({ssh_cm, CM, {data, Channel, _Type, Data}}, State) ->
    ssh_cm:adjust_window(CM, Channel, size(Data)),
    State#state.group ! {self(), {data, binary_to_list(Data)}},
    {noreply, State};
handle_info({ssh_cm, _CM, {pty, _Channel, _WantReply, Pty}}, State) ->
    {noreply, State#state{pty = Pty}};
handle_info({ssh_cm, _CM,
	     {window_change, _Channel, Width, Height, PixWidth, PixHeight}},
	    State) ->
    #state{buf = Buf, pty = Pty, cm = CM, channel = Channel} = State,
    NewPty = Pty#ssh_pty{width = Width, height = Height,
			 pixel_width = PixWidth,
			 pixel_height = PixHeight},
    {Chars, NewBuf} = io_request({window_change, Pty}, Buf, NewPty),
    write_chars(CM, Channel, Chars),
    {noreply, State#state{pty = NewPty, buf = NewBuf}};
handle_info({Group, Req}, State) when Group==State#state.group ->
    ?dbg(?DBG_IO_REQUEST, "io_request: ~w\n", [Req]),
    #state{buf = Buf, pty = Pty, cm = CM, channel = Channel} = State,
    {Chars, NewBuf} = io_request(Req, Buf, Pty),
    write_chars(CM, Channel, Chars),
    {noreply, State#state{buf = NewBuf}};
handle_info({ssh_cm, CM, {shell}}, State) ->
    Shell = proplists:get_value(shell, State#state., {shell, start, []}),
    ShellFun = case is_function(Shell) of
		   true ->
		       case erlang:fun_info(Shell, arity) of
			   {arity, 1} ->
			       User = ssh_userauth:get_user_from_cm(CM),
			       fun() -> Shell(User) end;
			   {arity, 2} ->
			       User = ssh_userauth:get_user_from_cm(CM),
			       {ok, PeerAddr} = ssh_cm:get_peer_addr(CM),
			       fun() -> Shell(User, PeerAddr) end;
			   _ ->
			       Shell
		       end;
		   _ ->
		       Shell
	       end,
    Group = group:start(self(), ShellFun, []),
    {noreply, State#state{group = Group}};
handle_info({ssh_cm, CM, {subsystem, _Channel, WantsReply, "sftp"}}, State) ->
    case WantsReply of
	true -> CM ! {ssh_cm, self(), {success, State#state.remote_channel}}
    end,
    {noreply, State};
handle_info({ssh_cm, _CM, {exec, Cmd}}, State) ->
    State#state.group ! {self(), {data, Cmd ++ "\n"}},
    {noreply, State};
handle_info({get_cm, From}, #state{cm=CM} = State) ->
    From ! {From, cm, CM},
    {noreply, State};
handle_info({ssh_cm, _CM, {eof, _Channel}}, State) ->
    {stop, normal, State};
handle_info({ssh_cm, _CM, {closed, _Channel}}, State) ->
    %% ignore -- we'll get an {eof, Channel} soon??
    {noreply, State};
handle_info({'EXIT', Group, normal},
	    #state{cm=CM, channel=Channel, group=Group} = State) ->
    ssh_cm:close(CM, Channel),
    ssh_cm:stop(CM),
    {stop, normal, State};
handle_info(Info, State) ->
    ?dbg(true, "~p:handle_info: BAD info ~p\n(State ~p)\n", [?MODULE, Info, State]),
    {stop, {bad_info, Info}, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
