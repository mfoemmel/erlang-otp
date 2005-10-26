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

%%% Description: ssh shell client

-module(ssh_ssh).

-export([connect/1, connect/2, connect/3]).

-export([input_loop/2, shell_loop/4]).

-include("ssh.hrl").
-include("ssh_connect.hrl").

connect(CM) when is_pid(CM) ->
    case ssh_cm:attach(CM) of
	{ok,CMPid} ->
	    session(CMPid);
	Error ->
	    Error
    end;
connect(Host) when is_list(Host) ->
    connect(Host, []).

connect(Host, Opts) ->
    connect(Host, 22, Opts).

connect(Host, Port, Opts) ->
    case ssh_cm:connect(Host, Port, Opts) of
	{ok, CM} ->
	    session(CM);
	Error ->
	    Error
    end.

session(CM) ->
    case ssh_cm:session_open(CM) of
	{ok,Channel}  ->
	    case ssh_cm:shell(CM, Channel) of
		ok ->
		    {group_leader,GIO} = 
			process_info(self(), group_leader),
		    IO = spawn(?MODULE, input_loop,
			       [GIO, self()]),
		    shell_loop(CM, Channel, IO, false);
		Error  ->
		    ssh_cm:close(CM, Channel),
		    Error
	    end;
	Error ->
	    Error
    end.


input_loop(Fd, Pid) ->
    case io:get_line(Fd, '>') of
	eof ->
	    Pid ! {input, self(), eof},
	    ok; % input_loop(Fd, Pid);
	Line ->
	    Pid ! {input, self(), Line},
	    input_loop(Fd, Pid)
    end.
    

shell_loop(CM, Channel, IO, SentClose) ->
    receive
	{input, IO, eof} ->
	    ssh_cm:send_eof(CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);
	    
	{input, IO, Line} ->
	    ssh_cm:send(CM, Channel, Line),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {data, Channel, Type, Data}} ->
	    if Type == 0 ->
		    io:format("~s", [binary_to_list(Data)]);
	       Type == ?SSH_EXTENDED_DATA_STDERR ->
		    io:format("STDERR: ~s", [binary_to_list(Data)]);
	       true ->
		    ok
	    end,
	    ssh_cm:adjust_window(CM, Channel, size(Data)),
	    ?MODULE:shell_loop(CM, Channel, IO, SentClose);

	{ssh_cm, CM, {exit_signal,Channel,SIG, Err, _Lang}} ->
	    io:format("SIGNAL: ~s (~s)\n", [SIG, Err]),
	    send_close(SentClose, CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {exit_status,Channel,_Status}} ->
	    %send_close(SentClose, CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {eof, Channel}} ->
	    %send_close(SentClose, CM, Channel),
	    ?MODULE:shell_loop(CM, Channel, IO, true);

	{ssh_cm, CM, {closed, Channel}} ->
	    ssh_cm:detach(CM),
	    exit(IO, kill);

	Other ->
	    io:format("ssh_ssh:shell_loop: unexpected msg ~p\n", [Other])
    end.

send_close(false, CM, Channel) ->
    ssh_cm:close(CM, Channel);
send_close(_, _, _) ->
    ok.
