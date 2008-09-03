%%<copyright>
%% <year>2005-2007</year>
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

%%% Description: ssh shell client

-module(ssh_ssh).

-export([connect/1, connect/2, connect/3]).

-deprecated({connect, 1, next_major_release}).
-deprecated({connect, 2, next_major_release}).
-deprecated({connect, 3, next_major_release}).

-export([input_loop/2, shell_loop/4]).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-define(default_timeout, 10000).

connect(A) ->
    connect(A, []).

connect(Host, Opts) when is_list(Host) ->
    connect(Host, 22, Opts);
connect(CM, Opts) ->
    Timeout = proplists:get_value(connect_timeout, Opts, ?default_timeout),
    case ssh:attach(CM, Timeout) of
	{ok,CMPid} ->
	    session(CMPid, Timeout);
	Error ->
	    Error
    end.

connect(Host, Port, Opts) ->
    case ssh:connect(Host, Port, Opts) of
	{ok, CM} ->
	    session(CM, proplists:get_value(connect_timeout,
					    Opts, ?default_timeout));
	Error ->
	    Error
    end.

session(CM, Timeout) ->
    case ssh_connection:session_channel(CM, Timeout) of
	{ok,ChannelId}  ->
	    case ssh_connection:shell(CM, ChannelId) of
		ok ->
		    {group_leader,GIO} = 
			process_info(self(), group_leader),
		    IO = spawn(?MODULE, input_loop,
			       [GIO, self()]),
		    shell_loop(CM, ChannelId, IO, false);
		Error  ->
		    ssh_connection:close(CM, ChannelId),
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
    

shell_loop(CM, ChannelId, IO, SentClose) ->
    receive
	{input, IO, eof} ->
	    ssh_connection:send_eof(CM, ChannelId),
	    ?MODULE:shell_loop(CM, ChannelId, IO, SentClose);
	    
	{input, IO, Line} ->
	    ssh_connection:send(CM, ChannelId, Line),
	    ?MODULE:shell_loop(CM, ChannelId, IO, SentClose);

	{ssh_cm, CM, {data, ChannelId, Type, Data}} ->
	    if Type == 0 ->
		    io:format("~s", [binary_to_list(Data)]);
	       Type == ?SSH_EXTENDED_DATA_STDERR ->
		    Report = io_lib:format("~s", [binary_to_list(Data)]),
		    %% May not be an error!
		    error_logger:info_report(Report);
	       true ->
		    ok
	    end,
	    ssh_connection:adjust_window(CM, ChannelId, size(Data)),
	    ?MODULE:shell_loop(CM, ChannelId, IO, SentClose);

	{ssh_cm, CM, {exit_signal, ChannelId, _SIG, _Err, _Lang}} ->
	    ?dbg(true, "SIGNAL: ~s (~s)\n", [_SIG, _Err]),
	    send_close(SentClose, CM, ChannelId),
	    ?MODULE:shell_loop(CM, ChannelId, IO, true);

	{ssh_cm, CM, {exit_status,ChannelId,_Status}} ->
	    %send_close(SentClose, CM, ChannelId),
	    ?MODULE:shell_loop(CM, ChannelId, IO, true);

	{ssh_cm, CM, {eof, ChannelId}} ->
	    %send_close(SentClose, CM, ChannelId),
	    ?MODULE:shell_loop(CM, ChannelId, IO, true);

	{ssh_cm, CM, {closed, ChannelId}} ->
	    exit(IO, kill);

	Other ->
	    error_logger:format("ssh_ssh:shell_loop: unexpected msg ~p \n", [Other])
    end.

send_close(false, CM, ChannelId) ->
    ssh_connection:close(CM, ChannelId);
send_close(_, _, _) ->
    ok.
