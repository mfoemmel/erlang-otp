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
-module(disksup). 

-export([start_link/0, get_disk_data/0, get_check_interval/0,
	 get_almost_full_threshold/0]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, format_status/2]).

%%%-----------------------------------------------------------------
%%% This is a rewrite of disksup from BS.3 by Peter Högfeldt.
%%%
%%%  This module implements a server process that checks remaining 
%%%  disk space.
%%%-----------------------------------------------------------------
-record(state, {threshold, timeout, os, diskdata = [],port}).

start_link() -> gen_server:start_link({local, disksup}, disksup, [], []).

%%-----------------------------------------------------------------
%% Returns: [{Id, KByte, Capacity}]
%%    where  Id = string()
%%           KByte = Capacity = integer()
%%-----------------------------------------------------------------
get_disk_data() -> gen_server:call(disksup, get_disk_data).

get_check_interval() -> gen_server:call(disksup, get_check_interval).

get_almost_full_threshold() ->
    gen_server:call(disksup, get_almost_full_threshold).

init([]) ->  
    Port = new_port(),
    Timeout = get_timeout(),
    Threshold = get_threshold(),
    OS = get_os(),
    process_flag(trap_exit, true),
    process_flag(priority, low),
    State = #state{threshold = Threshold, timeout = Timeout, os = OS, 
		   port = Port},
    self() ! timeout, % Check space first thing when we're started
    {ok, State}.

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
handle_call(get_disk_data, _From, State) ->
    {reply, State#state.diskdata, State};
handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};
handle_call(get_almost_full_threshold, _From, State) ->
    {reply, State#state.threshold, State}.

handle_info(timeout, State) ->
    NewDiskData = check_disk_space(State),
    timer:send_after(State#state.timeout, timeout),
    {noreply, State#state{diskdata = NewDiskData}};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

check_disk_space(State) when element(1,State#state.os) == win32 ->
    Result = os_mon_sysinfo:get_disk_info(),
    check_disks_win32(Result, State#state.threshold);
check_disk_space(State) when State#state.os == {unix, solaris} ->
    Result = my_cmd("/usr/bin/df -lk",State#state.port),
    check_disks_solaris(skip_to_eol(Result), State#state.threshold);
check_disk_space(State) when State#state.os == {unix, sunos4} ->
    Result = my_cmd("df",State#state.port),
    check_disks_solaris(skip_to_eol(Result), State#state.threshold).

check_disks_solaris("", _Threshold) ->
    [];
check_disks_solaris("\n", _Threshold) ->
    [];
check_disks_solaris(Str, Threshold) ->
    case io_lib:fread("~s~d~d~d~d%~s", Str) of
	{ok, [_FS, KB, _Used, _Avail, Cap, MntOn], RestStr} ->
	    if
		Cap >= Threshold ->
		    set_disk_alarm(disk_almost_full, MntOn);
		true ->
		    clear_disk_alarm(disk_almost_full, MntOn)
	    end,
	    [{MntOn, KB, Cap} |
	     check_disks_solaris(RestStr, Threshold)];
	Other ->
	    check_disks_solaris(skip_to_eol(Str),Threshold)
    end.

check_disks_win32([], _Threshold) ->
    [];
check_disks_win32([H|T], Threshold) ->
    case io_lib:fread("~s~s~d~d~d", H) of
	{ok, [Drive,"DRIVE_FIXED",BAvail,BTot,_TotFree], RestStr} ->
	    Cap = trunc((BTot-BAvail) / BTot * 100),
	    if
		 Cap >= Threshold ->
		    set_disk_alarm(disk_almost_full,Drive);
		true ->
		    clear_disk_alarm(disk_almost_full,Drive)
	    end,
	    [{Drive, BTot, Cap} |
	     check_disks_win32(T, Threshold)];
	{ok,_,RestStr} ->
	    check_disks_win32(T,Threshold);
	Other ->
	    []
    end.

set_disk_alarm(AlarmCode, Id) ->
    case get({AlarmCode, Id}) of
	set ->
	    ok;
	_ ->
	    alarm_handler:set_alarm({{AlarmCode, Id}, []}),
	    put({AlarmCode, Id}, set)
    end.

clear_disk_alarm(AlarmCode, Id) ->
    case get({AlarmCode, Id}) of
	set ->
	    alarm_handler:clear_alarm({AlarmCode, Id}),
	    erase({AlarmCode, Id});
	_ ->
	    ok
    end.

get_timeout() ->
    case application:get_env(os_mon, disk_space_check_interval) of
	{ok, Value} -> minutes_to_ms(Value);
	_ -> minutes_to_ms(30)
    end.

get_threshold() ->
    case application:get_env(os_mon, disk_almost_full_threshold) of
	{ok, Value} -> trunc(Value * 100);
	_ -> trunc(0.8 * 100)
    end.

get_os() ->
    case os:type() of
	{unix, sunos} ->
	    case os:version() of
		{5,_,_} -> {unix, solaris};
		{4,_,_} -> {unix, sunos4};
		V -> exit({{unknown_os_version, V}, {disk_sup, get_os, []}})
	    end;
	{win32,W} ->
	    {win32,W};	    
	Type ->
	    exit({{unknown_os_type, Type}, {disk_sup, get_os, []}})
    end.

minutes_to_ms(Minutes) ->
    trunc(60000*Minutes).

skip_to_eol([]) ->
    [];
skip_to_eol([$\n | T]) ->
    T;
skip_to_eol([_ | T]) ->
    skip_to_eol(T).

format_status(Opt, [PDict, #state{os = OS, threshold = Threshold,
				  timeout = Timeout, diskdata = DiskData}]) ->
    [{data, [{"OS", OS},
	     {"Timeout", Timeout},
	     {"Threshold", Threshold},
	     {"DiskData", DiskData}]}].

%%%---------------------------------------------
%%% Pseudo os:cmd()
%%%---------------------------------------------

new_port () -> 
     case catch open_port({spawn, "sh -s disksup 2>&1"}, [stream]) of
         {'EXIT', R} -> exit({?MODULE, {open_port_failed, R}});
         P -> P
     end.


my_cmd(Cmd,Port) ->
    get_reply(send2port(mk_cmd(Cmd),Port), []).

mk_cmd(Cmd) when list(Cmd) ->
    %% We insert a new line after the command, in case the command
    %% contains a comment character.
    io_lib:format("(~s\n) </dev/null; echo  \"\^M\"\n", [Cmd]);
mk_cmd(Cmd) ->
    exit({?MODULE, {bad_command, Cmd}}).

send2port(Cmd,P) ->
    P ! {self(), {command, [Cmd, 10]}},
    P.
            
get_reply(P, O) ->
    receive 
        {P, {data, N}} -> 
            case newline(N, O) of
                {ok, Str} -> Str;
                {more, Acc} -> get_reply(P, Acc)
            end;
        {'EXIT', P, _} -> 0
    end.
newline([13|_], B) -> {ok, lists:reverse(B)};
newline([H|T], B) -> newline(T, [H|B]);
newline([], B) -> {more, B}.



