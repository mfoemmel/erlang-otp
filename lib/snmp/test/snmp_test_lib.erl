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

-module(snmp_test_lib).

-include_lib("kernel/include/file.hrl").


-export([hostname/0, hostname/1, localhost/0, os_type/0, sz/1]).
-export([replace_config/3, set_config/3, get_config/2, get_config/3]).
-export([fail/3, skip/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([ping/1, local_nodes/0, nodes_on/1]).
-export([start_node/2]).
-export([is_app_running/1, 
	 is_crypto_running/0, is_mnesia_running/0, is_snmp_running/0]).
-export([crypto_start/0, crypto_support/0]).
-export([watchdog/3, watchdog_start/1, watchdog_start/2, watchdog_stop/1]).
-export([del_dir/1]).
-export([cover/1]).
-export([p/2, print/5]).


%% ----------------------------------------------------------------------
%% Misc functions
%%

hostname() ->
    hostname(node()).

hostname(Node) ->
    from($@, atom_to_list(Node)).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

localhost() ->
    {ok, Ip} = snmp_misc:ip(net_adm:localhost()),
    Ip.

sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


os_type() ->
    case (catch test_server:os_type()) of
	{'EXIT', _} ->
	    %% Pre-R10 test server does not have this function
	    os:type();
	OsType ->
	    OsType
    end.


%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

replace_config(Key, Config, NewValue) ->
    lists:keyreplace(Key, 1, Config, {Key, NewValue}).

set_config(Key, Def, Config) ->
    case get_config(Key, Config) of
        undefined ->
            [{Key, Def}|Config];
        _ ->
            Config
    end.

get_config(Key,C) ->
    get_config(Key,C,undefined).

get_config(Key,C,Default) ->
    case lists:keysearch(Key,1,C) of
        {value,{Key,Val}} ->
            Val;
        _ ->
            Default
    end.


fail(Reason, Mod, Line) ->
    exit({suite_failed, Reason, Mod, Line}).
    
skip(Reason) ->
    String = lists:flatten(io_lib:format("Skipping: ~p~n",[Reason])),
    exit({skipped, String}).
    

%% ----------------------------------------------------------------
%% Time related function
%% 

millis() ->
    erlang:now().

millis_diff(A,B) ->
    T1 = (element(1,A)*1000000) + element(2,A) + (element(3,A)/1000000),
    T2 = (element(1,B)*1000000) + element(2,B) + (element(3,B)/1000000),
    T1 - T2.

hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


sleep(infinity) ->
    receive
    after infinity ->
            ok
    end;
sleep(MSecs) ->
    receive
    after trunc(MSecs) ->
            ok
    end,
    ok.


%% ----------------------------------------------------------------
%% Process utility function
%% 

flush_mqueue() ->
    io:format("~p~n", [lists:reverse(flush_mqueue([]))]).

flush_mqueue(MQ) ->
    receive
        Any ->
            flush_mqueue([Any|MQ])
    after 0 ->
            MQ
    end.

    
trap_exit() -> 
    {trap_exit,Flag} = process_info(self(),trap_exit),Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit,Flag).



%% ----------------------------------------------------------------
%% Node utility functions
%% 

ping(N) ->
    case net_adm:ping(N) of
 	pang ->
 	    error;
 	pong ->
 	    ok
     end.

local_nodes() ->
    nodes_on(net_adm:localhost()).

nodes_on(Host) when list(Host) ->
    net_adm:world_list([list_to_atom(Host)]).


start_node(Name, Args) ->
    Opts = [{cleanup,false}, {args,Args}],
    test_server:start_node(Name, slave, Opts).


%% ----------------------------------------------------------------
%% Application and Crypto utility functions
%% 

is_app_running(App) when atom(App) ->
    Apps = application:which_applications(),
    lists:keymember(App,1,Apps).

is_crypto_running() ->
    is_app_running(crypto).

is_mnesia_running() ->
    is_app_running(mnesia).

is_snmp_running() ->
    is_app_running(snmp).

crypto_start() ->
    case crypto:start() of
        ok ->
            ok;
        {error, {already_started,crypto}} ->
            ok;
        Else ->
            Else
    end.
 
crypto_support() ->
    crypto_support([md5_mac_96, sha_mac_96], []).
 
crypto_support([], []) ->
    yes;
crypto_support([], Acc) ->
    {no, Acc};
crypto_support([Func|Funcs], Acc) ->
    case is_crypto_supported(Func) of
        true ->
            crypto_support(Funcs, Acc);
        false ->
            crypto_support(Funcs, [Func|Acc])
    end.
 
is_crypto_supported(Func) ->
    %% The 'catch' handles the case when 'crypto' is
    %% not present in the system (or not started).
    case (catch lists:member(Func, crypto:info())) of
        true -> true;
        _ -> false
    end.
 
 
%% ----------------------------------------------------------------
%% Watchdog functions
%% 

watchdog_start(Timeout) ->
    watchdog_start(unknown, Timeout).

watchdog_start(Case, Timeout) ->
    spawn_link(?MODULE, watchdog, [Case, Timeout, self()]).

watchdog_stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok.

watchdog(Case, Timeout0, Pid) ->
    process_flag(priority, max),
    Timeout = timeout(Timeout0),
    receive
    after Timeout ->
	    Mon = erlang:monitor(process, Pid),
	    case erlang:process_info(Pid) of
		undefined ->
		    ok;
		ProcInfo ->
		    Line = 
			case lists:keysearch(dictionary, 1, ProcInfo) of
			    {value, {_, Dict}} when is_list(Dict) ->
				case lists:keysearch(test_server_loc, 1, Dict) of
				    {value, {_, {_Mod, L}}} when is_integer(L) ->
					L;
				    _ ->
					0
				end;
			    _ -> % This borders on paranoia, but...
				0
			end,
		    Trap = {timetrap_timeout, Timeout, Line},
		    exit(Pid, Trap),
		    receive
			{'DOWN', Mon, process, Pid, _} ->
			    ok
		    after 10000 ->
			    warning_msg("Failed stopping "
					"test case ~p process ~p "
					"[~w] after ~w: killing instead", 
				[Case, Pid, Line, Timeout]),
			    exit(Pid, kill)
		    end
	    end
    end.

warning_msg(F, A) ->
    (catch error_logger:warning_msg(F ++ "~n", A)).

timeout(T) ->
    trunc(timeout(T, os:type())).

timeout(T, vxworks) ->
    5 * T * timetrap_scale_factor();
timeout(T, _) ->
    T * timetrap_scale_factor().
            
timetrap_scale_factor() ->
    case (catch test_server:timetrap_scale_factor()) of
	{'EXIT', _} ->
	    1;
	N ->
	    N
    end.

    
%% ----------------------------------------------------------------------
%% file & dir functions
%%

del_dir(Dir) when list(Dir) ->
    (catch do_del_dir(Dir)).

do_del_dir(Dir) ->
    io:format("delete directory ~s~n", [Dir]),
    case file:list_dir(Dir) of
	{ok, Files} -> 
	    Files2 = [filename:join(Dir, File) || File <- Files],
	    del_dir2(Files2),
	    case file:del_dir(Dir) of
		ok ->
		    io:format("directory ~s deleted~n", [Dir]),
		    ok;
		{error, eexist} = Error1 ->
		    io:format("directory not empty: ~n", []),
		    {ok, Files3} = file:list_dir(Dir),
		    io:format("found additional files: ~n~p~n", 
			      [Files3]),
		    throw(Error1);
		{error, Reason2} = Error2 ->
		    io:format("failed deleting directory: ~w~n", [Reason2]),
		    throw(Error2)
	    end;
	Else ->
	    Else
    end.
    
del_dir2([]) ->
    ok;
del_dir2([File|Files]) ->
    del_file_or_dir(File),
    del_dir2(Files).

del_file_or_dir(FileOrDir) ->
    case file:read_file_info(FileOrDir) of
	{ok, #file_info{type = directory}} ->
	    do_del_dir(FileOrDir);
	{ok, _} ->
	    io:format("   delete file ~s~n", [FileOrDir]),
	    case file:delete(FileOrDir) of
		ok ->
		    io:format("   => deleted~n", []),
		    ok;
		{error, Reason} = Error ->
		    io:format("   => failed - ~w~n", [Reason]),
		    throw(Error)
	    end;
		
	_ ->
	    ok
    end.
	    

%% ----------------------------------------------------------------------
%% cover functions
%%

cover([Suite, Case] = Args) when atom(Suite), atom(Case) ->
    Mods0 = cover:compile_directory("../src"),
    Mods1 = [Mod || {ok, Mod} <- Mods0],
    snmp_test_server:t(Args),
    Files0 = [cover:analyse_to_file(Mod) || Mod <- Mods1],
    [io:format("Cover output: ~s~n", [File]) || {ok, File} <- Files0],
    ok.


%% ----------------------------------------------------------------------
%% (debug) Print functions
%%

p(Mod, Case) when is_atom(Mod) and is_atom(Case) ->
    case get(test_case) of
	undefined ->
	    put(test_case, Case),
	    p("~n~n************ ~w:~w ************", [Mod, Case]);
	_ ->
	    ok
    end;

p(F, A) when is_list(F) and is_list(A) ->
    io:format(user, F ++ "~n", A).

print(Prefix, Module, Line, Format, Args) ->
    io:format("*** [~s] ~s ~p ~p ~p:~p *** " ++ Format ++ "~n", 
	      [format_timestamp(now()), 
	       Prefix, node(), self(), Module, Line|Args]).

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).

