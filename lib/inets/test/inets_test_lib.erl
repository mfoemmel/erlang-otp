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
-module(inets_test_lib).

-include("inets_test_lib.hrl").


%% Various small utility functions
-export([hostname/0, sz/1]).
-export([connect/3, send/3, close/2]).
-export([get_config/2, get_config/3, fail/3, skip/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([watchdog/2, watchdog_start/1, watchdog_stop/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([info/4, log/4, debug/4, print/4]).
-export([expandable/3]).
-export([app_test/0, app_test/1]).
-export([cover/1]).


%% ----------------------------------------------------------------------
%% print functions
%%

info(F, A, Mod, Line) ->
    print("INF ", F, A, Mod, Line).

log(F, A, Mod, Line) ->
    print("LOG ", F, A, Mod, Line).

debug(F, A, Mod, Line) ->
    print("DBG ", F, A, Mod, Line).

print(P, F, A, Mod, Line) ->
    io:format("~s[~p:~p:~p] : " ++ F ++ "~n", [P, self(), Mod, Line| A]).

print(F, A, Mod, Line) ->
    print("", F, A, Mod, Line).


%% ----------------------------------------------------------------------
%% Test case "suite" functions
%%

expandable(Init, Cases, Fin) when atom(Init), list(Cases), atom(Fin) ->
    {req, [], {conf, Init, Cases, Fin}}.

%% ----------------------------------------------------------------------
%% Misc functions
%%

hostname() ->
    from($@, atom_to_list(node())).
from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].


sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


%% ----------------------------------------------------------------------
%% Socket functions:
%% open(SocketType, Host, Port) -> {ok, Socket} | {error, Reason}
%% SocketType -> ssl | ip_comm
%% Host       -> atom() | string() | {A, B, C, D} 
%% Port       -> integer()

connect(ssl,Host,Port) ->
    ssl_socket:start(),
    case ssl:connect(Host, Port, [{packet,0}]) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    Error
    end;
connect(ip_comm,Host,Port) ->
    case gen_tcp:connect(Host,Port,[{packet,0},{reuseaddr,true}]) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, {enfile,_}} ->
	    {error, enfile};
	Error ->
	    Error
    end.


send(ssl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ip_comm,Socket,Data) ->
    gen_tcp:send(Socket,Data).


close(ssl,Socket) ->
    catch ssl:close(Socket);
close(ip_comm,Socket) ->
    catch gen_tcp:close(Socket).


%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

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
    exit({skipped, Reason}).
    

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
    receive
	Any ->
	    ?INFO("flush_mqueue -> Any: ~p",[Any]),
	    flush_mqueue()
    after 0 ->
	    ok
    end.

    
trap_exit() -> 
    {trap_exit,Flag} = process_info(self(),trap_exit),Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit,Flag).



%% ----------------------------------------------------------------
%% Watchdog functions
%% 

watchdog_start(Timeout) ->
    spawn_link(?MODULE, watchdog, [Timeout, self()]).

watchdog_stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok.

watchdog(Timeout0, Pid) ->
    process_flag(priority, max),
    Timeout = timeout(Timeout0),
    receive
    after Timeout ->
	    exit(Pid, watchdog_timeout)
    end.


timeout(T) ->
    trunc(timeout(T,os:type())).

timeout(T,vxworks) ->
    5 * T;
timeout(T,_) ->
    T.
	    
%% ----------------------------------------------------------------
%% App-test functions
%% 

app_test() ->
    app_test(inets).

app_test(App) ->
    case is_app(App) of
	{ok, AppFile} ->
	    case (catch do_app_test(App, AppFile)) of
		{error, Reason} ->
		    fail(Reason, ?MODULE, ?LINE);
		ok ->
		    ok
	    end;
	{error, Reason} ->
	    fail(Reason, ?MODULE, ?LINE)
    end.

is_app(App) ->
    LibDir = code:lib_dir(App),
    File = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
	{ok, [{application, App, AppFile}]} ->
	    {ok, AppFile};
	Error ->
	    {error, {invalid_format, Error}}
    end.


do_app_test(App, AppFile) ->
    %% Check mandatory (?) fields
    check_fields([description, modules, registered, applications], AppFile),
    
    %% Check for missing or/end extra modules
    Mods = check_modules(App, AppFile),
    
    %% Check that no modules has export_all
    check_export_all(Mods),
    
    %% Check that all specified apps exist
    check_spec_apps(AppFile).


check_fields([], AppFile) ->
    ok;
check_fields([F|Fields], AppFile) ->
    check_field(F, AppFile),
    check_fields(Fields, AppFile).


check_field(Name, AppFile) ->
    case lists:keymember(Name, 1, AppFile) of
	true ->
	    ok;
	false ->
	    throw({error, {missing_field, Name}})
    end.


check_modules(App, AppFile) ->
    %% Since we have checked fields, modules is there
    {value, {modules, Mods}} = lists:keysearch(modules, 1, AppFile),
    EbinList = get_ebin_mods(App),
    case missing_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Missing ->
	    throw({error, {missing_modules, Missing}})
    end,
    case extra_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Extra ->
	    throw({error, {extra_modules, Extra}})
    end,
    Mods.
	    
get_ebin_mods(App) ->
    LibDir  = code:lib_dir(App),
    EbinDir = filename:join([LibDir,"ebin"]),
    {ok, Files0} = file:list_dir(EbinDir),
    Files1 = [lists:reverse(File) || File <- Files0],
    [list_to_atom(lists:reverse(Name)) || [$m,$a,$e,$b,$.|Name] <- Files1].
    

missing_modules([], Ebins, Missing) ->
    Missing;
missing_modules([Mod|Mods], Ebins, Missing) ->
    case lists:member(Mod, Ebins) of
	true ->
	    missing_modules(Mods, Ebins, Missing);
	false ->
	    missing_modules(Mods, Ebins, [Mod|Missing])
    end.


extra_modules(Mods, [], Extra) ->
    Extra;
extra_modules(Mods, [Mod|Ebins], Extra) ->
    case lists:member(Mod, Mods) of
	true ->
	    extra_modules(Mods, Ebins, Extra);
	false ->
	    extra_modules(Mods, Ebins, [Mod|Extra])
    end.


check_export_all([]) ->
    ok;
check_export_all([Mod|Mods]) ->
    case (catch apply(Mod, module_info, [compile])) of
	{'EXIT', {undef, _}} ->
	    check_export_all(Mods);
	O ->
            case lists:keysearch(options, 1, O) of
                false ->
                    check_export_all(Mods);
                {value, {options, List}} ->
                    case lists:member(export_all, List) of
                        true ->
			    throw({error, {export_all, Mod}});
			false ->
			    check_export_all(Mods)
                    end
            end
    end.

	    
check_spec_apps(AppFile) ->
    %% Since we have checked fields, modules is there
    {value, {applications, Apps}} = lists:keysearch(applications, 1, AppFile),
    
    check_apps(Apps).

check_apps([]) ->
    ok;
check_apps([App|Apps]) ->
    case is_app(App) of
	{ok, _} ->
	    check_apps(Apps);
	Error ->
	    throw({error, {missing_app, {App, Error}}})
    end.


%% ----------------------------------------------------------------------
%% cover functions
%%

cover([Suite, Case] = Args) when atom(Suite), atom(Case) ->
    Mods0 = cover:compile_directory("../src"),
    Mods1 = [Mod || {ok, Mod} <- Mods0],
    inets_test_server:t(Args),
    Files0 = [cover:analyse_to_file(Mod) || Mod <- Mods1],
    [io:format("Cover output: ~s~n", [File]) || {ok, File} <- Files0],
    ok.

