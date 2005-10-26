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
-module(snmp).


%%----------------------------------------------------------------------
%% This module contains the user interface to the snmp toolkit.
%%----------------------------------------------------------------------

%% Application exports
-export([start/0,
	 config/0,
	 
         versions1/0, versions2/0,
	 print_versions/1, print_versions/2, 
 
	 date_and_time/0, 
	 universal_time_to_date_and_time/1,
	 local_time_to_date_and_time_dst/1, 
	 date_and_time_to_universal_time_dst/1,
	 validate_date_and_time/1, 
	 date_and_time_to_string/1,

	 str_apply/1,

	 sys_up_time/1, system_start_time/1,

         passwd2localized_key/3, localize_key/3,
 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7,
	 change_log_size/2]).

%% Compiler exports
-export([c/1, c/2, is_consistent/1, mib_to_hrl/1, 
	 compile/3]).

%% Agent exports (Dont use these, they will be removed eventually)
-export([current_request_id/0, current_community/0, current_address/0,
	 current_context/0, current_net_if_data/0, 

	 get_symbolic_store_db/0,
	 name_to_oid/1, name_to_oid/2, 
	 oid_to_name/1, oid_to_name/2,
	 int_to_enum/2, int_to_enum/3, 
	 enum_to_int/2, enum_to_int/3,

	 get/2,
	 info/1, 
	 load_mibs/2, unload_mibs/2, dump_mibs/0, dump_mibs/1,

	 register_subagent/3, unregister_subagent/2, 

	 send_notification/3, send_notification/4, send_notification/5,
	 send_notification/6,
	 send_trap/3, send_trap/4,

	 add_agent_caps/2, del_agent_caps/1, get_agent_caps/0,

	 log_to_txt/2, log_to_txt/3, log_to_txt/4, 
	 change_log_size/1 
	]).

%% This is for XREF
-deprecated([{c,                     1, eventually},
	     {c,                     2, eventually},
	     {compile,               3, eventually},
	     {is_consistent,         1, eventually},
	     {mib_to_hrl,            1, eventually},

	     {change_log_size,       1, eventually},
	     {log_to_txt,            2, eventually},
	     {log_to_txt,            3, eventually},
	     {log_to_txt,            4, eventually},

	     {current_request_id,    0, eventually},
	     {current_community,     0, eventually},
	     {current_address,       0, eventually},
	     {current_context,       0, eventually},
	     {current_net_if_data,   0, eventually},

	     {get_symbolic_store_db, 0, eventually},
	     {name_to_oid,           1, eventually},
	     {name_to_oid,           2, eventually},
	     {oid_to_name,           1, eventually},
	     {oid_to_name,           2, eventually},
	     {int_to_enum,           2, eventually},
	     {int_to_enum,           3, eventually},
	     {enum_to_int,           2, eventually},
	     {enum_to_int,           3, eventually},

	     {get,                   2, eventually},
	     {info,                  1, eventually},
	     {load_mibs,             2, eventually},
	     {unload_mibs,           2, eventually},
	     {dump_mibs,             0, eventually},
	     {dump_mibs,             1, eventually},

	     {register_subagent,     3, eventually},
	     {unregister_subagent,   2, eventually},

	     {send_notification,     3, eventually},
	     {send_notification,     4, eventually},
	     {send_notification,     5, eventually},
	     {send_notification,     6, eventually},
	     {send_trap,             3, eventually},
	     {send_trap,             4, eventually},

	     {add_agent_caps,        2, eventually},
	     {del_agent_caps,        1, eventually},
	     {get_agent_caps,        0, eventually}]).
 

-define(APPLICATION, snmp).


%%-----------------------------------------------------------------
%% Application
%%-----------------------------------------------------------------

start() ->
    application:start(snmp).


config() -> snmp_config:config().


%%-----------------------------------------------------------------
%% {ok, Vs} = snmp:versions1(), snmp:print_versions(Vs).

print_versions(Versions) ->
    print_versions("", Versions).

print_versions(Prefix, Versions) 
  when is_list(Prefix) and is_list(Versions) ->
    do_print_versions(Prefix, Versions);
print_versions(Prefix, Versions) 
  when (is_integer(Prefix) and (Prefix >= 0)) and is_list(Versions) ->
    do_print_versions(lists:duplicate(Prefix, $ ), Versions);
print_versions(Prefix, BadVersions) 
  when is_list(Prefix) or (is_integer(Prefix) and (Prefix >= 0)) ->
    {error, {bad_versions, BadVersions}};
print_versions(Prefix, BadVersions) 
  when is_list(BadVersions) ->
    {error, {bad_prefix, Prefix}};
print_versions(Prefix, BadVersions) -> 
    {error, {bad_args, Prefix, BadVersions}}.

do_print_versions(Prefix, Versions) ->
    print_sys_info(Prefix, Versions),
    print_os_info(Prefix, Versions),
    print_mods_info(Prefix, Versions).

print_sys_info(Prefix, Versions) ->
    case key1search(sys_info, Versions) of
        {value, SysInfo} when list(SysInfo) ->
            {value, Arch} = key1search(arch, SysInfo, "Not found"),
            {value, Ver}  = key1search(ver, SysInfo, "Not found"),
            io:format("~sSystem info: "
                      "~n~s   Arch: ~s"
                      "~n~s   Ver:  ~s"
                      "~n", [Prefix, 
			     Prefix, Arch, 
			     Prefix, Ver]),
            ok;
        _ ->
            io:format("System info: Not found~n", []),
            not_found
    end.

print_os_info(Prefix, Versions) ->
    case key1search(os_info, Versions) of
        {value, OsInfo} when list(OsInfo) ->
            Fam =
                case key1search(fam, OsInfo, "Not found") of
                    {value, F} when atom(F) ->
                        atom_to_list(F);
                    {value, LF} when list(LF) ->
                        LF;
                    {value, XF} ->
                        lists:flatten(io_lib:format("~p", [XF]))
                end,
            Name =
                case key1search(name, OsInfo) of
                    {value, N} when atom(N) ->
                        "[" ++ atom_to_list(N) ++ "]";
                    {value, LN} when list(LN) ->
                        "[" ++ LN ++ "]";
                    not_found ->
                        ""
                end,
            Ver =
                case key1search(ver, OsInfo, "Not found") of
                    {value, T} when tuple(T) ->
                        tversion(T);
                    {value, LV} when list(LV) ->
                        LV;
                    {value, XV} ->
                        lists:flatten(io_lib:format("~p", [XV]))
                end,
            io:format("~sOS info: "
                      "~n~s   Family: ~s ~s"
                      "~n~s   Ver:    ~s"
                      "~n", [Prefix, 
			     Prefix, Fam, Name, 
			     Prefix, Ver]),
            ok;
        _ ->
            io:format("~sOS info:     Not found~n", [Prefix]),
            not_found
    end.

tversion(T) ->
    L = tuple_to_list(T),
    lversion(L).

lversion([]) ->
    "";
lversion([A]) ->
    integer_to_list(A);
lversion([A|R]) ->
    integer_to_list(A) ++ "." ++ lversion(R).

print_mods_info(Prefix, Versions) ->
    case key1search(mod_info, Versions) of
        {value, ModsInfo} when list(ModsInfo) ->
            io:format("~sModule info: ~n", [Prefix]),
	    F = fun(MI) -> print_mod_info(Prefix, MI) end,
            lists:foreach(F, ModsInfo);
        _ ->
            io:format("~sModule info: Not found~n", [Prefix]),
            not_found
    end.

print_mod_info(Prefix, {Module, Info}) ->
    Vsn =
        case key1search(vsn, Info) of
            {value, I} when integer(I) ->
                integer_to_list(I);
            _ ->
                "Not found"
        end,
    AppVsn =
        case key1search(app_vsn, Info) of
            {value, S1} when list(S1) ->
                S1;
            _ ->
                "Not found"
        end,
    CompVer =
        case key1search(compiler_version, Info) of
            {value, S2} when list(S2) ->
                S2;
            _ ->
                "Not found"
        end,
    CompDate =
        case key1search(compile_time, Info) of
            {value, {Year, Month, Day, Hour, Min, Sec}} ->
                lists:flatten(
                  io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                                [Year, Month, Day, Hour, Min, Sec]));
            _ ->
                "Not found"
        end,
    io:format("~s   ~w:~n"
              "~s      Vsn:          ~s~n"
              "~s      App vsn:      ~s~n"
              "~s      Compiler ver: ~s~n"
              "~s      Compile time: ~s~n",
              [Prefix, Module, 
	       Prefix, Vsn, 
	       Prefix, AppVsn, 
	       Prefix, CompVer, 
	       Prefix, CompDate]),
    ok.

key1search(Key, Vals) ->
    case key1search(Key, Vals, undefined) of
        undefined ->
            not_found;
        Value ->
            Value
    end.

key1search(Key, Vals, Def) ->
    case lists:keysearch(Key, 1, Vals) of
        {value, {Key, Val}} ->
            {value, Val};
        false ->
            {value, Def}
    end.


%%-----------------------------------------------------------------

versions1() ->
    case ms1() of
        {ok, Mods} ->
            {ok, version_info(Mods)};
        Error ->
            Error
    end.
 
versions2() ->
    case ms2() of
        {ok, Mods} ->
            {ok, version_info(Mods)};
        Error ->
            Error
    end.
 
version_info(Mods) ->
    SysInfo = sys_info(),
    OsInfo  = os_info(),
    ModInfo = [mod_version_info(Mod) || Mod <- Mods],
    [{sys_info, SysInfo}, {os_info, OsInfo}, {mod_info, ModInfo}].
     
mod_version_info(Mod) ->
    Info = Mod:module_info(),
    {value, {attributes, Attr}}   = lists:keysearch(attributes, 1, Info),
    {value, {vsn,        [Vsn]}}  = lists:keysearch(vsn,        1, Attr),
    {value, {app_vsn,    AppVsn}} = lists:keysearch(app_vsn,    1, Attr),
    {value, {compile,    Comp}}   = lists:keysearch(compile,    1, Info),
    {value, {version,    Ver}}    = lists:keysearch(version,    1, Comp),
    {value, {time,       Time}}   = lists:keysearch(time,       1, Comp),
    {Mod, [{vsn,              Vsn},
           {app_vsn,          AppVsn},
           {compiler_version, Ver},
           {compile_time,     Time}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].
 
os_info() ->
    V = os:version(),
    case os:type() of
        {OsFam, OsName} ->
            [{fam, OsFam}, {name, OsName}, {ver, V}];
        OsFam ->
            [{fam, OsFam}, {ver, V}]
    end.

ms1() ->
    App    = ?APPLICATION,
    LibDir = code:lib_dir(App),
    File   = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
        {ok, [{application, App, AppFile}]} ->
            case lists:keysearch(modules, 1, AppFile) of
                {value, {modules, Mods}} ->
                    {ok, Mods};
                _ ->
                    {error, {invalid_format, modules}}
            end;
        Error ->
            {error, {invalid_format, Error}}
    end.

ms2() ->
    application:get_key(?APPLICATION, modules).


%%-----------------------------------------------------------------
%% Returns: current time as a DateAndTime type (defined in rfc1903)
%%-----------------------------------------------------------------
date_and_time() ->
    UTC   = calendar:universal_time(),
    Local = calendar:universal_time_to_local_time(UTC),
    date_and_time(Local, UTC).

date_and_time(Local, UTC) ->
    DiffSecs = calendar:datetime_to_gregorian_seconds(Local) -
	calendar:datetime_to_gregorian_seconds(UTC),
    short_time(Local) ++ diff(DiffSecs).

short_time({{Y,M,D},{H,Mi,S}}) ->
    [y1(Y), y2(Y), M, D, H, Mi, S, 0].

date_and_time_to_string(DAT) ->
    case validate_date_and_time(DAT) of
	true ->
	        dat2str(DAT);
	false ->
	        exit({badarg, {?MODULE, date_and_time_to_string, [DAT]}})
    end.

dat2str([Y1,Y2, Mo, D, H, M, S, Ds | Diff]) ->
    lists:flatten(io_lib:format("~w-~w-~w,~w:~w:~w.~w",
				[y(Y1,Y2),Mo,D,H,M,S,Ds]) ++
		    case Diff of
			      [Sign,Hd,Md] ->
			      io_lib:format(",~c~w:~w",
					    [Sign,Hd,Md]);
			      _ -> []
		      end).
    

y1(Y) -> (Y bsr 8) band 255.
y2(Y) -> Y band 255.

y(Y1, Y2) -> 256 * Y1 + Y2.
    
diff(Secs) ->
    case calendar:seconds_to_daystime(Secs) of
	{0, {H, M,_}} ->
	        [$+, H, M];
	{-1, _} ->
	        {0, {H, M, _}} = calendar:seconds_to_daystime(-Secs),
	        [$-, H, M]
    end.

universal_time_to_date_and_time(UTC) ->
    short_time(UTC) ++ [$+, 0, 0].

local_time_to_date_and_time_dst(Local) ->
    case calendar:local_time_to_universal_time_dst(Local) of
	[] ->
	    [];
	[UTC] ->
	    [date_and_time(Local, UTC)];
	[UTC1, UTC2] ->
	    [date_and_time(Local, UTC1), date_and_time(Local, UTC2)]
    end.

date_and_time_to_universal_time_dst([Y1, Y2, Mo, D, H, M, S, _Ds]) ->
    %% Local time specified, convert to UTC
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    calendar:local_time_to_universal_time_dst(Local);
date_and_time_to_universal_time_dst([Y1, Y2, Mo, D, H, M, S, _Ds, Sign, Hd, Md]) ->
     %% Time specified as local time + diff from UTC. Conv to UTC.
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    LocalSecs = calendar:datetime_to_gregorian_seconds(Local),
    Diff = (Hd*60 + Md)*60,
    UTCSecs = if Sign == $+ -> LocalSecs - Diff;
		 Sign == $- -> LocalSecs + Diff
	      end,
    [calendar:gregorian_seconds_to_datetime(UTCSecs)].


validate_date_and_time([Y1,Y2, Mo, D, H, M, S, Ds | Diff]) 
  when 0 =< Y1, 0 =< Y2, 
       0 < Mo, Mo < 13, 
       0 < D, D < 32, 0 =< H,
       H < 24, 
       0 =< M, M < 60, 
       0 =< S, S < 61, 
       0 =< Ds, Ds < 10 ->
    case check_diff(Diff) of
	true ->
	        calendar:valid_date(y(Y1,Y2), Mo, D);
	false ->
	        false
    end;
validate_date_and_time(_) -> false.

%% OTP-4206 (now according to RFC-2579)
check_diff([]) -> true;
check_diff([$+, H, M]) when 0 =< H, H < 14, 0 =< M, M < 60 -> true;
check_diff([$-, H, M]) when 0 =< H, H < 14, 0 =< M, M < 60 -> true;
check_diff(_) -> false.


%%-----------------------------------------------------------------
%% System start- and up-time
%%-----------------------------------------------------------------

system_start_time(agent) ->
    snmpa:system_start_time();
system_start_time(manager) ->
    snmpm:system_start_time().

sys_up_time(agent) ->
    snmpa:sys_up_time();
sys_up_time(manager) ->
    snmpm:sys_up_time().



%%%-----------------------------------------------------------------
%%% USM functions
%%%-----------------------------------------------------------------

passwd2localized_key(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).

localize_key(Alg, Key, EngineID) ->
    snmp_usm:localize_key(Alg, Key, EngineID).


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions
%%%-----------------------------------------------------------------

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    snmp_log:log_to_txt(LogName, LogFile, LogDir, Mibs, OutFile).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    snmp_log:log_to_txt(LogName, LogFile, LogDir, Mibs, OutFile, Start).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    snmp_log:log_to_txt(LogName, LogFile, LogDir, Mibs, OutFile, Start, Stop).


change_log_size(LogName, NewSize) -> 
    snmp_log:change_size(LogName, NewSize).


%%%-----------------------------------------------------------------
%%% Misc
%%%-----------------------------------------------------------------

%% Usage: erl -s snmp str_apply '{Mod,Func,ArgList}'
str_apply([Atom]) ->
    Str = atom_to_list(Atom),
    {Mod,Func,Args} = to_erlang_term(Str),
    apply(Mod,Func,Args).

to_erlang_term(String) ->
    {ok, Tokens, _} = erl_scan:string(lists:append([String, ". "])),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.




%%%-----------------------------------------------------------------
%%% BACKWARD COMPATIBILLITY CRAP
%%%-----------------------------------------------------------------

c(File) -> snmpc:compile(File).
c(File, Options) -> snmpc:compile(File, Options).

is_consistent(Filenames) ->
    snmpc:is_consistent(Filenames).

mib_to_hrl(MibName) ->
    snmpc:mib_to_hrl(MibName).

compile(Input, Output, Options) ->
    snmpc:compile(Input, Output, Options).

get_symbolic_store_db() -> snmpa:get_symbolic_store_db().

name_to_oid(Name)           -> snmpa:name_to_oid(Name).
name_to_oid(Db, Name)       -> snmpa:name_to_oid(Db, Name).
oid_to_name(OID)            -> snmpa:oid_to_name(OID).
oid_to_name(Db, OID)        -> snmpa:oid_to_name(Db, OID).
enum_to_int(Name, Enum)     -> snmpa:enum_to_int(Name, Enum).
enum_to_int(Db, Name, Enum) -> snmpa:enum_to_int(Db, Name, Enum).
int_to_enum(Name, Int)      -> snmpa:int_to_enum(Name, Int).
int_to_enum(Db, Name, Int)  -> snmpa:int_to_enum(Db, Name, Int).

current_request_id()  -> snmpa:current_request_id().
current_context()     -> snmpa:current_context().
current_community()   -> snmpa:current_community().
current_address()     -> snmpa:current_address().
current_net_if_data() -> snmpa:current_net_if_data().

get(Agent, Vars) -> snmpa:get(Agent, Vars).
info(Agent) -> snmpa:info(Agent).
dump_mibs()     -> snmpa:dump_mibs().
dump_mibs(File) -> snmpa:dump_mibs(File).
load_mibs(Agent, Mibs) -> snmpa:load_mibs(Agent, Mibs).
unload_mibs(Agent, Mibs) -> snmpa:unload_mibs(Agent, Mibs).
send_notification(Agent, Notification, Recv) -> 
    snmpa:send_notification(Agent, Notification, Recv).
send_notification(Agent, Notification, Recv, Varbinds) ->
    snmpa:send_notification(Agent, Notification, Recv, Varbinds).
send_notification(Agent, Notification, Recv, NotifyName, Varbinds) ->
    snmpa:send_notification(Agent, Notification, Recv, NotifyName, Varbinds).
send_notification(Agent, Notification, Recv, NotifyName, 
		  ContextName, Varbinds) ->
    snmpa:send_notification(Agent, Notification, Recv, NotifyName, 
			    ContextName, Varbinds).
send_trap(Agent, Trap, Community) ->
    snmpa:send_trap(Agent, Trap, Community).
send_trap(Agent, Trap, Community, Varbinds) ->
    snmpa:send_trap(Agent, Trap, Community, Varbinds).
register_subagent(Agent, SubTree, SubAgent) ->
    snmpa:register_subagent(Agent, SubTree, SubAgent).
unregister_subagent(Agent, SubOidOrPid) ->
    snmpa:unregister_subagent(Agent, SubOidOrPid).

add_agent_caps(Oid, Descr) -> snmpa:add_agent_caps(Oid, Descr).
del_agent_caps(Index) -> snmpa:del_agent_caps(Index).
get_agent_caps() -> snmpa:get_agent_caps().

log_to_txt(LogDir, Mibs) -> 
    snmpa:log_to_txt(LogDir, Mibs).
log_to_txt(LogDir, Mibs, OutFile) -> 
    snmpa:log_to_txt(LogDir, Mibs, OutFile).
log_to_txt(LogDir, Mibs, OutFile, LogName) -> 
    snmpa:log_to_txt(LogDir, Mibs, OutFile, LogName).
change_log_size(NewSize) -> 
    snmpa:change_log_size(NewSize).
