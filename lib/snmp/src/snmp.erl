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

-include_lib("stdlib/include/erl_compile.hrl").
%-include("erl_compile.hrl").

%%----------------------------------------------------------------------
%% This module contains the user interface to the snmp toolkit.
%%----------------------------------------------------------------------

-export([c/1, c/2, debug/2, is_consistent/1, mib_to_hrl/1, config/0,
	 current_request_id/0, current_community/0, current_address/0,
	 current_context/0,
	 current_net_if_data/0, name_to_oid/1, oid_to_name/1,
	 int_to_enum/2, enum_to_int/2,
	 date_and_time/0, universal_time_to_date_and_time/1,
	 local_time_to_date_and_time/1, date_and_time_to_universal_time/1,
	 validate_date_and_time/1, date_and_time_to_string/1,
	 get/2,
	 info/1, load_mibs/2, register_subagent/3,
	 unload_mibs/2, unregister_subagent/2, 
	 send_notification/3, send_notification/4, send_notification/5,
	 send_notification/6,
	 send_trap/3, send_trap/4,
	 str_apply/1, sys_up_time/0, system_start_time/0, compile/3]).
-export([passwd2localized_key/3, localize_key/3]).
-export([add_agent_caps/2, del_agent_caps/1, get_agent_caps/0]).
-export([log_to_txt/2, log_to_txt/3, log_to_txt/4, log_to_txt/5,
	 change_log_size/1]).

c(File) -> snmp_compile:compile(File).
c(File, Options) -> snmp_compile:compile(File, Options).

config() -> snmp_config:config().

debug(Agent, Flag) -> snmp_agent:debug(Agent, Flag).

is_consistent(Filenames) ->
    snmp_compile_lib:is_consistent(Filenames).

mib_to_hrl(MibName) ->
    snmp_mib_to_hrl:convert(MibName).

%%-----------------------------------------------------------------
%% These 4 functions returns {value, Val} | false
%%-----------------------------------------------------------------
name_to_oid(Name) ->
    snmp_symbolic_store:aliasname_to_oid(Name).

oid_to_name(OID) ->
    snmp_symbolic_store:oid_to_aliasname(OID).

enum_to_int(Name, Enum) ->
    snmp_symbolic_store:enum_to_int(Name, Enum).

int_to_enum(Name, Int) ->
    snmp_symbolic_store:int_to_enum(Name, Int).

%%-----------------------------------------------------------------
%% These functions must only be called in the process context
%% where the instrumentation functions are called!
%%-----------------------------------------------------------------
current_request_id() -> 
    case get(snmp_request_id) of
	undefined -> false;
	X -> {value, X}
    end.

current_context() ->
    case get(snmp_context) of
	undefined -> false;
	X -> {value, X}
    end.

current_community() ->
    case get(snmp_community) of
	undefined -> false;
	X -> {value, X}
    end.

current_address() ->
    case get(snmp_address) of
	undefined -> false;
	X -> {value, X}
    end.

current_net_if_data() ->
    case get(snmp_net_if_data) of
	undefined -> false;
	X -> {value, X}
    end.


%%-----------------------------------------------------------------
%% Returns: current time as a DateAndTime type (defined in rfc1903)
%%-----------------------------------------------------------------
date_and_time() ->
    case catch erlang:now() of
	{'EXIT', _} -> % We don't have info about UTC
	        short_time(calendar:local_time());
	
	Now ->
	        UTC = calendar:now_to_universal_time(Now),
	        Local = calendar:universal_time_to_local_time(UTC),
	        date_and_time(Local, UTC)
    end.
 
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

local_time_to_date_and_time(Local) ->
    UTC = calendar:local_time_to_universal_time(Local),
    date_and_time(Local, UTC).

date_and_time_to_universal_time([Y1,Y2, Mo, D, H, M, S, _Ds]) ->
    %% Local time specified, convert to UTC
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    calendar:local_time_to_universal_time(Local);
date_and_time_to_universal_time([Y1,Y2, Mo, D, H, M, S, _Ds, Sign, Hd, Md]) ->
    %% Time specified as local time + diff from UTC. Conv to UTC.
    Local = {{y(Y1,Y2), Mo, D}, {H, M, S}},
    LocalSecs = calendar:datetime_to_gregorian_seconds(Local),
    Diff = (Hd*60 + Md)*60,
    UTCSecs = if Sign == $+ -> LocalSecs - Diff;
		   Sign == $- -> LocalSecs + Diff
	            end,
    calendar:gregorian_seconds_to_datetime(UTCSecs).

validate_date_and_time([Y1,Y2, Mo, D, H, M, S, Ds | Diff]) 
  when 0 =< Y1, 0 =< Y2, 0 < Mo, Mo < 13, 0 < D, D < 32, 0 =< H,
       H < 24, 0 =< M, M < 60, 0 =< S, S < 61, 0 =< Ds, Ds < 10 ->
    case check_diff(Diff) of
	true ->
	        calendar:valid_date(y(Y1,Y2), Mo, D);
	false ->
	        false
    end;
validate_date_and_time(_) -> false.

check_diff([]) -> true;
check_diff([$+, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
check_diff([$-, H, M]) when 0 =< H, H < 12, 0 =< M, M < 60 -> true;
check_diff(_) -> false.

get(Agent, Vars) -> snmp_agent:get(Agent, Vars).

info(Agent) -> snmp_agent:info(Agent).

load_mibs(Agent, Mibs) when list(Mibs) -> snmp_agent:load_mibs(Agent, Mibs).

unload_mibs(Agent, Mibs) when list(Mibs) -> snmp_agent:unload_mibs(Agent, Mibs).

send_notification(Agent, Notification, Recv) ->
    send_notification(Agent, Notification, Recv, "", "", []).

send_notification(Agent, Notification, Recv, Varbinds) ->
    send_notification(Agent, Notification, Recv, "", "", Varbinds).

send_notification(Agent, Notification, Recv, NotifyName, Varbinds) ->
    send_notification(Agent, Notification, Recv, NotifyName, "", Varbinds).

send_notification(Agent, Notification, Recv,NotifyName,ContextName,Varbinds) 
  when list(NotifyName), list(ContextName), list(Varbinds) ->
    snmp_agent:send_trap(Agent, Notification, NotifyName, 
			 ContextName, Recv, Varbinds).

%% Kept for backwards compatibility
send_trap(Agent, Trap, Community) ->
    send_notification(Agent, Trap, no_receiver, Community, "", []).

send_trap(Agent, Trap, Community, Varbinds) ->
    send_notification(Agent, Trap, no_receiver, Community, "", Varbinds).

register_subagent(Agent, SubTree, SubAgent) ->
    snmp_agent:register_subagent(Agent, SubTree, SubAgent).

unregister_subagent(Agent, SubOidOrPid) ->
    snmp_agent:unregister_subagent(Agent, SubOidOrPid).

%% Usage: erl -s snmp str_apply '{Mod,Func,ArgList}'
str_apply([Atom]) ->
    Str = atom_to_list(Atom),
    {Mod,Func,Args} = to_erlang_term(Str),
    apply(Mod,Func,Args).

to_erlang_term(String) ->
    {ok, Tokens, _} = erl_scan:string(lists:append([String, ". "])),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = snmp:system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).

system_start_time() ->
    [{_, Time}] = ets:lookup(snmp_agent_table, system_start_time),
    Time.

%%%-----------------------------------------------------------------
%%% USM functions
%%%-----------------------------------------------------------------
passwd2localized_key(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).

localize_key(Alg, Key, EngineID) ->
    snmp_usm:localize_key(Alg, Key, EngineID).

%%%-----------------------------------------------------------------
%%% Agent Capabilities functions
%%%-----------------------------------------------------------------
add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

get_agent_caps() ->
    snmp_standard_mib:get_agent_caps().

%%%-----------------------------------------------------------------
%%% Audit Trail Log functions
%%%-----------------------------------------------------------------
log_to_txt(LogDir, Mibs) -> 
    snmp_log:log_to_txt(LogDir, Mibs).
log_to_txt(LogDir, Mibs, OutFile) -> 
    snmp_log:log_to_txt(LogDir, Mibs, OutFile).
log_to_txt(LogDir, Mibs, OutFile, LogName) -> 
    snmp_log:log_to_txt(LogDir, Mibs, OutFile, LogName).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    snmp_log:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).

change_log_size(NewSize) -> snmp_log:change_log_size(NewSize).

%%%-----------------------------------------------------------------
%%% Interface for erl_compile.
%%%-----------------------------------------------------------------

compile(Input, _Output, Options) ->
    case c(Input, make_snmp_options(Options)) of
	{ok, _} ->
	    ok;
	{error, Reason} ->
	    io:format("~p", [Reason]),
	    error
    end.

%% Converts generic options to format expected by snmp:c/2

make_snmp_options(Opts) ->

    Includes = Opts#options.includes,
    Outdir = Opts#options.outdir,
    Warning0 = Opts#options.warning,
    Specific = Opts#options.specific,

    Warning = 
	case Warning0 of
	    0 -> {warnings, false};
	    _ -> {warnings, true}
	end,
    
    IncludeOption =
	{i, case Includes of
		[] ->
		    [""];
		Includes ->
		    lists:map(fun(Dir) -> Dir++"/" end, Includes)
	    end},
	    
    [Warning, {outdir, Outdir}, IncludeOption|Specific].

