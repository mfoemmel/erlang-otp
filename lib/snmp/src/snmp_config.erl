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
-module(snmp_config).
-include("snmp_types.hrl").
-export([config/0, write_files/7, write_files/8, write_files/11]).

-export([write_sys_config/3]).

%%----------------------------------------------------------------------
%% Handy SNMP configuration
%%----------------------------------------------------------------------

config() ->
    case catch config2() of
	ok -> ok;
	{error, Reason} -> {error, Reason};
	Q -> {error,failed}
    end.

config2() ->
    case catch config3() of
	ok -> ok;
	{error, Reason} -> {error, Reason};
	Q -> {error, dialog_failed}
    end.

config3() ->
    io:format("\nSimple SNMP configuration tool (v~s)\n", [?version]),
    p("----------------------------------------------"),
    p("Note: Non-trivial configurations still has to be done manually."),
    p("IP addresses may be entered as dront.ericsson.se (UNIX only) "
      "or 123.12.13.23\n"),
    AgentName=get_agent_name(),
    EngineName=get_engine_name(),
    SysName=ask(["1. System name (sysName standard variable) [",AgentName,"]"],
		 AgentName),
    EngineID=ask(["2. Engine ID (snmpEngineID standard variable)"
		  "[",EngineName,"]"], EngineName),
    AgentUDP=ask("3. The UDP port the agent listens to. (standard 161) [4000]",
		 "4000"),
    Host = '$HOST'(),
    AgentIP=make_ip(ask(["4. IP address for the agent (only used as id\n"
			 "   when sending traps) [", Host, "]"], Host)),
    ManagerIP=make_ip(ask(["5. IP address for the manager (only this manager "
			   "will have access\n   to the agent, traps are sent "
			   "to this one) [", Host, "]"], Host)),
    TrapUdp=ask("6. To what UDP port at the manager should traps\n"
		"   be sent (standard 162)? [5000]", "5000"),
    Ver = ask("7. What SNMP version should be used (1,2,3,1&2,1&2&3,2&3)? [3]",
	      "3"),
    NewVer = verify_ver(Ver),
    NotifType=
	case lists:member(1, NewVer) of
	    true ->
		"trap";
	    false ->
		ask("7b. Should notifications be sent as traps or informs? "
		    "[trap]", "trap")
	end,
    NewNotifType = verify_notif_type(NotifType),
    SecType = ask("8. Do you want a none- minimum- or semi-secure"
		  " configuration?\n"
		  "   Note that if you chose v1 or v2, you won't get any "
		  "security for these\n"
		  "   requests (none, minimum, semi) [minimum]", "minimum"),
    NewSecType = verify_sec_type(SecType),
    Passwd = 
	case lists:member(3, NewVer) and (NewSecType /= none) of
	    true ->
		P=ask("8b. Give a password of at least length 8. It is used to "
		      "generate\nprivate keys for the  configuration.",
		      mandatory),
		verify_passwd(P),
		P;
	    false ->
		""
	end,
    {ok, DefDir} = file:get_cwd(),
    Dir=ask("9. Where is the configuration directory (absolute)? "
	    "[" ++ DefDir ++ "]", DefDir),
    verify_dir(Dir),
    "y" = ask("10. Current configuration files will now be overwritten. "
	      "Ok [y]/n?", "y"),
    Dir2 = snmp_misc:ensure_trailing_dir_delimiter(Dir),
    case catch write_files(Dir2,NewVer,ManagerIP,TrapUdp,AgentIP,AgentUDP,
			   SysName,NewNotifType,NewSecType,Passwd,EngineID) of
	ok ->
	   p("------------------------"),
	   p("Info: 1. SecurityName \"initial\" has noAuthNoPriv read access "
	     "and authenticated write access to the \"restricted\" subtree."),
	   p("      2. SecurityName \"all-rights\" has noAuthNoPriv read/write "
	     "access to the \"internet\" subtree."),
	   p("      3. Standard traps are sent to the manager."),
	   case lists:member(1, NewVer) or lists:member(2, NewVer) of
	       true ->
		   p("      4. Community \"public\" is mapped to security name "
		     "\"initial\"."),
		   p("      5. Community \"all-rights\" is mapped to security "
		     "name \"all-rights\".");
	       false ->
		   ok
	   end,
	   p("The following files were written: agent.conf, "
	      "community.conf,\n   standard.conf, target_addr.conf, "
	      "target_params.conf,\n   notify.conf vacm.conf, sys.config" ++
	     case lists:member(3, NewVer) of
		 true -> ", usm.conf";
		 false -> ""
	     end),
	   p("------------------------"),
	   ok;
	Q -> {error, {"write files failed", Q}}
    end.

verify_ver("1") -> [1];
verify_ver("2") -> [2];
verify_ver("3") -> [3];
verify_ver("1&2") -> [1,2];
verify_ver("1&3") -> [1,3];
verify_ver("2&3") -> [2,3];
verify_ver("1&2&3") -> [1,2,3];
verify_ver(_) -> throw({error, "version syntax error"}).
    
verify_passwd(Passwd) when length(Passwd) >= 8 ->
    ok;
verify_passwd(_) ->
    throw({error, bad_passwd}).

verify_dir(Dir) ->
    case filename:pathtype(Dir) of
	absolute -> ok;
	Q -> throw({error,bad_dir})
    end.
	    
verify_notif_type("trap") -> trap;
verify_notif_type("inform") -> inform;
verify_notif_type(_) -> throw({error, "notifcation type syntax error"}).
    
verify_sec_type("none") -> none;
verify_sec_type("minimum") -> minimum;
verify_sec_type("semi") -> semi;
verify_sec_type(_) -> throw({error, "security type syntax error"}).
    

ip(Host) ->
    case catch snmp_misc:ip(Host) of
	{ok, IPtuple} -> tuple_to_list(IPtuple);
	{error, Reason} -> throw({error, Reason});
	Q -> throw({error, {"ip conversion failed", Host}})
    end.

make_ip(Str) ->
    case catch snmp_misc:ip(Str) of
	{ok, IPtuple} -> tuple_to_list(IPtuple);
	Q -> ip(Str)
    end.
		 
    
p(Str) ->
    io:format(Str), io:format("\n").

%% Defval = string() | mandatory
ask(Str, Defval) ->
    io:format(Str),
    Answ = io:get_line(''),
    case remove_newline(Answ) of
	"" when Defval /= mandatory -> Defval;
	"" -> ask(Str, Defval);
	Q -> Q
    end.

'$HOST'() ->
    case os:type() of
	{unix, _} ->
	    remove_newline(os:cmd("echo $HOST"));
	_ -> "127.0.0.1"
    end.

get_agent_name() ->
    case os:type() of
	{unix, _} ->
	    lists:append(remove_newline(os:cmd("echo $USER")), "'s agent");
	{_,_} -> "my agent"
    end.

get_engine_name() ->
    case os:type() of
	{unix, _} ->
	    lists:append(remove_newline(os:cmd("echo $USER")), "'s engine");
	{_,_} -> "my engine"
    end.

remove_newline(Str) -> 
    lists:delete($\n, Str).

%%======================================================================
%% File generation
%%======================================================================

%%----------------------------------------------------------------------
%% Dir: string()  (ex: "../conf/")
%% ManagerIP, AgentIP: [int(),int(),int(),int()]
%% TrapUdp, AgentUDP: string()    (ex: "4000")
%% SysName: string()
%%----------------------------------------------------------------------
write_files(Dir,Ver,ManagerIP,TrapUdp,AgentIP,AgentUDP,SysName) ->
    write_files(Dir,Ver,ManagerIP,TrapUdp,AgentIP,AgentUDP,
		SysName,"trap",none,"","agentEngine").
write_files(Dir,Ver,ManagerIP,TrapUdp,AgentIP,AgentUDP,SysName,NotifType) ->
    write_files(Dir,Ver,ManagerIP,TrapUdp,AgentIP,AgentUDP,
		SysName,NotifType,none,"","agentEngine").
write_files(Dir,Ver,ManagerIP,TrapUdp,AgentIP,AgentUDP,SysName,NotifType,
	    SecType,Passwd,EngineID) ->
    write_agent_conf(Dir, AgentIP, AgentUDP, EngineID),
    write_context_conf(Dir),
    write_community_conf(Dir),
    write_standard_conf(Dir, SysName),
    write_target_addr_conf(Dir, ManagerIP, TrapUdp, Ver),
    write_target_params_conf(Dir, Ver),
    write_notify_conf(Dir, NotifType),
    write_usm_conf(Dir, Ver, EngineID, SecType, Passwd),
    write_vacm_conf(Dir, Ver, SecType),
    %% Verbs = [{snmp_master_agent_verbosity,log},
    %%	     {snmp_mibserver_verbosity,debug},
    %%	     {snmp_net_if_verbosity,debug}],
    %% write_sys_config(Dir,Ver,Verbs),
    write_sys_config(Dir,Ver),
    ok.

write_agent_conf(Dir, AgentIP, AgentUDP, EngineID) -> 
    Comment = 
"%% This files defines the Agent local configuration info\n"
"%% The data is inserted into the snmpEngine* variables defined\n"
"%% in SNMP-FRAMEWORK-MIB, and the intAgent* variables defined\n"
"%% in OTP-SNMPEA-MIB.\n"
"%% Each row is a 2-tuple:\n"
"%% {AgentVariable, Value}.\n"
"%% For example\n"
"%% {intAgentUDPPort, 4000}.\n"
"%% The ip address for the agent is sent as id in traps.\n"
"%% {intAgentIpAddress, [127,42,17,5]}.\n"
"%% {snmpEngineID, \"agentEngine\"}.\n",
"%% {snmpEngineMaxMessageSize, 484}.\n",
    {ok, Fid} = file:open(filename:join(Dir,"agent.conf"),write),
    ok = io:format(Fid, "~s~s\n{intAgentUDPPort, ~s}.\n~w.\n"
		   "{snmpEngineID, \"~s\"}.\n"
		   "{snmpEngineMaxMessageSize, 484}.\n",
		   [header(), Comment, AgentUDP, {intAgentIpAddress, AgentIP},
		   EngineID]),
    file:close(Fid).

write_context_conf(Dir) ->
    Comment = 
"%% This files defines the contexts known to the agent.\n"
"%% The data is inserted into the vacmContextTable defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is a string:\n"
"%% ContextName.\n"
"%%\n"
"%% The empty string is the default context.\n"
"%% For example\n"
"%% \"bridge1\".\n"
"%% \"bridge2\".\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"context.conf"),write),
    ok = io:format(Fid, "~s~s\"\".\n",
		   [header(), Comment]),
    file:close(Fid).

write_community_conf(Dir) ->
    Comment = 
"%% This files defines the community info which maps to VACM parameters.\n"
"%% The data is inserted into the snmpCommunityTable defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {CommunityIndex, CommunityName, SecurityName, ContextName, TransportTag}.\n"
"%% For example\n"
"%% {1, \"public\", \"initial\", \"\", \"\"}.\n"
"%% {2, \"secret\", \"secret_name\", \"\", \"tag\"}.\n"
"%% {3, \"bridge1\", \"initial\", \"bridge1\", \"\"}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"community.conf"),write),
    ok = io:format(
	   Fid, "~s~s"
	   "{\"public\", \"public\", \"initial\", \"\", \"\"}.\n"
	   "{\"all-rights\", \"all-rights\", \"all-rights\", \"\", \"\"}.\n"
	   "{\"standard trap\", \"standard trap\", \"initial\", \"\", \"\"}.\n",
	   [header(), Comment]),
    file:close(Fid).

write_standard_conf(Dir, SysName) ->
        Comment = 
"%% This files defines the STANDARD-MIB info.\n"
"%% Each row is a 2-tuple:\n"
"%% {StandardVariable, Value}.\n"
"%% For example\n"
"%% {sysDescr, \"Erlang SNMP agent\"}.\n"
"%% {sysObjectID, [1,2,3]}.\n"
"%% {sysContact, \"{mbj,eklas}@erlang.ericsson.se\"}.\n"
"%% {sysName, \"test\"}.\n"
"%% {sysLocation, \"erlang\"}.\n"
"%% {sysServices, 72}.\n"
"%% {snmpEnableAuthenTraps, enabled}.\n"
"%%\n"
"{sysDescr, \"Erlang SNMP agent\"}.\n"
"{sysObjectID, [1,2,3]}.\n"
"{sysContact, \"{mbj,eklas}@erlang.ericsson.se\"}.\n"
"{sysLocation, \"erlang\"}.\n"
"{sysServices, 72}.\n"
"{snmpEnableAuthenTraps, enabled}.\n",
    {ok, Fid} = file:open(filename:join(Dir,"standard.conf"),write),
    ok = io:format(Fid, "~s~s{sysName, \"~s\"}.\n",
		   [header(), Comment, SysName]),
    file:close(Fid).
	
write_target_addr_conf(Dir, ManagerIp, UDP, Vers) -> 
        Comment = 
"%% This files defines the target address parameters.\n"
"%% The data is inserted into the snmpTargetAddrTable defined\n"
"%% in SNMP-TARGET-MIB, and in the snmpTargeAddrExtTabke defined\n"
"%% in SNMP-COMMUNITY-MIB.\n"
"%% Each row is a 9-tuple:\n"
"%% {Name, Ip, Udp, Timeout, RetryCount, TagList, ParamsName, EngineId,\n"
"%%        TMask, MaxMessageSize}.\n"
"%% The EngineId value is only used if Inform-Requests are sent to this\n"
"%% target.  If Informs are not sent, this value is ignored, and can be\n"
"%% e.g. an empty string.  However, if Informs are sent, it is essential\n"
"%% that the value of EngineId matches the value of the target's\n"
"%% actual snmpEngineID.\n"
"%% For example\n"
"%% {\"1.2.3.4 v1\", [1,2,3,4], 162, 1500, 3, \"std_inform\", \"otp_v2\", \"\",\n"
"%%                [127,0,0,0],  2048}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"target_addr.conf"),write),
    io:format(Fid, "~s~s", [header(), Comment]),
    lists:foreach(fun(Ver) ->
			  ok = io:format(Fid, "{\"~s\", ~w, ~s, 1500, 3, "
					 "\"std_trap\", \"target_v~w\", "
					 "\"\", [], 2048}.\n",
					 [mk_ip(ManagerIp, Ver),
					  ManagerIp, UDP, Ver])
		  end,
		  Vers),
    file:close(Fid).

mk_ip([A,B,C,D], Ver) ->
    io_lib:format("~w.~w.~w.~w v~w", [A,B,C,D,Ver]).

write_target_params_conf(Dir, Vers) -> 
        Comment = 
"%% This files defines the target parameters.\n"
"%% The data is inserted into the snmpTargetParamsTable defined\n"
"%% in SNMP-TARGET-MIB.\n"
"%% Each row is a 5-tuple:\n"
"%% {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}.\n"
"%% For example\n"
"%% {\"target_v3\", v3, usm, \"\", noAuthNoPriv}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"target_params.conf"),write),
    io:format(Fid, "~s~s", [header(), Comment]),
    lists:foreach(fun(Ver) ->
			  MP = if Ver == 1 -> v1;
				  Ver == 2 -> v2c;
				  Ver == 3 -> v3
			       end,
			  SM = if Ver == 1 -> v1;
				  Ver == 2 -> v2c;
				  Ver == 3 -> usm
			       end,
			  ok = io:format(Fid, "{\"target_v~w\", ~w, ~w, "
					 "\"initial\", noAuthNoPriv}.\n",
					 [Ver, MP, SM])
		  end,
		  Vers),
    file:close(Fid).

write_notify_conf(Dir, NotifyType) -> 
        Comment = 
"%% This files defines the notification parameters.\n"
"%% The data is inserted into the snmpNotifyTable defined\n"
"%% in SNMP-NOTIFICATION-MIB.\n"
"%% The Name is used as CommunityString for v1 and v2c.\n"
"%% Each row is a 3-tuple:\n"
"%% {Name, Tag, Type}.\n"
"%% For example\n"
"%% {\"standard trap\", \"std_trap\", trap}.\n"
"%% {\"standard inform\", \"std_inform\", inform}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"notify.conf"),write),
    ok = io:format(Fid, "~s~s{\"standard trap\", \"std_trap\", ~s}.\n",
		   [header(), Comment, NotifyType]),
    file:close(Fid).

write_usm_conf(Dir, Ver, EngineID, SecType, Passwd) -> 
    case lists:member(3, Ver) of
	false -> ok;
	true -> write_usm_conf(Dir, EngineID, SecType, Passwd)
    end.

write_usm_conf(Dir, EngineID, SecType, Passwd) -> 
        Comment = 
"%% This files defines the security parameters for the user-based\n"
"%% security model.\n"
"%% The data is inserted into the usmUserTable defined\n"
"%% in SNMP-USER-BASED-SM-MIB.\n"
"%% Each row is a 14-tuple:\n"
"%% {EngineID, UserName, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,\n"
"%%  PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}.\n"
"%% For example\n"
"%% {\"agentEngine\", \"initial\", \"initial\", zeroDotZero,\n"
"%%  usmNoAuthProtocol, \"\", \"\", usmNoPrivProtocol, \"\", \"\", \"\",\n"
"%%  \"\", \"\"}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"usm.conf"),write),
    case SecType of
	none ->
	    ok = io:format(Fid, "~s~s\n"
			   "{\"~s\", \"initial\", \"initial\", zeroDotZero, "
			   "usmNoAuthProtocol, \"\", \"\", "
			   "usmNoPrivProtocol, \"\", \"\", \"\", "
			   "\"\", \"\"}.\n",
			   [header(), Comment, EngineID]);
	_ ->
	    Secret16 = mk_secret(md5, Passwd, EngineID),
	    Secret20 = mk_secret(sha, Passwd, EngineID),
	    {PrivProt, PrivSecret} = 
		case SecType of
		    minimum ->
			{usmNoPrivProtocol, ""};
		    semi ->
			{usmDESPrivProtocol, Secret16}
		end,
	    ok = io:format(Fid, "~s~s\n"
			   "{\"~s\", \"initial\", \"initial\", zeroDotZero, "
			   "usmHMACMD5AuthProtocol, \"\", \"\", "
			   "~w, \"\", \"\", \"\", "
			   "~w, ~w}.\n",
			   [header(), Comment, EngineID, PrivProt,
			    Secret16, PrivSecret]),
	    ok = io:format(Fid,
			   "{\"~s\", \"templateMD5\", \"templateMD5\", "
			   "zeroDotZero, "
			   "usmHMACMD5AuthProtocol, \"\", \"\", "
			   "~w, \"\", \"\", \"\", "
			   "~w, ~w}.\n",
			   [EngineID, PrivProt, Secret16, PrivSecret]),
	    ok = io:format(Fid,
			   "{\"~s\", \"templateSHA\", \"templateSHA\", "
			   "zeroDotZero, "
			   "usmHMACSHAAuthProtocol, \"\", \"\", "
			   "~w, \"\", \"\", \"\", "
			   "~w, ~w}.\n",
			   [EngineID, PrivProt, Secret20, PrivSecret])
    end,
    file:close(Fid).


write_vacm_conf(Dir, Ver, SecType) -> 
        Comment = 
"%% This files defines the Mib Views.\n"
"%% The data is inserted into the vacm* tables defined\n"
"%% in SNMP-VIEW-BASED-ACM-MIB.\n"
"%% Each row is one of 3 tuples; one for each table in the MIB:\n"
"%% {vacmSecurityToGroup, SecModel, SecName, GroupName}.\n"
"%% {vacmAccess, GroupName, Prefix, SecModel, SecLevel, Match, RV, WV, NV}.\n"
"%% {vacmViewTreeFamily, ViewIndex, ViewSubtree, ViewStatus, ViewMask}.\n"
"%% For example\n"
"%% {vacmSecurityToGroup, v2c, \"initial\", \"initial\"}.\n"
"%% {vacmSecurityToGroup, usm, \"initial\", \"initial\"}.\n"
"%%  read/notify access to system\n"
"%% {vacmAccess, \"initial\", \"\", any, noAuthNoPriv, exact,\n"
"%%              \"system\", \"\", \"system\"}.\n"
"%% {vacmViewTreeFamily, \"system\", [1,3,6,1,2,1,1], included, null}.\n"
"%% {vacmViewTreeFamily, \"exmib\", [1,3,6,1,3], included, null}."
" % for EX1-MIB\n"
"%% {vacmViewTreeFamily, \"internet\", [1,3,6,1], included, null}.\n"
"%%\n",
    {ok, Fid} = file:open(filename:join(Dir,"vacm.conf"),write),
    Groups = lists:foldl(
	       fun(V, Str) ->
		       io_lib:format("~s{vacmSecurityToGroup, ~w, \"initial\", "
				     "\"initial\"}.\n"
				     "{vacmSecurityToGroup, ~w, "
				     "\"all-rights\", \"all-rights\"}.\n",
				     [Str, vacm_ver(V), vacm_ver(V)])
	       end,
	       "", Ver),
    Restricted = case SecType of
		     none ->
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1], included, null}.\n";
		     minimum ->
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1], included, null}.\n";
		     semi ->
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1,2,1,1], included, null}.\n"
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1,2,1,11], included, null}.\n"
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1,6,3,10,2,1], included, null}.\n"
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1,6,3,11,2,1], included, null}.\n"
			 "{vacmViewTreeFamily, \"restricted\", "
			     "[1,3,6,1,6,3,15,1,1], included, null}.\n"
		 end,
    ok = io:format(Fid, "~s~s\n~s\n"
		   "{vacmAccess, \"initial\", \"\", any, noAuthNoPriv, exact, "
		   "\"restricted\", \"\", \"restricted\"}.\n"
		   "{vacmAccess, \"initial\", \"\", usm, authNoPriv, exact, "
		   "\"internet\", \"internet\", \"internet\"}.\n"
		   "{vacmAccess, \"initial\", \"\", usm, authPriv, exact, "
		   "\"internet\", \"internet\", \"internet\"}.\n\n"
		   "{vacmAccess, \"all-rights\", \"\", any, noAuthNoPriv, "
		   "exact, \"internet\", \"internet\", \"internet\"}.\n\n"

		   "{vacmViewTreeFamily, \"internet\", [1,3,6,1], "
		   "included, null}.\n"
		   "~s",
		   [header(), Comment, Groups, Restricted]),
    file:close(Fid).

vacm_ver(1) -> v1;
vacm_ver(2) -> v2c;
vacm_ver(3) -> usm.
     

write_sys_config(Dir,Version,Verbosity) ->
    {ok, Fid} = file:open(filename:join(Dir,"sys.config"),write),
    ok = io:format(Fid, 
		   "~s[{snmp, [{snmp_config_dir, \"~s\"},\n"
		   "         {snmp_db_dir, \"~s\"},\n~s"
		   "         ~s]}].\n",
		   [header(),Dir,Dir,verbosity(Verbosity),
		    ver_to_sys_str(Version)]),
    file:close(Fid).


write_sys_config(Dir,Version) ->
    {ok, Fid} = file:open(filename:join(Dir,"sys.config"),write),
    ok = io:format(Fid, "~s[{snmp, [{snmp_config_dir, \"~s\"},\n"
		   "         {snmp_db_dir, \"~s\"},\n         ~s]}].\n",
		   [header(),Dir,Dir,ver_to_sys_str(Version)]),
    file:close(Fid).


verbosity(V) ->
    verbosity(V,"").

verbosity([],S) ->
    S;
verbosity([H|T],S) ->
    verbosity(T,io_lib:format("~s         ~w,\n",[S,H])).


ver_to_sys_str(Vsns) ->
    io_lib:format("{v1, ~w}, {v2, ~w}, {v3, ~w}",
		  [lists:member(1, Vsns),
		   lists:member(2, Vsns),
		   lists:member(3, Vsns)]).				

header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was generated by "
		  "snmp_config (v~s) ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

mk_secret(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).
