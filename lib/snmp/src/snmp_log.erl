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
-module(snmp_log).

-export([create_log/0, log/4]).
-export([log_to_txt/2, log_to_txt/3, log_to_txt/4, log_to_txt/5, 
	 log_to_txt/6, log_to_txt/7,
	 change_log_size/1]).

-export([format/2]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").

-define(log_name, "snmp log").
-define(log_file, "snmp.log").

-define(VMODULE,"LOG").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%% This module contains Audit Trail Logging functions.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: create_log() -> {ok, Log} | false | {error, Error}
%% Types: Log = log()
%% Purpose: Checks the snmp application's parameters to see
%%          if a log should be created, and creates the log.
%%-----------------------------------------------------------------
create_log() ->
    LogType = case application:get_env(snmp, audit_trail_log) of
		  {ok, write_log} -> w_log;
		  {ok, read_write_log} -> rw_log;
		  _ -> false
	      end,
    case application:get_env(snmp, audit_trail_log_dir) of
	{ok, LogDir} when list(LogDir), LogType /= false ->
	    case application:get_env(snmp, audit_trail_log_size) of
		{ok, Size} ->
		    case open_disk_log(LogDir, Size, 1) of
			{error, Reason} ->
			    FileName = filename:join([LogDir, ?log_file]),
			    error_logger:error_msg("snmp: cannot create "
						   "audit trail log: ~s, "
						   "reason: ~w~n", 
						   [FileName, Reason]),
			    false;
			_ ->
			    {ok, LogType}
		    end;
		_ ->
		    {error, {snmp, undefined_parameter, audit_trail_log_size}}
	    end;
	_ when LogType == false ->
	    false;
	{ok, Dir1} ->
	    {error, {snmp, bad_type, {audit_trail_log_dir, Dir1}}};
	_ ->
	    {error, {snmp, undefined_parameter, audit_trail_log_dir}}
    end.

change_log_size(NewSize) ->
    disk_log:change_size(?log_name, NewSize).

open_disk_log(LogDir, Size, Try) ->
    case dlopen(LogDir, Size) of
	{error, Reason} when Try < 2 ->
	    ?vlog("failed opening log file: ~w", [Reason]),
	    clean_dir(LogDir),
	    open_disk_log(LogDir, Size, Try + 1);
	{error, Reason} ->
	    ?vlog("final log file open attempt failed: ~w", [Reason]),
	    {error, Reason};
	_  ->
	    ok
    end.

clean_dir(LogDir) ->
    case file:list_dir(LogDir) of
	{ok, Files} ->
	    lists:foreach(fun(F) ->
				  file:delete(filename:join(LogDir, F))
			  end, 
			  Files);
	_ ->
	    ok
    end.

dlopen(LogDir, Size) ->
    DLArgs = [{name, ?log_name},
	      {file, filename:join(LogDir,?log_file)},
	      {type, wrap},
	      {format, internal}],
    %% First try to open the log without the size-spec.  This will succeed
    %% if the log has already been created.  In that case, we'll use whatever
    %% size the log had at the time it was closed.
    case disk_log:open(DLArgs) of
	{error, {badarg, size}} ->
	    %% The log didn't exist, try with the size-spec
	    disk_log:open([{size, Size} | DLArgs]);
	Else ->
	    Else
    end.

%%-----------------------------------------------------------------
%% For efficiency reasons, we want to log the packet as a binary.
%% This is only possible for messages that are not encrypted.
%% Therefore, Packet can ba either a binary (encoded message), or
%% a tuple {V3Hdr, ScopedPduBytes}
%%-----------------------------------------------------------------
log(w_log, 'set-request', Packet, Addr) ->
    disk_log:alog(?log_name, {timestamp(), Packet, Addr});
log(w_log, _, _, _) -> ok;
log(_, _, Packet, Addr) ->
    disk_log:alog(?log_name, {timestamp(), Packet, Addr}).

timestamp() ->     
    {calendar:local_time(), calendar:universal_time()}.



log_to_txt(LogDir, Mibs) -> 
    log_to_txt(LogDir, Mibs, "./snmp_log.txt").
log_to_txt(LogDir, Mibs, TxtFile) ->
    log_to_txt(LogDir, Mibs, TxtFile, ?log_name).
log_to_txt(LogDir, Mibs, TxtFile, LogName) ->
    log_to_txt(LogDir, Mibs, TxtFile, LogName, ?log_file).
log_to_txt(LogDir, Mibs, TxtFile, LogName, LogFile) ->
    log_to_txt(LogDir, Mibs, TxtFile, LogName, LogFile, null).
log_to_txt(LogDir, Mibs, TxtFile, LogName, LogFile, Start) ->
    log_to_txt(LogDir, Mibs, TxtFile, LogName, LogFile, Start, null).

log_to_txt(LogDir, Mibs, TxtFile, LogName, LogFile, Start, Stop) ->
    %% First check if the caller process has already opened the
    %% log, because if we close an already open log we will cause
    %% a runtime error.
    LogPids = case disk_log:info(LogName) of
		  {error, {no_such_log, _}} ->
		      [];
		  {error, no_such_log} ->
		      [];
		  Info ->
		      Owners = lists:keysearch(owners, 1, Info),
		      {value, {_, Pids}} = Owners,
		      [P || {P, _} <- Pids]
	      end,
    case lists:member(self(), LogPids) of
	true ->
	    %% The caller already has the log open 
	    do_log_to_txt(LogName, TxtFile, Mibs, Start, Stop);
	false ->
	    case disk_log:open([{name, LogName},
				{file, filename:join(LogDir, LogFile)},
				{type, wrap},
				{format, internal},
				{mode, read_only}]) of
		{ok, _} ->
		    Res = do_log_to_txt(LogName, TxtFile, Mibs, Start, Stop),
		    disk_log:close(LogName),
		    Res;
		{error, {name_already_open, _}} ->
		    do_log_to_txt(LogName, TxtFile, Mibs, Start, Stop);
		{error, Reason} ->
		    {error, {LogName, Reason}}
	    end
    end.

do_log_to_txt(LogName, TxtFile, Mibs, Start, Stop) ->
    case file:open(TxtFile, write) of
	{ok, Fd} ->
	    Mib = snmp_misc:make_mini_mib(Mibs),
	    Res = (catch loop(disk_log:chunk(LogName, start), LogName, 
			      Mib, Start, Stop, Fd)),
	    file:close(Fd),
	    Res;
	{error, Reason} ->
	    {error, {TxtFile, Reason}}
    end.


loop({Cont, Terms}, LogName, Mib, Start, Stop, Fd) ->
    lists:foreach(fun(X) -> write(X, Mib, Start, Stop, Fd) end, Terms),
    loop(disk_log:chunk(LogName, Cont), LogName, Mib, Start, Stop, Fd);
loop(eof, _LogName, _Mib, _Start, _Stop, _Fd) ->
    ok;
loop(Error, _LogName, _Mib, _Start, _Stop, _Fd) ->
    Error.
			  
write({{V3Hdr, ScopedPdu}, Address}, Mib, Start, Stop, Fd) ->
    write({null, {V3Hdr, ScopedPdu}, Address}, Mib, Start, Stop, Fd);
write({TimeStamp, {V3Hdr, ScopedPdu}, Address}, Mib, Start, Stop, Fd) ->
    case timestamp_filter(TimeStamp,Start,Stop) of
	true ->
	    case catch snmp_pdus:dec_scoped_pdu(ScopedPdu) of
		ScopedPDU when record(ScopedPDU, scopedPdu) -> 
		    Msg = #message{version = 'version-3',
				   vsn_hdr = V3Hdr,
				   data = ScopedPDU},
		    w(ts2str(TimeStamp), Msg, Address, Mib, Fd);
		{'EXIT', R} ->
		    format_tab(Fd, "** error in log file ~p\n\n", [R])
	    end;
	false ->
	    ok
    end;
write({Packet, Address}, Mib, Start, Stop, Fd) ->
    write({null, Packet, Address}, Mib, Start, Stop, Fd);
write({TimeStamp, Packet, Address}, Mib, Start, Stop, Fd) ->
    case timestamp_filter(TimeStamp,Start,Stop) of
	true ->
	    case catch snmp_pdus:dec_message(binary_to_list(Packet)) of
		Msg when record(Msg, message) ->
		    w(ts2str(TimeStamp), Msg, Address, Mib, Fd);
		{'EXIT', R} ->
		    format_tab(Fd, "** error in log file ~p\n\n", [R])
	    end;
	false ->
	    ok
    end;
write(_, _Mib, _Start, _Stop, Fd) ->
    format_tab(Fd, "** unknown entry in log file\n\n", []).

w(TimeStamp, #message{version = Vsn, vsn_hdr = VsnHdr, data = Data}, 
  Addr, Mib, Fd) ->
    Str = format_pdu(Data, Mib),
    HdrStr = format_header(Vsn, VsnHdr),
    case get_type(Data) of
	trappdu ->
	    w_trap(TimeStamp, Vsn, HdrStr, Str, Addr, Fd);
	'snmpv2-trap' ->
	    w_trap(TimeStamp, Vsn, HdrStr, Str, Addr, Fd);
	'inform-request' ->
	    w_inform(TimeStamp, Vsn, HdrStr, Str, Addr, Fd);
	'get-response' ->
	    w_response(TimeStamp, Vsn, HdrStr, Str, Addr, Fd);
	report ->
	    w_report(TimeStamp, Vsn, HdrStr, Str, Addr, Fd);
	_ ->
	    w_request(TimeStamp, Vsn, HdrStr, Str, Addr, Fd)
    end.

w_request(TimeStamp, Vsn, HdrStr, Str, {Ip, Port}, Fd) ->
    format_tab(Fd, "request ~s:~w - ~s [~s] ~w\n~s", 
	      [ip(Ip), Port, HdrStr, TimeStamp, Vsn, Str]).

w_response(TimeStamp, Vsn, HdrStr, Str, {Ip, Port}, Fd) ->
    format_tab(Fd, "response ~s:~w - ~s [~s] ~w\n~s", 
	      [ip(Ip), Port, HdrStr, TimeStamp, Vsn, Str]).
    
w_report(TimeStamp, Vsn, HdrStr, Str, {Ip, Port}, Fd) ->
    format_tab(Fd, "report ~s:~w - ~s [~s] ~w\n~s", 
	      [ip(Ip), Port, HdrStr, TimeStamp, Vsn, Str]).
    
w_trap(TimeStamp, Vsn, HdrStr, Str, Addrs, Fd) ->
    format_tab(Fd, "trap ~s - ~s [~s] ~w\n~s", 
	      [addr(Addrs), HdrStr, TimeStamp, Vsn, Str]).

w_inform(TimeStamp, Vsn, HdrStr, Str, Addrs, Fd) ->
    format_tab(Fd, "inform ~s - ~s [~s] ~w\n~s", 
	      [addr(Addrs), HdrStr, TimeStamp, Vsn, Str]).


%% Convert a timestamp 2-tupple to a printable string
%%
ts2str({Local,Universal}) ->
    dat2str(Local) ++ " , " ++ dat2str(Universal);
ts2str(_) ->
    "".

%% Convert a datetime 2-tupple to a printable string
%%
dat2str({{Y,M,D},{H,Min,S}}) -> 
    io_lib:format("~w-~w-~w,~w:~w:~w",[Y,M,D,H,Min,S]).
    

timestamp_filter({Local,Universal},Start,Stop) ->
    tsf_ge(Local,Universal,Start) and tsf_le(Local,Universal,Stop);
timestamp_filter(_,_Start,_Stop) -> 
    true.

tsf_ge(_Local,_Universal,null) ->
    true;
tsf_ge(Local,_Universal,{local_time,DateTime}) ->
    tsf_ge(Local,DateTime);
tsf_ge(_Local,Universal,{universal_time,DateTime}) ->
    tsf_ge(Universal,DateTime);
tsf_ge(Local,_Universal,DateTime) ->
    tsf_ge(Local,DateTime).

tsf_ge(TimeStamp,DateTime) ->    
    T1 = calendar:datetime_to_gregorian_seconds(TimeStamp),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime),
    T1 >= T2.

tsf_le(_Local,_Universal,null) ->
    true;
tsf_le(Local,_Universal,{local_time,DateTime}) ->
    tsf_le(Local,DateTime);
tsf_le(_Local,Universal,{universal_time,DateTime}) ->
    tsf_le(Universal,DateTime);
tsf_le(Local,_Universal,DateTime) ->
    tsf_le(Local,DateTime).

tsf_le(TimeStamp,DateTime) ->
    T1 = calendar:datetime_to_gregorian_seconds(TimeStamp),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime),
    T1 =< T2.
	

%% In the output replace TAB by ESC TAB, and add a single trailing TAB.
%%
format_tab(Fd, Format, Args) ->
    Str = lists:flatten(io_lib:format(Format, Args)),
    DStr = lists:map(fun($\t) -> "\e\t"; (C) -> C end, Str),
    io:format(Fd, "~s\t", [DStr]).
		     

format_header('version-1', CommunityStr) ->
    CommunityStr;
format_header('version-2', CommunityStr) ->
    CommunityStr;
format_header('version-3', #v3_hdr{msgFlags = MsgFlags,
				   msgSecurityModel = SecModel,
				   msgSecurityParameters = SecParams}) ->
    SecLevel = snmp_misc:get_sec_level(MsgFlags),
    case SecModel of
	?SEC_USM ->
	    case catch snmp_pdus:dec_usm_security_parameters(SecParams) of
		#usmSecurityParameters{msgAuthoritativeEngineID = AuthEngineID,
				       msgUserName = UserName} ->
		    io_lib:format("~w:\"~s\":\"~s\"",
				  [SecLevel, AuthEngineID, UserName]);
		_ ->
		    "-"
	    end;
	_ ->
	    "\"unknown security model\""
    end.


format_pdu(#scopedPdu{contextName = Context, data = Pdu}, Mib) ->
    io_lib:format("Context: \"~s\"\n~s",
		  [Context, snmp_misc:format_pdu(Pdu, Mib)]);
format_pdu(Pdu, Mib) ->
    snmp_misc:format_pdu(Pdu, Mib).

get_type(#scopedPdu{data = Pdu}) ->
    get_type(Pdu);
get_type(Pdu) when record(Pdu, trappdu) ->
    trappdu;
get_type(Pdu) ->
    Pdu#pdu.type.



%% Addrs can be a single Addr or a list of Addr.
addr({snmpUDPDomain, {Ip, Port}}) ->
    io_lib:format("~s:~w", [ip(Ip), Port]);
addr({Ip, Port}) ->
    io_lib:format("~s:~w", [ip(Ip), Port]);
addr(Addrs) -> addr(Addrs, "").

addr([{snmpUDPDomain, {Ip, Port}}], Str) ->
    [Str, io_lib:format("~s:~w", [ip(Ip), Port])];
addr([{snmpUDPDomain, {Ip, Port}} | Addrs], _Str) ->
    addr(Addrs, [io_lib:format("~s:~w, ", [ip(Ip), Port])]);
%% Handle old format as well...
addr([{snmpUDPDomain, Ip, Port}], Str) ->
    [Str, io_lib:format("~s:~w", [ip(Ip), Port])];
addr([{snmpUDPDomain, Ip, Port} | Addrs], _Str) ->
    addr(Addrs, [io_lib:format("~s:~w, ", [ip(Ip), Port])]);
addr([], Str) ->
    Str.

ip({A,B,C,D}) ->
    io_lib:format("~w.~w.~w.~w", [A,B,C,D]).


%% Temporary fix, since we dont really know what Term is...
%% grumble, mutter, ...
%% Mib is a list of {Oid,Aliasname,bertype} (with no duplicates)
format({Packet,Ip,Port}, _Mib) when binary(Packet) ->
    L = binary_to_list(Packet),
    case catch snmp_pdus:dec_message(L) of
	{'EXIT', Reason} -> 
	    %% Ok, failed the entire message. Try decode without the pdu.
	    case catch snmp_pdus:dec_message_only(L) of
		{'EXIT', Reason} -> 
		    io_lib:format("failed decoding ~w bytes packet: ~p from ~p:~p",
				  [size(Packet),Packet,Ip,Port]);
		DecodedMessageOnly -> 
		    io_lib:format("~p received from ~p:~p",
				  [DecodedMessageOnly,Ip,Port])
		    
	    end;
	DecodedMessage -> 
	    io_lib:format("~p received from ~p:~p",[DecodedMessage,Ip,Port])
    end;
format(Term,_Mib) ->
    io_lib:format("~p",[Term]).

