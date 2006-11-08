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
-module(otp_internal).

-export([obsolete/3]).

obsolete(init, get_flag, 1) ->
    {true, {init, get_argument, 1}};
obsolete(init, get_flags, 0) ->
    {true, {init, get_arguments, 0}};
obsolete(init, get_args, 0) ->
    {true, {init, get_plain_arguments, 0}};
obsolete(unix, cmd, 1) ->
    {true, {os, cmd, 1}};

%% This function is sometimes useful.
%% obsolete(calendar, time_difference, 2) ->
%%    {true, "not recommended; use the functions for gregorian "
%%     "days/seconds instead"};

obsolete(net, _, _) ->
    {true, "module 'net' obsolete; use 'net_adm'"};

obsolete(erl_internal, builtins, 0) ->
    {true, {erl_internal, bif, 2}};

obsolete(string, re_sh_to_awk, 1) ->
    {true, {regexp, sh_to_awk, 1}};
obsolete(string, re_parse, 1) ->
    {true, {regexp, parse, 1}};
obsolete(string, re_match, 2) ->
    {true, {regexp, match, 2}};
obsolete(string, re_sub, 3) ->
    {true, {regexp, sub, 3}};
obsolete(string, re_gsub, 3) ->
    {true, {regexp, gsub, 3}};
obsolete(string, re_split, 2) ->
    {true, {regexp, split, 2}};

obsolete(string, index, 2) ->
    {true, {string, str, 2}};

obsolete(erl_eval, seq, 2) ->
    {true, {erl_eval, exprs, 2}};
obsolete(erl_eval, seq, 3) ->
    {true, {erl_eval, exprs, 3}};
obsolete(erl_eval, arg_list, 2) ->
    {true, {erl_eval, expr_list, 2}};
obsolete(erl_eval, arg_list, 3) ->
    {true, {erl_eval, expr_list, 3}};

obsolete(erl_pp, seq, 1) ->
    {true, {erl_pp, exprs, 1}};
obsolete(erl_pp, seq, 2) ->
    {true, {erl_pp, exprs, 2}};

obsolete(io, scan_erl_seq, 1) ->
    {true, {io, scan_erl_exprs, 1}};
obsolete(io, scan_erl_seq, 2) ->
    {true, {io, scan_erl_exprs, 2}};
obsolete(io, scan_erl_seq, 3) ->
    {true, {io, scan_erl_exprs, 3}};
obsolete(io, parse_erl_seq, 1) ->
    {true, {io, parse_erl_exprs, 1}};
obsolete(io, parse_erl_seq, 2) ->
    {true, {io, parse_erl_exprs, 2}};
obsolete(io, parse_erl_seq, 3) ->
    {true, {io, parse_erl_exprs, 3}};
obsolete(io, parse_exprs, 2) ->
    {true, {io, parse_erl_exprs, 2}};

obsolete(io_lib, scan, 1) ->
    {true, {erl_scan, string, 1}};
obsolete(io_lib, scan, 2) ->
    {true, {erl_scan, string, 2}};
obsolete(io_lib, scan, 3) ->
    {true, {erl_scan, tokens, 3}};
obsolete(io_lib, reserved_word, 1) ->
    {true, {erl_scan, reserved_word, 1}};

obsolete(lists, keymap, 4) ->
    {true, {lists, keymap, 3}};
obsolete(lists, all, 3) ->
    {true, {lists, all, 2}};
obsolete(lists, any, 3) ->
    {true, {lists, any, 2}};
obsolete(lists, map, 3) ->
    {true, {lists, map, 2}};
obsolete(lists, flatmap, 3) ->
    {true, {lists, flatmap, 2}};
obsolete(lists, foldl, 4) ->
    {true, {lists, foldl, 3}};
obsolete(lists, foldr, 4) ->
    {true, {lists, foldr, 3}};
obsolete(lists, mapfoldl, 4) ->
    {true, {lists, mapfoldl, 3}};
obsolete(lists, mapfoldr, 4) ->
    {true, {lists, mapfoldr, 3}};
obsolete(lists, filter, 3) ->
    {true, {lists, filter, 2}};
obsolete(lists, foreach, 3) ->
    {true, {lists, foreach, 2}};

obsolete(ets, fixtable, 2) ->
    {true, {ets, safe_fixtable, 2}};

obsolete(erlang, old_binary_to_term, 1) ->
    {true, "deprecated BIF"};
obsolete(erlang, info, 1) ->
    {true, {erlang, system_info, 1}};
obsolete(erlang, hash, 2) ->
    {true, {erlang, phash2, 2}};

obsolete(file, file_info, 1) ->
    {true, {file, read_file_info, 1}};

obsolete(dict, dict_to_list, 1) ->
    {true, {dict, to_list, 1}};
obsolete(dict, list_to_dict, 1) ->
    {true, {dict, from_list, 1}};
obsolete(orddict, dict_to_list, 1) ->
    {true, {orddict, to_list, 1}};
obsolete(orddict, list_to_dict, 1) ->
    {true, {orddict, from_list, 1}};

obsolete(sets, new_set, 0) ->
    {true, {sets, new, 0}};
obsolete(sets, set_to_list, 1) ->
    {true, {sets, to_list, 1}};
obsolete(sets, list_to_set, 1) ->
    {true, {sets, from_list, 1}};
obsolete(sets, subset, 2) ->
    {true, {sets, is_subset, 2}};
obsolete(ordsets, new_set, 0) ->
    {true, {ordsets, new, 0}};
obsolete(ordsets, set_to_list, 1) ->
    {true, {ordsets, to_list, 1}};
obsolete(ordsets, list_to_set, 1) ->
    {true, {ordsets, from_list, 1}};
obsolete(ordsets, subset, 2) ->
    {true, {ordsets, is_subset, 2}};

obsolete(calendar, local_time_to_universal_time, 1) ->
    {true, {calendar, local_time_to_universal_time_dst, 1}};

obsolete(rpc, safe_multi_server_call, A) when A =:= 2; A =:= 3 ->
    {true, {rpc, multi_server_call, A}};

obsolete(snmp, N, A) ->
    case is_snmp_agent_function(N, A) of
	false -> false;
	true ->
	    {true,"Deprecated; use snmpa:"++atom_to_list(N)++"/"++
	     integer_to_list(A)++" instead"}
    end;

obsolete(megaco, format_versions, 1) ->
    {true, "Deprecated; use megaco:print_version_info/0,1 instead"};

obsolete(os_mon_mib, init, 1) ->
    {true, {os_mon_mib, load, 1}};
obsolete(os_mon_mib, stop, 1) ->
    {true, {os_mon_mib, unload, 1}};

obsolete(auth, is_auth, 1) ->
    {true, {net_adm, ping, 1}};
obsolete(auth, cookie, 0) ->
    {true, {erlang, get_cookie, 0}};
obsolete(auth, cookie, 1) ->
    {true, {erlang, set_cookie, 2}};
obsolete(auth, node_cookie, 1) ->
    {true, "Deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead"};
obsolete(auth, node_cookie, 2) ->
    {true, "Deprecated; use erlang:set_cookie/2 and net_adm:ping/1 instead"};

obsolete(_, _, _) ->
    false.

is_snmp_agent_function(c,                     1) -> true;
is_snmp_agent_function(c,                     2) -> true;
is_snmp_agent_function(compile,               3) -> true;
is_snmp_agent_function(is_consistent,         1) -> true;
is_snmp_agent_function(mib_to_hrl,            1) -> true;
is_snmp_agent_function(change_log_size,       1) -> true;
is_snmp_agent_function(log_to_txt,            2) -> true;
is_snmp_agent_function(log_to_txt,            3) -> true;
is_snmp_agent_function(log_to_txt,            4) -> true;
is_snmp_agent_function(current_request_id,    0) -> true;
is_snmp_agent_function(current_community,     0) -> true;
is_snmp_agent_function(current_address,       0) -> true;
is_snmp_agent_function(current_context,       0) -> true;
is_snmp_agent_function(current_net_if_data,   0) -> true;
is_snmp_agent_function(get_symbolic_store_db, 0) -> true;
is_snmp_agent_function(name_to_oid,           1) -> true;
is_snmp_agent_function(name_to_oid,           2) -> true;
is_snmp_agent_function(oid_to_name,           1) -> true;
is_snmp_agent_function(oid_to_name,           2) -> true;
is_snmp_agent_function(int_to_enum,           2) -> true;
is_snmp_agent_function(int_to_enum,           3) -> true;
is_snmp_agent_function(enum_to_int,           2) -> true;
is_snmp_agent_function(enum_to_int,           3) -> true;
is_snmp_agent_function(get,                   2) -> true;
is_snmp_agent_function(info,                  1) -> true;
is_snmp_agent_function(load_mibs,             2) -> true;
is_snmp_agent_function(unload_mibs,           2) -> true;
is_snmp_agent_function(dump_mibs,             0) -> true;
is_snmp_agent_function(dump_mibs,             1) -> true;
is_snmp_agent_function(register_subagent,     3) -> true;
is_snmp_agent_function(unregister_subagent,   2) -> true;
is_snmp_agent_function(send_notification,     3) -> true;
is_snmp_agent_function(send_notification,     4) -> true;
is_snmp_agent_function(send_notification,     5) -> true;
is_snmp_agent_function(send_notification,     6) -> true;
is_snmp_agent_function(send_trap,             3) -> true;
is_snmp_agent_function(send_trap,             4) -> true;
is_snmp_agent_function(add_agent_caps,        2) -> true;
is_snmp_agent_function(del_agent_caps,        1) -> true;
is_snmp_agent_function(get_agent_caps,        0) -> true;
is_snmp_agent_function(_,		      _) -> false.

