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

obsolete(calendar, time_difference, 2) ->
    {true, "not recommended; use the functions for gregorian "
     "days/seconds instead"};

obsolete(net, _, _) ->
    {true, "module 'net' obsolete; use 'net_adm'"};

%obsolete(lib, _, _) ->
%    {true, "module 'lib' is deprecated"};
obsolete(socket, _, _) ->
    {true, "module 'socket' obsolete; use 'gen_tcp'"};
obsolete(udp, _, _) ->
    {true, "module 'udp' obsolete; use 'gen_udp'"};
obsolete(bplus_tree, _, _) ->
    {true, "module 'bplus_tree' is deprecated"};

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

obsolete(erlang, db_all_tables, 0) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_create, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_erase, 1) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_erase, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_first, 1) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_fixtable, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_get, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_get_element, 3) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_info, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_last, 1) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_match, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_match_erase, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_match_object, 3) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_next_key, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_prev_key, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_put, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_rename, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_slot, 2) ->
    {true, "deprecated BIF"};
obsolete(erlang, db_update_counter, 3) ->
    {true, "deprecated BIF"};
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

obsolete(_, _, _) ->
    false.
