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
-module(erl_internal).

%% Define Erlang bifs, guard tests and other internal stuff.

-export([bif/2,bif/3,guard_bif/2,type_test/2,obsolete/3]).
-export([arith_op/2,bool_op/2,comp_op/2,list_op/2,send_op/2,op_type/2]).
-export([behaviour_info/0, behaviour_info/1]).

%% -type guard_bif(Name, Arity) -> bool()
%%	when Name = atom(), Arity = integer().
%%  Erlang builtin functions allowed in guards.

guard_bif(abs, 1) -> true;
guard_bif(float, 1) -> true;
guard_bif(trunc, 1) -> true;
guard_bif(round, 1) -> true;
guard_bif(length, 1) -> true;
guard_bif(hd, 1) -> true;
guard_bif(tl, 1) -> true;
guard_bif(size, 1) -> true;
guard_bif(element, 2) -> true;
guard_bif(self, 0) -> true;
guard_bif(node,0) -> true;
guard_bif(node,1) -> true;
guard_bif(N, A) -> false.

%% -type type_test(Name, Arity) -> bool()
%%	when Name = atom(), Arity = integer().
%%  Erlang type tests.

type_test(integer, 1) -> true;
type_test(float, 1) -> true;
type_test(number, 1) -> true;
type_test(atom, 1) -> true;
type_test(constant, 1) -> true;
type_test(list, 1) -> true;
type_test(tuple, 1) -> true;
type_test(pid, 1) -> true;
type_test(reference, 1) -> true;
type_test(port, 1) -> true;
type_test(binary, 1) -> true;
type_test(record, 2) -> true;
type_test(function, 1) -> true;
type_test(N, A) -> false.

%% -type arith_op(Op, Arity) -> bool()
%%	when Op = atom(), Arity = integer().

arith_op('+', 1) -> true;
arith_op('-', 1) -> true;
arith_op('*', 2) -> true;
arith_op('/', 2) -> true;
arith_op('+', 2) -> true;
arith_op('-', 2) -> true;
arith_op('bnot', 1) -> true;
arith_op('div', 2) -> true;
arith_op('rem', 2) -> true;
arith_op('band', 2) -> true;
arith_op('bor', 2) -> true;
arith_op('bxor', 2) -> true;
arith_op('bsl', 2) -> true;
arith_op('bsr', 2) -> true;
arith_op(N, A) -> false.

%% -type bool_op(Op, Arity) -> bool()
%%	when Op = atom(), Arity = integer().

bool_op('not', 1) -> true;
bool_op('and', 2) -> true;
bool_op('or', 2) -> true;
bool_op('xor', 2) -> true;
bool_op(N, A) -> false.

%% -type comp_op(Op, Arity) -> bool()
%%	when Op = atom(), Arity = integer().

comp_op('==', 2) -> true;
comp_op('/=', 2) -> true;
comp_op('=<', 2) -> true;
comp_op('<', 2) -> true;
comp_op('>=', 2) -> true;
comp_op('>', 2) -> true;
comp_op('=:=', 2) -> true;
comp_op('=/=', 2) -> true;
comp_op(N, A) -> false.

%% -type list_op(Op, Arity) -> bool()
%%	when Op = atom(), Arity = integer().

list_op('++', 2) -> true;
list_op('--', 2) -> true;
list_op(_, _) -> false.

%% -type send_op(Op, Arity) -> bool()
%%	when Op = atom(), Arity = integer().

send_op('!', 2) -> true;
send_op(_, _) -> false.

%% -type op_type(Op, Arity) -> arith | bool | comp | list | send
%%	when Op = atom(), Arity = integer().

op_type('+', 1) -> arith;
op_type('-', 1) -> arith;
op_type('*', 2) -> arith;
op_type('/', 2) -> arith;
op_type('+', 2) -> arith;
op_type('-', 2) -> arith;
op_type('bnot', 1) -> arith;
op_type('div', 2) -> arith;
op_type('rem', 2) -> arith;
op_type('band', 2) -> arith;
op_type('bor', 2) -> arith;
op_type('bxor', 2) -> arith;
op_type('bsl', 2) -> arith;
op_type('bsr', 2) -> arith;
op_type('not', 1) -> bool;
op_type('and', 2) -> bool;
op_type('or', 2) -> bool;
op_type('xor', 2) -> bool;
op_type('==', 2) -> comp;
op_type('/=', 2) -> comp;
op_type('=<', 2) -> comp;
op_type('<', 2) -> comp;
op_type('>=', 2) -> comp;
op_type('>', 2) -> comp;
op_type('=:=', 2) -> comp;
op_type('=/=', 2) -> comp;
op_type('++', 2) -> list;
op_type('--', 2) -> list;
op_type('!', 2) -> send.

bif(erlang, Name, Arity) -> bif(Name, Arity);
bif(Mod, Func, Arity) -> false.

%% bif(Name, Arity) -> true|false
%%   Returns true if erlang:Name/Arity is an auto-imported BIF, false otherwise.
%%   Use erlang:is_bultin(Mod, Name, Arity) to find whether a function is a BIF
%%   (meaning implemented in C) or not.

bif(abs, 1) -> true;
bif(apply, 2) -> true;
bif(apply, 3) -> true;
bif(atom_to_list, 1) -> true;
bif(binary_to_list, 1) -> true;
bif(binary_to_list, 3) -> true;
bif(binary_to_term, 1) -> true;
bif(check_process_code, 2) -> true;
bif(concat_binary, 1) -> true;
bif(date, 0) -> true;
bif(delete_module, 1) -> true;
bif(disconnect_node, 1) -> true;
bif(element, 2) -> true;
bif(erase, 0) -> true;
bif(erase, 1) -> true;
bif(exit, 1) -> true;
bif(exit, 2) -> true;
bif(float, 1) -> true;
bif(float_to_list, 1) -> true;
bif(garbage_collect, 0) -> true;
bif(garbage_collect, 1) -> true;
bif(get, 0) -> true;
bif(get, 1) -> true;
bif(get_keys, 1) -> true;
bif(group_leader, 0) -> true;
bif(group_leader, 2) -> true;
bif(halt, 0) -> true;
bif(halt, 1) -> true;
bif(hd, 1) -> true;
bif(integer_to_list, 1) -> true;
bif(is_alive, 0) -> true;
bif(is_process_alive, 1) -> true;
bif(length, 1) -> true;
bif(link, 1) -> true;
bif(list_to_atom, 1) -> true;
bif(list_to_binary, 1) -> true;
bif(list_to_float, 1) -> true;
bif(list_to_integer, 1) -> true;
bif(list_to_pid, 1) -> true;
bif(list_to_tuple, 1) -> true;
bif(load_module, 2) -> true;
bif(make_ref, 0) -> true;
bif(module_loaded, 1) -> true;
bif(monitor_node, 2) -> true;
bif(node, 0) -> true;
bif(node, 1) -> true;
bif(nodes, 0) -> true;
bif(now, 0) -> true;
bif(open_port, 2) -> true;
bif(pid_to_list, 1) -> true;
bif(port_close, 1) -> true;
bif(port_command, 2) -> true;
bif(port_connect, 2) -> true;
bif(port_control, 3) -> true;
bif(pre_loaded, 0) -> true;
bif(process_flag, 2) -> true;
bif(process_flag, 3) -> true;
bif(process_info, 1) -> true;
bif(process_info, 2) -> true;
bif(processes, 0) -> true;
bif(purge_module, 1) -> true;
bif(put, 2) -> true;
bif(register, 2) -> true;
bif(registered, 0) -> true;
bif(round, 1) -> true;
bif(self, 0) -> true;
bif(setelement, 3) -> true;
bif(size, 1) -> true;
bif(spawn, 1) -> true;
bif(spawn, 2) -> true;
bif(spawn, 3) -> true;
bif(spawn, 4) -> true;
bif(spawn_link, 1) -> true;
bif(spawn_link, 2) -> true;
bif(spawn_link, 3) -> true;
bif(spawn_link, 4) -> true;
bif(spawn_opt, 4) -> true;
bif(split_binary, 2) -> true;
bif(statistics, 1) -> true;
bif(term_to_binary, 1) -> true;
bif(term_to_binary, 2) -> true;
bif(throw, 1) -> true;
bif(time, 0) -> true;
bif(tl, 1) -> true;
bif(trunc, 1) -> true;
bif(tuple_to_list, 1) -> true;
bif(unlink, 1) -> true;
bif(unregister, 1) -> true;
bif(whereis, 1) -> true;
bif(Name, Arity) -> false.

behaviour_info() -> otp_internal:behaviour_info().
behaviour_info(Key) -> otp_internal:behaviour_info(Key).

obsolete(Mod, Fun, Arity) ->
    %% Just in case.
    case catch otp_internal:obsolete(Mod, Fun, Arity) of
	{true,Arg} -> {true,Arg};
	Other -> false				%False, no otp_internal
    end.
