%%-compile(export_all).
%%-export([Function/Arity, ...]).
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

-module(com_gen_SUITE).
-author('jakob@gamgi').

%% Test functions
-export([all/1,not_run/1,init_per_testcase/2, fin_per_testcase/2,
	 ado_gen_dispatch/1, ado_gen_virtual/1,
	 excel_gen_dispatch/1, excel_gen_virtual/1]).

-include("test_server.hrl").
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(Case, Config) ->
    ?line Datadir=?config(data_dir, Config),
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(suite) ->
    case os:type() of
	{win32, _} ->
	    [ado_gen_dispatch, ado_gen_virtual, excel_gen_dispatch, excel_gen_virtual];
	_ ->
	    [not_run]
    end.

not_run(doc) -> ["Doesn't run on UNIX"];
not_run(suite) -> [];
not_run(Config) when list(Config) -> {comment, "Doesn't run on UNIX."}.

get_erl_ado_obj(Driver) ->
    ?line {ok, Pid}= erl_com:get_or_start(list_to_atom("tst_"++atom_to_list(Driver)), Driver),
    ?line erl_com:create_dispatch(Pid, "ADODB.Connection", 15).

get_erl_excel_obj(Driver) ->
    ?line {ok, Pid}= erl_com:get_or_start(list_to_atom("tst_"++atom_to_list(Driver)), Driver),
    ?line erl_com:create_dispatch(Pid, "Excel.Application", 4).

ado_gen_dispatch(suite) -> [];
ado_gen_dispatch(doc) -> ["Generating ADO with dispatch flag"];
ado_gen_dispatch(Config) when list(Config) ->
    ?line C= get_erl_ado_obj(program),
    ?line file:set_cwd(filename:join(?config(data_dir, Config), "ado_dispatch")),
    ?line {ok, Files}= file:list_dir("."),
    ?line lists:foreach(fun(F) -> file:delete(F) end, Files),
    ?line com_gen:gen_typelib(C, [{verbose, 2}]),
    ?line {ok, Genfiles}= file:list_dir("."),
    ?line Erlfiles= lists:filter(fun(F) -> lists:suffix(".erl", F) end, Genfiles),
    ?line lists:foreach(fun(F) -> {ok, _, _}= compile:file(F, [binary]) end, Erlfiles),
    ok.

ado_gen_virtual(suite) -> [];
ado_gen_virtual(doc) -> ["Generating ADO with virtual flag"];
ado_gen_virtual(Config) when list(Config) ->
    ?line C= get_erl_ado_obj(program),
    ?line file:set_cwd(filename:join(?config(data_dir, Config), "ado_virtual")),
    ?line {ok, Files}= file:list_dir("."),
    ?line lists:foreach(fun(F) -> file:delete(F) end, Files),
    ?line com_gen:gen_typelib(C, [{verbose, 2}, virtual]),
    ?line {ok, Genfiles}= file:list_dir("."),
    ?line Erlfiles= lists:filter(fun(F) -> lists:suffix(".erl", F) end, Genfiles),
    ?line lists:foreach(fun(F) -> {ok, _, _}= compile:file(F, [binary]) end, Erlfiles),
    ok.

excel_gen_virtual(suite) -> [];
excel_gen_virtual(doc) -> ["Generating Excel w virtual flag"];
excel_gen_virtual(Config) when list(Config) ->
    ?line C= get_erl_excel_obj(program),
    ?line file:set_cwd(filename:join(?config(data_dir, Config), "excel_virtual")),
    ?line {ok, Files}= file:list_dir("."),
    ?line lists:foreach(fun(F) -> file:delete(F) end, Files),
    ?line com_gen:gen_typelib(C, [{verbose, 2}, virtual]),
    ?line {ok, Genfiles}= file:list_dir("."),
    ?line Erlfiles= lists:filter(fun(F) -> lists:suffix(".erl", F) end, Genfiles),
    ?line lists:foreach(fun(F) -> {ok, _, _}= compile:file(F, [binary]) end, Erlfiles),
    ok.

excel_gen_dispatch(suite) -> [];
excel_gen_dispatch(doc) -> ["Generating Excel w dispatch flag"];
excel_gen_dispatch(Config) when list(Config) ->
    ?line C= get_erl_excel_obj(program),
    ?line file:set_cwd(filename:join(?config(data_dir, Config), "excel_dispatch")),
    ?line {ok, Files}= file:list_dir("."),
    ?line lists:foreach(fun(F) -> file:delete(F) end, Files),
    ?line com_gen:gen_typelib(C, [{verbose, 2}, dispatch]),
    ?line {ok, Genfiles}= file:list_dir("."),
    ?line Erlfiles= lists:filter(fun(F) -> lists:suffix(".erl", F) end, Genfiles),
    ?line lists:foreach(fun(F) -> {ok, _, _}= compile:file(F, [binary]) end, Erlfiles),
    ok.
