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
-module(beam_listing).

-export([module/2]).

-import(lists, [foreach/2]).

module(File, Core) when element(1, Core) == c_mdef ->
    %% This is a core module.
    io:put_chars(File, core_pp:format(Core));
module(File, Kern) when element(1, Kern) == k_mdef ->
    %% This is a kernel module.
    io:put_chars(File, v3_kernel_pp:format(Kern));
    %%io:put_chars(File, io_lib:format("~p~n", [Kern]));
module(File, {Mod,Exp,Ker}) ->
    %% This is output from sys_life (v2).
    %%io:fwrite(File, "-module(~w).~n-export(~p).~n", [Mod,Exp]),
    io:fwrite(File, "~w.~n~p.~n", [Mod,Exp]),
    foreach(fun (F) -> io:put_chars(File, function(F)) end, Ker);
module(File, {Mod,Exp,Attr,Ker}) ->
    %% This is output from beam_life (v3).
    io:fwrite(File, "~w.~n~p.~n~p.~n", [Mod,Exp,Attr]),
    foreach(fun (F) -> io:put_chars(File, function(F)) end, Ker);
module(Stream, {Mod,Exp,Attr,Code,NumLabels}) ->
    %% This is output from beam_codegen.
    io:format(Stream, "{module, ~s}.  %% version = ~w\n", 
	      [Mod, beam_opcodes:format_number()]),
    io:format(Stream, "\n{exports, ~p}.\n", [Exp]),
    io:format(Stream, "\n{attributes, ~p}.\n", [Attr]),
    io:format(Stream, "\n{labels, ~p}.\n", [NumLabels]),
    foreach(
      fun ({function,Name,Arity,Entry,Asm}) ->
	      io:format(Stream, "\n\n{function, ~w, ~w, ~w}.\n",
			[Name, Arity, Entry]),
	      foreach(fun(Op) -> print_op(Stream, Op) end, Asm) end,
      Code).

function(F) ->
    io_lib:format("~p.~n", [F]).

print_op(Stream, Label) when element(1, Label) == label ->
    io:format(Stream, "  ~p.\n", [Label]);
print_op(Stream, Op) ->
    io:format(Stream, "    ~p.\n", [Op]).

    
