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
-module(dbg_iasm).

-export([module/3]).
-export([format_error/1]).

module({Mod,Exp,Forms},Defs,FileN) ->
    case file:read_file(FileN) of
	{ok,Binary} ->
	    module(Mod,Exp,Forms,Defs,FileN,Binary);
	{error,E} ->
	    {error, [{dbg_iasm, {read, FileN, E}}]}
    end.

module(Mod, Exp, Forms, Defs0, FileN, Src) ->
    Defs = string_defs(Defs0),
    {ok,term_to_binary({interpreter_module,Exp,Defs,Forms,Src})}.

string_defs([{D,{Args,Toks}}|Defs]) ->
    case catch mk_string(Toks) of
	{'EXIT',_} ->
	    [{D,[]}|string_defs(Defs)];
	String ->
	    A = args(Args),
	    S = io_lib:format("-define(~w~s,~s).",[D,A,String]),
	    [{D,lists:flatten(S)}|string_defs(Defs)]
    end;
string_defs([]) ->
    [].

args(none) -> "";
args(A) ->
    case catch arg_list(A) of
	{'EXIT',_} ->
	    ["(...)"];
	Al ->
	    ["("|Al]
    end.

arg_list([A1,A2|T]) ->
    [atom_to_list(A1),","|arg_list([A2|T])];
arg_list([A]) ->
    [atom_to_list(A),")"];
arg_list([]) ->
    [")"].

mk_string([]) ->
    [];
mk_string([{atom,_,A}|T]) ->
    [atom_to_list(A)|mk_string(T)];
mk_string([{integer,_,I}|T]) ->
    [integer_to_list(I)|mk_string(T)];
mk_string([{float,_,F}|T]) ->
    [float_to_list(F)|mk_string(T)];
mk_string([{char,_,C}|T]) ->
    [atom_to_list(C)|mk_string(T)];
mk_string([{var,_,V}|T]) ->
    [atom_to_list(V)|mk_string(T)];
mk_string([{string,L,S1},{string,_,S2}|T]) ->
    mk_string([{string,L,lists:append(S1,S2)}|T]);
mk_string([{string,_,S}|T]) ->
    ["\"",S,"\""|mk_string(T)];
mk_string([{'->',_}|T]) ->
    [" -> "|mk_string(T)];
mk_string([{';',_}|T]) ->
    ["; "|mk_string(T)];
mk_string([{W,_}|T]) ->
    case erl_scan:reserved_word(W) of
	true ->
	    [" ",atom_to_list(W)," "|mk_string(T)];
	_ ->
	    [atom_to_list(W)|mk_string(T)]
    end.

format_error({read,File,Error}) ->
    io_lib:format("dbg_iasm: Could not read file ~p (Reason: ~p)",
		  [File,Error]);
format_error(Other) ->
    io_lib:format("dbg_iasm: Error - ~p",[Other]).
