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
-module(erts_debug).

%% Low-level debugging support. EXPERIMENTAL!

-export([size/1,df/1,df/2,df/3]).

%% This module contains the following *experimental* BIFs:
%%   apply/4 
%%   disassemble/1
%%   breakpoint/2
%%   same/2
%%   flat_size/1

%% size(Term)
%%  Returns the size of Term in actual heap words. Shared subterms are
%%  counted once.  Example: If A = [a,b], B =[A,A] then size(B) returns 8,
%%  while flat_size(B) returns 12.

size(Term) ->
    {Sum,Seen} = size(Term, [], 0),
    Sum.

size(Term, Seen0, Sum0) ->
    case erts_debug:flat_size(Term) of
	0 -> {Sum0,Seen0};
	Sz ->
	    case is_seen(Term, Seen0) of
		yes -> {Sum0,Seen0};
		no ->
		    Seen1 = [Term|Seen0],
		    case Term of
			[H|T] ->
			    {Sum,Seen} = size(H, Seen1, Sum0+2),
			    size(T, Seen, Sum);
			Tuple when tuple(Tuple) ->
			    Sum = Sum0 + 1 - size(Tuple),
			    size(tuple_to_list(Tuple), Seen1, Sum);
			Other ->
			    {Sum0+Sz,Seen1}
		    end
	    end
    end.

is_seen(Term, [H|T]) ->
    case erts_debug:same(Term, H) of
	true -> yes;
	false -> is_seen(Term, T)
    end;
is_seen(Term, []) -> no.

%% df(Mod)               -- Disassemble Mod to file Mod.dis.
%% df(Mod, Func)         -- Disassemble Mod:Func/Any to file Mod_Func.dis.
%% df(Mod, Func, Arity)  -- Disassemble Mod:Func/Arity to file Mod_Func_Arity.dis.

df(Mod) when atom(Mod) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod, ".dis"]),
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs);
	{'EXIT',Reason} ->
	    {undef,Mod}
    end.

df(Mod, Func) when atom(Mod), atom(Func) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod,"_",Func,".dis"]),
	    Fs = [{Mod,Func1,Arity} || {Func1,Arity} <- Fs0, Func1 == Func],
	    dff(Name, Fs);
	{'EXIT',Reason} ->
	    {undef,Mod}
    end.

df(Mod, Func, Arity) when atom(Mod), atom(Func) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod,"_",Func,"_",Arity,".dis"]),
	    Fs = [{Mod,Func1,Arity1} || {Func1,Arity1} <- Fs0,
					Func1 == Func, Arity1 == Arity],
	    dff(Name, Fs);
	{'EXIT',Reason} ->
	    {undef,Mod}
    end.

dff(File, Fs) when pid(File), list(Fs) ->
    lists:foreach(fun(Mfa) -> disassemble_function(File, Mfa),
			      io:nl(File) end, Fs);
dff(Name, Fs) when list(Name) ->
    case file:open(Name, [write]) of
	{ok,F} ->
	    dff(F, Fs);
	{error,Reason} ->
	    {error,{badopen,Reason}}
    end.

disassemble_function(File, {M,F,A}=MFA) ->
    cont_dis(File, erts_debug:disassemble(MFA), MFA).

cont_dis(File, false, MFA) ->
    ok;
cont_dis(File, {Addr,Str,MFA}, MFA) ->
    io:put_chars(File, binary_to_list(Str)),
    cont_dis(File, erts_debug:disassemble(Addr), MFA);
cont_dis(File, {Addr,Str,Other}, MFA) ->
    ok.

