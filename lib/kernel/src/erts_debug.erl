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
    {Sum,_} = size(Term, gb_trees:empty(), 0),
    Sum.

size([H|T]=Term, Seen0, Sum0) ->
    case remember_term(Term, Seen0) of
	seen -> {Sum0,Seen0};
	Seen1 ->
	    {Sum,Seen} = size(H, Seen1, Sum0+2),
	    size(T, Seen, Sum)
    end;
size(Tuple, Seen0, Sum0) when is_tuple(Tuple) ->
    case remember_term(Tuple, Seen0) of
	seen -> {Sum0,Seen0};
	Seen ->
	    Sum = Sum0 + 1 + erlang:size(Tuple),
	    tuple_size(1, erlang:size(Tuple), Tuple, Seen, Sum)
    end;
size(Term, Seen0, Sum) ->
    case erts_debug:flat_size(Term) of
	0 -> {Sum,Seen0};
	Sz ->
	    case remember_term(Term, Seen0) of
		seen -> {Sum,Seen0};
		Seen -> {Sum+Sz,Seen}
	    end
    end.

tuple_size(I, Sz, _, Seen, Sum) when I > Sz ->
    {Sum,Seen};
tuple_size(I, Sz, Tuple, Seen0, Sum0) ->
    {Sum,Seen} = size(element(I, Tuple), Seen0, Sum0),
    tuple_size(I+1, Sz, Tuple, Seen, Sum).
	    
remember_term(Term, Seen) ->
    case gb_trees:lookup(Term, Seen) of
	none -> gb_trees:insert(Term, [Term], Seen);
	{value,Terms} ->
	    case is_term_seen(Term, Terms) of
		false -> gb_trees:update(Term, [Term|Terms], Seen);
		true -> seen
	    end
    end.

is_term_seen(Term, [H|T]) ->
    case erts_debug:same(Term, H) of
	true -> true;
	false -> is_term_seen(Term, T)
    end;
is_term_seen(_, []) -> false.

%% df(Mod)               -- Disassemble Mod to file Mod.dis.
%% df(Mod, Func)         -- Disassemble Mod:Func/Any to file Mod_Func.dis.
%% df(Mod, Func, Arity)  -- Disassemble Mod:Func/Arity to file Mod_Func_Arity.dis.

df(Mod) when atom(Mod) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod, ".dis"]),
	    Fs = [{Mod,Func,Arity} || {Func,Arity} <- Fs0],
	    dff(Name, Fs);
	{'EXIT',_} ->
	    {undef,Mod}
    end.

df(Mod, Func) when atom(Mod), atom(Func) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod,"_",Func,".dis"]),
	    Fs = [{Mod,Func1,Arity} || {Func1,Arity} <- Fs0, Func1 == Func],
	    dff(Name, Fs);
	{'EXIT',_} ->
	    {undef,Mod}
    end.

df(Mod, Func, Arity) when atom(Mod), atom(Func) ->
    case catch Mod:module_info(functions) of
	Fs0 when list(Fs0) ->
	    Name = lists:concat([Mod,"_",Func,"_",Arity,".dis"]),
	    Fs = [{Mod,Func1,Arity1} || {Func1,Arity1} <- Fs0,
					Func1 == Func, Arity1 == Arity],
	    dff(Name, Fs);
	{'EXIT',_} ->
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

disassemble_function(File, {_,_,_}=MFA) ->
    cont_dis(File, erts_debug:disassemble(MFA), MFA).

cont_dis(_, false, _) -> ok;
cont_dis(File, {Addr,Str,MFA}, MFA) ->
    io:put_chars(File, binary_to_list(Str)),
    cont_dis(File, erts_debug:disassemble(Addr), MFA);
cont_dis(_, {_,_,_}, _) -> ok.
