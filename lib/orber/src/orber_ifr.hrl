%%--------------------------------------------------------------------
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
%%%----------------------------------------------------------------------
%%% File    : orber_ifr.hrl
%%% Purpose : Macros for the Interface Repository
%%% Created : 14 May 1997
%%%----------------------------------------------------------------------

% "Type" checking
-define(type_checking, true).

-ifdef(type_checking).
-define(tcheck(Type, Thing), when Type == Thing).
-else.
-define(tcheck(Type, Thing), ).
-endif.

% Existence checking
-define(existence_checking, true).
-ifdef(existence_checking).
-define(exists_check(Thing,Thing_ID),
	orber_ifr_utils:existence_check(Thing,Thing_ID)).
-else.
-define(exists_check(Thing,Thing_ID), ok).
-endif.

% Raise an IFR exception
-define(ifr_exception(Message,Errval),
	?debug_print(Message, Errval),
	corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})).

% Print an error message and exit
-define(ifr_error(Str, Things),
	exit(list_to_atom(lists:flatten(io_lib:format(
					  "[Module: ~w, line: ~w] ~s ~p",
					  [?MODULE, ?LINE, Str, Things]))))).

%Print a warning message
-define(ifr_warning(Str, Things), io:format("[Mod: ~w, line: ~w] ~s ~p~n",
					    [?MODULE, ?LINE, Str, Things])).

% Debugging output
%-define(debug_output, true).

-ifdef(debug_output).
-define(debug_print(Str, Things), io:format("[Mod: ~w, line: ~w] ~s ~p~n",
					     [?MODULE, ?LINE, Str, Things])).
-else.
-define(debug_print(Str, Things), ok).
-endif.

% Uniqueness
%-define(debug_ifr, true).

-ifdef(debug_ifr).
-define(unique_code,case get(unique) of
			undefined ->
			    put(unique,1),
			    1;
			Val ->
			    put(unique,Val+1),
			    Val+1
		    end).
-else.
-define(unique_code,term_to_binary({node(), now()})).
-endif.

% Transaction checking
%-define(dirty_context, true).

-ifdef(dirty_context).
-define(read_check_1(Q_res), [Q_res]).
-else.
-define(read_check_1(Q_res), {atomic,[Q_res]}).
-endif.

-ifdef(dirty_context).
-define(read_check_2(Q_res), []).
-else.
-define(read_check_2(Q_res), {atomic,[]}).
-endif.

-ifdef(dirty_context).
-define(write_check(W_res), ok).
-else.
-define(write_check(W_res), {atomic,ok}).
-endif.

% This macro returns a query function suitable for evaluation in a transaction
%-define(query_function(Table,Id,Attribute),
%	fun() ->
%		Q = query [X.Attribute || X <- table(Table),
%					  X.ir_Internal_ID = Id]
%		    end,
%		mnemosyne:eval(Q)
%	end).

% This macro returns a read function suitable for evaluation in a transaction
-define(read_function(Oid),
	fun() ->
		mnesia:read(Oid)
	end).

% This macro returns a write function suitable for evaluation in a transaction
-define(write_function(Object),
	fun() ->
		mnesia:write(Object)
	end).

% This macro returns a delete function suitable for evaluation in a transaction
-define(delete_function(Table,Internal_ID),
	fun() ->
		mnesia:delete({Table,Internal_ID})
	end).

% Return a write list function suitable for evalutation in a transaction
-define(write_list_function(Object_list),
	fun() ->
		mnesia_write_list(Object_list)
	end).

% make an inherited object reference
-define(inherit_obj(Inherited_obj,Objtypename),
	{Objtypename,Inherited_obj#Objtypename.ir_Internal_ID}).

% update an object (i.e. make a new object with a new value in one field
-define(update_obj(Obj,Val,Rec_type,Field_name),
	Obj#Rec_type{Field_name = Val}).
