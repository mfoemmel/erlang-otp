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
%%-----------------------------------------------------------------
%% File: CosNaming_NamingContext_impl.erl
%% Author: Lars Thorsen
%% 
%% Creation date: 970902
%% Modified:
%%
%%-----------------------------------------------------------------
-module('CosNaming_NamingContext_impl').

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("CosNaming.hrl").
-include("CosNaming_NamingContext.hrl").
-include("orber_cosnaming.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, install/2]).
-export([bind/4, rebind/4, bind_context/4, rebind_context/4,
	 resolve/3, unbind/3, new_context/2, bind_new_context/3,
	 list/3, destroy/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([dump/0]).


%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Debugging function
%%-----------------------------------------------------------------
dump() ->
    case catch mnesia:dirty_first('orber_CosNaming') of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_loop(PreviousKey) ->
    case catch mnesia:dirty_next('orber_CosNaming', PreviousKey) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	'$end_of_table' ->
	    ok;
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_print(Key) ->
    case catch mnesia:dirty_read({'orber_CosNaming', Key}) of
       {'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	[X] ->
	    io:format("name_context: ~p\n-----------------------------\n"
		      " nameindex structure\n-----------------------------\n~p\n\n",
		      [binary_to_term(X#orber_CosNaming.name_context),
		       X#orber_CosNaming.nameindex]);
	_ ->
	    ok
    end.
       

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
init(Env) ->
    %%    ?PRINTDEBUG("name server init"),
    case mnesia:wait_for_tables(['orber_CosNaming'], infinity) of
	ok ->
	    {ok, []};
	StopReason ->
	    {stop, StopReason}
    end.

terminate(Reason, State) ->
    ok.

install(Timeout, Options) ->
    %% Fetch a list of the defined tables to see if 'CosNaming' is defined.
    AllTabs = mnesia:system_info(tables),
    DB_tables_created =
	case lists:member('orber_CosNaming', AllTabs) of
	    true ->
		case lists:member({local_content, true},
				  Options) of
		    true->
			mnesia:add_table_copy('orber_CosNaming',
					      node(),
					      ram_copies);
		    _->
			mnesia:create_table('orber_CosNaming',[{attributes,
								record_info(fields,
									    'orber_CosNaming')}
							       |Options])
		end;
	    _ ->
		mnesia:create_table('orber_CosNaming',[{attributes,
							record_info(fields,
								    'orber_CosNaming')}
						       |Options])
	end,
    Wait = mnesia:wait_for_tables(['orber_CosNaming'], Timeout),
    %% Check if any error has occured yet. If there are errors, return them.

    if
	DB_tables_created == {atomic, ok},
	Wait == ok ->
	    _F = ?write_function(#orber_CosNaming{name_context=
						  term_to_binary('undefined'),
						  nameindex=[]}),
	    write_result(mnesia:transaction(_F));
	true -> 
	    {error, [DB_tables_created, Wait]}
    end.

%%-----------------------------------------------------------------
%% Interface CosNaming::NamingContext
%%-----------------------------------------------------------------
bind(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								   cxt=OE_THIS});
	X ->
	    case lists:keysearch(N, 1, X) of
		{value, _} ->
		    corba:raise(#'CosNaming_NamingContext_AlreadyBound'{});
		false ->
		    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
							  nameindex=[{N, nobject,
								      Obj} | X]}),
		    {write_result(mnesia:transaction(_F)), OE_State}
	    end
    end;
bind(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} ->
		    bind(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.

rebind(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								       cxt=OE_THIS});
	X ->
	    KList = case lists:keysearch(N, 1, X) of
			{value, {N, _, V}} ->
			    lists:keyreplace(N, 1, X, {N, nobject, Obj});
			false ->
			    [{N, nobject, Obj} | X]
		    end,
	    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
						      nameindex=KList}),
	    {write_result(mnesia:transaction(_F)), OE_State}
    end;
rebind(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
						       cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} ->
		    rebind(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.

bind_context(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								   cxt=OE_THIS});
	X ->
	    case lists:keysearch(N, 1, X) of
		{value, _} ->
		    corba:raise(#'CosNaming_NamingContext_AlreadyBound'{});
		false ->
		    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
							  nameindex=[{N, ncontext,
								      Obj} | X]}),
		    {write_result(mnesia:transaction(_F)), OE_State}
	    end
    end;
bind_context(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} ->
		    bind_context(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.

rebind_context(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								     cxt=OE_THIS});
	X ->
	    KList = case lists:keysearch(N, 1, X) of
			{value, {N, _, V}} ->
			    lists:keyreplace(N, 1, X, {N, ncontext, Obj});
			false ->
			    [{N, ncontext, Obj} | X]
		    end,
	    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
						  nameindex=KList}),
	    {write_result(mnesia:transaction(_F)), OE_State}
    end;
rebind_context(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H,ncontext, NC}} ->
		    rebind_context(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.

resolve(OE_THIS, OE_State, [N]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								       cxt=OE_THIS});
	X ->
	    case lists:keysearch(N, 1, X) of
		{value, {N, _, Value}} ->
		    {Value, OE_State};
		false ->
		    corba:raise(#'CosNaming_NamingContext_NotFound'
				{rest_of_name=[N], why='not_object'})
	    end
    end;
resolve(OE_THIS, OE_State, [H|T]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} ->
		    resolve(NC, OE_State, T);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.

unbind(OE_THIS, OE_State, [N]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								 cxt=OE_THIS});
	X ->
	    KList = lists:keydelete(N, 1, X),
	    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
						  nameindex=KList}),
	    {write_result(mnesia:transaction(_F)), OE_State}
    end;
unbind(OE_THIS, OE_State, [H|T]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} ->
		    unbind(NC, OE_State, T);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end.


new_context(OE_THIS, OE_State) ->
    SubobjKey = term_to_binary({now(), node()}),
    NewKey = corba:create_subobject_key(OE_THIS, SubobjKey),
    %% Create a record in the table and set the key to a newly
    %% generated objectkey.
    _F = ?write_function(#orber_CosNaming{name_context=SubobjKey,
					  nameindex=[]}),
    write_result(mnesia:transaction(_F)),
    {NewKey, OE_State}.

bind_new_context(OE_THIS, OE_State, N) ->
    %%?PRINTDEBUG("bind_new_context"),
    %% Create a name context
    {NewKey, NewState} = new_context(OE_THIS, OE_State),
    %% Bind the created name context to a name
    case catch bind_context(OE_THIS, NewState, N, NewKey) of
	{'EXCEPTION', E} ->
	    destroy(NewKey, NewState),
	    corba:raise(E);
	{ok, NewState1} ->
	    {NewKey, NewState1}
    end.


list(OE_THIS, OE_State, HowMany) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
	X ->
	    List = lists:sublist(X, HowMany),
	    BList = lists:map(
		      fun({N, T, O}) ->
			      #'CosNaming_Binding'{binding_name=[N],
						   binding_type=T}
		      end, List),
	    BIterator = 'CosNaming_BindingIterator':oe_create({SubobjKey, HowMany}),
	    {{ok, BList, BIterator}, OE_State}
    end.


destroy(OE_THIS, OE_State) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    case term_to_binary(SubobjKey) of
	'undefined' ->
	    corba:raise(#'NO_PERMISSION'{completion_status=?COMPLETED_NO});
	_ ->
	    _RF = ?read_function({orber_CosNaming, SubobjKey}),
	    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
		error ->
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
		[] ->
		    _DF = ?delete_function({orber_CosNaming,
					    SubobjKey}),
		    {write_result(mnesia:transaction(_DF)), OE_State};
		_ -> 
		    corba:raise(#'CosNaming_NamingContext_NotEmpty'{})
	    end
    end.


%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

%% Check a write transaction
write_result({atomic,ok}) -> ok;
write_result(Foo) ->
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

check_name({Id, Kind}) when length(Id) == 0 ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
check_name(X) ->
    ok.
 
