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
%% File: orber_initial_references.erl
%% 
%% Description:
%%    This file contains the CORBA::InitialReferences interface
%%
%% Creation date: 970827
%%
%%-----------------------------------------------------------------
-module(orber_initial_references).

-behaviour(gen_server).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, shutdown/1, init/1,
	 terminate/2, handle_call/3, code_change/3, 
	 get/2, list/1, add/3, remove/2, typeID/0]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_cast/2, handle_info/2]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Env) ->
    gen_server:start_link({local, 'orber_init'}, ?MODULE, Env, []).

shutdown(EO_this) ->
    gen_server:call(EO_this, stop).

%%-----------------------------------------------------------------
%% InitialReferences Interface 
%%-----------------------------------------------------------------
get(EO_this, Id) ->
    corba:call(EO_this, 'get', [Id], {{'tk_objref', 12, "object"},
				      [{"id", {'tk_string', 0}}],
				      []}).

list(EO_this) ->
    corba:call(EO_this, 'list', [], {{'tk_sequence',{"id", {'tk_string', 0}, 0}},
				     [], []}).

add(EO_this, Id, ObjRef) ->
    corba:call(EO_this, 'add', [Id, ObjRef], {'tk_boolean',
				      [{"id", {'tk_string', 0}}, {'tk_objref', 12, "object"}],
				      []}).

remove(EO_this, Id) ->
    corba:call(EO_this, 'remove', [Id], {'tk_boolean',
				      [{"id", {'tk_string', 0}}],
				      []}).

typeID() ->
    "IDL:Orber/InitialReferences:1.0".

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
init([]) ->
    TableName = ets:new('InitialReferences', []), 
    NSObjKey = 'CosNaming_NamingContextExt':oe_create([], [{pseudo, true}]),
    ets:insert(TableName, {"NameService", NSObjKey}),
    ErlIfr = 'OrberApp_IFR':oe_create([], [{pseudo, true}]),
    ets:insert(TableName, {"OrberIFR", ErlIfr}),
    {ok, TableName}.

terminate(Reason, TableName) ->
    ets:delete(TableName),
    ok.


%%-----------------------------------------------------------------
%% Handle incomming calls 
handle_call({EO_this, 'get', [Id]}, From, TableName) ->
    ORef = case ets:lookup(TableName, Id) of
	       [{_, ObjRef} ] ->
		   ObjRef;
	       _ ->
		   corba:create_nil_objref()
	   end,
    {'reply', ORef, TableName};
handle_call({EO_this, 'list', []}, From, TableName) ->
    ObjIdList = get_all_objectids(ets:tab2list(TableName), []),
    %% We do not want OrberIFR to exported, remove it.
    {'reply', lists:delete("OrberIFR", ObjIdList), TableName};

handle_call({EO_this, 'add', [Id, ObjectRef]}, From, TableName) ->
    B = case ets:lookup(TableName, Id) of
	    [{_, O} ] ->
		false;
	    _ ->
		ets:insert(TableName, {Id, ObjectRef})
	end,
    {'reply', B, TableName};

handle_call({EO_this, 'remove', [Id]}, From, TableName) ->
    B = case ets:lookup(TableName, Id) of
	    [{_, O}] ->
		ets:delete(TableName, Id);
	    _ ->
		false
	end,
    {'reply', B, TableName};
handle_call({EO_THIS, oe_get_interface, []},
            EO_From, EO_State) ->
    {'reply', [{"get", {{'tk_objref', 12, "object"},
			[{'tk_string', 0}],
			[]}},
	       {"list", {{'tk_sequence',{'tk_string', 0}, 0},
			 [],
			 []}},
	       {"add", {'tk_boolean',
			[{'tk_string', 0}, {'tk_objref', 12, "object"}],
			[]}}
	      ], EO_State};
handle_call('stop', From, State) ->
    {'stop', normal, 'ok', State};
handle_call(Req, From,State) ->
    {'reply', {'ok', 'nil', 'nil'}, State}.


%%-----------------------------------------------------------------
%% Standard gen_server cast handle
%%-----------------------------------------------------------------
handle_cast(_, State) ->
    {noreply,  State}.


%%-----------------------------------------------------------------
%% Standard gen_server handles
%%-----------------------------------------------------------------
handle_info(_, State) ->
    {noreply,  State}.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%------------------------------------------------------------
%% get_all_objectids/2
%%------------------------------------------------------------
get_all_objectids([], Acc) ->
    Acc;
get_all_objectids([{Key, _} | Rest], Acc) ->
    get_all_objectids(Rest, [ Key | Acc]).

