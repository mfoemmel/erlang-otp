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
%% File: corba_boa.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the CORBA::BOA interface
%%
%% Creation date: 970708
%%
%%-----------------------------------------------------------------
-module(corba_boa).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([%create/3,
	 dispose/1,
	 get_id/1]).
%	 change_implementation/2,
%	 set_exception/3,
%	 impl_is_ready/1,
%	 deactivate_impl/1,
%	 obj_is_ready/2,
%	 deactivate_obj/1,
%	 get_principal/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%create(Id, Interface, Implementation) ->
%    corba:create(Implementation#orb_ImplDef.module,
%		 Interface#fullinterfacedescription.id).

dispose(Object) ->
    case binary_to_term(iop_ior:get_privfield(Object)) of
	undefined ->
	    {Location, Key} = iop_ior:get_key(Object),
	    if
		Location == 'internal' ->
		    gen_server:call(orber_objectkeys:get_pid(Key), stop);
		Location == 'internal_registered' -> 	
		    gen_server:call(Key, stop);
		Location == 'external' -> 
		    %% Must be fixed !!!!!!!!
		    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO})
	    end;
	_ ->
	    corba:raise(#'NO_PERMISSION'{completion_status=?COMPLETED_NO})
    end.

get_id(Object) ->
    iop_ior:get_objkey(Object).

%change_implementation(Object, ImplementationDef) ->
%    ok.

%get_principal(Object, Env) ->
%    ok.

%set_exception(Major, Id, Param) ->
%    ok.

%impl_is_ready(ImplementationDef) ->
%    ok.

%deactivate_impl(ImplementationDef) ->
%    ok.

%obj_is_ready(Object, ImplementationDef) ->
%    ok.

%deactivate_obj(Object) ->
%    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
