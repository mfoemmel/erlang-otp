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
%% File: corba_object.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the CORBA::Object interface
%%
%% Creation date: 970709
%%
%%-----------------------------------------------------------------
-module(corba_object).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% Standard interface CORBA::Object
%%-----------------------------------------------------------------
-export([%get_implementation/1, 
	 get_interface/1,
	 get_policy/2,
	 is_nil/1, 
%	 duplicate/1, 
%	 release/1,
	 is_a/2,
	 is_remote/1,
	 non_existent/1,
	 is_equivalent/2,
	 hash/2,
	 create_request/6]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([]).

%%%----------------------------------------------------------------------
%%% Local defines
%% Prefix allowed.
-define(IFRDEF_22, "IDL:omg.org/").
-define(IFRDEF_20, "IDL:").

%%------------------------------------------------------------
%% Implementation of standard interface
%%------------------------------------------------------------
%%get_implementation(Obj) ->
%%    exit(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

get_interface(Obj) ->
    TypeId = iop_ior:get_typeID(Obj),
    Rep = orber_ifr:find_repository(),
    case orber_ifr_repository:lookup_id(Rep, TypeId) of
	%% If all we get is an empty list there are no such
	%% object registered in the IFR.
	[] ->
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
	Int ->
	    orber_ifr_interfacedef:describe_interface(Int)
    end.


is_nil(Object) when record(Object, 'IOP_IOR') ->
    iop_ior:check_nil(Object);
is_nil({I,T,K,P,O,F}) ->
    iop_ior:check_nil({I,T,K,P,O,F});
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
is_nil({I,T,K,P}) ->
    iop_ior:check_nil({I,T,K,P});
is_nil(_) ->
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

%%duplicate(Obj) ->
%%    Obj.

%%release(Obj) ->
%%    ok.

get_policy(Obj, PolicyType) when integer(PolicyType) ->
    case catch iop_ior:get_key(Obj) of
	{'external', Key} ->
	    orber_iiop:request(Key, get_policy, [PolicyType], 
			       {orber_policy_server:get_tc(),
				[orber_tc:unsigned_long()],[]},
			       true);
	{'internal', _} ->
	    orber_policy_server:get_policy(Obj, PolicyType);
	{'internal_registered', _} ->
	    orber_policy_server:get_policy(Obj, PolicyType);
	_ ->
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_policy(_, _) ->
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).
    


is_a(Obj, Logical_type_id) ->
    case catch iop_ior:get_key(Obj) of
	{'external', Key} ->
	    orber_iiop:request(Key, '_is_a', [Logical_type_id], 
			       {orber_tc:boolean(),[orber_tc:string(0)],[]},
			       true);
	{_Local, _Key} ->
	    TypeId = iop_ior:get_typeID(Obj),
	    %%To be backward compatible (2.2->2.0) we must recognize two 
	    %% IFR id types:
	    %%"IDL:omg.org/Name:x.x" and "IDL:Name:x.x"
	    Id2 = case lists:prefix(?IFRDEF_22, Logical_type_id) of
		      true ->
			  ?IFRDEF_20++lists:nthtail(length(?IFRDEF_22),
						    Logical_type_id);
		      _ ->
			  ?IFRDEF_22++lists:nthtail(length(?IFRDEF_20), 
						    Logical_type_id)
		  end,
	    if
		TypeId == Logical_type_id ->
		    true;
		TypeId == Id2 ->
		    true;
		true ->
		    Rep = orber_ifr:find_repository(),
		    case orber_ifr_repository:lookup_id(Rep, TypeId) of
			%% If all we get is an empty list there are no such
			%% object registered in the IFR.
			[] ->
			    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
			Int ->
			    orber_ifr_interfacedef:is_a(Int, Logical_type_id)
		    end
	    end;
	_ ->
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end.

non_existent(Obj) ->
    {Location, Key} = iop_ior:get_key(Obj),
    if
	Location == 'internal' ->
	    case catch orber_objectkeys:get_pid(Key) of
		{'EXCEPTION', E} when record(E,'OBJECT_NOT_EXIST') ->
		    true;
		{'EXCEPTION', X} ->
		    corba:raise(X);
		{'EXIT', R} ->
		    exit(R);
		_ ->
		    false
	    end;
	Location == 'internal_registered' ->
	    case whereis(Key) of
		undefined ->
		    true;
		P ->
		    false
	    end;
	Location == 'external' ->
	    orber_iiop:request(Key, '_not_existent', [], 
			       {orber_tc:boolean(), [],[]}, 'true');
	true -> 	
	    false
    end.

is_remote(Obj) ->
    case catch iop_ior:get_key(Obj) of
	{'external', _} ->
	    true;
	_ ->
	    false
    end.


is_equivalent({I,T,K,P,O,F}, {I2,T2,K2,P2,O2,F2}) ->
    case {I,T,K,P,O,F} of
	{I2,T2,K2,P2,_,_} ->
	    true;
	_ ->
	    false
    end;
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
is_equivalent(Obj, Other_object) ->
    case Obj of
	Other_object ->
	    true;
	_ ->
	    false
    end.

hash(Obj, Maximum) ->
    erlang:hash(iop_ior:get_key(Obj), Maximum).


create_request(Obj, Ctx, Op, ArgList, NamedValueResult, ReqFlags) ->
    {ok, NamedValueResult, []}.
