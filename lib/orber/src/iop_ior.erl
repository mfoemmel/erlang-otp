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
%% File: iop_ior.erl
%% Description:
%%    This file contains the IOP::IOR handling
%%
%% Creation date: 970327
%%
%%-----------------------------------------------------------------
-module(iop_ior).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([code/4, decode/4, string_code/1, string_decode/1, create/5, create/6,
	 get_key/1, get_key/2, get_typeID/1, get_objkey/1, check_nil/1,
	 get_privfield/1, set_privfield/2, 
	 get_orbfield/1, set_orbfield/2, 
	 get_flagfield/1, set_flagfield/2, get_version/1,
	 create_external/5, create_external/6, print/1, print/2,
	 get_alt_addr/1, add_component/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 6).

get_version({normal, Host, IIOP_port, OK, #'IIOP_Version'{major=Ma, minor=Mi}}) ->
    {Ma, Mi};
get_version({ssl, Host, SSLData, OK, #'IIOP_Version'{major=Ma, minor=Mi}}) ->
    {Ma, Mi};
get_version(_) ->
    orber:giop_version().

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: create/5/6
%%-----------------------------------------------------------------
%% There are a few restrictions if a certain IIOP-version may contain certain components
%% and contexts The ones we currently, and the ones we perhaps will, support is:
%% 
%%    Feature                          1.0  1.1  1.2
%% TransactionService Service Context  yes  yes  yes
%% CodeSets Service Context                 yes  yes
%% Object by Value Service Context               yes
%% Bi-Directional IIOP Service Context           yes
%% IOR components in IIOP profile           yes  yes
%% TAG_ORB_TYPE                             yes  yes
%% TAG_CODE_SETS                            yes  yes
%% TAG_ALTERNATE_IIOP_ADDRESS                    yes
%% TAG_SSL_SEC_TRANS                        yes  yes
%% Extended IDL data types                  yes  yes
%% Bi-Directional GIOP Features                  yes
%% Value types and Abstract Interfaces           yes
%%-----------------------------------------------------------------
create(Version, TypeID, Host, IIOP_port, Objkey) ->
    create(Version, TypeID, Host, IIOP_port, Objkey, []).
create({1, 0}, TypeID, Host, IIOP_port, Objkey, MC) ->
    V=#'IIOP_Version'{major=1,
		      minor=0},
    PB=#'IIOP_ProfileBody_1_0'{iiop_version=V,
			   host=Host,
			   port=IIOP_port,
			   object_key=Objkey},
    #'IOP_IOR'{type_id=TypeID, profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							      profile_data=PB}]};
create({1, 1}, TypeID, Host, IIOP_port, Objkey, MC) ->
    V=#'IIOP_Version'{major=1,
		      minor=1},
    Components = case orber:iiop_ssl_port() of
		       -1 ->
			 MC;
		       SSLPort ->
			 [#'IOP_TaggedComponent'
			  {tag=?TAG_SSL_SEC_TRANS, 
			   component_data=#'SSLIOP_SSL'{target_supports = 2, 
							target_requires = 2, 
							port = SSLPort}}|MC]
		   end,
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create({1, 2}, TypeID, Host, IIOP_port, Objkey, MC) ->
    V=#'IIOP_Version'{major=1,
		      minor=2},
    Components = case orber:iiop_ssl_port() of
		       -1 ->
			 MC;
		       SSLPort ->
			 [#'IOP_TaggedComponent'
			  {tag=?TAG_SSL_SEC_TRANS, 
			   component_data=#'SSLIOP_SSL'{target_supports = 2, 
							target_requires = 2, 
							port = SSLPort}}|MC]
		   end,
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create(Version, TypeID, Host, IIOP_port, Objkey, MC) ->
    orber:debug_level_print("[~p] iop_ior:create(~p, ~p, ~p, ~p, ~p, ~p); unsupported IIOP-version.", 
			    [?LINE, Version, TypeID, Host, IIOP_port, Objkey, MC], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

   
%%-----------------------------------------------------------------
%% Func: create_external/5/6
%%-----------------------------------------------------------------
create_external(Version, TypeID, Host, IIOP_port, Objkey) ->
    create_external(Version, TypeID, Host, IIOP_port, Objkey, []).
create_external({1, 0}, TypeID, Host, IIOP_port, Objkey, MC) ->
    V=#'IIOP_Version'{major=1,
		      minor=0},
    PB=#'IIOP_ProfileBody_1_0'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey},
    #'IOP_IOR'{type_id=TypeID, profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							      profile_data=PB}]};
create_external({1, 1}, TypeID, Host, IIOP_port, Objkey, Components) ->
    V=#'IIOP_Version'{major=1,
		      minor=1},
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create_external({1, 2}, TypeID, Host, IIOP_port, Objkey, Components) ->
    V=#'IIOP_Version'{major=1,
		      minor=2},
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			       host=Host,
			       port=IIOP_port,
			       object_key=Objkey,
			       components=Components},
    #'IOP_IOR'{type_id=TypeID, 
	       profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=PB}]};
create_external(Version, TypeID, Host, IIOP_port, Objkey, MC) ->
    orber:dbg("[~p] iop_ior:create(~p, ~p, ~p, ~p, ~p, ~p); unsupported IIOP-version.", 
			    [?LINE, Version, TypeID, Host, IIOP_port, Objkey, MC], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).
   
%%-----------------------------------------------------------------
%% Func: get_key/1
%%-----------------------------------------------------------------
get_key(#'IOP_IOR'{profiles=P})  ->
    get_key_1(P, false, 0, undefined);
get_key({Module, Type, Key, _UserDef, OrberDef, Flags}) ->
    if
	binary(Key) ->
	    {'internal', Key, OrberDef, Flags, Module};
	Type == pseudo ->
	    {'internal_registered', {pseudo, Key}, OrberDef, Flags, Module};
	atom(Key) ->
	    {'internal_registered', Key, OrberDef, Flags, Module}
    end.

get_key(#'IOP_IOR'{profiles=P}, Exclude)  ->
    get_key_1(P, true, 0, Exclude).

get_key_1([], false, _, _)  ->
    orber:dbg("[~p] iop_ior:get_key_1([]); bad object reference, profile not found.", 
			    [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_key_1([], true, _, _)  ->
    undefined;
get_key_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} = TP | P], 
	  Retry, Counter, Exclude) ->
    [_, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Module, Type, Key, _UserDef, OrberDef, Flags} ->
	    if
		binary(Key) ->
		    {'internal', Key, OrberDef, Flags, Module};
		Type == pseudo ->
		    {'internal_registered', {pseudo, Key}, OrberDef, Flags, Module};
		atom(Key) ->
		    {'internal_registered', Key, OrberDef, Flags, Module}
	    end;
	OK when Version#'IIOP_Version'.minor > 0, Exclude == undefined ->
	    {'external', {Host, IIOP_port, OK, Counter, TP, 
			  check_component_data(Rest, Version)}};
	OK when Version#'IIOP_Version'.minor > 0 ->
	    case lists:member(Counter, Exclude) of
		true ->
		    get_key_1(P, Retry, Counter+1, Exclude);
		false ->
		    {'external', {Host, IIOP_port, OK, Counter, TP, 
				  check_component_data(Rest, Version)}}
	    end;
	OK when Exclude == undefined ->
	    %% This case is "necessary" if an ORB adds several IIOP-profiles since,
	    %% for example, wchar isn't supported for 1.0.
	    case get_key_1(P, true, Counter+1, Exclude) of
		undefined ->
		    %% We now it's IIOP-1.0 and it doesn't contain any
		    %% components. Hence, no need to check for it.
		    {'external', {Host, IIOP_port, OK, Counter, TP, 
				  check_component_data(Rest, Version)}};
		LaterVersion ->
		    LaterVersion
	    end;
	OK ->
	    case lists:member(Counter, Exclude) of
		true ->
		    get_key_1(P, Retry, Counter+1, Exclude);
		false ->
		    %% This case is "necessary" if an ORB adds several IIOP-profiles since,
		    %% for example, wchar isn't supported for 1.0.
		    case get_key_1(P, true, Counter+1, Exclude) of
			undefined ->
			    {'external', {Host, IIOP_port, OK, Counter, TP, 
					  check_component_data(Rest, Version)}};
			LaterVersion ->
			    LaterVersion
		    end
	    end
    end;
get_key_1([_ | P], Retry, Counter, Exclude)  ->
    get_key_1(P, Retry, Counter+1, Exclude).

check_component_data([], Version) ->
    #host_data{version = {Version#'IIOP_Version'.major,
			  Version#'IIOP_Version'.minor}};
check_component_data([CompData], Version) ->
    check_component_data_2(CompData, #host_data{version = {Version#'IIOP_Version'.major,
							   Version#'IIOP_Version'.minor}}).

check_component_data_2([], HostData) ->
    HostData;
check_component_data_2([#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					       component_data=SSLStruct}|Rest], 
		       HostData) when record(SSLStruct, 'SSLIOP_SSL') ->
    check_component_data_2(Rest, HostData#host_data{protocol = ssl,
						    ssl_data = SSLStruct});
check_component_data_2([#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					       component_data=#'CONV_FRAME_CodeSetComponentInfo'
					       {'ForCharData' = Char,
						'ForWcharData' = Wchar}}|Rest], 
		       HostData) ->

    CharData = check_char_codeset(Char),
    WcharData = check_wchar_codeset(Wchar),
    check_component_data_2(Rest, HostData#host_data{charset = CharData,
						    wcharset = WcharData});
check_component_data_2([_ | Rest], HostData) ->
    check_component_data_2(Rest, HostData).

check_char_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO8859_1_ID}) ->
    ?ISO8859_1_ID;
check_char_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO646_IRV_ID}) ->
    ?ISO646_IRV_ID;
check_char_codeset(#'CONV_FRAME_CodeSetComponent'{conversion_code_sets=Converters}) ->
    %% Since the list of Converters usually is very short (0 or 1 element) we
    %% can use lists:member.
    case lists:member(?ISO8859_1_ID, Converters) of
	true ->
	    ?ISO8859_1_ID;
	false ->
	    %% Since we are 100% sure strings will be (e.g. IFR-ids) used we
	    %% can raise an exception at this point.
	    orber:dbg("[~p] iop_ior:check_char_codeset(~p); 
Orber cannot communicate with this ORB. 
It doesn't support a Char CodeSet known to Orber.", [?LINE, Converters], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status = ?COMPLETED_NO})
    end.

check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?UTF_16_ID}) ->
    ?UTF_16_ID;
check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{native_code_set=?UCS_2_ID}) ->
    ?UCS_2_ID;
check_wchar_codeset(#'CONV_FRAME_CodeSetComponent'{conversion_code_sets=Converters}) ->
    case lists:member(?UTF_16_ID, Converters) of
	true ->
	    ?UTF_16_ID;
	false ->
	    %% We should not raise an exception here since we do not know if
	    %% wchar/wstring is used.
	    ?UTF_16_ID
%	    ?UNSUPPORTED_WCHAR
    end.


%%-----------------------------------------------------------------
%% Func: add_component/2
%%-----------------------------------------------------------------
add_component(Objref, Component) when record(Objref, 'IOP_IOR') ->
    add_component_ior(Objref, Component);
add_component(Objref, Component) ->
    add_component_local(Objref, Component, orber:giop_version()).

add_component_local(_, Component, {1,0}) ->
    orber:dbg("[~p] iop_ior:add_component(~p); 
IIOP-1.0 objects cannot contain any components.", [?LINE, Component], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
add_component_local(_, #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS} 
		    = Component, {1,1}) ->
    orber:dbg("[~p] iop_ior:add_component(~p); 
IIOP-1.1 objects may not contain ALTERNATE_IIOP_ADDRESS components.", 
	      [?LINE, Component], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
add_component_local({Id, Type, Key, UserDef, OrberDef, Flags}, Component, Version) ->
    CodeSetComp = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS},
    create(Version, Id:typeID(), orber:host(), orber:iiop_port(),
	   {Id, Type, Key, UserDef, OrberDef, Flags}, [Component, CodeSetComp]).

add_component_ior(#'IOP_IOR'{profiles=P} = IOR, Component) ->
    case add_component_ior_helper(P, Component, false, []) of
	{false, _} ->
	    orber:dbg("[~p] iop_ior:add_component_ior(~p); 
The IOR do not contain a valid IIOP-version for the supplied component.", 
		      [?LINE, Component], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO});
	{_, NewProfiles} ->
	    IOR#'IOP_IOR'{profiles=NewProfiles}
    end.

add_component_ior_helper([], Component, Status, Acc) ->
    {Status, Acc};
add_component_ior_helper([#'IOP_TaggedProfile'
			  {tag=?TAG_INTERNET_IOP, 
			   profile_data=#'IIOP_ProfileBody_1_1'
			   {iiop_version= #'IIOP_Version'{minor=1}}}|T], 
			 #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS} 
			 = Component, Status, Acc) ->
    %% 'ALTERNATE_IIOP_ADDRESS' may only be added to IIOP-1.2 IOR's.
    add_component_ior_helper(T, Component, Status, Acc);
add_component_ior_helper([#'IOP_TaggedProfile'
			  {tag=?TAG_INTERNET_IOP, 
			   profile_data=#'IIOP_ProfileBody_1_1'
			   {object_key=Objkey,
			    components=Components} = PB} = H|T], Component, Status, Acc) 
  when tuple(Objkey) ->
    %% The objectkey must be a tuple if it's a local object. We cannot(!!) add components
    %% to an external IOR.
    add_component_ior_helper(T, Component, true, 
			     [H#'IOP_TaggedProfile'
			      {profile_data=PB#'IIOP_ProfileBody_1_1'
			       {components = [Component|Components]}}|Acc]);
add_component_ior_helper([_|T], Component, Status, Acc) ->
    add_component_ior_helper(T, Component, Status, Acc).

%%-----------------------------------------------------------------
%% Func: get_alt_addr/1
%%-----------------------------------------------------------------
%% TAG_ALTERNATE_IIOP_ADDRESS may only occur in IIOP-1.2 IOR's.
get_alt_addr(#'IOP_TaggedProfile'
	     {tag=?TAG_INTERNET_IOP, 
	      profile_data=#'IIOP_ProfileBody_1_1'{iiop_version=
						   #'IIOP_Version'{minor=2},
						   components=Components}}) ->
    get_alt_addr_helper(Components, []);
get_alt_addr(_) ->
    [].

get_alt_addr_helper([], Acc) -> Acc;
get_alt_addr_helper([#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					    component_data=#'ALTERNATE_IIOP_ADDRESS'
					    {'HostID'=Host, 'Port'=Port}}|T], Acc) ->
    get_alt_addr_helper(T, [{Host, Port}|Acc]);
get_alt_addr_helper([_|T], Acc) ->
    get_alt_addr_helper(T, Acc).

%%-----------------------------------------------------------------
%% Func: get_typeID/1
%%-----------------------------------------------------------------
get_typeID(#'IOP_IOR'{type_id=TypeID}) ->
    TypeID;
get_typeID({Id, _Type, _Key, _UserDef, _OrberDef, _Flags}) when atom(Id) ->
    Id:typeID();
get_typeID({Id, _Type, _Key, _UserDef, _OrberDef, _Flags}) ->
    binary_to_list(Id).

%%-----------------------------------------------------------------
%% Func: get_objkey/1
%%-----------------------------------------------------------------
get_objkey(#'IOP_IOR'{profiles=P}) ->
    get_objkey_1(P);
get_objkey({Id, Type, Key, UserDef, OrberDef, Flags}) ->
    {Id, Type, Key, UserDef, OrberDef, Flags}.

get_objkey_1([]) ->
    orber:dbg("[~p] iop_ior:get_objkey_1([]); bad object key, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_objkey_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    ObjectKey;
get_objkey_1([_ | P]) ->
    get_objkey_1(P).

%%-----------------------------------------------------------------
%% Func: get_privfield/1
%%-----------------------------------------------------------------
get_privfield(#'IOP_IOR'{profiles=P}) ->
    get_privfield_1(P);
get_privfield({_Id, _Type, _Key, UserDef, _OrberDef, _Flags}) ->
    UserDef.

get_privfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_privfield_1([]); bad object key, profile not found.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_privfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, UserDef, _OrberDef, _Flags} ->
	    UserDef;
	_ ->
	    orber:dbg("[~p] iop_ior:get_privfield_1(~p); bad object key.", 
				    [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_privfield_1([_| P]) ->
    get_privfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_privfield/2
%%-----------------------------------------------------------------
set_privfield(#'IOP_IOR'{type_id=Id, profiles=P}, UserData) ->
    #'IOP_IOR'{type_id=Id, profiles=set_privfield_1(P, UserData)};
set_privfield({Id, Type, Key, _, OrberDef, Flags}, UserData) ->
	    {Id, Type, Key, UserData, OrberDef, Flags}.

set_privfield_1([], UserData) ->
    orber:dbg("[~p] iop_ior:set_privfield_1([]); bad object key, profile not found or external object.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_privfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P], UserData) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, _, OrberDef, Flags} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, UserData, OrberDef, Flags}|
							      Rest])} |
	     set_privfield_1(P, UserData)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_privfield_1(P, UserData)]
    end;
set_privfield_1([PB| P], UserData) ->
    [PB | set_privfield_1(P, UserData)].

%%-----------------------------------------------------------------
%% Func: get_orbfield/1
%%-----------------------------------------------------------------
get_orbfield(#'IOP_IOR'{profiles=P}) ->
    get_orbfield_1(P);
get_orbfield({_Id, _Type, _Key, _UserDef, OrberDef, _Flags}) ->
    OrberDef.

get_orbfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_orbfield_1([]); 
bad object key, profile not found.", [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_orbfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, OrberDef, _Flags} ->
	    OrberDef;
	_ ->
	    orber:dbg("[~p] iop_ior:get_orbfield_1(~p); 
bad object key.", [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_orbfield_1([_| P]) ->
    get_orbfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_orbfield/2
%%-----------------------------------------------------------------
set_orbfield(#'IOP_IOR'{type_id=Id, profiles=P}, OrberDef) ->
    #'IOP_IOR'{type_id=Id, profiles=set_orbfield_1(P, OrberDef)};
set_orbfield({Id, Type, Key, Priv, _, Flags}, OrberDef) ->
	    {Id, Type, Key, Priv, OrberDef, Flags}.

set_orbfield_1([], OrberDef) ->
    orber:dbg("[~p] iop_ior:set_orbfield_1([]); 
bad object key, profile not found or external object.", [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_orbfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P], OrberDef) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, Priv, _, Flags} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, OrberDef, Flags}|
							      Rest])} |
	     set_orbfield_1(P, OrberDef)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_orbfield_1(P, OrberDef)]
    end;
set_orbfield_1([PB| P], OrberDef) ->
    [PB | set_orbfield_1(P, OrberDef)].

%%-----------------------------------------------------------------
%% Func: get_flagfield/1
%%-----------------------------------------------------------------
get_flagfield(#'IOP_IOR'{profiles=P}) ->
    get_flagfield_1(P);
get_flagfield({_Id, _Type, _Key, _UserDef, _OrberDef, Flags}) ->
    Flags.

get_flagfield_1([]) ->
    orber:dbg("[~p] iop_ior:get_flagfield_1([]); bad object key, profile not found.", 
			    [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
get_flagfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, _OrberDef, Flags} ->
	    Flags;
	_ ->
	    orber:dbg("[~p] iop_ior:get_flagfield_1(~p); bad object key.", 
		      [?LINE, ObjectKey], ?DEBUG_LEVEL),
	    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end;
get_flagfield_1([_| P]) ->
    get_flagfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_flagfield/2
%%-----------------------------------------------------------------
set_flagfield(#'IOP_IOR'{type_id=Id, profiles=P}, Flags) ->
    #'IOP_IOR'{type_id=Id, profiles=set_flagfield_1(P, Flags)};
set_flagfield({Id, Type, Key, Priv, OrberDef, _}, Flags) ->
	    {Id, Type, Key, Priv, OrberDef, Flags}.

set_flagfield_1([], Flags) ->
    orber:dbg("[~p] iop_ior:set_flagfield_1([]); bad object key, profile not found or external object.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO});
set_flagfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P], Flags) ->
    [RecName, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{Id, Type, Key, Priv, OrberDef, _} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, OrberDef, Flags}|
							      Rest])} |
	     set_flagfield_1(P, Flags)];
	_ ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | set_flagfield_1(P, Flags)]
    end;
set_flagfield_1([PB| P], Flags) ->
    [PB | set_flagfield_1(P, Flags)].

%%-----------------------------------------------------------------
%% Func: check_nil/1
%%-----------------------------------------------------------------
check_nil(#'IOP_IOR'{type_id="", profiles=[]}) ->
    true;
check_nil({Id, _, _, _, _, _}) when atom(Id) ->
    false;
check_nil({Id, _, _, _, _, _}) ->  
    case binary_to_list(Id) of
	"" ->
	    true; 
	_ ->
	    false
    end;
check_nil(_) ->
    false.



%%----------------------------------------------------------------------
%% Function   : print
%% Arguments  : An object represented as one of the following:
%%               - local (tuple)
%%               - IOR
%%               - stringified IOR
%%               - corbaloc- or corbaname-schema
%%              IoDevice - the same as the io-module defines.
%% Returns    : 
%% Description: Prints the object's components.
%%----------------------------------------------------------------------
print(Object) ->
    print(undefined, Object).
print(IoDevice, #'IOP_IOR'{type_id="", profiles=[]}) ->
    print_it(IoDevice, "================== IOR ====================
NIL Object Reference.
================== END ====================~n");
print(IoDevice, IORStr) when list(IORStr) ->
    IOR = corba:string_to_object(IORStr),
    print_helper(IoDevice, IOR);
print(IoDevice, IOR) when record(IOR, 'IOP_IOR') ->
    print_helper(IoDevice, IOR);
print(IoDevice, {Id, Type, Key, UserDef, OrberDef, Flags}) when atom(Id) ->
    CodeSetComp = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS},
    IOR = create(orber:giop_version(), Id:typeID(), orber:host(), orber:iiop_port(),
		 {Id, Type, Key, UserDef, OrberDef, Flags}, [CodeSetComp]),    
    print_helper(IoDevice, IOR);
print(IoDevice, {Id, Type, Key, UserDef, OrberDef, Flags}) ->
    CodeSetComp = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=?DEFAULT_CODESETS},
    IOR = create(orber:giop_version(), binary_to_list(Id), orber:host(), orber:iiop_port(),
		 {Id, Type, Key, UserDef, OrberDef, Flags}, [CodeSetComp]),
    print_helper(IoDevice, IOR);
print(_, _) ->
    exit("Bad parameter").

print_helper(IoDevice, #'IOP_IOR'{type_id=TypeID, profiles=Profs}) ->
    Data = io_lib:format("================== IOR ====================
------------------ IFR ID -----------------~n~s~n", [TypeID]),
    NewData = print_profiles(Profs, []),
    print_it(IoDevice, [Data|NewData]).

print_profiles([], Acc) ->
   lists:flatten([Acc | io_lib:format("================== END ====================~n", [])]);
print_profiles([#'IOP_TaggedProfile'
		{tag=?TAG_INTERNET_IOP,
		 profile_data = #'IIOP_ProfileBody_1_0'{iiop_version=
							#'IIOP_Version'{major=Major,
									minor=Minor},
							host=Host, port=Port,
							object_key=Objkey}}|T], Acc) ->
    Profile = io_lib:format("~n------------------ IIOP Profile -----------
Version..........: ~p.~p
Host.............: ~s
Port.............: ~p~n", [Major, Minor, Host, Port]),
    ObjKeyStr = print_objkey(Objkey),
    print_profiles(T, [Profile, ObjKeyStr | Acc]);
print_profiles([#'IOP_TaggedProfile'
		{tag=?TAG_INTERNET_IOP,
		 profile_data = #'IIOP_ProfileBody_1_1'{iiop_version=
							#'IIOP_Version'{major=Major,
									minor=Minor},
							host=Host,
							port=Port,
							object_key=Objkey,
							components=Components}}|T], Acc) ->
    Profile = io_lib:format("~n------------------ IIOP Profile -----------
Version..........: ~p.~p
Host.............: ~s
Port.............: ~p~n", [Major, Minor, Host, Port]),
    ComponentsStr = print_components(Components, []),
    ObjKeyStr = print_objkey(Objkey),
    print_profiles(T, [Profile, ComponentsStr, ObjKeyStr |Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS,
				     profile_data = Components}|T], Acc) ->
    MComp = io_lib:format("~n------------------ Multiple Components ----~n", []),
    ComponentsStr = print_components(Components, []),
    print_profiles(T, [MComp, ComponentsStr | Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=?TAG_SCCP_IOP,
				     profile_data = Data}|T], Acc) ->
    SCCP = io_lib:format("~n------------------ SCCP IOP ---------------~n", []),
    print_profiles(T, [SCCP | Acc]);
print_profiles([#'IOP_TaggedProfile'{tag=Tag,
				     profile_data = Data}|T], Acc) ->
    TAG = io_lib:format("~n------------------ TAG ~p -----------------~n", [Tag]),
    print_profiles(T, [TAG|Acc]).

print_components([], Data) -> lists:flatten(lists:reverse(Data));
print_components([#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					 component_data=ORB}|T], Data) ->
    OType = io_lib:format("ORB Type.........: ~p~n", [ORB]),
    print_components(T, [OType | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					 component_data=
					 #'CONV_FRAME_CodeSetComponentInfo'
					 {'ForCharData' = Char,
					  'ForWcharData' = Wchar}}|T], Data) ->
    CharSet = io_lib:format("Native Char......: ~p
Char Conversion..: ~p
Native Wchar.....: ~p
Wchar Conversion.: ~p~n", 
	      [Char#'CONV_FRAME_CodeSetComponent'.native_code_set,
	       Char#'CONV_FRAME_CodeSetComponent'.conversion_code_sets,
	       Wchar#'CONV_FRAME_CodeSetComponent'.native_code_set,
	       Wchar#'CONV_FRAME_CodeSetComponent'.conversion_code_sets]),
    print_components(T, [CharSet | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					 component_data=#'ALTERNATE_IIOP_ADDRESS'
					 {'HostID'=Host, 'Port'=Port}}|T], Data) ->
    AltAddr = io_lib:format("Alternate Address: ~s:~p~n", [Host, Port]),
    print_components(T, [AltAddr | Data]);
print_components([#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					 component_data=#'SSLIOP_SSL'
					 {target_supports=Supports, 
					  target_requires=Requires,
					  port=Port}}|T], Data) ->
    SSL = io_lib:format("SSL Port.........: ~p
SSL Requires.....: ~p
SSL Supports.....: ~p~n", [Port, Requires, Supports]),
    print_components(T, [SSL | Data]);
print_components([#'IOP_TaggedComponent'{tag=TAG, 
					 component_data=CData}|T], Data) ->
    Unused = io_lib:format("Unused Component.: ~s~n", [match_tag(TAG)]),
    Octets = print_octets(CData, [], 1, []),
    print_components(T, [lists:flatten([Unused | Octets])| Data]).


print_objkey(Objkey) when tuple(Objkey) ->
    io_lib:format("Local Object.....:~n~p~n", [Objkey]);
print_objkey(Objkey) ->
    Hdr = io_lib:format("External Object..: ~n", []),
    Octets = print_octets(Objkey, [], 1, []),
    lists:flatten([Hdr | Octets]).

print_octets([], [], _, Data) ->
    lists:reverse(Data);
print_octets([], Acc, C, Data) ->
    Filling = lists:duplicate((4*(9-C)), 32),
    FData = io_lib:format("~s", [Filling]),
    Rest = io_lib:format("  ~p~n", [Acc]),
    [lists:reverse(Data), FData | Rest];
print_octets([H|T], Acc, 8, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~4w", [H]),
    D2 = io_lib:format("  ~p~n", [lists:reverse([H|Acc])]),
    print_octets(T, [], 1, [D2, D1 | Data]);
print_octets([H|T], Acc, 1, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~3w", [H]),
    print_octets(T, [H|Acc], 2, [D1 | Data]);
print_octets([H|T], Acc, C, Data) when H > 31 , H < 127 ->
    D1 = io_lib:format("~4w", [H]),
    print_octets(T, [H|Acc], C+1, [D1 | Data]);
print_octets([H|T], Acc, 8, Data) ->
    D1 = io_lib:format("~4w", [H]),
    D2 = io_lib:format("  ~p~n", [lists:reverse([$.|Acc])]),
    print_octets(T, [], 1, [D2, D1 | Data]);
print_octets([H|T], Acc, 1, Data) ->
    D1 = io_lib:format("~3w", [H]),
    print_octets(T, [$.|Acc], 2, [D1|Data]);
print_octets([H|T], Acc, C, Data) ->
    D1 = io_lib:format("~4w", [H]),
    print_octets(T, [$.|Acc], C+1, [D1|Data]).

print_it(undefined, Data) ->
    io:format(Data);
print_it(error_report, Data) ->
    error_logger:error_report(Data);
print_it(info_msg, Data) ->
    error_logger:info_msg(Data);
print_it(string, Data) ->
    lists:flatten(Data);
print_it({error_report, Msg}, Data) ->
    error_logger:error_report(io_lib:format("================== Reason =================~n~s~n~s", 
					    [Msg, Data]));
print_it({info_msg, Msg}, Data) ->
    error_logger:info_msg(io_lib:format("================== Comment ================~n~s~n~s", 
					[Msg, Data]));
print_it(IoDevice, Data) ->
    io:format(IoDevice, Data).

match_tag(?TAG_ORB_TYPE) -> ?TAG_ORB_TYPE_STR;
match_tag(?TAG_CODE_SETS) -> ?TAG_CODE_SETS_STR;
match_tag(?TAG_POLICIES) -> ?TAG_POLICIES_STR;
match_tag(?TAG_ALTERNATE_IIOP_ADDRESS) -> ?TAG_ALTERNATE_IIOP_ADDRESS_STR;
match_tag(?TAG_COMPLETE_OBJECT_KEY) -> ?TAG_COMPLETE_OBJECT_KEY_STR;
match_tag(?TAG_ENDPOINT_ID_POSITION) -> ?TAG_ENDPOINT_ID_POSITION_STR;
match_tag(?TAG_LOCATION_POLICY) -> ?TAG_LOCATION_POLICY_STR;
match_tag(?TAG_ASSOCIATION_OPTIONS) -> ?TAG_ASSOCIATION_OPTIONS_STR;
match_tag(?TAG_SEC_NAME) -> ?TAG_SEC_NAME_STR;
match_tag(?TAG_SPKM_1_SEC_MECH) -> ?TAG_SPKM_1_SEC_MECH_STR;
match_tag(?TAG_SPKM_2_SEC_MECH) -> ?TAG_SPKM_2_SEC_MECH_STR;
match_tag(?TAG_KerberosV5_SEC_MECH) -> ?TAG_KerberosV5_SEC_MECH_STR;
match_tag(?TAG_CSI_ECMA_Secret_SEC_MECH) -> ?TAG_CSI_ECMA_Secret_SEC_MECH_STR;
match_tag(?TAG_CSI_ECMA_Hybrid_SEC_MECH) -> ?TAG_CSI_ECMA_Hybrid_SEC_MECH_STR;
match_tag(?TAG_SSL_SEC_TRANS) -> ?TAG_SSL_SEC_TRANS_STR;
match_tag(?TAG_CSI_ECMA_Public_SEC_MECH) -> ?TAG_CSI_ECMA_Public_SEC_MECH_STR;
match_tag(?TAG_GENERIC_SEC_MECH) -> ?TAG_GENERIC_SEC_MECH_STR;
match_tag(?TAG_FIREWALL_TRANS) -> ?TAG_FIREWALL_TRANS_STR;
match_tag(?TAG_SCCP_CONTACT_INFO) -> ?TAG_SCCP_CONTACT_INFO_STR;
match_tag(?TAG_JAVA_CODEBASE) -> ?TAG_JAVA_CODEBASE_STR;
match_tag(?TAG_TRANSACTION_POLICY) -> ?TAG_TRANSACTION_POLICY_STR;
match_tag(?TAG_FT_GROUP) -> ?TAG_FT_GROUP_STR;
match_tag(?TAG_FT_PRIMARY) -> ?TAG_FT_PRIMARY_STR;
match_tag(?TAG_FT_HEARTBEAT_ENABLED) -> ?TAG_FT_HEARTBEAT_ENABLED_STR;
match_tag(?TAG_MESSAGE_ROUTERS) -> ?TAG_MESSAGE_ROUTERS_STR;
match_tag(?TAG_OTS_POLICY) -> ?TAG_OTS_POLICY_STR;
match_tag(?TAG_INV_POLICY) -> ?TAG_INV_POLICY_STR;
match_tag(?TAG_CSI_SEC_MECH_LIST) -> ?TAG_CSI_SEC_MECH_LIST_STR;
match_tag(?TAG_NULL_TAG) -> ?TAG_NULL_TAG_STR;
match_tag(?TAG_SECIOP_SEC_TRANS) -> ?TAG_SECIOP_SEC_TRANS_STR;
match_tag(?TAG_TLS_SEC_TRANS) -> ?TAG_TLS_SEC_TRANS_STR;
match_tag(?TAG_DCE_STRING_BINDING) -> ?TAG_DCE_STRING_BINDING_STR;
match_tag(?TAG_DCE_BINDING_NAME) -> ?TAG_DCE_BINDING_NAME_STR;
match_tag(?TAG_DCE_NO_PIPES) -> ?TAG_DCE_NO_PIPES_STR;
match_tag(?TAG_DCE_SEC_MECH) -> ?TAG_DCE_SEC_MECH_STR;
match_tag(?TAG_INET_SEC_TRANS) -> ?TAG_INET_SEC_TRANS_STR;
match_tag(Tag) -> integer_to_list(Tag).

%%-----------------------------------------------------------------
%% Func: string_code/1
%%-----------------------------------------------------------------
string_code(IOR) ->
    Version = orber:giop_version(),
    {IorByteSeq0, Length0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {IorByteSeq, _} = code(Version, IOR, IorByteSeq0, Length0),
    IorByteSeq1 = binary_to_list(list_to_binary(lists:reverse(IorByteSeq))),
    IorHexSeq = bytestring_to_hexstring(IorByteSeq1),
    [$I,$O,$R,$: | IorHexSeq].
    
%%-----------------------------------------------------------------
%% Func: code/3
%%-----------------------------------------------------------------
code(Version, #'IOP_IOR'{type_id=TypeId, profiles=Profiles}, Bytes, Len) ->
    ProfileSeq =code_profile_datas(Version, Profiles),
    %% Byte order
    cdr_encode:enc_type(?IOR_TYPEDEF,
			Version, 
			#'IOP_IOR'{type_id=TypeId, profiles=ProfileSeq},
			Bytes, Len);
code(Version, {Id, Type, Key, UserDef, OrberDef, Flags}, Bytes, Len) when atom(Id) ->
    case orber:exclude_codeset_component() of
	true ->
	    IOR = create(Version, Id:typeID(), orber:host(), orber:iiop_port(),
			 {Id, Type, Key, UserDef, OrberDef, Flags}, []),
	    code(Version, IOR, Bytes, Len);
	_ ->
	    CodeSetComp = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
						 component_data=?DEFAULT_CODESETS},
	    IOR = create(Version, Id:typeID(), orber:host(), orber:iiop_port(),
			 {Id, Type, Key, UserDef, OrberDef, Flags}, [CodeSetComp]),
	    code(Version, IOR, Bytes, Len)
    end;
code(Version, {Id, Type, Key, UserDef, OrberDef, Flags}, Bytes, Len) ->
    case orber:exclude_codeset_component() of
	true ->
	    IOR = create(Version, binary_to_list(Id), orber:host(), orber:iiop_port(),
			 {Id, Type, Key, UserDef, OrberDef, Flags}, []),
	    code(Version, IOR, Bytes, Len);
	_ ->
	    CodeSetComp = #'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
						 component_data=?DEFAULT_CODESETS},
	    IOR = create(Version, binary_to_list(Id), orber:host(), orber:iiop_port(),
			 {Id, Type, Key, UserDef, OrberDef, Flags}, [CodeSetComp]),
	    code(Version, IOR, Bytes, Len)
    end.

code_profile_datas(_, []) ->
    [];
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=P} | Profiles]) ->
    NewBytes = list_to_binary(code_profile_data(Version, P)),
    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=NewBytes} | 
     code_profile_datas(Version, Profiles)];
%code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
%						  profile_data=P} | Profiles]) ->
%    Profs= code_comp(Version, P, []),
%    [#'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
%			  profile_data=Profs}| code_profile_datas(Version, Profiles)];
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=N, profile_data=P} | Profiles]) ->
    [#'IOP_TaggedProfile'{tag=N, profile_data=P} | code_profile_datas(Version, Profiles)];
code_profile_datas(_, Data) ->
    orber:dbg("[~p] iop_ior:code_profile_datas(~p); unsupported TaggedProfile.", 
	      [?LINE, Data], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

code_profile_data(Version, ProfileData) ->
    [RecTag, V, H, P, O |Rest] = tuple_to_list(ProfileData),
    {Bytes, Length} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Length1} = cdr_encode:enc_type(?IIOP_VERSION, Version, V, Bytes, Length),
    {Bytes2, Length2} = cdr_encode:enc_type({'tk_string', 0}, Version,
					    H, Bytes1, Length1),
    {Bytes3, Length3} = cdr_encode:enc_type('tk_ushort', Version, P, Bytes2, Length2),
    {Bytes4, Length4} = cdr_encode:enc_type({'tk_sequence', 'tk_octet', 0}, Version,
					    corba:objkey_to_string(O), Bytes3, Length3),
    {Bytes5, Length5} = code_profile_data_1(Version, RecTag, Rest, Bytes4, Length4), 
    Bytes6 = lists:reverse(Bytes5),
    lists:flatten(Bytes6).

code_profile_data_1(Version, 'IIOP_ProfileBody_1_0', [], Bytes, Length) ->
    {Bytes, Length};
code_profile_data_1(Version, 'IIOP_ProfileBody_1_1', [TaggedComponentSeq], Bytes, Length) ->
    Comps = code_comp(Version, TaggedComponentSeq, []),
    cdr_encode:enc_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Comps, Bytes, Length);
code_profile_data_1(_,V,S,_,_) ->
    orber:dbg("[~p] iop_ior:code_profile_datas(~p, ~p); probably unsupported IIOP-version", 
	      [?LINE, V, S], ?DEBUG_LEVEL),
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

code_comp(Version, [], CompData) ->
    CompData;
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					   component_data=CodeSet}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Len1} = cdr_encode:enc_type(?CONV_FRAME_CODESETCOMPONENTINFO, Version, 
					 CodeSet, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					   component_data=ORBType}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Len1} = cdr_encode:enc_type(?ORB_TYPE, Version, 
					 ORBType, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					   component_data=AltAddr}|Comps], CompData) ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Len1} = cdr_encode:enc_type(?ALTERNATE_IIOP_ADDRESS, Version, 
					 AltAddr, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					   component_data=SSLStruct}|Comps], CompData)
  when record(SSLStruct, 'SSLIOP_SSL') ->
    {Bytes0, Len0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {Bytes1, Len1} = cdr_encode:enc_type(?SSLIOP_SSL, Version, 
					 SSLStruct, Bytes0, Len0),
    Bytes = binary_to_list(list_to_binary(lists:reverse(Bytes1))),
    code_comp(Version, Comps, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
						      component_data=Bytes}|CompData]);
code_comp(Version, [C|Comps], CompData) -> 
    code_comp(Version, Comps, [C|CompData]).

%%-----------------------------------------------------------------
%% Func: string_decode/1
%%-----------------------------------------------------------------
string_decode([$I,$O,$R,$: | IorHexSeq]) ->
    Version = orber:giop_version(),
    IorByteSeq = list_to_binary(hexstring_to_bytestring(IorHexSeq)),
    {ByteOrder, IorRest} = cdr_decode:dec_byte_order(IorByteSeq),
    decode(Version, IorRest, 1, ByteOrder);
string_decode([$i,$o,$r,$: | IorHexSeq]) ->
    Version = orber:giop_version(),
    IorByteSeq = list_to_binary(hexstring_to_bytestring(IorHexSeq)),
    {ByteOrder, IorRest} = cdr_decode:dec_byte_order(IorByteSeq),
    decode(Version, IorRest, 1, ByteOrder);
string_decode(What) ->
    orber:dbg("[~p] iop_ior:string_decode(~p); Should be IOR:.. or ior:..", 
	      [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Func: decode/3
%%-----------------------------------------------------------------
decode(Version, IorByteSeq, Len, ByteOrder) ->
    {#'IOP_IOR'{type_id=TypeId, profiles=Profiles}, Rest, Length} =
	cdr_decode:dec_type(?IOR_TYPEDEF, Version, IorByteSeq, Len, ByteOrder),
    L = decode_profiles(Version, Profiles),
    {#'IOP_IOR'{type_id=TypeId, profiles=L}, Rest, Length}.

decode_profiles(_, []) ->
    [];
decode_profiles(Version, [P | Profiles]) ->
    Struct = decode_profile(Version, P),
    L = decode_profiles(Version, Profiles),
    [Struct | L].

decode_profile(Version, #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=ProfileData}) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(ProfileData)),
    Length = 1,
    {V, Rest1, Length1} = cdr_decode:dec_type(?IIOP_VERSION, Version, Rest, Length,
					      ByteOrder),
    {H, Rest2, Length2} = cdr_decode:dec_type({'tk_string', 0}, Version, Rest1, Length1,
					      ByteOrder),
    {P, Rest3, Length3} = cdr_decode:dec_type('tk_ushort', Version, Rest2, Length2,
					      ByteOrder),
    {ObjKey, Rest4, Length4} = cdr_decode:dec_type({'tk_sequence', 'tk_octet', 0},
						   Version, Rest3, Length3, 
						   ByteOrder),
    Struct = decode_profile_1(V, H, P, ObjKey, Version, Rest4, Length4, ByteOrder),
    #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=Struct};
%decode_profile(Version, #'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, 
%					     profile_data=ProfileData}) ->
%    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(ProfileData)),
%    {Components, <<>>, Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, 1, ByteOrder),
%    CompData = decode_comp(Version, Components, []),
%    #'IOP_TaggedProfile'{tag=?TAG_MULTIPLE_COMPONENTS, profile_data=CompData};
decode_profile(_, #'IOP_TaggedProfile'{tag=N, profile_data=ProfileData}) ->
    #'IOP_TaggedProfile'{tag=N, profile_data=ProfileData};
decode_profile(_, Data) ->
    orber:dbg("[~p] iop_ior:decode_profile(~p); unsupported TaggedProfile.", 
	      [?LINE, Data], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).

decode_profile_1(#'IIOP_Version'{major=1, minor=0}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    #'IIOP_ProfileBody_1_0'{iiop_version=#'IIOP_Version'{major=1,
							 minor=0}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey)};
decode_profile_1(#'IIOP_Version'{major=1, minor=1}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    {Components, <<>>, Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, Length, ByteOrder),
    CompData = decode_comp(Version, Components, []),
    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=1,
							 minor=1}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey),
			    components=CompData};
decode_profile_1(#'IIOP_Version'{major=1, minor=2}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    {Components, <<>>, Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT_SEQ, Version, Rest, Length, ByteOrder),
    CompData = decode_comp(Version, Components, []),
    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=1,
							 minor=2}, 
			    host=H, port=P,
			    object_key=corba:string_to_objkey(ObjKey),
			    components=CompData};
decode_profile_1(V, _, _, _, _, _, _,_) ->
    orber:dbg("[~p] iop_ior:decode_profile_1(~p); probably unsupported IIOP-version.", 
	      [?LINE, V], ?DEBUG_LEVEL),
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

decode_comp(Version, [], Components) ->
%    ?PRINTDEBUG2("COMPONENTS: ~p~n", [Components]),
    Components;
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {CodeSet, _, _} = cdr_decode:dec_type(?CONV_FRAME_CODESETCOMPONENTINFO, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_CODE_SETS, 
					component_data=CodeSet}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {ORBType, _, _} = cdr_decode:dec_type(?ORB_TYPE, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_ORB_TYPE, 
					component_data=ORBType}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					     component_data=Bytes}|Comps],
	    Components) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(list_to_binary(Bytes)),
    {AltIIOP, _, _} = cdr_decode:dec_type(?ALTERNATE_IIOP_ADDRESS, 
					  Version, Rest, 1, ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_ALTERNATE_IIOP_ADDRESS, 
					component_data=AltIIOP}|Components]);
decode_comp(Version, [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					     component_data=Data}|Comps], Components) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(list_to_binary(Data)),
    {SSLStruct, Rest1, Length1} = cdr_decode:dec_type(?SSLIOP_SSL, Version, R, 1,
						      ByteOrder),
    decode_comp(Version, Comps, 
		[#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
					component_data=SSLStruct}|Components]);
decode_comp(Version, [C|Comps], Components) ->
    %% Not used but we cannot discard it.
    decode_comp(Version, Comps, [C|Components]).

%%-----------------------------------------------------------------
%% Func: hexstring_to_bytestring/1
%%-----------------------------------------------------------------
hexstring_to_bytestring(HexString) ->
    ByteString = hexstring_to_bytestring(HexString, []),
    lists:reverse(ByteString).

hexstring_to_bytestring([], Acc) ->
    Acc;
hexstring_to_bytestring([H1, H2 |Rest], Acc) ->
    I1 = hex_to_int(H1),
    I2 = hex_to_int(H2),
    I = I1 * 16 + I2,
    Acc2 = cdrlib:enc_octet(I, Acc),
    hexstring_to_bytestring(Rest, Acc2).


hex_to_int(H) when H >= $a ->
    10 + H - $a;
hex_to_int(H) when H >= $A ->
    10 + H -$A;
hex_to_int(H) ->
    H - $0.
%%-----------------------------------------------------------------
%% Func: bytestring_to_hexstring/1
%% Args: A byte string
%% Returns: A list of hexadecimal digits (onebyte will be represented as 
%%          two hexadecimal digits).
%%-----------------------------------------------------------------
bytestring_to_hexstring(ByteString) ->
    HexString = bytestring_to_hexstring(ByteString, []),
    lists:reverse(HexString).

bytestring_to_hexstring([], Acc) ->
    Acc;
bytestring_to_hexstring([B |Rest], Acc) ->
    [C1, C2] = int_to_hex(B),
    bytestring_to_hexstring(Rest,[C2, C1| Acc]).

int_to_hex(B) when B < 256, B >= 0 ->
    N1 = B div 16,
    N2 = B rem 16,
    [code_character(N1),
     code_character(N2)].

code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $a + (N - 10).

