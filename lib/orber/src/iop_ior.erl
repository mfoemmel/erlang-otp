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
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the IOP::IOR handling
%%
%% Creation date: 970327
%%
%%-----------------------------------------------------------------
-module(iop_ior).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([code/4, decode/4, string_code/1, string_decode/1, create/5,
	 get_key/1, get_typeID/1, get_objkey/1, check_nil/1,
	 get_privfield/1, set_privfield/2, 
	 get_orbfield/1, set_orbfield/2, 
	 get_flagfield/1, set_flagfield/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: create/4
%%-----------------------------------------------------------------
create(TypeID, Host, IIOP_port, Objkey) ->
    create(orber:giop_version(), TypeID, Host, IIOP_port, Objkey).

create({1, 0}, TypeID, Host, IIOP_port, Objkey) ->
    V=#'IIOP_Version'{major=1,
		      minor=0},
    PB=#'IIOP_ProfileBody_1_0'{iiop_version=V,
			   host=Host,
			   port=IIOP_port,
			   object_key=Objkey},
    #'IOP_IOR'{type_id=TypeID, profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							      profile_data=PB}]};
create({1, 1}, TypeID, Host, IIOP_port, Objkey) ->
    V=#'IIOP_Version'{major=1,
		      minor=1},
    Components = case orber:iiop_ssl_port() of
		       -1 ->
			   [];
		       SSLPort ->
			   [#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, 
				  component_data=[0 | cdrlib:enc_unsigned_short(2, 
				  cdrlib:enc_unsigned_short(2,
				    cdrlib:enc_unsigned_short(SSLPort, [])))]}]
		   end,
    PB=#'IIOP_ProfileBody_1_1'{iiop_version=V,
			   host=Host,
			   port=IIOP_port,
			   object_key=Objkey,
			   components=Components},
    #'IOP_IOR'{type_id=TypeID, profiles=[#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
							      profile_data=PB}]}.

%%-----------------------------------------------------------------
%% Func: get_key/1
%%-----------------------------------------------------------------
get_key(#'IOP_IOR'{profiles=P})  ->
    get_key_1(P);
get_key({_Id, _Type, Key, _UserDef, _OrberDef, _Flags}) ->
    if
	binary(Key) ->
	    {'internal', Key};
	atom(Key) ->
	    {'internal_registered', Key}
    end;
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
get_key({_Id, _Type, Key, _UserDef}) ->
    if
	binary(Key) ->
	    {'internal', Key};
	atom(Key) ->
	    {'internal_registered', Key}
    end.

get_key_1([])  ->
    corba:raise(#'INV_OBJREF'{});
get_key_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB} | P])  ->
    [_, Version, Host, IIOP_port, ObjectKey | Rest] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, Key, _UserDef, _OrberDef, _Flags} ->
	    if
		binary(Key) ->
		    {'internal', Key};
		atom(Key) ->
		    {'internal_registered', Key}
	    end;
	%% Remove next case when we no longer wish to handle ObjRef/4 
	%% (only ObjRef/6).
	{_Id, _Type, Key, _UserDef} ->
	    if
		binary(Key) ->
		    {'internal', Key};
		atom(Key) ->
		    {'internal_registered', Key}
	    end;
	OK ->
	    case check_component_data(Rest, Version) of
		[] ->
		    {'external', {normal, Host, IIOP_port, OK}};
		{ssl, SSLData} ->
		    {'external', {ssl, Host, SSLData, OK}}
	    end
    end;
get_key_1([_ | P])  ->
    get_key_1(P).
check_component_data([], Version) ->
    [];
check_component_data([CompData], Version) ->
    check_ssl(CompData, Version).

check_ssl([], Version) ->
    [];
check_ssl([#'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS, component_data=Data} | Rest], Version) ->
    {ByteOrder, R} = cdr_decode:dec_byte_order(Data),
    {SSLStruct, Rest1, Length1} = cdr_decode:dec_type(?SSLIOP_SSL, Version, R, 0,
					      ByteOrder),
    {ssl, SSLStruct};
check_ssl([_ | Rest], Version) ->
    check_ssl(Rest, Version).


%%-----------------------------------------------------------------
%% Func: get_typeID/1
%%-----------------------------------------------------------------
get_typeID(#'IOP_IOR'{type_id=TypeID}) ->
    TypeID;
get_typeID({Id, _Type, _Key, _UserDef, _OrberDef, _Flags}) ->
    binary_to_list(Id);
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
get_typeID({Id, _Type, _Key, _UserDef}) ->
    binary_to_list(Id).

%%-----------------------------------------------------------------
%% Func: get_objkey/1
%%-----------------------------------------------------------------
get_objkey(#'IOP_IOR'{profiles=P}) ->
    get_objkey_1(P);
get_objkey({Id, Type, Key, UserDef, OrberDef, Flags}) ->
    {Id, Type, Key, UserDef, OrberDef, Flags};
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
get_objkey({Id, Type, Key, UserDef}) ->
    {Id, Type, Key, UserDef}.

get_objkey_1([]) ->
    corba:raise(#'INV_OBJREF'{});
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
    UserDef;
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
get_privfield({Id, Type, Key, UserDef}) ->
    UserDef.

get_privfield_1([]) ->
    corba:raise(#'INV_OBJREF'{});
get_privfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, UserDef, _OrberDef, _Flags} ->
	    UserDef;
	%% Remove next case when we no longer wish to handle ObjRef/4
	%% (only ObjRef/6).
	{_Id, _Type, _Key, UserDef} ->
	    UserDef;
	_ ->
	    corba:raise(#'INV_OBJREF'{})
    end;
get_privfield_1([_| P]) ->
    get_privfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_privfield/2
%%-----------------------------------------------------------------
set_privfield(#'IOP_IOR'{type_id=Id, profiles=P}, UserData) ->
    #'IOP_IOR'{type_id=Id, profiles=set_privfield_1(P, UserData)};
set_privfield({Id, Type, Key, _, OrberDef, Flags}, UserData) ->
	    {Id, Type, Key, UserData, OrberDef, Flags};
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
set_privfield({Id, Type, Key, _}, UserData) ->
	    {Id, Type, Key, UserData}.

set_privfield_1([], UserData) ->
    [];
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
	%% Remove next case when we no longer wish to handle ObjRef/4
	%% (only ObjRef/6).
	{Id, Type, Key, _} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, UserData}|
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
    corba:raise(#'INV_OBJREF'{});
get_orbfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, OrberDef, _Flags} ->
	    OrberDef;
	_ ->
	    corba:raise(#'INV_OBJREF'{})
    end;
get_orbfield_1([_| P]) ->
    get_orbfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_orbfield/2
%%-----------------------------------------------------------------
set_orbfield(#'IOP_IOR'{type_id=Id, profiles=P}, OrberDef) ->
    #'IOP_IOR'{type_id=Id, profiles=set_orbfield_1(P, OrberDef)};
set_orbfield({Id, Type, Key, Priv, _, Flags}, OrberDef) ->
	    {Id, Type, Key, Priv, OrberDef, Flags};
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
%% NOTE(!!): calling this function with ObjeRef/4 will return ObjRef/6!!!
set_orbfield({Id, Type, Key, Priv}, OrberDef) ->
	    {Id, Type, Key, Priv, OrberDef, term_to_binary(undefined)}.

set_orbfield_1([], OrberDef) ->
    [];
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
	%% Remove next case when we no longer wish to handle ObjRef/4 
	%% (only ObjRef/6).
	{Id, Type, Key, Priv} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, OrberDef, term_to_binary(undefined)}|
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
    corba:raise(#'INV_OBJREF'{});
get_flagfield_1([#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP, profile_data=PB}| P]) ->
    [_, _, Host, IIOP_port, ObjectKey | _] = tuple_to_list(PB),
    case ObjectKey of
	{_Id, _Type, _Key, _UserDef, _OrberDef, Flags} ->
	    Flags;
	_ ->
	    corba:raise(#'INV_OBJREF'{})
    end;
get_flagfield_1([_| P]) ->
    get_flagfield_1(P).
    
%%-----------------------------------------------------------------
%% Func: set_flagfield/2
%%-----------------------------------------------------------------
set_flagfield(#'IOP_IOR'{type_id=Id, profiles=P}, Flags) ->
    #'IOP_IOR'{type_id=Id, profiles=set_flagfield_1(P, Flags)};
set_flagfield({Id, Type, Key, Priv, OrberDef, _}, Flags) ->
	    {Id, Type, Key, Priv, OrberDef, Flags};
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
%% NOTE(!!): calling this function with ObjeRef/4 will return ObjRef/6!!!
set_flagfield({Id, Type, Key, Priv}, Flags) ->
	    {Id, Type, Key, Priv, term_to_binary(undefined), Flags}.

set_flagfield_1([], Flags) ->
    [];
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
	%% Remove next case when we no longer wish to handle ObjRef/4 
	%% (only ObjRef/6).
	{Id, Type, Key, Priv} ->
	    [#'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
				  profile_data=list_to_tuple([RecName,
							      Version, Host,
							      IIOP_port,
							      {Id, Type, Key, Priv, term_to_binary(undefined), Flags}|
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
check_nil({Id, _, _, _, _, _}) ->  
    case binary_to_list(Id) of
	"" ->
	    true; 
	_ ->
	    false
    end;
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
check_nil({Id, _, _, _}) ->  
    case binary_to_list(Id) of
	"" ->
	    true; 
	_ ->
	    false
    end;
check_nil(_) ->
    false.
%%-----------------------------------------------------------------
%% Func: string_code/1
%%-----------------------------------------------------------------
string_code(IOR) ->
    Version = orber:giop_version(),
    {IorByteSeq0, Length0} = cdr_encode:enc_type('tk_octet', Version, 0, [], 0),
    {IorByteSeq, _} = code(Version, IOR, IorByteSeq0, Length0),
    IorByteSeq1 = lists:reverse(IorByteSeq),
    IorByteSeq2 = lists:flatten(IorByteSeq1),
    IorHexSeq = bytestring_to_hexstring(IorByteSeq2),
    [$I,$O,$R,$: | IorHexSeq].
    
%%-----------------------------------------------------------------
%% Func: code/3
%%-----------------------------------------------------------------
code(Version, #'IOP_IOR'{type_id=TypeId, profiles=Profiles}, Bytes, Len) ->
    ProfileSeq = code_profile_datas(Version, Profiles),
    %% Byte order
    cdr_encode:enc_type(?IOR_TYPEDEF,
			Version, 
			#'IOP_IOR'{type_id=TypeId, profiles=ProfileSeq},
			Bytes, Len);
code(Version, {Id, Type, Key, UserDef, OrberDef, Flags}, Bytes, Len) ->
    IOR = create(Version, binary_to_list(Id), orber:host(), orber:iiop_port(),
		 {Id, Type, Key, UserDef, OrberDef, Flags}),
    code(Version, IOR, Bytes, Len);
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
code(Version, {Id, Type, Key, UserDef}, Bytes, Len) ->
    IOR = create(Version, binary_to_list(Id), orber:host(), orber:iiop_port(),
		 {Id, Type, Key, UserDef}),
    code(Version, IOR, Bytes, Len).

code_profile_datas(_, []) ->
    [];
code_profile_datas(Version, [#'IOP_TaggedProfile'{tag=0, profile_data=P} | Profiles]) ->
%%    P1 = convert_objkey_to_byteseq(P),
    NewBytes = code_profile_data(Version, P),
    [#'IOP_TaggedProfile'{tag=0, profile_data=NewBytes} | code_profile_datas(Version, Profiles)];
code_profile_datas(_, [#'IOP_TaggedProfile'{tag=1, profile_data=P} | Profiles]) ->
        exit({'code_profile_datas', 'multiple components not supported'}).

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
    cdr_encode:enc_type(?IOP_TAGGEDCOMPONENT, Version, TaggedComponentSeq, Bytes, Length);
code_profile_data_1(_,_,_,_,_) ->
    corba:raise(#'NO_IMPLEMENT'{}).

%convert_objkey_to_byteseq(#'IIOP_ProfileBody_1_0'{iiop_version=#'IIOP_Version'{major=Major,
%									       minor=Minor},
%						  host=Host, port=Port,
%						  object_key=ObjKey}) ->
%    #'IIOP_ProfileBody_1_0'{iiop_version=#'IIOP_Version'{major=Major,
%							 minor=Minor}, 
%			    host=Host, port=Port,
%			    object_key=corba:objkey_to_string(ObjKey)};
%convert_objkey_to_byteseq(#'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=Major,
%									       minor=Minor}, 
%						  host=Host, port=Port,
%						  object_key=ObjKey,
%						  components=Components}) ->
%    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=Major,
%							 minor=Minor}, 
%			    host=Host, port=Port,
%			    object_key=corba:objkey_to_string(ObjKey),
%			    components=Components};
%convert_objkey_to_byteseq(_) ->
%        corba:raise(#'NO_IMPLEMENT'{}).

%%-----------------------------------------------------------------
%% Func: string_decode/1
%%-----------------------------------------------------------------
string_decode([$I,$O,$R,$: | IorHexSeq]) ->
    Version = orber:giop_version(),
    IorByteSeq = hexstring_to_bytestring(IorHexSeq),
    {ByteOrder, IorRest} = cdr_decode:dec_byte_order(IorByteSeq),
    decode(Version, IorRest, 1, ByteOrder).

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

decode_profile(Version, #'IOP_TaggedProfile'{tag=0, profile_data=ProfileData}) ->
    {ByteOrder, Rest} = cdr_decode:dec_byte_order(ProfileData),
    Length = 1,
    {V, Rest1, Length1} = cdr_decode:dec_type(?IIOP_VERSION, Version, Rest, Length,
					      ByteOrder),
    {H, Rest2, Length2} = cdr_decode:dec_type({'tk_string', 0}, Version, Rest1, Length1,
					      ByteOrder),
    {P, Rest3, Length3} = cdr_decode:dec_type('tk_ushort', Version, Rest2, Length2,
					      ByteOrder),

    {ObjKey, Rest4, Length4} = cdr_decode:dec_type({'tk_sequence', 'tk_octet', 0}, Version, Rest3, Length3,
					      ByteOrder),
    
    Struct = decode_profile_1(V, H, P, corba:string_to_objkey(ObjKey), Version, Rest4, Length4, ByteOrder),
    #'IOP_TaggedProfile'{tag=0, profile_data=Struct};
decode_profile(_, #'IOP_TaggedProfile'{tag=1, profile_data=ProfileData}) ->
    %exit({'decode_profile_data', 'multiple components not supported'}).
    corba:raise(#'NO_IMPLEMENT'{}).

decode_profile_1(#'IIOP_Version'{major=1, minor=0}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    #'IIOP_ProfileBody_1_0'{iiop_version=#'IIOP_Version'{major=1,
							 minor=0}, 
			    host=H, port=P,
			    object_key=ObjKey};
decode_profile_1(#'IIOP_Version'{major=1, minor=1}, H, P, ObjKey, Version, Rest, Length, ByteOrder) ->
    {Components, [], Length1} =cdr_decode:dec_type(?IOP_TAGGEDCOMPONENT, Version, Rest, Length, ByteOrder),
    #'IIOP_ProfileBody_1_1'{iiop_version=#'IIOP_Version'{major=1,
							 minor=1}, 
			    host=H, port=P,
			    object_key=ObjKey,
			    components=Components};
decode_profile_1(_, _, _, _, _, _, _,_) ->
    corba:raise(#'NO_IMPLEMENT'{}).
    

%convert_byteseq_to_objkey(#'IIOP_ProfileBody'{iiop_version=#'IIOP_Version'{major=Major,
%							      minor=Minor}, 
%					      host=Host, port=Port,
%					      object_key=ObjKey}) ->
%    #'IIOP_ProfileBody'{iiop_version=#'IIOP_Version'{major=Major, minor=Minor}, 
%			host=Host, port=Port, object_key=corba:string_to_objkey(ObjKey)}.

%%-----------------------------------------------------------------
%% Func: parseIORfromServiceLog/1
%% Was only used when testing against JacORB !!!!!!!!!!
%%-----------------------------------------------------------------
%parseIORfromServiceLog([HexSeq|_]) ->
%    StartPos = string:str(HexSeq,"IOR:"),
%    NewHexSeq = string:substr(HexSeq, StartPos),
%    EndPos = string:str(NewHexSeq,"\n"),
%    IorString = string:substr(NewHexSeq, 1, EndPos - 1),
%    string_decode(IorString).

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

