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
%%----------------------------------------------------------------------
%% Purpose: 
%%----------------------------------------------------------------------

-module(megaco_sdp).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/include/megaco_sdp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------

-export([
	 parse_LRDesc/1,
	 parse_propertygroup_list/1,
	 parse_propertygroup/1,
	 parse_propertyparm/1,
	 gen_LRDesc/1,
	 gen_propertygroup_list/1,
	 gen_propertygroup/1,
	 gen_propertyparm/1,
	 get_sdp_record_from_group/2
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

-export([
        ]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%-----------------------------------------------------------------
%% Generate sdp records from PropertyParm records
%%-----------------------------------------------------------------


parse_LRDesc(Descr) when record(Descr, 'LocalRemoteDescriptor') ->
    DecodedPropGroups = parse_propertygroup_list(Descr#'LocalRemoteDescriptor'.propGrps),
    Descr#'LocalRemoteDescriptor'{propGrps = DecodedPropGroups}.

parse_propertygroup_list([]) ->
    [];
parse_propertygroup_list(asn1_NOVALUE) ->
    asn1_NOVALUE;
parse_propertygroup_list([PropGrp |PropGrps]) ->
    case catch(parse_propertygroup(PropGrp)) of
	{'EXIT', _} ->
	    parse_propertygroup_list(PropGrps);
	NewPropGrp ->
	    [NewPropGrp | parse_propertygroup_list(PropGrps)]
    end.

parse_propertygroup([]) ->
    [];
parse_propertygroup([PP |PPs]) ->
    [parse_propertyparm(PP) |parse_propertygroup(PPs)].


parse_propertyparm(#'PropertyParm'{name="v", value=[V], extraInfo=EI}) when EI == asn1_NOVALUE  ->
		     #sdp_v{version=V};
parse_propertyparm(#'PropertyParm'{name="c", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	[Network, AddressType, ConnectionAddr]	->
	    #sdp_c{network_type = Network,
		   address_type = AddressType,
		   connection_addr = ConnectionAddr
		  };
	_ ->
	    exit("Wrong Format on Propertyparm")
    end;
parse_propertyparm(#'PropertyParm'{name="m", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	[Media, PortInfo, Transport | FMT]	->
	    {Port, NoOfPorts} = 
		case string:tokens(PortInfo, "/") of
		    [P1, NP] ->
			{list_to_integer(P1), list_to_integer(NP)};
		    [P2] ->
			{list_to_integer(P2), undefined}
		end,
	    #sdp_m{media = Media,
		   port = Port,
		   num_ports = NoOfPorts,
		   transport = Transport,
		   fmt_list = FMT};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;
parse_propertyparm(#'PropertyParm'{name="o", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	[User, SessionId, Version, Network, AddressType, Address] ->
	    #sdp_o{user_name = User,
		   session_id = SessionId,
		   version = Version,
		   network_type = Network,
		   address_type = AddressType,
		   address = Address};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;
parse_propertyparm(#'PropertyParm'{name="a", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, ":/ \t") of
	["rtpmap", Payload, EncName, ClockRate | EncPar] ->
	    #sdp_a_rtpmap{payload_type = 
			  list_to_integer(Payload), 
			  encoding_name = EncName,
			  clock_rate = 
			  list_to_integer(ClockRate),
			  encoding_parms = EncPar};
	["ptime", PacketTime] ->
	    #sdp_a_ptime{packet_time = 
			  list_to_integer(PacketTime)};
	[Var, Value] ->
	    #sdp_a{attribute = Var, value = Value};
	[Var] ->
	    #sdp_a{attribute = Var};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;
parse_propertyparm(#'PropertyParm'{name="b", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, ":") of
	[Modifier, Bandwidth] ->
	    #sdp_b{modifier=Modifier,
		   bandwidth=list_to_integer(Bandwidth)};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;
parse_propertyparm(#'PropertyParm'{name="t", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	[Start, Stop] ->    
	    #sdp_t{start=list_to_integer(Start), 
		   stop=list_to_integer(Stop)};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;	
parse_propertyparm(#'PropertyParm'{name="r", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	[Repeat, Duration | ListOfOffsets] ->    
	   #sdp_r{repeat_interval=Repeat,
		  active_duration=Duration, 
		  list_of_offsets=ListOfOffsets};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;	
parse_propertyparm(#'PropertyParm'{name="z", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, " \t") of
	List ->    
	    #sdp_z{list_of_adjustments=List};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;	
parse_propertyparm(#'PropertyParm'{name="k", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    case string:tokens(Val, ":") of
	[Method, EncryptionKey] ->    
	    #sdp_k{method=Method, 
		   encryption_key=EncryptionKey};
	_ ->
	    exit("Wrong Format on PropertyParm")
    end;	

parse_propertyparm(#'PropertyParm'{name="s", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    #sdp_s{name=Val};
parse_propertyparm(#'PropertyParm'{name="i", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    #sdp_i{session_descriptor=Val};
parse_propertyparm(#'PropertyParm'{name="u", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    #sdp_u{uri=Val};
parse_propertyparm(#'PropertyParm'{name="e", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    #sdp_e{email=Val};
parse_propertyparm(#'PropertyParm'{name="p", value=[Val], extraInfo=EI}) when EI == asn1_NOVALUE  ->
    #sdp_p{phone_number=Val};
parse_propertyparm(PropertyParm) ->
    PropertyParm.

%%-----------------------------------------------------------------
%% Generate PropertyParm records from sdp records
%%-----------------------------------------------------------------

gen_LRDesc(Descr) when record(Descr,'LocalRemoteDescriptor') ->
    DecodedPropGroups = gen_propertygroup_list(Descr#'LocalRemoteDescriptor'.propGrps),
    Descr#'LocalRemoteDescriptor'{propGrps = DecodedPropGroups}.

gen_propertygroup_list([]) ->
    [];
gen_propertygroup_list(asn1_NOVALUE) ->
    asn1_NOVALUE;
gen_propertygroup_list([PropGrp |PropGrps]) ->
    case catch gen_propertygroup(PropGrp) of
	{'EXIT', _} ->
	    gen_propertygroup_list(PropGrps);
	NewPropGrp ->
	    [NewPropGrp | gen_propertygroup_list(PropGrps)]
    end.

gen_propertygroup([]) ->
    [];
gen_propertygroup([R |Rs]) ->
    [gen_propertyparm(R) | gen_propertygroup(Rs)].

gen_propertyparm(V) when record(V, sdp_v) ->
    #'PropertyParm'{name="v", 
		    value=[
			   V#sdp_v.version
			  ]};
gen_propertyparm(C) when record(C, sdp_c) ->
    #'PropertyParm'{name="c", 
		    value=[
			   C#sdp_c.network_type ++ " " ++
			   C#sdp_c.address_type ++ " " ++
			   C#sdp_c.connection_addr
			  ]};
gen_propertyparm(M) when record(M, sdp_m) ->
    NoOfPorts = case M#sdp_m.num_ports of
		    undefined ->
			"";
		    X ->
			"/" ++ integer_to_list(X) 
		end,
    #'PropertyParm'{name="m", 
		    value=[
			   M#sdp_m.media ++ " " ++
			   integer_to_list(M#sdp_m.port) ++
			   NoOfPorts ++" " ++
			   M#sdp_m.transport ++ 
			   lists:foldl(fun(Element, String) -> 
					       String ++ " " ++ Element 
				       end, 
				       [], 
				       M#sdp_m.fmt_list)
			  ]};
gen_propertyparm(O) when record(O, sdp_o) ->
    #'PropertyParm'{name="o", 
		    value=[
			   O#sdp_o.user_name ++ " " ++
			   O#sdp_o.session_id ++ " " ++
			   O#sdp_o.version ++ " " ++
			   O#sdp_o.network_type ++ " " ++
			   O#sdp_o.address_type ++ " " ++
			   O#sdp_o.address
			  ]};

gen_propertyparm(A) when record(A, sdp_a_rtpmap) ->
    #'PropertyParm'{name="a", 
		    value=["rtpmap:" ++
			   integer_to_list(A#sdp_a_rtpmap.payload_type) ++
			   " " ++
			   A#sdp_a_rtpmap.encoding_name ++ "/" ++
			   integer_to_list(A#sdp_a_rtpmap.clock_rate) ++ 
			   lists:foldl(fun(Element, String) -> 
					       String ++ "/" ++ Element 
				       end, 
				       [], 
				       A#sdp_a_rtpmap.encoding_parms)
			  ]};
gen_propertyparm(A) when record(A, sdp_a_ptime) ->
    #'PropertyParm'{name="a", 
		    value=["ptime:" ++
			   integer_to_list(A#sdp_a_ptime.packet_time)
			  ]};
gen_propertyparm(A) when record(A, sdp_a) ->
    Value = case A#sdp_a.value of
		undefined ->
		    [];
		Val -> 
		    ":" ++ Val
	    end,
    #'PropertyParm'{name="a", 
		    value=[
			   A#sdp_a.attribute ++ Value
			  ]};
gen_propertyparm(B) when record(B, sdp_b) ->
   #'PropertyParm'{name="b", 
			     value=[
				    B#sdp_b.modifier ++ ":" ++
				    integer_to_list(B#sdp_b.bandwidth)
				    ]};
gen_propertyparm(T) when record(T, sdp_t) ->
    #'PropertyParm'{name="t", 
		    value=[
			   integer_to_list(T#sdp_t.start) ++ 
			   " " ++
			   integer_to_list(T#sdp_t.stop)
			  ]};
gen_propertyparm(R) when record(R, sdp_r) ->
    #'PropertyParm'{name="r", 
		    value=[
			   R#sdp_r.repeat_interval ++ " " ++
			   R#sdp_r.active_duration ++ 
			   lists:foldl(fun(Element, String) -> 
					       String ++ " " ++ 
						   Element 
				       end, 
				       [], 
				       R#sdp_r.list_of_offsets)
				   ]};
gen_propertyparm(Z) when record(Z, sdp_z) ->
    [First | Rest] = Z#sdp_z.list_of_adjustments,
    #'PropertyParm'{name="z", 
		    value=[
			   lists:foldl(fun(Element, String) -> 
					       String ++ " " ++ 
						   Element 
				       end, 
				       First, 
				       Rest)
			  ]};
gen_propertyparm(K) when record(K, sdp_k) ->
    #'PropertyParm'{name="k", 
		    value=[
			   K#sdp_k.method ++ ":" ++
			   K#sdp_k.encryption_key
			  ]};

gen_propertyparm(S) when record(S, sdp_s) ->
    #'PropertyParm'{name="s", 
		    value=[
			   S#sdp_s.name
			  ]};
gen_propertyparm(I) when record(I, sdp_i) ->
    #'PropertyParm'{name="i", 
		    value=[
			   I#sdp_i.session_descriptor
			  ]};

gen_propertyparm(U) when record(U, sdp_u) ->
    #'PropertyParm'{name="u", 
		    value=[
			   U#sdp_u.uri
			  ]};

gen_propertyparm(E) when record(E, sdp_e) ->
    #'PropertyParm'{name="e", 
		    value=[
			   E#sdp_e.email
			  ]};

gen_propertyparm(P) when record(P, sdp_p) ->
    #'PropertyParm'{name="p", 
		    value=[
			   P#sdp_p.phone_number
			  ]};
gen_propertyparm(PP) ->
    PP.

%%-----------------------------------------------------------------
%% Func: get_sdp_record_from_group/2
%% Description: Get all sdp records of a certain typoe from a 
%%              property group
%%-----------------------------------------------------------------
get_sdp_record_from_group(_, []) ->
    [];
get_sdp_record_from_group(v, [R |Rs]) when record(R, sdp_v) ->
    [R |get_sdp_record_from_group(v, Rs)];
get_sdp_record_from_group(c, [R |Rs]) when record(R, sdp_c) ->
    [R |get_sdp_record_from_group(c, Rs)];
get_sdp_record_from_group(m, [R |Rs]) when record(R, sdp_m) ->
    [R |get_sdp_record_from_group(m, Rs)];
get_sdp_record_from_group(o, [R |Rs]) when record(R, sdp_o) ->
    [R |get_sdp_record_from_group(o, Rs)];
get_sdp_record_from_group(a, [R |Rs]) when record(R, sdp_a) ->
    [R |get_sdp_record_from_group(a, Rs)];
get_sdp_record_from_group(a, [R |Rs]) when record(R, sdp_a_ptime) ->
    [R |get_sdp_record_from_group(a, Rs)];
get_sdp_record_from_group(a, [R |Rs]) when record(R, sdp_a_rtpmap) ->
    [R |get_sdp_record_from_group(a, Rs)];
get_sdp_record_from_group(b, [R |Rs]) when record(R, sdp_b) ->
    [R |get_sdp_record_from_group(b, Rs)];
get_sdp_record_from_group(t, [R |Rs]) when record(R, sdp_t) ->
    [R |get_sdp_record_from_group(t, Rs)];
get_sdp_record_from_group(r, [R |Rs]) when record(R, sdp_r) ->
    [R |get_sdp_record_from_group(r, Rs)];
get_sdp_record_from_group(z, [R |Rs]) when record(R, sdp_z) ->
    [R |get_sdp_record_from_group(z, Rs)];
get_sdp_record_from_group(k, [R |Rs]) when record(R, sdp_k) ->
    [R |get_sdp_record_from_group(k, Rs)];
get_sdp_record_from_group(s, [R |Rs]) when record(R, sdp_s) ->
    [R |get_sdp_record_from_group(s, Rs)];
get_sdp_record_from_group(i, [R |Rs]) when record(R, sdp_i) ->
    [R |get_sdp_record_from_group(i, Rs)];
get_sdp_record_from_group(u, [R |Rs]) when record(R, sdp_u) ->
    [R |get_sdp_record_from_group(u, Rs)];
get_sdp_record_from_group(e, [R |Rs]) when record(R, sdp_e) ->
    [R |get_sdp_record_from_group(e, Rs)];
get_sdp_record_from_group(p, [R |Rs]) when record(R, sdp_p) ->
    [R |get_sdp_record_from_group(p, Rs)];
get_sdp_record_from_group(RecordType, [_R |Rs]) ->
    get_sdp_record_from_group(RecordType, Rs).

    
%%======================================================================
%% Internal functions
%%======================================================================

    
