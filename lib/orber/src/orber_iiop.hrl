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
%%----------------------------------------------------------------------
%% File: orber_iiop.hrl
%% Author: Lars Thorsen
%% 
%% Creation date: 970115
%%
%%----------------------------------------------------------------------
-ifndef(orber_iiop_hrl).
-define(orber_iiop_hrl, true).

%% The identifiers which indicates if a fixed value has a negative or
%% positive scale.
-define(FIXED_NEGATIVE, 13).
-define(FIXED_POSITIVE, 12).

%% Defines if the ip_comm application is used instead of sockets.
-define(IIOP_SOCKET_MOD, 'orber_socket').

%% Major version of GIOP protocol which are supported
-define(GIOP_MAJOR, 1).

%% Minor version of GIOP protocol which are supported
-define(GIOP_MINOR, 0).

%% Major version of IIOP protocol which are supported
-define(IIOP_MAJOR, 1).

%% Minor version of IIOP protocol which are supported
-define(IIOP_MINOR, 0).

%% Fragment flags for the flags bitfield in GIOP message headers
-define(GIOP_BYTE_ORDER_MSB, 0).
-define(GIOP_BYTE_ORDER_LSB, 1).

%% Fragment flags for the flags bitfield in GIOP message headers
-define(GIOP_MORE_FRAGMENTS_FALSE, 0).
-define(GIOP_MORE_FRAGMENTS_TRUE,  1).

%% PROFILE_ID's
-define(TAG_INTERNET_IOP,        0).
-define(TAG_MULTIPLE_COMPONENTS, 1).
-define(TAG_SCCP_IOP,            2).


%% COMPONENT_ID's
-define(TAG_ORB_TYPE,                  0).
-define(TAG_CODE_SETS,                 1).
-define(TAG_POLICIES,                  2).
-define(TAG_ALTERNATE_IIOP_ADDRESS,    3).
-define(TAG_COMPLETE_OBJECT_KEY,       5).
-define(TAG_ENDPOINT_ID_POSITION,      6).
-define(TAG_LOCATION_POLICY,          12).
-define(TAG_ASSOCIATION_OPTIONS,      13).
-define(TAG_SEC_NAME,                 14).
-define(TAG_SPKM_1_SEC_MECH,          15).
-define(TAG_SPKM_2_SEC_MECH,          16).
-define(TAG_KerberosV5_SEC_MECH,      17).
-define(TAG_CSI_ECMA_Secret_SEC_MECH, 18).
-define(TAG_CSI_ECMA_Hybrid_SEC_MECH, 19).
-define(TAG_SSL_SEC_TRANS,            20).
-define(TAG_CSI_ECMA_Public_SEC_MECH, 21).
-define(TAG_GENERIC_SEC_MECH,         22).
-define(TAG_FIREWALL_TRANS,           23).
-define(TAG_SCCP_CONTACT_INFO,        24).
-define(TAG_JAVA_CODEBASE,            25).
-define(TAG_TRANSACTION_POLICY,       26).
-define(TAG_FT_GROUP,                 27).
-define(TAG_FT_PRIMARY,               28).
-define(TAG_FT_HEARTBEAT_ENABLED,     29).
-define(TAG_MESSAGE_ROUTERS,          30).
-define(TAG_OTS_POLICY,               31).
-define(TAG_INV_POLICY,               32).
-define(TAG_CSI_SEC_MECH_LIST,        33).
-define(TAG_NULL_TAG,                 34).
-define(TAG_SECIOP_SEC_TRANS,         35).
-define(TAG_TLS_SEC_TRANS,            36).
-define(TAG_DCE_STRING_BINDING,      100).
-define(TAG_DCE_BINDING_NAME,        101).
-define(TAG_DCE_NO_PIPES,            102).
-define(TAG_DCE_SEC_MECH,            103).
-define(TAG_INET_SEC_TRANS,          123).

%% COMPONENT_ID strings
-define(TAG_ORB_TYPE_STR,                  "TAG_ORB_TYPE").
-define(TAG_CODE_SETS_STR,                 "TAG_CODE_SETS").
-define(TAG_POLICIES_STR,                  "TAG_POLICIES").
-define(TAG_ALTERNATE_IIOP_ADDRESS_STR,    "TAG_ALTERNATE_IIOP_ADDRESS").
-define(TAG_COMPLETE_OBJECT_KEY_STR,       "TAG_COMPLETE_OBJECT_KEY").
-define(TAG_ENDPOINT_ID_POSITION_STR,      "TAG_ENDPOINT_ID_POSITION").
-define(TAG_LOCATION_POLICY_STR,           "TAG_LOCATION_POLICY").
-define(TAG_ASSOCIATION_OPTIONS_STR,       "TAG_ASSOCIATION_OPTIONS").
-define(TAG_SEC_NAME_STR,                  "TAG_SEC_NAME").
-define(TAG_SPKM_1_SEC_MECH_STR,           "TAG_SPKM_1_SEC_MECH").
-define(TAG_SPKM_2_SEC_MECH_STR,           "TAG_SPKM_2_SEC_MECH").
-define(TAG_KerberosV5_SEC_MECH_STR,       "TAG_KerberosV5_SEC_MECH").
-define(TAG_CSI_ECMA_Secret_SEC_MECH_STR,  "TAG_CSI_ECMA_Secret_SEC_MECH").
-define(TAG_CSI_ECMA_Hybrid_SEC_MECH_STR,  "TAG_CSI_ECMA_Hybrid_SEC_MECH").
-define(TAG_SSL_SEC_TRANS_STR,             "TAG_SSL_SEC_TRANS").
-define(TAG_CSI_ECMA_Public_SEC_MECH_STR,  "(TAG_CSI_ECMA_Public_SEC_MECH").
-define(TAG_GENERIC_SEC_MECH_STR,          "TAG_GENERIC_SEC_MECH").
-define(TAG_FIREWALL_TRANS_STR,            "TAG_FIREWALL_TRANS").
-define(TAG_SCCP_CONTACT_INFO_STR,         "TAG_SCCP_CONTACT_INFO").
-define(TAG_JAVA_CODEBASE_STR,             "TAG_JAVA_CODEBASE").
-define(TAG_TRANSACTION_POLICY_STR,        "TAG_TRANSACTION_POLICY").
-define(TAG_FT_GROUP_STR,                  "TAG_FT_GROUP").
-define(TAG_FT_PRIMARY_STR,                "TAG_FT_PRIMARY").
-define(TAG_FT_HEARTBEAT_ENABLED_STR,      "TAG_FT_HEARTBEAT_ENABLED").
-define(TAG_MESSAGE_ROUTERS_STR,           "TAG_MESSAGE_ROUTERS").
-define(TAG_OTS_POLICY_STR,                "TAG_OTS_POLICY").
-define(TAG_INV_POLICY_STR,                "TAG_INV_POLICY").
-define(TAG_CSI_SEC_MECH_LIST_STR,         "TAG_CSI_SEC_MECH_LIST").
-define(TAG_NULL_TAG_STR,                  "TAG_NULL_TAG").
-define(TAG_SECIOP_SEC_TRANS_STR,          "TAG_SECIOP_SEC_TRANS").
-define(TAG_TLS_SEC_TRANS_STR,             "TAG_TLS_SEC_TRANS").
-define(TAG_DCE_STRING_BINDING_STR,        "TAG_DCE_STRING_BINDING").
-define(TAG_DCE_BINDING_NAME_STR,          "TAG_DCE_BINDING_NAME").
-define(TAG_DCE_NO_PIPES_STR,              "TAG_DCE_NO_PIPES").
-define(TAG_DCE_SEC_MECH_STR,              "TAG_DCE_SEC_MECH").
-define(TAG_INET_SEC_TRANS_STR,            "TAG_INET_SEC_TRANS").

%% GIOP header size
-define(GIOP_HEADER_SIZE, 12).

%% CODESET's we support.
%% Latin-1. This CodeSet is default if no information exists in the IOR.
-define(ISO8859_1_ID, 16#00010001).

%% UTF-16, UCS Transformation Format 16-bit form
-define(UTF_16_ID, 16#00010109).

%% X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
-define(UTF_8_ID, 16#05010001).

%% The limited UTF-16 without the surrogate mechanism is called UCS-2.
%% The two-byte subset which is identical with the original Unicode. 
%% UCS-2, Level 1. Used by JDK-1.3 as native wchar.
-define(UCS_2_ID, 16#00010100).

%% ISO 646:1991 IRV (International Reference Version).
%% Used by JavaIDL as Native Char (JDK-1.3). A.k.a PCS.
-define(ISO646_IRV_ID, 16#00010020).

%% Fallback is *not* the same thing as default!!
-define(FALLBACK_CHAR,  16#05010001).
-define(FALLBACK_WCHAR, 16#00010109).

%% This is used when the wchar codeset is unknown.
-define(UNSUPPORTED_WCHAR, 0).

%% Integer limits
-define(SHORTMIN, -32768).
-define(SHORTMAX, 32767).
-define(USHORTMIN, 0).
-define(USHORTMAX, 65535).
-define(LONGMIN, -2147483648).
-define(LONGMAX, 2147483647).
-define(ULONGMIN, 0).
-define(ULONGMAX, 4294967295).
-define(LONGLONGMIN, -9223372036854775808).
-define(LONGLONGMAX, 9223372036854775807).
-define(ULONGLONGMIN, 0).
-define(ULONGLONGMAX, 18446744073709551615).

%% Orber OMG assigned TAG's
%% Service Cxt IDs 0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
%% Component IDs   0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
%% ORB type IDs    0x45524904 - 0x45524907  ("ERI\x04" - "ERI\x07")
-define(ORBER_ORB_TYPE_1, 16#45524904).
-define(ORBER_ORB_TYPE_2, 16#45524905).
-define(ORBER_ORB_TYPE_3, 16#45524906).
-define(ORBER_ORB_TYPE_4, 16#45524907).

-define(ORBER_COMPONENT_1, 16#45524904).
-define(ORBER_COMPONENT_2, 16#45524905).
-define(ORBER_COMPONENT_3, 16#45524906).
-define(ORBER_COMPONENT_4, 16#45524907).

-define(ORBER_SERVICE_CTX_1, 16#45524904).
-define(ORBER_SERVICE_CTX_2, 16#45524905).
-define(ORBER_SERVICE_CTX_3, 16#45524906).
-define(ORBER_SERVICE_CTX_4, 16#45524907).

%%----------------------------------------------------------------------
%% GIOP Message Header
%% 
%% magic: identifies the GIOP message headers, array of four characters.
%% giop_version: contains the version number of the giop protocol being 
%%		used in the message.
%% byte_order: indicating the byte order being used in subsequent 
%%		elements of the message.
%%	0 - big-endian byte ordering, 1 - little-endian byte ordering
%% message_type: indicating the type of the message 
%% message_size: gives the length of the message following the message 
%%		headerin octets.
%%----------------------------------------------------------------------
-record(giop_message, {magic, giop_version, byte_order, message_type, message_size, message}).



%%----------------------------------------------------------------------
%% Request Message Header
%%
%% service_context: contains ORB service data being passed from client to server.
%%	(IOP::ServiceContextList)
%% request_id: id used to assosciate reply messages with request messages.
%% response_expected: true if the request is expected to have a reply message.
%% object_key: identifies the object wich is the target of the invocation.
%% operation: contains the name of the operation being invoked.
%% requesting_principal: contains a value that identifying the requesting 
%%		principal.
%%----------------------------------------------------------------------
-record(request_header, {service_context, request_id, response_expected, object_key, operation, requesting_principal}).



%%----------------------------------------------------------------------
%% Reply Message Header
%%
%% service_context: contains ORB service data being passed from client to server.
%%	(IOP::ServiceContextList)
%% request_id: id used to assosciate reply messages with request messages.
%% reply_status: indicates the completion status of the request
%%----------------------------------------------------------------------
-record(reply_header, {service_context, request_id, reply_status}).



%%----------------------------------------------------------------------
%% Cancel Request Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%%----------------------------------------------------------------------
-record(cancel_request_header, {request_id}).



%%----------------------------------------------------------------------
%% Locate Request Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%% object_key: identifies the object being located (octet sequence).
%%----------------------------------------------------------------------
-record(locate_request_header, {request_id, object_key}).



%%----------------------------------------------------------------------
%% Locate Reply Message Header
%%
%% request_id: id used to assosciate reply messages with request messages.
%% locate_status: indicates the completion status of the locate request
%%----------------------------------------------------------------------
-record(locate_reply_header, {request_id, locate_status}).



%%----------------------------------------------------------------------
%% Profile Body
%%
%% iiop_version: describes the version of IIOP that the agent at the 
%%		specified adress is prepared to receive.
%% host: identifies the internet host to which the GIOP messages
%%		for the specified object may be sent.
%% port: contains the TCP?IP port number where the target agnet is listening
%%		for connection requests.
%% object_key: is an opaque value supplied by the agent producing the IOR.
%%----------------------------------------------------------------------
-record(profile_body, {iiop_version,host,port,object_key}).

%%----------------------------------------------------------------------
%% Version
%% 
%% major: major version number of iiop protocol 
%% minor: minor version number of iiop protocol. 
%%
%% When an agnet generates profiles specifying a particular version,
%% it must be able to accept messages complying with the specified 
%% version or any porevious minor version.
%%----------------------------------------------------------------------
-record(version, {major,minor}).


%%----------------------------------------------------------------------
%% ORB_FLAGS macros. Used in {_,_,_,_,{Key, Flags},_}/6
%% 
%%----------------------------------------------------------------------

%% Definition of flag positions:
-define(ORB_SEC_ATTRIBUTES, 16#01).
-define(ORB_CONTEXT,        16#02).
-define(ORB_TYPECHECK,      16#04).

%% Flags below should be changed :-) At the moment not used. When updated
%% it is necessary to update ../test/corba_SUITE.erl
-define(ORB_THING,          16#08).
-define(ORB_ELSE1,          16#10).
-define(ORB_ELSE2,          16#20).
-define(ORB_ELSE3,          16#40).
-define(ORB_ELSE,           16#80).

-define(ORB_INIT_FLAGS, 16#00).


%% Definition of flag operations:
%% USAGE: Boolean = ?ORB_FLAG_TEST(Flags, ?ORB_SEC_ATTRIBUTES)
-define(ORB_FLAG_TEST(_F1, _I1),   ((_F1 band _I1) == _I1)).

%% USAGE: NewFlags = ?ORB_SET_TRUE(Flags, ?ORB_CONTEXT)
-define(ORB_SET_TRUE(_F2, _I2),    (_I2 bor _F2)).

%% USAGE: NewFlags = ?ORB_SET_FALSE(Flags, ?ORB_CONTEXT)
-define(ORB_SET_FALSE(_F3, _I3),   ((_I3 bxor 16#ff) band _F3)).

%% USAGE: NewFlags = ?ORB_SET_FALSE_LIST(Flags, [?ORB_SEC_ATTRIBUTES, ?ORB_SOME])
-define(ORB_SET_FALSE_LIST(_F4, _IList1),
	lists:foldl(fun(_I4, _F5) ->
			    ((_I4 bxor 16#ff) band _F5)
		    end, 
		    _F4, _IList1)).

%% USAGE: NewFlags = ?ORB_SET_TRUE_LIST(Flags, [?ORB_SEC_ATTRIBUTES, ?ORB_SOME])
-define(ORB_SET_TRUE_LIST(_F6, _IList2),
	lists:foldl(fun(_I6, _F7) ->
			    (_I6 bor _F7)
		    end, 
		    _F6, _IList2)).

%% USAGE: Boolean = ?ORB_FLAG_TEST_LIST(Flags, [?ORB_CONTEXT, ?ORB_THING])
-define(ORB_FLAG_TEST_LIST(_F8, _IList3),
	lists:all(fun(_I7) ->
			  ((_F8 band _I7) == _I7)
		  end,
		  _IList3)).

%%----------------------------------------------------------------------
%% IOR
%% 
%%----------------------------------------------------------------------
-record('IOP_IOR', {type_id, profiles}).
-record('IOP_TaggedProfile', {tag, profile_data}).
-record('IIOP_ProfileBody_1_0', {iiop_version,
				 host,
				 port,
				 object_key}).
-record('IIOP_ProfileBody_1_1', {iiop_version,
				 host,
				 port,
				 object_key,
				 components}).

-record('IIOP_Version', {major, minor}).

-record('SSLIOP_SSL', {target_supports, target_requires, port}).

-record('IOP_TaggedComponent', {tag, component_data}).

-record('GIOP_TargetAddress', {label, value}).

-record('GIOP_IORAddressingInfo', {selected_profile_index, ior}).

%%
%% Nil object reference
%%

-define(ORBER_NIL_OBJREF, #'IOP_IOR' {type_id = "", profiles = []}).

-define(IOR_TYPEDEF, {'tk_struct', 0, 'IOP_IOR',
		      [{"type_id", {'tk_string', 0}},
		       {"profiles", {'tk_sequence', {'tk_struct', 0,
					'IOP_TaggedProfile',
					[{"tag", 'tk_ulong'},
					 {"profile_data",
					  {'tk_sequence', 'tk_octet', 0}}]}, 0}}]}).

-define(IIOP_VERSION, {'tk_struct', 0, 'IIOP_Version',
				 [{"major vsn", 'tk_octet'},
				  {"minor vsn", 'tk_octet'}]}).
-define(IOP_TAGGEDCOMPONENT, {'tk_struct', 0,
			      'IOP_TaggedComponent',
			      [{"tag", 'tk_ulong'},
			       {"component_data",
				{'tk_sequence',
				 'tk_octet', 0}}]}).
-define(IOP_TAGGEDCOMPONENT_SEQ, {'tk_sequence', ?IOP_TAGGEDCOMPONENT, 0}).

-define(PROFILEBODY_1_0_TYPEDEF, {'tk_struct', 0, 'IIOP_ProfileBody_1_0',
			      [{"iiop_version", ?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}}]}).
			       
-define(PROFILEBODY_1_1_TYPEDEF, {'tk_struct', 0, 'IIOP_ProfileBody_1_1',
			      [{"iiop_version",?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}}
			       {"components", ?IOP_TAGGEDCOMPONENT_SEQ}]}).

-define(PROFILEBODY_1_2_TYPEDEF, {'tk_struct', 0, 'IIOP_ProfileBody_1_1',
			      [{"iiop_version",?IIOP_VERSION },
			       {"host", {'tk_string', 0}},
			       {"port", 'tk_ushort'},
			       {"object_key", {'tk_sequence', 'tk_octet', 0}}
			       {"components", ?IOP_TAGGEDCOMPONENT_SEQ}]}).
			       
-define(SSLIOP_SSL, {'tk_struct', 0, 'SSLIOP_SSL',
				 [{"target_supports", 'tk_ushort'},
				  {"target_requires", 'tk_ushort'},
				  {"port", 'tk_ushort'}]}).

-define(GIOP_KeyAddr, 0).
-define(GIOP_ProfileAddr, 1).
-define(GIOP_ReferenceAddr, 2).

-define(TARGETADDRESS, {'tk_union', 0, 'GIOP_TargetAddress', 'tk_short', -1,
			[{?GIOP_KeyAddr, "object_key", {'tk_sequence', 'tk_octet', 0}},
			 {?GIOP_ProfileAddr, "profile", {'tk_struct', 0,
					 'IOP_TaggedProfile',
					 [{"tag", 'tk_ulong'},
					  {"profile_data",
					   {'tk_sequence', 'tk_octet', 0}}]}},
			 {?GIOP_ReferenceAddr, "ior", {'tk_struct', 0, 
				     'GIOP_IORAddressingInfo',
				     [{"selected_profile_index", 'tk_ulong'},
				      {"ior", ?IOR_TYPEDEF}]}}]}).

% Zero or more instances of the TAG_ALTERNATE_IIOP_ADDRESS component type
% may be included in a version 1.2 TAG_INTERNET_IOP Profile.
-record('ALTERNATE_IIOP_ADDRESS', {'HostID', 'Port'}).
-define(ALTERNATE_IIOP_ADDRESS, {'tk_struct', 0,
				 'ALTERNATE_IIOP_ADDRESS',
				 [{"HostID", {'tk_string', 0}},
				  {"Port", 'tk_ushort'}]}).
% The TAG_ORB_TYPE component can appear at most once in any IOR profile. For
% profiles supporting IIOP 1.1 or greater, it is optionally present.
-define(ORB_TYPE, 'tk_ulong').

-record('CONV_FRAME_CodeSetComponent', {native_code_set, conversion_code_sets}).
-record('CONV_FRAME_CodeSetComponentInfo', {'ForCharData', 'ForWcharData'}).
-define(CONV_FRAME_CODESETCOMPONENT, {'tk_struct', 0, 
				      'CONV_FRAME_CodeSetComponent',
				      [{"native_code_set", 'tk_ulong'},
				       {"conversion_code_sets", 
					{'tk_sequence', 'tk_ulong', 0}}]}).
-define(CONV_FRAME_CODESETCOMPONENTINFO, {'tk_struct', 0, 
					  'CONV_FRAME_CodeSetComponentInfo',
					  [{"ForCharData", 
					    ?CONV_FRAME_CODESETCOMPONENT},
					   {"ForWcharData", 
					    ?CONV_FRAME_CODESETCOMPONENT}]}).




-define(DEFAULT_FOR_CHAR,  #'CONV_FRAME_CodeSetComponent'{native_code_set=?ISO8859_1_ID, 
							  conversion_code_sets=[]}).
-define(DEFAULT_FOR_WCHAR, #'CONV_FRAME_CodeSetComponent'{native_code_set=?UTF_16_ID, 
							  conversion_code_sets=[]}).
-define(DEFAULT_CODESETS, 
	#'CONV_FRAME_CodeSetComponentInfo'{'ForCharData' = ?DEFAULT_FOR_CHAR, 
					   'ForWcharData' = ?DEFAULT_FOR_WCHAR}).
	  

%%-- ServiceContext ID's ------------
%% Describes what type of context included, i.e.,
%% typedef unsigned long ServiceId; 
%% struct ServiceContext { 
%%	  ServiceId context_id; 
%%	  sequence <octet> context_data; 
%%	 };

-record('IOP_ServiceContext', {context_id, context_data}).
-define(IOP_SERVICECONTEXT, {'tk_sequence',
			     {'tk_struct', 0, 'IOP_ServiceContext',
			      [{"context_id", 'tk_ulong'},
			       {"context_data",
				{'tk_sequence', 'tk_octet', 0}}]}, 0}).

-record('CONV_FRAME_CodeSetContext', {char_data, wchar_data}).
-define(CONV_FRAME_CODESETCONTEXT, {'tk_struct', 0, 'CONV_FRAME_CodeSetContext',
				    [{"char_data", 'tk_ulong'},
				     {"wchar_data", 'tk_ulong'}]}).

	
-define(IOP_TransactionService,        0).
-define(IOP_CodeSets,                  1).
-define(IOP_ChainBypassCheck,          2).
-define(IOP_ChainBypassInfo,           3).
-define(IOP_LogicalThreadId,           4).
-define(IOP_BI_DIR_IIOP,               5).
-define(IOP_SendingContextRunTime,     6).
-define(IOP_INVOCATION_POLICIES,       7).
-define(IOP_FORWARDED_IDENTITY,        8).
-define(IOP_UnknownExceptionInfo,      9).
-define(IOP_RTCorbaPriority,          10).
-define(IOP_RTCorbaPriorityRange,     11).
-define(IOP_FT_GROUP_VERSION,         12).
-define(IOP_FT_REQUEST,               13).
-define(IOP_ExceptionDetailMessage,   14).
-define(IOP_SecurityAttributeService, 15).

%%----------------------------------------------------------------------
%% host_data
%% 
%%----------------------------------------------------------------------
-record(host_data, {protocol = normal, ssl_data, version, 
		    charset = ?ISO8859_1_ID, wcharset = ?UTF_16_ID}).


-endif.
