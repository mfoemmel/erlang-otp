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
-define(GIOP_MORE_FRAGMENTS_TRUE, 1).

%% PROFILE_ID's
-define(TAG_INTERNET_IOP, 0).
-define(TAG_MULTIPPLE_COMPONENTS, 1).


%% COMPONENT_ID's
-define(TAG_SSL_SEC_TRANS, 20).

%% GIOP header size
-define(GIOP_HEADER_SIZE, 12).


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
%% Flags below should be changed :-) At the moment not used. When updated
%% it is necessary to update ../test/corba_SUITE.erl
-define(ORB_SOME,           16#04).
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
-define(IOP_TAGGEDCOMPONENT, {'tk_sequence', {'tk_struct', 0,
							       'IOP_TaggedComponent',
							       [{"tag", 'tk_ulong'},
								{"component_data",
								 {'tk_sequence',
								  'tk_octet', 0}}]},
							       0}).

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
			       {"components", ?IOP_TAGGEDCOMPONENT}]}).
			       
-define(SSLIOP_SSL, {'tk_struct', 0, 'SSLIOP_SSL',
				 [{"target_supports", 'tk_ushort'},
				  {"target_requires", 'tk_ushort'},
				  {"port", 'tk_ushort'}]}).
-endif.
