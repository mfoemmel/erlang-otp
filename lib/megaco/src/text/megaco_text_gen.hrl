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
%% Purpose: Encode Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-define(META_ENC(Type, Item), Item) .
%% -define(META_ENC(Type, Item), megaco_meta_package:encode(text, Type, Item)).
%% -define(META_DEC(Type, Item), megaco_meta_package:decode(text, Type, Item)).

enc_MegacoMessage(Val) ->
    State = ?INIT_INDENT,
    enc_MegacoMessage(Val, State).

enc_MegacoMessage(Val, State)
  when record(Val, 'MegacoMessage') ->
    [
     ?LWSP,
     enc_AuthenticationHeader(Val#'MegacoMessage'.authHeader, State),
     enc_Message( Val#'MegacoMessage'.mess, State)
    ].

enc_Transaction(Val) ->
    State = ?INIT_INDENT,
    enc_Transaction(Val, State).

enc_CommandRequest(Val) ->
    State = ?INIT_INDENT,
    enc_CommandRequest(Val, State).

enc_ActionReply(Val) ->
    State = ?INIT_INDENT,
    enc_ActionReply(Val, State).

enc_AuthenticationHeader(asn1_NOVALUE, _State) ->
    [];
enc_AuthenticationHeader(Val, State)
  when record(Val, 'AuthenticationHeader') ->
    [
     ?AuthToken,
     ?EQUAL,
     enc_SecurityParmIndex(Val#'AuthenticationHeader'.secParmIndex, State),
     ?COLON,
     enc_SequenceNum(Val#'AuthenticationHeader'.seqNum, State),
     ?COLON,
     enc_AuthData(Val#'AuthenticationHeader'.ad, State),
     ?SEP_INDENT(State)
    ].

enc_SecurityParmIndex({'SecurityParmIndex',Val}, State) ->
    enc_SecurityParmIndex(Val, State);
enc_SecurityParmIndex(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 8, 8)
    ].

enc_SequenceNum({'SequenceNum',Val}, State) ->
    enc_SequenceNum(Val, State);
enc_SequenceNum(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 8, 8)
    ].

enc_AuthData({'AuthData',Val}, State) ->
    enc_AuthData(Val, State);
enc_AuthData(Val, State) ->
    [
     "0x",
     enc_HEXDIG(Val, State, 32, 64)
    ].

enc_Message(Val, State)
  when record(Val, 'Message') ->
    [
     ?MegacopToken,
     ?SLASH,
     enc_version(Val#'Message'.version, State),
     ?SEP,
     enc_MId(Val#'Message'.mId, State),
     ?SEP_INDENT(State),
     enc_Message_messageBody(Val#'Message'.messageBody, State)
    ].

enc_version(Val, State) when integer(Val), Val >= 0 ->
    enc_DIGIT(Val, State, 0, 99).

enc_Message_messageBody({'Message_messageBody',Val}, State) ->
    enc_Message_messageBody(Val, State);
enc_Message_messageBody({Tag, Val}, State) ->
    case Tag of
	messageError ->
	    enc_ErrorDescriptor(Val, State);
	transactions ->
	    enc_Message_messageBody_transactions(Val, State)
    end.

enc_Message_messageBody_transactions({'Message_messageBody_transactions',Val}, State) ->
    enc_Message_messageBody_transactions(Val, State);
enc_Message_messageBody_transactions(Val, State)
  when list(Val), Val /= []->
    [enc_Transaction(T, State) || T <- Val].

enc_MId({'MId',Val}, State) ->
    enc_MId(Val, State);
enc_MId({Tag, Val}, State) ->
     case Tag of
	 ip4Address ->
	     enc_IP4Address(Val, State);
	 ip6Address ->
	     enc_IP6Address(Val, State);
	 domainName ->
	     enc_DomainName(Val, State);
	 deviceName ->
	     enc_PathName(Val, State);
	 mtpAddress ->
	     enc_mtpAddress(Val, State)
     end.

enc_mtpAddress(Val, State) ->
    [
     ?MtpToken,
     ?LBRKT,
     enc_OCTET_STRING(Val, State, 2, 4),
     ?RBRKT
    ].

enc_DomainName(Val, State)
  when record(Val, 'DomainName') ->
    [
     $<,
     %% BUGBUG: (ALPHA / DIGIT) *63(ALPHA / DIGIT / "-" / ".")
     enc_STRING(Val#'DomainName'.name, State, 1, 64),
     $>,
     case Val#'DomainName'.portNumber of
	 asn1_NOVALUE ->
	     [];
	 PortNumber ->
	     [
	      $:,
	      enc_portNumber(PortNumber, State)
	     ]
     end
    ].

enc_IP4Address(Val, State)
  when record(Val, 'IP4Address') ->
    [A1, A2, A3, A4] = Val#'IP4Address'.address,
    [
     $[,
     enc_V4hex(A1, State),
     ?DOT,
     enc_V4hex(A2, State),
     ?DOT,
     enc_V4hex(A3, State),
     ?DOT,
     enc_V4hex(A4, State),    
     $],
     case Val#'IP4Address'.portNumber of
	 asn1_NOVALUE ->
	     [];
	 PortNumber ->
	     [
	      $:,
	      enc_portNumber(PortNumber, State)
	     ]
     end
    ].

enc_V4hex(Val, State) ->
    enc_DIGIT(Val, State, 0, 255).

enc_IP6Address(Val, State)
  when record(Val, 'IP6Address'),
       list(Val#'IP6Address'.address),
       length(Val#'IP6Address'.address) == 16 ->
    exit(ipv6_not_supported), %% BUGBUG: nyi
    [
     $[,
     Val#'IP6Address'.address,
     $],
     case Val#'IP6Address'.portNumber of
	 asn1_NOVALUE ->
	     [];
	 PortNumber ->
	     [
	      $:,
	      enc_portNumber(PortNumber, State)
	     ]
     end
    ].

enc_PathName({'PathName',Val}, State) ->
    enc_PathName(Val, State);
enc_PathName(Val, State) ->
    %% BUGBUG: ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
    %% BUGBUG: ["@" pathDomainName ]
    enc_STRING(Val, State, 1, 64).

enc_Transaction({'Transaction',Val}, State) ->
    enc_Transaction(Val, State);
enc_Transaction({Tag, Val}, State) ->
    case Tag of
	transactionRequest ->
	    enc_TransactionRequest(Val, State);
	transactionPending ->
	    enc_TransactionPending(Val, State);
	transactionReply ->
	    enc_TransactionReply(Val, State);
	transactionResponseAck ->
	    [enc_TransactionAck(T, State) || T <- Val]
    end.

enc_TransactionAck(Val, State)
  when record(Val, 'TransactionAck') ->
    [
     ?ResponseAckToken,
     ?LBRKT_INDENT(State),
     enc_TransactionId(Val#'TransactionAck'.firstAck, ?INC_INDENT(State)),
     case Val#'TransactionAck'.lastAck of
	 asn1_NOVALUE ->
	     [];
	 LastAck ->
	     enc_TransactionId(LastAck, State)
     end,
     ?RBRKT_INDENT(State)
    ].

enc_TransactionId({'TransactionId',Val}, State) ->
    enc_TransactionId(Val, State);
enc_TransactionId(Val, State) ->
    enc_UINT32(Val, State).

enc_TransactionRequest(Val, State)
  when record(Val, 'TransactionRequest') ->
    [
     ?TransToken,
     ?EQUAL,
     enc_TransactionId(Val#'TransactionRequest'.transactionId, State),
     ?LBRKT_INDENT(State),
     enc_TransactionRequest_actions(Val#'TransactionRequest'.actions,
				    ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_TransactionRequest_actions({'TransactionRequest_actions',Val}, State) ->
    enc_TransactionRequest_actions(Val, State);
enc_TransactionRequest_actions([Mand | Opt], State) ->
    [enc_ActionRequest(Mand, State) |
     [[?COMMA_INDENT(State), enc_ActionRequest(Val, State)] || Val <- Opt]].

enc_TransactionPending(Val, State)
  when record(Val, 'TransactionPending') ->
    [?PendingToken,
     ?EQUAL,
     enc_TransactionId(Val#'TransactionPending'.transactionId, State),
     ?LBRKT_INDENT(State),
     ?RBRKT_INDENT(State)
    ].

enc_TransactionReply(Val, State)
  when record(Val, 'TransactionReply') ->
    [
     ?ReplyToken,
     ?EQUAL,
     enc_TransactionId(Val#'TransactionReply'.transactionId, State),
     ?LBRKT_INDENT(State),
     enc_immAckRequired(Val#'TransactionReply'.immAckRequired, State),
     enc_TransactionReply_transactionResult(Val#'TransactionReply'.transactionResult,
					    ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_immAckRequired(Val, State) ->
    case Val of
	asn1_NOVALUE -> [];
	'NULL'       -> [?ImmAckRequiredToken, ?COMMA_INDENT(?INC_INDENT(State))]
    end.

enc_TransactionReply_transactionResult({'TransactionReply_transactionResult',Val}, State) ->
    enc_TransactionReply_transactionResult(Val, State);
enc_TransactionReply_transactionResult({Tag, Val}, State) ->
    case Tag of
	transactionError ->
	    enc_ErrorDescriptor(Val, State);
	 actionReplies ->
	     enc_TransactionReply_transactionResult_actionReplies(Val, State)
     end.

enc_TransactionReply_transactionResult_actionReplies({'TransactionReply_transactionResult_actionReplies',Val}, State) ->
    enc_TransactionReply_transactionResult_actionReplies(Val, State);
enc_TransactionReply_transactionResult_actionReplies([Mand | Opt], State) ->
    [enc_ActionReply(Mand, State),
     [[?COMMA_INDENT(State), enc_ActionReply(Val, State)] || Val <- Opt]].

enc_ErrorDescriptor(Val, State)
  when record(Val, 'ErrorDescriptor') ->
    [
     ?ErrorToken,
     ?EQUAL,
     enc_ErrorCode(Val#'ErrorDescriptor'.errorCode, State),
     ?LBRKT,
     case Val#'ErrorDescriptor'.errorText of
	 asn1_NOVALUE ->
	     [];
	 ErrorText ->
	     enc_ErrorText(ErrorText, State)
     end,
     ?RBRKT
    ].

enc_ErrorCode({'ErrorCode',Val}, State)->
    enc_ErrorCode(Val, State);
enc_ErrorCode(Val, State) ->
    enc_DIGIT(Val, State, 0, 999).

enc_ErrorText({'ErrorText',Val}, State) ->
    enc_ErrorText(Val, State);
enc_ErrorText(Val, State)  ->
    enc_QUOTED_STRING(Val, State).

enc_ContextID({'ContextID',Val}, State) ->
    enc_ContextID(Val, State);
enc_ContextID(Val, State) ->
    case Val of
	?megaco_all_context_id    -> $*;
	?megaco_null_context_id   -> $-;
	?megaco_choose_context_id -> $$;
	Int when integer(Int) -> enc_UINT32(Int, State)
    end.

enc_ActionRequest(Val, State)
  when record(Val, 'ActionRequest') ->
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(Val#'ActionRequest'.contextId, State),
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'ActionRequest'.contextAttrAuditReq],
		fun enc_ContextAttrAuditRequest/2}] ++
	      decompose_ContextRequest(Val#'ActionRequest'.contextRequest) ++
	      [{Val#'ActionRequest'.commandRequests,
		fun enc_CommandRequest/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_ActionReply(Val, State)
  when record(Val, 'ActionReply') ->
    [
     ?CtxToken,
     ?EQUAL,
     enc_ContextID(Val#'ActionReply'.contextId, State),
     ?LBRKT_INDENT(State),
     case Val#'ActionReply'.errorDescriptor of
	 asn1_NOVALUE ->
	     enc_list(decompose_ContextRequest(Val#'ActionReply'.contextReply) ++
		      [{Val#'ActionReply'.commandReply,
			fun enc_CommandReply/2}],
		      ?INC_INDENT(State));
	 ErrorDesc when Val#'ActionReply'.contextReply == asn1_NOVALUE,
			Val#'ActionReply'.commandReply == [] ->
	     enc_ErrorDescriptor(ErrorDesc, ?INC_INDENT(State))
     end,
     ?RBRKT_INDENT(State)
    ].

decompose_ContextRequest(asn1_NOVALUE) ->
    [{[], dummy}] ;
decompose_ContextRequest(Val)
  when record(Val, 'ContextRequest') ->
    OptPriority = 
	case Val#'ContextRequest'.priority of
	    asn1_NOVALUE -> {[], dummy};
	    Prio -> {[Prio], fun enc_priority/2}
	end,
    OptEmergency = 
	case Val#'ContextRequest'.emergency of
	    asn1_NOVALUE -> {[], dummy};
	    false -> {[], dummy};
	    true -> {[?EmergencyToken], fun(Elem, _) -> Elem end}
	end,
    OptTopologyReq = 
	case Val#'ContextRequest'.topologyReq of
	    asn1_NOVALUE ->
		{[], dummy};
	    {'ContextRequest_topologyReq', asn1_NOVALUE} ->
		{[], dummy};	    
	    {'ContextRequest_topologyReq', List} ->
		{List, fun enc_TopologyRequest/2};
	    List ->
		{[List], fun enc_TopologyRequest/2}
     end,
    [OptPriority, OptEmergency, OptTopologyReq].

enc_priority(Val, State) ->
    [
     ?PriorityToken,
     ?EQUAL,
     enc_UINT16(Val, State)
    ].

enc_ContextAttrAuditRequest(Val, State)
  when record(Val, 'ContextAttrAuditRequest') ->
    [
     ?ContextAuditToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'ContextAttrAuditRequest'.topology],
		fun('NULL', _) -> ?TopologyToken end},
	       {[Val#'ContextAttrAuditRequest'.emergency],
		fun('NULL', _) -> ?EmergencyToken end},
	       {[Val#'ContextAttrAuditRequest'.priority],
		fun('NULL', _) -> ?PriorityToken end}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_CommandRequest(Val, State)
  when record(Val, 'CommandRequest') ->
    [
     case Val#'CommandRequest'.optional of
	 asn1_NOVALUE ->
	     [];
	 'NULL' ->
	     "O-"
     end,
     case Val#'CommandRequest'.wildcardReturn of
	 asn1_NOVALUE ->
	     [];
	 'NULL' ->
	     "W-"
     end,
     enc_Command(Val#'CommandRequest'.command, State)
    ]. 

enc_Command({'Command',Val}, State) ->
    enc_Command(Val, State);
enc_Command({Tag, Val}, State) ->
    case Tag of
	addReq ->
	    [?AddToken, enc_AmmRequest(Val, State)];
	moveReq ->
	    [?MoveToken, enc_AmmRequest(Val, State)];
	modReq ->
	    [?ModifyToken, enc_AmmRequest(Val, State)];
	subtractReq ->
	    [?SubtractToken, enc_SubtractRequest(Val, State)];
	auditCapRequest ->
	    [?AuditCapToken, enc_AuditRequest(Val, State)];
	auditValueRequest ->
	    [?AuditValueToken, enc_AuditRequest(Val, State)];
	notifyReq ->
	    [?NotifyToken, enc_NotifyRequest(Val, State)];
	serviceChangeReq ->
	    [?ServiceChangeToken, enc_ServiceChangeRequest(Val, State)]
    end.

enc_CommandReply({'CommandReply',Val}, State) ->
    enc_CommandReply(Val, State);
enc_CommandReply({Tag, Val}, State) ->
     case Tag of
	 addReply ->
	     [?AddToken, enc_AmmsReply(Val, State)];
	 moveReply ->
	     [?MoveToken, enc_AmmsReply(Val, State)];
	 modReply ->
	     [?ModifyToken, enc_AmmsReply(Val, State)];
	 subtractReply ->
	     [?SubtractToken, enc_AmmsReply(Val, State)];
	 auditCapReply ->
	     [?AuditCapToken, enc_AuditReply(Val, State)];
	 auditValueReply ->
	     [?AuditValueToken, enc_AuditReply(Val, State)];
	 notifyReply ->
	     [?NotifyToken, enc_NotifyReply(Val, State)];
	 serviceChangeReply ->
	     [?ServiceChangeToken, enc_ServiceChangeReply(Val, State)]
     end.

enc_TopologyRequest(Val, State)
  when list(Val) ->
    [
     ?TopologyToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val, fun enc_TopologyRequest1/2}],?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_TopologyRequest1(Val, State)
  when record(Val, 'TopologyRequest') ->
    [
     fun(S) ->
	     [
	      enc_TerminationID(Val#'TopologyRequest'.terminationFrom, S),
	      ?COMMA_INDENT(S), 
	      enc_TerminationID(Val#'TopologyRequest'.terminationTo, S),
	      ?COMMA_INDENT(S),
	      case Val#'TopologyRequest'.topologyDirection of
		  bothway -> ?BothwayToken;
		  isolate -> ?IsolateToken;
		  oneway ->  ?OnewayToken
	      end
	     ]
     end(?INC_INDENT(State))
    ].

enc_AmmRequest(Val, State)
  when record(Val, 'AmmRequest') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1(Val#'AmmRequest'.terminationID, State),
     enc_opt_brackets(
       enc_list([{Val#'AmmRequest'.descriptors, fun enc_ammDescriptor/2}],
		?INC_INDENT(State)),
       State)
    ].

enc_ammDescriptor({Tag, Desc}, State) ->
    case Tag of
	mediaDescriptor       -> enc_MediaDescriptor(Desc, State);
        modemDescriptor       -> enc_ModemDescriptor(Desc, State);      
        muxDescriptor         -> enc_MuxDescriptor(Desc, State);   
        eventsDescriptor      -> enc_EventsDescriptor(Desc, State);      
        eventBufferDescriptor -> enc_EventBufferDescriptor(Desc, State); 
        signalsDescriptor     -> enc_SignalsDescriptor(Desc, State);    
        digitMapDescriptor    -> enc_DigitMapDescriptor(Desc, State);    
        auditDescriptor       -> enc_AuditDescriptor(Desc, State)
    end.

enc_AmmsReply(Val, State)
  when record(Val, 'AmmsReply') ->
    [
     ?EQUAL,
     enc_TerminationIDList1(Val#'AmmsReply'.terminationID, State),
     case Val#'AmmsReply'.terminationAudit of
	 asn1_NOVALUE ->
	     [];
	 [] ->
	     [];
	 TermAudit ->
	     [
	      ?LBRKT_INDENT(State) ,
	      enc_TerminationAudit(TermAudit, ?INC_INDENT(State)),
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

enc_SubtractRequest(Val, State)
  when record(Val, 'SubtractRequest') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1(Val#'SubtractRequest'.terminationID, State),
     case Val#'SubtractRequest'.auditDescriptor of
	 asn1_NOVALUE ->
	     [];
	 AuditDescr ->
	     [
	      ?LBRKT_INDENT(State) ,
	      enc_AuditDescriptor(AuditDescr, ?INC_INDENT(State)),
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].    

enc_AuditRequest(Val, State)
  when record(Val, 'AuditRequest') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1([Val#'AuditRequest'.terminationID], State),
     case Val#'AuditRequest'.auditDescriptor of
	 asn1_NOVALUE ->
	     [];
	 AuditDescr ->
	     [
	      ?LBRKT_INDENT(State) ,
	      enc_AuditDescriptor(AuditDescr, ?INC_INDENT(State)),
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].    

%% auditReply           = (AuditValueToken / AuditCapToken ) 
%% 			  ( contextTerminationAudit  / auditOther)
%% auditOther           = EQUAL TerminationID LBRKT 
%% 			  terminationAudit RBRKT
%% terminationAudit     = auditReturnParameter *(COMMA auditReturnParameter) 
%% 
%% contextTerminationAudit = EQUAL CtxToken ( terminationIDList / 
%% 			  LBRKT errorDescriptor RBRKT )
enc_AuditReply({Tag, Val}, State) ->
    case Tag of
	contextAuditResult ->
	    [
	     ?EQUAL,
	     ?CtxToken,
	     enc_TerminationIDListN(Val, State)
	    ];
	error ->
	    [
	     ?EQUAL,
	     ?CtxToken,
	     ?LBRKT_INDENT(State),
	     enc_ErrorDescriptor(Val, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]; 
	auditResult when record(Val, 'AuditResult') ->
	    %% auditOther
	    [
	     ?EQUAL,
	     enc_TerminationID(Val#'AuditResult'.terminationID, State),
	     ?LBRKT_INDENT(State),
	     enc_TerminationAudit(Val#'AuditResult'.terminationAuditResult, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]
    end.

enc_AuditDescriptor(Val, State)
  when record(Val, 'AuditDescriptor') ->
    [
     ?AuditToken,
     case Val#'AuditDescriptor'.auditToken of
	 asn1_NOVALUE ->
	     [?LBRKT, ?RBRKT];
	 List ->
	     [
	      ?LBRKT_INDENT(State),
	      enc_list([{List, fun enc_auditItem/2}], ?INC_INDENT(State)),
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

enc_auditItem(Val, _State) ->
    case Val of
	muxToken               -> ?MuxToken;
	modemToken             -> ?ModemToken;
	mediaToken             -> ?MediaToken;
	eventsToken            -> ?EventsToken;
	signalsToken           -> ?SignalsToken;
	digitMapToken          -> ?DigitMapToken;
	statsToken             -> ?StatsToken;
	observedEventsToken    -> ?ObservedEventsToken;
	packagesToken          -> ?PackagesToken;
	eventBufferToken       -> ?EventBufferToken
    end.

enc_TerminationAudit({'TerminationAudit',Val}, State) ->
    enc_TerminationAudit(Val, State);
enc_TerminationAudit([], _State) ->
    [];
enc_TerminationAudit([Mand | Opt], State) ->
  [enc_AuditReturnParameter(Mand, State),
   [[?COMMA_INDENT(State), enc_AuditReturnParameter(Val, State)] || Val <- Opt]].

enc_AuditReturnParameter({'AuditReturnParameter',Val}, State) ->
    enc_AuditReturnParameter(Val, State);
enc_AuditReturnParameter({Tag, Val}, State) ->
    case Tag of
	errorDescriptor ->
	    enc_ErrorDescriptor(Val, State);
	mediaDescriptor ->
	    enc_MediaDescriptor(Val, State);
	modemDescriptor ->
	    enc_ModemDescriptor(Val, State);
	muxDescriptor ->
	    enc_MuxDescriptor(Val, State);
	eventsDescriptor ->
	    enc_EventsDescriptor(Val, State);
	eventBufferDescriptor ->
	    enc_EventBufferDescriptor(Val, State);
	signalsDescriptor ->
	    enc_SignalsDescriptor(Val, State);
	digitMapDescriptor ->
	    enc_DigitMapDescriptor(Val, State);
	observedEventsDescriptor ->
	    enc_ObservedEventsDescriptor(Val, State);
	statisticsDescriptor ->
	    enc_StatisticsDescriptor(Val, State);
	packagesDescriptor ->
	    enc_PackagesDescriptor(Val, State);
        emptyDescriptors ->
            enc_EmptyDescriptors(Val, State)
    end.

enc_EmptyDescriptors(Val, State)
  when record(Val, 'AuditDescriptor') ->
    [
     case Val#'AuditDescriptor'.auditToken of
	 asn1_NOVALUE ->
	     [];
	 List ->
	     enc_list([{List, fun enc_auditItem/2}], ?INC_INDENT(State))
     end
    ].


enc_NotifyRequest(Val, State)
  when record(Val, 'NotifyRequest') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1(Val#'NotifyRequest'.terminationID, State),
     ?LBRKT_INDENT(State),
     %% BUGBUG: Mismatch between ASN.1 and ABNF
     %% BUGBUG: The following ought to be a 'choice'
     case Val#'NotifyRequest'.errorDescriptor of
	 asn1_NOVALUE ->
	     OED = Val#'NotifyRequest'.observedEventsDescriptor,
	     enc_ObservedEventsDescriptor(OED, ?INC_INDENT(State));
	 ErrorDescr ->
	     enc_ErrorDescriptor(ErrorDescr, ?INC_INDENT(State))
     end,
     ?RBRKT_INDENT(State)
    ].

enc_NotifyReply(Val, State)
  when record(Val, 'NotifyReply') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     case Val#'NotifyReply'.terminationID of
	 asn1_NOVALUE ->
	     exit(asn1_not_compliant_with_abnf);
	 TermId ->
	     enc_TerminationIDList1(TermId, State)
     end,
     case Val#'NotifyReply'.errorDescriptor of
	 asn1_NOVALUE ->
	     [];
	 ErrorDescr ->
	     [
	      ?LBRKT_INDENT(State),
	      enc_ErrorDescriptor(ErrorDescr, ?INC_INDENT(State)),
	      ?RBRKT_INDENT(State)
	     ]
     end
    ].

enc_ObservedEventsDescriptor(Val, State)
  when record(Val, 'ObservedEventsDescriptor') ->
    [
     ?ObservedEventsToken,
     ?EQUAL,
     enc_RequestID(Val#'ObservedEventsDescriptor'.requestId, State),
     ?LBRKT_INDENT(State),
     enc_observedEventsDescriptors(Val#'ObservedEventsDescriptor'.observedEventLst, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_observedEventsDescriptors([Mand | Opt], State) ->
    [enc_ObservedEvent(Mand, State),
     [[?COMMA_INDENT(State), enc_ObservedEvent(Val, State)] || Val <- Opt]].

%% ;time per event, because it might be buffered
%% observedEvent        = [ TimeStamp LWSP COLON] LWSP 
%% 			  pkgdName [ LBRKT observedEventParameter
%% 			  *(COMMA observedEventParameter) RBRKT ]
%% 
%% ;at-most-once eventStream, every eventParameterName at most once
%% observedEventParameter = eventStream / eventOther
enc_ObservedEvent(Val, State)
  when record(Val, 'ObservedEvent') ->
    [
     case Val#'ObservedEvent'.timeNotation of
	 asn1_NOVALUE ->
	     [];
	 TimeStamp ->
	     [
	      enc_TimeNotation(TimeStamp, State),
	      ?LWSP,
	      ?COLON
	     ]
     end,
     ?LWSP,
     enc_EventName(Val#'ObservedEvent'.eventName, State),
     enc_opt_brackets(
       enc_list([{[Val#'ObservedEvent'.streamID],   fun enc_eventStream/2},
		 {Val#'ObservedEvent'.eventParList, fun enc_eventOther/2}],
		?INC_INDENT(State)),
       State)
    ].

enc_EventName({'EventName',Val}, State) ->
    enc_EventName(Val, State);
enc_EventName(Val, State) ->
    PkgdName = ?META_ENC(event, Val),
    enc_PkgdName(PkgdName, State).

enc_eventStream(Val, State) ->
    [
     ?StreamToken,
     ?EQUAL,
     enc_StreamID(Val, State)
    ].

enc_eventOther(Val, State) 
  when record(Val, 'EventParameter') ->
    [
     enc_Name(Val#'EventParameter'.eventParameterName, State),
     enc_propertyParmValues(Val#'EventParameter'.value,
			    Val#'EventParameter'.extraInfo,
			    State)
    ].

enc_ServiceChangeRequest(Val, State)
  when record(Val, 'ServiceChangeRequest') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1(Val#'ServiceChangeRequest'.terminationID, State),
     ?LBRKT_INDENT(State),
     enc_ServiceChangeParm(Val#'ServiceChangeRequest'.serviceChangeParms,
			   ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

%% serviceChangeReply   = ServiceChangeToken EQUAL TerminationID
%% 			  [LBRKT (errorDescriptor / 
%% 			  serviceChangeReplyDescriptor) RBRKT]
%% serviceChangeReplyDescriptor = ServicesToken LBRKT
%% 			  servChgReplyParm *(COMMA servChgReplyParm) RBRKT
%% 
%% ;at-most-once. Version is REQUIRED on first ServiceChange response
%% servChgReplyParm     = (serviceChangeAddress / serviceChangeMgcId /
%% 			  serviceChangeProfile / serviceChangeVersion )
enc_ServiceChangeReply(Val, State)
  when record(Val, 'ServiceChangeReply') ->
    [
     %% Assume that Token is added elsewhere
     ?EQUAL,
     enc_TerminationIDList1(Val#'ServiceChangeReply'.terminationID, State),
     enc_ServiceChangeResult(Val#'ServiceChangeReply'.serviceChangeResult, State)
     ].

enc_ServiceChangeResult({'ServiceChangeResult',Val}, State) ->
    enc_ServiceChangeResult(Val, State);
enc_ServiceChangeResult({Tag, Val}, State) ->
    case Tag of
	errorDescriptor ->
	    [
	     ?LBRKT_INDENT(State),
	     enc_ErrorDescriptor(Val, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ];
	serviceChangeResParms ->
	     case enc_ServiceChangeResParm(Val, ?INC_INDENT(?INC_INDENT(State))) of
		 [] ->
		     [];
		 ResParms ->
		     [
		      ?LBRKT_INDENT(State),
		      ?ServicesToken,
		      fun(S) ->
			      [
			       ?LBRKT_INDENT(S),
			       ResParms,
			       ?RBRKT_INDENT(S),
			       ?RBRKT_INDENT(S)
			      ]
		      end(?INC_INDENT(State))
		     ]
	     end
     end.

%% Required length of termination ID list is 1
enc_TerminationIDList1({'TerminationIDList',Val}, State) ->
    enc_TerminationIDList1(Val, State);
enc_TerminationIDList1([Singleton], State) ->
    enc_TerminationID(Singleton, State).

%% No required length of termination ID list
enc_TerminationIDListN({'TerminationIDList',Val}, State) ->
    enc_TerminationIDListN(Val, State);
enc_TerminationIDListN(TidList, State) ->
    enc_list([{TidList, fun enc_TerminationID/2}], State).

%% TerminationID        = "ROOT" / pathNAME / "$" / "*"
%% ; Total length of pathNAME must not exceed 64 chars.
%% pathNAME             = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" ) 
%% 			  ["@" pathDomainName ]
enc_TerminationID(Tid, State)
  when record(Tid,  megaco_term_id) ->
    List = [{Tid#megaco_term_id.id, fun enc_tid_component/2 }],
    enc_list(List, State, fun(_S) -> ?SLASH end, false).    

enc_tid_component(Component, State) ->
    [enc_tid_sub_component(Sub, State) || Sub <- Component].

enc_tid_sub_component(Sub, _State) ->
    case Sub of
	all    -> ?megaco_all;
	choose -> ?megaco_choose;
	Char when integer(Char) -> Char
    end.

%% mediaDescriptor      = MediaToken LBRKT mediaParm *(COMMA mediaParm) RBRKT
%% ; at-most-once per item
%% ; and either streamParm or streamDescriptor but not both
%% mediaParm            = (streamParm / streamDescriptor / 
%% 			   terminationStateDescriptor)
%% ; at-most-once
%% streamParm           = ( localDescriptor / remoteDescriptor / 
%% 			   localControlDescriptor )
%% streamDescriptor     = StreamToken EQUAL StreamID LBRKT streamParm 
%% 			  *(COMMA streamParm) RBRKT
enc_MediaDescriptor(Val, State)
  when record(Val, 'MediaDescriptor') ->
    [
     ?MediaToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'MediaDescriptor'.termStateDescr],
		fun enc_TerminationStateDescriptor/2} |
	       decompose_streams(Val#'MediaDescriptor'.streams)],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

decompose_streams({'MediaDescriptor_streams',Val}) ->
    decompose_streams(Val);
decompose_streams({Tag, Val}) ->
    case Tag of
	oneStream ->
	    decompose_StreamParms(Val);
	multiStream ->
	    [{Val, fun enc_StreamDescriptor/2}]
    end.

decompose_StreamParms(Val)
  when record(Val, 'StreamParms') ->
    [
     {[Val#'StreamParms'.localControlDescriptor],
      fun enc_LocalControlDescriptor/2},
     {[Val#'StreamParms'.localDescriptor],
      fun enc_localDescriptor/2},
     {[Val#'StreamParms'.remoteDescriptor],
      fun enc_remoteDescriptor/2}
    ].

enc_StreamDescriptor(Val, State) 
    when record(Val, 'StreamDescriptor') ->
    [
     ?StreamToken,
     ?EQUAL,
     enc_StreamID(Val#'StreamDescriptor'.streamID, State),
     ?LBRKT_INDENT(State),
     enc_list(decompose_StreamParms(Val#'StreamDescriptor'.streamParms),
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

%% localControlDescriptor = LocalControlToken LBRKT localParm 
%% 			    *(COMMA localParm) RBRKT
%% 
%% ; at-most-once per item
%% localParm            = ( streamMode / propertyParm /
%%                          reservedValueMode  / reservedGroupMode ) 
%% reservedValueMode       = ReservedValueToken EQUAL ( "ON" / "OFF" ) 
%% reservedGroupMode       = ReservedGroupToken EQUAL ( "ON" / "OFF" ) 
%% 
%% reservedMode	     = ReservedToken EQUAL ( "ON" / "OFF" )
%% 
%% streamMode           = ModeToken EQUAL streamModes
enc_LocalControlDescriptor(Val, State)
  when record(Val, 'LocalControlDescriptor') ->
    [
     ?LocalControlToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'LocalControlDescriptor'.streamMode],
		fun enc_StreamMode/2},
	       {[Val#'LocalControlDescriptor'.reserveGroup],
		fun enc_reservedGroupMode/2},
	       {[Val#'LocalControlDescriptor'.reserveValue],
		fun enc_reservedValueMode/2},
	       {Val#'LocalControlDescriptor'.propertyParms,
		fun enc_PropertyParm/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_reservedGroupMode(Val, _State) ->
    [
     ?ReservedGroupToken,
     ?EQUAL,
     case Val of
	 false -> ?OffToken;
	 true  -> ?OnToken
     end
    ].

enc_reservedValueMode(Val, _State) ->
    [
     ?ReservedValueToken,
     ?EQUAL,
     case Val of
	 false -> ?OffToken;
	 true  -> ?OnToken
     end
    ].

enc_StreamMode({'StreamMode',Val}, State) ->
    enc_StreamMode(Val, State);
enc_StreamMode(Val, _State) ->
    [
     ?ModeToken,
     ?EQUAL,
     case Val of
	 sendOnly -> ?SendonlyToken;
	 recvOnly -> ?RecvonlyToken;
	 sendRecv -> ?SendrecvToken;
	 inactive -> ?InactiveToken;
	 loopBack -> ?LoopbackToken
     end
    ].

enc_Name({'Name',Val}, State) ->
    enc_Name(Val, State);
enc_Name(Val, State) ->
    %% BUGBUG: NAME = ALPHA *63(ALPHA / DIGIT / "_" )
    enc_STRING(Val, State, 1, 64).

enc_PkgdName({'PkgdName', Val}, State) ->
    enc_PkgdName(Val, State);
enc_PkgdName(Val, State) ->
    %% BUGBUG:  pkgdName =  (NAME / "*")  SLASH  (ItemID / "*" )
    enc_OCTET_STRING(Val, State, 1, 64).

enc_localDescriptor(Val, State) 
  when record(Val, 'LocalRemoteDescriptor') ->
    [
     ?LocalToken,
     ?LBRKT,
     enc_LocalRemoteDescriptor(Val, State),
     ?RBRKT_INDENT(State)
    ].

enc_remoteDescriptor(Val, State) 
  when record(Val, 'LocalRemoteDescriptor') ->
    [
     ?RemoteToken,
     ?LBRKT,
     enc_LocalRemoteDescriptor(Val, State),
     ?RBRKT_INDENT(State)
    ].

%% When text encoding the protocol, the descriptors consist of session
%% descriptions as defined in SDP (RFC2327), except that the "s=", "t="
%% and "o=" lines are optional. When multiple session descriptions are
%% provided in one descriptor, the "v=" lines are required as delimiters;
%% otherwise they are optional.  Implementations shall accept session
%% descriptions that are fully conformant to RFC2327. When binary
%% encoding the protocol the descriptor consists of groups of properties
%% (tag-value pairs) as specified in Annex C.  Each such group may
%% contain the parameters of a session description.
enc_LocalRemoteDescriptor(Val, State)
  when record(Val, 'LocalRemoteDescriptor') ->
    case Val#'LocalRemoteDescriptor'.propGrps of
	[] ->
	    [];
	[OptV | MandV] ->
	    [?LfToken,
	     enc_PropertyGroup(OptV, opt_v, State) |
	     [enc_PropertyGroup(M, mand_v, State) || M <- MandV]]
    end.

enc_PropertyGroup({'PropertyGroup',Val}, RequiresV, State) ->
    enc_PropertyGroup(Val, RequiresV, State);
enc_PropertyGroup([H | _T] = List, mand_v, State) when record(H, 'PropertyParm'), H#'PropertyParm'.name == "v" ->
    enc_PropertyGroup(List, opt_v, State);
enc_PropertyGroup(PG, opt_v, State) ->
    [
     [[enc_PropertyGroupParm(PP, State), ?LfToken] || PP <- PG]
    ].

enc_PropertyGroupParm(Val, State)
  when record(Val, 'PropertyParm') ->
    [OctetString] = Val#'PropertyParm'.value,
    [
     enc_PkgdName(Val#'PropertyParm'.name, State),
     ?EqualToken,
     enc_OCTET_STRING(OctetString, State, 0, infinity)
    ].

%% propertyParm         = pkgdName parmValue
%% parmValue            = (EQUAL alternativeValue/ INEQUAL VALUE)
%% alternativeValue     = ( VALUE / LSBRKT VALUE *(COMMA VALUE) RSBRKT  / 
%% 			  LSBRKT VALUE DOT DOT VALUE RSBRKT )
enc_PropertyParm(Val, State)
  when record(Val, 'PropertyParm') ->
    PkgdName = ?META_ENC(property, Val#'PropertyParm'.name),
    [
     enc_PkgdName(PkgdName, State),
     enc_propertyParmValues(Val#'PropertyParm'.value,
			    Val#'PropertyParm'.extraInfo,
			    State)
    ].
     
enc_propertyParmValues([Single], asn1_NOVALUE, State) ->
    [
     ?EqualToken,
     enc_Value(Single, State)
    ];
enc_propertyParmValues([Single], {relation, Rel}, State) ->
    case Rel of
	greaterThan -> [$>, enc_Value(Single, State)];
	smallerThan -> [$<, enc_Value(Single, State)];
	unequalTo   -> [$#, enc_Value(Single, State)]
    end;
enc_propertyParmValues([Low, High], {range, true}, State)->
    %% Exact two values
    [
     ?EqualToken,
     ?LSBRKT,
     enc_Value(Low, State),
     ?COLON,
     enc_Value(High, State),
     ?RSBRKT
    ];
enc_propertyParmValues(Values, {sublist, true}, State)->
    %% sublist (i.e. A AND B AND ...)
    [
     ?EqualToken,
     ?LSBRKT,
     enc_list([{Values, fun enc_Value/2}], State),
     ?RSBRKT
    ];
enc_propertyParmValues(Values, {sublist, false}, State) ->
    %% alternatives (i.e. A OR B OR ...)
    [
     ?EqualToken,
     ?LBRKT,
     enc_list([{Values, fun enc_Value/2}], State),
     ?RBRKT
    ].

enc_TerminationStateDescriptor(Val, State)
  when record(Val, 'TerminationStateDescriptor') ->
    [
     ?TerminationStateToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val#'TerminationStateDescriptor'.propertyParms,
		fun enc_PropertyParm/2},
	       {[Val#'TerminationStateDescriptor'.eventBufferControl],
		fun enc_eventBufferControl/2},
	       {[Val#'TerminationStateDescriptor'.serviceState],
		fun enc_serviceState/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_eventBufferControl(Val, _State) ->
    [

     ?BufferToken,
     ?EQUAL,  
     case Val of
	 off      -> ?OffToken;
	 lockStep -> ?LockStepToken
    end
    ].
    
enc_serviceState({'ServiceState',Val}, State) ->
    enc_serviceState(Val, State);
enc_serviceState(Val, _State) ->
    [
     ?ServiceStatesToken,
     ?EQUAL,
     case Val of
	 test     -> ?TestToken;
	 outOfSvc -> ?OutOfSvcToken;
	 inSvc    -> ?InSvcToken
     end
    ].

enc_MuxDescriptor(Val, State)
  when record(Val, 'MuxDescriptor') ->
    [
     ?MuxToken,
     ?EQUAL,
     enc_MuxType(Val#'MuxDescriptor'.muxType, State),
     enc_TerminationIDList1(Val#'MuxDescriptor'.termList, State)
    ].

enc_MuxType({'MuxType',Val}, State) ->
    enc_MuxType(Val, State);
enc_MuxType(Val, _State) ->
    case Val of
	h221 -> ?H221Token;
	h223 -> ?H223Token;
	h226 -> ?H226Token;
	v76  -> ?V76Token
    end.

enc_StreamID({'StreamID',Val}, State) ->
    enc_StreamID(Val, State);
enc_StreamID(Val, State) ->
    enc_UINT16(Val, State).

enc_EventsDescriptor(Val, State)
  when record(Val, 'EventsDescriptor') ->
    RequestId = Val#'EventsDescriptor'.requestID,
    Events = Val#'EventsDescriptor'.eventList,
    if
	%% BUGBUG: IG 6.82 introduces parse conflict
	%% RequestId == asn1_NOVALUE, Events == [] ->
	%%     [
	%% 	?EventsToken
	%%     ];
	RequestId /= asn1_NOVALUE, Events /= [] ->
	    [
	     ?EventsToken,
	     ?EQUAL,
	     enc_RequestID(RequestId, State),
	     ?LBRKT_INDENT(State),
	     enc_list([{Events, fun enc_RequestedEvent/2}],
		      ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]
    end.

enc_RequestedEvent(Val, State)
  when record(Val, 'RequestedEvent') ->
    PkgdName = ?META_ENC(event, Val#'RequestedEvent'.pkgdName),
    [
     enc_PkgdName(PkgdName, State),
     enc_opt_brackets(
       enc_list([{[Val#'RequestedEvent'.streamID],  fun enc_eventStream/2},
		 {Val#'RequestedEvent'.evParList, fun enc_eventOther/2} |
		 decompose_requestedActions(Val#'RequestedEvent'.eventAction)],
		?INC_INDENT(State)),
      State)
    ].

decompose_requestedActions(asn1_NOVALUE) ->
    [];
decompose_requestedActions(Val)
  when record(Val, 'RequestedActions') ->
    [
     {[Val#'RequestedActions'.keepActive],  fun enc_keepActive/2},
     {[Val#'RequestedActions'.eventDM],     fun enc_EventDM/2},
     {[Val#'RequestedActions'.secondEvent], fun enc_SecondEventsDescriptor/2},
     {[Val#'RequestedActions'.signalsDescriptor], fun enc_SignalsDescriptor/2}
    ].

enc_keepActive(Val, _State) ->
    case Val of
	true -> [?KeepActiveToken];
	false -> []
    end.
    
enc_EventDM({'EventDM',Val}, State) ->
    enc_EventDM(Val, State);
enc_EventDM({Tag, Val}, State) ->
    case Tag of
	digitMapName ->
	    [
	     ?DigitMapToken,
	     ?EQUAL,
	     enc_DigitMapName(Val, State)
	    ];
	digitMapValue ->
	    [
	     ?DigitMapToken,
	     ?LBRKT_INDENT(State),
	     enc_DigitMapValue(Val, ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]
    end.

enc_SecondEventsDescriptor(Val, State)
  when record(Val, 'SecondEventsDescriptor') ->
    RequestId = Val#'SecondEventsDescriptor'.requestID,
    Events = Val#'SecondEventsDescriptor'.eventList,
    if
	%% BUGBUG: IG 6.82 introduces parse conflict
	%% RequestId == asn1_NOVALUE, Events == [] ->
	%%     [
	%% 	?EmbedToken
	%%     ];
	RequestId /= asn1_NOVALUE, Events /= [] ->
	    [
	     ?EmbedToken,
	     ?LBRKT_INDENT(State),
	     enc_list([{Events,	fun(V, S) -> enc_embedFirst(V, S, RequestId) end}],
		      ?INC_INDENT(State)),
	     ?RBRKT_INDENT(State)
	    ]
    end.

enc_embedFirst(Val, State, RequestId)
  when record(Val, 'SecondRequestedEvent') ->
    [
     ?EventsToken,
     ?EQUAL,
     enc_RequestID(RequestId, State),
     ?LBRKT_INDENT(State),
     %% BUGBUG: Does this really work?
     enc_list([{[Val], fun enc_SecondRequestedEvent/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_SecondRequestedEvent(Val, State)
  when record(Val, 'SecondRequestedEvent') ->
    PkgdName = ?META_ENC(event, Val#'SecondRequestedEvent'.pkgdName),
    [
     enc_PkgdName(PkgdName, State),
     enc_opt_brackets(
       enc_list(
	 [{[Val#'SecondRequestedEvent'.streamID], fun enc_eventStream/2},
	  {Val#'SecondRequestedEvent'.evParList, fun enc_eventOther/2} |
	  decompose_secondRequestedActions(Val#'SecondRequestedEvent'.eventAction)],
	 ?INC_INDENT(State)),
       State)
    ].

decompose_secondRequestedActions(asn1_NOVALUE) ->
    [];
decompose_secondRequestedActions(Val)
  when record(Val, 'SecondRequestedActions') ->
    [
     {[Val#'SecondRequestedActions'.keepActive],
      fun enc_keepActive/2},
     {[Val#'SecondRequestedActions'.eventDM],
      fun enc_EventDM/2},
     {[Val#'SecondRequestedActions'.signalsDescriptor],
      fun enc_embeddedSignalsDescriptor/2}
    ].

enc_embeddedSignalsDescriptor(Val, State) ->
    [
     ?EmbedToken,
     ?LBRKT_INDENT(State),
     enc_SignalsDescriptor(Val, ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].
    
enc_EventBufferDescriptor({'EventBufferDescriptor',Val}, State) ->
    enc_EventBufferDescriptor(Val, State);
enc_EventBufferDescriptor([Mand | Opt], State) ->
    [
     ?EventBufferToken,
     ?LBRKT_INDENT(State),
     enc_eventSpecs([Mand | Opt], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)   
    ].

enc_eventSpecs([Mand | Opt], State) ->
    [enc_eventSpecs(Mand, State),
     [[?COMMA_INDENT(State), enc_eventSpec(Val, State)] || Val <- Opt]].

enc_eventSpec(Val, State)
  when record(Val, 'EventSpec') ->
    [
     enc_EventName(Val#'EventSpec'.eventName, State),
     enc_opt_brackets(
       enc_list([{[Val#'EventSpec'.streamID],   fun enc_eventStream/2},
		 {Val#'EventSpec'.eventParList, fun enc_eventOther/2}],
		?INC_INDENT(State)),
       State)
    ].

enc_SignalsDescriptor({'SignalsDescriptor',Val}, State) ->
    enc_SignalsDescriptor(Val, State);
enc_SignalsDescriptor(List, State) when list(List) ->
    [
     ?SignalsToken,
     ?LBRKT_INDENT(State),
     enc_list([{List, fun enc_SignalRequest/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_SignalRequest({'SignalRequest',Val}, State) ->
    enc_SignalRequest(Val, State);
enc_SignalRequest({Tag, Val}, State) ->
    case Tag of
	signal ->
	    enc_Signal(Val, State);
	seqSigList ->
	    enc_SeqSigList(Val, State)
    end.


enc_SeqSigList(Val, State)
  when record(Val, 'SeqSigList') ->
    [
     ?SignalListToken,
     ?EQUAL,
     enc_UINT16(Val#'SeqSigList'.id, State),
     ?LBRKT_INDENT(State),
     enc_list([{Val#'SeqSigList'.signalList, fun enc_Signal/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_Signal(Val, State)
  when record(Val, 'Signal') ->
    [
     enc_SignalName(Val#'Signal'.signalName, State),
     enc_opt_brackets(
       enc_list([{[Val#'Signal'.streamID],         fun enc_sigStream/2},
		 {[Val#'Signal'.sigType],          fun enc_sigSignalType/2},
		 {[Val#'Signal'.duration],         fun enc_sigDuration/2},
		 {[Val#'Signal'.notifyCompletion], fun enc_notifyCompletion/2},
		 {[Val#'Signal'.keepActive],       fun enc_keepActive/2},
		 {Val#'Signal'.sigParList,         fun enc_sigOther/2}],
		?INC_INDENT(State)),
      State)
    ].

enc_sigStream(Val, State) ->
    [
     ?StreamToken,
     ?EQUAL, 
     enc_StreamID(Val, State)
    ].

enc_sigSignalType(Val, State) ->
    [
     ?SignalTypeToken,
     ?EQUAL,
     enc_SignalType(Val, State)
    ].

enc_sigDuration(Val, State) ->
    [
     ?DurationToken,
     ?EQUAL,
     enc_UINT16(Val, State)
    ].

enc_notifyCompletion(List, State) when list(List) ->
    [
     ?NotifyCompletionToken,
     ?EQUAL,
     ?LBRKT_INDENT(State),
     enc_list([{List, fun enc_notifyCompletionItem/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_notifyCompletionItem(Val, _State) ->
    case Val of
	onTimeOut                   -> ?TimeOutToken;
        onInterruptByEvent          -> ?InterruptByEventToken;
        onInterruptByNewSignalDescr -> ?InterruptByNewSignalsDescrToken;
        otherReason                 -> ?OtherReasonToken
    end.

enc_SignalType({'SignalType',Val}, State) ->
    enc_SignalType(Val, State);
enc_SignalType(Val, _State) ->
    case Val of
	brief ->   ?BriefToken;
	onOff ->   ?OnOffToken;
	timeOut -> ?TimeOutToken
    end.

enc_SignalName({'SignalName',Val}, State)->
    enc_SignalName(Val, State);
enc_SignalName(Val, State) ->
    PkgdName = ?META_ENC(signal, Val),
    enc_PkgdName(PkgdName, State).

enc_sigOther(Val, State)
  when record(Val, 'SigParameter') ->
    [
     enc_Name(Val#'SigParameter'.sigParameterName, State),
     enc_propertyParmValues(Val#'SigParameter'.value,
			    Val#'SigParameter'.extraInfo,
			    State)
    ].

enc_RequestID({'RequestID',Val}, State) ->
    enc_RequestID(Val, State);
enc_RequestID(Val, _State) when Val == ?megaco_all_request_id ->
    "*";
enc_RequestID(Val, State) ->
    enc_UINT32(Val, State).

enc_ModemDescriptor(Val, State)
  when record(Val, 'ModemDescriptor') ->
    [
     ?ModemToken,
     %% BUGBUG: Does never generate: EQUAL modemType
     ?LSBRKT,
     enc_list([{Val#'ModemDescriptor'.mtl,fun enc_ModemType/2}], State),
     ?RSBRKT,
     enc_opt_brackets(
       enc_list([{Val#'ModemDescriptor'.mpl, fun enc_PropertyParm/2}],
		?INC_INDENT(State)),
       State)
     %% BUGBUG: Is PropertyParm == NAME parmValue?
    ].

enc_ModemType({'ModemType',Val}, State)->
    enc_ModemType(Val, State);
enc_ModemType(Val, _State) ->
    %% BUGBUG: Does not handle extensionParameter
    case Val of
        v18    	  -> ?V18Token;
        v22    	  -> ?V22Token;
        v22bis 	  -> ?V22bisToken;
        v32    	  -> ?V32Token;
        v32bis 	  -> ?V32bisToken;
        v34    	  -> ?V34Token;
        v90    	  -> ?V90Token;
        v91    	  -> ?V91Token;
        synchISDN -> ?SynchISDNToken
    end.

enc_DigitMapDescriptor(Val, State)
  when record(Val, 'DigitMapDescriptor') ->
    [
     ?DigitMapToken,
     ?EQUAL,
     enc_DigitMapName(Val#'DigitMapDescriptor'.digitMapName, State),
     ?LBRKT_INDENT(State),
     enc_DigitMapValue(Val#'DigitMapDescriptor'.digitMapValue,
		       ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_DigitMapName({'DigitMapName',Val}, State) ->
    enc_DigitMapName(Val, State);
enc_DigitMapName(Val, State) ->
    enc_Name(Val, State).

enc_DigitMapValue(Val, State)
  when record(Val, 'DigitMapValue') ->
    [
     enc_timer(Val#'DigitMapValue'.startTimer, $T, State),
     enc_timer(Val#'DigitMapValue'.shortTimer, $S, State),
     enc_timer(Val#'DigitMapValue'.longTimer,  $L, State),
     %% BUGBUG: digitMapBody not handled at all
     enc_STRING(Val#'DigitMapValue'.digitMapBody, State, 0, infinity)
    ].

enc_timer(asn1_NOVALUE, _Prefix, _State) ->
    [];
enc_timer(Timer, Prefix, State) ->
    [
     Prefix,
     ?COLON,
     enc_DIGIT(Timer, State, 0, 99),
     ?COMMA_INDENT(State)
    ].

enc_ServiceChangeParm(Val, State)
  when record(Val, 'ServiceChangeParm') ->
    [
     ?ServicesToken,
     ?LBRKT_INDENT(State),
     enc_list([{[Val#'ServiceChangeParm'.serviceChangeMethod],
		fun enc_ServiceChangeMethod/2},
	       {[Val#'ServiceChangeParm'.serviceChangeAddress],
		fun enc_ServiceChangeAddress/2},
	       {[Val#'ServiceChangeParm'.serviceChangeVersion],
		fun enc_serviceChangeVersion/2},
	       {[Val#'ServiceChangeParm'.serviceChangeProfile],
		fun enc_ServiceChangeProfile/2},
	       {[{reason, Val#'ServiceChangeParm'.serviceChangeReason}],
		fun enc_serviceChangeReason/2},
	       {[Val#'ServiceChangeParm'.serviceChangeDelay],
		fun enc_serviceChangeDelay/2},
	       {[Val#'ServiceChangeParm'.serviceChangeMgcId],
		fun enc_serviceChangeMgcId/2},
	       {[Val#'ServiceChangeParm'.timeStamp],
		fun enc_TimeNotation/2}],
	      ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_ServiceChangeMethod({'ServiceChangeMethod',Val}, State) ->
    enc_ServiceChangeMethod(Val, State);
enc_ServiceChangeMethod(Val, _State) ->
    [
     ?MethodToken,
     ?EQUAL,
     case Val of
        failover      -> ?FailoverToken;
        forced        -> ?ForcedToken;
        graceful      -> ?GracefulToken;
        restart       -> ?RestartToken;
        disconnected  -> ?DisconnectedToken;
        handOff       -> ?HandOffToken
     end
     %% BUGBUG: extension
    ].

enc_ServiceChangeAddress({'ServiceChangeAddress',Val}, State) ->
    enc_ServiceChangeAddress(Val, State);
enc_ServiceChangeAddress({Tag, Val}, State) ->
    [
     ?ServiceChangeAddressToken,
     ?EQUAL,
     case Tag of
	 portNumber ->
	     enc_portNumber(Val, State);
	 ip4Address ->
	     enc_IP4Address(Val, State);
	 ip6Address ->
	     enc_IP6Address(Val, State);
	 domainName ->
	     enc_DomainName(Val, State);
	 deviceName ->
	     enc_PathName(Val, State);
	 mtpAddress ->
	     enc_mtpAddress(Val, State)
     end
    ].

enc_serviceChangeVersion(Val, State) ->
    [
     ?VersionToken,
     ?EQUAL,
     enc_version(Val, State)
    ].

enc_ServiceChangeProfile(Val, State)
  when record(Val, 'ServiceChangeProfile') ->
    ProfName = ?META_ENC(profile, Val#'ServiceChangeProfile'.profileName),
    [
     ?ProfileToken,
     ?EQUAL,
     enc_Name(ProfName, State),
     ?SLASH,
     enc_version(Val#'ServiceChangeProfile'.version, State)
    ].

enc_serviceChangeReason({reason, Val}, State) ->
    case Val of
	asn1_NOVALUE ->
	    [];
	[List] when list(List) ->
	    [
	     ?ReasonToken,
	     ?EQUAL,
	     enc_QUOTED_STRING(List,State) % OTP-4632 enc_Value(List, State)
	    ]
    end.

enc_serviceChangeDelay(Val, State) ->
    [
     ?DelayToken,
     ?EQUAL,
     enc_UINT32(Val, State)
    ].

enc_serviceChangeMgcId(Val, State) ->
    [
     ?MgcIdToken,
     ?EQUAL,
     enc_MId(Val, State)
    ].

enc_portNumber(Val, State) when integer(Val), Val >= 0 ->
    enc_UINT16(Val, State).
     
enc_ServiceChangeResParm(Val, State)
  when record(Val, 'ServiceChangeResParm') ->
    enc_list([{[Val#'ServiceChangeResParm'.serviceChangeAddress],
	       fun enc_ServiceChangeAddress/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeVersion],
	       fun enc_serviceChangeVersion/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeProfile],
	       fun enc_ServiceChangeProfile/2},
	      {[Val#'ServiceChangeResParm'.serviceChangeMgcId],
	       fun enc_serviceChangeMgcId/2},
	      {[Val#'ServiceChangeResParm'.timeStamp],
	       fun enc_TimeNotation/2}],
	     State).

enc_PackagesDescriptor({'PackagesDescriptor',Val}, State) ->
    enc_PackagesDescriptor(Val, State);
enc_PackagesDescriptor(Val, State) ->
    [
     ?PackagesToken,
     ?LBRKT_INDENT(State),
     enc_list([{Val, fun enc_PackagesItem/2}], ?INC_INDENT(State)),  
     ?RBRKT_INDENT(State)    
    ].

enc_PackagesItem(Val, State)
  when record(Val, 'PackagesItem') ->
    PkgdName = ?META_ENC(package, Val#'PackagesItem'.packageName),
    [
     enc_Name(PkgdName, State),
     "-",
     enc_UINT16(Val#'PackagesItem'.packageVersion, State)
    ].

enc_StatisticsDescriptor({'StatisticsDescriptor',Val}, State) ->
    enc_StatisticsDescriptor(Val, State);
enc_StatisticsDescriptor(List, State) when list(List) ->
    [
     ?StatsToken,
     ?LBRKT_INDENT(State),
     enc_list([{List, fun enc_StatisticsParameter/2}], ?INC_INDENT(State)),
     ?RBRKT_INDENT(State)
    ].

enc_StatisticsParameter(Val, State)
  when record(Val, 'StatisticsParameter') ->
    PkgdName = ?META_ENC(statistics, Val#'StatisticsParameter'.statName),
    case Val#'StatisticsParameter'.statValue of
	asn1_NOVALUE ->
	    [
	     enc_PkgdName(PkgdName, State)
	    ];
	[StatVal] when list(StatVal) ->
	    [
	     enc_PkgdName(PkgdName, State),
	     ?EQUAL,
	     enc_Value(StatVal, State)
	    ]
    end.

enc_TimeNotation(Val, State)
  when record(Val, 'TimeNotation') ->
    [
     enc_STRING(Val#'TimeNotation'.date, State, 8, 8), % "yyyymmdd"
     "T",
     enc_STRING(Val#'TimeNotation'.time, State, 8, 8)  % "hhmmssss"
    ].

%% BUGBUG: Does not verify that string must contain at least one char
%% BUGBUG: This violation of the is required in order to comply with
%% BUGBUG: the dd/ce ds parameter that may possibly be empty.
enc_Value({'Value',Val}, State) ->
    enc_Value(Val, State);
enc_Value(String, _State) ->
    case quoted_string_count(String, 0, true) of
	{_, 0} ->
	    [?DQUOTE, String, ?DQUOTE];
	{false, _} ->
	    [?DQUOTE, String, ?DQUOTE];
	{true, _} ->
	    [String]
    end.
 
quoted_string_count([H | T], Count, IsSafe) ->
    case ?classify_char(H) of
	safe_char   -> quoted_string_count(T, Count + 1, IsSafe);
	rest_char   -> quoted_string_count(T, Count + 1, false);
	white_space -> quoted_string_count(T, Count + 1, false);
	_           -> exit({illegal_char, H})
    end;
quoted_string_count([], Count, IsSafe) ->
    {IsSafe, Count}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Encode an octet string, escape } by \ if necessary 
enc_OCTET_STRING(List, State, Min, Max) ->
    do_enc_OCTET_STRING(List, State, Min, Max, 0).
    
do_enc_OCTET_STRING([H | T], State, Min, Max, Count) ->
    case H of
	$} ->
	    [$\\, H | do_enc_OCTET_STRING(T, State, Min, Max, Count + 1)];
	_ ->
	    [H | do_enc_OCTET_STRING(T, State, Min, Max, Count + 1)]
    end;
do_enc_OCTET_STRING([], _State, Min, Max, Count) ->
    verify_count(Count, Min, Max),
    [].

enc_QUOTED_STRING(String, _State) when list(String) ->
    {_IsSafe, Count} = quoted_string_count(String, 0, true),
    verify_count(Count, 1, infinity),
    [?DQUOTE, String, ?DQUOTE].

%% The internal format of hex digits is a list of octets
%% Min and Max means #hexDigits
%% Leading zeros are prepended in order to fulfill Min
enc_HEXDIG(Octets, State, Min, Max) when list(Octets) ->
    do_enc_HEXDIG(Octets, State, Min, Max, 0, []).

do_enc_HEXDIG([Octet | Rest], State, Min, Max, Count, Acc) 
  when Octet >= 0, Octet =< 255  ->
    Acc2 = [hex(Octet) | Acc],
    if
	Octet =< 15 ->
	    do_enc_HEXDIG(Rest, State, Min, Max, Count + 2, ["0" | Acc2]);
	true -> 
	    do_enc_HEXDIG(Rest, State, Min, Max, Count + 2, Acc2)
    end;
do_enc_HEXDIG([], State, Min, Max, Count, Acc)
  when integer(Min), Count < Min ->
    do_enc_HEXDIG([0], State, Min, Max, Count, Acc);
do_enc_HEXDIG([], _State, Min, Max, Count, Acc)
  when integer(Min), Count < Min ->
    verify_count(Count, Min, Max),
    lists:reverse(Acc).

enc_DIGIT(Val, State, Min, Max) ->
    enc_integer(Val, State, Min, Max).

enc_STRING(String, _State, Min, Max) when list(String) ->
    verify_count(length(String), Min, Max),
    String.

enc_UINT16(Val, State) ->
    enc_integer(Val, State, 0, 65535).

enc_UINT32(Val, State) ->
    enc_integer(Val, State, 0, 4294967295).

enc_integer(Val, _State, Min, Max) ->
    verify_count(Val, Min, Max),
    integer_to_list(Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Encodes a list of elements with separator tokens between
%% the elements. Optional asn1_NOVALUE values are ignored.

enc_list(List, State) ->
    enc_list(List, State, fun(S) -> ?COMMA_INDENT(S) end, false).

enc_list([{Elems, ElemEncoder} | Tail], State, SepEncoder, NeedsSep) ->
    case do_enc_list(Elems, State, ElemEncoder, SepEncoder, NeedsSep) of
	[] ->
	    enc_list(Tail, State, SepEncoder, NeedsSep);
	List ->
	    [List,
	     enc_list(Tail, State, SepEncoder, true)]
    end;
enc_list([], _State, _SepEncoder, _NeedsSep) ->
    [];
enc_list(asn1_NOVALUE, _State, _SepEncoder, _NeedsSep) ->
    [].

do_enc_list(asn1_NOVALUE, _State, _ElemEncoder, _SepEncoder, _NeedsSep) ->
    [];
do_enc_list([], _State, _ElemEncoder, _SepEncoder, _NeedsSep) ->
    [];
do_enc_list([asn1_NOVALUE | T], State, ElemEncoder, SepEncoder, NeedsSep) ->
    do_enc_list(T, State, ElemEncoder, SepEncoder, NeedsSep);
do_enc_list([H | T], State, ElemEncoder, SepEncoder, NeedsSep)
  when function(ElemEncoder), function(SepEncoder) ->
    case ElemEncoder(H, State) of
	[] ->
	    do_enc_list(T, State, ElemEncoder, SepEncoder, NeedsSep);
	List when NeedsSep == true ->
	    [SepEncoder(State),
	     List, do_enc_list(T, State, ElemEncoder, SepEncoder, true)];
	List when NeedsSep == false ->
	    [List,
	     do_enc_list(T, State, ElemEncoder, SepEncoder, true)]
    end.

%% Add brackets if list is non-empty
enc_opt_brackets([], _State) ->
    [];
enc_opt_brackets(List, State) when list(List) ->
    [?LBRKT_INDENT(State), List, ?RBRKT_INDENT(State)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Int -> list of hex chars
hex(Int) ->
    hexi(get_lo_bits(Int, 4), []).

hexi({0, Lo}, Ack) ->
    [hex4(Lo) | Ack];
hexi({Hi, Lo} , Ack) ->
    hexi(get_lo_bits(Hi, 4), [hex4(Lo) | Ack]).

hex4(Int) when Int < 10 ->
    Int + $0;
hex4(Int) ->
    ($A - 10) + Int.

get_lo_bits(Int, Size) ->
    Lo = Int band ones_mask(Size),
    Hi = Int bsr Size,
    {Hi, Lo}.

ones_mask(Ones) ->
    (1 bsl Ones) - 1.

%% Verify that Count is within the range of Min and Max
verify_count(Count, Min, Max) ->
    if
	integer(Count) ->
	    if
		integer(Min), Count >= Min ->
		    if
			integer(Max), Count =< Max ->
			    Count;
			Max == infinity ->
			    Count;
			true ->
			    exit({count_too_large, Count, Max})
		    end;
		true ->
		    exit({count_too_small, Count, Min})
	    end;
	true ->
	    exit({count_not_an_integer, Count})
    end.



