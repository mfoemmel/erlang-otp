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
%% Purpose : Define semantic text parser actions
%%----------------------------------------------------------------------


-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_text_tokens.hrl").

make_safe_token({_TokenTag, Line, Text}) ->
    {safeToken, Line, Text}.

ensure_value({safeToken, _Line, Text}) ->
    ensure_value(Text);
ensure_value({'QuotedChars', _Line, Text}) ->
    ensure_value(Text);
ensure_value(Text) when list(Text) ->
    Text. %% BUGBUG: ensure length

%% NAME       = ALPHA *63(ALPHA / DIGIT / "_" )
ensure_NAME({_TokenTag, _Line, Text}) ->
    Text.  %% BUGBUG: ensure length and chars

ensure_requestID({safeToken, _Line, "*"}) ->
    ?megaco_all_request_id;
ensure_requestID(RequestId) ->
    ensure_uint32(RequestId).

ensure_streamID(StreamId) ->
    ensure_uint16(StreamId).

ensure_auth_header(SpiToken, SnToken, AdToken) ->
    Spi = ensure_hex(SpiToken, 8, 8),
    Sn  = ensure_hex(SnToken, 8, 8), 
    Ad  = ensure_hex(AdToken, 24, 64),
    #'AuthenticationHeader'{secParmIndex = Spi, seqNum = Sn, ad = Ad}.

%% ContextID         = (UINT32 / "*" / "-" / "$")
ensure_contextID({_TokenTag, _Line, Text}) ->
    case Text of
        "*"  -> ?megaco_all_context_id;
        "-"  -> ?megaco_null_context_id;
        "\$" -> ?megaco_choose_context_id;
        Int  -> ensure_uint32(Int)
    end.

ensure_domainAddress({TokenTag, Line, Addr}, Port) ->
    %% BUGBUG: handle IPv6
    case string:tokens(Addr, [$.]) of
        [T1, T2, T3, T4] ->
            A1 = ensure_uint({TokenTag, Line, T1}, 0, 255),
            A2 = ensure_uint({TokenTag, Line, T2}, 0, 255),
            A3 = ensure_uint({TokenTag, Line, T3}, 0, 255),
            A4 = ensure_uint({TokenTag, Line, T4}, 0, 255),
            {ip4Address, #'IP4Address'{address = [A1, A2, A3, A4],
                                       portNumber = Port}};
        _ ->
            return_error(Line, {bad_domainAddress, Addr})
    end.

ensure_domainName({_TokenTag, _Line, Name}, Port) ->
    %% BUGBUG: validate name
    {domainName, #'DomainName'{name = Name, portNumber = Port}}.

%% extensionParameter= "X"  ("-" / "+") 1*6(ALPHA / DIGIT)
ensure_extensionParameter({_TokenTag, Line, Text}) ->
    case Text of
        [X, S | _Chars] ->
            if
                X /= $X, X /= $x,
                S /= $+, S /= $- ->
                    return_error(Line, {bad_extension_parameter, Text});
                true ->
                    {extension_parameter, Text}
            end;
        _ ->
            return_error(Line, {bad_extension_parameter, Text})
    end.

ensure_message(MegacopToken,  MID, Body) ->
    Profile = ensure_profile(MegacopToken),
    Version = Profile#'ServiceChangeProfile'.version,
    case Profile#'ServiceChangeProfile'.profileName of
        "megaco" ->
            #'Message'{version = Version, mId = MID, messageBody = Body};
        [$!]  ->
            #'Message'{version = Version, mId = MID, messageBody = Body}
    end.

%% modemType         = (V32bisToken / V22bisToken / V18Token / 
%%                      V22Token / V32Token / V34Token / V90Token / 
%%                      V91Token / SynchISDNToken / extensionParameter)
ensure_modemType({_TokenTag, _Line, Text} = Token) ->
    case Text of
        "v32b"      -> v32bis;
        "v22b"      -> v22bis;
        "v18"       -> v18;
        "v22"       -> v22;
        "v32"       -> v32;
        "v32"       -> v32;
        "v34"       -> v34;
        "v90"       -> v90;
        "v91"       -> v91;
        "synchisdn" -> synchISDN;
        "sn"        -> synchISDN;
        [$x | _]               -> ensure_extensionParameter(Token)
    end.

%% An mtp address is five octets long
ensure_mtpAddress({_TokenTag, _Line, Addr}) ->
    %% BUGBUG: validate address
    {mtpAddress, Addr}.

%% MuxType = ( H221Token / H223Token / H226Token / V76Token / extensionParameter )
ensure_muxType({_TokenTag, _Line, Text} = Token) ->
    case Text of
        "h221" -> h221;
        "h223" -> h223;
        "h226" -> h226;
        "v76"  -> v76;
        [$x | _]          -> ensure_extensionParameter(Token)
    end.

%% packagesItem      = NAME "-" UINT16
%% NAME              = ALPHA *63(ALPHA / DIGIT / "_" )
ensure_packagesItem({TokenTag, Line, Text}) ->
    case string:tokens(Text, [$-]) of
        [Name, Version] ->
            #'PackagesItem'{packageName    = ensure_NAME({TokenTag, Line, Name}),
                            packageVersion = ensure_uint({TokenTag, Line, Version}, 0, 99)};
        _ ->
            return_error(Line, {bad_PackagesItem, Text})
    end.

%% pkgdName          =  (PackageName / "*")  SLASH  (ItemID / "*" )
%% PackageName       = NAME
%% ItemID            = NAME
ensure_pkgdName({TokenTag, Line, Text}) ->
    case string:tokens(Text, [$/]) of
        [Name, Item] ->
            ensure_name_or_star({TokenTag, Line, Name}),
            ensure_name_or_star({TokenTag, Line, Item}),
            Text;
        _ ->
            return_error(Line, {bad_pkgdName, Text})
    end.

ensure_name_or_star({_, _, Name}) when Name == "*" ->
    Name;
ensure_name_or_star(Name) ->
    ensure_NAME(Name).

merge_ServiceChangeParm({Tag, Val}, SCP) ->
    case Tag of
        address   -> SCP#'ServiceChangeParm'{serviceChangeAddress = Val};
        mgc_id    -> SCP#'ServiceChangeParm'{serviceChangeMgcId   = Val};
        profile   -> SCP#'ServiceChangeParm'{serviceChangeProfile = Val};
        version   -> SCP#'ServiceChangeParm'{serviceChangeVersion = Val};
        reason    -> SCP#'ServiceChangeParm'{serviceChangeReason  = Val};
        delay     -> SCP#'ServiceChangeParm'{serviceChangeDelay   = Val};
        method    -> SCP#'ServiceChangeParm'{serviceChangeMethod  = Val};
        time_stamp-> SCP#'ServiceChangeParm'{timeStamp            = Val};
        extension -> SCP %% BUGBUG: Should not be discarded
    end.

merge_ServiceChangeResParm({Tag, Val}, SCRP) ->
    case Tag of
        address    -> SCRP#'ServiceChangeResParm'{serviceChangeAddress = Val};
        mgc_id     -> SCRP#'ServiceChangeResParm'{serviceChangeMgcId   = Val};
        profile    -> SCRP#'ServiceChangeResParm'{serviceChangeProfile = Val};
        version    -> SCRP#'ServiceChangeResParm'{serviceChangeVersion = Val};
	time_stamp -> SCRP#'ServiceChangeResParm'{timeStamp            = Val}
    end.

ensure_serviceChangeMethod({safeToken, Line, Text}) ->
    case Text of
        "fl"            -> failover;
        "failover"      -> failover;
        "fo"            -> forced;
        "forced"        -> forced;
        "gr"            -> graceful;
        "graceful"      -> graceful;
        "rs"            -> restart;
        "restart"       -> restart;
        "dc"            -> disconnected;
        "disconnected"  -> disconnected;
        "ho"            -> handOff;
        "handoff"       -> handOff;
        Bad             -> return_error(Line, {bad_serviceChangeMethod, Bad})
    end.
ensure_profile({_TokenTag, Line, Text}) ->
    case string:tokens(Text, [$/]) of
        [Name, Version] ->
            Version2 = ensure_version(Version),
            #'ServiceChangeProfile'{profileName = Name, version = Version2};
        _ ->
            return_error(Line, {bad_profile, Text})
    end.

ensure_version(Version) ->
    ensure_uint(Version, 0, 99).

merge_signalRequest(SignalName, PropertyParms) ->
    Sig = #'Signal'{signalName = SignalName},
    SPL = [],
    do_merge_signalRequest(Sig, PropertyParms, SPL).

do_merge_signalRequest(Sig, [H | T], SPL) ->
    case H of
        {stream, StreamId} when Sig#'Signal'.streamID == asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{streamID = StreamId}, T, SPL);
        {signal_type, SigType} when Sig#'Signal'.sigType == asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{sigType = SigType}, T, SPL);
        {duration, Duration} when Sig#'Signal'.duration == asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{duration = Duration}, T, SPL);
        {notify_completion, NC} when Sig#'Signal'.notifyCompletion == asn1_NOVALUE ->
            do_merge_signalRequest(Sig#'Signal'{notifyCompletion = NC}, T, SPL);
        keepActive when Sig#'Signal'.keepActive == asn1_NOVALUE->
            do_merge_signalRequest(Sig#'Signal'{keepActive = true}, T, SPL);
        {other, Name, PP} ->
            SP = #'SigParameter'{sigParameterName = Name, 
				 value            = PP#'PropertyParm'.value,
				 extraInfo        = PP#'PropertyParm'.extraInfo},
            do_merge_signalRequest(Sig, T, [SP | SPL]);
        _ ->
            return_error(0, {bad_sigParm, H})
    end;
do_merge_signalRequest(Sig, [], SPL) ->
    Sig#'Signal'{sigParList = lists:reverse(SPL)} .

%% eventStream       = StreamToken EQUAL StreamID
%% eventOther        = eventParameterName parmValue
select_stream_or_other(Name, Value) ->
    case Name of
        "st" ->
            {stream, ensure_uint16(Value)};
        "stream" ->
            {stream, ensure_uint16(Value)};
        _ ->
            EP = #'EventParameter'{eventParameterName = Name,
                                   value              = Value#'PropertyParm'.value},
            {other, EP}
    end.

ensure_eventDM({_TokenTag, Line, DMD}) when record(DMD, 'DigitMapDescriptor') ->
    Name = DMD#'DigitMapDescriptor'.digitMapName,
    Val  = DMD#'DigitMapDescriptor'.digitMapValue,
    if
        Name  == asn1_NOVALUE, Val /= asn1_NOVALUE ->
            {eventDM, {digitMapValue, Val}};
        Name  /= asn1_NOVALUE, Val == asn1_NOVALUE ->
            {eventDM, {digitMapName, Name}};
        true ->
            return_error(Line, {bad_eventDM, DMD})
    end.
    
merge_observed_event(ObservedEvents, EventName, TimeStamp) ->
    StreamId = asn1_NOVALUE,
    EPL = [],
    do_merge_observed_event(ObservedEvents, EventName, TimeStamp, StreamId, EPL).

do_merge_observed_event([{stream, StreamID} | T], EventName, TimeStamp, asn1_NOVALUE, EPL) ->
    do_merge_observed_event(T, EventName, TimeStamp, StreamID, EPL);
do_merge_observed_event([{other, PP} | T], EventName, TimeStamp, StreamID, EPL) ->
    do_merge_observed_event(T, EventName, TimeStamp, StreamID, [PP | EPL]);
do_merge_observed_event([], EventName, TimeStamp, StreamID, EPL) ->
    #'ObservedEvent'{eventName    = EventName,
                     timeNotation = TimeStamp,
                     streamID     = StreamID,
                     eventParList = lists:reverse(EPL)}.

merge_eventSpec(OE) when record(OE, 'ObservedEvent'),
                         OE#'ObservedEvent'.timeNotation == asn1_NOVALUE ->
    #'EventSpec'{eventName     = OE#'ObservedEvent'.eventName,
                 streamID      = OE#'ObservedEvent'.streamID,
                 eventParList  = OE#'ObservedEvent'.eventParList};
merge_eventSpec(OE) ->
    return_error(0, {bad_event_spec, OE}).

merge_eventParameters(Params) ->
    StreamId = asn1_NOVALUE,
    EPL      = [],
    RA       = #'RequestedActions'{},
    HasA     = no,
    do_merge_eventParameters(Params, StreamId, EPL, RA, HasA) .
                                   
do_merge_eventParameters([H | T], StreamId, EPL, RA, HasA) ->
    case H of
        keepActive when RA#'RequestedActions'.keepActive == asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{keepActive = true},
            do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
        {embed, SD, SED} when RA#'RequestedActions'.signalsDescriptor == asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{signalsDescriptor = SD,
                                          secondEvent       = SED},
            do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
        {eventDM, DM} when RA#'RequestedActions'.eventDM == asn1_NOVALUE ->
            RA2 = RA#'RequestedActions'{eventDM = DM},
            do_merge_eventParameters(T, StreamId, EPL, RA2, yes);
        {stream, NewStreamId} when StreamId == asn1_NOVALUE ->
            do_merge_eventParameters(T, StreamId, EPL, RA, HasA);
        {other, PP} when record(PP, 'PropertyParm') ->
            EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
                                   value              = PP#'PropertyParm'.value,
				   extraInfo          = PP#'PropertyParm'.extraInfo},
            do_merge_eventParameters(T, StreamId, [EP | EPL], RA, HasA);
        {other, EP} when record(EP, 'EventParameter') ->
            do_merge_eventParameters(T, StreamId, [EP | EPL], RA, HasA);
        _ ->
            return_error(0, {bad_eventParameter, H})
    end;
do_merge_eventParameters([], StreamId, EPL, RA, yes) ->
    #'RequestedEvent'{streamID    = StreamId,
                      eventAction = RA, 
                      evParList   = lists:reverse(EPL)};
do_merge_eventParameters([], StreamId, EPL, _RA, no) ->
    #'RequestedEvent'{streamID    = StreamId,
                      eventAction = asn1_NOVALUE, 
                      evParList   = lists:reverse(EPL)}.

merge_secondEventParameters(Params) ->
    StreamId = asn1_NOVALUE,
    EPL      = [],
    SRA      = #'SecondRequestedActions'{},
    HasA     = no,
    do_merge_secondEventParameters(Params, StreamId, EPL, SRA, HasA) .
                                   
do_merge_secondEventParameters([H | T], StreamId, EPL, SRA, HasA) ->
    case H of
        keepActive when SRA#'SecondRequestedActions'.keepActive == asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{keepActive = true},
            do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
        {second_embed, SD} when SRA#'SecondRequestedActions'.signalsDescriptor == asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{signalsDescriptor = SD},
            do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
        {eventDM, DM} when SRA#'SecondRequestedActions'.eventDM == asn1_NOVALUE ->
            SRA2 = SRA#'SecondRequestedActions'{eventDM = DM},
            do_merge_secondEventParameters(T, StreamId, EPL, SRA2, yes);
        {stream, NewStreamId} when StreamId == asn1_NOVALUE ->
            do_merge_secondEventParameters(T, StreamId, EPL, SRA, HasA);
        {other, PP} when record(PP, 'PropertyParm') ->
            EP = #'EventParameter'{eventParameterName = PP#'PropertyParm'.name,
                                   value              = PP#'PropertyParm'.value,
				   extraInfo          = PP#'PropertyParm'.extraInfo},
            do_merge_secondEventParameters(T, StreamId, [EP | EPL], SRA, HasA);
        {other, EP} when record(EP, 'EventParameter') ->
            do_merge_secondEventParameters(T, StreamId, [EP | EPL], SRA, HasA);
        _ ->
            return_error(0, {bad_secondEventParameter, H})
    end;
do_merge_secondEventParameters([], StreamId, EPL, SRA, yes) ->
    #'SecondRequestedEvent'{streamID    = StreamId,
                            eventAction = SRA, 
                            evParList   = lists:reverse(EPL)};
do_merge_secondEventParameters([], StreamId, EPL, _SRA, no) ->
    #'SecondRequestedEvent'{streamID    = StreamId,
                            eventAction = asn1_NOVALUE, 
                            evParList   = lists:reverse(EPL)}.

%% terminationID     = "ROOT" / pathName / "$" / "*"
%% Total length of pathName must not exceed 64 chars.
%% pathName          = ["*"] NAME *("/" / "*"/ ALPHA / DIGIT /"_" / "$" )
%%                     ["@" pathDomainName ]
%% ABNF allows two or more consecutive "." although it is meaningless
%% in a path domain name.
%% pathDomainName    = (ALPHA / DIGIT / "*" )
%%                        *63(ALPHA / DIGIT / "-" / "*" / ".")
ensure_terminationID({safeToken, _Line, LowerText}) ->
    %% terminationID     = "ROOT" / pathName / "$" / "*"
    decode_term_id(LowerText, false, [], []).

decode_term_id([H | T], Wild, Id, Component) ->
    case H of
        $/ -> decode_term_id(T, Wild, [lists:reverse(Component) | Id], []);
        $* -> decode_term_id(T, true, Id, [?megaco_all    | Component]);
        $$ -> decode_term_id(T, true, Id, [?megaco_choose | Component]);
        _  -> decode_term_id(T, Wild, Id, [H | Component])
    end;
decode_term_id([], Wild, Id, Component) ->
    Id2 = [lists:reverse(Component) | Id],
    #megaco_term_id{contains_wildcards = Wild, id = lists:reverse(Id2)}.
            
ensure_pathName({_TokenTag, _Line, Text}) ->
    Text.  %% BUGBUG: ensure values

%% TimeStamp            = Date "T" Time ; per ISO 8601:1988
%% Date                 = 8(DIGIT) ; Date = yyyymmdd
%% Time                 = 8(DIGIT) ; Time = hhmmssss
ensure_timeStamp({safeToken, Line, Text}) ->
    case string:tokens(Text, [$T, $t]) of
        [Date, Time] ->
            #'TimeNotation'{date = Date, time = Time};
        _ ->
            return_error(Line, {bad_timeStamp, Text})
    end.

ensure_transactionID(TransId) ->
    ensure_uint32(TransId).

%% transactionAck       = transactionID / (transactionID "-" transactionID)
ensure_transactionAck({safeToken, _Line, Text}) ->
    case string:tokens(Text, [$-]) of
        [Id] ->
            #'TransactionAck'{firstAck = ensure_transactionID(Id)};
        [Id, Id2] ->
            #'TransactionAck'{firstAck = ensure_transactionID(Id),
			      lastAck  = ensure_transactionID(Id2)}
    end.

merge_action_requests(ContextId, Items) ->
    CtxReq      = #'ContextRequest'{},
    CtxAuditReq = #'ContextAttrAuditRequest'{},
    CmdReq      = [],
    TopReq      = [],
    do_merge_action_requests(ContextId, CtxReq, CtxAuditReq, CmdReq, TopReq, Items).

do_merge_action_requests(ContextId, CtxReq, CtxAuditReq, CmdReq, TopReq, [H | T]) ->
    case H of
        _ when record(H, 'CommandRequest') ->
            do_merge_action_requests(ContextId, CtxReq, CtxAuditReq, [H | CmdReq], TopReq, T);

        {priority, Int} when CtxReq#'ContextRequest'.priority == asn1_NOVALUE ->
            CtxReq2 = CtxReq#'ContextRequest'{priority = Int},
            do_merge_action_requests(ContextId, CtxReq2, CtxAuditReq, CmdReq, 
				     TopReq, T);
        {emergency, Bool} when CtxReq#'ContextRequest'.emergency == asn1_NOVALUE ->
            CtxReq2 = CtxReq#'ContextRequest'{emergency = Bool},
            do_merge_action_requests(ContextId, CtxReq2, CtxAuditReq, CmdReq, 
				     TopReq, T);
        {topology, Desc} ->
            TopReq2 = Desc ++ TopReq, %% OTP-4088
            do_merge_action_requests(ContextId, CtxReq, CtxAuditReq, CmdReq, 
				     TopReq2, T);

        priorityAudit when CtxAuditReq#'ContextAttrAuditRequest'.priority == asn1_NOVALUE ->
            CtxAuditReq2 = CtxAuditReq#'ContextAttrAuditRequest'{priority = 'NULL'},
            do_merge_action_requests(ContextId, CtxReq, CtxAuditReq2, CmdReq, 
				     TopReq, T);
        emergencyAudit when CtxAuditReq#'ContextAttrAuditRequest'.emergency == asn1_NOVALUE ->
            CtxAuditReq2 = CtxAuditReq#'ContextAttrAuditRequest'{emergency = 'NULL'},
            do_merge_action_requests(ContextId, CtxReq, CtxAuditReq2, CmdReq, 
				     TopReq, T);
        topologyAudit when CtxAuditReq#'ContextAttrAuditRequest'.topology == asn1_NOVALUE ->
            CtxAuditReq2 = CtxAuditReq#'ContextAttrAuditRequest'{topology = 'NULL'},
            do_merge_action_requests(ContextId, CtxReq, CtxAuditReq2, CmdReq, 
				     TopReq, T)
    end;
do_merge_action_requests(ContextId, CtxReq, CtxAuditReq, CmdReq, TopReq, []) ->
    #'ActionRequest'{contextId           = ContextId,
                     contextRequest      = strip_contextRequest(CtxReq, TopReq),
                     contextAttrAuditReq = strip_contextAttrAuditRequest(CtxAuditReq),
                     commandRequests     = lists:reverse(CmdReq)}.

merge_action_reply(ReplyList) ->
    CtxReq  = #'ContextRequest'{},
    TopReq  = [],
    CmdList = [],
    do_merge_action_reply(ReplyList, CtxReq, TopReq, CmdList). 

do_merge_action_reply([H | T], CtxReq, TopReq, CmdList) ->
    case H of
        {command, Cmd} ->
            do_merge_action_reply(T, CtxReq, TopReq, [Cmd | CmdList]);
        {context, Ctx} ->
            case Ctx of
                {priority, Int} when CtxReq#'ContextRequest'.priority == asn1_NOVALUE ->
                    CtxReq2 = CtxReq#'ContextRequest'{priority = Int},
                    do_merge_action_reply(T, CtxReq2, TopReq, CmdList);
                {emergency, Bool} when CtxReq#'ContextRequest'.emergency == asn1_NOVALUE ->
                    CtxReq2 = CtxReq#'ContextRequest'{emergency = Bool},
                    do_merge_action_reply(T, CtxReq2, TopReq, CmdList);
                {topology, Desc} ->
                    TopReq2 = Desc ++ TopReq, %% OTP-4088
                    do_merge_action_reply(T, CtxReq, TopReq2, CmdList)
            end
    end;
do_merge_action_reply([], CtxReq, TopReq, CmdList) ->
    #'ActionReply'{contextReply = strip_contextRequest(CtxReq, TopReq),
                   commandReply = lists:reverse(CmdList)}.

strip_contextRequest(R, TopReq)
  when R#'ContextRequest'.priority    == asn1_NOVALUE,
       R#'ContextRequest'.emergency   == asn1_NOVALUE,
       TopReq == [] ->
    asn1_NOVALUE;
strip_contextRequest(R, []) ->
    R#'ContextRequest'{topologyReq = asn1_NOVALUE};
strip_contextRequest(R, TopReq) ->
    R#'ContextRequest'{topologyReq = TopReq}. %% OTP-4088


strip_contextAttrAuditRequest(R)
  when R#'ContextAttrAuditRequest'.priority  == asn1_NOVALUE,
       R#'ContextAttrAuditRequest'.emergency == asn1_NOVALUE,
       R#'ContextAttrAuditRequest'.topology  == asn1_NOVALUE ->
    asn1_NOVALUE;
strip_contextAttrAuditRequest(R) ->
    R.

make_commandRequest({CmdTag, {_TokenTag, _Line, Text}}, Cmd) ->
    Req = #'CommandRequest'{command  = {CmdTag, Cmd}},
    case Text of
        [$w, $- | _] ->
            Req#'CommandRequest'{wildcardReturn = 'NULL'};
        [$o, $-, $w, $- | _] ->
            Req#'CommandRequest'{optional = 'NULL', wildcardReturn = 'NULL'};
        [$o, $- | _] ->
            Req#'CommandRequest'{optional = 'NULL'};
        _ -> 
            Req
    end.

merge_terminationAudit(AuditReturnParameters) ->
    lists:reverse(do_merge_terminationAudit(AuditReturnParameters, [], [])).

do_merge_terminationAudit([H| T], ARPs, AuditItems) ->
    case H of
	{auditItem, AuditItem} ->
	    do_merge_terminationAudit(T, ARPs, [AuditItem | AuditItems]);
	AuditReturnParameter ->
	    do_merge_terminationAudit(T, [AuditReturnParameter | ARPs], AuditItems)
    end;
do_merge_terminationAudit([], AuditReturnParameters, []) ->
    AuditReturnParameters;
do_merge_terminationAudit([], AuditReturnParameters, AuditItems) ->
    AuditDescriptor = #'AuditDescriptor'{auditToken = AuditItems},
    AuditReturnParameter = {emptyDescriptors, AuditDescriptor},
    [AuditReturnParameter | AuditReturnParameters].
        
merge_mediaDescriptor(MediaParms) ->
    do_merge_mediaDescriptor(MediaParms, asn1_NOVALUE, [], []).

do_merge_mediaDescriptor([H | T], TS, One, Multi) ->
    case H of
        {streamParm, Parm} when Multi == [] ->
            do_merge_mediaDescriptor(T, TS, [Parm | One], Multi);
        {streamDescriptor, Desc} when One == [] ->
            do_merge_mediaDescriptor(T, TS, One, [Desc | Multi]);
        {termState, TS2} when TS  == asn1_NOVALUE ->
            do_merge_mediaDescriptor(T, TS2, One, Multi);
        _ ->
            return_error(0, {bad_merge_mediaDescriptor, [H, TS, One, Multi]})
    end;
do_merge_mediaDescriptor([], TS, One, Multi) ->
    if
	One == [], Multi == [] ->
	    #'MediaDescriptor'{streams = asn1_NOVALUE,
			       termStateDescr = TS};
	One /= [], Multi == [] ->
	    #'MediaDescriptor'{streams = {oneStream, merge_streamParms(One)},
			       termStateDescr = TS};
	One == [], Multi /= [] ->
	    #'MediaDescriptor'{streams = {multiStream, lists:reverse(Multi)},
			       termStateDescr = TS}
    end.
  
merge_streamParms(TaggedStreamParms) ->
    SP = #'StreamParms'{},
    do_merge_streamParms(TaggedStreamParms, SP).

do_merge_streamParms([{Tag, D} | T] = All, SP) ->
    case Tag of
        local when SP#'StreamParms'.localDescriptor  == asn1_NOVALUE ->
            do_merge_streamParms(T, SP#'StreamParms'{localDescriptor = D});
        remote when SP#'StreamParms'.remoteDescriptor == asn1_NOVALUE ->
            do_merge_streamParms(T, SP#'StreamParms'{remoteDescriptor = D});
        control ->
            LCD = 
                case SP#'StreamParms'.localControlDescriptor of
                    asn1_NOVALUE ->
                        #'LocalControlDescriptor'{propertyParms = []};
                    PrevLCD ->
                        PrevLCD
                end,
            LCD2 = do_merge_control_streamParms(D, LCD),
            do_merge_streamParms(T, SP#'StreamParms'{localControlDescriptor = LCD2});
        _ ->
            return_error(0, {do_merge_streamParms, [All, SP]})
    end;
do_merge_streamParms([], SP) when record(SP#'StreamParms'.localControlDescriptor, 'LocalControlDescriptor') ->
    LCD  = SP#'StreamParms'.localControlDescriptor,
    PP   = LCD#'LocalControlDescriptor'.propertyParms,
    LCD2 = LCD#'LocalControlDescriptor'{propertyParms = lists:reverse(PP)},
    SP#'StreamParms'{localControlDescriptor = LCD2};
do_merge_streamParms([], SP) ->
    SP.


do_merge_control_streamParms([{SubTag, SD} | T] = All, LCD) ->
    case SubTag of
        group when LCD#'LocalControlDescriptor'.reserveGroup == asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{reserveGroup = SD},
            do_merge_control_streamParms(T, LCD2);
        value when LCD#'LocalControlDescriptor'.reserveValue == asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{reserveValue = SD},
            do_merge_control_streamParms(T, LCD2);
        mode when LCD#'LocalControlDescriptor'.streamMode == asn1_NOVALUE ->
            LCD2 = LCD#'LocalControlDescriptor'{streamMode = SD},
            do_merge_control_streamParms(T, LCD2);
        prop ->
            PP = LCD#'LocalControlDescriptor'.propertyParms,
            LCD2 = LCD#'LocalControlDescriptor'{propertyParms = [SD | PP]},
            do_merge_control_streamParms(T, LCD2);
        _ ->
            return_error(0, {do_merge_control_streamParms, [All, LCD]})
  end;
do_merge_control_streamParms([], LCD) ->
    LCD.

merge_terminationStateDescriptor(Parms) ->
    TSD = #'TerminationStateDescriptor'{propertyParms = []},
    do_merge_terminationStateDescriptor(Parms, TSD).

do_merge_terminationStateDescriptor([{Tag, Val} | T], TSD) ->
    case Tag of
        serviceState when TSD#'TerminationStateDescriptor'.serviceState == asn1_NOVALUE ->
            TSD2 = TSD#'TerminationStateDescriptor'{serviceState = Val},
            do_merge_terminationStateDescriptor(T, TSD2);
        eventBufferControl when TSD#'TerminationStateDescriptor'.eventBufferControl == asn1_NOVALUE->
            TSD2 = TSD#'TerminationStateDescriptor'{eventBufferControl = Val},
            do_merge_terminationStateDescriptor(T, TSD2);
        propertyParm ->
            PP = TSD#'TerminationStateDescriptor'.propertyParms,
            TSD2 = TSD#'TerminationStateDescriptor'{propertyParms = [Val | PP]},
            do_merge_terminationStateDescriptor(T, TSD2)
    end;
do_merge_terminationStateDescriptor([], TSD) ->
    PP = TSD#'TerminationStateDescriptor'.propertyParms,
    TSD#'TerminationStateDescriptor'{propertyParms = lists:reverse(PP)}.

ensure_prop_groups({_TokenTag, _Line, Text}) ->
    Group  = [],
    Groups = [],
    parse_prop_name(Text, Group, Groups).

parse_prop_name([Char | Rest] = All, Group, Groups) ->
    case ?classify_char(Char) of
        white_space ->
            parse_prop_name(Rest, Group, Groups);
        end_of_line ->
            parse_prop_name(Rest, Group, Groups);
        _ ->
            Name = [],
            do_parse_prop_name(All, Name, Group, Groups)
    end;
parse_prop_name([] = All, Group, Groups) ->
    Name = [],
    do_parse_prop_name(All, Name, Group, Groups).

do_parse_prop_name([Char | Rest], Name, Group, Groups) ->
    case ?classify_char(Char) of
        safe_char ->
            do_parse_prop_name(Rest, [Char | Name], Group, Groups);
        rest_char when Char == $=, Name /= [] ->
            %% Now we have a complete name
            if
                Name == "v", Group /= [] ->
                    %% v= is a property group delimiter,
                    %% lets create yet another property group.
                    Groups2 = [lists:reverse(Group) | Groups],
                    Group2 = [],
                    parse_prop_value(Rest, Name, Group2, Groups2);
                true ->
                    %% Use current property group
                    parse_prop_value(Rest, Name, Group, Groups)
            end;
        _ ->
            return_error(0, {bad_prop_name, lists:reverse(Name), Char})
    end;
do_parse_prop_name([], [], [], Groups) ->
    lists:reverse(Groups);
do_parse_prop_name([], [], Group, Groups) ->
    Group2 = lists:reverse(Group),
    lists:reverse([Group2 | Groups]);
do_parse_prop_name([], Name, Group, Groups) when Name /= [] ->
    %% Assume end of line
    Value = [],
    PP = make_prop_parm(Name, Value),
    Group2 = lists:reverse([PP | Group]),
    lists:reverse([Group2 | Groups]).
                   
parse_prop_value(Chars, Name, Group, Groups) ->
    Value = [],
    do_parse_prop_value(Chars, Name, Value, Group, Groups).

do_parse_prop_value([Char | Rest], Name, Value, Group, Groups) ->
    case ?classify_char(Char) of
        end_of_line ->
            %% Now we have a complete "name=value" pair
            PP = make_prop_parm(Name, Value),
            parse_prop_name(Rest, [PP | Group], Groups);
        _ ->
            do_parse_prop_value(Rest, Name, [Char | Value], Group, Groups)
    end;
do_parse_prop_value([], Name, Value, Group, Groups) ->
    %% Assume end of line
    PP = make_prop_parm(Name, Value),
    Group2 = lists:reverse([PP | Group]),
    lists:reverse([Group2 | Groups]).

make_prop_parm(Name, Value) ->
    #'PropertyParm'{name  = lists:reverse(Name),
                    value = [lists:reverse(Value)]}.

ensure_uint({_TokenTag, Line, Val}, Min, Max) when integer(Val) ->
    if
        integer(Min), Val >= Min ->
            if
                integer(Max), Val =< Max ->
                    Val;
                Max == infinity ->
                    Val;
                true ->
                    return_error(Line, {too_large_integer, Val, Max})
            end;
        true ->
            return_error(Line, {too_small_integer, Val, Min})
    end;
ensure_uint({TokenTag, Line, Text}, Min, Max) ->
    case catch list_to_integer(Text) of
        {'EXIT', _} ->
            return_error(Line, {not_an_integer, Text});
        Val when integer(Val) ->
            ensure_uint({TokenTag, Line, Val}, Min, Max)
   end;
ensure_uint(Val, Min, Max) ->
    ensure_uint({uint, 0, Val}, Min, Max).

ensure_uint16(Int) ->
    ensure_uint(Int, 0, 65535).

ensure_uint32(Int) ->
    ensure_uint(Int, 0, 4294967295) .

%% OTP-4710
ensure_hex({_TokenTag, _Line, [$0, $x |Chars]}, Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex({_TokenTag, _Line, [$0, $X |Chars]}, Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex([$0, $x |Chars], Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []);
ensure_hex([$0, $X |Chars], Min, Max) ->
    ensure_uint(length(Chars), Min, Max),
    hex_to_int(Chars, []).

%% OTP-4710
hex_to_int([], Acc) ->
    lists:reverse(Acc);
hex_to_int([Char1,Char2|Tail], Acc) ->
    Int1 = hchar_to_int(Char1),
    Int2 = hchar_to_int(Char2),
    Val  = Int2 bor (Int1 bsl 4),
    hex_to_int(Tail, [Val| Acc]);
hex_to_int([Char], Acc) ->
    Int = hchar_to_int(Char),
    lists:reverse([Int|Acc]).

hchar_to_int(Char) when $0 =< Char, Char =< $9 ->
    Char - $0;
hchar_to_int(Char) when $A =< Char, Char =< $F ->
    Char - $A + 10; % OTP-4710
hchar_to_int(Char) when $a =< Char, Char =< $f ->
    Char - $a + 10. % OTP-4710

value_of({_TokenTag, _Line, Text}) ->
    Text.

