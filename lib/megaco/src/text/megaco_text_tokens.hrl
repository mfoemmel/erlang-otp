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
%% Purpose: Define of tokens used in text encoding of Megaco/H.248
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Adding a new token requires changes in the following files:
%%    
%%    megaco_text_tokens.hrl
%%    megaco_text_gen.hrl
%%    megaco_compact_text_encoder.erl
%%    megaco_pretty_text_encoder.erl
%%    megaco_text_scanner.erl
%%    megaco_text_parser.yrl (safeToken rule, make_safe_token/1, actual rule)
%%
%% Plus regeneration the ASN.1 related files including
%% manual patches 
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Special characters
%%----------------------------------------------------------------------

-define(EqualToken,       16#3d).
-define(ColonToken,       16#3a).
-define(LbrktToken,       16#7b).
-define(RbrktToken,       16#7d).
-define(LsbrktToken,      $[).
-define(RsbrktToken,      $]).
-define(CommaToken,       16#2c).
-define(DotToken,         16#2e).
-define(SlashToken,       16#2f).
-define(DoubleQuoteToken, 16#22).
-define(SpToken,          16#20).
-define(HtabToken,        16#09).
-define(CrToken,          16#0d).
-define(LfToken,          16#0a).

-define(SemiColonToken,   $;).
-define(NequalToken,      $#).
-define(GreaterToken,     $>).
-define(LesserToken,      $<).
-define(BackslashToken,   $\\).
-define(LparToken,        $().
-define(RparToken,        $)).
-define(VbarToken,        $|).

%%----------------------------------------------------------------------
%% Pretty version of tokens
%%----------------------------------------------------------------------

-define(PrettyAddToken                   , "Add"                   ).
-define(PrettyAuditToken                 , "Audit"                 ).
-define(PrettyAuditCapToken              , "AuditCapability"       ).
-define(PrettyAuditValueToken            , "AuditValue"            ).
-define(PrettyAuthToken                  , "Authentication"        ).
-define(PrettyBothwayToken               , "Bothway"               ).
-define(PrettyBriefToken                 , "Brief"                 ).
-define(PrettyBufferToken                , "Buffer"                ).
-define(PrettyCtxToken                   , "Context"               ).
-define(PrettyContextAuditToken          , "ContextAudit"          ).
-define(PrettyDigitMapToken              , "DigitMap"              ).
-define(PrettyDiscardToken               , "Discard"               ).
-define(PrettyDisconnectedToken          , "Disconnected"          ).
-define(PrettyDelayToken                 , "Delay"                 ).
-define(PrettyDurationToken              , "Duration"              ).
-define(PrettyEmbedToken                 , "Embed"                 ).
-define(PrettyEmergencyToken             , "Emergency"             ).
-define(PrettyErrorToken                 , "Error"                 ).
-define(PrettyEventBufferToken           , "EventBuffer"           ).
-define(PrettyEventsToken                , "Events"                ).
-define(PrettyFailoverToken              , "Failover"              ).
-define(PrettyForcedToken                , "Forced"                ).
-define(PrettyGracefulToken              , "Graceful"              ).
-define(PrettyH221Token                  , "H221"                  ).
-define(PrettyH223Token                  , "H223"                  ).
-define(PrettyH226Token                  , "H226"                  ).
-define(PrettyHandOffToken               , "HandOff"               ).
-define(PrettyImmAckRequiredToken        , "ImmAckRequired"        ).
-define(PrettyInactiveToken              , "Inactive"              ).
-define(PrettyInterruptByEventToken      , "IntByEvent"            ). 
-define(PrettyInterruptByNewSignalsDescrToken, "IntBySigDescr"     ). 
-define(PrettyIsolateToken               , "Isolate"               ).
-define(PrettyInSvcToken                 , "InService"             ).
-define(PrettyKeepActiveToken            , "KeepActive"            ).
-define(PrettyLocalToken                 , "Local"                 ).
-define(PrettyLocalControlToken          , "LocalControl"          ).
-define(PrettyLockStepToken              , "LockStep"              ).
-define(PrettyLoopbackToken              , "Loopback"              ).
-define(PrettyMediaToken                 , "Media"                 ).
-define(PrettyMegacopToken               , "MEGACO"                ).
-define(PrettyMethodToken                , "Method"                ).
-define(PrettyMgcIdToken                 , "MgcIdToTry"            ).
-define(PrettyModeToken                  , "Mode"                  ).
-define(PrettyModifyToken                , "Modify"                ).
-define(PrettyModemToken                 , "Modem"                 ).
-define(PrettyMoveToken                  , "Move"                  ).
-define(PrettyMtpToken                   , "MTP"                   ).
-define(PrettyMuxToken                   , "Mux"                   ).
-define(PrettyNotifyToken                , "Notify"                ).
-define(PrettyNotifyCompletionToken      , "NotifyCompletion"      ).
-define(PrettyNx64kToken                 , "Nx64Kservice"          ).
-define(PrettyObservedEventsToken        , "ObservedEvents"        ).
-define(PrettyOffToken                   , "OFF"                   ).
-define(PrettyOnewayToken                , "Oneway"                ).
-define(PrettyOnOffToken                 , "OnOff"                 ).
-define(PrettyOnToken                    , "ON"                    ).
-define(PrettyOtherReasonToken           , "OtherReason"           ).
-define(PrettyOutOfSvcToken              , "OutOfService"          ).
-define(PrettyPackagesToken              , "Packages"              ).
-define(PrettyPendingToken               , "Pending"               ).
-define(PrettyPriorityToken              , "Priority"              ).
-define(PrettyProfileToken               , "Profile"               ).
-define(PrettyReasonToken                , "Reason"                ).
-define(PrettyRecvonlyToken              , "ReceiveOnly"           ).
-define(PrettyReplyToken                 , "Reply"                 ).
-define(PrettyResponseAckToken           , "TransactionResponseAck").
-define(PrettyRestartToken               , "Restart"               ).
-define(PrettyRemoteToken                , "Remote"                ).
-define(PrettyReservedGroupToken         , "ReservedGroup"         ).
-define(PrettyReservedValueToken         , "ReservedValue"         ).
-define(PrettySendonlyToken              , "SendOnly"              ).
-define(PrettySendrecvToken              , "SendReceive"           ).
-define(PrettyServicesToken              , "Services"              ).
-define(PrettyServiceStatesToken         , "ServiceStates"         ).
-define(PrettyServiceChangeToken         , "ServiceChange"         ).
-define(PrettyServiceChangeAddressToken  , "ServiceChangeAddress"  ).
-define(PrettySignalListToken            , "SignalList"            ).
-define(PrettySignalsToken               , "Signals"               ).
-define(PrettySignalTypeToken            , "SignalType"            ).
-define(PrettyStatsToken                 , "Statistics"            ).
-define(PrettyStreamToken                , "Stream"                ).
-define(PrettySubtractToken              , "Subtract"              ).
-define(PrettySynchISDNToken             , "SynchISDN"             ).
-define(PrettyTerminationStateToken      , "TerminationState"      ).
-define(PrettyTestToken                  , "Test"                  ).
-define(PrettyTimeOutToken               , "TimeOut"               ).
-define(PrettyTopologyToken              , "Topology"              ).
-define(PrettyTransToken                 , "Transaction"           ).
-define(PrettyV18Token                   , "V18"                   ).
-define(PrettyV22Token                   , "V22"                   ).
-define(PrettyV22bisToken                , "V22b"                  ).
-define(PrettyV32Token                   , "V32"                   ).
-define(PrettyV32bisToken                , "V32b"                  ).
-define(PrettyV34Token                   , "V34"                   ).
-define(PrettyV76Token                   , "V76"                   ).
-define(PrettyV90Token                   , "V90"                   ).
-define(PrettyV91Token                   , "V91"                   ).
-define(PrettyVersionToken               , "Version"               ).

%%----------------------------------------------------------------------
%% Compact version of tokens
%%----------------------------------------------------------------------

-define(CompactAddToken                   , "A"                    ).
-define(CompactAuditToken                 , "AT"                   ).
-define(CompactAuditCapToken              , "AC"                   ).
-define(CompactAuditValueToken            , "AV"                   ).
-define(CompactAuthToken                  , "AU"                   ).
-define(CompactBothwayToken               , "BW"                   ).
-define(CompactBriefToken                 , "BR"                   ).
-define(CompactBufferToken                , "BF"                   ).
-define(CompactCtxToken                   , "C"                    ).
-define(CompactContextAuditToken          , "CA"                   ).
-define(CompactDigitMapToken              , "DM"                   ).
-define(CompactDiscardToken               , "DS"                   ).
-define(CompactDisconnectedToken          , "DC"                   ).
-define(CompactDelayToken                 , "DL"                   ).
-define(CompactDurationToken              , "DR"                   ).
-define(CompactEmbedToken                 , "EM"                   ).
-define(CompactEmergencyToken             , "EG"                   ).
-define(CompactErrorToken                 , "ER"                   ).
-define(CompactEventBufferToken           , "EB"                   ).
-define(CompactEventsToken                , "E"                    ).
-define(CompactFailoverToken              , "FL"                   ).
-define(CompactForcedToken                , "FO"                   ).
-define(CompactGracefulToken              , "GR"                   ).
-define(CompactH221Token                  , ?PrettyH221Token       ).
-define(CompactH223Token                  , ?PrettyH223Token       ).
-define(CompactH226Token                  , ?PrettyH226Token       ).
-define(CompactHandOffToken               , "HO"                   ).
-define(CompactImmAckRequiredToken        , "IA"                   ).
-define(CompactInactiveToken              , "IN"                   ).
-define(CompactInterruptByEventToken      , "IBE"                  ).
-define(CompactInterruptByNewSignalsDescrToken, "IBS"              ). 
-define(CompactIsolateToken               , "IS"                   ).
-define(CompactInSvcToken                 , "IV"                   ).
-define(CompactKeepActiveToken            , "KA"                   ).
-define(CompactLocalToken                 , "L"                    ).
-define(CompactLocalControlToken          , "O"                    ).
-define(CompactLockStepToken              , "SP"                   ).
-define(CompactLoopbackToken              , "LB"                   ).
-define(CompactMediaToken                 , "M"                    ).
-define(CompactMegacopToken               , "!"                    ).
-define(CompactMethodToken                , "MT"                   ).
-define(CompactMgcIdToken                 , "MG"                   ).
-define(CompactModeToken                  , "MO"                   ).
-define(CompactModifyToken                , "MF"                   ).
-define(CompactModemToken                 , "MD"                   ).
-define(CompactMoveToken                  , "MV"                   ).
-define(CompactMtpToken                   , ?PrettyMtpToken        ).
-define(CompactMuxToken                   , "MX"                   ).
-define(CompactNotifyToken                , "N"                    ).
-define(CompactNotifyCompletionToken      , "NC"                   ).
-define(CompactNx64kToken                 , "N64"                  ).
-define(CompactObservedEventsToken        , "OE"                   ).
-define(CompactOffToken                   , "OFF"                  ).
-define(CompactOnewayToken                , "OW"                   ).
-define(CompactOnOffToken                 , "OO"                   ).
-define(CompactOnToken                    , "ON"                   ).
-define(CompactOtherReasonToken           , "OR"                   ).
-define(CompactOutOfSvcToken              , "OS"                   ).
-define(CompactPackagesToken              , "PG"                   ).
-define(CompactPendingToken               , "PN"                   ).
-define(CompactPriorityToken              , "PR"                   ).
-define(CompactProfileToken               , "PF"                   ).
-define(CompactReasonToken                , "RE"                   ).
-define(CompactRecvonlyToken              , "RC"                   ).
-define(CompactReplyToken                 , "P"                    ).
-define(CompactResponseAckToken           , "K"                    ).
-define(CompactRestartToken               , "RS"                   ).
-define(CompactRemoteToken                , "R"                    ).
-define(CompactReservedGroupToken         , "RG"                   ).
-define(CompactReservedValueToken         , "RV"                   ).
-define(CompactSendonlyToken              , "SO"                   ).
-define(CompactSendrecvToken              , "SR"                   ).
-define(CompactServicesToken              , "SV"                   ).
-define(CompactServiceStatesToken         , "SI"                   ).
-define(CompactServiceChangeToken         , "SC"                   ).
-define(CompactServiceChangeAddressToken  , "AD"                   ).
-define(CompactSignalListToken            , "SL"                   ).
-define(CompactSignalsToken               , "SG"                   ).
-define(CompactSignalTypeToken            , "SY"                   ).
-define(CompactStatsToken                 , "SA"                   ).
-define(CompactStreamToken                , "ST"                   ).
-define(CompactSubtractToken              , "S"                    ).
-define(CompactSynchISDNToken             , "SN"                   ).
-define(CompactTerminationStateToken      , "TS"                   ).
-define(CompactTestToken                  , "TE"                   ).
-define(CompactTimeOutToken               , "TO"                   ).
-define(CompactTopologyToken              , "TP"                   ).
-define(CompactTransToken                 , "T"                    ).
-define(CompactV18Token                   , ?PrettyV18Token        ).
-define(CompactV22Token                   , ?PrettyV22Token        ).
-define(CompactV22bisToken                , ?PrettyV22bisToken     ).
-define(CompactV32Token                   , ?PrettyV32Token        ).
-define(CompactV32bisToken                , ?PrettyV32bisToken     ).
-define(CompactV34Token                   , ?PrettyV34Token        ).
-define(CompactV76Token                   , ?PrettyV76Token        ).
-define(CompactV90Token                   , ?PrettyV90Token        ).
-define(CompactV91Token                   , ?PrettyV91Token        ).
-define(CompactVersionToken               , "V"                    ).

-define(classify_char(Char),
    (case Char of
        _ when Char >= $0, Char =< $9       -> safe_char;
        _ when Char >= $a, Char =< $z       -> safe_char;
        _ when Char >= $A, Char =< $Z       -> safe_char;
        $+                                  -> safe_char;
        $-                                  -> safe_char;
        $&                                  -> safe_char;
        $!                                  -> safe_char;
        $_                                  -> safe_char;
        $/                                  -> safe_char;
        $'                                  -> safe_char;
        $?                                  -> safe_char;
        $@                                  -> safe_char;
        $^                                  -> safe_char;
        $`                                  -> safe_char;
        $~                                  -> safe_char;
        $*                                  -> safe_char;
        $$                                  -> safe_char;                    
        ?BackslashToken                     -> safe_char;
        ?LparToken                          -> safe_char;
        ?RparToken                          -> safe_char;
        $%                                  -> safe_char;
        ?VbarToken                          -> safe_char;
        $.                                  -> safe_char;
        ?SemiColonToken                     -> rest_char;
        ?LsbrktToken                        -> rest_char;
        ?RsbrktToken                        -> rest_char;
        ?LbrktToken                         -> rest_char;
        ?RbrktToken                         -> rest_char;
        ?ColonToken                         -> rest_char;
        ?CommaToken                         -> rest_char;
        ?NequalToken                        -> rest_char;
        ?LesserToken                        -> rest_char;
        ?GreaterToken                       -> rest_char;
        ?EqualToken                         -> rest_char;
        ?DoubleQuoteToken                   -> double_quote;
        ?SpToken                            -> white_space;
        ?HtabToken                          -> white_space;
        ?LfToken                            -> end_of_line;
        ?CrToken                            -> end_of_line;
        _                                   -> bad_char
    end)).

