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
%% Purpose: Encode COMPACT Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_compact_text_encoder).

-behaviour(megaco_encoder).

-export([encode_message/2, decode_message/2]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_text_tokens.hrl").

%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message([], MegaMsg) when record(MegaMsg, 'MegacoMessage') ->
    case catch enc_MegacoMessage(MegaMsg) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end;
encode_message([{flex,_}], MegaMsg) when record(MegaMsg, 'MegacoMessage') ->
    case catch enc_MegacoMessage(MegaMsg) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Bin when binary(Bin) ->
	    {ok, Bin};
	DeepIoList ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin}
    end;
encode_message(EncodingConfig, MegaMsg) when record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, EncodingConfig}};
encode_message(EncodingConfig, MegaMsg) ->
    {error, bad_megaco_message}.

%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message([], Bin) when binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, LastLine} ->
	    case (catch megaco_text_parser:parse(Tokens)) of %% OTP-4007
		{ok, MegacoMessage} ->
		    {ok, MegacoMessage};
		{error, Reason} ->
		    {error, [{reason, Reason},
			     {token, Tokens},
			     {chars, Bin}]};
		%% OTP-4007
		{'EXIT', Reason} ->
		    {error,[{reason, Reason},
			    {token, Tokens},
			    {chars, Bin}]}
	    end;
	{error, Reason, Line} ->             %% OTP-4007
	   {error, [{reason, Reason, Line},  %% OTP-4007
		    {token, []},
		    {chars, Bin}]}
    end;
decode_message([{flex, Port}], Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, LastLine} ->
	    case (catch megaco_text_parser:parse(Tokens)) of %% OTP-4007
		{ok, MegacoMessage} ->
		    {ok, MegacoMessage};
		{error, Reason} ->
		    {error, [{reason, Reason},
			     {token, Tokens},
			     {chars, Bin}]};
		%% OTP-4007
		{'EXIT', Reason} ->
		    {error,[{reason, Reason},
			    {token, Tokens},
			    {chars, Bin}]}
	    end;
	{error, Reason, Line} ->             %% OTP-4007
	   {error, [{reason, Reason, Line},  %% OTP-4007
		    {token, []},
		    {chars, Bin}]}
    end;
decode_message(EncodingConfig, Bin) when binary(Bin) ->
    {error, {bad_encoding_config, EncodingConfig}};
decode_message(EncodingConfig, BadBin) ->
    {error, bad_binary}.

%%----------------------------------------------------------------------
%% Define various macros used by the actual generator code
%%----------------------------------------------------------------------

-define(EQUAL,  [?EqualToken]).
-define(COLON,  [?ColonToken]).
-define(LBRKT,  [?LbrktToken]).
-define(RBRKT,  [?RbrktToken]).
-define(LSBRKT, [?LsbrktToken]).
-define(RSBRKT, [?RsbrktToken]).
-define(COMMA,  [?CommaToken]).
-define(DOT,    [?DotToken]).
-define(SLASH,  [?SlashToken]).
-define(DQUOTE, [?DoubleQuoteToken]).
-define(SP,     [?SpToken]).
-define(HTAB,   [?HtabToken]).
-define(CR,     [?CrToken]).
-define(LF,     [?LfToken]).
-define(LWSP,   []).
-define(EOL,    ?LF).
-define(WSP,    ?SP).
-define(SEP,    ?WSP).

-define(INIT_INDENT,          []).
-define(INC_INDENT(State),    State).
-define(INDENT(State),        State).
-define(LBRKT_INDENT(State),  [?LbrktToken]).
-define(RBRKT_INDENT(State),  [?RbrktToken]).
-define(COMMA_INDENT(State),  [?CommaToken]).
-define(SEP_INDENT(State),    [?LfToken]).

%%----------------------------------------------------------------------
%% Define token macros
%%----------------------------------------------------------------------

-define(AddToken                   , ?CompactAddToken).
-define(AuditToken                 , ?CompactAuditToken).
-define(AuditCapToken              , ?CompactAuditCapToken).
-define(AuditValueToken            , ?CompactAuditValueToken).
-define(AuthToken                  , ?CompactAuthToken).
-define(BothwayToken               , ?CompactBothwayToken).
-define(BriefToken                 , ?CompactBriefToken).
-define(BufferToken                , ?CompactBufferToken).
-define(CtxToken                   , ?CompactCtxToken).
-define(ContextAuditToken          , ?CompactContextAuditToken).
-define(DigitMapToken              , ?CompactDigitMapToken).
-define(DiscardToken               , ?CompactDiscardToken).
-define(DisconnectedToken          , ?CompactDisconnectedToken).
-define(DelayToken                 , ?CompactDelayToken).
-define(DeleteToken                , ?CompactDeleteToken).
-define(DurationToken              , ?CompactDurationToken).
-define(EmbedToken                 , ?CompactEmbedToken).
-define(EmergencyToken             , ?CompactEmergencyToken).
-define(ErrorToken                 , ?CompactErrorToken).
-define(EventBufferToken           , ?CompactEventBufferToken).
-define(EventsToken                , ?CompactEventsToken).
-define(FailoverToken              , ?CompactFailoverToken).
-define(ForcedToken                , ?CompactForcedToken).
-define(GracefulToken              , ?CompactGracefulToken).
-define(H221Token                  , ?CompactH221Token).
-define(H223Token                  , ?CompactH223Token).
-define(H226Token                  , ?CompactH226Token).
-define(HandOffToken               , ?CompactHandOffToken).
-define(ImmAckRequiredToken        , ?CompactImmAckRequiredToken).
-define(InactiveToken              , ?CompactInactiveToken).
-define(InterruptByEventToken      , ?CompactInterruptByEventToken).
-define(InterruptByNewSignalsDescrToken, ?CompactInterruptByNewSignalsDescrToken).
-define(IsolateToken               , ?CompactIsolateToken).
-define(InSvcToken                 , ?CompactInSvcToken).
-define(KeepActiveToken            , ?CompactKeepActiveToken).
-define(LocalToken                 , ?CompactLocalToken).
-define(LocalControlToken          , ?CompactLocalControlToken).
-define(LockStepToken              , ?CompactLockStepToken).
-define(LoopbackToken              , ?CompactLoopbackToken).
-define(MediaToken                 , ?CompactMediaToken).
-define(MegacopToken               , ?CompactMegacopToken).
-define(MethodToken                , ?CompactMethodToken).
-define(MgcIdToken                 , ?CompactMgcIdToken).
-define(ModeToken                  , ?CompactModeToken).
-define(ModifyToken                , ?CompactModifyToken).
-define(ModemToken                 , ?CompactModemToken).
-define(MoveToken                  , ?CompactMoveToken).
-define(MtpToken                   , ?CompactMtpToken).
-define(MuxToken                   , ?CompactMuxToken).
-define(NotifyToken                , ?CompactNotifyToken).
-define(NotifyCompletionToken      , ?CompactNotifyCompletionToken).
-define(ObservedEventsToken        , ?CompactObservedEventsToken).
-define(OffToken                   , ?CompactOffToken).
-define(OnewayToken                , ?CompactOnewayToken).
-define(OnOffToken                 , ?CompactOnOffToken).
-define(OnToken                    , ?CompactOnToken).
-define(OtherReasonToken           , ?CompactOtherReasonToken).
-define(OutOfSvcToken              , ?CompactOutOfSvcToken).
-define(PackagesToken              , ?CompactPackagesToken).
-define(PendingToken               , ?CompactPendingToken).
-define(PriorityToken              , ?CompactPriorityToken).
-define(ProfileToken               , ?CompactProfileToken).
-define(ReasonToken                , ?CompactReasonToken).
-define(RecvonlyToken              , ?CompactRecvonlyToken).
-define(ReplyToken                 , ?CompactReplyToken).
-define(ResponseAckToken           , ?CompactResponseAckToken).
-define(RestartToken               , ?CompactRestartToken).
-define(RemoteToken                , ?CompactRemoteToken).
-define(ReservedGroupToken         , ?CompactReservedGroupToken).
-define(ReservedValueToken         , ?CompactReservedValueToken).
-define(SendonlyToken              , ?CompactSendonlyToken).
-define(SendrecvToken              , ?CompactSendrecvToken).
-define(ServicesToken              , ?CompactServicesToken).
-define(ServiceStatesToken         , ?CompactServiceStatesToken).
-define(ServiceChangeToken         , ?CompactServiceChangeToken).
-define(ServiceChangeAddressToken  , ?CompactServiceChangeAddressToken).
-define(SignalListToken            , ?CompactSignalListToken).
-define(SignalsToken               , ?CompactSignalsToken).
-define(SignalTypeToken            , ?CompactSignalTypeToken).
-define(StatsToken                 , ?CompactStatsToken).
-define(StreamToken                , ?CompactStreamToken).
-define(SubtractToken              , ?CompactSubtractToken).
-define(SynchISDNToken             , ?CompactSynchISDNToken).
-define(TerminationStateToken      , ?CompactTerminationStateToken).
-define(TestToken                  , ?CompactTestToken).
-define(TimeOutToken               , ?CompactTimeOutToken).
-define(TopologyToken              , ?CompactTopologyToken).
-define(TransToken                 , ?CompactTransToken).
-define(V18Token                   , ?CompactV18Token).
-define(V22Token                   , ?CompactV22Token).
-define(V22bisToken                , ?CompactV22bisToken).
-define(V32Token                   , ?CompactV32Token).
-define(V32bisToken                , ?CompactV32bisToken).
-define(V34Token                   , ?CompactV34Token).
-define(V76Token                   , ?CompactV76Token).
-define(V90Token                   , ?CompactV90Token).
-define(V91Token                   , ?CompactV91Token).
-define(VersionToken               , ?CompactVersionToken).

%%----------------------------------------------------------------------
%% Include the generator code
%%----------------------------------------------------------------------

-include("megaco_text_gen.hrl").

