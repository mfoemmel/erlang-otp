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
%%% Purpose: Encode PRETTY Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_pretty_text_encoder).

-behaviour(megaco_encoder).

-export([encode_message/2,
	 decode_message/2,
	 encode_transaction/1,
	 encode_command_request/1,
	 encode_action_reply/1]).

-export([trim_quoted_string/1,
	 term_to_compact_string/1,
	 term_to_pretty_string/1]).

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
	    case (catch megaco_text_parser:parse(Tokens)) of
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
	{error, Reason, Line} ->              %% OTP-4007
	   {error, [{reason, Reason, Line},   %% OTP-4007
		    {token, []},
		    {chars, Bin}]}
    end;
decode_message([{flex, Port}], Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, LastLine} ->
	    case (catch megaco_text_parser:parse(Tokens)) of
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
	{error, Reason, Line} ->              %% OTP-4007
	   {error, [{reason, Reason, Line},   %% OTP-4007
		    {token, []},
		    {chars, Bin}]}
    end;
decode_message(EncodingConfig, Bin) when binary(Bin) ->
    {error, {bad_encoding_config, EncodingConfig}};
decode_message(EncodingConfig, BadBin) ->
    {error, bad_binary}.

%%----------------------------------------------------------------------
%% Convert a transaction record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction(Trans) ->
    case catch enc_Transaction(Trans) of
	{'EXIT', Reason} ->
	    {error, Reason};
	DeepIoList ->
	    {ok, DeepIoList}
    end.
	    
%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request(CmdReq) ->
    case catch enc_CommandRequest(CmdReq) of
	{'EXIT', Reason} ->
	    {error, Reason};
	DeepIoList ->
	    {ok, DeepIoList}
    end.
	    
%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply(ActRep) ->
    case catch enc_ActionReply(ActRep) of
	{'EXIT', Reason} ->
	    {error, Reason};
	DeepIoList ->
	    {ok, DeepIoList}
    end.
	    
%%----------------------------------------------------------------------
term_to_compact_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~w", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
term_to_pretty_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~p", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
trim_quoted_string([H | T]) ->
    case ?classify_char(H) of
	safe_char   -> [H  | trim_quoted_string(T)];
	rest_char   -> [H  | trim_quoted_string(T)];
	white_space -> [H  | trim_quoted_string(T)];
	BadChar     -> [$? | trim_quoted_string(T)]
    end;
trim_quoted_string([]) ->
    [].

%%----------------------------------------------------------------------
%% Define various macros used by the actual generator code
%%----------------------------------------------------------------------

-define(EQUAL,  [?SpToken, ?EqualToken, ?SpToken]).
-define(COLON,  [?ColonToken]).
-define(LBRKT,  [?SpToken, ?LbrktToken, ?SpToken]).
-define(RBRKT,  [?SpToken, ?RbrktToken, ?SpToken]).
-define(LSBRKT, [?LsbrktToken]).
-define(RSBRKT, [?RsbrktToken]).
-define(COMMA,  [?CommaToken, ?SpToken]).
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
-define(INC_INDENT(State),    [?HtabToken | State]).
-define(INDENT(State),        [?LfToken | State]).
-define(LBRKT_INDENT(State),  [?SpToken, ?LbrktToken, ?INDENT(?INC_INDENT(State))]).
-define(RBRKT_INDENT(State),  [?INDENT(State), ?RbrktToken]).
-define(COMMA_INDENT(State),  [?CommaToken, ?INDENT(State)]).
-define(SEP_INDENT(State),    [?LfToken]).

%%----------------------------------------------------------------------
%% Define token macros
%%----------------------------------------------------------------------

-define(AddToken                   , ?PrettyAddToken).
-define(AuditToken                 , ?PrettyAuditToken).
-define(AuditCapToken              , ?PrettyAuditCapToken).
-define(AuditValueToken            , ?PrettyAuditValueToken).
-define(AuthToken                  , ?PrettyAuthToken).
-define(BothwayToken               , ?PrettyBothwayToken).
-define(BriefToken                 , ?PrettyBriefToken).
-define(BufferToken                , ?PrettyBufferToken).
-define(CtxToken                   , ?PrettyCtxToken).
-define(ContextAuditToken          , ?PrettyContextAuditToken).
-define(DigitMapToken              , ?PrettyDigitMapToken).
-define(DiscardToken               , ?PrettyDiscardToken).
-define(DisconnectedToken          , ?PrettyDisconnectedToken).
-define(DelayToken                 , ?PrettyDelayToken).
-define(DeleteToken                , ?PrettyDeleteToken).
-define(DurationToken              , ?PrettyDurationToken).
-define(EmbedToken                 , ?PrettyEmbedToken).
-define(EmergencyToken             , ?PrettyEmergencyToken).
-define(ErrorToken                 , ?PrettyErrorToken).
-define(EventBufferToken           , ?PrettyEventBufferToken).
-define(EventsToken                , ?PrettyEventsToken).
-define(FailoverToken              , ?PrettyFailoverToken).
-define(ForcedToken                , ?PrettyForcedToken).
-define(GracefulToken              , ?PrettyGracefulToken).
-define(H221Token                  , ?PrettyH221Token).
-define(H223Token                  , ?PrettyH223Token).
-define(H226Token                  , ?PrettyH226Token).
-define(HandOffToken               , ?PrettyHandOffToken).
-define(ImmAckRequiredToken        , ?PrettyImmAckRequiredToken).
-define(InactiveToken              , ?PrettyInactiveToken).
-define(IsolateToken               , ?PrettyIsolateToken).
-define(InSvcToken                 , ?PrettyInSvcToken).
-define(InterruptByEventToken      , ?PrettyInterruptByEventToken).
-define(InterruptByNewSignalsDescrToken, ?PrettyInterruptByNewSignalsDescrToken).
-define(KeepActiveToken            , ?PrettyKeepActiveToken).
-define(LocalToken                 , ?PrettyLocalToken).
-define(LocalControlToken          , ?PrettyLocalControlToken).
-define(LockStepToken              , ?PrettyLockStepToken).
-define(LoopbackToken              , ?PrettyLoopbackToken).
-define(MediaToken                 , ?PrettyMediaToken).
-define(MegacopToken               , ?PrettyMegacopToken).
-define(MethodToken                , ?PrettyMethodToken).
-define(MgcIdToken                 , ?PrettyMgcIdToken).
-define(ModeToken                  , ?PrettyModeToken).
-define(ModifyToken                , ?PrettyModifyToken).
-define(ModemToken                 , ?PrettyModemToken).
-define(MoveToken                  , ?PrettyMoveToken).
-define(MtpToken                   , ?PrettyMtpToken).
-define(MuxToken                   , ?PrettyMuxToken).
-define(NotifyToken                , ?PrettyNotifyToken).
-define(NotifyCompletionToken      , ?PrettyNotifyCompletionToken).
-define(ObservedEventsToken        , ?PrettyObservedEventsToken).
-define(OffToken                   , ?PrettyOffToken).
-define(OnewayToken                , ?PrettyOnewayToken).
-define(OnOffToken                 , ?PrettyOnOffToken).
-define(OnToken                    , ?PrettyOnToken).
-define(OtherReasonToken           , ?PrettyOtherReasonToken).
-define(OutOfSvcToken              , ?PrettyOutOfSvcToken).
-define(PackagesToken              , ?PrettyPackagesToken).
-define(PendingToken               , ?PrettyPendingToken).
-define(PriorityToken              , ?PrettyPriorityToken).
-define(ProfileToken               , ?PrettyProfileToken).
-define(ReasonToken                , ?PrettyReasonToken).
-define(RecvonlyToken              , ?PrettyRecvonlyToken).
-define(ReplyToken                 , ?PrettyReplyToken).
-define(ResponseAckToken           , ?PrettyResponseAckToken).
-define(RestartToken               , ?PrettyRestartToken).
-define(RemoteToken                , ?PrettyRemoteToken).
-define(ReservedGroupToken         , ?PrettyReservedGroupToken).
-define(ReservedValueToken         , ?PrettyReservedValueToken).
-define(SendonlyToken              , ?PrettySendonlyToken).
-define(SendrecvToken              , ?PrettySendrecvToken).
-define(ServicesToken              , ?PrettyServicesToken).
-define(ServiceStatesToken         , ?PrettyServiceStatesToken).
-define(ServiceChangeToken         , ?PrettyServiceChangeToken).
-define(ServiceChangeAddressToken  , ?PrettyServiceChangeAddressToken).
-define(SignalListToken            , ?PrettySignalListToken).
-define(SignalsToken               , ?PrettySignalsToken).
-define(SignalTypeToken            , ?PrettySignalTypeToken).
-define(StatsToken                 , ?PrettyStatsToken).
-define(StreamToken                , ?PrettyStreamToken).
-define(SubtractToken              , ?PrettySubtractToken).
-define(SynchISDNToken             , ?PrettySynchISDNToken).
-define(TerminationStateToken      , ?PrettyTerminationStateToken).
-define(TestToken                  , ?PrettyTestToken).
-define(TimeOutToken               , ?PrettyTimeOutToken).
-define(TopologyToken              , ?PrettyTopologyToken).
-define(TransToken                 , ?PrettyTransToken).
-define(V18Token                   , ?PrettyV18Token).
-define(V22Token                   , ?PrettyV22Token).
-define(V22bisToken                , ?PrettyV22bisToken).
-define(V32Token                   , ?PrettyV32Token).
-define(V32bisToken                , ?PrettyV32bisToken).
-define(V34Token                   , ?PrettyV34Token).
-define(V76Token                   , ?PrettyV76Token).
-define(V90Token                   , ?PrettyV90Token).
-define(V91Token                   , ?PrettyV91Token).
-define(VersionToken               , ?PrettyVersionToken).

%%----------------------------------------------------------------------
%% Include the generator code
%%----------------------------------------------------------------------

-include("megaco_text_gen.hrl").

