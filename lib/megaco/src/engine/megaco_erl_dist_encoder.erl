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
%% Purpose: Externalize/internalize Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_erl_dist_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3
	]).
-export([version_of/2]).

%% Backward compatible funcs:
-export([encode_message/2, decode_message/2]).


-include("megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(Config, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(Config, V, MegaMsg).

encode_message([megaco_compressed|Config], _Vsn, MegaMsg) 
  when record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(encode(MegaMsg), Config)};
encode_message([{megaco_compressed, Mod}|Config], _Vsn, MegaMsg) 
  when atom(Mod), record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(Mod:encode(MegaMsg), Config)};
encode_message(Config, _Vsn, MegaMsg) when record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(MegaMsg, Config)};
encode_message(_Config, _Vsn, _MegaMsg) ->
    {error, not_a_megaco_message}.


%%----------------------------------------------------------------------
%% Convert a transaction record into a binary
%% Return {ok, Bin} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction([megaco_compressed|Config], _Vsn, Trans) ->
    {ok, erlang:term_to_binary(encode(Trans), Config)};
encode_transaction([{megaco_compressed, Mod}|Config], _Vsn, Trans) ->
    {ok, erlang:term_to_binary(Mod:encode(Trans), Config)};
encode_transaction(Config, _Vsn, Trans) ->
    {ok, erlang:term_to_binary(Trans, Config)}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests([megaco_compressed|Config], _Vsn, ActReqs0) 
  when list(ActReqs0) ->
    ActReqs = [encode(AR) || AR <- ActReqs0],
    {ok, erlang:term_to_binary(ActReqs, Config)};
encode_action_requests([{megaco_compressed, Mod}|Config], _Vsn, ActReqs0) 
  when list(ActReqs0) ->
    ActReqs = [Mod:encode(AR) || AR <- ActReqs0],
    {ok, erlang:term_to_binary(ActReqs, Config)};
encode_action_requests(Config, _Vsn, ActReqs) 
  when list(ActReqs) ->
    {ok, erlang:term_to_binary(ActReqs, Config)}.



%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request([megaco_compressed|Config], _Vsn, ActReq) 
  when tuple(ActReq) ->
    {ok, erlang:term_to_binary(encode(ActReq), Config)};
encode_action_request([{megaco_compressed, Mod}|Config], _Vsn, ActReq) 
  when tuple(ActReq) ->
    {ok, erlang:term_to_binary(Mod:encode(ActReq), Config)};
encode_action_request(Config, _Vsn, ActReq) 
  when tuple(ActReq) ->
    {ok, erlang:term_to_binary(ActReq, Config)}.



%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

version_of(Config, Bin) when binary(Bin) ->
    case decode_message(Config, 1, Bin) of
	{ok, M} ->
	    V = (M#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Error ->
	    Error
    end.
				   
decode_message(Config, Bin) ->
    decode_message(Config, 1, Bin).
				   
decode_message([megaco_compressed = MC|_Config], _Vsn, Bin) ->
    case catch erlang:binary_to_term(Bin) of
	Msg when tuple(Msg) ->
	    case (decode(Msg)) of
		MegaMsg when record(MegaMsg, 'MegacoMessage') ->
		    {ok, dm(MegaMsg, MC)};
		_ ->
		    {error, {bad_message, Msg}}
	    end;
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end;
decode_message([{megaco_compressed, Mod} = MC|_Config], _Vsn, Bin) when atom(Mod) ->
    case catch erlang:binary_to_term(Bin) of
	Msg when tuple(Msg) ->
	    case (Mod:decode(Msg)) of
		MegaMsg when record(MegaMsg, 'MegacoMessage') ->
		    {ok, dm(MegaMsg, MC)};
		_ ->
		    {error, {bad_message, Msg}}
	    end;
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end;
decode_message(_Config, _Vsn, Bin) ->
    case catch erlang:binary_to_term(Bin) of
	MegaMsg when record(MegaMsg, 'MegacoMessage') ->
	    {ok, dm(MegaMsg, undefined)};
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end.


%% This crap is because the transactions or the action-requests
%% might have been encoded separetely

dm(#'MegacoMessage'{mess = Mess} = M, MC) ->
    #'Message'{messageBody = Body} = Mess,
    case Body of
	{transactions, Transactions} ->
	    Body2 = {transactions, dmt(Transactions, [], MC)},
	    Mess2 = Mess#'Message'{messageBody = Body2},
	    M#'MegacoMessage'{mess = Mess2};
	_ ->
	    M
    end.

dmt([], Acc, _) ->
    lists:reverse(Acc);
dmt([Trans0|Transactions], Acc, MC) when binary(Trans0) ->
    Trans1 = erlang:binary_to_term(Trans0),
    Trans2 = dmt1(Trans1, MC),
    dmt(Transactions, [Trans2|Acc], MC);
dmt([{Tag, Trans0}|Transactions], Acc, MC) when binary(Trans0) ->
    Trans1 = erlang:binary_to_term(Trans0),
    Trans2 = dmt1(Trans1, MC),
    dmt(Transactions, [{Tag, Trans2}|Acc], MC);
dmt([{transactionRequest, 
      #'TransactionRequest'{actions = Acts0} = TR0}|Transactions], 
    Acc, MC) 
  when binary(Acts0) ->
    Acts1 = erlang:binary_to_term(Acts0),
    Acts2 = dmt1(Acts1, MC),
    TR1 = TR0#'TransactionRequest'{actions = Acts2},
    dmt(Transactions, [{transactionRequest, TR1}|Acc], MC);
dmt([{transactionRequest, 
      #'TransactionRequest'{actions = Acts0} = TR0}|Transactions], 
    Acc, MC) ->
    Acts2 = [dmt2(AR, MC) || AR <- Acts0],
    TR1 = TR0#'TransactionRequest'{actions = Acts2},
    dmt(Transactions, [{transactionRequest, TR1}|Acc], MC);
dmt([Trans|Transactions], Acc, MC) ->
    dmt(Transactions, [Trans|Acc], MC).

dmt1(L, megaco_compressed) when list(L) ->
    [decode(E) || E <- L];
dmt1(L, {megaco_compressed, Mod}) when list(L) ->
    [Mod:decode(E) || E <- L];
dmt1(T, megaco_compressed) when tuple(T) ->
    decode(T);
dmt1(T, {megaco_compressed, Mod}) when tuple(T) ->
    Mod:decode(T);
dmt1(Else, _) ->
    Else.
	    
dmt2(Bin, MC) when binary(Bin) ->
    AR = erlang:binary_to_term(Bin),
    dmt1(AR, MC);
dmt2(AR, _MC) ->
    AR.

%% -------

encode(M) -> e(M, 1).
% encode(M) -> 
%     i("encode -> entry with"
%       "~nM: ~p", [M]),
%     M1 = e(M, 1),
%     i("encode -> encoded:"
%       "~nM1: ~p", [M1]),
%     M1.
decode(M) -> d(M, 1).
% decode(M) -> 
%     i("decode -> entry with"
%       "~nM: ~p", [M]),
%     M1 = d(M, 1),
%     i("decode -> decoded:"
%       "~nM1: ~p", [M1]),
%     M1.

el(L, V)  when list(L) -> [e(T, V) || T <- L];
el(L, _V)              -> L.
dl(L, V)  when list(L) -> [d(T, V) || T <- L];
dl(L, _V)              -> L.

ell(L, V) when list(L) -> [el(T, V) || T <- L];
ell(L, _V)             -> L.
dll(L, V) when list(L) -> [dl(T, V) || T <- L];
dll(L, _V)             -> L.

e(asn1_NOVALUE, _) ->
    {1};
e('NULL', _V) ->
    {2};
e(sendRecv, _V) ->
    {3};
e(recvOnly, _V) ->
    {4};
e(restart, _V) ->
    {5};
e(mediaToken, _V) ->
    {6};
e(eventsToken, _V) ->
    {7};
e(signalsToken, _V) ->
    {8};
e(digitMapToken, _V) ->
    {9};
e(statsToken, _V) ->
    {10};
e(packagesToken, _V) ->
    {11};
e(h221, _V) ->
    {12};
e(h223, _V) ->
    {13};
e(h226, _V) ->
    {14};
e(v76, _V) ->
    {15};

e({'MegacoMessage', asn1_NOVALUE, {'Message', 1 = V, Mid, Body}}, _) ->
    {20, e(Mid, V), e(Body, V)};
e({'MegacoMessage', asn1_NOVALUE, {'Message', 2 = V, Mid, Body}}, _) ->
    {21, e(Mid, V), e(Body, V)};
e({'MegacoMessage', asn1_NOVALUE, {'Message', V, Mid, Body}}, _) ->
    {22, V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', 1 = V, Mid, Body}}, _) ->
    {23, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', 2 = V, Mid, Body}}, _) ->
    {24, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, {'Message', V, Mid, Body}}, _) ->
    {25, V, e(AuthHeader, V), V, e(Mid, V), e(Body, V)};
e({'MegacoMessage', AuthHeader, Mess}, V) ->
    {26, e(AuthHeader, V), e(Mess, V)};
e({'Message', V, Mid, Body}, _) ->
    {27, V, e(Mid, V), e(Body, V)};

e({domainName, {'DomainName', Name, asn1_NOVALUE}}, _V) ->
    {30, Name};
e({domainName, {'DomainName', Name, PortNumber}}, _V) ->
    {31, Name, PortNumber};
e({domainName, N}, V) ->
    {32, e(N, V)};
e({'DomainName', Name, asn1_NOVALUE}, _V) ->
    {33, Name};
e({'DomainName', Name, PortNumber}, _V) ->
    {34, Name, PortNumber};
e({ip4Address, {'IP4Address', Addr, asn1_NOVALUE}}, _V) ->
    {35, Addr};
e({ip4Address, {'IP4Address', Addr, PortNumber}}, _V) ->
    {36, Addr, PortNumber};
e({ip4Address, A}, V) ->
    {37, e(A, V)};
e({'IP4Address', Addr, asn1_NOVALUE}, _V) ->
    {38, Addr};
e({'IP4Address', Addr, PortNumber}, _V) ->
    {39, Addr, PortNumber};
e({ip6Address, {'IP6Address', Addr, asn1_NOVALUE}}, _V) ->
    {40, Addr};
e({ip6Address, {'IP6Address', Addr, PortNumber}}, _V) ->
    {41, Addr, PortNumber};
e({ip6Address, A}, V) ->
    {42, e(A, V)};
e({'IP6Address', Addr, asn1_NOVALUE}, _V) ->
    {43, Addr};
e({'IP6Address', Addr, PortNumber}, _V) ->
    {44, Addr, PortNumber};

e({transactions, [Transaction]}, V) ->
    {50, e(Transaction, V)};
e({transactions, Transactions}, V) ->
    {51, el(Transactions, V)};
e({messageError, {'ErrorDescriptor', EC, asn1_NOVALUE}}, _V) ->
    {52, EC};
e({messageError, {'ErrorDescriptor', EC, ET}}, _V) ->
    {53, EC, ET};
e({messageError, Error}, V) ->
    {54, e(Error, V)};
e({transactionRequest, {'TransactionRequest', TransId, Actions}}, V) ->
    {55, TransId, el(Actions, V)};
e({transactionPending, {'TransactionPending', TransId}}, _V) ->
    {56, TransId};
e({transactionReply, {'TransactionReply', TransId, asn1_NOVALUE, TransRes}}, V) ->
    {57, TransId, e(TransRes, V)};
e({transactionReply, {'TransactionReply', TransId, 'NULL', TransRes}}, V) ->
    {58, TransId, e(TransRes, V)};
e({transactionReply, {'TransactionReply', TransId, ImmAckReq, TransRes}}, V) ->
    {59, TransId, e(ImmAckReq, V), e(TransRes, V)};
e({transactionResponseAck, T}, V) ->
    {60, el(T, V)};
e({'TransactionAck', FirstAck, asn1_NOVALUE}, _V) ->
    {61, FirstAck};
e({'TransactionAck', FirstAck, LastAck}, _V) ->
    {62, FirstAck, LastAck};

e({'ErrorDescriptor', EC, asn1_NOVALUE}, _V) ->
    {70, EC};
e({'ErrorDescriptor', EC, ET}, _V) ->
    {71, EC, ET};

e({'ActionRequest', Cid, CtxReq, CtxAAR, [CmdReq]}, V) ->
    {80, Cid, e(CtxReq, V), e(CtxAAR, V), e(CmdReq, V)};
e({'ActionRequest', Cid, CtxReq, CtxAAR, CmdReqs}, V) ->
    {81, Cid, e(CtxReq, V), e(CtxAAR, V), el(CmdReqs, V)};

e({'ContextRequest', P, E, T}, V) ->
    {90, e(P, V), e(E, V), e(T, V)};

e({'ContextAttrAuditRequest', P, E, T}, V) ->
    {100, e(P, V), e(E, V), e(T, V)};

e({'CommandRequest', Cmd, asn1_NOVALUE, asn1_NOVALUE}, V) ->
    {110, e(Cmd, V)};
e({'CommandRequest', Cmd, 'NULL', asn1_NOVALUE}, V) ->
    {111, e(Cmd, V)};
e({'CommandRequest', Cmd, asn1_NOVALUE, 'NULL'}, V) ->
    {112, e(Cmd, V)};
e({'CommandRequest', Cmd, 'NULL', 'NULL'}, V) ->
    {113, e(Cmd, V)};
e({'CommandRequest', Cmd, Opt, WR}, V) ->
    {114, e(Cmd, V), e(Opt, V), e(WR, V)};

e({'TopologyRequest', From, To, Dir}, 1 = V) ->
    {120, e(From, V), e(To, V), e(Dir, V)};
e({'TopologyRequest', From, To, Dir, SID}, 2 = V) ->
    {121, e(From, V), e(To, V), e(Dir, V), e(SID, V)};

e({modReq, {'AmmRequest', TID, []}}, V) ->
    {130, el(TID, V)};
e({modReq, {'AmmRequest', TID, [Desc]}}, V) ->
    {131, el(TID, V), e(Desc, V)};
e({modReq, {'AmmRequest', TID, Descs}}, V) ->
    {132, el(TID, V), el(Descs, V)};
e({addReq, {'AmmRequest', TID, []}}, V) ->
    {133, el(TID, V)};
e({addReq, {'AmmRequest', TID, [Desc]}}, V) ->
    {134, el(TID, V), e(Desc, V)};
e({addReq, {'AmmRequest', TID, Descs}}, V) ->
    {135, el(TID, V), el(Descs, V)};
e({'AmmRequest', TID, Descs}, V) ->
    {136, el(TID, V), el(Descs, V)};

e({subtractReq, {'SubtractRequest', TID, asn1_NOVALUE}}, V) ->
    {140, el(TID, V)};
e({subtractReq, {'SubtractRequest', TID, AudDesc}}, V) ->
    {141, el(TID, V), e(AudDesc, V)};
e({'SubtractRequest', TID, asn1_NOVALUE}, V) ->
    {142, el(TID, V)};
e({'SubtractRequest', TID, AudDesc}, V) ->
    {143, el(TID, V), e(AudDesc, V)};

e({auditValueRequest, AR}, V) ->
    {150, e(AR, V)};

e({'AuditRequest', TID, AudDesc}, V) ->
    {160, e(TID, V), e(AudDesc, V)};

e({actionReplies, [AR]}, V) ->
    {170, e(AR, V)};
e({actionReplies, ARs}, V) ->
    {171, el(ARs, V)};

e({'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, [CmdRep]}, V) ->
    {180, CID, e(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, CmdRep}, V) ->
    {181, CID, el(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, CtxRep, [CmdRep]}, V) ->
    {182, CID, e(CtxRep, V), e(CmdRep, V)};
e({'ActionReply', CID, asn1_NOVALUE, CtxRep, CmdRep}, V) ->
    {183, CID, e(CtxRep, V), el(CmdRep, V)};
e({'ActionReply', CID, ED, asn1_NOVALUE, [CmdRep]}, V) ->
    {184, CID, e(ED, V), e(CmdRep, V)};
e({'ActionReply', CID, ED, asn1_NOVALUE, CmdRep}, V) ->
    {185, CID, e(ED, V), el(CmdRep, V)};
e({'ActionReply', CID, ED, CtxRep, [CmdRep]}, V) ->
    {186, CID, e(ED, V), e(CtxRep, V), e(CmdRep, V)};
e({'ActionReply', CID, ED, CtxRep, CmdRep}, V) ->
    {187, CID, e(ED, V), e(CtxRep, V), el(CmdRep, V)};

e({'AuditDescriptor', asn1_NOVALUE}, 1 = _V) ->
    {190};
e({'AuditDescriptor', AT}, 1 = V) ->
    {191, el(AT, V)};
e({'AuditDescriptor', asn1_NOVALUE, asn1_NOVALUE}, 2 = _V) ->
    {192};
e({'AuditDescriptor', AT, APT}, 2 = V) when list(AT), list(APT) ->
    {193, el(AT, V), el(APT, V)};
e({'AuditDescriptor', AT, APT}, 2 = V) when list(APT) ->
    {194, e(AT, V), el(APT, V)};
e({'AuditDescriptor', AT, APT}, 2 = V) when list(AT) ->
    {195, el(AT, V), e(APT, V)};
e({'AuditDescriptor', AT, APT}, 2 = V) ->
    {196, e(AT, V), e(APT, V)};

e({notifyReq, {'NotifyRequest', TID, OED, asn1_NOVALUE}}, V) ->
    {200, el(TID, V), e(OED, V)};
e({notifyReq, {'NotifyRequest', TID, OED, ED}}, V) ->
    {201, el(TID, V), e(OED, V), e(ED, V)};
e({'NotifyRequest', TID, OED}, V) ->
    {202, el(TID, V), e(OED, V)};
e({'NotifyRequest', TID, OED, ED}, V) ->
    {203, el(TID, V), e(OED, V), e(ED, V)};

e({'ObservedEventsDescriptor', RID, OEL}, V) ->
    {210, RID, el(OEL, V)};

e({'ObservedEvent', EN, SID, EPL, TN}, V) ->
    {220, EN, e(SID, V), el(EPL, V), e(TN, V)};

e({'EventParameter', "type", ["est"], asn1_NOVALUE}, _V) ->
    {230};
e({'EventParameter', "type", [Val], asn1_NOVALUE}, _V) ->
    {231, Val};
e({'EventParameter', "type", Val, asn1_NOVALUE}, _V) ->
    {232, Val};
e({'EventParameter', "Generalcause", ["NR"], asn1_NOVALUE}, _V) ->
    {233};
e({'EventParameter', "Generalcause", ["UR"], asn1_NOVALUE}, _V) ->
    {234};
e({'EventParameter', "Generalcause", ["FT"], asn1_NOVALUE}, _V) ->
    {235};
e({'EventParameter', "Generalcause", ["FP"], asn1_NOVALUE}, _V) ->
    {236};
e({'EventParameter', "Generalcause", ["IW"], asn1_NOVALUE}, _V) ->
    {237};
e({'EventParameter', "Generalcause", ["UN"], asn1_NOVALUE}, _V) ->
    {238};
e({'EventParameter', "Generalcause", [Val], asn1_NOVALUE}, _V) ->
    {239, Val};
e({'EventParameter', "Generalcause", Val, asn1_NOVALUE}, _V) ->
    {240, Val};
e({'EventParameter', "Failurecause", [Val], asn1_NOVALUE}, _V) ->
    {241, Val};
e({'EventParameter', "Failurecause", Val, asn1_NOVALUE}, _V) ->
    {242, Val};
e({'EventParameter', EPN, Val, asn1_NOVALUE}, _V) ->
    {243, EPN, Val};
e({'EventParameter', EPN, Val, EI}, _V) ->
    {244, EPN, Val, EI};

e({serviceChangeReq, {'ServiceChangeRequest', TID, SCPs}}, V) ->
    {260, el(TID, V), e(SCPs, V)};
e({serviceChangeReq, SCR}, V) ->
    {261, e(SCR, V)};
e({'ServiceChangeRequest', TID, SCPs}, V) ->
    {262, el(TID, V), e(SCPs, V)};

e({serviceChangeReply, {'ServiceChangeReply', TID, SCR}}, V) ->
    {270, el(TID, V), e(SCR, V)};
e({serviceChangeReply, SCR}, V) ->
    {271, e(SCR, V)};
e({'ServiceChangeReply', TID, SCR}, V) -> %% KOLLA
    {272, el(TID, V), e(SCR, V)};

e({mediaDescriptor, {'MediaDescriptor', TSD, S}}, V) ->
    {280, e(TSD, V), e(S, V)};
e({mediaDescriptor, MD}, V) ->
    {281, e(MD, V)};
e({'MediaDescriptor', TSD, S}, V) ->
    {282, e(TSD, V), e(S, V)};

e({oneStream, S}, V) ->
    {290, e(S, V)};
e({multiStream, S}, V) ->
    {291, el(S, V)};
e({'StreamDescriptor', SID, SP}, V) ->
    {292, e(SID, V), e(SP, V)};

e({'StreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE}, V) ->
    {300, e(LCD, V)};
e({'StreamParms', LCD, LD, asn1_NOVALUE}, V) ->
    {301, e(LCD, V), e(LD, V)};
e({'StreamParms', LCD, LD, RD}, V) ->
    {302, e(LCD, V), e(LD, V), e(RD, V)};

e({'LocalControlDescriptor', SM, RV, RG, PP}, V) ->
    {310, e(SM, V), e(RV, V), e(RG, V), el(PP, V)};

e({'PropertyParm', "v", [Val], asn1_NOVALUE}, _V) ->
    {320, Val};
e({'PropertyParm', "v", Val, asn1_NOVALUE}, _V) ->
    {321, Val};
e({'PropertyParm', "o", [Val], asn1_NOVALUE}, _V) ->
    {332, Val};
e({'PropertyParm', "o", Val, asn1_NOVALUE}, _V) ->
    {333, Val};
e({'PropertyParm', "s", [Val], asn1_NOVALUE}, _V) ->
    {334, Val};
e({'PropertyParm', "s", Val, asn1_NOVALUE}, _V) ->
    {335, Val};
e({'PropertyParm', "i", [Val], asn1_NOVALUE}, _V) ->
    {336, Val};
e({'PropertyParm', "i", Val, asn1_NOVALUE}, _V) ->
    {337, Val};
e({'PropertyParm', "u", [Val], asn1_NOVALUE}, _V) ->
    {338, Val};
e({'PropertyParm', "u", Val, asn1_NOVALUE}, _V) ->
    {339, Val};
e({'PropertyParm', "e", [Val], asn1_NOVALUE}, _V) ->
    {340, Val};
e({'PropertyParm', "e", Val, asn1_NOVALUE}, _V) ->
    {341, Val};
e({'PropertyParm', "p", [Val], asn1_NOVALUE}, _V) ->
    {342, Val};
e({'PropertyParm', "p", Val, asn1_NOVALUE}, _V) ->
    {343, Val};
e({'PropertyParm', "c", [Val], asn1_NOVALUE}, _V) ->
    {344, Val};
e({'PropertyParm', "c", Val, asn1_NOVALUE}, _V) ->
    {345, Val};
e({'PropertyParm', "b", [Val], asn1_NOVALUE}, _V) ->
    {346, Val};
e({'PropertyParm', "b", Val, asn1_NOVALUE}, _V) ->
    {347, Val};
e({'PropertyParm', "z", [Val], asn1_NOVALUE}, _V) ->
    {348, Val};
e({'PropertyParm', "z", Val, asn1_NOVALUE}, _V) ->
    {349, Val};
e({'PropertyParm', "k", [Val], asn1_NOVALUE}, _V) ->
    {350, Val};
e({'PropertyParm', "k", Val, asn1_NOVALUE}, _V) ->
    {351, Val};
e({'PropertyParm', "a", [Val], asn1_NOVALUE}, _V) ->
    {352, Val};
e({'PropertyParm', "a", Val, asn1_NOVALUE}, _V) ->
    {353, Val};
e({'PropertyParm', "t", [Val], asn1_NOVALUE}, _V) ->
    {354, Val};
e({'PropertyParm', "t", Val, asn1_NOVALUE}, _V) ->
    {355, Val};
e({'PropertyParm', "r", [Val], asn1_NOVALUE}, _V) ->
    {356, Val};
e({'PropertyParm', "r", Val, asn1_NOVALUE}, _V) ->
    {357, Val};
e({'PropertyParm', "m", [Val], asn1_NOVALUE}, _V) ->
    {358, Val};
e({'PropertyParm', "m", Val, asn1_NOVALUE}, _V) ->
    {359, Val};
e({'PropertyParm', "nt/jit", [Val], asn1_NOVALUE}, _V) ->
    {360, Val};
e({'PropertyParm', "nt/jit", Val, asn1_NOVALUE}, _V) ->
    {361, Val};
e({'PropertyParm', "tdmc/ec", ["on"], asn1_NOVALUE}, _V) ->
    {362};
e({'PropertyParm', "tdmc/ec", ["off"], asn1_NOVALUE}, _V) ->
    {363};
e({'PropertyParm', "tdmc/gain", ["automatic"], asn1_NOVALUE}, _V) ->
    {364};
e({'PropertyParm', "tdmc/gain", [Val], asn1_NOVALUE}, _V) ->
    {365, Val};
e({'PropertyParm', "tdmc/gain", Val, asn1_NOVALUE}, _V) ->
    {366, Val};
e({'PropertyParm', "maxNumberOfContexts", [Val], asn1_NOVALUE}, _V) ->
    {367, Val};
e({'PropertyParm', "maxNumberOfContexts", Val, asn1_NOVALUE}, _V) ->
    {368, Val};
e({'PropertyParm', "maxTerminationsPerContext", [Val], asn1_NOVALUE}, _V) ->
    {369, Val};
e({'PropertyParm', "maxTerminationsPerContext", Val, asn1_NOVALUE}, _V) ->
    {370, Val};
e({'PropertyParm', "normalMGExecutionTime", [Val], asn1_NOVALUE}, _V) ->
    {371, Val};
e({'PropertyParm', "normalMGExecutionTime", Val, asn1_NOVALUE}, _V) ->
    {372, Val};
e({'PropertyParm', "normalMGCExecutionTime", [Val], asn1_NOVALUE}, _V) ->
    {373, Val};
e({'PropertyParm', "normalMGCExecutionTime", Val, asn1_NOVALUE}, _V) ->
    {374, Val};
e({'PropertyParm', "MGProvisionalResponseTimerValue", [Val], asn1_NOVALUE}, _V) ->
    {375, Val};
e({'PropertyParm', "MGProvisionalResponseTimerValue", Val, asn1_NOVALUE}, _V) ->
    {376, Val};
e({'PropertyParm', "MGCProvisionalResponseTimerValue", [Val], asn1_NOVALUE}, _V) ->
    {377, Val};
e({'PropertyParm', "MGCProvisionalResponseTimerValue", Val, asn1_NOVALUE}, _V) ->
    {378, Val};
e({'PropertyParm', N, [Val], asn1_NOVALUE}, _V) ->
    {379, N, Val};
e({'PropertyParm', N, Val, asn1_NOVALUE}, _V) ->
    {380, N, Val};
e({'PropertyParm', N, Val, EI}, _V) ->
    {381, N, Val, EI};

e({'LocalRemoteDescriptor', [[PG]]}, V) ->
    {400, e(PG, V)};
e({'LocalRemoteDescriptor', [PG]}, V) ->
    {401, el(PG, V)};
e({'LocalRemoteDescriptor', PG}, V) ->
    {402, ell(PG, V)};

e({'TerminationStateDescriptor', PP, EBC, SS}, V) ->
    {410, el(PP, V), e(EBC, V), e(SS, V)};

e({eventsDescriptor, {'EventsDescriptor', RID, [E]}}, V) ->
    {420, e(RID, V), e(E, V)};
e({eventsDescriptor, {'EventsDescriptor', RID, EL}}, V) ->
    {421, e(RID, V), el(EL, V)};
e({eventsDescriptor, ED}, V) ->
    {422, e(ED, V)};
e({'EventsDescriptor', RID, [E]}, V) ->
    {423, e(RID, V), e(E, V)};
e({'EventsDescriptor', RID, EL}, V) ->
    {424, e(RID, V), el(EL, V)};

e({'RequestedEvent', PN, SID, EA, EPL}, V) ->
    {430, PN, e(SID, V), e(EA, V), el(EPL, V)};

e({'RequestedActions', KA, EDM, SE, SD}, V) ->
    {440, e(KA, V), e(EDM, V), e(SE, V), e(SD, V)};

e({'SecondEventsDescriptor', RID, [E]}, V) ->
    {450, e(RID, V), e(E, V)};
e({'SecondEventsDescriptor', RID, EL}, V) ->
    {451, e(RID, V), el(EL, V)};

e({'SecondRequestedEvent', PN, SID, EA, EPL}, V) ->
    {460, PN, e(SID, V), e(EA, V), e(EPL, V)};

e({'SecondRequestedActions', KA, EDM, SD}, V) ->
    {470, e(KA, V), e(EDM, V), e(SD, V)};

e({'EventSpec', EN, SID, EPL}, V) ->
    {480, EN, e(SID, V), el(EPL, V)};

e({'SeqSigList', ID, SL}, V) ->
    {490, ID, el(SL, V)};

e({signalsDescriptor, S}, V) ->
    {500, el(S, V)};
e({signal, S}, V) ->
    {510, e(S, V)};

e({'Signal', SN, SID, ST, D, NC, KA, SPL}, V) ->
    {520, SN, e(SID, V), e(ST, V), e(D, V), e(NC, V), e(KA, V), el(SPL, V)};

e({'SigParameter', SPN, Val, asn1_NOVALUE}, _V) ->
    {530, SPN, Val};
e({'SigParameter', SPN, Val, EI}, _V) ->
    {531, SPN, Val, EI};

e({'ModemDescriptor', MTL, MPL, asn1_NOVALUE}, _V) ->
    {550, MTL, MPL};
e({'ModemDescriptor', MTL, MPL, NSD}, _V) ->
    {551, MTL, MPL, NSD};

e({digitMapDescriptor, {'DigitMapDescriptor', DMN, DMV}}, V) ->
    {560, DMN, e(DMV, V)};
e({digitMapDescriptor, DMD}, V) ->
    {561, e(DMD, V)};
e({'DigitMapDescriptor', DMN, DMV}, V) ->
    {562, DMN, e(DMV, V)};

e({'DigitMapValue', Start, Stop, Long, DMB}, 1 = V) ->
    {570, e(Start, V), e(Stop, V), e(Long, V), DMB};
e({'DigitMapValue', Start, Stop, Long, DMB, Dur}, 2 = V) ->
    {571, e(Start, V), e(Stop, V), e(Long, V), DMB, e(Dur, V)};

e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, asn1_NOVALUE, asn1_NOVALUE}, V) ->
    {580, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V)};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, asn1_NOVALUE}, V) ->
    {581, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V)};
e({'ServiceChangeParm', M, A, Ver, Prof, R, D, Id, TS, NSD}, V) ->
    {582, e(M, V), e(A, V), e(Ver, V), e(Prof, V), R, e(D, V), e(Id, V), 
     e(TS, V), NSD};

e({serviceChangeResParms, {'ServiceChangeResParm', Id, A, Ver, Prof, TS}}, V) ->
    {590, Id, e(A, V), Ver, e(Prof, V), TS};
e({serviceChangeResParms, SCRP}, V) ->
    {591, e(SCRP, V)};
e({'ServiceChangeResParm', Id, A, Ver, Prof, TS}, V) ->
    {592, Id, e(A, V), Ver, e(Prof, V), TS};

e({portNumber, N}, _V) ->
    {600, N};

e({'TimeNotation', D, T}, _V) ->
    {610, D, T};

e({'ServiceChangeProfile', N, Ver}, _V) ->
    {620, N, Ver};

e({digitMapName, N}, _V) ->
    {630, N};

e({megaco_term_id, false, Id}, _V) ->
    {640, Id};
e({megaco_term_id, true, [[$*]]}, _V) ->
    {641};
e({megaco_term_id, true, [[$$]]}, _V) ->
    {642};
e({megaco_term_id, true, Id}, _V) ->
    {643, Id};
e({'TerminationID', W, ID}, _V) ->
    {644, W, ID};

e({modReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {650, el(TID, V)};
e({modReply, {'AmmsReply', TID, [TA]}}, V) ->
    {651, el(TID, V), e(TA, V)};
e({modReply, {'AmmsReply', TID, TA}}, V) when list(TA) ->
    {652, el(TID, V), el(TA, V)};
e({modReply, R}, V) ->
    {653, e(R, V)};

e({addReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {660, el(TID, V)};
e({addReply, {'AmmsReply', TID, [TA]}}, V) ->
    {661, el(TID, V), e(TA, V)};
e({addReply, {'AmmsReply', TID, TA}}, V) when list(TA) ->
    {662, el(TID, V), el(TA, V)};
e({addReply, R}, V) ->
    {663, e(R, V)};

e({subtractReply, {'AmmsReply', TID, asn1_NOVALUE}}, V) ->
    {670, el(TID, V)};
e({subtractReply, {'AmmsReply', TID, [TA]}}, V) ->
    {671, el(TID, V), e(TA, V)};
e({subtractReply, {'AmmsReply', TID, TA}}, V) when list(TA) ->
    {672, el(TID, V), el(TA, V)};
e({subtractReply, R}, V) ->
    {673, e(R, V)};

e({'AmmsReply', TID, asn1_NOVALUE}, V) ->
    {680, el(TID, V)};
e({'AmmsReply', TID, [TA]}, V) ->
    {681, el(TID, V), e(TA, V)};
e({'AmmsReply', TID, TA}, V) when list(TA) ->
    {682, el(TID, V), el(TA, V)};

e({notifyReply, {'NotifyReply', TID, asn1_NOVALUE}}, V) ->
    {690, el(TID, V)};
e({notifyReply, {'NotifyReply', TID, ED}}, V) ->
    {691, el(TID, V), e(ED, V)};
e({notifyReply, R}, V) ->
    {692, e(R, V)};
e({'NotifyReply', TID, asn1_NOVALUE}, V) ->
    {693, el(TID, V)};
e({'NotifyReply', TID, ED}, V) ->
    {694, el(TID, V), e(ED, V)};

e({auditValueReply, AVR}, V) ->
    {700, e(AVR, V)};

e({auditResult, {'AuditResult', TID, [TAR]}}, V) ->
    {710, e(TID, V), e(TAR, V)};
e({auditResult, {'AuditResult', TID, TAR}}, V) ->
    {711, e(TID, V), el(TAR, V)};
e({auditResult, AR}, V) ->
    {712, e(AR, V)};
e({'AuditResult', TID, [TAR]}, V) ->
    {713, e(TID, V), e(TAR, V)};
e({'AuditResult', TID, TAR}, V) ->
    {714, e(TID, V), el(TAR, V)};

e({packagesDescriptor, PsD}, V) ->
    {720, el(PsD, V)};

e({'PackagesItem', "g", 1}, _V) ->
    {730};
e({'PackagesItem', "tonegen", 1}, _V) ->
    {731};
e({'PackagesItem', "tonedet", 1}, _V) ->
    {732};
e({'PackagesItem', "tg", 1}, _V) ->
    {733};
e({'PackagesItem', "dd", 1}, _V) ->
    {734};
e({'PackagesItem', "cg", 1}, _V) ->
    {735};
e({'PackagesItem', "cd", 1}, _V) ->
    {736};
e({'PackagesItem', "al", 1}, _V) ->
    {737};
e({'PackagesItem', "ct", 1}, _V) ->
    {738};
e({'PackagesItem', "nt", 1}, _V) ->
    {739};
e({'PackagesItem', "rtp", 1}, _V) ->
    {740};
e({'PackagesItem', "tdmc", 1}, _V) ->
    {741};
e({'PackagesItem', Name, Ver}, _V) ->
    {742, Name, Ver};

e({statisticsDescriptor, [SD]}, V) ->
    {770, e(SD, V)};
e({statisticsDescriptor, SsD}, V) ->
    {771, el(SsD, V)};

e({'StatisticsParameter', Name, asn1_NOVALUE}, _V) ->
    {780, Name};
e({'StatisticsParameter', Name, Value}, _V) ->
    {781, Name, Value};

e({'MuxDescriptor', MT, TL, asn1_NOVALUE}, V) ->
    {800, e(MT, V), el(TL, V)};
e({'MuxDescriptor', MT, TL, NSD}, V) ->
    {801, e(MT, V), el(TL, V), NSD};

e({indAudPackagesDescriptor, {'IndAudPackagesDescriptor', N, Ver}}, 2 = _V) ->
    {900, N, Ver};
e({indAudPackagesDescriptor, IAPD}, 2 = V) ->
    {900, e(IAPD, V)};
e({'IndAudPackagesDescriptor', N, Ver}, 2 = _V) ->
    {901, N, Ver};

e({indAudStatisticsDescriptor, {'IndAudStatisticsDescriptor', N}}, 2 = _V) ->
    {910, N};
e({indAudStatisticsDescriptor, IASD}, 2 = V) ->
    {911, e(IASD, V)};
e({'IndAudStatisticsDescriptor', N}, 2 = _V) ->
    {912, N};

e({indAudDigitMapDescriptor, {'IndAudDigitMapDescriptor', DMN}}, 2 = _V) ->
    {920, DMN};
e({indAudDigitMapDescriptor, IADMD}, 2 = V) ->
    {921, e(IADMD, V)};
e({'IndAudDigitMapDescriptor', DMN}, 2 = _V) ->
    {922, DMN};

e({indAudSignalsDescriptor, {seqSigList, IASD}}, 2 = V) ->
    {930, e(IASD, V)};
e({indAudSignalsDescriptor, {signal, IAS}}, 2 = V) ->
    {931, e(IAS, V)};

e({'IndAudSeqSigList', Id, SL}, 2 = V) ->
    {940, Id, e(SL, V)};

e({'IndAudSignal', N, SID}, 2 = V) ->
    {950, N, e(SID, V)};

e({indAudEventBufferDescriptor, {'IndAudEventBufferDescriptor', EN, SID}}, 2 = V) ->
    {960, EN, e(SID, V)};
e({indAudEventBufferDescriptor, IAEBD}, 2 = V) ->
    {961, e(IAEBD, V)};
e({'IndAudEventBufferDescriptor', EN, SID}, 2 = V) ->
    {962, EN, e(SID, V)};

e({indAudEventsDescriptor, {'IndAudEventsDescriptor', RID, N, SID}}, 2 = V) ->
    {970, e(RID, V), N, e(SID, V)};
e({indAudEventsDescriptor, IAED}, 2 = V) ->
    {971, e(IAED, V)};
e({'IndAudEventsDescriptor', RID, N, SID}, 2 = V) ->
    {972, e(RID, V), N, e(SID, V)};

e({indAudMediaDescriptor, {'IndAudMediaDescriptor', TSD, S}}, 2 = V) ->
    {980, e(TSD, V), e(S, V)};
e({indAudMediaDescriptor, IAMD}, 2 = V) ->
    {981, e(IAMD, V)};
e({'IndAudMediaDescriptor', TSD, S}, 2 = V) ->
    {982, e(TSD, V), e(S, V)};

e({'IndAudTerminationStateDescriptor', PP, EBC, SS}, 2 = V) ->
    {990, el(PP, V), e(EBC, V), e(SS, V)};

e({'IndAudStreamParms', LCD, asn1_NOVALUE, asn1_NOVALUE}, 2 = V) ->
    {1000, e(LCD, V)};
e({'IndAudStreamParms', LCD, LD, RD}, 2 = V) ->
    {1001, e(LCD, V), e(LD, V), e(RD, V)};

e({'IndAudLocalControlDescriptor', SM, RV, RG, asn1_NOVALUE}, 2 = V) ->
    {1010, e(SM, V), e(RV, V), e(RG, V)};
e({'IndAudLocalControlDescriptor', SM, RV, RG, PP}, 2 = V) when list(PP) ->
    {1011, e(SM, V), e(RV, V), e(RG, V), el(PP, V)};

e({'IndAudPropertyParm', N}, 2 = _V) ->
    {1020, N};

e(T, _V) ->
%     io:format("e -> ~nT: ~w~n", [T]),
    T.


d({1}, _) ->
    asn1_NOVALUE;
d({2}, _V) ->
    'NULL';
d({3}, _V) ->
    sendRecv;
d({4}, _V) ->
    recvOnly;
d({5}, _V) ->
    restart;
d({6}, _) ->
    mediaToken;
d({7}, _) ->
    eventsToken;
d({8}, _) ->
    signalsToken;
d({9}, _) ->
    digitMapToken;
d({10}, _) ->
    statsToken;
d({11}, _) ->
    packagesToken;
d({12}, _V) ->
    h221;
d({13}, _V) ->
    h223;
d({14}, _V) ->
    h226;
d({15}, _V) ->
    v76;

d({20, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', 1, d(Mid, 1), d(Body, 1)}};
d({21, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', 2, d(Mid, 2), d(Body, 2)}};
d({22, V, Mid, Body}, _) ->
    {'MegacoMessage', asn1_NOVALUE, {'Message', V, d(Mid, V), d(Body, V)}};
d({23, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, 1), {'Message', 1, d(Mid, 1), d(Body, 1)}};
d({24, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, 2), {'Message', 2, d(Mid, 2), d(Body, 2)}};
d({25, V, AuthHeader, Mid, Body}, _) ->
    {'MegacoMessage', d(AuthHeader, V), {'Message', V, d(Mid, V), d(Body, V)}};
d({26, AuthHeader, Mess}, V) ->
    {'MegacoMessage', d(AuthHeader, V), d(Mess, V)};
d({27, V, Mid, Body}, _) ->
    {'Message', V, d(Mid, V), d(Body, V)};

d({30, Name}, _V) ->
    {domainName, {'DomainName', Name, asn1_NOVALUE}};
d({31, Name, PortNumber}, _V) ->
    {domainName, {'DomainName', Name, PortNumber}};
d({32, N}, V) ->
    {domainName, d(N, V)};
d({33, Name}, _V) ->
    {'DomainName', Name, asn1_NOVALUE};
d({34, Name, PortNumber}, _V) ->
    {'DomainName', Name, PortNumber};
d({35, Addr}, _V) ->
    {ip4Address, {'IP4Address', Addr, asn1_NOVALUE}};
d({36, Addr, PortNumber}, _V) ->
    {ip4Address, {'IP4Address', Addr, PortNumber}};
d({37, A}, V) ->
    {ip4Address, d(A, V)};
d({38, Addr}, _V) ->
    {'IP4Address', Addr, asn1_NOVALUE};
d({39, Addr, PortNumber}, _V) ->
    {'IP4Address', Addr, PortNumber};
d({40, Addr}, _V) ->
    {ip6Address, {'IP6Address', Addr, asn1_NOVALUE}};
d({41, Addr, PortNumber}, _V) ->
    {ip6Address, {'IP6Address', Addr, PortNumber}};
d({42, A}, V) ->
    {ip6Address, d(A, V)};
d({43, Addr}, _V) ->
    {'IP6Address', Addr, asn1_NOVALUE};
d({44, Addr, PortNumber}, _V) ->
    {'IP6Address', Addr, PortNumber};

d({50, Transaction}, V) ->
    {transactions, [d(Transaction, V)]};
d({51, Transactions}, V) ->
    {transactions, dl(Transactions, V)};
d({52, EC}, _V) ->
    {messageError, {'ErrorDescriptor', EC, asn1_NOVALUE}};
d({53, EC, ET}, _V) ->
    {messageError, {'ErrorDescriptor', EC, ET}};
d({54, Error}, V) ->
    {messageError, d(Error, V)};
d({55, TransId, Actions}, V) ->
    {transactionRequest, {'TransactionRequest', TransId, dl(Actions, V)}};
d({56, TransId}, _V) ->
    {transactionPending, {'TransactionPending', TransId}};
d({57, TransId, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, asn1_NOVALUE, d(TransRes, V)}};
d({58, TransId, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, 'NULL', d(TransRes, V)}};
d({59, TransId, ImmAckReq, TransRes}, V) ->
    {transactionReply, {'TransactionReply', TransId, d(ImmAckReq, V), d(TransRes, V)}};
d({60, T}, V) ->
    {transactionResponseAck, dl(T, V)};
d({61, FirstAck}, _V) ->
    {'TransactionAck', FirstAck, asn1_NOVALUE};
d({62, FirstAck, LastAck}, _V) ->
    {'TransactionAck', FirstAck, LastAck};

d({70, EC}, _V) ->
    {'ErrorDescriptor', EC, asn1_NOVALUE};
d({71, EC, ET}, _V) ->
    {'ErrorDescriptor', EC, ET};

d({80, Cid, CtxReq, CtxAAR, CmdReq}, V) ->
    {'ActionRequest', Cid, d(CtxReq, V), d(CtxAAR, V), [d(CmdReq, V)]};
d({81, Cid, CtxReq, CtxAAR, CmdReqs}, V) ->
    {'ActionRequest', Cid, d(CtxReq, V), d(CtxAAR, V), dl(CmdReqs, V)};

d({90, P, E, T}, V) ->
    {'ContextRequest', d(P, V), d(E, V), d(T, V)};

d({100, P, E, T}, V) ->
    {'ContextAttrAuditRequest', d(P, V), d(E, V), d(T, V)};

d({110, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), asn1_NOVALUE, asn1_NOVALUE};
d({111, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), 'NULL', asn1_NOVALUE};
d({112, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), asn1_NOVALUE, 'NULL'};
d({113, Cmd}, V) ->
    {'CommandRequest', d(Cmd, V), 'NULL', 'NULL'};
d({114, Cmd, Opt, WR}, V) ->
    {'CommandRequest', d(Cmd, V), d(Opt, V), d(WR, V)};

d({120, From, To, Dir}, 1 = V) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V)};
d({121, From, To, Dir, SID}, 2 = V) ->
    {'TopologyRequest', d(From, V), d(To, V), d(Dir, V), d(SID, V)};

d({130, TID}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), []}};
d({131, TID, Desc}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), [d(Desc, V)]}};
d({132, TID, Descs}, V) ->
    {modReq, {'AmmRequest', dl(TID, V), dl(Descs, V)}};
d({133, TID}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), []}};
d({134, TID, Desc}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), [d(Desc, V)]}};
d({135, TID, Descs}, V) ->
    {addReq, {'AmmRequest', dl(TID, V), dl(Descs, V)}};
d({136, TID, Descs}, V) ->
    {'AmmRequest', dl(TID, V), dl(Descs, V)};

d({140, TID}, V) ->
    {subtractReq, {'SubtractRequest', dl(TID, V), asn1_NOVALUE}};
d({141, TID, AudDesc}, V) ->
    {subtractReq, {'SubtractRequest', dl(TID, V), d(AudDesc, V)}};
d({142, TID}, V) ->
    {'SubtractRequest', dl(TID, V), asn1_NOVALUE};
d({143, TID, AudDesc}, V) ->
    {'SubtractRequest', dl(TID, V), d(AudDesc, V)};

d({150, AR}, V) ->
    {auditValueRequest, d(AR, V)};

d({160, TID, AudDesc}, V) ->
    {'AuditRequest', d(TID, V), d(AudDesc, V)};

d({170, AR}, V) ->
    {actionReplies, [d(AR, V)]};
d({171, ARs}, V) ->
    {actionReplies, dl(ARs, V)};

d({180, CID, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, [d(CmdRep, V)]};
d({181, CID, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, asn1_NOVALUE, dl(CmdRep, V)};
d({182, CID, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, d(CtxRep, V), [d(CmdRep, V)]};
d({183, CID, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, asn1_NOVALUE, d(CtxRep, V), dl(CmdRep, V)};
d({184, CID, ED, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), asn1_NOVALUE, [d(CmdRep, V)]};
d({185, CID, ED, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), asn1_NOVALUE, dl(CmdRep, V)};
d({186, CID, ED, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), d(CtxRep, V), [d(CmdRep, V)]};
d({187, CID, ED, CtxRep, CmdRep}, V) ->
    {'ActionReply', CID, d(ED, V), d(CtxRep, V), dl(CmdRep, V)};

d({190}, 1 = _V) ->
    {'AuditDescriptor', asn1_NOVALUE};
d({191, AT}, 1 = V) ->
    {'AuditDescriptor', dl(AT, V)};
d({192}, 2 = _V) ->
    {'AuditDescriptor', asn1_NOVALUE, asn1_NOVALUE};
d({193, AT, APT}, 2 = V) when list(AT), list(APT) ->
    {'AuditDescriptor', dl(AT, V), dl(APT, V)};
d({194, AT, APT}, 2 = V) when list(APT) ->
    {'AuditDescriptor', d(AT, V), dl(APT, V)};
d({195, AT, APT}, 2 = V) when list(AT) ->
    {'AuditDescriptor', dl(AT, V), d(APT, V)};
d({196, AT, APT}, 2 = V) ->
    {'AuditDescriptor', d(AT, V), d(APT, V)};

d({200, TID, OED}, V) ->
    {notifyReq, {'NotifyRequest', dl(TID, V), d(OED, V), asn1_NOVALUE}};
d({201, TID, OED, ED}, V) ->
    {notifyReq, {'NotifyRequest', dl(TID, V), d(OED, V), d(ED, V)}};
d({202, TID, OED}, V) ->
    {'NotifyRequest', dl(TID, V), d(OED, V), asn1_NOVALUE};
d({203, TID, OED, ED}, V) ->
    {'NotifyRequest', dl(TID, V), d(OED, V), d(ED, V)};

d({210, RID, OEL}, V) ->
    {'ObservedEventsDescriptor', RID, dl(OEL, V)};

d({220, EN, SID, EPL, TN}, V) ->
    {'ObservedEvent', EN, d(SID, V), dl(EPL, V), d(TN, V)};

d({230}, _V) ->
    {'EventParameter', "type", ["est"], asn1_NOVALUE};
d({231, Val}, _V) ->
    {'EventParameter', "type", [Val], asn1_NOVALUE};
d({232, Val}, _V) ->
    {'EventParameter', "type", Val, asn1_NOVALUE};
d({233}, _V) ->
    {'EventParameter', "Generalcause", ["NR"], asn1_NOVALUE};
d({234}, _V) ->
    {'EventParameter', "Generalcause", ["UR"], asn1_NOVALUE};
d({235}, _V) ->
    {'EventParameter', "Generalcause", ["FT"], asn1_NOVALUE};
d({236}, _V) ->
    {'EventParameter', "Generalcause", ["FP"], asn1_NOVALUE};
d({237}, _V) ->
    {'EventParameter', "Generalcause", ["IW"], asn1_NOVALUE};
d({238}, _V) ->
    {'EventParameter', "Generalcause", ["UN"], asn1_NOVALUE};
d({239, Val}, _V) ->
    {'EventParameter', "Generalcause", [Val], asn1_NOVALUE};
d({240, Val}, _V) ->
    {'EventParameter', "Generalcause", Val, asn1_NOVALUE};
d({241, Val}, _V) ->
    {'EventParameter', "Failurecause", [Val], asn1_NOVALUE};
d({242, Val}, _V) ->
    {'EventParameter', "Failurecause", Val, asn1_NOVALUE};
d({243, EPN, Val}, _V) ->
    {'EventParameter', EPN, Val, asn1_NOVALUE};
d({244, EPN, Val, EI}, _V) ->
    {'EventParameter', EPN, Val, EI};

d({260, TID, SCPs}, V) ->
    {serviceChangeReq, {'ServiceChangeRequest', dl(TID, V), d(SCPs, V)}};
d({261, SCR}, V) ->
    {serviceChangeReq, d(SCR, V)};
d({262, TID, SCPs}, V) ->
    {'ServiceChangeRequest', dl(TID, V), d(SCPs, V)};

d({270, TID, SCR}, V) ->
    {serviceChangeReply, {'ServiceChangeReply', dl(TID, V), d(SCR, V)}};
d({271, SCR}, V) ->
    {serviceChangeReply, d(SCR, V)};
d({272, TID, SCR}, V) -> %% KOLLA
    {'ServiceChangeReply', dl(TID, V), d(SCR, V)};

d({280, TSD, S}, V) ->
    {mediaDescriptor, {'MediaDescriptor', d(TSD, V), d(S, V)}};
d({281, MD}, V) ->
    {mediaDescriptor, d(MD, V)};
d({282, TSD, S}, V) ->
    {'MediaDescriptor', d(TSD, V), d(S, V)};

d({290, S}, V) ->
    {oneStream, d(S, V)};
d({291, S}, V) ->
    {multiStream, dl(S, V)};
d({292, SID, SP}, V) ->
    {'StreamDescriptor', d(SID, V), d(SP, V)};

d({300, LCD}, V) ->
    {'StreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE};
d({301, LCD, LD}, V) ->
    {'StreamParms', d(LCD, V), d(LD, V), asn1_NOVALUE};
d({302, LCD, LD, RD}, V) ->
    {'StreamParms', d(LCD, V), d(LD, V), d(RD, V)};

d({310, SM, RV, RG, PP}, V) ->
    {'LocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V)};

d({320, Val}, _V) ->
    {'PropertyParm', "v", [Val], asn1_NOVALUE};
d({321, Val}, _V) ->
    {'PropertyParm', "v", Val, asn1_NOVALUE};
d({332, Val}, _V) ->
    {'PropertyParm', "o", [Val], asn1_NOVALUE};
d({333, Val}, _V) ->
    {'PropertyParm', "o", Val, asn1_NOVALUE};
d({334, Val}, _V) ->
    {'PropertyParm', "s", [Val], asn1_NOVALUE};
d({335, Val}, _V) ->
    {'PropertyParm', "s", Val, asn1_NOVALUE};
d({336, Val}, _V) ->
    {'PropertyParm', "i", [Val], asn1_NOVALUE};
d({337, Val}, _V) ->
    {'PropertyParm', "i", Val, asn1_NOVALUE};
d({338, Val}, _V) ->
    {'PropertyParm', "u", [Val], asn1_NOVALUE};
d({339, Val}, _V) ->
    {'PropertyParm', "u", Val, asn1_NOVALUE};
d({340, Val}, _V) ->
    {'PropertyParm', "e", [Val], asn1_NOVALUE};
d({341, Val}, _V) ->
    {'PropertyParm', "e", Val, asn1_NOVALUE};
d({342, Val}, _V) ->
    {'PropertyParm', "p", [Val], asn1_NOVALUE};
d({343, Val}, _V) ->
    {'PropertyParm', "p", Val, asn1_NOVALUE};
d({344, Val}, _V) ->
    {'PropertyParm', "c", [Val], asn1_NOVALUE};
d({345, Val}, _V) ->
    {'PropertyParm', "c", Val, asn1_NOVALUE};
d({346, Val}, _V) ->
    {'PropertyParm', "b", [Val], asn1_NOVALUE};
d({347, Val}, _V) ->
    {'PropertyParm', "b", Val, asn1_NOVALUE};
d({348, Val}, _V) ->
    {'PropertyParm', "z", [Val], asn1_NOVALUE};
d({349, Val}, _V) ->
    {'PropertyParm', "z", Val, asn1_NOVALUE};
d({350, Val}, _V) ->
    {'PropertyParm', "k", [Val], asn1_NOVALUE};
d({351, Val}, _V) ->
    {'PropertyParm', "k", Val, asn1_NOVALUE};
d({352, Val}, _V) ->
    {'PropertyParm', "a", [Val], asn1_NOVALUE};
d({353, Val}, _V) ->
    {'PropertyParm', "a", Val, asn1_NOVALUE};
d({354, Val}, _V) ->
    {'PropertyParm', "t", [Val], asn1_NOVALUE};
d({355, Val}, _V) ->
    {'PropertyParm', "t", Val, asn1_NOVALUE};
d({356, Val}, _V) ->
    {'PropertyParm', "r", [Val], asn1_NOVALUE};
d({357, Val}, _V) ->
    {'PropertyParm', "r", Val, asn1_NOVALUE};
d({358, Val}, _V) ->
    {'PropertyParm', "m", [Val], asn1_NOVALUE};
d({359, Val}, _V) ->
    {'PropertyParm', "m", Val, asn1_NOVALUE};
d({360, Val}, _V) ->
    {'PropertyParm', "nt/jit", [Val], asn1_NOVALUE};
d({361, Val}, _V) ->
    {'PropertyParm', "nt/jit", Val, asn1_NOVALUE};
d({362}, _V) ->
    {'PropertyParm', "tdmc/ec", ["on"], asn1_NOVALUE};
d({363}, _V) ->
    {'PropertyParm', "tdmc/ec", ["off"], asn1_NOVALUE};
d({364}, _V) ->
    {'PropertyParm', "tdmc/gain", ["automatic"], asn1_NOVALUE};
d({365, Val}, _V) ->
    {'PropertyParm', "tdmc/gain", [Val], asn1_NOVALUE};
d({366, Val}, _V) ->
    {'PropertyParm', "tdmc/gain", Val, asn1_NOVALUE};
d({367, Val}, _V) ->
    {'PropertyParm', "maxNumberOfContexts", [Val], asn1_NOVALUE};
d({368, Val}, _V) ->
    {'PropertyParm', "maxNumberOfContexts", Val, asn1_NOVALUE};
d({369, Val}, _V) ->
    {'PropertyParm', "maxTerminationsPerContext", [Val], asn1_NOVALUE};
d({370, Val}, _V) ->
    {'PropertyParm', "maxTerminationsPerContext", Val, asn1_NOVALUE};
d({371, Val}, _V) ->
    {'PropertyParm', "normalMGExecutionTime", [Val], asn1_NOVALUE};
d({372, Val}, _V) ->
    {'PropertyParm', "normalMGExecutionTime", Val, asn1_NOVALUE};
d({373, Val}, _V) ->
    {'PropertyParm', "normalMGCExecutionTime", [Val], asn1_NOVALUE};
d({374, Val}, _V) ->
    {'PropertyParm', "normalMGCExecutionTime", Val, asn1_NOVALUE};
d({375, Val}, _V) ->
    {'PropertyParm', "MGProvisionalResponseTimerValue", [Val], asn1_NOVALUE};
d({376, Val}, _V) ->
    {'PropertyParm', "MGProvisionalResponseTimerValue", Val, asn1_NOVALUE};
d({377, Val}, _V) ->
    {'PropertyParm', "MGCProvisionalResponseTimerValue", [Val], asn1_NOVALUE};
d({378, Val}, _V) ->
    {'PropertyParm', "MGCProvisionalResponseTimerValue", Val, asn1_NOVALUE};
d({379, N, Val}, _V) ->
    {'PropertyParm', N, [Val], asn1_NOVALUE};
d({380, N, Val}, _V) ->
    {'PropertyParm', N, Val, asn1_NOVALUE};
d({381, N, Val, EI}, _V) ->
    {'PropertyParm', N, Val, EI};

d({400, PG}, V) ->
    {'LocalRemoteDescriptor', [[d(PG, V)]]};
d({401, PG}, V) ->
    {'LocalRemoteDescriptor', [dl(PG, V)]};
d({402, PG}, V) ->
    {'LocalRemoteDescriptor', dll(PG, V)};

d({410, PP, EBC, SS}, V) ->
    {'TerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V)};

d({420, RID, E}, V) ->
    {eventsDescriptor, {'EventsDescriptor', d(RID, V), [d(E, V)]}};
d({421, RID, EL}, V) ->
    {eventsDescriptor, {'EventsDescriptor', d(RID, V), dl(EL, V)}};
d({422, ED}, V) ->
    {eventsDescriptor, d(ED, V)};
d({423, RID, E}, V) ->
    {'EventsDescriptor', d(RID, V), [d(E, V)]};
d({424, RID, EL}, V) ->
    {'EventsDescriptor', d(RID, V), dl(EL, V)};

d({430, PN, SID, EA, EPL}, V) ->
    {'RequestedEvent', PN, d(SID, V), d(EA, V), dl(EPL, V)};

d({440, KA, EDM, SE, SD}, V) ->
    {'RequestedActions', d(KA, V), d(EDM, V), d(SE, V), d(SD, V)};

d({450, RID, E}, V) ->
    {'SecondEventsDescriptor', d(RID, V), [d(E, V)]};
d({451, RID, EL}, V) ->
    {'SecondEventsDescriptor', d(RID, V), dl(EL, V)};

d({460, PN, SID, EA, EPL}, V) ->
    {'SecondRequestedEvent', PN, d(SID, V), d(EA, V), d(EPL, V)};

d({470, KA, EDM, SD}, V) ->
    {'SecondRequestedActions', d(KA, V), d(EDM, V), d(SD, V)};

d({480, EN, SID, EPL}, V) ->
    {'EventSpec', EN, d(SID, V), dl(EPL, V)};

d({490, ID, SL}, V) ->
    {'SeqSigList', ID, dl(SL, V)};

d({500, S}, V) ->
    {signalsDescriptor, dl(S, V)};

d({510, S}, V) ->
    {signal, d(S, V)};

d({520, SN, SID, ST, D, NC, KA, SPL}, V) ->
    {'Signal', 
     SN, d(SID, V), d(ST, V), d(D, V), d(NC, V), d(KA, V), dl(SPL, V)};

d({530, SPN, Val}, _V) ->
    {'SigParameter', SPN, Val, asn1_NOVALUE};
d({531, SPN, Val, EI}, _V) ->
    {'SigParameter', SPN, Val, EI};

d({550, MTL, MPL}, _V) ->
    {'ModemDescriptor', MTL, MPL, asn1_NOVALUE};
d({551, MTL, MPL, NSD}, _V) ->
    {'ModemDescriptor', MTL, MPL, NSD};

d({560, DMN, DMV}, V) ->
    {digitMapDescriptor, {'DigitMapDescriptor', DMN, d(DMV, V)}};
d({561, DMD}, V) ->
    {digitMapDescriptor, d(DMD, V)};
d({562, DMN, DMV}, V) ->
    {'DigitMapDescriptor', DMN, d(DMV, V)};

d({570, Start, Stop, Long, DMB}, 1 = V) ->
    {'DigitMapValue', d(Start, V), d(Stop, V), d(Long, V), DMB};
d({571, Start, Stop, Long, DMB, Dur}, 2 = V) ->
    {'DigitMapValue', d(Start, V), d(Stop, V), d(Long, V), DMB, d(Dur, V)};

d({580, M, A, Ver, Prof, R, D, Id}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     asn1_NOVALUE, asn1_NOVALUE};
d({581, M, A, Ver, Prof, R, D, Id, TS}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), asn1_NOVALUE};
d({582, M, A, Ver, Prof, R, D, Id, TS, NSD}, V) ->
    {'ServiceChangeParm', 
     d(M, V), d(A, V), d(Ver, V), d(Prof, V), R, d(D, V), d(Id, V), 
     d(TS, V), NSD};

d({590, Id, A, Ver, Prof, TS}, V) ->
    {serviceChangeResParms, {'ServiceChangeResParm', Id, d(A, V), Ver, d(Prof, V), TS}};
d({591, SCRP}, V) ->
    {serviceChangeResParms, d(SCRP, V)};
d({592, Id, A, Ver, Prof, TS}, V) ->
    {'ServiceChangeResParm', Id, d(A, V), Ver, d(Prof, V), TS};

d({600, N}, _V) ->
    {portNumber, N};

d({610, D, T}, _V) ->
    {'TimeNotation', D, T};

d({620, N, Ver}, _V) ->
    {'ServiceChangeProfile', N, Ver};

d({630, N}, _) ->
    {digitMapName, N};

d({640, Id}, _V) ->
    {megaco_term_id, false, Id};
d({641}, _V) ->
    {megaco_term_id, true, [[$*]]};
d({642}, _V) ->
    {megaco_term_id, true, [[$$]]};
d({643, Id}, _V) ->
    {megaco_term_id, true, Id};
d({644, W, ID}, _V) ->
    {'TerminationID', W, ID};

d({650, TID}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({651, TID, TA}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({652, TID, TA}, V) ->
    {modReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({653, R}, V) ->
    {modReply, d(R, V)};

d({660, TID}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({661, TID, TA}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({662, TID, TA}, V) ->
    {addReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({663, R}, V) ->
    {addReply, d(R, V)};

d({670, TID}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), asn1_NOVALUE}};
d({671, TID, TA}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), [d(TA, V)]}};
d({672, TID, TA}, V) ->
    {subtractReply, {'AmmsReply', dl(TID, V), dl(TA, V)}};
d({673, R}, V) ->
    {subtractReply, d(R, V)};

d({680, TID}, V) ->
    {'AmmsReply', dl(TID, V), asn1_NOVALUE};
d({681, TID, TA}, V) ->
    {'AmmsReply', dl(TID, V), [d(TA, V)]};
d({682, TID, TA}, V) ->
    {'AmmsReply', dl(TID, V), dl(TA, V)};

d({690, TID}, V) ->
    {notifyReply, {'NotifyReply', dl(TID, V), asn1_NOVALUE}};
d({691, TID, ED}, V) ->
    {notifyReply, {'NotifyReply', dl(TID, V), d(ED, V)}};
d({692, R}, V) ->
    {notifyReply, d(R, V)};
d({693, TID}, V) ->
    {'NotifyReply', dl(TID, V), asn1_NOVALUE};
d({694, TID, ED}, V) ->
    {'NotifyReply', dl(TID, V), d(ED, V)};

d({700, AVR}, V) ->
    {auditValueReply, d(AVR, V)};

d({710, TID, TAR}, V) ->
    {auditResult, {'AuditResult', d(TID, V), [d(TAR, V)]}};
d({711, TID, TAR}, V) ->
    {auditResult, {'AuditResult', d(TID, V), dl(TAR, V)}};
d({712, AR}, V) ->
    {auditResult, d(AR, V)};
d({713, TID, TAR}, V) ->
    {'AuditResult', d(TID, V), [d(TAR, V)]};
d({714, TID, TAR}, V) ->
    {'AuditResult', d(TID, V), dl(TAR, V)};

d({720, PsD}, V) ->
    {packagesDescriptor, dl(PsD, V)};

d({730}, _V) ->
    {'PackagesItem', "g", 1};
d({731}, _V) ->
    {'PackagesItem', "tonegen", 1};
d({732}, _V) ->
    {'PackagesItem', "tonedet", 1};
d({733}, _V) ->
    {'PackagesItem', "tg", 1};
d({734}, _V) ->
    {'PackagesItem', "dd", 1};
d({735}, _V) ->
    {'PackagesItem', "cg", 1};
d({736}, _V) ->
    {'PackagesItem', "cd", 1};
d({737}, _V) ->
    {'PackagesItem', "al", 1};
d({738}, _V) ->
    {'PackagesItem', "ct", 1};
d({739}, _V) ->
    {'PackagesItem', "nt", 1};
d({740}, _V) ->
    {'PackagesItem', "rtp", 1};
d({741}, _V) ->
    {'PackagesItem', "tdmc", 1};
d({742, Name, Ver}, _V) ->
    {'PackagesItem', Name, Ver};

d({770, SD}, V) ->
    {statisticsDescriptor, [d(SD, V)]};
d({771, SsD}, V) ->
    {statisticsDescriptor, dl(SsD, V)};

d({780, Name}, _V) ->
    {'StatisticsParameter', Name, asn1_NOVALUE};
d({781, Name, Value}, _V) ->
    {'StatisticsParameter', Name, Value};

d({800, MT, TL}, V) ->
    {'MuxDescriptor', d(MT, V), dl(TL, V), asn1_NOVALUE};
d({801, MT, TL, NSD}, V) ->
    {'MuxDescriptor', d(MT, V), dl(TL, V), NSD};

d({900, N, Ver}, 2 = _V) ->
    {indAudPackagesDescriptor, {'IndAudPackagesDescriptor', N, Ver}};
d({900, IAPD}, 2 = V) ->
    {indAudPackagesDescriptor, d(IAPD, V)};
d({901, N, Ver}, 2 = _V) ->
    {'IndAudPackagesDescriptor', N, Ver};

d({910, N}, 2 = _V) ->
    {indAudStatisticsDescriptor, {'IndAudStatisticsDescriptor', N}};
d({911, IASD}, 2 = V) ->
    {indAudStatisticsDescriptor, d(IASD, V)};
d({912, N}, 2 = _V) ->
    {'IndAudStatisticsDescriptor', N};

d({920, DMN}, 2 = _V) ->
    {indAudDigitMapDescriptor, {'IndAudDigitMapDescriptor', DMN}};
d({921, IADMD}, 2 = V) ->
    {indAudDigitMapDescriptor, d(IADMD, V)};
d({922, DMN}, 2 = _V) ->
    {'IndAudDigitMapDescriptor', DMN};

d({930, IASD}, 2 = V) ->
    {indAudSignalsDescriptor, {seqSigList, d(IASD, V)}};
d({931, IAS}, 2 = V) ->
    {indAudSignalsDescriptor, {signal, d(IAS, V)}};

d({940, Id, SL}, 2 = V) ->
    {'IndAudSeqSigList', Id, d(SL, V)};

d({950, N, SID}, 2 = V) ->
    {'IndAudSignal', N, d(SID, V)};

d({960, EN, SID}, 2 = V) ->
    {indAudEventBufferDescriptor, {'IndAudEventBufferDescriptor', EN, d(SID, V)}};
d({961, IAEBD}, 2 = V) ->
    {indAudEventBufferDescriptor, d(IAEBD, V)};
d({962, EN, SID}, 2 = V) ->
    {'IndAudEventBufferDescriptor', EN, d(SID, V)};

d({970, RID, N, SID}, 2 = V) ->
    {indAudEventsDescriptor, {'IndAudEventsDescriptor', d(RID, V), N, d(SID, V)}};
d({971, IAED}, 2 = V) ->
    {indAudEventsDescriptor, d(IAED, V)};
d({972, RID, N, SID}, 2 = V) ->
    {'IndAudEventsDescriptor', d(RID, V), N, d(SID, V)};

d({980, TSD, S}, 2 = V) ->
    {indAudMediaDescriptor, {'IndAudMediaDescriptor', d(TSD, V), d(S, V)}};
d({981, IAMD}, 2 = V) ->
    {indAudMediaDescriptor, d(IAMD, V)};
d({982, TSD, S}, 2 = V) ->
    {'IndAudMediaDescriptor', d(TSD, V), d(S, V)};

d({990, PP, EBC, SS}, 2 = V) ->
    {'IndAudTerminationStateDescriptor', dl(PP, V), d(EBC, V), d(SS, V)};

d({1000, LCD}, 2 = V) ->
    {'IndAudStreamParms', d(LCD, V), asn1_NOVALUE, asn1_NOVALUE};
d({1001, LCD, LD, RD}, 2 = V) ->
    {'IndAudStreamParms', d(LCD, V), d(LD, V), d(RD, V)};

d({1010, SM, RV, RG}, 2 = V) ->
    {'IndAudLocalControlDescriptor', 
     d(SM, V), d(RV, V), d(RG, V), asn1_NOVALUE};
d({1011, SM, RV, RG, PP}, 2 = V) when list(PP) ->
    {'IndAudLocalControlDescriptor', d(SM, V), d(RV, V), d(RG, V), dl(PP, V)};

d({1020, N}, 2 = _V) ->
    {'IndAudPropertyParm', N};

d(T, _) ->
    T.


% i(F, A) ->
%     %% i(get(dbg), F, A).
%     i(true, F, A).

% i(true, F, A) ->
%     io:format("DBG:" ++ F ++ "~n", A);
% i(_, _, _) ->
%     ok.

