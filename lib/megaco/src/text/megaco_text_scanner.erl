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
%%----------------------------------------------------------------------
%% Purpose : Scanner for text encoded Megaco/H.248 messages
%%----------------------------------------------------------------------

-module('megaco_text_scanner').

-export([scan/1, skip_sep_chars/2]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/src/engine/megaco_message_internal.hrl").
-include("megaco_text_tokens.hrl").

-define(LOWER(Char),
	if
	    Char >= $A, Char =< $Z ->
		Char - ($A - $a);
	    true ->
		Char
	end).

scan(Bin) when binary(Bin) ->
    Chars = erlang:binary_to_list(Bin),
    tokens1(Chars, 1, []);
scan(Chars) when list(Chars) ->
    tokens1(Chars, 1, []).

%% As long as we dont know the version, we will loop in this function
tokens1(Chars, Line, Acc) ->
    case any_chars(Chars, Line) of
	{token, Token, [], LatestLine} ->
	    %% We got to the end without actually getting a version token.
	    Tokens = [{endOfMessage, LatestLine, endOfMessage}, Token | Acc],
	    {error, no_version_found, lists:reverse(Tokens), Line};

        %% -- Version token for version 1 --
        {token, {'SafeChars',_,"!/1"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 1, [Token | Acc]);

        {token, {'SafeChars',_,"megaco/1"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 1, [Token | Acc]);


        %% -- Version token for version 2 --
        {token, {'SafeChars',_,"!/2"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 2, [Token | Acc]);

        {token, {'SafeChars',_,"megaco/2"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 2, [Token | Acc]);


        %% -- Version token for version 3 --
        {token, {'SafeChars',_,"!/3"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 3, [Token | Acc]);

        {token, {'SafeChars',_,"megaco/3"} = Token, Rest, LatestLine} ->
            tokens2(Rest, LatestLine, 3, [Token | Acc]);


        %% -- Version token for version X --
        {token, {'SafeChars',_,[$!,$/| Vstr]} = Token, Rest, LatestLine} ->
	    case guess_version(Vstr) of
		{ok, V} ->
		    tokens2(Rest, LatestLine, V, [Token | Acc]);
		{error, Reason} ->
		    {error, Reason, LatestLine}
	    end;

        {token, {'SafeChars',_,[$m,$e,$g,$a,$c,$o,$/|Vstr]} = Token, Rest, LatestLine} ->
	    case guess_version(Vstr) of
		{ok, V} ->
		    tokens2(Rest, LatestLine, V, [Token | Acc]);
		{error, Reason} ->
		    {error, Reason, LatestLine}
	    end;

	%% -- Other tokens --
	{token, Token, Rest, LatestLine} ->
	    tokens1(Rest, LatestLine, [Token | Acc]);

	{bad_token, Token, _Rest, _LatestLine} ->
	    {error, {bad_token, [Token, Acc]}, Line}
    end.

tokens2(Chars, Line0, Version, Tokens0) ->
    case tokens2(Chars, Line0, Tokens0) of
	{ok, Tokens, Line} ->
	    {ok, Tokens, Version, Line};
	Error ->
	    Error
    end.

tokens2(Chars, Line, Acc) ->
%%     d("tokens2 -> entry with"
%%       "~n   Chars: ~s"
%%       "~n   Line:  ~p", [Chars, Line]),
    case any_chars(Chars, Line) of
	{token, Token, [], LatestLine} ->
%% 	    d("tokens2 -> Token: ~n~p", [Token]),
	    Tokens = [{endOfMessage, LatestLine, endOfMessage}, Token | Acc],
	    {ok, lists:reverse(Tokens), Line};

	{token, Token, Rest, LatestLine} ->
%% 	    d("tokens2 -> Token: ~n~p", [Token]),
	    tokens2(Rest, LatestLine, [Token | Acc]);

	{bad_token, Token, _Rest, _LatestLine} ->
	    {error, {bad_token, [Token, Acc]}, Line}
    end.


guess_version([C]) when (48 =< C) and (C =< 57) ->
    {ok, C-48};
guess_version(Str) when is_list(Str) ->
    case (catch list_to_integer(Str)) of
	I when is_integer(I) ->
	    {ok, I};
	_ ->
	    {error, {invalid_version, Str}}
    end.


%% Returns {token,     Token, Rest, LatestLine}
%% Returns {bad_token, Token, Rest, LatestLine}
any_chars([Char | Rest], Line) ->
%     Class = ?classify_char(Char),
%     d("any_chars -> ~w of class ~w", [Char, Class]),
%     case Class of
    case ?classify_char(Char) of
	safe_char ->
	    safe_chars(Rest, [Char], [?LOWER(Char)], Line);
	rest_char ->
	    case Char of
		?SemiColonToken ->
		    comment_chars(Rest, Line);
		_ ->
		    rest_chars(Rest, [Char], Line)
	    end;
	double_quote ->
	    quoted_chars(Rest, [], Line);
	white_space ->
	    sep_chars(Rest, Line);
	end_of_line ->
	    sep_chars(Rest, Line);
	bad_char ->
	    %% {bad_token, {'SEP', Line, Char}, Rest, Line}
	    {bad_token, {'AnyChars', Line, Char}, Rest, Line}
    end;
any_chars([] = All, Line) ->
    {token, {'SEP', Line, end_of_input}, All, Line}.

comment_chars([Char | Rest], Line) ->
    case ?classify_char(Char) of
	safe_char ->
	    comment_chars(Rest, Line);
	rest_char ->
	    comment_chars(Rest, Line);
	white_space -> 
	    comment_chars(Rest, Line);
	end_of_line ->
	    sep_chars(Rest, Line);
	_ when Char == 22 ->
	    comment_chars(Rest, Line);
	_ ->
	    %% {bad_token, {'SEP', Line, Char}, Rest, Line}
	    {bad_token, {'CommentChars', Line, Char}, Rest, Line}
    end;
comment_chars([] = All, Line) ->
    {token, {'SEP', Line}, All, Line}.
    
sep_chars([Char | Rest] = All, Line) ->
    case ?classify_char(Char) of
	safe_char ->
	    {token, {'SEP', Line}, All, Line};
	rest_char ->
	    case Char of
		?SemiColonToken ->
		    comment_chars(Rest, Line);
		_ ->
		    rest_chars(Rest, [Char], Line)
	    end;
	white_space -> 
	    sep_chars(Rest, Line);
	end_of_line ->
	    sep_chars(Rest, Line + 1);
	_ ->
	    %% {bad_token, {'SEP', Line, Char}, Rest, Line}
	    {bad_token, {'SepChars', Line, Char}, Rest, Line}
    end;
sep_chars([] = All, Line) ->
    {token, {'SEP', Line}, All, Line}.

rest_chars(Rest, [?ColonToken], Line) ->
    {token, {'COLON',   Line}, Rest, Line};
rest_chars(Rest, [AccChar], Line) ->
    TokenTag =
	case AccChar of
	    ?EqualToken   -> 'EQUAL';
	    ?NequalToken  -> 'NEQUAL';
	    ?LesserToken  -> 'LESSER';
	    ?GreaterToken -> 'GREATER';
	    ?LbrktToken   -> 'LBRKT';
	    ?RbrktToken   -> 'RBRKT';
	    ?LsbrktToken  -> 'LSBRKT';
	    ?RsbrktToken  -> 'RSBRKT';
	    ?LparToken    -> 'LPAR';
	    ?RparToken    -> 'RPAR';
	    ?VbarToken    -> 'VBAR';
	    ?CommaToken   -> 'COMMA'
	end,
    {Rest2, Line2} = skip_sep_chars(Rest, Line),
    {token, {TokenTag, Line}, Rest2, Line2}.

skip_sep_chars([Char | Rest] = All, Line) ->
    case ?classify_char(Char) of
	rest_char when Char == ?SemiColonToken ->
	    skip_comment_chars(Rest, Line);
	white_space -> 
	    skip_sep_chars(Rest, Line);
	end_of_line ->
	    skip_sep_chars(Rest, Line + 1);
	_ ->
	    {All, Line}
    end;
skip_sep_chars([] = All, Line) ->
    {All, Line}.

skip_comment_chars([Char | Rest] = All, Line) ->
    case ?classify_char(Char) of
	safe_char ->
	    skip_comment_chars(Rest, Line);
	rest_char ->
	    skip_comment_chars(Rest, Line);
	double_quote ->
	    skip_comment_chars(Rest, Line);
	white_space -> 
	    skip_comment_chars(Rest, Line);
	end_of_line ->
	    skip_sep_chars(Rest, Line + 1);
	_ ->
	    {All, Line}
    end;
skip_comment_chars([] = All, Line) ->
    {All, Line}.

quoted_chars([Char | Rest], Acc, Line) ->
    case ?classify_char(Char) of
	safe_char ->
	    quoted_chars(Rest, [Char | Acc], Line);
	rest_char ->
	    quoted_chars(Rest, [Char | Acc], Line);
	white_space -> 
	    quoted_chars(Rest, [Char | Acc], Line);
	double_quote -> 
	    {token, {'QuotedChars', Line, lists:reverse(Acc)}, Rest, Line};
	_ ->
	    {bad_token, {'QuotedChars', Line, Char}, Rest, Line}
    end;
quoted_chars([] = All, _Acc, Line) ->
    {bad_token, {'QuotedChars', Line, end_of_input}, All, Line}.
    
safe_chars([Char | Rest] = All, Acc, LowerAcc, Line) ->
    case ?classify_char(Char) of
	safe_char ->
	    safe_chars(Rest, [Char | Acc], [?LOWER(Char) | LowerAcc], Line);
	_ ->
	    LowerSafeChars = lists:reverse(LowerAcc),
	    TokenTag = select_token(LowerSafeChars),
	    SafeChars = lists:reverse(Acc),
	    case TokenTag of
		'MtpToken' ->
		    %% 'MtpToken' 'LBRKT' OctetString 'RBRKT'
		    special_chars(All, LowerSafeChars, Line, TokenTag);
		'LocalToken' ->
		    %% 'LocalToken' 'LBRKT' OctetString 'RBRKT'
		    special_chars(All, SafeChars, Line, TokenTag);
		'RemoteToken' ->
		    %% 'RemoteToken' 'LBRKT' OctetString 'RBRKT'
		    special_chars(All, SafeChars, Line, TokenTag);
		'DigitMapToken' -> 
		    %% 'DigitMapToken'
		    %% 'DigitMapToken' 'EQUAL' Name
		    %% 'DigitMapToken' 'EQUAL' Name 'LBRKT' Value 'RBRKT'
		    %% 'DigitMapToken' 'EQUAL' 'LBRKT' Value 'RBRKT'
		    %% 'DigitMapToken' 'LBRKT' Value 'RBRKT'
		    special_chars(All, LowerSafeChars, Line, TokenTag);
		_ ->
		    {token, {TokenTag, Line, LowerSafeChars}, All, Line}
	    end
    end;
safe_chars([] = All, _Acc, LowerAcc, Line) ->
    LowerSafeChars = lists:reverse(LowerAcc),
    TokenTag = select_token(LowerSafeChars),
    %%SafeChars = lists:reverse(Acc),
    {token, {TokenTag, Line, LowerSafeChars}, All, Line}.
    
collect_safe_chars([Char | Rest] = All, LowerAcc) ->
    case ?classify_char(Char) of
	safe_char ->
	    collect_safe_chars(Rest, [?LOWER(Char) | LowerAcc]);
	_ ->
	    {All, lists:reverse(LowerAcc)}
    end;
collect_safe_chars([] = Rest, LowerAcc) ->
    {Rest, lists:reverse(LowerAcc)}.

special_chars(All, SafeChars, Line, TokenTag) ->
    {Rest, Line2} = skip_sep_chars(All, Line),
    case Rest of
	[?LbrktToken | Rest2] ->
	    {token, {'OctetString', _, OctetString}, Rest4, Line4} =
		octet_string(Rest2, Line2),
	    case Rest4 of
		[?RbrktToken | Rest6] ->
		    Token = 
			case TokenTag of
			    'MtpToken' ->
				%% 'MtpToken' 'LBRKT' OctetString 'RBRKT'
				{'MtpAddressToken', Line, OctetString};
			    'LocalToken' ->
				%% 'LocalToken' 'LBRKT' OctetString 'RBRKT'
				{'LocalDescriptorToken', Line, OctetString};
			    'RemoteToken' ->
				%% 'RemoteToken' 'LBRKT' OctetString 'RBRKT'
				{'RemoteDescriptorToken', Line, OctetString};
			    'DigitMapToken' ->
				%% 'DigitMapToken' 'LBRKT' OctetString 'RBRKT'
				DMV = digit_map_value(OctetString),
				DMD = #'DigitMapDescriptor'{digitMapValue = DMV},
				{'DigitMapDescriptorToken', Line, DMD}
			end,
		    {token, Token, Rest6, Line4};
		_ when TokenTag == 'DigitMapToken' ->
		    %% 'DigitMapToken'
		    {token, {'DigitMapToken', Line, SafeChars}, All, Line};
		_ ->
		    {token, {'SafeChars', Line, SafeChars}, All, Line}
	    end;
	[?EqualToken | Rest2] when TokenTag == 'DigitMapToken' ->
	    {Rest3, Line3} = skip_sep_chars(Rest2, Line2),
	    {Rest4, DigitMapName} = collect_safe_chars(Rest3, []),
	    {Rest6, Line6, DMD} = 
		if
		    DigitMapName == [] ->
			{Rest3, Line3, #'DigitMapDescriptor'{}};
		    true ->
			{Rest5, Line5} = skip_sep_chars(Rest4, Line3),
			{Rest5, Line5, #'DigitMapDescriptor'{digitMapName  = DigitMapName}}
		end,
	    case Rest6 of
		[?LbrktToken | Rest7] ->
		    {token, {'OctetString', _, OctetString}, Rest8, Line8} = 
			octet_string(Rest7, Line6),
		    case Rest8 of
			[?RbrktToken | Rest10] ->
			    %% 'DigitMapToken' 'EQUAL' 'LBRKT' OctetString 'RBRKT'
			    %% 'DigitMapToken' 'EQUAL' Name 'LBRKT' OctetString 'RBRKT'
			    {Rest11, Line11} = skip_sep_chars(Rest10, Line8),
			    DMV = digit_map_value(OctetString),
			    DMD2 = DMD#'DigitMapDescriptor'{digitMapValue = DMV},
			    {token, {'DigitMapDescriptorToken', Line, DMD2}, Rest11, Line11};
			_ when DMD#'DigitMapDescriptor'.digitMapName /= asn1_NOVALUE ->
			    %% 'DigitMapToken' 'EQUAL' Name
			    {token, {'DigitMapDescriptorToken', Line, DMD}, Rest4, Line3};
			_ ->
			    %% 'DigitMapToken'
			    {token, {'DigitMapToken', Line, SafeChars}, All, Line}
		    end;
		_ when DMD#'DigitMapDescriptor'.digitMapName /= asn1_NOVALUE ->
		    %% 'DigitMapToken' 'EQUAL' Name
		    {token, {'DigitMapDescriptorToken', Line, DMD}, Rest4, Line3};
		_ ->
		    %% 'DigitMapToken'
		    {token, {'DigitMapToken', Line, SafeChars}, All, Line}
	    end;
	_  when TokenTag == 'DigitMapToken' ->
	    %% 'DigitMapToken'
	    {token, {'DigitMapToken', Line, SafeChars}, All, Line};
	_ ->
	    %% 'DigitMapToken'
	    {token, {'SafeChars', Line, SafeChars}, All, Line}
    end.

octet_string(Chars, Line) ->
    {Chars2, Line2} = skip_sep_chars(Chars, Line),
    Acc = [],
    {Rest, RevChars, RestLine} = octet_string(Chars2, Acc, Line2),
    {RevChars2, _} = skip_sep_chars(RevChars, RestLine),
    OctetString = lists:reverse(RevChars2),
    {token, {'OctetString', Line, OctetString}, Rest, RestLine}.
    
    
octet_string([Char | Rest] = All, Acc, Line) ->
    if
	Char == ?CrToken ->
	    octet_string(Rest, [Char | Acc], Line + 1);
	Char == ?LfToken ->
	    octet_string(Rest, [Char | Acc], Line + 1);
	Char >= 8#1, Char =< 8#174 ->
	    octet_string(Rest, [Char | Acc], Line);
	Char >= 8#176, Char =< 8#377 ->
	    octet_string(Rest, [Char | Acc], Line);
	Char == ?BackslashToken ->
	    case Rest of
		[?RbrktToken | _Rest2] ->
		    %% OTP-4357
		    octet_string(Rest, [?RbrktToken, ?BackslashToken | Acc], Line);
		_ ->
		    octet_string(Rest, [Char | Acc], Line)
	    end;
	true ->
	    {All, Acc, Line}
    end;
octet_string([] = All, Acc, Line) ->
    {All, Acc, Line}.
	

%% digitMapValue      = ["T" COLON Timer COMMA]
%%			["S" COLON Timer COMMA] 
%%			["L" COLON Timer COMMA] 
%%			["Z" COLON Timer COMMA] digitMap  
digit_map_value(Chars) ->
    digit_map_value(Chars, #'DigitMapValue'{}).

%% NOTE: The swap of the digitMapBody and the durationTimer is
%% intentional. The reason is a problem with the flex scanner.
%% Hopefully this is temporary...
%% The values are swapped back later by the parser...
digit_map_value([Char, ?ColonToken | Rest] = All, DMV) ->
    case ?LOWER(Char) of
	$t -> digit_map_timer(All, Rest, #'DigitMapValue'.startTimer, DMV);
	$s -> digit_map_timer(All, Rest, #'DigitMapValue'.shortTimer, DMV);
	$l -> digit_map_timer(All, Rest, #'DigitMapValue'.longTimer, DMV);
% 	$z -> digit_map_timer(All, Rest, #'DigitMapValue'.durationTimer, DMV);
% 	_  -> DMV#'DigitMapValue'{digitMapBody = All}
	$z -> digit_map_timer(All, Rest, #'DigitMapValue'.digitMapBody, DMV);
	_  -> DMV#'DigitMapValue'{durationTimer = All}
    end;
digit_map_value(Chars, DMV) ->
    DMV#'DigitMapValue'{durationTimer = Chars}.

digit_map_timer(All, Chars, TimerPos, DMV) ->
    {Rest, Digits} = collect_safe_chars(Chars, []),
    {Rest2, _} = skip_sep_chars(Rest, 0),
   case {Rest2, catch list_to_integer(Digits)} of
       {[?CommaToken | Rest3], Int} when integer(Int), Int >= 0,
					 element(TimerPos, DMV) == asn1_NOVALUE ->
	   {Rest4, _} = skip_sep_chars(Rest3, 0),
	   DMV2 = setelement(TimerPos, DMV, Int),
	   digit_map_value(Rest4, DMV2);
       _ ->
	   DMV#'DigitMapValue'{digitMapBody = All}
   end.

select_token([$o, $- | LowerText]) ->
    select_token(LowerText);
select_token([$w, $- | LowerText]) ->
    select_token(LowerText);
select_token(LowerText) ->
    case LowerText of
        "add"                   -> 'AddToken';
        "a"                     -> 'AddToken';
        "andlgc"                -> 'AndAUDITSelectToken'; % v3
        "audit"                 -> 'AuditToken';
        "at"                    -> 'AuditToken';
        "auditcapability"       -> 'AuditCapToken';
        "ac"                    -> 'AuditCapToken';
        "auditvalue"            -> 'AuditValueToken';
        "av"                    -> 'AuditValueToken';
	"authentication"        -> 'AuthToken';
        "au"                    -> 'AuthToken';
        "both"                  -> 'BothToken';         % v3
        "b"                     -> 'BothToken';         % v3
        "bothway"               -> 'BothwayToken';
        "bw"                    -> 'BothwayToken';
        "brief"                 -> 'BriefToken';
        "br"                    -> 'BriefToken';
        "buffer"                -> 'BufferToken';
        "bf"                    -> 'BufferToken';
        "context"               -> 'CtxToken';
        "c"                     -> 'CtxToken';
        "contextattr"           -> 'ContextAttrToken';  % v3
        "ct"                    -> 'ContextAttrToken';  % v3
        "contextlist"           -> 'ContextListToken';  % v3
        "clt"                   -> 'ContextListToken';  % v3
        "contextaudit"          -> 'ContextAuditToken';
        "ca"                    -> 'ContextAuditToken';
	"digitmap"              -> 'DigitMapToken';
	"dm"                    -> 'DigitMapToken';
        "spadirection"          -> 'DirectionToken';    % v3
        "direction"             -> 'DirectionToken';    % v3 (pre-v3a/v3b)
        "spadi"                 -> 'DirectionToken';    % v3
        "di"                    -> 'DirectionToken';    % v3 (pre-v3a/v3b)
        "discard"               -> 'DiscardToken';
        "ds"                    -> 'DiscardToken';
        "disconnected"          -> 'DisconnectedToken';
        "dc"                    -> 'DisconnectedToken';
        "delay"                 -> 'DelayToken';
        "dl"                    -> 'DelayToken';
        "delete"                -> 'DeleteToken';
        "de"                    -> 'DeleteToken';
        "duration"              -> 'DurationToken';
        "dr"                    -> 'DurationToken';
        "embed"                 -> 'EmbedToken';
        "em"                    -> 'EmbedToken';
        "emergency"             -> 'EmergencyToken';
        "eg"                    -> 'EmergencyToken';
        "emergencyofftoken"     -> 'EmergencyOffToken';
        "emergencyoff"          -> 'EmergencyOffToken';   % v3 (as of prev3c)
        "ego"                   -> 'EmergencyOffToken';
        "emergencyvalue"        -> 'EmergencyValueToken'; % v3 
        "egv"                   -> 'EmergencyValueToken'; % v3
        "error"                 -> 'ErrorToken';
        "er"                    -> 'ErrorToken';
        "eventbuffer"           -> 'EventBufferToken';
        "eb"                    -> 'EventBufferToken';
        "events"                -> 'EventsToken';
        "e"                     -> 'EventsToken';
        "external"              -> 'ExternalToken';     % v3
        "ex"                    -> 'ExternalToken';     % v3
        "failover"              -> 'FailoverToken';
        "fl"                    -> 'FailoverToken';
        "forced"                -> 'ForcedToken';
        "fo"                    -> 'ForcedToken';
        "graceful"              -> 'GracefulToken';
        "gr"                    -> 'GracefulToken';
        "h221"                  -> 'H221Token';
        "h223"                  -> 'H223Token';
        "h226"                  -> 'H226Token';
        "handoff"               -> 'HandOffToken';
        "ho"                    -> 'HandOffToken';
        "iepscall"              -> 'IEPSToken';         % v3
        "ieps"                  -> 'IEPSToken';         % v3
        "inactive"              -> 'InactiveToken';
        "in"                    -> 'InactiveToken';
        "internal"              -> 'InternalToken';     % v3
        "it"                    -> 'InternalToken';     % v3
        "immackrequired"        -> 'ImmAckRequiredToken';
        "ia"                    -> 'ImmAckRequiredToken';
        "inservice"             -> 'InSvcToken';
        "intersignal"           -> 'IntsigDelayToken'; % v3
        "spais"                 -> 'IntsigDelayToken'; % v3
        "intbyevent"            -> 'InterruptByEventToken';
        "ibe"                   -> 'InterruptByEventToken';
        "intbysigdescr"         -> 'InterruptByNewSignalsDescrToken';
        "ibs"                   -> 'InterruptByNewSignalsDescrToken';
        "iv"                    -> 'InSvcToken';
        "isolate"               -> 'IsolateToken';
        "is"                    -> 'IsolateToken';
	"iterationtoken"        -> 'IterationToken'; % v3
	"ir"                    -> 'IterationToken'; % v3
        "keepactive"            -> 'KeepActiveToken';
        "ka"                    -> 'KeepActiveToken';
	"local"                 -> 'LocalToken';
        "l"                     -> 'LocalToken';
        "localcontrol"          -> 'LocalControlToken';
        "lockstep"              -> 'LockStepToken';
        "sp"                    -> 'LockStepToken';
        "o"                     -> 'LocalControlToken';
        "loopback"              -> 'LoopbackToken';
        "lb"                    -> 'LoopbackToken';
        "media"                 -> 'MediaToken';
        "m"                     -> 'MediaToken';
        %% "megaco"                -> 'MegacopToken';
	%% "!"                     -> 'megacoptoken';
	%% "segment"               -> 'MessageSegmentToken'; % v3
	%% "sm"                    -> 'MessageSegmentToken'; % v3
        "method"                -> 'MethodToken';
        "mt"                    -> 'MethodToken';
        "mtp"                   -> 'MtpToken';
        "mgcidtotry"            -> 'MgcIdToken';
        "mg"                    -> 'MgcIdToken';
        "mode"                  -> 'ModeToken';
        "mo"                    -> 'ModeToken';
        "modify"                -> 'ModifyToken';
        "mf"                    -> 'ModifyToken';
        "modem"                 -> 'ModemToken';
        "md"                    -> 'ModemToken';
        "move"                  -> 'MoveToken';
        "mv"                    -> 'MoveToken';
        "mux"                   -> 'MuxToken';
        "mx"                    -> 'MuxToken';
        "nevernotify"           -> 'NeverNotifyToken'; % v3
        "nbnn"                  -> 'NeverNotifyToken'; % v3
        "notify"                -> 'NotifyToken';
        "n"                     -> 'NotifyToken';
        "notifycompletion"      -> 'NotifyCompletionToken';
        "nc"                    -> 'NotifyCompletionToken';
        "immediatenotify"       -> 'NotifyImmediateToken';    % v3
        "nbin"                  -> 'NotifyImmediateToken';    % v3
        "regulatednotify"       -> 'NotifyRegulatedToken';    % v3
        "nbrn"                  -> 'NotifyRegulatedToken';    % v3
        "nx64kservice"          -> 'Nx64kToken';              % v2
        "n64"                   -> 'Nx64kToken';              % v2
        "observedevents"        -> 'ObservedEventsToken';
        "oe"                    -> 'ObservedEventsToken';
        "oneway"                -> 'OnewayToken';
        "ow"                    -> 'OnewayToken';
        "onewayboth"            -> 'OnewayBothToken';     % v3
        "owb"                   -> 'OnewayBothToken';     % v3
        "onewayexternal"        -> 'OnewayExternalToken'; % v3
        "owe"                   -> 'OnewayExternalToken'; % v3
        "off"                   -> 'OffToken';
        "on"                    -> 'OnToken';
        "onoff"                 -> 'OnOffToken';
        "oo"                    -> 'OnOffToken';
        "orlgc"                 -> 'OrAUDITselectToken';  % v3
        "otherreason"           -> 'OtherReasonToken';
        "or"                    -> 'OtherReasonToken';
        "outofservice"          -> 'OutOfSvcToken';
        "os"                    -> 'OutOfSvcToken';
        "packages"              -> 'PackagesToken';
        "pg"                    -> 'PackagesToken';
        "pending"               -> 'PendingToken';
        "pn"                    -> 'PendingToken';
        "priority"              -> 'PriorityToken';
        "pr"                    -> 'PriorityToken';
        "profile"               -> 'ProfileToken';
        "pf"                    -> 'ProfileToken';
        "reason"                -> 'ReasonToken';
        "re"                    -> 'ReasonToken';
        "receiveonly"           -> 'RecvonlyToken';
        "requestid"             -> 'RequestIDToken';    % v3
        "rq"                    -> 'RequestIDToken';    % v3
        "rc"                    -> 'RecvonlyToken';
        "reply"                 -> 'ReplyToken';
        "p"                     -> 'ReplyToken';
        "reseteventsdescriptor" -> 'ResetEventsDescriptorToken'; % v3
        "rse"                   -> 'ResetEventsDescriptorToken'; % v3
        "transactionresponseack"-> 'ResponseAckToken';
        "k"                     -> 'ResponseAckToken';
        "restart"               -> 'RestartToken';
        "rs"                    -> 'RestartToken';
	"remote"                -> 'RemoteToken';
	"r"                     -> 'RemoteToken';
        "sparequestid"          -> 'RequestIDToken';
        "sparq"                 -> 'RequestIDToken';
        "reservedgroup"         -> 'ReservedGroupToken';
        "rg"                    -> 'ReservedGroupToken';
        "reservedvalue"         -> 'ReservedValueToken';
        "rv"                    -> 'ReservedValueToken';
        "end"                   -> 'SegmentCompleteToken'; % v3
        "&"                     -> 'SegmentCompleteToken'; % v3
        "sendonly"              -> 'SendonlyToken';
        "so"                    -> 'SendonlyToken';
        "sendreceive"           -> 'SendrecvToken';
        "sr"                    -> 'SendrecvToken';
        "services"              -> 'ServicesToken';
        "sv"                    -> 'ServicesToken';
        "servicestates"         -> 'ServiceStatesToken';
        "si"                    -> 'ServiceStatesToken';
        "servicechange"         -> 'ServiceChangeToken';
        "sc"                    -> 'ServiceChangeToken';
        "servicechangeaddress"  -> 'ServiceChangeAddressToken';
        "ad"                    -> 'ServiceChangeAddressToken';
        "servicechangeinc"      -> 'ServiceChangeIncompleteToken'; % v3
        "sic"                   -> 'ServiceChangeIncompleteToken'; % v3
        "signallist"            -> 'SignalListToken';
        "sl"                    -> 'SignalListToken';
        "signals"               -> 'SignalsToken';
        "sg"                    -> 'SignalsToken';
        "signaltype"            -> 'SignalTypeToken';
        "sy"                    -> 'SignalTypeToken';
        "statistics"            -> 'StatsToken';
        "sa"                    -> 'StatsToken';
        "stream"                -> 'StreamToken';
        "st"                    -> 'StreamToken';
        "subtract"              -> 'SubtractToken';
        "s"                     -> 'SubtractToken';
        "synchisdn"             -> 'SynchISDNToken';
        "sn"                    -> 'SynchISDNToken';
        "terminationstate"      -> 'TerminationStateToken';
        "ts"                    -> 'TerminationStateToken';
        "test"                  -> 'TestToken';
        "te"                    -> 'TestToken';
        "timeout"               -> 'TimeOutToken';
        "to"                    -> 'TimeOutToken';
        "topology"              -> 'TopologyToken';
        "tp"                    -> 'TopologyToken';
        "transaction"           -> 'TransToken';
        "t"                     -> 'TransToken';
        "v18"                   -> 'V18Token';
        "v22"                   -> 'V22Token';
        "v22b"                  -> 'V22bisToken';
        "v32"                   -> 'V32Token';
        "v32b"                  -> 'V32bisToken';
        "v34"                   -> 'V34Token';
        "v76"                   -> 'V76Token';
        "v90"                   -> 'V90Token';
        "v91"                   -> 'V91Token';
        "version"               -> 'VersionToken';
        "v"                     -> 'VersionToken';
	%% TimeStamp when length(TimeStamp) == 17 -> 'TimeStampToken';
	[D1,D2,D3,D4,D5,D6,D7,D8,$t,T1,T2,T3,T4,T5,T6,T7,T8] 
	when $0 =< D1; D1 =< $9;
	     $0 =< D2; D2 =< $9;
	     $0 =< D3; D3 =< $9;
	     $0 =< D4; D4 =< $9;
	     $0 =< D5; D5 =< $9;
	     $0 =< D6; D6 =< $9;
	     $0 =< D7; D7 =< $9;
	     $0 =< D8; D8 =< $9;
	     $0 =< T1; T1 =< $9;
	     $0 =< T2; T2 =< $9;
	     $0 =< T3; T3 =< $9;
	     $0 =< T4; T4 =< $9;
	     $0 =< T5; T5 =< $9;
	     $0 =< T6; T6 =< $9;
	     $0 =< T7; T7 =< $9;
	     $0 =< T8; T8 =< $9 -> 'TimeStampToken';
	_                       -> 'SafeChars'
    end.


%% d(F) ->
%%     d(F, []).

%% d(F, A) ->
%%     d(get(dbg), F, A).

%% d(true, F, A) ->
%%     io:format("DBG:~p:" ++ F ++ "~n", [?MODULE|A]);
%% d(_, _, _) ->
%%     ok.
