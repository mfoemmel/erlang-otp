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
%% Purpose: Parse and evaluate digit maps
%%----------------------------------------------------------------------
%%
%% digitMap           =  digitString
%%                    /   LWSP "(" LWSP digitStringList LWSP ")" LWSP 
%% digitStringList    = digitString *( LWSP "|" LWSP digitString ) 
%% digitString        = 1*(digitStringElement) 
%% digitStringElement = digitPosition [DOT] 
%% digitPosition      = digitMapLetter / digitMapRange
%% digitMapRange      = ("x" / LWSP "[" LWSP digitLetter LWSP "]" LWSP)
%% digitLetter        = *((DIGIT "-" DIGIT ) / digitMapLetter)
%% digitMapLetter     = DIGIT   ; Basic event symbols 
%%                    / %x41-4B ; a-k
%%                    / %x61-6B ; A-K 
%%                    / "L"     ; Long  inter-event timers, e.g. 16 sec
%%                    / "S"     ; Short inter-event timers
%%                    / "Z"     ; Long duration modifier
%% DIGIT              = %x30-39 ; 0-9 
%%                   
%%----------------------------------------------------------------------
%% Example of a digit map:
%% 
%% (0| 00|[1-7]xxx|8xxxxxxx|Fxxxxxxx|Exx|91xxxxxxxxxx|9011x.) 
%%----------------------------------------------------------------------

-module(megaco_digit_map).

-export([parse/1, eval/1, eval/2, report/2, test/2]). % Public
-export([test_eval/2]).                               % Internal

-include("megaco_message_internal.hrl").
-include_lib("megaco/src/text/megaco_text_tokens.hrl").

-record(state_transition, {mode, next, cont}).

-record(timers, {mode       = state_dependent,
		 start 	    = 0,
		 short 	    = timer_to_millis(3),
		 long  	    = timer_to_millis(9),
		 unexpected = reject}). % ignore | reject

%%----------------------------------------------------------------------
%% Parses a digit map body, represented as a list of chars,
%% into a list of state transitions.
%% 
%% Returns {ok, StateTransitionList} | {error, Reason}
%%----------------------------------------------------------------------

parse(DigitMapBody) when list(DigitMapBody) ->
    case parse_digit_map(DigitMapBody) of
	{ok, STL} ->
	    {ok, STL};
	{error, Reason} ->
	    {error, Reason}
    end;
parse(_DigitMapBody) ->
    {error, not_a_digit_map_body}.

parse_digit_map(Chars) ->
    parse_digit_map(Chars, 1, [], []).

parse_digit_map(Chars, Line, DS, STL) ->
    case megaco_text_scanner:skip_sep_chars(Chars, Line) of
	{[], _Line2} when DS /= [] ->
	    case parse_digit_string(DS) of
		{ok, DS2} ->
		    ST = #state_transition{mode = state_dependent,
					   next = start,
					   cont = DS2},
		    STL2 = lists:reverse([ST | STL]),
		    {ok, STL2};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{[Char | Chars2], Line2} ->
	    case Char of
		$( when DS == [], STL == [] ->
		    parse_digit_map(Chars2, Line2, DS, STL);
		$) when DS /= [] ->
		    case megaco_text_scanner:skip_sep_chars(Chars2, Line2) of
			{[], _Line3} ->
			    case parse_digit_string(DS) of
				{ok, DS2} ->
				    ST = #state_transition{mode = state_dependent,
							   next = start,
							   cont = DS2},
				    STL2 = lists:reverse([ST | STL]),
				    {ok, STL2};
				{error, Reason} ->
				    {error, Reason}
			    end;
			{Chars3, Line3} ->
			    Trash =  lists:reverse(Chars3),
			    {error, {round_bracket_mismatch, Trash, Line3}}
		    end;
		$| when DS /= [] ->
		    case parse_digit_string(DS) of
			{ok, DS2} ->
			    ST = #state_transition{mode = state_dependent,
						   next = start,
						   cont = DS2},
			    parse_digit_map(Chars2, Line2, [], [ST | STL]);
			{error, Reason} ->
			    {error, Reason}
		    end;
		_ when Char /= $(, Char /= $|, Char /= $) ->
		    parse_digit_map(Chars2, Line2, [Char | DS], STL);
		_ ->
		    {error, {round_bracket_mismatch, Line2}}
	    end;
	{[], Line2} ->
	    {error, {digit_string_expected, Line2}}
    end.

parse_digit_string(Chars) ->
    parse_digit_string(Chars, []).

parse_digit_string([Char | Chars], DS) ->
    case Char of
	$] ->
	    parse_digit_letter(Chars, [], DS);
	$[ ->
	    {error, square_bracket_mismatch};
	$x ->
	    parse_digit_string(Chars, [{range, $0, $9} | DS]);
	$. ->
	    parse_digit_string(Chars, [zero_or_more | DS]);
	I when I >= $0, I =< $9 ->
	    parse_digit_string(Chars, [{single, I} | DS]);
	A when A >= $a, A =< $k ->
	    parse_digit_string(Chars, [{single, A} | DS]);
	A when A >= $A, A =< $K ->
	    parse_digit_string(Chars, [{single, A} | DS]);
	$S ->
	    parse_digit_string(Chars, [use_short_timer | DS]);
	$L ->
	    parse_digit_string(Chars, [use_long_timer | DS]);
	$Z ->
	    parse_digit_string(Chars, [inter_event_timeout | DS]);
	$s ->
	    parse_digit_string(Chars, [use_short_timer | DS]);
	$l ->
	    parse_digit_string(Chars, [use_long_timer | DS]);
	$z ->
	    parse_digit_string(Chars, [inter_event_timeout | DS]);
	BadChar ->
	    {error, {illegal_char_in_digit_string, BadChar}}
    end;
parse_digit_string([], DM) ->
    {ok, DM}.
    
parse_digit_letter([Char | Chars], DL, DS) ->
    case Char of
	$[ ->
	    parse_digit_string(Chars, [DL | DS]);
	$] ->
	    {error, square_bracket_mismatch};
	To when To >= $0, To =< $9 ->
	    case Chars of
		[$-, From | Chars2] when From >= $0, From =< $9 ->
		    parse_digit_letter(Chars2, [{range, From, To} | DL], DS);
		_ ->
		    parse_digit_letter(Chars, [{single, To} | DL], DS)
	    end;
	A when A >= $a, A =< $k ->
	    parse_digit_letter(Chars, [{single, A} | DL], DS);
	A when A >= $A, A =< $K ->
	    parse_digit_letter(Chars, [{single, A} | DL], DS);
	$S ->
	    parse_digit_letter(Chars, [use_short_timer | DL], DS);
	$L ->
	    parse_digit_letter(Chars, [use_long_timer | DL], DS);
	$Z ->
	    parse_digit_letter(Chars, [inter_event_timeout | DL], DS);
	$s ->
	    parse_digit_letter(Chars, [use_short_timer | DL], DS);
	$l ->
	    parse_digit_letter(Chars, [use_long_timer | DL], DS);
	$z ->
	    parse_digit_letter(Chars, [inter_event_timeout | DL], DS);
	BadChar ->
	    {error, {illegal_char_between_square_brackets, BadChar}}
    end;
parse_digit_letter([], _DL, _DS) ->
    {error, square_bracket_mismatch}.

%%----------------------------------------------------------------------
%% Collect digit map letters according to digit map
%% Returns {ok, Letters} | {error, Reason}
%%----------------------------------------------------------------------
     
eval(DMV) when record(DMV, 'DigitMapValue') ->
    case parse(DMV#'DigitMapValue'.digitMapBody) of
	{ok, DigitMapBody} ->
	    eval(DigitMapBody, DMV);
	{error, Reason} ->
	    {error, Reason}
    end;
eval(STL) when list(STL) ->
     eval(STL, #timers{}).
	
eval(STL, DMV) when record(DMV, 'DigitMapValue') ->
    Timers = #timers{start = timer_to_millis(DMV#'DigitMapValue'.startTimer),
		     short = timer_to_millis(DMV#'DigitMapValue'.shortTimer),
		     long  = timer_to_millis(DMV#'DigitMapValue'.longTimer)},
    eval(STL, Timers);
eval(STL, {ignore, DMV}) when record(DMV, 'DigitMapValue') ->
    Timers = #timers{start = timer_to_millis(DMV#'DigitMapValue'.startTimer),
		     short = timer_to_millis(DMV#'DigitMapValue'.shortTimer),
		     long  = timer_to_millis(DMV#'DigitMapValue'.longTimer),
		     unexpected = ignore},
    eval(STL, Timers);
eval(STL, {reject, DMV}) when record(DMV, 'DigitMapValue') ->
    Timers = #timers{start = timer_to_millis(DMV#'DigitMapValue'.startTimer),
		     short = timer_to_millis(DMV#'DigitMapValue'.shortTimer),
		     long  = timer_to_millis(DMV#'DigitMapValue'.longTimer),
		     unexpected = reject},
    eval(STL, Timers);
eval(STL, Timers) when list(STL),
		       record(hd(STL), state_transition),
		       record(Timers, timers) ->
    collect(start, mandatory_event, Timers, STL, []);
eval(DigitMapBody, ignore) ->
    eval(DigitMapBody, #timers{unexpected = ignore});
eval(DigitMapBody, reject) ->
    eval(DigitMapBody, #timers{unexpected = reject});
eval(DigitMapBody, Timers) ->
    case parse(DigitMapBody) of
	{ok, STL} ->
	    eval(STL, Timers);
	{error, Reason} ->
	    {error, Reason}
    end.

collect(Event, State, Timers, STL, Letters) ->
    case handle_event(Event, State, Timers, STL, Letters) of
	{completed, _Timers2, _STL2, Letters2} ->
	    completed(Letters2);
	{State2, Timers2, STL2, Letters2} ->
	    MaxWait = choose_timer(State2, Event, Timers2),
	    %% ok = io:format("Timer: ~p ~p~n~p~n~p~n",
	    %%                [State2, MaxWait, Timers2, STL2]),
	    receive
		{?MODULE, _FromPid, Event2} ->
		    %% ok = io:format("Got event: ~p~n", [Event2]),
		    collect(Event2, State2, Timers2, STL2, Letters2)
	    after MaxWait ->
		    collect(inter_event_timeout, State2, Timers2, STL2, Letters2)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

choose_timer(State, start, T) ->
    Extra = T#timers.start,
    Timer = do_choose_timer(State, T),
    if
	Timer == infinity -> infinity;
	Extra == infinity -> infinity;
	true              -> Timer + Extra
    end;
choose_timer(State, _Event, T) ->
    do_choose_timer(State, T).

do_choose_timer(State, T) ->
    case T#timers.mode of
	state_dependent ->
	    case State of
		mandatory_event -> T#timers.long;
		optional_event  -> T#timers.short
	    end;
	use_short_timer -> 
	    T#timers.short;
	use_long_timer -> 
	    T#timers.long
    end.

timer_to_millis(asn1_NOVALUE) -> 0;
timer_to_millis(infinity)     -> infinity;
timer_to_millis(Seconds)      -> timer:seconds(Seconds).
    
completed(Letters) ->
    {ok, lists:reverse(Letters)}.

unexpected_event(Event, STL, Letters) ->
    Expected = [ST#state_transition.next || ST <- STL],
    SoFar = lists:reverse(Letters),
    Reason = {unexpected_event, Event, SoFar, Expected},
    {error, Reason}.
    
%%----------------------------------------------------------------------
%% Handles a received event according to digit map
%% State ::= optional_event | mandatory_event
%% 
%% Returns {State, NewSTL, Letters} | {error, Reason}
%%----------------------------------------------------------------------
handle_event(inter_event_timeout, optional_event, Timers, STL, Letters) ->
    {completed, Timers, STL, Letters};
handle_event(cancel, _State, _Timers, STL, Letters) ->
    unexpected_event(cancel, STL, Letters);
handle_event(start, _State, Timers, STL, Letters) ->
    {State2, Timers2, STL2} = compute(Timers, STL),
    {State2, Timers2, STL2, Letters};
handle_event(Event, State, Timers, STL, Letters) ->
    {STL2, Collect} = match_event(Event, STL, [], false),
    case STL2 of
	[] ->
	    case Timers#timers.unexpected of
		ignore ->
		    ok = io:format("<WARNING> Ignoring unexpected event: ~p~n"
				   "Expected: ~p~n",
				   [Event, STL]),
		    {State, Timers, STL, Letters};
		reject ->
		    unexpected_event(Event, STL, Letters)
	    end;
	STL2 ->
	    {State3, Timers2, STL3} = compute(Timers, STL2),
	    case Collect of
		true  -> {State3, Timers2, STL3, [Event | Letters]};
		false -> {State3, Timers2, STL3, Letters}
	    end
    end.

match_event(Event, [ST | OldSTL], NewSTL, Collect)
  when record(ST, state_transition) ->
    case ST#state_transition.next of
	{single, Event} ->
	    match_event(Event, OldSTL, [ST | NewSTL], true);
	{range, From, To} when Event >= From, Event =< To ->
	    ST2 = ST#state_transition{next = {single, Event}},
	    match_event(Event, OldSTL, [ST2 | NewSTL], true);
	Event ->
	    match_event(Event, OldSTL, [ST | NewSTL], Collect);
	_ ->
	    match_event(Event, OldSTL, NewSTL, Collect)
    end;
match_event(Event, [H | T], NewSTL, Collect) when list(H) ->
    {NewSTL2, _Letters} = match_event(Event, H, NewSTL, Collect),
    match_event(Event, T, NewSTL2, Collect);
match_event(_Event, [], NewSTL, Collect) ->
    {NewSTL, Collect}.
    
%%----------------------------------------------------------------------
%% Compute new state transitions
%% Returns {State, Timers, NewSTL}
%%----------------------------------------------------------------------
compute(Timers, OldSTL) ->
    {State, GlobalMode, NewSTL} = compute(mandatory_event, state_dependent, OldSTL, []),
    Timers2 = Timers#timers{mode = GlobalMode},
    {State, Timers2, NewSTL}.

compute(State, GlobalMode, [ST | OldSTL], NewSTL) when record(ST, state_transition) ->
    Cont = ST#state_transition.cont,
    Mode = ST#state_transition.mode,
    {State2, GlobalMode2, NewSTL2} =
	compute_cont(Cont, Mode, GlobalMode, State, NewSTL),
    compute(State2, GlobalMode2, OldSTL, NewSTL2);
compute(State, GlobalMode, [H | T], NewSTL) when list(H) ->
    {State2, GlobalMode2, NewSTL2} = compute(State, GlobalMode, H, NewSTL),
    compute(State2, GlobalMode2, T, NewSTL2);
compute(State, GlobalMode, [], NewSTL) ->
    case NewSTL of
	[] -> {completed, GlobalMode, NewSTL};
	_  -> {State,     GlobalMode, NewSTL}
    end.

compute_cont([Next | Cont] = All, Mode, GlobalMode, State, STL) ->
    case Next of
	use_short_timer ->
	    Mode2 = use_short_timer,
	    compute_cont(Cont, Mode2, GlobalMode, State, STL);
	use_long_timer ->
	    Mode2 = use_long_timer,
	    compute_cont(Cont, Mode2, GlobalMode, State, STL);
	[] ->
	    %% Skip empty list
	    case Cont of
		[zero_or_more | Cont2] ->
		    compute_cont(Cont2, Mode, GlobalMode, State, STL);
		_ ->
		    compute_cont(Cont, Mode, GlobalMode, State, STL)
	    end;
	_ ->
	    GlobalMode2 =
		case Mode of
		    state_dependent -> GlobalMode;
		    _               -> Mode
		end,
	    case Cont of
		[zero_or_more | Cont2] ->
		    ST = make_cont(Mode, Next, All),
		    compute_cont(Cont2, Mode, GlobalMode2, State, [ST | STL]);
		_ ->
		    ST = make_cont(Mode, Next, Cont),
		    {State, GlobalMode2, [ST | STL]}
	    end
    end;
compute_cont([], GlobalMode, _Mode, _State, STL) ->
    {optional_event, GlobalMode, STL}.

make_cont(Mode, [Next | Cont2], Cont) ->
    #state_transition{mode = Mode, next = Next, cont = [Cont2 | Cont]};
make_cont(Mode, Next, Cont) ->
    #state_transition{mode = Mode, next = Next, cont = Cont}.

%%----------------------------------------------------------------------
%% Send one or more events to event collector process
%% 
%% Events ::= Event* | Event
%% Event  ::= $0-$9 | $a-$k | $A-$K | $S | $L | $Z
%% $S means sleep one second
%% $L means sleep ten seconds
%% $Z means cancel
%% Returns ok | {error, Reason}
%%----------------------------------------------------------------------

report(Pid, [H | T])->
    case report(Pid, H) of
	ok ->
	    report(Pid, T);
	{error, Reason} ->
	    {error, Reason}
    end;
report(_Pid, [])->
    ok;
report(Pid, Event) when pid(Pid) ->
    case Event of
	I when I >= $0, I =< $9 -> cast(Pid, Event);
	A when A >= $a, A =< $k -> cast(Pid, Event);
	A when A >= $A, A =< $K -> cast(Pid, Event);
	cancel                  -> cast(Pid, Event);
	$Z                      -> cast(Pid, cancel);
	$z                      -> cast(Pid, cancel);
	$S                      -> sleep(1);
	$s                      -> sleep(1);
	$L                      -> sleep(10);
	$l                      -> sleep(10);
	_                       -> {error, {illegal_event, Event}}
    end.

sleep(Sec) ->
    timer:sleep(timer:seconds(Sec)),
    ok.

cast(Pid, Event) ->
    Pid ! {?MODULE, self(), Event},
    ok.

%%----------------------------------------------------------------------
%% Feed digit map collector with events
%% Returns: {ok, Letters} | {error, Reason}
%%----------------------------------------------------------------------

test(DigitMap, Events) ->
    Self = self(),
    Pid = spawn_link(?MODULE, test_eval, [DigitMap, Self]),
    report(Pid, Events),
    receive
	{Self, Pid, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    {error, {'EXIT', Reason}}
    end.

test_eval(DigitMap, Parent) ->
    Res = eval(DigitMap),
    unlink(Parent),
    Parent ! {Parent, self(), Res},
    exit(normal).
