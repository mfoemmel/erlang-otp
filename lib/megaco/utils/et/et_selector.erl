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
%% Purpose: Define event transforms and trace patterns 
%%----------------------------------------------------------------------

-module(et_selector).

-export([
         make_pattern/1, 
	 change_pattern/1,
         make_event/1
        ]).

-include("et.hrl").

%%----------------------------------------------------------------------
%% Makes trace pattern suitable to feed change_pattern/1
%%
%% Pattern()     = DetailLevel()
%% DetailLevel() = min | max | integer()
%%----------------------------------------------------------------------

make_pattern(min) ->
    [];
make_pattern(max) ->
    Head = ['$1', '_', '_', '_', '_'],
    Body = [],
    Cond = [],
    [{Head, Cond, Body}];
make_pattern(DetailLevel) when integer(DetailLevel) ->
    Head = ['$1', '_', '_', '_', '_'],
    Body = [],
    Cond = [{ '<', '$1', DetailLevel}],
    [{Head, Cond, Body}];
make_pattern(Pattern) ->
    exit({bad_pattern, Pattern}).

%%----------------------------------------------------------------------
%% Activates/deactivates tracing by changing the current trace Pattern.
%%
%% The final Pattern must be a valid MatchSpec according to
%% erlang:trace_pattern/2.
%%
%% For conveniance reasons the pattern can be replaced with the
%% shorter notion of a trace level. The trace level is internally
%% converted to an appropriate Pattern with et:make_pattern/1.
%%
%% change_pattern(min) ->
%%   deactivates tracing of calls to phone_home/4,5
%%   
%% change_pattern(max) ->
%%   activates tracing of all calls to phone_home/4,5
%%   
%% change_pattern(Integer) ->
%%   activates tracing of all calls to phone_home/4,5 with
%%   DetailLevel >= Integer
%%   
%% change_pattern([]) ->
%%   deactivates tracing of all calls to phone_home/4,5 with
%%----------------------------------------------------------------------

change_pattern(Pattern) ->
    MFA = {et, phone_home, 5},
    case Pattern of
        [] ->
            error_to_exit(dbg:ctp(MFA)),
            error_to_exit(dbg:p(all, clear));
        List when list(List) ->
            error_to_exit(dbg:ctp(MFA)),
            error_to_exit(dbg:tp(MFA, Pattern)),
            error_to_exit(dbg:p(all, [call, timestamp]));
        Other ->
            change_pattern(make_pattern(Other))
    end.

error_to_exit({error, Reason}) ->
    exit(Reason);
error_to_exit({ok, Res}) ->
    Res.

%%----------------------------------------------------------------------
%% Transforms trace data and makes an event record out of it
%%
%% Valid trace data:
%% 
%%     {trace, Pid, Label, Info}
%%     {trace, Pid, Label, Info, Extra}
%%     {trace_ts, Pid, Label, Info, ReportedTS}
%%     {trace_ts, Pid, Label, Info, Extra, ReportedTS}
%%     {seq_trace, Label, Info}
%%     {seq_trace, Label, Info, ReportedTS}
%%     {drop, NumberOfDroppedItems}
%%     #event{}
%%     
%% Returns:
%%
%%   {true, Event} - where Event is an #event{} record representing the
%% 		     trace data
%%   true          - means that the trace data already is an event
%%                   record and that it is valid as it is.
%%                   No transformation is needed.
%%   false         - means that the trace data is uninteresting and
%%                   should be dropped
%%----------------------------------------------------------------------

make_event(E) when record(E, event) ->
    true;
make_event(Trace) ->
    ParsedTS = erlang:now(),
    case Trace of
        {trace, Pid, Label, Info} ->
            make_event(Trace, ParsedTS, ParsedTS, Pid, Label, [Info]);
        {trace, Pid, Label, Info, Extra} ->
            make_event(Trace, ParsedTS, ParsedTS, Pid, Label, [Info, Extra]);
        {trace_ts, Pid, Label, Info, ReportedTS} ->
            make_event(Trace, ParsedTS, ReportedTS, Pid, Label, [Info]);
        {trace_ts, Pid, Label, Info, Extra, ReportedTS} ->
            make_event(Trace, ParsedTS, ReportedTS, Pid, Label, [Info, Extra]);
        {seq_trace, Label, Info} ->
            make_seq_event(Trace, ParsedTS, ParsedTS, Label, Info);
        {seq_trace, Label, Info, ReportedTS} ->
            make_seq_event(Trace, ParsedTS, ReportedTS, Label, Info);
        {drop, NumberOfDroppedItems} ->
            DetailLevel = 20,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ParsedTS,
                          event_ts     = ParsedTS,
                          from         = undefined,
                          to           = undefined, 
                          label        = drop, 
                          contents     = [{drop, NumberOfDroppedItems}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~p~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.

make_seq_event(Trace, ParsedTS, ReportedTS, Label, Info) ->
    case Info of
        {send, Serial, From, To, Msg} ->
            DetailLevel = 15,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS, 
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = To, 
                          label        = {seq_send, Label},
                          contents     = [{serial, Serial}, {msg, Msg}]}};
        {'receive', Serial, From, To, Msg} ->
            DetailLevel = 10,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = To,
                          label        = {seq_receive, Label}, 
                          contents     = [{serial, Serial}, {msg, Msg}]}};
        {print, Serial, From, _, UserInfo} ->
            DetailLevel = 5,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS, 
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = From, 
                          label        = {seq_print, Label},
                          contents     = [{serial, Serial}, {user_info, UserInfo}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~p~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.

make_event(Trace, ParsedTS, ReportedTS, From, Label, Contents) ->
    case Label of
        'receive' ->
            DetailLevel = 35,
            [Msg] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{msg, Msg}]}};
	send ->
            DetailLevel = 40,
            [Msg, To] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = To,
                          label        = Label,
                          contents     = [{msg, Msg}]}};
	send_to_non_existing_process ->
            DetailLevel = 40,
            [Msg, To] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = To,
                          label        = Label,
                          contents     = [{msg, Msg}]}};
        call ->
	    case Contents of
                [{et, phone_home, [UserDetailLevel, UserFrom, UserTo, UserLabel, UserContents]}] ->
                    {true, #event{detail_level = UserDetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = UserFrom,
                                  to           = UserTo,
                                  label        = UserLabel,
                                  contents     = UserContents}}; % Term
                [MFA] ->
                    DetailLevel = 45,
                    {true, #event{detail_level = DetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = From,
                                  to           = From,
                                  label        = Label,
                                  contents     = [{mfa, MFA}]}};
		[MFA, PamResult] ->
                    DetailLevel = 45,
                    {true, #event{detail_level = DetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = From,
                                  to           = From,
                                  label        = Label,
                                  contents     = [{mfa, MFA}, {pam_result, PamResult}]}}
            end;
	return_to ->
            DetailLevel = 50,
	    [MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{mfa, MFA}]}};
        return_from ->
            DetailLevel = 52,
 	    [MFA, ReturnValue] = Contents,
	    {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{mfa, MFA}, {return, ReturnValue}]}};
        spawn ->
            DetailLevel = 25,
	    [NewPid, MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = NewPid,
                          label        = Label,
                          contents     = [{pid, NewPid}, {mfa, MFA}]}}; % MFA | Term
        exit ->
            DetailLevel = 30,
	    [Reason] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{reason, Contents}]}};
        link ->
            DetailLevel = 55,
            [LinkTo] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = LinkTo,
                          label        = Label,
                          contents     = [{pid, LinkTo}]}};
        unlink ->
            DetailLevel = 60,
            [UnlinkFrom] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = UnlinkFrom,
                          label        = Label,
                          contents     = [{pid, UnlinkFrom}]}};
        getting_linked ->
            DetailLevel = 65,
            [LinkTo] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = LinkTo,
                          label        = Label,
                          contents     = [{pid, LinkTo}]}};
        getting_unlinked ->
            DetailLevel = 67,
            [UnlinkFrom] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = UnlinkFrom,
                          label        = Label,
                          contents     = [{pid, UnlinkFrom}]}};
        register ->
            DetailLevel = 70,
	    [Name] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{name, Name}]}};
        unregister ->
            DetailLevel = 75,
 	    [Name] = Contents,
	    {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{name, Name}]}};
        in ->
            DetailLevel = 90,
	    [MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{mfa, MFA}]}}; % MFA | 0
        out ->
            DetailLevel = 95,
 	    [MFA] = Contents,
	    {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{mfa, MFA}]}}; % MFA | 0
        gc_start ->
            DetailLevel = 80,
	    [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{gc_items, GcKeyValueList}]}};
        gc_end ->
            DetailLevel = 85,
	    [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
			  contents     = [{gc_items, GcKeyValueList}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~p~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.
