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
%% Purpose: Main API for Event Tracer
%%----------------------------------------------------------------------
%%
%% The Event Tracer (et) enables collection of trace events into
%% a backing storage 
%%
%% ???????????????????
%% Usage of a global tracer
%% 
%% The global tracer runs as a globally registered process.  On all
%% connected Erlang nodes, it starts a process tracer which sends its events
%% to a local port (the port number is generated). On the node where
%% the global broker is running, corresponding consumer processes are
%% started, configured to use the given HandlerSpec.
%% 
%% Whenever new nodes are (dis)connected, this is monitored and new
%% producer/consumers are automatically started and eventually
%% the trace Pattern is set on these nodes.
%%
%% change_pattern(Pattern) -> changes the Pattern on all nodes
%%----------------------------------------------------------------------


-module(et).

-export([
	 phone_home/4, report/4,
	 phone_home/5, report/5,
	 
         make_pattern/1,
	 change_pattern/1,

         make_event/1,

	 file/1
        ]).

%%----------------------------------------------------------------------
%% Reports an event, such as a message
%%
%% DetailLevel() = integer()
%% Actor()       = From | To | Who | term()
%% Label()       = string() | atom() | term()
%% Contents()    = [{Key, Value}] | term()
%%----------------------------------------------------------------------

report(DetailLevel, Who, Label, Contents) ->
    phone_home(DetailLevel, Who, Label, Contents).

report(DetailLevel, From, To, Label, Contents) ->
    %% N.B External call
    ?MODULE:phone_home(DetailLevel, From, To, Label, Contents).

phone_home(DetailLevel, Who, Label, Contents) ->
    %% N.B External call
    ?MODULE:phone_home(DetailLevel, Who, Who, Label, Contents).

phone_home(DetailLevel, From, To, Label, Contents)
  when integer(DetailLevel) ->
    hopefully_traced.

%%----------------------------------------------------------------------
%% Start a new viewer and collector and load them with
%% trace events from a trace file.
%%
%% FileName() = string()
%%----------------------------------------------------------------------

file(FileName) ->
    et_viewer:start_link([{trace_client, {file, FileName}}]).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

make_pattern(Pattern) ->
    et_selector:make_pattern(Pattern).

change_pattern(Pattern) ->
    et_selector:change_pattern(Pattern).

make_event(TraceRecord) ->
    et_selector:make_event(TraceRecord).
