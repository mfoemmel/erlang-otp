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
-module(sasl_report_tty_h).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all sasl_* events formatted to stdout.
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init(Type) ->
% should link to user (or group_leader???)
    {ok, Type}.
    
handle_event({Type, GL, Msg}, Type) when node(GL) /= node() ->
    {ok, Type};
handle_event(Event, Type) ->
    sasl_report:write_report(standard_io, Type, tag_event(Event)),
    {ok, Type}.

handle_info(_, Type) -> {ok, Type}.

handle_call(_Query, _Type) -> {error, bad_query}.

terminate(_Reason, _Type) ->
    [].

tag_event(Event) ->    
    {calendar:local_time(), Event}.

