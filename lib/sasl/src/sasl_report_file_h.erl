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
-module(sasl_report_file_h).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all sasl_* events formatted to file
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init({File, Type}) ->
    process_flag(trap_exit, true),
    case file:open(File, write) of
	{ok,Fd} ->
	    {ok, {Fd, File, Type}};
	What ->
	    What
    end.
    
handle_event({Type, GL, Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event(Event, {Fd, File, Type}) ->
    sasl_report:write_report(Fd, Type, tag_event(Event)),
    {ok, {Fd, File, Type}};
handle_event(_, State) ->
    {ok, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, File, Type}) ->
    remove_handler;
handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, _State) -> {error, bad_query}.

terminate(_, {Fd, File, Type}) ->
    file:close(Fd),
    [].

tag_event(Event) ->    
    {calendar:local_time(), Event}.
