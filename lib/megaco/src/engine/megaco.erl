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
%% Purpose : Main API for Megaco/H.248 protocol stack
%%----------------------------------------------------------------------

-module(megaco).

%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([
         start/0,
         stop/0,
        
         start_user/2,
         stop_user/1,

         user_info/2,
         update_user_info/3,
         conn_info/2,
         update_conn_info/3,
         system_info/1,

         connect/4,
         disconnect/2,

         call/3,
         cast/3,
         cancel/2,
         process_received_message/4,
         receive_message/4,

	 parse_digit_map/1,
	 eval_digit_map/1,
	 eval_digit_map/2,
	 report_digit_event/2,
	 test_digit_event/2,

	 encode_binary_term_id/2,
	 decode_binary_term_id/2,

	 ms/0, nc/0, nc/1, ni/0, ni/1,

	 enable_trace/2, disable_trace/0, set_trace/1
        ]).

-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include("megaco_internal.hrl").



%%-----------------------------------------------------------------
%% Starts the Megaco application
%%-----------------------------------------------------------------

start() ->
    application:start(?APPLICATION).

%%-----------------------------------------------------------------
%% Stops the Megaco application
%%-----------------------------------------------------------------

stop() ->
    application:stop(?APPLICATION).

%%-----------------------------------------------------------------
%% Initial configuration of a user
%%-----------------------------------------------------------------

start_user(UserMid, Config) ->
    megaco_config:start_user(UserMid, Config).

%%-----------------------------------------------------------------
%% Delete the configuration of a user
%%-----------------------------------------------------------------

stop_user(UserMid) ->
    megaco_config:stop_user(UserMid).

%%-----------------------------------------------------------------
%% Lookup user information
%%-----------------------------------------------------------------

user_info(UserMid, Item) ->
    megaco_config:user_info(UserMid, Item).

%%-----------------------------------------------------------------
%% Update information about a user
%%-----------------------------------------------------------------

update_user_info(UserMid, Item, Value) ->
    megaco_config:update_user_info(UserMid, Item, Value).

%%-----------------------------------------------------------------
%% Lookup information about an active connection
%%-----------------------------------------------------------------

conn_info(ConnHandle, Item) ->
    megaco_config:conn_info(ConnHandle, Item).

%%-----------------------------------------------------------------
%% Update information about an active connection
%%-----------------------------------------------------------------

update_conn_info(ConnHandle, Item, Value) ->
    megaco_config:update_conn_info(ConnHandle, Item, Value).

%%-----------------------------------------------------------------
%% Lookup system information
%%-----------------------------------------------------------------

system_info(Item) ->
    megaco_config:system_info(Item).

%%-----------------------------------------------------------------
%% Establish a "virtual" connection
%%-----------------------------------------------------------------

connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid) ->
    megaco_messenger:connect(ReceiveHandle, RemoteMid, SendHandle, ControlPid).

%%-----------------------------------------------------------------
%% Tear down a "virtual" connection
%%-----------------------------------------------------------------

disconnect(ConnHandle, Reason) ->
    megaco_messenger:disconnect(ConnHandle, {user_disconnect, Reason}).

%%-----------------------------------------------------------------
%% Sends a transaction request and waits for a reply
%%-----------------------------------------------------------------

call(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:call(ConnHandle, ActionRequests, Options).

%%-----------------------------------------------------------------
%% Sends a transaction request but does NOT wait for a reply
%%-----------------------------------------------------------------

cast(ConnHandle, ActionRequests, Options) ->
    megaco_messenger:cast(ConnHandle, ActionRequests, Options).

%%-----------------------------------------------------------------
%% Cancel all outstanding messages for this connection
%%-----------------------------------------------------------------

cancel(ConnHandle, Reason) ->
    megaco_messenger:cancel(ConnHandle, {user_cancel, Reason}).

%%-----------------------------------------------------------------
%% Process a received message
%%-----------------------------------------------------------------

process_received_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:process_received_message(ReceiveHandle, ControlPid, 
					      SendHandle, BinMsg).

receive_message(ReceiveHandle, ControlPid, SendHandle, BinMsg) ->
    megaco_messenger:receive_message(ReceiveHandle, ControlPid, 
				     SendHandle, BinMsg).


%%-----------------------------------------------------------------
%% Parses a digit map body
%%-----------------------------------------------------------------

parse_digit_map(DigitMapBody) ->
    megaco_digit_map:parse(DigitMapBody).

%%-----------------------------------------------------------------
%% Collect digit map letters according to the digit map
%%-----------------------------------------------------------------

eval_digit_map(DigitMap) ->
    megaco_digit_map:eval(DigitMap).

eval_digit_map(DigitMap, Timers) ->
    megaco_digit_map:eval(DigitMap, Timers).

%%-----------------------------------------------------------------
%% Send one or more events to event collector process
%%-----------------------------------------------------------------

report_digit_event(DigitMapEvalPid, Event) ->
    megaco_digit_map:report(DigitMapEvalPid, Event).

%%-----------------------------------------------------------------
%% Feed digit map collector with events and return the result
%%-----------------------------------------------------------------

test_digit_event(DigitMap, Events) ->
    megaco_digit_map:test(DigitMap, Events).

%%-----------------------------------------------------------------
%% encode_binary_term_id(Config, MegacoTermId) ->
%% 
%%   {ok, TerminationId} | {error, Reason}
%%
%% Encode the Megaco internal form of a termination id (a
%% megaco_term_id record) into ASN.1'1 internal form of a termination
%% id (a 'TerminationId' record).
%% %%-----------------------------------------------------------------

encode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:encode(Config, TermId).

%%-----------------------------------------------------------------
%% decode_binary_term_id(Config, TerminationId) ->
%% 
%%   {ok, MegacoTermId} | {error, Reason}
%%
%% Decode ASN.1's internal form of a termination id (a 'TerminationId'
%% record) into the Megaco internal form of a termination id (a
%% megaco_term_id record).
%%-----------------------------------------------------------------

decode_binary_term_id(Config, TermId) ->
    megaco_binary_term_id:decode(Config, TermId).

%%-----------------------------------------------------------------

ms() ->
    [
     megaco,
     megaco_config,
     megaco_digit_map,
     megaco_edist_encoder,
     megaco_filter,
     megaco_messenger,
     megaco_monitor,
     megaco_sup,
     megaco_user
    ]. 

nc() ->
    Mods = ms(),
    nc(Mods).

nc(all) ->
    application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    application:unload(?APPLICATION),
	    nc(Mods);
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end;
nc(Mods) when list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, compile)].

ni() -> 
    Mods = ms(),
    ni(Mods).

ni(all) -> 
    application:load(?APPLICATION),
    case application:get_key(?APPLICATION, modules) of
	{ok, Mods} ->
	    application:unload(?APPLICATION),
	    ni(Mods);
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end;
ni(Mods) when list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, interpret)].

load(Mod, How) when atom(Mod) ->
    case try_load(Mod, How) of
	ok ->
	    ok;
	_ ->
	    io:format( "~n RETRY ~p FROM: ", [Mod]),
	    ModString = atom_to_list(Mod) ++ ".erl",
	    LibDir = code:lib_dir(?APPLICATION),
	    case find_file([LibDir], ModString) of
		{ok, Abs} ->
		    load(Abs, How);
		{error, Reason} ->
		    io:format( " *** ERROR *** ~p~n", [Reason]),
		    {error, Reason}
	    end
    end;
load(Abs, How) ->
    case try_load(Abs, How) of
	ok ->
	    ok;
	{error, Reason} ->
	    io:format( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end.

try_load(Mod, How) ->
    io:format( " ~p ", [Mod]),
    Flags = [{d, debug}],
    case How of
	compile ->
	    case catch c:nc(Mod, Flags) of
		{ok, _} -> ok;
		Other   -> {error, Other}
	    end;
	interpret ->
	    case catch int:ni(Mod, Flags) of
		{module, _} -> ok;
		Other       -> {error, Other}
	    end
    end.

find_file([Dir | Dirs], File) ->
    case file:list_dir(Dir) of
	{ok, List} ->
	    case lists:member(File, List) of
		true ->
		    {ok, filename:join([Dir, File])};
		false ->
		    SubDirs = [filename:join([Dir, Sub]) || Sub <- List],
		    case find_file(SubDirs, File) of
			{ok, Abs} ->
			    {ok, Abs};
			{error, Reason} ->
			    find_file(Dirs, File)
		    end
	    end;
	{error, Reason} ->
	    find_file(Dirs, File)
    end;
find_file([], File) ->
    {error, {no_such_file, File}}.


%%-----------------------------------------------------------------

%% -----------------------------
%% These functions can be used instead of the et tool for
%% managing trace of the megaco application.

%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port
%% File -> string()
%% Port -> integer()  
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File or the port Port. Note that
%% it starts a tracer server.
%%-----------------------------------------------------------------
enable_trace(Level, File) when list(File) ->
    dbg:tracer(port, dbg:trace_port(file, File)),
    set_trace(Level);
enable_trace(Level, Port) when integer(Port) ->
    dbg:tracer(port, dbg:trace_port(ip, Port)),
    set_trace(Level);
enable_trace(Level, {Fun, Data} = HandleSpec) when function(Fun) ->
    dbg:tracer(process, HandleSpec),
    set_trace(Level).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%% 
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------
disable_trace() ->
    dbg:stop().


%%-----------------------------------------------------------------
%% set_trace(Level) -> void()
%% 
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started. 
%%-----------------------------------------------------------------
set_trace(Level) ->
    Pat = et:make_pattern(Level),
    et:change_pattern(Pat).




