%%<copyright>
%% <year>2003-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% Purpose: Utility module used by the codec suites when testing flex 
%%          configured text codec(s).
%%----------------------------------------------------------------------

-module(megaco_codec_flex_lib).

%% ----

-include("megaco_test_lib.hrl").

%% ----

-export([
	 init/1, 
	 finish/1,
	 scanner_conf/1,
	 start/0, 
	 stop/1
	 ]).

-export([
	 handler/1
	]).  


%% ----

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Config) when list(Config) ->
    Flag = process_flag(trap_exit, true),    
    Res = (catch start()),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
	    skip(Reason);
	{ok, FlexConfig} ->
	    [{flex_scanner, FlexConfig} | Config]
    end.
    

finish(Config) when list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, {Pid, _Conf}}} ->
	    stop(Pid),
	    lists:keydelete(flex_scanner, 1, Config);
	false ->
	    Config
    end.
    

start() ->
    Pid = proc_lib:spawn(?MODULE, handler, [self()]),
    receive
	{flex_scanner_started, Pid, Conf} ->
	    {ok, {Pid, Conf}};
	{flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
 	    ?LOG("start_flex_scanner -> failed loading flex scanner driver: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}};
	{flex_scanner_error, Reason} ->
 	    ?LOG("start_flex_scanner -> error: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}}
    after 10000 ->
	    exit(Pid, kill),
	    {error, {failed_starting_flex_scanner, timeout}}
    end.


scanner_conf(Config) when list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, {Pid, Conf}}} ->
	    case ping_flex_scanner(Pid) of
		ok ->
		    Conf;
		Else ->
		    skip({no_response_from_flex_scanner_handler, Else})
	    end;
	false ->
	    skip("Flex scanner driver not loaded")
    end.


ping_flex_scanner(Pid) ->
    Pid ! {ping, self()},
    receive
	{pong, Pid} ->
	    ok
    after 5000 ->
	    timeout
    end.


stop(Pid) ->
    Pid ! stop.


handler(Pid) ->
    case (catch megaco_flex_scanner:start()) of
	{ok, Port} when port(Port) ->
	    Pid ! {flex_scanner_started, self(), {flex, Port}},
	    handler(Pid, Port);
	{error, {load_driver, {open_error, Reason}}} ->
	    Error = {failed_loading_flex_scanner_driver, Reason},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error);
	{error, {load_driver, Reason}} ->
	    Error = {failed_loading_flex_scanner_driver, Reason},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error);
	Else ->
	    Error = {unknown_result_from_start_flex_scanner, Else},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error)
    end.

handler(Pid, Port) ->
    receive
	{ping, Pinger} ->
	    Pinger ! {pong, self()},
	    handler(Pid, Port);
	{'EXIT', Port, Reason} ->
	    Pid ! {flex_scanner_exit, Reason},
	    exit({flex_scanner_exit, Reason});
	stop ->
	    megaco_flex_scanner:stop(Port),
	    exit(normal);
	Other ->
	    io:format("flex scanner handler got something:~n"
		      "~p", [Other]),
	    handler(Pid, Port)
    end.
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip(Reason) ->
    megaco_codec_test_lib:skip(Reason).

