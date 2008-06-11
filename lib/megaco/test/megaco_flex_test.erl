%%<copyright>
%% <year>2008-2008</year>
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
%% Purpose: Test various aspects of the flex scanner handling
%%
%% Test:    ts:run(megaco, megaco_flex_test, [batch]).
%%
%%----------------------------------------------------------------------

-module(megaco_flex_test).

-include("megaco_test_lib.hrl").

-export([
	 t/0, t/1, 

	 init_per_testcase/2, fin_per_testcase/2,

	 all/1,
	 plain/1,
	 port_exit/1,
	 garbage_in/1

	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     plain,
     port_exit,
     garbage_in
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain(suite) ->
    [];
plain(doc) ->
    ["This is to simply test that it is possible to start and stop the "
     "flex handler."];
plain(Config) when is_list(Config) ->
    {ok, Pid, _PortInfo} = megaco_flex_scanner_handler:start_link(),
    megaco_flex_scanner_handler:stop(Pid),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

port_exit(suite) ->
    [];
port_exit(doc) ->
    ["Test that the handler detects and handles an exiting port."];
port_exit(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    {ok, Pid, {flex, Port}} = megaco_flex_scanner_handler:start_link(),

    exit(Port, simulated_crash), 
    
    receive
	{'EXIT', Pid, _} ->
	    ok
    after 5000 ->
	    megaco_flex_scanner_handler:stop(Pid),
	    {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

garbage_in(suite) ->
    [];
garbage_in(doc) ->
    ["Send in various unexpected messages and requeststo the handler "
     "to see that it does die on us. "];
garbage_in(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    {ok, Pid, _PortInfo} = megaco_flex_scanner_handler:start_link(),

    {error, _} = gen_server:call(Pid, garbage_request), 
    gen_server:cast(Pid, garbage_msg),
    Pid ! garbage_info,

    receive
	Any ->
	    {error, {unexpected_msg, Any}}
    after 1000 ->
	    megaco_flex_scanner_handler:stop(Pid),
	    ok
    end.

