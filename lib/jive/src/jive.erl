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
%%%----------------------------------------------------------------------
%%% Purpose   : Interface module for the process 'jive_server'
%%%----------------------------------------------------------------------

%%---------------------------------------------------------------------------
%%
%% The process implemented by this 'gen_server' callback module is 
%% responsible for the administration of process id
%% translation, access rights etc. The module was changed to use
%% the 'gen_server' behaviour but for backward compatibility it
%% returns the same values as before. This also means that a call
%% that fails and could have given a meaningsful error message
%% crash instead, else it would be an interface change.
%%
%%---------------------------------------------------------------------------
%%
%% APPLICATION PROCESS STRUCTURE
%%
%% The top of the application under the application master is a supervisor
%% that has the 'one_for_all' property (XX: Why not 'one_for_one'?).
%% 
%%
%%                         +---------------+
%%                         |   jive_sup    |
%%                         +---------------+
%%                            |   |   |
%%           +----------------+   |   +-------------------+
%%           |                    |                       |
%%           |                    |                       |
%%      +---------+          +---------+          +---------------+
%%     /           \        /           \         | Worker superv |
%%    | jive_server |      | jive_broker |        +---------------+
%%     \           /        \           /             |   |   | |
%%      +---------+          +---------+        +-----+   |   . .
%%                                              |         |
%%                                            +---+     +---+  
%%                                           /     \   /     \ . . . .
%%                                 Workers   \     /   \     / 
%%                                            +---+     +---+  
%%
%% The processes are:
%%
%% Reg.name.       Module          Description
%% ---------------------------------------------------------------------
%% jive_server     jive            The administrative process.
%% jive_broker     jive_broker     Waits for a connection and spawns workers
%% -               jive_worker_sup Supervisor for the workers
%% -               jive_worker     Represent the Java program on Erlang side
%% 
%%---------------------------------------------------------------------------

%%---------------------------------------------------------------------------
%% 
%% APPLICATION FUNCTIONAL DESCRIPTION
%% 
%% When the 'jive_broker' get a socket connection initiated from Java
%% it starts a "worker" process that represent the Java side in the
%% Erlang environment. This process can execute Erlang functions if
%% the access restrictions allow it and it can start new Erlang 
%% processes not part of this application (but linked to XXX: Is this
%% right?). It can send messages to Erlang processes that it know a 
%% Erlang process identifier (PID) mapping to a plain integer that is
%% used at the Java side to represent the Erlang process. It can receive
%% messages to the "proxy" process that is forwarded to the Java side.
%% A special format of the messages are used
%%
%%     	{send,Receiver,Msg}
%%
%% where "Receiver" is XXXXXXXX and "Msg" is what is sent to the Java
%% side.
%% 
%%---------------------------------------------------------------------------

%%
%% This process is started by the 'jive_sup' supervisor.
%%

-module(jive).

%% Interface functions
-export([start/0,
	 start/1,
	 stop/0,
	 get_pid/1,
	 register_pid/1,
	 unregister_pid/1,
	 list_to_string/1,
	 string_to_list/1,
	 allow/1]). 

%% Internal application interface
-export([check_allowed/3,
	 dump_state/0]).

-define(APPLICATION, ?MODULE).			% Happen to be the same

-include("jive.hrl").

%%---------------------------------------------------------------------------
%% Exported functions (interface)
%%---------------------------------------------------------------------------

%%
%% This interface try to support both the old way of starting Jive,
%% jive:start() or jive:start(Port) and as an application, 
%% application:start(jive).
%%

%%
%% Old interface, returns a plain Pid.
%%
%% The manual page states this function always return a value
%% so we crash if the start fails in any other way than that
%% it is already started.
%%
%% XXX: Is the start syncronious so we can use whereis(jive_server)
%% to get the pid?
%%

start() ->
    start(?DEFAULT_PORT).

start(Port) ->
    case jive_worker_sup:start() of
        {ok,_} ->
	    {ok,_} = jive_broker:start([{port,Port}]),
	    {ok,Pid} = jive_server:start_server(),
	    Pid;
	{error, {already_started, jive}} ->
	    whereis(jive_server)
    end.

%%    
%% Stop the server
%% XXX: Is this the way to stop a gen_server, we want
%%      to stop the whole app, right?
%%
stop() ->
    gen_server:cast(jive_server, shutdown),	% XXX: Is this needed?
    shutdown.

%%
%% Get the pid from a pid-id (Java process id)
%%
get_pid(Jpid) when integer(Jpid) ->
    gen_server:call(jive_server, {get,Jpid}, infinity);
get_pid({pidid, Jpid}) when integer(Jpid) ->
    gen_server:call(jive_server, {get,Jpid}, infinity).

%%
%% register a pid
%%
register_pid(Pid) when pid(Pid) ->
    gen_server:call(jive_server, {set,Pid}, infinity).

%%
%% unregister a pid
%%
unregister_pid(Pid) when pid(Pid) ->
    gen_server:cast(jive_server, {remove,Pid}),
    {remove,Pid}.

%%
%% allow Java to connect to a function in a module
%%
%% XXX: We could test if the function exists?
%%
allow({M, F, A}) when atom(M), atom(F), integer(A) ->
    gen_server:cast(jive_server, {allow,{M, F, A}}),
    {allow, {M, F, A}};

%%
%% allow Java to connect to all functions in all available modules
%% (for debugging)
%%
allow(all) ->
    gen_server:cast(jive_server, {allow,all}),
    {allow,all}.

%%
%% Convert a list to a string, no checking is done
%%
%% XXX: Check that it really is a string, i.e. list of numbers < 256?
%%
list_to_string(List) ->
    {string,List}.

%%
%% Convert a string to a list, no checking is done
%%
string_to_list({string, List}) ->
    List.

%%---------------------------------------------------------------------------
%% Application interface functions
%%---------------------------------------------------------------------------

check_allowed(Module, Function, Arity) ->
    gen_server:call(jive_server, {check_allowed, Module, Function, Arity},
			 infinity).

dump_state() ->
    gen_server:cast(jive_server, dump_state).

%%--------------------------------EOF----------------------------------------
