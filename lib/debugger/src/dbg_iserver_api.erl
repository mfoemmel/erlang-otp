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
%%% Purpose : This module contains the API for the interpreter
%%%   	      server process. It's purpouse is to encapsulate all use
%%%           of the registered process 'interpret' in one module.
%%%----------------------------------------------------------------------

-module(dbg_iserver_api).

%%-compile(export_all).
-export([start/0]).


-export([
	 new_break/3,
	 delete_break/2,
	 no_break/0,
	 no_break/1,
	 trace/1,
	 stack_trace/1,
	 new_break_options/3,
	 delete/1,
	 interpret/1,
	 load/4,
	 set_state/1,
	 break_at/3,
	 am_followed/1,
	 int_proc/4,
	 old_code/2
	]).


%%
%% Function for initializing the interpreter server
%%

start() ->
    case whereis(interpret) of
	undefined ->
	    Pid = spawn(dbg_iserver,start1,[]),
	    register(interpret,Pid),
	    Pid;
	Pid ->
	    Pid
    end.



%% The following functions makes no assumptions about the status 
%% of the interpreter. If it is not started, they will attempt to 
%% start it.

new_break(Mod, Line, Options) ->
    tell_interpreter({new_break,{Mod,Line},Options}).


delete_break(Mod, Line) ->
    tell_interpreter({delete_break,{Mod,Line}}).


no_break() ->
    tell_interpreter({no_break,{}}).


no_break(Mod) ->
    tell_interpreter({no_break,{Mod}}).
    
trace(Trace) ->
    tell_interpreter({trace,Trace}).

stack_trace(Flag) ->
    tell_interpreter({stack_trace,Flag}).

new_break_options(Mod, Line, NewOptions) ->
    tell_interpreter({new_break_options,{Mod,Line},NewOptions}).

%% Internal function to encapsulate the finding and possible restart
%% of the interpreter server.
%%
tell_interpreter(Mess) ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {self(),Mess};
	_ ->
	    Pid = start(),
	    Pid ! {self(),Mess}
    end.


%% The following functions assumes that the interpreter is started
%% and will return an error if it is not.
%% 

delete(M) ->
    tell_interpreter_norestart({delete,M}).


interpret(Module) ->
    tell_interpreter_norestart({interpret,Module}).


load(ReplyTo, M, File, Binary)->
    tell_interpreter_norestart({load,ReplyTo,M,File,Binary}).



%% tell_interpreter/1 - Sends a message to the interpreter server
%%
%%
%% (???) Currently returns 'ok' even if the message handling were 
%%       not 'successful'

tell_interpreter_norestart(Mess) ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {self(),Mess},
	    ok;					%(???) FIXME: Kludge fix already made. /olin
	_ ->
	    {error, no_interpreter}
    end.



set_state(running) ->
    interpret ! {self(), running};

set_state(waiting) ->
    interpret ! {self(), waiting};

set_state(idle) ->
    interpret ! {self(), idle}.


break_at(Cm, LineNo, AttP) ->
    interpret ! {self(),break_at,Cm,LineNo,AttP}.

am_followed(Fol) ->
    interpret ! {self(),am_followed,Fol}.



int_proc(Msg_handler, M, F,As) ->
    interpret ! {self(),{int_proc,Msg_handler,cr_func(M,F,As)}},
    receive
	{interpret,your_state,State0} ->
	    State0
    end.
    
cr_func(M,F,As) ->
    list_to_atom(lists:flatten(io_lib:format('~w:~w/~w',[M,F,length(As)]))).


old_code(Pid, Module) ->
    interpret ! {self(),old_code, Module ,Pid}.
