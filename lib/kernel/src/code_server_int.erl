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
-module(code_server_int).

%% Holds the interpreter related code of the code_server.

%%-compile(export_all).

-export([del_interpret/2,
	 delete_interpret/2,
	 load_interpret/4,
	 add_interpret/3,
	 ints/1]).


ints([Mod|Mods]) -> [{Mod,interpreted}|ints(Mods)];
ints([])         -> [].


del_interpret({module,Mod},Int) -> delete_interpret(Mod,Int);
del_interpret(_,Int)            -> Int.




delete_interpret(Module,Int) ->
    M = code_aux:to_atom(Module),
    case lists:member(M,Int) of
	true ->
	    NewModules = lists:delete(M,Int),
	    tell_interpreter({delete,M}),
	    NewModules;
	_ ->
	    Int
    end.



add_interpret(Module,Int,Db) when atom(Module) ->
    case code_aux:sticky(Module,Db) of
	true ->     %% Sticky file reject the load (interpretation)
	    error_logger:error_msg("Can't interpret module that resides in sticky dir\n",[]),
	    {{error, sticky_directory}, Int};
	false ->
	    case lists:member(Module,Int) of
		false ->
		    case tell_interpreter({interpret,Module}) of
			ok -> 
			    {{module,Module},[Module|Int]};
			Error ->
			    {Error,Int}
		    end;
		true ->
		    {{module,Module},Int}
	    end
    end;
add_interpret(Module,Int,Db) when list(Module) ->
    add_interpret(list_to_atom(Module),Int,Db);
add_interpret(_,Int,_) ->
    {{error,badarg},Int}.

%% tell_interpreter/1 - Sends a message to the interpreter server
%%
%%
%% (???) Currently returns 'ok' even if the message handling were 
%%       not 'successful'

tell_interpreter(Mess) ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {self(),Mess},
	    ok;					%(???) FIXME: Kludge fix already made. /olin
	_ ->
	    {error, no_interpreter}
    end.


load_interpret(ReplyTo,File,Module,Binary) ->
    M = code_aux:to_atom(Module),
    delete_modules([M]),
    tell_interpreter({load,ReplyTo,M,File,Binary}).

delete_modules([Mod|Modules]) ->
    code_aux:do_purge(Mod),
    erlang:delete_module(Mod),
    delete_modules(Modules);
delete_modules([]) ->
    true.
