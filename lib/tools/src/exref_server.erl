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
%%
%% Contains all callback functions for exref.
%% Used only by gen_server.

-module(exref_server).

-export([handle_call/3, init/1, terminate/2, handle_info/2]).

-include("exref_state.hrl").




init(_) ->
%    gen_server:loop(exref,
%		 #state {fgraph = digraph:new(), mgraph = digraph:new()},
%		 {exref, dispatch}).
    State = #state {fgraph = digraph:new(), mgraph = digraph:new()},
    {ok,State}.


terminate(_,S) ->
    digraph:delete(S#state.fgraph),
    digraph:delete(S#state.mgraph),
    stopped.

handle_info(_,S) ->
	{noreply,S}.

handle_call(stop,_From, S) ->
    {stop, normal, stopped, S};

handle_call({includes, Dirs},_From, S) ->
    AbsDirs = [filename:absname(X) || X <- Dirs],
    {reply,
     ok,
     S#state{includes = lists:append(S#state.includes, AbsDirs)}};

handle_call({excludes, Mods},_From, S) ->
    {reply,
     ok,
     S#state{excludes = lists:append(S#state.excludes, Mods)}};

handle_call({defs, Defs},_From, S) ->
    {reply,
     ok,
     S#state{defs = lists:append(S#state.defs, Defs)}};

handle_call(get_modules,_From, S) ->
    {reply, S#state.modules, S};

handle_call({delete_module, Ms},_From, S) ->
    case (catch exref_cross_ref:del_module(Ms, S)) of
	{'EXIT',Reason} ->
	    {reply, {error,Reason}, S};
	S1 ->
	    { reply, true, S1 }
    end;

handle_call({module, Mods, Opts},_From, S) ->
    S1 = S#state{auto = [], options = Opts, failed = []},
    case (catch exref_cross_ref:xref_module(Mods, S1)) of
	{'EXIT',Reason} ->
	    {reply,{error,Reason},S1};
	{OK, S2} ->
	    {reply, OK, S2}
    end;

handle_call({directory, Dir, Opts},_From, S) ->
    S1 = S#state{auto = [], options = Opts, failed = []},
    AbsDir = filename:absname(Dir),
    case (catch exref_cross_ref:xref_directory(AbsDir, S1)) of
	{'EXIT',Reason} ->
	    {reply,{error,Reason},S};
	{OK, S2} ->
	    {reply, OK, S2}
    end;

handle_call({directory_module, Dir, Mods, Opts},_From, S) ->
    S1 = S#state{auto = [], options = Opts, failed = []},
    AbsDir = filename:absname(Dir),
    case (catch exref_cross_ref:xref_directory_module(AbsDir, Mods, S1)) of
	{'EXIT',Reason} ->
	    {reply,{error,Reason},S};
	{OK, S2} ->
	    {reply, OK, S2}
    end;

handle_call({analyse, What, Args},_From, S) ->
    case (catch exref_analysis:do_analyse(What, Args, S)) of
	{'EXIT',Reason} ->
	    {reply,{error,Reason},S};
	Res ->
	    {reply, Res, S}
    end;

handle_call(Req,_From, S) ->
    {reply, {error, Req}, S}.

