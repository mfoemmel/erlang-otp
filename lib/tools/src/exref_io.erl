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
%% Various IO functions of exref.

-module(exref_io).

-export([verbose/3, report_error/2, warning/5]).

-include("exref_state.hrl").


%%
%% Emit verbose messages
%%
verbose(Fmt, Args, S) ->
    case lists:member(verbose, S#state.options) of
	true -> io:format(Fmt, Args);
	false -> false
    end.


%%
%% Report parse errors
%%
report_error({Line, Mod, What}, S) when integer(Line), atom(Mod) ->
    io:format("~s:~w: ~s\n",[S#state.file, Line, 
                             apply(Mod,format_error, [What])]);
report_error({unknown, Mod, What}, S) when  atom(Mod) ->
    io:format("~s: unknown : ~s\n",[S#state.file, 
				    apply(Mod,format_error, [What])]);
report_error(What, S) ->
    io:format("~s ** ERROR (report_error)** ~w\n", 
              [S#state.file,What]).


%%
%% Emit warning
%%
warning(File, Line, Fmt, Args, S) ->
    case lists:any(fun(El)->case El of
				warnings -> true;
				verbose -> true;
				_ -> false
			    end
		   end, S#state.options) of
	true ->
	    io:format("~s:~w: WARNING ", [File, Line]),
	    io:format(Fmt, Args),
	    io:nl();
	false -> false
    end.
