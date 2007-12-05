%%<copyright>
%% <year>2001-2007</year>
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
%%----------------------------------------------------------------------
%% Purpose : Scanner for text encoded Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_flex_scanner).

-export([start/0, stop/1, scan/2]).

start() ->
    (catch do_start()).


do_start() ->
    Path = lib_dir(),
    erl_ddll:start(), 
    load_driver(Path),
    Port = open_drv_port(),
    {ok, Port}.


lib_dir() ->
    case code:priv_dir(megaco) of
	{error, Reason} ->
	    throw({error, {priv_dir, Reason}});
	P when list(P) ->
	    P ++ "/lib"
    end.
    

load_driver(Path) ->
    case erl_ddll:load_driver(Path, drv_name()) of
	ok ->
	    ok;
	 {error, Reason} ->
	    throw({error, {load_driver, Reason}})
    end.


open_drv_port() ->
    case (catch erlang:open_port({spawn, drv_name()}, [binary])) of
	Port when port(Port) ->
	    Port;
	{'EXIT', Reason} ->
	    erl_ddll:unload_driver(drv_name()),
	    throw({error, {open_port, Reason}})
    end.

drv_name() ->
    case erlang:system_info(threads) of
	true ->
	    "megaco_flex_scanner_drv_mt";
	false ->
	    "megaco_flex_scanner_drv"
    end.

stop(Port) ->
    erlang:port_close(Port), 
    erl_ddll:unload_driver(drv_name()),
    stopped.


scan(Binary, Port) ->
    case erlang:port_control(Port, $s, Binary) of
	[] ->
	    receive
		{tokens, Tokens, LatestLine} ->
		    Vsn = version(Tokens),
		    {ok, Tokens, Vsn, LatestLine} 
	    after 0 ->
		    {error, "Driver term send failure", 1}
	    end;
	Reason ->
	    {error, Reason, 1}
    end.


version([]) ->
    99; % Let the parser deal with this
version([{'SafeChars',_,"!/1"}|_]) ->
    1;
version([{'SafeChars',_,"megaco/1"}|_]) ->
    1;
version([{'SafeChars',_,"!/2"}|_]) ->
    2;
version([{'SafeChars',_,"megaco/2"}|_]) ->
    2;
version([{'SafeChars',_,"!/3"}|_]) ->
    3;
version([{'SafeChars',_,"megaco/3"}|_]) ->
    3;
version([{'SafeChars',_,[$!, $/ | Vstr]}|_]) ->
    guess_version(Vstr);
version([{'SafeChars',_,[$m, $e, $g, $a, $c, $o, $/ | Vstr]}|_]) ->
    guess_version(Vstr);
version([_|T]) ->
    version(T).


guess_version([C]) when (48 =< C) and (C =< 57) ->
    C-48;
guess_version(Str) when is_list(Str) ->
    case (catch list_to_integer(Str)) of
	I when is_integer(I) ->
	    I;
	_ ->
	    99 % Let the parser deal with this
    end;
guess_version(_) ->
    99. % Let the parser deal with this

