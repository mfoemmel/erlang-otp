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
%%----------------------------------------------------------------------
%% Purpose : Scanner for text encoded Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_flex_scanner).

-export([start/0, stop/1, scan/2]).

-define(DRV_NAME, "megaco_flex_scanner_drv").


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
    case erl_ddll:load_driver(Path, ?DRV_NAME) of
	ok ->
	    ok;
	 {error, Reason} ->
	    throw({error, {load_driver, Reason}})
    end.


open_drv_port() ->
    case (catch erlang:open_port({spawn, ?DRV_NAME}, [binary])) of
	Port when port(Port) ->
	    Port;
	{'EXIT', Reason} ->
	    erl_ddll:unload_driver(?DRV_NAME),
	    throw({error, {open_port, Reason}})
    end.


stop(Port) ->
    erlang:port_close(Port), 
    erl_ddll:unload_driver(?DRV_NAME),
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
    1;
version([{'SafeChars',1,"!/1"}|_]) ->
    1;
version([{'SafeChars',1,"megaco/1"}|_]) ->
    1;
version([{'SafeChars',1,"!/2"}|_]) ->
    2;
version([{'SafeChars',1,"megaco/2"}|_]) ->
    2;
version([_|T]) ->
    version(T).
