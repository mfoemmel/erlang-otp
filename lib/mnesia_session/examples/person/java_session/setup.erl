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
%%% Purpose : Psevdo make and init the java_session
%%%----------------------------------------------------------------------

-module(setup).

-export([start_apps/1, server/0, make/0, make_clean/0]).


make() ->
    ICJAR = code:priv_dir(ic) ++ "/ic.jar",
    JIJAR = code:priv_dir(jinterface) ++ "/OtpErlang.jar",
    {ok,CWD} = file:get_cwd(),
    file:set_cwd(".."),

    io:format("\nic:gen(person,[{be,erl_genserv},{outdir,~p}]),\n",[CWD]),
    ic:gen(person,[{be,erl_genserv},{outdir,CWD}]),

    io:format("\nic:gen(person,[{be,java},{outdir,~p}]),\n",[CWD]),
    ic:gen(person,[{be,java},{outdir,CWD}]),
    
    file:set_cwd(CWD),
    io:format("\nmake:all([load]),\n"),
    make:all([load]),

    JAVACOMP = 
	"javac -classpath \".:./mnesia_session.jar:"++
	ICJAR++":"++JIJAR++"\" */*/*.java */*.java *.java",
    io:format("\n"++JAVACOMP++"\n\n"),
    os:cmd(JAVACOMP),
    ok.

make_clean() ->
    case os:type() of
        {win32, _} ->
	    not_ok;
	_ ->
	    CLEAN = "rm -rf persons *.class *.beam o*rl p*rl",
	    io:format(CLEAN++"\n\n"),
	    os:cmd(CLEAN),
	    ok
    end.


server() ->
    Ns = [node()],
    start_apps(Ns).


%% Start the needed applications mnesia, orber and mnesia_corba_session.
start_apps(Ns) ->    
    [rpc:call(Node, mnesia, start, []) || Node <- Ns],
    application:start(mnesia_session).

