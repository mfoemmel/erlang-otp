%%<copyright>
%% <year>1999-2007</year>
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
%% File    : client.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(client).

-export([produce/0,init/3,call/0]).

-define(SERVER,{rmod_random_impl,
		list_to_atom("babbis@"++hd(tl(string:tokens(atom_to_list(node()),"@"))))}).
-define(CLIENTMOD,'rmod_random').

produce() ->
    ?CLIENTMOD:produce(?SERVER).


init(Seed1, Seed2, Seed3) ->
    io:format("Init..."),
    ?CLIENTMOD:init(?SERVER,Seed1, Seed2, Seed3),
    io:format("ok\n").


call() ->
    init(1,2,3),
    produce(0).


produce(10) ->
    ok;
produce(Ctr) ->
    N = produce(),
    io:format("Random~p = ~p\n",[Ctr,N]),
    produce(Ctr+1).
