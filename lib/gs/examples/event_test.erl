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
%% Demo for testing some events

-module(event_test).

-export([start/0,init/0]).



start() ->
    spawn(event_test,init,[]).

init() ->
    S=gs:start(),
    W=gs:window(S,[{map,true},{keypress,true},{buttonpress,true}]),
    gs:button(W,[{label,{text,"Press Me"}},{enter,true},{leave,true}]),
    event_loop().


event_loop() ->
    receive
	X ->
	    io:format("Got event: ~p~n",[X]),
	    event_loop()
    end.


		     
    
