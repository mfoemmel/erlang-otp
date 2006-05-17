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
%%--------------------------------------------------------------------
%%
%%    COPYRIGHT Ericsson Telecom AB 1998
%%
%%    126 25 STOCKHOLM
%%    SWEDEN
%%    tel int + 46 8 719 0000
%%
%%    The copyright to the computer program herein is the property of
%%    Ericsson Telecom AB, Sweden. The program may be used and/or
%%    copied only with the written permission from Ericsson Telecom AB
%%    or in accordance with the terms and conditions stipulated in the
%%    agreement/contract under which the program have been supplied.
%%
%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: tracer.erl
%% 
%% Description:
%%    This file contains an example of pre and post conditions for 
%%    the corba backend.
%%
%% Creation date: 990125
%%
%%-----------------------------------------------------------------
-module(tracer).
-include("m.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([pre/3, post/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
pre(M, F, [State, I]) when integer(I) ->
    io:format("Precond called in process ~p: ~s:~s() ~p\n", [self(), M, F,  [State, I]]),
    ok;
pre(_M, _F, _A) -> %% Just an silly example to get an exception case
    corba:raise(#'m_NotAnInteger'{}).

post(M, F, A, R) ->
    io:format("Postcond called in process ~p: ~s:~s() ~p ~p\n", [self(), M, F, A, R]),
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
