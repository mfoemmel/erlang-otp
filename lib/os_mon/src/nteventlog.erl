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
-module(nteventlog).

%% Purpose : read and monitor the NT eventlog

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/2, start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Ident, MFA) ->
    gen_server:start_link({local, nteventlog}, nteventlog, [Ident, MFA], []).
start(Ident, MFA) ->
    gen_server:start({local, nteventlog}, nteventlog, [Ident, MFA], []).
stop() ->
    gen_server:call(nteventlog, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Identifier,MFA]) ->
    Exe =  filename:join(code:priv_dir(os_mon),"bin/nteventlog.exe")++" "++make_list(Identifier),
    Port = open_port({spawn,Exe},[{packet,2}]),
    {ok, {Port,MFA}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    case Request of
	stop ->
	    {stop, normal, stopped, State};
	_ ->
	    {reply, ok, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {Port,{M,F,A}} = State,
    case Info of
	{Port, {data, Data}} ->
	    T = parse_log(Data),
	    apply(M,F,[T | A]),
	    Port ! {self(), {command, "A"}},
	    {noreply, State};
	_ ->
	    {stop, port_program_failed, State}
    end.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, {Port, MFO}) ->
    Port ! {self(), close},
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

make_list(X) when atom(X) ->
    atom_to_list(X);
make_list(X) ->
    X.

holl_len([$H | Rest], Sum) ->
    {Sum, Rest};
holl_len([ N | Rest], Sum) ->
    NN = N - $0,
    holl_len(Rest, Sum * 10 + NN).
holl_len(L) ->
    holl_len(L,0).

splitlist(L,N) ->
    {lists:sublist(L,N),lists:nthtail(N,L)}. 

hollerith(Str) ->
    {Len, Rest} = holl_len(Str),
    splitlist(Rest,Len).

holl_time(Str) ->
    {Holl,Rest} = hollerith(Str),
    Rev = lists:reverse(Holl),
    B = list_to_integer(lists:reverse(lists:sublist(Rev,6))),
    A = list_to_integer(lists:reverse(lists:nthtail(6,Rev))),
    {{A,B,0},Rest}.

parse_log(Str) ->
    {Time, Rest1} = holl_time(Str),
    {Category,Rest2} = hollerith(Rest1),
    {Facility,Rest3} = hollerith(Rest2),
    {Severity,Rest4} = hollerith(Rest3),
    {Message,_} = hollerith(Rest4),
    {Time,Category,Facility,Severity,Message}.
