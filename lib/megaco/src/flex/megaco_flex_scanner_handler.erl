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
%%
%%----------------------------------------------------------------------
%% Purpose: Handle the flex scanner
%%----------------------------------------------------------------------

-module(megaco_flex_scanner_handler).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/app/megaco_internal.hrl"). 


%% External exports
-export([start_link/0, get_config/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2,
	 code_change/3]).

-record(state, {conf}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() -> 
    case gen_server:start_link(megaco_flex_scanner_handler, [], []) of
	{ok, Pid} ->
	    Conf = get_config(Pid),
	    {ok, Pid, Conf};
	Else ->
	    Else
    end.
	
get_config(Pid) ->
    gen_server:call(Pid, get_config).


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
init([]) ->
    process_flag(trap_exit, true),
    case megaco_flex_scanner:start() of
	{ok, Port} ->
	    {ok, #state{conf = {flex, Port}}};
	Else ->
	    {stop, {failed_starting_scanner, Else}}
    end.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(get_config, _From, #state{conf = Conf} = S) ->
    {reply, Conf, S};

handle_call(Req, From, S) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w", [From, Req]),
    {reply, ok, S}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Port, Error}, #state{conf = {flex, Port}} = S) ->
    error_msg("Port [~p] exited:"
	      "~n~w", [Port, Error]),
    {stop, {port_exit, Port, Error}, S};

handle_info({'EXIT', Port, _Error}, S) when is_port(Port) ->
    %% This is propably the old flex scanner, 
    %% terminating after a code change...
    {noreply, S};

handle_info({'EXIT', Id, Error}, S) ->
    warning_msg("received unexpected 'EXIT' signal from ~p:"
		"~n~w", [Id, Error]),
    {noreply, S};

handle_info(Info, S) ->
    warning_msg("received unexpected info: "
		"~n~w", [Info]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _S) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Called to change the internal state
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change({down, _Vsn}, #state{conf = Conf} = State, downgrade_to_pre_3_3) ->
    Port = update_flex_scanner(Conf),
    {ok, State#state{conf = {flex, Port}}};

code_change(_Vsn, #state{conf = Conf} = State, upgrade_from_pre_3_3) ->
    Port = update_flex_scanner(Conf),
    {ok, State#state{conf = {flex, Port}}};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

update_flex_scanner({flex, Port}) ->
    megaco_flex_scanner:stop(Port),
    case megaco_flex_scanner:start() of
	{ok, Port1} ->
	    Port1;
	Error ->
	    exit(Error)
    end;
update_flex_scanner(Error) ->
    exit({invalid_config, Error}).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

warning_msg(F, A) ->
    ?megaco_warning("Flex scanner handler: " ++ F, A).

error_msg(F, A) ->
    ?megaco_error("Flex scanner handler: " ++ F, A).



% d(F, A) ->
%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).
	      
