%%%----------------------------------------------------------------------
%%% File    : webtool_sup.erl
%%% Author  : Martin G. <marting@erix.ericsson.se>
%%% Purpose : 
%%% Created : 24 Apr 2001 by Martin G. <marting@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(webtool_sup).
-author('marting@erix.ericsson.se').

-behaviour(supervisor).

%% External exports
-export([start_link/0,stop/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local,websup},webtool_sup, []).

stop(Pid)->
   exit(Pid,normal).
%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(_StartArgs) ->
    %%Child1 = 
    %%Child2 ={webcover_backend,{webcover_backend,start_link,[]},permanent,2000,worker,[webcover_backend]},
    %%{ok,{{simple_one_for_one,5,10},[Child1]}}.
    {ok,{{one_for_one,100,10},[]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------



















