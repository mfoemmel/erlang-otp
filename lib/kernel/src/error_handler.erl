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
-module(error_handler).

%% A simple error handler

-export([undefined_function/3, undefined_lambda/3, stub_function/3]).

undefined_function(Module, Func, Args) ->
    case ensure_loaded(Module) of
	{module, Module} ->
	    case erlang:function_exported(Module, Func, length(Args)) of
		true ->
		    apply(Module, Func, Args);
		false ->
		    crash(Module, Func, Args)
	    end;
	{module, Other} ->
	    crash(Module, Func, Args);
	{interpret, Module} ->
	    int:eval(Module, Func, Args);
	Other ->
	    crash(Module, Func, Args)
    end.

%%
%% This function will currently only be called in Beam, where funs are
%% first-class citizens.
%%
undefined_lambda(Module, Fun, Args) ->
    case ensure_loaded(Module) of
	{module, Module} ->
	    %% There is no need (and no way) to test if the fun is present.
	    %% apply/2 will not call us again if the fun is missing.
	    apply(Fun, Args);
	{module, Other} ->
	    crash(Fun, Args);
	{interpret, Module} ->
	    int:eval(Module, Fun, Args);
	Other ->
	    crash(Fun, Args)
    end.

%%
%% Crash providing a beatiful stack back trace.
%%
crash(Fun, Args) ->
    crash({Fun,Args}).
crash(M, F, A) ->
    crash({M,F,A}).
crash(MFA) ->
    {'EXIT',{undef,[Current|Mfas]}} = (catch erlang:fault(undef)),
    exit({undef,[MFA|Mfas]}).

%% If the code_server has not been started yet dynamic code loading
%% is handled by init.
ensure_loaded(Module) ->
    case whereis(code_server) of
	Pid when pid(Pid) ->
	    code:ensure_loaded(Module);
	_ ->
	    init:ensure_loaded(Module)
    end.

stub_function(Mod, Func, Args) ->
    exit({undef,[{Mod,Func,Args}]}).
