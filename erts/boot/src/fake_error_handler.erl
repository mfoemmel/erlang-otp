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

%% A simple error handler.

-export([undefined_function/3, undefined_lambda/3, stub_function/3,
	 breakpoint/3, crash/1]).

undefined_function(Module, Func, Args) ->
    %% erlang:display({error_handler_undef_function,Module,Func,Args}),
    case ensure_loaded(Module) of
	{module, Module} ->
	    %% erlang:display(checking_exported),
	    case erlang:function_exported(Module, Func, length(Args)) of
		true ->   
		    %% erlang:display(apply),
		    apply(Module, Func, Args);
		false ->
		   %%   erlang:display({module,Module,doesnot_export,
		   %%			    Func, arity, length(Args)}),
		    crash(Module, Func, Args)
	    end;
	{module, Other} ->
	    %% erlang:display({error_handler_here1,Other}),
	    crash(Module, Func, Args);
	Other ->
	    %% erlang:display({error_handler_here2,Other}),
	    crash(Module, Func, Args)
    end.

undefined_lambda(Module, Fun, Args) ->
    case ensure_loaded(Module) of
	{module, Module} ->
	    %% There is no need (and no way) to test if the fun is present.
	    %% apply/2 will not call us again if the fun is missing.
	    apply(Fun, Args);
	{module, Other} ->
	    crash(Fun, Args);
	Other ->
	    crash(Fun, Args)
    end.

breakpoint(Module, Func, Args) ->
    int:eval(Module, Func, Args).

%%
%% Crash providing a beautiful stack backtrace.
%%
crash(Fun, Args) ->
    crash({Fun,Args}).
crash(M, F, A) ->
    crash({M,F,A}).
crash(MFA) ->
    {'EXIT',{undef,[Current|Mfas]}} = (catch erlang:fault(undef)),
    exit({undef,[MFA|Mfas]}).
%%    erlang:display({crash,MFA}),
%%    erlang:halt().

%% If the code_server has not been started yet dynamic code loading
%% is handled by init.

ensure_loaded(Module) ->
    %% erlang:display({fake_error_handler, ensure_loaded, Module}),
    Self = self(),
    case whereis(code_server) of
	%% Perhaps double fault should be detected in code:ensure_loaded/1 
	%% instead, since this error handler cannot know whether the 
	%% code server can resolve the problem or not.
	%% An {error, Reason} return from there would crash the code server and 
	%% bring down the node.
	Self ->
	    Error = "The code server called the unloaded module `" ++
		atom_to_list(Module) ++ "'",
	    halt(Error);
	Pid when pid(Pid) ->
	    boot_code_loader:ensure_loaded(Module);
	_ ->
	    init:ensure_loaded(Module)
    end.

stub_function(Mod, Func, Args) ->
    exit({undef,[{Mod,Func,Args}]}).
