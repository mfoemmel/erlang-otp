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
%% Author: Lennart Öhman, lennart.ohman@st.se
%%
-module(inviso_autostart_server).
-export([init/1]).

%% This module provides a (well working) example of how to program an
%% autostart server responsible for initializing trace, setting patterns
%% and flags.
%%
%% The general idea is that this code opens one or several files containing
%% erlang function calls which are evaluated in this process context.
%% The argument provided to init shall be a list of options controlling
%% how to initialize tracing, which file(s) to open and variable bindings.
%%
%% This autostart_server understands standard inviso trace case files.
%% The runtime component provides an API very similar to the API provided
%% by the control component. This program translates any control component
%% API calls (as could possibly be found in a trace case file) to corresponding
%% runtime component API calls.
%% It performs the trick by simply removing the parameters from the calls
%% before making the call, and of course changes the module name in the call.
%% This also means that variables concerning control components does not have
%% to be bound in this environment.
%% =============================================================================


%% -----------------------------------------------------------------------------

%% The independent autostart process spawned by the runtime component to carry
%% out initializations is spawened on this function (if using the example
%% autostart which comes with inviso).
%% ArgsFromConfig is as can be heard from the name comming from a paramater in
%% the autostart configuration file. Here it is supposed to be:
%%   ArgsFromConfig=[ServerParam,...]
%%     ServerParam={tracerdata,TracerData}|{cmdfiles,Files}|{bindings,Bindings}|
%%       {translations,Translations}|{debug,DbgLevel}
%%       TracerData=tracerdata given to inviso_rt:init_tracing/1 function.
%%       Files=[FileName,...] files with trace cases, which will be carried out
%%         in the order specified.
%%       Bindings=[{Var,Value},...] variable environment understood by
%%         erl_eval:exprs/2.
%%       Translations=[Translation,...]
%%       A translation file is a text-file with following tuples
%%         Translation={{Mod,Func,Arity,{Mod2,Func2,ParamMF}}}|
%%                     {{Func,Arity,{Mod2,Func2,ParamMF}}}
%%           ParamMF={M,F} | any()
%%           Translates Mod:Func/Arity to Mod2:Func2 with the arguments to
%%             Mod:Func translated using M:F/1. Note that ParamMF is not
%%             necessarily an MF. If no translation shall be done, ParamFun
%%             shall be anything else but an MF.
%%           Also note that Mod is optional in a Translation. That means that
%%           function calls without a module in the trace case file will
%%           be translated according to that translation.
init(ArgsFromConfig) ->
    case get_tracerdata_opts(ArgsFromConfig) of
	{ok,TracerData} ->                  % Otherwise we can not start a trace!
	    case inviso_rt:init_tracing(TracerData) of
		{ok,_} ->                   % Ok, tracing has been initiated.
		    case get_cmdfiles_opts(ArgsFromConfig) of
			{ok,CmdFiles} ->    % List of cmd-files.
			    Bindings=get_initialbindings_opts(ArgsFromConfig),
			    Translations=get_translations_opts(ArgsFromConfig),
			    Dbg=get_dbg_opts(ArgsFromConfig),
			    interpret_cmd_files(CmdFiles,
						Bindings,
						Translations,
						Dbg);
			false ->            % Then we can terminate normally.
			    true
		    end;
		{error,Reason} ->           % This is fault, lets terminate abnormally.
		    exit({inviso,{error,Reason}})
	    end;
	false ->                            % Then there is not much use then.
	    true                            % Just terminate normally.
    end.
%% -----------------------------------------------------------------------------

interpret_cmd_files([FileName|Rest],Bindings,Translations,Dbg) ->
    case file:open(FileName,[read]) of
	{ok,FD} ->
	    interpret_cmd_files_2(FD,Bindings,io:parse_erl_exprs(FD,""),Translations,Dbg),
	    file:close(FD),
	    interpret_cmd_files(Rest,Bindings,Translations,Dbg); % Yes, the original bindings!
	{error,Reason} ->                   % Something wrong with the file.
	    inviso_rt_lib:debug(Dbg,interpret_cmd_files,[FileName,{error,Reason}]),
	    interpret_cmd_files(Rest,Bindings,Translations,Dbg) % Yes, the original bindings!
    end;
interpret_cmd_files([],_,_,_) ->            % Done, return nothing significant!
    true.

%% Help function which handles Exprs returned from io:parse_erl_exprs and
%% tries to eval them. It is the side-effects we are interested in, like
%% setting flags and patterns. Note that we will get a failure should there
%% be a variable conflict.
%% Also note that there is logic to translate control component API calls to
%% corresponding runtime component calls.
%% Returns nothing significant.
interpret_cmd_files_2(FD,Bindings,{ok,Exprs,_},Translations,Dbg) ->
    case catch inviso_rt_lib:transform(Exprs,Translations) of
	NewExprs when list(NewExprs) ->     % We may have translated the API.
	    case catch erl_eval:exprs(NewExprs,Bindings) of
		{'EXIT',Reason} ->
		    inviso_rt_lib:debug(Dbg,exprs,[Exprs,Bindings,{'EXIT',Reason}]),
		    interpret_cmd_files_2(FD,
					  Bindings,
					  io:parse_erl_exprs(FD,""),
					  Translations,
					  Dbg);
		{value,_Val,NewBindings} -> % Only interested in the side effects!
		    interpret_cmd_files_2(FD,
					  NewBindings,
					  io:parse_erl_exprs(FD,""),
					  Translations,
					  Dbg)
	    end;
	{'EXIT',Reason} ->
	    inviso_rt_lib:debug(Dbg,translate2runtime_funcs,[Exprs,Reason]),
	    interpret_cmd_files_2(FD,Bindings,io:parse_erl_exprs(FD,""),Translations,Dbg)
    end;
interpret_cmd_files_2(FD,Bindings,{error,ErrorInfo,Line},Translations,Dbg) ->
    inviso_rt_lib:debug(Dbg,parse_erl_exprs,[ErrorInfo,Line]),
    interpret_cmd_files_2(FD,Bindings,io:parse_erl_exprs(FD,""),Translations,Dbg);
interpret_cmd_files_2(_,_,{eof,_},_,_) ->    % End of file.
    true.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Help functions working on the options given as argument to init during spawn.
%% -----------------------------------------------------------------------------

get_tracerdata_opts(ArgsFromConfig) ->
    case lists:keysearch(tracerdata,1,ArgsFromConfig) of
	{value,{_,TracerData}} ->
	    {ok,TracerData};
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_cmdfiles_opts(ArgsFromConfig) ->
    case lists:keysearch(cmdfiles,1,ArgsFromConfig) of
	{value,{_,CmdFiles}} ->
	    {ok,CmdFiles};
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_initialbindings_opts(ArgsFromConfig) ->
    case lists:keysearch(bindings,1,ArgsFromConfig) of
	{value,{_,Bindings}} ->
	    Bindings;
	false ->                            % Then we use empty bindings.
	    erl_eval:new_bindings()
    end.
%% -----------------------------------------------------------------------------

get_translations_opts(ArgsFromConfig) ->
    case lists:keysearch(translations,1,ArgsFromConfig) of
	{value,{_,Translations}} ->
	    Translations;
	false ->                            % This becomes nearly point less.
	    []
    end.
%% -----------------------------------------------------------------------------

get_dbg_opts(ArgsFromConfig) ->
    case lists:keysearch(debug,1,ArgsFromConfig) of
	{value,{_,DbgLevel}} ->
	    DbgLevel;
	false ->
	    off
    end.
%% -----------------------------------------------------------------------------

%% EOF



