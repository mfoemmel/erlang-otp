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
%% Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Purpose: stand-alone Erlang compiler
%% Usage 
%%   ecc F1.erl F2.erl ....
%%       Compiler F1.erl F2.erl etc.
%% Ecc will use its own code loader
%%   which demand loads code from the .ear
%%   and a hacked error_handler

-module(ecc).

-include("erl_compile.hrl").
-include_lib("kernel/include/file.hrl").

-export([compile_cmdline/1]).
%% Internal exports.
-export([compiler_runner/1]).


-export([start/1]).
-import(lists, [map/2]).
	
start([_,_|T]) ->       
    X = map(fun(I) -> binary_to_list(I) end, T),
    %% erlang:display({ecc,X}),
    compile_cmdline(X),
    erlang:halt().

%% derived from erl_compile 
%%   when ecc_compile works we can throw away erl_compile


%% Entry from command line.

compile_cmdline(List) ->
    %% I experimented using init:stop/0 instead of halt/0,
    %% to make sure we don't lose output, but it turned out
    %% to be too slow for practial use.
    case compile(List) of
	ok -> halt();
	error -> halt(1);
	_ -> halt(2)
    end.

%% Run the the compiler in a separate process, trapping EXITs.

compile(List) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, compiler_runner, [List]),
    open_port({fd,0,1}, [out]),			%Turn off stdin.
    receive
	{'EXIT', Pid, {compiler_result, Result}} ->
	    Result;
	{'EXIT', Pid, Reason} ->
	    io:format("Runtime error: ~p~n", [Reason]),
	    error
    end.
	    
compiler_runner(List) ->
    exit({compiler_result, compile1(List)}).

%% Main entry point
%% Parses the first part of the option list.

compile1(List) ->
    {ok, Cwd} = file:get_cwd(),
    compile1(List, #options {outdir=Cwd, cwd=Cwd, 
				  verbose=false, warning=0}).

%% Parses all options.

compile1(["-I", Dir|Rest], Opts) ->
    compile1(Rest, Opts#options{includes=[Dir|Opts#options.includes]});
compile1(["-I" ++ Dir|Rest], Opts) ->
    compile1(["-I", Dir|Rest], Opts);
compile1(["-o", Dir|Rest], Opts) ->
    case file_or_directory(Dir) of
	file ->
	    compile1(Rest, Opts#options{outfile=Dir});
	directory ->
	    compile1(Rest, Opts#options{outdir=Dir})
    end;
compile1(["-o"++Dir|Rest], Opts) ->
    compile1(["-o",Dir|Rest], Opts);
compile1(["-D" ++ Name0|Rest], Opts) ->
    case lists:member($=, Name0) of
	true ->
	    [Name1,Term]=string:tokens(Name0, "="),
	    Name = list_to_atom(Name1),
	    Defines = Opts#options.defines,
	    Value = make_term(Term),
	    compile1(Rest, Opts#options{defines=[{Name, Value}|Defines]});
	false ->
	    Name = list_to_atom(Name0),
	    Defines = Opts#options.defines,
	    compile1(Rest, Opts#options{defines=[Name|Defines]})
    end;
compile1(["-W"|Rest], Opts) ->
    compile1(Rest, Opts#options{warning=1});
compile1(["-E"|Rest], #options{specific=Specific}=Opts) ->
    compile1(Rest, Opts#options{specific=['E'|Specific]});
compile1(["-P"|Rest], #options{specific=Specific}=Opts) ->
    compile1(Rest, Opts#options{specific=['P'|Specific]});
compile1(["-S"|Rest], #options{specific=Specific}=Opts) ->
    compile1(Rest, Opts#options{specific=['S'|Specific]});
compile1(["-v"|Rest], Opts) ->
    compile1(Rest, Opts#options{verbose=true});
compile1(["-O" ++ Str|Rest], Opts) ->
    Term = make_term(Str),
    compile1(Rest, Opts#options{optimize=Term});
compile1(["-b" ++ Type|Rest], Opts) ->
    compile1(Rest, Opts#options{output_type=list_to_atom(Type)});
compile1(["-" ++ Str|Rest], Opts) ->
    io:format("Ignoring unknown option: -~s\n", [Str]),
    compile1(Rest, Opts);
compile1(["+" ++ Str|Rest], Opts) ->
    Term = make_term(Str),
    Specific = Opts#options.specific,
    compile1(Rest, Opts#options{specific=[Term|Specific]});
compile1(Rest, Opts) ->
    %% Rest should all be files with (.erl) extension
    Includes = lists:reverse(Opts#options.includes),
    compile2(Rest, Opts#options{includes=Includes}).

compile2(Files, Opts) ->
    case {Opts#options.outfile,length(Files)} of
	{"", _} ->
	    compile3(Files, Opts);
	{[_|_], 1} ->
	    compile3(Files, Opts);
	{[_|_], _} ->
	    io:format("Output file name given, but more than one input file.~n"),
	    error
    end.

%% Compiles the list of files, until done or compilation fails.

compile3([File|Rest], Options) ->
    Ext = filename:extension(File),
    InFile = Root = filename:rootname(File),
    OutFile =
	case Options#options.outfile of
	    "" ->
		filename:join(Options#options.outdir, filename:basename(Root));
	    Outfile ->
		filename:rootname(Outfile)
	end,
    case compile_file(Ext, InFile, OutFile, Options) of
	ok -> compile3(Rest, Options);
	Other -> Other
    end;
compile3([], _) -> ok.

%% Invokes the appropriate compiler, depending on the file extension.

compile_file(".erl", Input, Output, Options) ->
    case catch compile:compile(Input, Output, Options) of
	ok -> ok;
	error -> error;
	{'EXIT',Reason} ->
	    io:format("Compiler function ~w:~w/3 failed:\n~p~n",
		      [compile,compile,Reason]),
	    error;
	Other ->
	    io:format("Compiler function ~w:~w/3 returned:\n~p~n",
		      [compile,compile,Other]),
	    error
    end;
compile_file(_, Input, _, _) ->
    io:format("File has wrong extension: ~s~n", [Input]),
    error.

%% Guesses if a give name refers to a file or a directory.

file_or_directory(Name) ->
    case file:read_file_info(Name) of
	{ok, #file_info{type=regular}} ->
	    file;
	{ok, _} ->
	    directory;
	{error, _} ->
	    case filename:extension(Name) of
		[] -> directory;
		_Other -> file
	    end
    end.

%% Makes an Erlang term given a string.

make_term(Str) -> 
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->		  
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error, {_,_,Reason}} ->
		    io:format("~s: ~s~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format("~s: ~s~n", [Reason, Str]),
	    throw(error)
    end.


















