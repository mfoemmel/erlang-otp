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
-module(erl_compile).

-include("erl_compile.hrl").
-include("file.hrl").

%% Purpose: Consistent interface to various compilers.

-export([compile_cmdline/1, remote_compile/1]).

%% Internal exports.
-export([capture_init/0, compiler_runner/1]).

%% Here is the mapping from file extensions to the module and function
%% that should handle it.

suffix_rules() ->
    [{".S", compile, compile},
     {".erl", compile, compile},
     {".mib", snmp, compile},
     {".bin", snmp_mib_to_hrl, compile},
     {".yrl", yecc, compile},
     {".xrl", leex, compile},
     {".script", systools, script2boot},
     {".rel", systools, compile_rel},
     {".h",ig,compile},
     {".idl",ic,compile},
     {".asn1", asn1ct, compile_asn1},
     {".asn", asn1ct, compile_asn},
     {".py", asn1ct, compile_py}].

%% Entry point from remote C node.

remote_compile(List) ->
    capture_output(),
    Result = compile(List),
    Output = lists:flatten(get_captured_output()),
    {Result, Output}.

%% Entry from command line.

compile_cmdline(List) ->
    Code = case compile(List) of
	       ok -> 0;
	       error -> 1;
	       _ -> 2
	   end,
    halt(Code).

%% Run the the compiler in a separate process, trapping EXITs.

compile(List) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, compiler_runner, [List]),
    receive
	{'EXIT', Pid, {compiler_result, Result}} ->
	    Result;
	{'EXIT', Pid, Reason} ->
	    io:format("Runtime error: ~p~n", [Reason]),
	    error
    end.
	    
compiler_runner(List) ->
    exit({compiler_result, compile1(List)}).


%% Parses the first part of the option list.

compile1(['@cwd', Cwd|Rest]) ->
    compile1(Rest, Cwd, #options {outdir=atom_to_list(Cwd), cwd=Cwd});
compile1(Other) ->
    throw({error, {bad_input, Other}}).

%% Parses all options.

compile1(['@i', Dir|Rest], Cwd, Opts) ->
    AbsDir = filename:absname(Dir, Cwd),
    compile1(Rest, Cwd, Opts#options{includes=[AbsDir|Opts#options.includes]});
compile1(['@ilroot', Ilroot|Rest], Cwd, Opts) ->
    AbsDir = filename:absname(Ilroot, Cwd),
    compile1(Rest, Cwd, Opts#options{ilroot=Ilroot});
compile1(['@outdir', Dir|Rest], Cwd, Opts) ->
    AbsName = filename:absname(Dir, Cwd),
    case file_or_directory(AbsName) of
	file ->
	    compile1(Rest, Cwd, Opts#options{outfile=AbsName});
	directory ->
	    compile1(Rest, Cwd, Opts#options{outdir=AbsName})
    end;
compile1(['@d', Name|Rest], Cwd, Opts) ->
    Defines = Opts#options.defines,
    compile1(Rest, Cwd, Opts#options{defines=[Name|Defines]});
compile1(['@dv', Name, Term|Rest], Cwd, Opts) ->
    Defines = Opts#options.defines,
    Value = make_term(atom_to_list(Term)),
    compile1(Rest, Cwd, Opts#options{defines=[{Name, Value}|Defines]});
compile1(['@warn', Level|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{warning=Level});
compile1(['@verbose', false|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{verbose=false});
compile1(['@verbose', true|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{verbose=true});
compile1(['@optimize', Atom|Rest], Cwd, Opts) ->
    Term = make_term(atom_to_list(Atom)),
    compile1(Rest, Cwd, Opts#options{optimize=Term});
compile1(['@option', Atom|Rest], Cwd, Opts) ->
    Term = make_term(atom_to_list(Atom)),
    Specific = Opts#options.specific,
    compile1(Rest, Cwd, Opts#options{specific=[Term|Specific]});
compile1(['@output_type', OutputType|Rest], Cwd, Opts) ->
    compile1(Rest, Cwd, Opts#options{output_type=OutputType});
compile1(['@files'|Rest], Cwd, Opts) ->
    Includes = lists:reverse(Opts#options.includes),
    compile2(Rest, Cwd, Opts#options{includes=Includes}).

compile2(Files, Cwd, Opts) ->
    case {Opts#options.outfile, length(Files)} of
	{"", _} ->
	    compile3(Files, Cwd, Opts);
	{[C|_], 1} ->
	    compile3(Files, Cwd, Opts);
	{[C|_], N} ->
	    io:format("Output file name given, but more than one input file.~n"),
	    error
    end.

%% Compiles the list of files, until done or compilation fails.

compile3([File|Rest], Cwd, Options) ->
    Ext = filename:extension(File),
    Root = filename:rootname(File),
    InFile = filename:absname(Root, Cwd),
    OutFile =
	case Options#options.outfile of
	    "" ->
		filename:join(Options#options.outdir, filename:basename(Root));
	    Outfile ->
		filename:rootname(Outfile)
	end,
    case compile_file(Ext, InFile, OutFile, Options) of
	ok ->
	    compile3(Rest, Cwd, Options);
	Other ->
	    Other
    end;
compile3([], _Cwd, _Options) ->
    ok.


%% Invokes the appropriate compiler, depending on the file extension.

compile_file("", Input, Output, Options) ->
    io:format("File has no extension: ~s~n", [Input]),
    error;
compile_file(Ext, Input, Output, Options) ->
    compile_file1(suffix_rules(), Ext, [Input, Output, Options]).

compile_file1([{Ext, Mod, Func}|Rest], Ext, Args) ->
    case catch apply(Mod, Func, Args) of
	{'EXIT', Reason} ->
	    io:format("Compiler function ~p:~p/~p failed:\n~p~n",
		      [Mod, Func, length(Args), Reason]),
	    error;
	ok ->
	    ok;
	error ->
	    error;
	Other ->
	    io:format("Compiler function ~p:~p/~p returned:\n~p~n",
		      [Mod, Func, length(Args), Other]),
	    error
    end;
compile_file1([_|Rest], Ext, Args) ->
    compile_file1(Rest, Ext, Args);
compile_file1([], Ext, _Args) ->
    io:format("Extension '~s' is not known.\n", [Ext]),
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
		Other -> file
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
    

%%% Capture standard output.

%% Start to collect the standard output of the current process.

capture_output() ->
    Leader = spawn_link(?MODULE, capture_init, []),
    group_leader(Leader, self()).

%% Gets the collected output and terminates the sink process.

get_captured_output() ->
    group_leader() ! {get_buffer, self()},
    receive
	{buffer, Buffer} ->
	    Buffer
    end.

capture_init() ->
    capture_loop([]).

capture_loop(Buf0) ->
    receive
	{get_buffer, From} when pid(From) ->
	    From ! {buffer, Buf0};
	{io_request, From, ReplyAs, Request} when pid(From) ->
	    capture_loop(io_request(Request, From, ReplyAs, Buf0));
	Other ->				% Ignore other messages
	    capture_loop(Buf0)
    end.

%% io_request(Request, Sender, ReplyAs, Buffer)
%%  Returns the new buffer.

io_request(Req, From, ReplyAs, Buf0) ->
    case io_request(Req, Buf0) of
	{Status, Reply, Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit, normal} ->
	    io_reply(From, ReplyAs, ok),
	    exit(normal);
	{exit, R} ->
	    io_reply(From, ReplyAs, {error,R}),
	    exit(normal)
    end.

io_request({put_chars, Chars}, Buf0) ->
    {ok, ok, [Buf0|Chars]};

io_request({put_chars, Mod, Func, Args}, Buf) ->
    case catch apply(Mod, Func, Args) of
	Chars when list(Chars) ->
	    io_request({put_chars, Chars}, Buf);
	Other ->
	    {error, {error, Func}, Buf}
    end;
io_request({requests,Reqs},  Buf) ->
    io_requests(Reqs, {ok,ok,Buf});
io_request(R, Buf) ->	                	% Unknown request
    {ok, {error, {request,R}}, Buf}.		% Ignore, but give error

%% Status = io_requests(RequestList, PrevStat, Descriptor, Server)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,Res,Buf}) ->
    io_requests(Rs, io_request(R, Buf));
io_requests([_|_], Error) ->
    Error;
io_requests([], Stat) ->
    Stat.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.
