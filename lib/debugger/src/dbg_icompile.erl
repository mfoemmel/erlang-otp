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
%%           Rewritten from compile.erl to suit the interpreter.
%% Purpose : Compiler driver for interpreter.

-module(dbg_icompile).

%% High-level interface.
-export([file/1,file/2,format_error/1]).

%% Internal functions.
-export([internal/3]).

-import(lists, [member/2,flatten/1,keysearch/3]).

%% file(FileName)
%% file(FileName, Options)
%%  Compile the module in file FileName.

file(File) ->
    file(File, [verbose,report_errors,report_warnings]).

file(File, Opts0) when list(Opts0) ->
    Opts1 = lists:foldr(fun expand_opt/2, [], Opts0),
    Serv = spawn_link(?MODULE, internal, [self(),File,Opts1]),
    receive
	{Serv,Rep} -> Rep
    end.

expand_opt(report, Os) -> [report_errors,report_warnings|Os];
expand_opt(return, Os) -> [return_errors,return_warnings|Os];
expand_opt(O, Os) -> [O|Os].

%% format_error(ErrorDescriptor) -> string()

format_error({open,E}) ->
    io_lib:format("open error '~s'", [E]);
format_error(write_error) ->
    "error writing file";
format_error({rename,S}) ->
    io_lib:format("error renaming ~s", [S]);
format_error({parse_transform,M}) ->
    io_lib:format("error in transform '~s'", [M]).

%% The compile state record.
-record(compile, {filename="",
		  dir="",
		  base="",
		  ifile="",
		  ofile="",
		  module=[],
		  code=[],
		  defs=[],
		  options=[],
		  errors=[],
		  warnings=[]
		 }).

%% internal(Master, FileName, [Option]) ->
%%	<>

internal(Master, File, Opts) ->
    {Dir,Base} = iofile(File),
    St0 = #compile{filename=File, dir=Dir, base=Base,
		  ifile=erlfile(Dir, Base),
		  ofile=objfile(Base, Opts),
		  options=Opts},
    Passes = [fun parse_module/1,fun transform_module/1,fun lint_module/1,
	      fun expand_module/1, fun interpret_module/1],
    R = case fold_comp(St0, Passes) of
	    {ok,St1} ->
                comp_ret_bin(St1);
	    {break,St1} ->
		comp_ret_ok(St1);
	    {error,St1} ->
		comp_ret_err(St1)
	end,
    Master ! {self(),R}.

fold_comp(St0, [P|Ps]) ->
    case P(St0) of
	{ok,St1} -> fold_comp(St1, Ps);
	{break,St1} -> {break,St1};
	{error,St1} -> {error,St1}
    end;
fold_comp(St, []) -> {ok,St}.

parse_module(St) ->
    Opts = St#compile.options,
    case parse_file(St#compile.ifile,
		    [".",St#compile.dir|inc_paths(Opts)],
		    pre_defs(Opts)) of
	{ok,Forms,Defs} ->
	    {ok,St#compile{code=Forms, defs=Defs}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,compile,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

parse_file(Ifile, Path, Predefs) ->
    case epp:open(Ifile, Path, Predefs) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    Defs = epp:macro_defs(Epp),
	    epp:close(Epp),
	    {ok,Forms,Defs};
	{error,E} ->
	    {error,E}
    end.

parse_file(Epp) ->
    case epp:parse_erl_form(Epp) of
	{ok,Form} ->
	    [Form|parse_file(Epp)];
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{eof,Line} ->
	    [{eof,Line}]
    end.

compile_options([{attribute,L,compile,C}|Fs]) when list(C) ->
    C ++ compile_options(Fs);
compile_options([{attribute,L,compile,C}|Fs]) ->
    [C|compile_options(Fs)];
compile_options([_F|Fs]) -> compile_options(Fs);
compile_options([]) -> [].

transforms(Os) -> [ M || {parse_transform,M} <- Os ]. 

transform_module(St) ->
    %% Extract compile options from code into options field.
    Ts = transforms(St#compile.options ++ compile_options(St#compile.code)),
    foldl_transform(St, Ts).

foldl_transform(St, [T|Ts]) ->
    case catch apply(T, parse_transform, [St#compile.code, St#compile.options]) of
	Forms ->
	    foldl_transform(St#compile{code=Forms}, Ts);
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{parse_transform,T}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end;
foldl_transform(St, []) ->
    {ok, St}.

lint_module(St) ->
    case erl_lint:module(St#compile.code,
			 St#compile.ifile, St#compile.options) of
	{ok,Ws} ->
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

%% expand_module(State) -> State'
%%  Do the common preprocessing of the input forms.

expand_module(St0) ->
    {Mod,Exp,Forms,Opts} = sys_pre_expand:module(St0#compile.code,
						 St0#compile.options),
    {ok,St0#compile{module=Mod,options=Opts,code={Mod,Exp,Forms}}}.

interpret_module(St) ->
    case dbg_iasm:module(St#compile.code, St#compile.defs, St#compile.ifile) of
	{ok,Bin} ->
	    {ok,St#compile{code=Bin}};
	{error,Es} ->
	    {error,St#compile{errors=St#compile.errors ++
			      [{St#compile.ifile,Es}]}}
    end.

%% comp_ret_ok(ModuleName, State) -> OkReturn
%% comp_ret_bin(ModuleName, Binary, State) -> OkBinReturn
%% comp_ret_err(State) -> ErrorReturn

comp_ret_ok(St) ->
    report_warnings(St),
    case member(return_warnings, St#compile.options) of
	true -> {ok,St#compile.module,St#compile.warnings};
	false -> {ok,St#compile.module}
    end.

comp_ret_bin(St) ->
    report_warnings(St),
    case member(return_warnings, St#compile.options) of
	true ->
	    {ok,St#compile.module,St#compile.ifile,
	     St#compile.code,St#compile.warnings};
	false ->
	    {ok,St#compile.module,St#compile.ifile,St#compile.code}
    end.

comp_ret_err(St) ->
    report_errors(St),
    report_warnings(St),
    case member(return_errors, St#compile.options) of
	true -> {error,St#compile.errors,St#compile.warnings};
	false -> error
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(St) ->
    case member(report_errors, St#compile.options) of
	true ->
	    lists:foreach(fun ({{F,L},Eds}) -> list_errors(F, Eds);
			      ({F,Eds}) -> list_errors(F, Eds) end,
			  St#compile.errors);
	false -> ok
    end.

report_warnings(St) ->
    case member(report_warnings, St#compile.options) of
	true ->
	    lists:foreach(fun ({{F,L},Eds}) -> list_warnings(F, Eds);
			      ({F,Eds}) -> list_warnings(F, Eds) end,
			  St#compile.warnings);
	false -> ok
    end.

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~s:~w: ~s\n", [F,Line,apply(Mod, format_error, [E])]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:fwrite("~s: ~s\n", [F,apply(Mod, format_error, [E])]),
    list_errors(F, Es);
list_errors(F, []) ->
    ok.

%% list_warnings(File, ErrorDescriptors) -> ok

list_warnings(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~s:~w: Warning: ~s\n", [F,Line,apply(Mod, format_error, [E])]),
    list_warnings(F, Es);
list_warnings(F, [{Mod,E}|Es]) ->
    io:fwrite("~s: Warning: ~s\n", [F,apply(Mod, format_error, [E])]),
    list_warnings(F, Es);
list_warnings(F, []) ->
    ok.

%% iofile(File) -> {Dir,Base}
%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Options) -> ObjFile
%%  Work out the correct input and output file names.

iofile(File) when atom(File) ->
    iofile(atom_to_list(File));
iofile(File) ->
    case string:rchr(File, $/) of
	0 -> {".",File};
	N -> {string:substr(File, 1, N-1),string:substr(File, N+1)}
    end.

erlfile(Dir, Base) ->
    flatten([Dir,"/",Base,".erl"]).

outfile(Base, Ext, Opts) ->
    Obase = case keysearch(outdir, 1, Opts) of
		{value,{outdir,Odir}} -> [Odir,"/",Base];
		Other ->			%Not found or bad format
		    Base
	    end,
    flatten([Obase,Ext]).

objfile(Base, Opts) ->
    outfile(Base, code:objfile_extension(), Opts).

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([O|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, list(P) ].

