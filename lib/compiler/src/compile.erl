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
%% Purpose: Run the Erlang compiler.

-module(compile).
-include("erl_compile.hrl").

%% High-level interface.
-export([file/1,file/2,format_error/1,iofile/1,compile/3]).
-export([forms/1,forms/2]).
-export([output_generated/1]).
-export([options/0,options/1]).

%% Internal functions.
-export([internal/3]).


-import(lists, [member/2,reverse/1,keysearch/3,last/1,
		map/2,foreach/2,foldr/3,any/2,filter/2]).

-define(DEF_VERSION, v3).			%Default compiler version.

%% file(FileName)
%% file(FileName, Options)
%%  Compile the module in file FileName.

-define(DEFAULT_OPTIONS, [verbose,report_errors,report_warnings]).

-define(pass(P), {P,fun P/1}).

file(File) -> file(File, ?DEFAULT_OPTIONS).

file(File, Opts) when atom(Opts) ->
    file(File, [Opts|?DEFAULT_OPTIONS]);
file(File, Opts) when list(Opts) ->
    do_compile({file,File}, Opts++env_default_opts()).

forms(File) -> forms(File, ?DEFAULT_OPTIONS).

forms(Forms, Opts) when atom(Opts) ->
    forms(Forms, [Opts|?DEFAULT_OPTIONS]);
forms(Forms, Opts) when list(Opts) ->
    do_compile({forms,Forms}, [binary|Opts++env_default_opts()]).

env_default_opts() ->
    Key = "ERL_COMPILER_OPTIONS",
    case os:getenv(Key) of
	false -> [];
	Str when list(Str) ->
	    case erl_scan:string(Str) of
		{ok,Tokens,_} ->
		    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
			{ok,List} when list(List) -> List;
			{ok,Term} -> [Term];
			{error,Reason} ->
			    io:format("Ignoring bad term in ~s\n", [Key]),
			    []
		    end;
		{error, {_,_,Reason}, _} ->
		    io:format("Ignoring bad term in ~s\n", [Key]),
		    []
	    end
    end.
	    
do_compile(Input, Opts0) ->
    Opts = foldr(fun expand_opt/2, [], Opts0),
    Serv = spawn_link(?MODULE, internal, [self(),Input,Opts]),
    receive
	{Serv,Rep} -> Rep
    end.

%% Given a list of compilation options, returns true if compile:file/2
%% would have generated a Beam file, false otherwise (if only a binary or a
%% listing file would have been generated).

output_generated(Opts) ->
    any(fun ({save_binary,F}) -> true;
	    (Other) -> false
	end, passes(file, ?DEF_VERSION, Opts)).

expand_opt(report, Os) -> [report_errors,report_warnings|Os];
expand_opt(return, Os) -> [return_errors,return_warnings|Os];
expand_opt(r7, Os) -> [no_float_opt,no_new_funs];
expand_opt(O, Os) -> [O|Os].

%% format_error(ErrorDescriptor) -> string()

format_error(no_native_support) ->
    "this system is not configured for native-code compilation.";
format_error({native, E}) ->
    io_lib:fwrite("native-code compilation failed with reason: ~P.",
		  [E, 25]);
format_error({native_crasch, E}) ->
    io_lib:fwrite("native-code compilation crasched with reason: ~P.",
		  [E, 25]);
format_error(jam_is_dead) ->
    "JAM is dead!";
format_error(v1_is_dead) ->
    "The v1 compiler is no longer supported.";
format_error(v2_is_dead) ->
    "The v2 compiler is no longer supported.";
format_error({open,E}) ->
    io_lib:format("open error '~s'", [file:format_error(E)]);
format_error(write_error) ->
    "error writing file";
format_error({rename,S}) ->
    io_lib:format("error renaming ~s", [S]);
format_error({parse_transform,M,R}) ->
    io_lib:format("error in parse transform '~s': ~p", [M, R]);
format_error({core_transform,M,R}) ->
    io_lib:format("error in core transform '~s': ~p", [M, R]);
format_error({crash,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\ncrash reason: ~p", [Pass,Reason]);
format_error({bad_return,Pass,Reason}) ->
    io_lib:format("internal error in ~p;\nbad return value: ~p", [Pass,Reason]).

%% The compile state record.
-record(compile, {filename="",
		  dir="",
		  base="",
		  ifile="",
		  ofile="",
		  module=[],
		  code=[],
		  abstract_code=[],		%Abstract code for debugger.
		  options=[],
		  errors=[],
		  warnings=[]}).

internal(Master, Input, Opts) ->
    Master ! {self(),
	      case catch internal(Input, Opts) of
		  {'EXIT', Reason} ->
		      {error, Reason};
		  Other ->
		      Other
	      end}.

internal({forms,Forms}, Opts0) ->
    {Ver,Opts} = compiler_version(Opts0),
    Ps = passes(forms, Ver, Opts),
    internal_comp(Ps, "", "", #compile{code=Forms,options=Opts});
internal({file,File}, Opts0) ->
    {Ver,Opts} = compiler_version(Opts0),
    Ps = passes(file, Ver, Opts),
    case member(asm, Opts) of
	false -> internal_comp(Ps, File, ".erl", #compile{options=Opts});
	true -> internal_comp(Ps, File, ".S", #compile{options=Opts})
    end.

internal_comp(Passes, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir, base=Base,
		      ifile=erlfile(Dir, Base, Suffix),
		      ofile=objfile(Base, St0)},
    Run = case member(time, St1#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({Name,Fun}, St) -> catch Fun(St) end
	  end,
    case fold_comp(Passes, Run, St1) of
	{ok,St2} -> comp_ret_ok(St2);
	{error,St2} -> comp_ret_err(St2)
    end.

fold_comp([{Name,Pass}|Ps], Run, St0) ->
    case Run({Name,Pass}, St0) of
	{ok,St1} -> fold_comp(Ps, Run, St1);
	{error,St1} -> {error,St1};
	{'EXIT',Reason} ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{crash,Name,Reason}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}};
	Other ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{bad_return,Name,Other}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}}
    end;
fold_comp([], Run, St) -> {ok,St}.

os_process_size() ->
    case os:type() of
	{unix, sunos} ->
	    Size0 = os:cmd("ps -o vsz -p " ++ os:getpid() ++ " | tail -1"),
	    Size = list_to_integer(lib:nonl(Size0));
	_ ->
	    0
    end.	    

run_tc(NameFun, St) ->
    run_tc(NameFun, statistics(runtime), St).

run_tc({Name,Fun}, Before0, St) ->
    %% This division into two functions is a hack.  If we would had stack
    %% trimming, dead variables would have been removed from the stack.
    %% Well, anyway, the St variable will not be saved on the stack,
    %% because it is not referenced after the catch.

    Val = (catch Fun(St)),
    After0 = statistics(runtime),
    {Before_c, _} = Before0,
    {After_c, _} = After0,
    io:format(" ~-30s: ~10.3f s (~w k)\n",
	      [Name, (After_c-Before_c) / 1000, os_process_size()]),
    Val.

comp_ret_ok(St) ->
    report_warnings(St),
    Ret1 = case member(binary, St#compile.options) of
	       true -> [St#compile.code];
	       false -> []
	   end,
    Ret2 = case member(return_warnings, St#compile.options) of
	       true -> Ret1 ++ [St#compile.warnings];
	       false -> Ret1
	   end,
    list_to_tuple([ok,St#compile.module|Ret2]).

comp_ret_err(St) ->
    report_errors(St),
    report_warnings(St),
    case member(return_errors, St#compile.options) of
	true -> {error,St#compile.errors,St#compile.warnings};
	false -> error
    end.

%% passes(form|file, [Option]) -> [{Name,PassFun}]
%%  Figure out which passes that need to be run.

passes(forms, Ver, Opts) ->
    select_passes(standard_passes(Ver), Opts);
passes(file, Ver, Opts) ->
    Ps = case member(asm, Opts) of
	     true ->
		 [?pass(beam_consult_asm),?pass(beam_asm),
		  {unless,binary,?pass(save_binary)}];
	     false ->
		 [?pass(parse_module)|standard_passes(Ver)]
	 end,
    Fs = select_passes(Ps, Opts),
    
    %% If the last pass saves the resulting binary to a file,
    %% insert a first pass to remove the file.
    [?pass(error_if_jam)|
     case last(Fs) of
	 {save_binary,Fun} -> [?pass(remove_file)|Fs];
	 Other -> Fs
     end].

%% select_passes([Command], Opts) ->  [{Name,Function}]
%%  Interpret the lists of commands to return a pure list of passes.
%%
%%  Command can be one of:
%%
%%    {pass,Mod}	Will be expanded to a call to the external
%%			function Mod:module(Code, Options).  This
%%			function must transform the code and return
%%			{ok,NewCode} or {error,Term}.
%%			Example: {pass,beam_codegen}
%%
%%    {Name,Fun}	Name is an atom giving the name of the pass.
%%    			Fun is an 'fun' taking one argument: a compile record.
%%			The fun should return {ok,NewCompileRecord} or
%%			{error,NewCompileRecord}.
%%			Note: ?pass(Name) is equvivalent to {Name, fun Name/1}.
%%			Example: ?pass(parse_module)
%%
%%    {src_listing,Ext}	Produces an Erlang source listing with the
%%			the file extension Ext.  (Ext should not contain
%%			a period.)  No more passes will be run.
%%
%%    {listing,Ext}	Produce an listing of the terms in the internal
%%			representation.  The extension of the listing
%%			file will be Ext.  (Ext should not contain
%%			a period.)   No more passes will be run.
%%
%%    {iff,Flag,Cmd}	If the given Flag is given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {iff,dcg,{listing,"codegen}}
%%
%%    {unless,Flag,Cmd}	If the given Flag is NOT given in the option list,
%%			Cmd will be interpreted as a command.
%%			Otherwise, Cmd will be ignored.
%%			Example: {unless,no_kernopt,{pass,sys_kernopt}}
%%

select_passes([{pass,Mod}|Ps], Opts) ->
    F = fun(St) ->
		case catch Mod:module(St#compile.code, St#compile.options) of
		    {ok,Code} -> {ok,St#compile{code=Code}};
		    {error,Es} -> {error,St#compile{errors=St#compile.errors ++ Es}}
		end
	end,
    [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|Ps], Opts) ->
    [{listing,fun (St) -> src_listing(Ext, St) end}];
select_passes([{listing,Ext}|Ps], Opts) ->
    [{listing,fun (St) -> listing(Ext, St) end}];
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{Name,Fun}|Ps], Opts) when function(Fun) ->
    [{Name,Fun}|select_passes(Ps, Opts)];
select_passes([], Opts) -> [];
select_passes([List|Ps], Opts) when list(List) ->
    Nested = select_passes(List, Opts),
    case last(Nested) of
	{listing,Fun} -> Nested;
	Other         -> Nested ++ select_passes(Ps, Opts)
    end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
    ShouldNotBe = not ShouldBe,
    case member(Flag, Opts) of 
	ShouldBe    -> select_passes([Pass|Ps], Opts);
	ShouldNotBe -> select_passes(Ps, Opts)
    end.

%% The standard passes (almost) always run.

standard_passes(v1) -> [?pass(v1_is_dead)];
standard_passes(v2) -> [?pass(v2_is_dead)];
standard_passes(v3) ->
    [?pass(transform_module),
     {iff,'P',{src_listing,"P"}},
     ?pass(lint_module),
     ?pass(expand_module),
     {iff,dexp,{listing,"expand"}},
     {iff,'E',{src_listing,"E"}},
     {iff,'abstr',{listing,"abstr"}},
     {iff,debug_info,?pass(save_abstract_code)},

     %% Core Erlang passes.
     {pass,v3_core},
     {iff,dcore,{listing,"core"}},
     {unless,no_copt,{pass,v3_core_opt}},
     {unless,no_copt,?pass(core_transforms)},
     {iff,dcopt,{listing,"coreopt"}},
     {iff,clint,?pass(core_lint_module)},

     %% Kernel Erlang and code generation.
     {pass,v3_kernel},
     {iff,dkern,{listing,"kernel"}},
     {pass,v3_life},
     {iff,dlife,{listing,"life"}},
     {pass,v3_codegen},
     {iff,dcg,{listing,"codegen"}},

     %% Assembly level optimisations.
     {unless,no_postopt,
      [{pass,beam_block},
       {iff,dblk,{listing,"block"}},
       {pass,beam_bs},
       {iff,dbs,{listing,"bs"}},
       {unless,no_jopt,{pass,beam_jump}},
       {iff,djmp,{listing,"jump"}},
       {unless,no_topt,{pass,beam_type}},
       {iff,dtype,{listing,"type"}},
       {pass,beam_clean},
       {iff,dclean,{listing,"clean"}},
       {pass,beam_flatten}]},
     {iff,dopt,{listing,"optimize"}},
     {iff,'S',{listing,"S"}},

     ?pass(beam_asm),
     {iff,native,?pass(native_compile)},
     {unless,binary,?pass(save_binary)}].

compiler_version(Opts) ->
    compiler_version(Opts, []).

compiler_version([O|Opts], Acc) ->
    case is_compiler_version(O) of
	true  -> {O,[O|[Opt || Opt <- Opts, not is_compiler_version(Opt)]++Acc]};
	false -> compiler_version(Opts, [O|Acc])
    end;
compiler_version([], Acc) -> {?DEF_VERSION,[?DEF_VERSION|Acc]}.

is_compiler_version(v1) -> true;
is_compiler_version(v2) -> true;
is_compiler_version(v3) -> true;
is_compiler_version(_)  -> false.

%%%
%%% Compiler passes.
%%%

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(St) ->
    file:delete(St#compile.ofile),
    {ok,St}.

-record(asm_module, {module,
		     exports,
		     labels,
		     functions=[],
		     cfun,
		     code,
		     attributes=[]}).

preprocess_asm_forms(Forms) ->
    R = #asm_module{},
    R1 = collect_asm(Forms, R),
    {R1#asm_module.module,
     {R1#asm_module.module,
      R1#asm_module.exports,
      R1#asm_module.attributes,
      R1#asm_module.functions,
      R1#asm_module.labels}}.

collect_asm([], R) ->
    case R#asm_module.cfun of
	undefined ->
	    R;
	{A,B,C} ->
	    R#asm_module{functions=R#asm_module.functions++
			 [{function,A,B,C,R#asm_module.code}]}
    end;
collect_asm([{module,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{module=M});
collect_asm([{exports,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{exports=M});
collect_asm([{labels,M} | Rest], R) ->
    collect_asm(Rest, R#asm_module{labels=M});
collect_asm([{function,A,B,C} | Rest], R) ->
    R1 = case R#asm_module.cfun of
	     undefined ->
		 R;
	     {A0,B0,C0} ->
		 R#asm_module{functions=R#asm_module.functions++
			      [{function,A0,B0,C0,R#asm_module.code}]}
	 end,
    collect_asm(Rest, R1#asm_module{cfun={A,B,C}, code=[]});
collect_asm([{attributes, Attr} | Rest], R) ->
    collect_asm(Rest, R#asm_module{attributes=Attr});
collect_asm([X | Rest], R) ->
    collect_asm(Rest, R#asm_module{code=R#asm_module.code++[X]}).

beam_consult_asm(St) ->
    case file:consult(St#compile.ifile) of
	{ok, Forms0} ->
	    {Module, Forms} = preprocess_asm_forms(Forms0),
	    {ok,St#compile{module=Module, code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,compile,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

error_if_jam(St) ->	
    case member(jam, St#compile.options) of
	true  ->
	    Es = [{St#compile.ifile,[{none,compile,jam_is_dead}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	false ->
	    {ok,St}
    end.

v1_is_dead(St) ->
    Es = [{St#compile.ifile,[{none,compile,v1_is_dead}]}],
    {error,St#compile{errors=St#compile.errors ++ Es}}.

v2_is_dead(St) ->
    Es = [{St#compile.ifile,[{none,compile,v2_is_dead}]}],
    {error,St#compile{errors=St#compile.errors ++ Es}}.

parse_module(St) ->
    Opts = St#compile.options,
    Cwd = case keysearch(cwd, 1, Opts) of
	      {value, {cwd, Dir}} -> Dir;
	      _ -> "."
	  end,
    IncludePath = [Cwd, St#compile.dir|inc_paths(Opts)],
    Tab = ets:new(compiler__tab, [protected,named_table]),
    ets:insert(Tab, {compiler_options,Opts}),
    R =  epp:parse_file(St#compile.ifile, IncludePath, pre_defs(Opts)),
    ets:delete(Tab),
    case R of
	{ok,Forms} ->
	    {ok,St#compile{code=Forms}};
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,compile,{open,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
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
    Name = "transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:parse_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({N,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{parse_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	Forms ->
	    foldl_transform(St#compile{code=Forms}, Ts)
    end;
foldl_transform(St, []) -> {ok,St}.

get_core_transforms(Opts) -> [M || {core_transform,M} <- Opts]. 

core_transforms(St) ->
    %% The options field holds the complete list of options at this

    Ts = get_core_transforms(St#compile.options),
    foldl_core_transforms(St, Ts).

foldl_core_transforms(St, [T|Ts]) ->
    Name = "core transform " ++ atom_to_list(T),
    Fun = fun(S) -> T:core_transform(S#compile.code, S#compile.options) end,
    Run = case member(time, St#compile.options) of
	      true  -> fun run_tc/2;
	      false -> fun({N,F}, S) -> catch F(S) end
	  end,
    case Run({Name, Fun}, St) of
	{'EXIT',R} ->
	    Es = [{St#compile.ifile,[{none,compile,{core_transform,T,R}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}};
	Forms ->
	    foldl_core_transforms(St#compile{code=Forms}, Ts)
    end;
foldl_core_transforms(St, []) -> {ok,St}.

%%% Fetches the module name from a list of forms. The module attribute must
%%% be present.
get_module([{attribute,_,module,M} | _]) -> M;
get_module([_ | Rest]) ->
    get_module(Rest).

%%% A #compile state is returned, where St.base has been filled in
%%% with the module name from Forms, as a string, in case it wasn't
%%% set in St (i.e., it was "").
add_default_base(St, Forms) ->
    F = St#compile.filename,
    case F of
	"" ->
	    M = get_module(Forms),
	    St#compile{base = atom_to_list(M)};
	_ ->
	    St
    end.

lint_module(St) ->
    case erl_lint:module(St#compile.code,
			 St#compile.ifile, St#compile.options) of
	{ok,Ws} ->
	    %% Insert name of module as base name, if needed. This is
	    %% for compile:forms to work with listing files.
	    St1 = add_default_base(St, St#compile.code),
	    {ok,St1#compile{warnings=St1#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

core_lint_module(St) ->
    case core_lint:module(St#compile.code, St#compile.options) of
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

save_abstract_code(St) ->
    {ok,St#compile{abstract_code=abstract_code(St)}}.

abstract_code(#compile{code={Mod,Exp,Forms}}) ->
    Abstr = {abstract_v2,Forms},
    case catch erlang:term_to_binary(Abstr, [compressed]) of
	{'EXIT',_} -> term_to_binary(Abstr);
	Other -> Other
    end.

beam_asm(#compile{code=Code0,abstract_code=Abst,options=Opts0}=St) ->
    Opts = filter(fun is_informative_option/1, Opts0),
    case beam_asm:module(Code0, Abst, Opts) of
	{ok,Code} -> {ok,St#compile{code=Code,abstract_code=[]}};
	{error,Es} -> {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

native_compile(St) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    Ws = [{St#compile.ifile,[{none,compile,no_native_support}]}],
	    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
	_ ->
	    native_compile_1(St)
    end.

native_compile_1(St) ->
    Opts0 = St#compile.options,
    IgnoreErrors = member(ignore_native_errors, Opts0),
    Opts = case keysearch(hipe, 1, Opts0) of
	       {value,{hipe, L}} when list(L) -> L;
	       _ -> []
	   end,
    case catch hipe:compile(St#compile.module, St#compile.code, Opts) of
	{ok, {Type,Bin}} when binary(Bin) ->
	    {ok, embed_native_code(St, {Type,Bin})};
	{error, R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    Es = [{St#compile.ifile,[{none,?MODULE,{native,R}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{'EXIT',R} ->
	    case IgnoreErrors of
		true ->
		    Ws = [{St#compile.ifile,[{none,?MODULE,{native_crasch,R}}]}],
		    {ok,St#compile{warnings=St#compile.warnings ++ Ws}};
		false ->
		    exit(R)
	    end
    end.

embed_native_code(St, {Type,NativeCode}) ->
    {ok, _, Chunks0} = beam_lib:all_chunks(St#compile.code),
    ChunkName =
	case Type of
	    ultrasparc -> "HS8P"; %% HiPE, SPARC, V8+ (implicit: 32-bit)
	    x86 ->        "HX86"  %% HiPE, x86, (implicit: Unix)
	    %% Future:     HSV9      HiPE, SPARC, V9 (implicit: 64-bit)
            %%             HW32      HiPE, x86, Win32 
	end,
    Chunks = Chunks0 ++ [{ChunkName,NativeCode}],
    {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
    St#compile{code=BeamPlusNative}.

%% Returns true if the option is informative and therefore should be included
%% in the option list of the compiled module.

is_informative_option(beam) -> false;
is_informative_option(report_warnings) -> false;
is_informative_option(report_errors) -> false;
is_informative_option(binary) -> false;
is_informative_option(verbose) -> false;
is_informative_option(_) -> true.
    
save_binary(St) ->
    Tfile = tmpfile(St#compile.ofile),		%Temp working file
    case write_binary(Tfile, St#compile.code, St) of
	ok ->
	    case file:rename(Tfile, St#compile.ofile) of
		ok ->
		    {ok,St};
		{error,E} ->
		    file:delete(Tfile),
		    Es = [{St#compile.ofile,[{none,?MODULE,{rename,Tfile}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,E} ->
	    Es = [{Tfile,[{compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

write_binary(Name, Bin, St) ->
    Opts = case member(compressed, St#compile.options) of
	       true -> [compressed];
	       false -> []
	   end,
    case file:open(Name, [write, raw|Opts]) of
	{ok, Fd} ->
	    Res = case file:write(Fd, Bin) of
		      ok ->
			  ok;
		      {error, Reason} ->
			  {error, Reason}
		  end,
	    file:close(Fd),
	    Res;
	{error, Reason} ->
	    {error, Reason}
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(St) ->
    case member(report_errors, St#compile.options) of
	true ->
	    foreach(fun ({{F,L},Eds}) -> list_errors(F, Eds);
			({F,Eds}) -> list_errors(F, Eds) end,
		    St#compile.errors);
	false -> ok
    end.

report_warnings(St) ->
    case member(report_warnings, St#compile.options) of
	true ->
	    foreach(fun ({{F,L},Eds}) -> list_warnings(F, Eds);
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

%% erlfile(Dir, Base) -> ErlFile
%% outfile(Base, Extension, Options) -> OutputFile
%% objfile(Base, Target, Options) -> ObjFile
%% tmpfile(ObjFile) -> TmpFile
%%  Work out the correct input and output file names.

iofile(File) when atom(File) ->
    iofile(atom_to_list(File));
iofile(File) ->
    {filename:dirname(File), filename:basename(File, ".erl")}.

erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base++Suffix).

outfile(Base, Ext, Opts) when atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		Other -> Base			% Not found or bad format
	    end,
    Obase++"."++Ext.

objfile(Base, St) ->
    outfile(Base, "beam", St#compile.options).

tmpfile(Ofile) ->
    reverse([$#|tl(reverse(Ofile))]).

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

src_listing(Ext, St) ->
    listing(fun (Lf, {Mod,Exp,Fs}) -> do_src_listing(Lf, Fs);
		(Lf, Fs) -> do_src_listing(Lf, Fs) end,
	    Ext, St).

do_src_listing(Lf, Fs) ->
    foreach(fun (F) -> io:put_chars(Lf, [erl_pp:form(F),"\n"]) end,
	    Fs).

listing(Ext, St) ->
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, St).

listing(LFun, Ext, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write,delayed_write]) of
	{ok,Lf} -> 
	    LFun(Lf, St#compile.code),
	    ok = file:close(Lf),
	    {ok,St};
	{error,E} ->
	    Es = [{Lfile,[{none,compile,write_error}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

options() ->
    help(standard_passes(?DEF_VERSION)).

%% Intentionally undocumented.

options(Version) ->
    help(standard_passes(Version)).

help([{iff,Flag,{src_listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s source listing file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{listing,Ext}}|T]) ->
    io:fwrite("~p - Generate .~s file\n", [Flag,Ext]),
    help(T);
help([{iff,Flag,{Name,Fun}}|T]) when function(Fun) ->
    io:fwrite("~p - Run ~s\n", [Flag,Name]),
    help(T);
help([{iff,Flag,Action}|T]) ->
    help(Action),
    help(T);
help([{unless,Flag,{pass,Pass}}|T]) ->
    io:fwrite("~p - Skip the ~s pass\n", [Flag,Pass]),
    help(T);
help([{unless,no_postopt=Flag,List}|T]) when list(List) ->
    %% Hard-coded knowledgde here.
    io:fwrite("~p - Skip all post optimisation\n", [Flag]),
    help(List),
    help(T);
help([{unless,Flag,Action}|T]) ->
    help(Action),
    help(T);
help([H|T]) ->
    help(T);
help(_) ->
    ok.


%% compile(AbsFileName, Outfilename, Options)
%%   Compile entry point for erl_compile.

compile(File, _OutFile, Options) ->
    case file(File, make_erl_options(Options)) of
	{ok, _Mod} -> ok;
	Other -> Other
    end.

%% Converts generic compiler options to specific options.

make_erl_options(Opts) ->

    %% This way of extracting will work even if the record passed
    %% has more fields than known during compilation.

    Includes0 = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    Optimize = Opts#options.optimize,
    OutputType = Opts#options.output_type,
    Cwd = Opts#options.cwd,

    Includes = 
	case Opts#options.ilroot of
	    undefined ->
		Includes0;
	    Ilroot ->
		[Ilroot|Includes0]
	end,

    Options =
	case Verbose of
	    true ->  [verbose];
	    false -> []
	end ++
	case Warning of
	    0 -> [];
	    _ -> [report_warnings]
 	end ++
 	case Optimize of
	    0 -> [no_kernopt,no_postopt];
	    1 -> [no_postopt];
	    Other -> []
	end ++
	map(
	  fun ({Name, Value}) ->
		  {d, Name, Value};
	      (Name) ->
		  {d, Name}
	  end,
	  Defines) ++
	case OutputType of
	    undefined -> [];
	    jam -> [jam];
	    beam -> [beam];
	    native -> [native]
	end,

    Options++[report_errors, {cwd, Cwd}, {outdir, Outdir}|
	      map(fun(Dir) -> {i, Dir} end, Includes)]++Specific.
