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

-module(coast_compile_funcs).



-export([compile/4]).


-include("coast_server.hrl").



-record(vars, {mod_name,
	       obj_dir,
	       comp_filename,
	       trace_mode,
	       max_bumps,
	       init_info,
	       all_funcs,
	       pretty_mode,
	       line_no,
	       fcn_name,
	       arity,
	       clause_no,
	       clause_line,
	       call_depth,
	       call_direction    %% Used only on call_depth level 1, otherwise not interesting!
	      }).





%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




%%======================================================================
%% Function:       compile/4
%%
%% Return Value:  
%%
%% Description:   Compiles a source file for coverage analysis. This is 
%%                done in two steps:
%%                1. The source file is parsed and re-written, in a 
%%                   "pretty-format" way, to a file called 
%%                    <modulename>.COAST.pretty.erl.
%%                2. The file created in step 1. is parsed, and function 
%%                   calls to coast:bump is added where apropriate. (The 
%%                   reason for using the file from step 1 is that we want 
%%                   proper line numbering, i.e., only one statement on each 
%%                   line, since we in a graphical presentation like to 
%%                   identify each statement and link it to the number of
%%                   times it has been called.
%%
%% Parameters:    
%%
%%======================================================================


compile(File, Mode, Options, Trace) ->
    case catch compile2(File, Mode, Options, Trace) of
	{module, Module} ->
	    Module;
	{error, Reason} ->
	    {error, {File, Reason}};
	{'EXIT', Reason} ->
	    {error, {File, {'EXIT', Reason}}}
    end.






%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




compile2(File, coastify, Opts, TraceMode) ->
       %% Check that the file exists!
    {ModNameS, ModNameA, SrcDirS} = check_file(File, coastify),
    coast_comm_funcs:remove_from_database(ModNameA),

    ObjDirS = get_objdir(Opts),
    %%NewOptions = lists:keydelete(outdir, 1, Opts),
    {UsrIncPath, Defs} = get_incpath_and_defs(Opts),

    Vars = #vars{mod_name      = ModNameA,
		 obj_dir       = ObjDirS,
		 comp_filename = filename:join(SrcDirS, ModNameS) ++ ".erl",
		 trace_mode    = TraceMode
		},

    mk_coast_srcfile(ModNameS, SrcDirS, ObjDirS, UsrIncPath, Vars),
    comp_cover_srcfile(ModNameS, ObjDirS, Opts, Defs),

    {module, ModNameA};
compile2(File, uncoastify, Opts, TraceMode) ->
    {ModNameS, ModNameA, SrcDirS} = check_file(File, uncoastify),
    coast_comm_funcs:remove_from_database(ModNameA),
    OutDir = case lists:keysearch(outdir, 1, Opts) of
		 {value, {outdir,Dir}} ->
		     %% We want the full pathname, not (e.g.) "../ebin"
		     filename:absname(Dir);
		 false ->
		     element(2, file:get_cwd())
	     end,
    NewOpts = lists:keydelete(outdir, 1, Opts),
    {UsrIncPath, Defs} = get_incpath_and_defs(NewOpts),
    comp_org_srcfile(ModNameS, SrcDirS),
    {module, ModNameA}.


				     



get_objdir(Options) ->
    case lists:keysearch(outdir, 1, Options) of
	{value, {outdir, Dir}} ->
	    %% We want the full pathname, not (e.g.) "../ebin"
	    filename:absname(Dir);
	false ->
	    element(2, file:get_cwd())
    end.







check_file(File, uncoastify) ->
    case coast_comm_funcs:get_source_files([File]) of
	{error, Reason} ->
	    {error, Reason};
	SrcFile ->
	    check_file(SrcFile, coastify)
    end;
check_file(File, coastify) when atom(File) ->
    check_file(atom_to_list(File), coastify);
check_file(File, coastify) ->
       % Remove ".erl"-extension, and directory path!
    ModNameS = filename:rootname(filename:basename(File)),
       % Get directory!
    SrcDirS = filename:dirname(filename:absname(File)),
    CompNameS = filename:join(SrcDirS, ModNameS) ++ ".erl",
       % Check if there is such a file!
    case file:open(CompNameS, read) of
	{ok, Fd} ->
	    file:close(Fd),
	    {ModNameS, list_to_atom(ModNameS), SrcDirS};
	{error, enoent} ->
	    throw({error, {no_such_file, CompNameS}});
	{error, eaccess} ->
	    throw({error, {cannot_read_file, CompNameS}});
	{error, eisdir} ->
	    throw({error, {filename_is_directory, CompNameS}});
	{error, Reason} ->
	    throw({error, {Reason, CompNameS}})
    end.


		    





get_incpath_and_defs(Options) ->
    F = fun(A1) ->
		lists:reverse(
		  lists:foldl(
		    fun({A2, X}, Acc) when A2 == A1 ->
			    [X | Acc];
		       (_, Acc) ->
			    Acc
		    end, 
		    [],
		    Options))
	end,
    {F(i), F(d)}.




	    

mk_coastdirs(ObjDirS) ->
    mk_dir(ObjDirS, ?COAST_DIR),
    mk_dir(ObjDirS, ?COAST_SRC_DIR).




mk_dir(RootDirS, SubDirS) ->
    case mk_dir2(filename:join(RootDirS, SubDirS)) of
	ok ->
	    done;
	{error, Reason} ->
	    throw({error, {Reason, RootDirS}})
    end.




mk_dir2(Dir) ->
    case file:make_dir(Dir) of
	{error, eexist} ->
	    ok;
	ok ->
	    ok;
	{error, eaccess} ->
	    {error, no_write_access};
	{error, enoent} ->
	    {error, erroneous_pathname};
	{error, enotdir} ->
	    {error, erroneous_pathname}
    end.
    





mk_coast_srcfile(ModS, SrcDirS, ObjDirS, UsrIncPath, Vars) ->
       %% Create the COAST directories, if they haven't been already.
    mk_coastdirs(ObjDirS),
    CoastSrcDirS = filename:join(ObjDirS, ?COAST_SRC_DIR),
    InFileS      = filename:join(SrcDirS, ModS) ++ ".erl",

       %% Create a prettyfile, i.e., without any comments, just the code.
    PrettyFileS = filename:join(CoastSrcDirS, ModS ++ ?COAST_PRETTY_SUFFIX),
    IncPath     = UsrIncPath ++ [SrcDirS],
       %% Has to put SrcDirS in a list, since it is a string, and 
       %% that string would be interpreted as a list otherwise!
       %% Remember that we try to add UserIncludePath to a list...  :-/
    transform(pretty, InFileS, PrettyFileS, IncPath, Vars),

       %% Create a coast source file, i.e., with bump calls added, and so on.
    CoastFileS = filename:join(CoastSrcDirS, ModS ++ ?COAST_SOURCE_SUFFIX),
    transform(not_pretty, PrettyFileS, CoastFileS, [], Vars).
	    

	    


	    
	    
comp_cover_srcfile(ModS, ObjDirS, Opts, Defs) ->
    FileNameNoSuffixS = filename:join(ObjDirS, 
				      filename:join(?COAST_SRC_DIR, ModS)),
    SrcFileS = FileNameNoSuffixS ++ ".COAST",
    ObjFileS = filename:join(ObjDirS, ModS),
    case compile:file(SrcFileS, [report_errors, report_warnings | Opts]) of
	{ok, Module} when Module /= [] ->
	    ObjExt = code:objfile_extension(),
	    file:rename(ObjFileS ++ ".COAST" ++ ObjExt, ObjFileS ++ ObjExt),
	    code:purge(Module),
	    code:load_abs(ObjFileS);
	{ok, []} ->
	    throw({error, unknown_module});
	{error, Reason} ->
	    throw({error, {compilation_failed, Reason}})
    end.
	





comp_org_srcfile(ModName, SourceCodeDirStr) ->
    SrcFile = filename:join(SourceCodeDirStr, ModName) ++ ".erl",
    file:delete(ModName ++ ?COAST_PRETTY_SUFFIX),
    file:delete(ModName ++ ?COAST_SOURCE_SUFFIX),
    case compile:file(SrcFile, [report_errors, report_warnings]) of
	{ok, []} ->
	    throw({error, unknown_module});
	{ok, Module} ->
	    X = code:purge(Module),
	    Y = code:load_abs(Module);
	{error, Reason} ->
	    throw({error, {compilation_failed, Reason}})
    end.
    


	    


transform(Mode, InFile, OutFile, IncludePath, Vars) ->
    {ok, InFd, OutFd} = open_in_and_outfiles(InFile, OutFile, IncludePath),
    
    NewVars1 = Vars#vars{max_bumps   = 0,
			 init_info   = [],
			 all_funcs   = [],
			 pretty_mode = Mode,
			 call_depth  = 0
			},
    case catch translate(Mode, InFd, OutFd, NewVars1) of
	{transformed, NewVars2} ->
	    windup(Mode, OutFd, OutFile, NewVars2), 
	    close_in_and_outfiles(InFd, OutFd),
	    transformed;
	{error, Reason} ->
	    throw({error, Reason});
	OtherError ->
	    throw({error, OtherError})
    end.


	


open_in_and_outfiles(InFile, OutFile, IncludePath) ->
    InFd = case epp:open(InFile, IncludePath) of
	       {error, Reason1} ->
		   throw({error, Reason1});
	       {ok, Fd1} ->
		   Fd1
	   end,
    OutFd = case file:open(OutFile, write) of
		{error, Reason2} ->
		    throw({error, Reason2});
		{ok, Fd2} ->
		    Fd2
	    end,
    {ok, InFd, OutFd}.
		    




close_in_and_outfiles(InFd, OutFd) ->
    epp:close(InFd),
    file:close(OutFd).
    







%%----------------------------------------------------------------------
%% Functions used when parsing the source code file.
%%----------------------------------------------------------------------



translate(PrettyMode, InFd, OutFd, Vars) ->
    NextForm = epp:parse_erl_form(InFd),
    translation_loop(PrettyMode, NextForm, InFd, OutFd, Vars).





translation_loop(pretty, {ok, {attribute,Line,file,Mod}}, InFd, OutFd, Vars) ->
    translate(pretty, InFd, OutFd, Vars);
translation_loop(pretty, {ok, {attribute,Line,module,Mod}}, InFd, OutFd, Vars) ->
    NewVars = Vars#vars{mod_name=Mod},
    io:format(OutFd, "~n~s", [erl_pp:form({attribute,Line,module,Mod})]),
    translate(pretty, InFd, OutFd, NewVars);
translation_loop(pretty, 
		 {ok, {attribute,Line,AttrName,AttrData}}, InFd, OutFd, Vars) ->
    io:format(OutFd, "~n~s", [erl_pp:form({attribute,Line,AttrName,AttrData})]),
    translate(pretty, InFd, OutFd, Vars);
translation_loop(pretty, {ok,Form}, InFd, OutFd, Vars) ->
    {MungedForm, NewVars} = munge(Form, Vars),
    io:format(OutFd, "~n~n~n~s", [erl_pp:form(MungedForm)]),
    case MungedForm of
	{function,Line,Name,Arity,MungeRes} ->
	    io:format(OutFd, 
		      "~n%%%%%--> End of function ~p/~p <--%%%%%~n", 
		      [Name,Arity]);
	_OtherForm ->
	    done
    end,
    translate(pretty, InFd, OutFd, NewVars);
translation_loop(PrettyMode, 
		 {ok, {attribute,Line,module,Mod}}, InFd, OutFd, Vars) ->
    NewVars = Vars#vars{mod_name=Mod},
    io:format(OutFd, "~s", [erl_pp:form({attribute,Line,module,Mod})]),
    io:format(OutFd, "~s", [erl_pp:form({attribute,Line,coast_compiled,
					 {Mod,NewVars#vars.comp_filename}})
			   ]),
    io:format(OutFd, "~s", [erl_pp:form({attribute,Line,coast_obj_dir,
					 NewVars#vars.obj_dir})
			   ]),
    io:format(OutFd, "~s~n", [erl_pp:form({attribute,Line,export, 
					 [{'coast_data_$$_private',0}, 
					  {'coast_init_info_$$_private', 0}]})
			     ]),
    translate(PrettyMode, InFd, OutFd, NewVars);
translation_loop(PrettyMode, {ok,Form}, InFd, OutFd, Vars) ->
    {MungedForm, NewVars} = munge(Form, Vars),
    io:format(OutFd, "~s", [erl_pp:form(MungedForm)]),
    translate(PrettyMode, InFd, OutFd, NewVars);
translation_loop(PrettyMode, {eof,Line}, InFd, OutFd, Vars) ->
    {transformed, Vars};
translation_loop(PrettyMode, {error, {Line,Mod,Error}}, InFd, OutFd, Vars) ->
    {error, {parse_error,Line,Error}}.






windup(pretty, OutFd, OutFileName, Vars) ->
    done;
windup(Mode, OutFd, OutFileName, Vars) ->
       % Add the info necessary to "afterwards" initialise the database.
    InitInfo = Vars#vars.init_info,
    Form1    = {function,0,'coast_init_info_$$_private',0, 
		[{clause,0,[],[],
		  [erl_parse:abstract(InitInfo)]}
		]},
    io:format(OutFd, "~s~n", [erl_pp:form(Form1)]),

    AllFuncs = Vars#vars.all_funcs,
    Form2    = {function,0,'coast_data_$$_private',0,
		[{clause,0,[],[],
		  [erl_parse:abstract({OutFileName, AllFuncs})]}
		]},
    io:format(OutFd, "~s~n", [erl_pp:form(Form2)]).





% Module epp returns the following, in the case of a function:
% {ok, {function, LineNo, FunctionName, Arity, ListOfClauseForms}}

munge({attribute,Line,module,Mod}, Vars) ->
    {{attribute,Line,module,Mod}, Vars#vars{mod_name=Mod}};
munge({function,Line,Name,Arity,Clauses}, Vars) ->
    NewVars1 = Vars#vars{line_no   = Line,
			 fcn_name  = Name,
			 arity     = Arity,
			 clause_no = 1
			},
    {MungeRes, NewVars2} = munge_clauses(Clauses, NewVars1),
    {{function,Line,Name,Arity,MungeRes}, NewVars2};
munge(X, Vars) ->
    {X, Vars}.






% Each function clause has the following format:
% {clause, LineNo, ListOfArgForms, ListOfGuardForms, ListOfBodyForms}

munge_clauses(ClauseForm, Vars) ->
    munge_clauses(ClauseForm, Vars, []).


munge_clauses([{clause,Line,Head,Guard,Body} | T], Vars, MungeAcc) ->
    {BodyRes, NewVars} = 
	munge_body(Body, Vars#vars{line_no=Line,clause_line=Line}),
    NewClauseNo = NewVars#vars.clause_no + 1,
    munge_clauses(T, NewVars#vars{clause_no=NewClauseNo},
		  [{clause,Line,Head,Guard,BodyRes} | MungeAcc]
		 );
munge_clauses([], Vars, MungeAcc) -> 
    {lists:reverse(MungeAcc), Vars}.








munge_body([H | T], Vars) ->
    {Bump, NewVars1} = 
	case Vars#vars.pretty_mode of
	    pretty ->
		{[], Vars};
	    _OtherMode ->
		CurrMod = Vars#vars.mod_name,
		case H of
		    {call, _Line1, {remote, _Line2, {atom,_Line3,CurrMod}, _Func}, _Args} ->
			add_bump(Vars#vars{call_direction=internal});
		    {call, _Line1, {remote, _Line2, _Mod, _Func}, _Args} ->
			add_bump(Vars#vars{call_direction=external});
		    {call, _Line1, {tuple, _Line2, [{atom,_Line3,CurrMod}, _Func]}, _Args} ->
			add_bump(Vars#vars{call_direction=internal});
		    {call, _Line1, {tuple, _Line2, [_Mod, _Func]}, _Args} ->
			add_bump(Vars#vars{call_direction=external});
		    {call, _Line1, {atom,_Line2,apply}, [{atom,_Line3,CurrMod}, _Func, _Args]} ->
			add_bump(Vars#vars{call_direction=internal});
		    {call, _Line1, {atom,_Line2,apply}, [_Mod, _Func, _Args]} ->
			add_bump(Vars#vars{call_direction=external});
		    _OtherH ->
			   %% Direction only interesting if external,
			   %% set it to 'internal' otherwise!
			add_bump(Vars#vars{call_direction=internal})
		end
	end,
    {ExprRes, NewVars2} = munge_expr(H, NewVars1),
    {BodyRes, NewVars3} = munge_body(T, NewVars2),
    {Bump ++ [ExprRes | BodyRes], NewVars3};
munge_body([], Vars) ->
    {[], Vars}.










% The following expressions are of interest:
%
%  {'if', Line, Opts}
%  {'case', Line, Func, Opts}
%  {'receive', Line, Opts, Func, TimeOutBody}
%  {'receive', Line, Opts}
%  {'catch', Line, E0}
%  {match,Line,LS,RS}
%  {call,Line,Label,Args}
%  {'fun', Line, Body}
%  {tuple, Line, Es0}
%  {op, Line, Op, A0}
%  {op, Line, Op, L0, R0}
%  {remote, Line, M0, F0}
%  {var, Line, VariabelName}
%  {TermName, Line, Value}   where TermName may be any of integer, float, atom, or string
%  {nil, Line}
%
%
% The following expressions are probably not of interest:
%
%  {record_index,Line,Name,Field0}
%  {record,Line,Name,Inits0}
%  {record_field,Line,Rec0,Name,Field0}
%  {record,Line,Rec0,Name,Upds0}
%  {record_field,Line,Rec0,Field0}
%  {block,Line,Es0}
%
% (For further information about the forms that exist, 
% see 'erl_id_trans.erl' in the stdlib!)

munge_expr({integer,Line,Value}, Vars) ->
    {{integer,Line,Value}, Vars#vars{line_no=Line}};
munge_expr({float, Line, Value}, Vars) ->
    {{float,Line,Value}, Vars#vars{line_no=Line}};
munge_expr({atom,Line,Value}, Vars) ->
    {{atom,Line,Value}, Vars#vars{line_no=Line}};
munge_expr({string,Line,Value}, Vars) ->
    {{string,Line,Value}, Vars#vars{line_no=Line}};
munge_expr({tuple,Line,Es0}, Vars) ->
    {Es1, NewVars} = munge_tuple_expr_list(Es0, Vars#vars{line_no=Line}),
    {{tuple,Line,Es1}, NewVars};
munge_expr({var,Line,Value}, Vars) ->
    {{var,Line,Value}, Vars#vars{line_no=Line}};
munge_expr({op,Line,Op,A0}, Vars) ->
    {A1, NewVars} = munge_expr(A0, Vars#vars{line_no=Line}),
    {{op,Line,Op,A1}, NewVars};
munge_expr({op,Line,Op,L0,R0}, Vars) ->
    {L1, NewVars1} = munge_expr(L0, Vars#vars{line_no=Line}),
    {R1, NewVars2} = munge_expr(R0, NewVars1),
    {{op,Line,Op,L1,R1}, NewVars2};
munge_expr({match,Line,LS,RS}, Vars) ->
    {ExprRes1, NewVars1} = munge_expr(LS, Vars#vars{line_no=Line}),
    {ExprRes2, NewVars2} = munge_expr(RS, NewVars1),
    {{match,Line,ExprRes1,ExprRes2}, NewVars2};
munge_expr({call,Line,Label,Args}, Vars) ->
    CallDepth = Vars#vars.call_depth,
    {ExprRes, NewVars} = munge_call_expr_list(Args, Vars#vars{line_no=Line,
							      call_depth=CallDepth+1}),
    if 
	CallDepth >= 1, Vars#vars.pretty_mode /= pretty ->
	    add_last_call_setting({call,Line,Label,ExprRes}, 
				  NewVars#vars{call_depth=CallDepth}); 
	true ->
	    {{call,Line,Label,ExprRes}, 
	     NewVars#vars{call_depth=CallDepth}}   
    end;
munge_expr({'case',Line,Func,Opts}, Vars) -> 
    {OptionRes, NewVars} = munge_option_list(Opts, Vars#vars{line_no=Line}),
       %% Since the 'end' word isn't shown in the parsing, 
       %% we have to manually count that line.
    NewLine = NewVars#vars.line_no + 1,
    {{'case',Line,Func,OptionRes}, NewVars#vars{line_no=NewLine}};
munge_expr({'if',Line,Opts}, Vars) -> 
    {OptionRes, NewVars} = munge_option_list(Opts, Vars#vars{line_no=Line}),
       %% Since the 'end' word isn't shown in the parsing, 
       %% we have to manually count that line.
       %% Can't use variable 'Line' here!
    NewLine = NewVars#vars.line_no + 1,
    {{'if',Line,OptionRes}, NewVars#vars{line_no=NewLine}};
munge_expr({'receive',Line,Opts}, Vars) -> 
    {OptionRes, NewVars} = munge_option_list(Opts, Vars#vars{line_no=Line}),
       %% Since the 'end' word isn't shown in the parsing, 
       %% we have to manually count that line.
    NewLine = NewVars#vars.line_no + 1,
    {{'receive',Line,OptionRes}, NewVars#vars{line_no=NewLine}};
munge_expr({'receive',Line,Opts,Func,TimeOutBody}, Vars) ->
    {OptionRes, NewVars1} = munge_option_list(Opts, Vars#vars{line_no=Line}),
    {FuncRes, NewVars2}   = munge_expr(Func, NewVars1),
    {BodyRes, NewVars3}   = munge_body(TimeOutBody, NewVars2),
       %% Since the 'end' word isn't shown in the parsing, 
       %% we have to manually count that line.
    NewLine = NewVars3#vars.line_no + 1,
    {{'receive',Line,OptionRes,Func,BodyRes}, NewVars3#vars{line_no=NewLine}};
munge_expr({'catch',Line,E0}, Vars) ->
    {E1, NewVars} = munge_expr(E0, Vars#vars{line_no=Line}),
    {{'catch',Line,E1}, NewVars};
munge_expr({'fun',Line,{clauses,Cs0}}, Vars) ->
    {Cs1, NewVars} = munge_fun_clauses(Cs0, Vars#vars{line_no=Line}),
    {{'fun',Line,{clauses,Cs1}}, NewVars};
munge_expr({'fun',Line,FunctionForm}, Vars) ->
    {{'fun',Line,FunctionForm}, Vars#vars{line_no=Line}};
munge_expr({record,Line,Name,Inits}, Vars) ->
    {{record,Line,Name,Inits}, Vars#vars{line_no=Line}};
munge_expr({record,Line,Rec,Name,Upds}, Vars) ->
    {{record,Line,Rec,Name,Upds}, Vars#vars{line_no=Line}};
munge_expr({record_field,Line,Rec,Name,Field}, Vars) ->
    {{record_field,Line,Rec,Name,Field}, Vars#vars{line_no=Line}};
munge_expr({record_index,Line,Name,Field}, Vars) ->
    {{record_index,Line,Name,Field}, Vars#vars{line_no=Line}};
munge_expr({block,Line,Es}, Vars) ->
    {{block,Line,Es}, Vars#vars{line_no=Line}};
munge_expr({remote,Line,M0,F0}, Vars) ->
    {M1, NewVars1} = munge_expr(M0, Vars#vars{line_no=Line}),
    {F1, NewVars2} = munge_expr(F0, NewVars1),
    {{remote,Line,M1,F1}, NewVars2};
munge_expr({cons,Line,H0,T0}, Vars) ->
    {H1, NewVars1} = munge_expr(H0, Vars#vars{line_no=Line}),
    {T1, NewVars2} = munge_expr(T0, NewVars1),
    {{cons,Line,H1,T1}, NewVars2};
munge_expr({lc,Line,E0,Qs0}, Vars) ->
    {E1, NewVars1}  = munge_expr(E0, Vars#vars{line_no=Line}),
    {Qs1, NewVars2} = munge_lc_quals(Qs0, NewVars1),
    {{lc,Line,E1,Qs1}, NewVars2};
munge_expr({nil,Line}, Vars) ->
    {{nil, Line}, Vars#vars{line_no=Line}};
munge_expr({TermName,Line,Value}, Vars) ->  
       %% If we have missed some term names above...
    {{TermName,Line,Value}, Vars#vars{line_no=Line}};
munge_expr(X, Vars) ->       
       %% To catch other expressions, yet unknown...  :-/
    {X, Vars}.



munge_tuple_expr_list(ExprList, Vars) ->
    munge_tuple_expr_list(ExprList, Vars, []).

munge_tuple_expr_list([E0 | Es], Vars, Acc) ->
    {E1, NewVars} = munge_expr(E0, Vars),
    munge_tuple_expr_list(Es, NewVars, [E1 | Acc]);
munge_tuple_expr_list([], Vars, Acc) ->
    {lists:reverse(Acc), Vars}.




munge_call_expr_list(ExprList, Vars) ->
    munge_call_expr_list(ExprList, Vars, []).

munge_call_expr_list([E0 | Es], Vars, Acc) ->
    {E1, NewVars} = munge_expr(E0, Vars),
    munge_call_expr_list(Es, NewVars, [E1 | Acc]);
munge_call_expr_list([], Vars, Acc) ->
    {lists:reverse(Acc), Vars}.




munge_option_list(OptionList, Vars) ->
    munge_option_list(OptionList, Vars, []).



munge_option_list([{clause,Line,Guard,Body} | T], Vars, Acc) ->
    {BodyRes, NewVars} = munge_body(Body, Vars#vars{line_no=Line}),
    munge_option_list(T, NewVars, [{clause,Line,Guard,BodyRes} | Acc]);
munge_option_list([{clause,Line,Pattern,Guard,Body} | T], Vars, Acc) ->
    {BodyRes, NewVars} = munge_body(Body, Vars#vars{line_no=Line}),
    munge_option_list(T, NewVars, [{clause, Line, Pattern, Guard, BodyRes} | Acc]);
munge_option_list([], Vars, Acc) -> 
    {lists:reverse(Acc), Vars};
munge_option_list(X, Vars, Acc) ->
    throw({error, {unknown_option, X}}).






munge_lc_quals(LcQualsList, Vars) ->
    munge_lc_quals(LcQualsList, Vars, []).



munge_lc_quals([{generate,Line,P0,E0} | Qs], Vars, Acc) ->
    {P1, NewVars1} = munge_expr(P0, Vars#vars{line_no=Line}),
    {E1, NewVars2} = munge_expr(E0, NewVars1),
    munge_lc_quals(Qs, NewVars2, [{generate,Line,P1,E1} | Acc]);
munge_lc_quals([E0 | Qs], Vars, Acc) ->
    {E1, NewVars} = munge_expr(E0, Vars),
    munge_lc_quals(Qs, NewVars, [E1 | Acc]);
munge_lc_quals([], Vars, Acc) ->
       %% Since the closing bracket isn't shown in the parsing, 
       %% we have to add that line manually!
    NewLine = Vars#vars.line_no + 1,
    {lists:reverse(Acc), Vars#vars{line_no=NewLine}}.







% Each fun clause has the following format:
% {clause, LineNo, ListOfArgForms, ListOfGuardForms, ListOfBodyForms}
% This function is essentially the same as the function clause function, 
% the only difference being we don't want any clause numbering here!

munge_fun_clauses(FunClauseList, Vars) ->
    munge_fun_clauses(FunClauseList, Vars, []).



munge_fun_clauses([{clause,Line,Head,Guard,Body} | T], Vars, Acc) ->
    {BodyRes, NewVars} = munge_body(Body, Vars#vars{line_no=Line}),
    munge_fun_clauses(T, NewVars, 
		      [{clause,Line,Head,Guard,BodyRes} | Acc]
		     );
munge_fun_clauses([], Vars, Acc) ->
    {lists:reverse(Acc), Vars}.








% Results in 'coast:bump(Module, 
%                        Function, 
%                        Arity, 
%                        Index(?), 
%                        ClauseNumber, 
%                        ClauseLineNumber, 
%                        CodeLineNumber, 
%                        TraceOnOrOff)'
% where ClauseLineNumber is the line where the specific clause starts.
%
% NOTE: ClauseLineNumber is probably unnecessary information, since the
% same information can be obtained by getting the lowest line number for a given
% clause. Maybe this information will be removed later on.

add_bump(Vars) ->
    #vars{max_bumps      = N,
	  mod_name       = Mod, 
	  fcn_name       = Name,
	  arity          = Arity,
	  clause_no      = ClauseNo,
	  clause_line    = ClauseLine,
	  line_no        = CodeLine,
	  init_info      = InitInfo,
	  all_funcs      = AllFuncs,
	  call_direction = CallDirection}  = Vars,

    Trace = case Vars#vars.trace_mode of
		true ->
		    trace_on;
		Other ->
		    trace_off
	    end,

       %% Store information necessary for "initialisation" of the database. 
       %% Please note that this initialisation takes place afterwards, 
       %% when statistics for a certain module is requested!
    NewVars = Vars#vars{max_bumps = N + 1,
			init_info = [{Mod,Name,Arity,N,
				      ClauseNo,ClauseLine,CodeLine} | InitInfo],
			all_funcs = [{Name, Arity, N} | AllFuncs]
		       },
    
    {[{call, 0, {remote, 0, {atom,0,coast}, {atom,0,bump}},
       [{atom,    0, Mod},
	{atom,    0, Name},
	{integer, 0, Arity},
	{integer, 0, N},
	{integer, 0, ClauseNo}, 
	{integer, 0, ClauseLine},
	{integer, 0, CodeLine},
	{atom,    0, CallDirection},
	{atom,    0, Trace}
       ]
      }
     ],
     NewVars}.




add_last_call_setting(CallExpr, Vars) ->
    #vars{mod_name       = Mod, 
	  fcn_name       = Name,
	  arity          = Arity,
	  call_direction = CallDirection} = Vars,

    {{call, 0, {atom,0,element}, 
      [{integer,0,2}, {tuple, 0, 
		       [{call, 0, {remote, 0, {atom,0,coast}, {atom,0,set_last_call}},
			 [{atom,0,Mod}, 
			  {atom,0,Name}, 
			  {atom,0,Arity},
			  {atom,0,CallDirection}
			 ]
			},
			CallExpr, 
			{call, 0, {remote, 0, {atom,0,coast}, {atom,0,set_last_call}},
			 [{atom,0,Mod},
			  {atom,0,Name},
			  {atom,0,Arity},
			  {atom,0,CallDirection}
			 ]
			}
		       ]
		      }
      ]
     },
     Vars}.
