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
-module(asn1ct).

%% Compile Time functions for ASN.1 (e.g ASN.1 compiler).

%%-compile(export_all).
%% Public exports
-export([compile/1, compile/2]).
-export([start/0, start/1, stop/0]).
-export([encode/2, encode/3, decode/3]).
-export([test/1, test/2, test/3, value/2]).
%% Application internal exports
-export([compile_asn/3,compile_asn1/3,compile_py/3,compile/3,value/1,vsn/0]).
-include("asn1_records.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the interface to the compiler
%% 
%% 


compile(File) ->
    compile(File,[]).

compile(File,Options) when list(Options) ->
    case (catch input_file_type(File)) of
	{single_file,PrefixedFile} ->
	    (catch compile1(PrefixedFile,Options));
	{multiple_files_file,SetBase,FileName} ->
	    FileList = get_file_list(FileName),
	    (catch compile_set(SetBase,filename:dirname(FileName),
			       FileList,Options));
	Err = {input_file_error,Reason} ->
	    {error,Err}
    end.


compile1(File,Options) when list(Options) ->
    io:format("Erlang ASN.1 version ~p compiling ~p ~n",[?vsn,File]),
    io:format("Compiler Options: ~p~n",[Options]),
    Ext = filename:extension(File),
    Base = filename:basename(File,Ext),
    OutFile = outfile(Base,"",Options),
    DbFile = outfile(Base,"asn1db",Options),
    Includes = [I || {i,I} <- Options],
    EncodingRule = get_rule(Options),
    Continue1 = scan({true,true},File,Options),
    Continue2 = parse(Continue1,File,Options),
    Continue3 = check(Continue2,File,OutFile,Includes,EncodingRule,
		      DbFile,Options,[]),
    Continue4 = generate(Continue3,OutFile,EncodingRule,Options),
    compile_erl(Continue4,OutFile,Options).

%%****************************************************************************%%
%% functions dealing with compiling of several input files to one output file %%
%%****************************************************************************%%
compile_set(SetBase,DirName,Files,Options) when list(hd(Files)),list(Options) ->
    %% case when there are several input files in a list
    io:format("Erlang ASN.1 version ~p compiling ~p ~n",[?vsn,Files]),    
    io:format("Compiler Options: ~p~n",[Options]),
    OutFile = outfile(SetBase,"",Options),
    DbFile = outfile(SetBase,"asn1db",Options),
    Includes = [I || {i,I} <- Options],
    EncodingRule = get_rule(Options),
    ScanRes = scan_set(DirName,Files,Options),
    ParseRes = parse_set(ScanRes,Options),
    case [X||X <- ParseRes,element(1,X)==true] of
	[] -> %% all were false, time to quit
	    lists:map(fun(X)->element(2,X) end,ParseRes);
	ParseRes -> %% all were true, continue with check
	    InputModules = lists:map(
			     fun(F)->
				     E = filename:extension(F),
				     B = filename:basename(F,E),
				     if
					 list(B) -> list_to_atom(B);
					 true -> B
				     end
			     end,
			     Files),
	    CheckRes = check_set(ParseRes,SetBase,OutFile,Includes,
				 EncodingRule,DbFile,Options,InputModules);
	Other ->
	    {error,{'unexpected error in scan/parse phase',
		    lists:map(fun(X)->element(3,X) end,Other)}}
    end.

check_set(ParseRes,SetBase,OutFile,Includes,EncRule,DbFile,
	  Options,InputModules) ->
    lists:foreach(fun({T,M,File})->
			  cmp(M#module.name,File)
		  end,
		  ParseRes),
    MergedModule = merge_modules(ParseRes,SetBase),
    SetM = MergedModule#module{name=SetBase},
    Continue1 = check({true,SetM},SetBase,OutFile,Includes,EncRule,DbFile,
		      Options,InputModules),
    Continue2 = generate(Continue1,OutFile,EncRule,Options),
    compile_erl(Continue2,OutFile,Options).


%% merge/1 -> returns a module record where the typeorval lists are merged,
%% the exports lists are merged, the imports lists are merged when the 
%% elements come from other modules than the merge set, the tagdefault 
%% field gets the shared value if all modules have same tagging scheme,
%% otherwise an tagging_error exception is thrown, 
%% the extensiondefault ...(not handled yet).
merge_modules(ParseRes,CommonName) ->
    ModuleList = lists:map(fun(X)->element(2,X) end,ParseRes),
    TypeOrVal = 
	lists:append(lists:map(fun(X)->X#module.typeorval end,ModuleList)),
    InputMNameList = lists:map(fun(X)->X#module.name end,ModuleList),
    CExports = common_exports(ModuleList),
    ImportsModuleNameList = lists:map(fun(X)->
					      {X#module.imports,
					       X#module.name} end,
				      ModuleList),
    CImports = common_imports(ImportsModuleNameList,InputMNameList),
    TagDefault = common_tagdefault(ModuleList),
    #module{name=CommonName,tagdefault=TagDefault,exports=CExports,
	    imports=CImports,typeorval=TypeOrVal}.

common_exports(ModuleList) ->
    %% if all modules exports 'all' then export 'all', 
    %% otherwise export each typeorval name
    case lists:filter(fun(X)->
			      element(2,X#module.exports) /= all
		      end,
		      ModuleList) of
	[]->
	    {exports,all};
	ModsWithExpList ->
	    CExports1 = 
		lists:append(lists:map(fun(X)->element(2,X#module.exports) end,
				       ModsWithExpList)),
	    CExports2 = export_all(lists:subtract(ModuleList,ModsWithExpList)),
	    {exports,CExports1++CExports2}
    end.

export_all([])->[];
export_all(ModuleList) ->
    ExpList =
	lists:map(
	  fun(M)->
		  TorVL=M#module.typeorval,
		  RefL = 
		      lists:map(
			fun(Def)->
				case Def of
				    T when record(T,typedef)->
					#'Externaltypereference'{pos=0,
								 type=T#typedef.name};
				    V when record(V,valuedef) ->
					#'Externalvaluereference'{pos=0,
								  value=V#valuedef.name};
				    C when record(C,classdef) ->
					#'Externaltypereference'{pos=0,
								 type=C#classdef.name};
				    P when record(P,ptypedef) ->
					#'Externaltypereference'{pos=0,
								 type=P#ptypedef.name}
				end
			end,
			TorVL)
	  end,
	  ModuleList),
    lists:append(ExpList).

common_imports(IList,InputMNameL) ->
    SetExternalImportsList = remove_in_set_imports(IList,InputMNameL,[]),
    {imports,remove_import_doubles(SetExternalImportsList)}.

common_tagdefault(ModList) ->
    Mod = lists:last(ModList),
    TagDef = Mod#module.tagdefault, 
    case lists:filter(fun(M)->
			      M#module.tagdefault /= TagDef
		      end,
		      ModList) of
	[] ->
	    TagDef;
	_ -> throw({tagging_error,{'all modules must have same tagging scheme'}})
    end.

%% remove_in_set_imports/3 :
%% input: list with tuples of each module's imports and module name 
%% respectively.
%% output: one list with same format but each occured import from a
%% module in the input set (IMNameL) is removed.
remove_in_set_imports([{{imports,ImpL},ModName}|Rest],InputMNameL,Acc) ->
    NewImpL = remove_in_set_imports1(ImpL,InputMNameL,[]),
    remove_in_set_imports(Rest,InputMNameL,NewImpL++Acc);
remove_in_set_imports([],_,Acc) ->
    lists:reverse(Acc).

remove_in_set_imports1([I|Is],InputMNameL,Acc) ->
    case I#'SymbolsFromModule'.module of
	#'Externaltypereference'{type=MName} ->
	    case lists:member(MName,InputMNameL) of
		true ->
		    remove_in_set_imports1(Is,InputMNameL,Acc);
		false ->
		    remove_in_set_imports1(Is,InputMNameL,[I|Acc])
	    end;
	Other ->
	    remove_in_set_imports1(Is,InputMNameL,[I|Acc])
    end;
remove_in_set_imports1([],_,Acc) ->
    lists:reverse(Acc).

remove_import_doubles([]) ->
    [];
remove_import_doubles(ImportList) ->
    MergedImportList = 
	merge_symbols_from_module(lists:map(fun(X)->element(1,X) end, 
					    ImportList),[]),
    delete_double_of_symbol(MergedImportList,[]).

merge_symbols_from_module([Imp|Imps],Acc) ->
    #'Externaltypereference'{type=ModName} = Imp#'SymbolsFromModule'.module,
    IfromModName = 
	lists:filter(
	  fun(I)->
		  case I#'SymbolsFromModule'.module of
		      #'Externaltypereference'{type=ModName} ->
			  true;
		      #'Externalvaluereference'{value=ModName} ->
			  true;
		      _ -> false
		  end
	  end,
	  Imps),
    NewImps = lists:subtract(Imps,IfromModName),
    NewImp =
	Imp#'SymbolsFromModule'{
	  symbols = lists:append(
		      lists:map(fun(SL)->
					SL#'SymbolsFromModule'.symbols 
				end,[Imp,IfromModName]))},
    merge_symbols_from_module(NewImps,[NewImp|Acc]);
merge_symbols_from_module([],Acc) ->
    lists:reverse(Acc).

delete_double_of_symbol([I|Is],Acc) ->
    SymL=I#'SymbolsFromModule'.symbols,
    NewSymL = delete_double_of_symbol1(SymL,[]),
    delete_double_of_symbol(Is,[I#'SymbolsFromModule'{symbols=NewSymL}|Acc]);
delete_double_of_symbol([],Acc) ->
    Acc.

delete_double_of_symbol1([TRef={typereference,_,TrefName}|Rest],Acc)->
    NewRest = 
	lists:filter(fun(S)->
			     case S of
				 #'Externaltypereference'{type=TrefName}->
				     false;
				 _ -> true
			     end
		     end,
		     Rest),
    delete_double_of_symbol1(NewRest,[TRef|Acc]);
delete_double_of_symbol1([VRef={identifier,_,VName}|Rest],Acc) ->
    NewRest = 
	lists:filter(fun(S)->
			     case S of
				 #'Externalvaluereference'{value=VName}->
				     false;
				 _ -> true
			     end
		     end,
		     Rest),
    delete_double_of_symbol1(NewRest,[VRef|Acc]);
delete_double_of_symbol1([TRef={#'Externaltypereference'{type=MRef},
				#'Externaltypereference'{type=TRef}}|Rest],
			 Acc)->
    NewRest = 
	lists:filter(
	  fun(S)->
		  case S of
		      {#'Externaltypereference'{type=MRef},
		       #'Externaltypereference'{type=TRef}}->
			  false;
		      _ -> true
		  end
	  end,
	  Rest),
    delete_double_of_symbol1(NewRest,[TRef|Acc]);
delete_double_of_symbol1([],Acc) ->
    Acc.


scan_set(DirName,Files,Options) ->
    lists:map(
      fun(F)->
	      case scan({true,true},filename:join([DirName,F]),Options) of
		  {false,{error,Reason}} ->
		      throw({error,{'scan error in file:',F,Reason}});
		  {TrueOrFalse,Res} ->
		      {TrueOrFalse,Res,F}
	      end
      end,
      Files).

parse_set(ScanRes,Options) ->
    lists:map(
      fun({TorF,Toks,F})->
	      case parse({TorF,Toks},F,Options) of
		  {false,{error,Reason}} ->
		      throw({error,{'parse error in file:',F,Reason}});
		  {TrueOrFalse,Res} ->
		      {TrueOrFalse,Res,F}
	      end
      end,
      ScanRes).


%%***********************************


scan({true,_}, File,Options) ->
    case asn1ct_tok:file(File) of
	{error,Reason} ->
	    io:format("~p~n",[Reason]),
	    {false,{error,Reason}};
        Tokens ->
	    case lists:member(ss,Options) of
		true -> % we terminate after scan
		    {false,Tokens};
		false -> % continue with next pass
		    {true,Tokens}
	    end
    end;
scan({false,Result},_,_) ->
    Result.


parse({true,Tokens},File,Options) ->
    %Presult = asn1ct_parser2:parse(Tokens),
    %%case lists:member(p1,Options) of
    %%		  true ->
    %%		      asn1ct_parser:parse(Tokens);
    %%		  _ ->
    %%		      asn1ct_parser2:parse(Tokens)
    %%	      end,
    case catch asn1ct_parser2:parse(Tokens) of
	{error,{{Line,Mod,Message},TokTup}} ->
	    if 
		integer(Line) ->
		    BaseName = filename:basename(File),
		    io:format("syntax error at line ~p in module ~s:~n",
			      [Line,BaseName]);
		true ->
		    io:format("syntax error in module ~p:~n",[File])
	    end,
	    print_error_message(Message),
	    {false,{error,Message}};
	{error,{Line,Mod,[Message,Token]}} ->
	    io:format("syntax error: ~p ~p at line ~p~n",
		      [Message,Token,Line]),
	    {false,{error,{Line,[Message,Token]}}};
	{ok,M} ->
	    case lists:member(sp,Options) of
		true -> % terminate after parse
		    {false,M};
		false -> % continue with next pass
		    {true,M}
	    end;
	OtherError ->
	    io:format("~p~n",[OtherError])
    end;
parse({false,Tokens},_,_) ->
    {false,Tokens}.

check({true,M},File,OutFile,Includes,EncodingRule,DbFile,Options,InputMods) ->
    cmp(M#module.name,File),
    start(["."|Includes]),
    case asn1ct_check:storeindb(M) of 
	ok   ->
	    Module = asn1_db:dbget(M#module.name,'MODULE'),
	    State = #state{mname=Module#module.name,
			   module=Module#module{typeorval=[]},
			   erule=EncodingRule,
			   inputmodules=InputMods,
			   options=Options},
	    Check = asn1ct_check:check(State,Module#module.typeorval),
	    case {Check,lists:member(abs,Options)} of
		{{error,Reason},_} ->
		    {false,{error,Reason}};
		{{ok,NewTypeOrVal,_},true} ->
		    NewM = Module#module{typeorval=NewTypeOrVal},
		    asn1_db:dbput(NewM#module.name,'MODULE',NewM),
		    pretty2(M#module.name,lists:concat([OutFile,".abs"])),
		    {false,ok};
		{{ok,NewTypeOrVal,GenTypeOrVal},_} ->
		    NewM = Module#module{typeorval=NewTypeOrVal},
		    asn1_db:dbput(NewM#module.name,'MODULE',NewM),
		    asn1_db:dbsave(DbFile,M#module.name),
		    io:format("--~p--~n",[{generated,DbFile}]),
		    {true,{M,NewM,GenTypeOrVal}}
	    end
    end;
check({false,M},_,_,_,_,_,_,_) ->
    {false,M}.

generate({true,{M,Module,GenTOrV}},OutFile,EncodingRule,Options) ->
    debug_on(Options),
    case lists:member(compact_bit_string,Options) of
	true -> put(compact_bit_string,true);
	_ -> ok
    end,
    put(encoding_options,Options),
    ets:new(check_functions,[named_table]),
    asn1ct_gen:pgen(OutFile,EncodingRule,M#module.name,GenTOrV),
    debug_off(Options),
    put(compact_bit_string,false),
    erase(encoding_options),
    ets:delete(check_functions),
    case lists:member(sg,Options) of
	true -> % terminate here , with .erl file generated
	    {false,true};
	false ->
	    {true,true}
    end;
generate({false,M},_,_,_) ->
    {false,M}.

compile_erl({true,_},OutFile,Options) ->
    erl_compile(OutFile,Options);
compile_erl({false,true},_,_) ->
    ok;
compile_erl({false,Result},_,_) ->
    Result.

input_file_type([]) ->
    {empty_name,[]};
input_file_type(File) ->
    case filename:extension(File) of
	[] ->
	    case file:read_file_info(lists:concat([File,".asn1"])) of
		{ok,FileInfo} ->
		    {single_file, lists:concat([File,".asn1"])};
		_Error ->
		    case file:read_file_info(lists:concat([File,".asn"])) of
			{ok,FileInfo} ->
			    {single_file, lists:concat([File,".asn"])};
			_Error ->
			    {single_file, lists:concat([File,".py"])}
		    end
	    end;
	Asn1PFix ->
	    Base = filename:basename(File,Asn1PFix),
	    case filename:extension(Base) of
		[] ->
		    {single_file,File};
		SetPFix when (SetPFix == ".set") ->
		    {multiple_files_file,
		     filename:basename(Base,SetPFix),
		     File};
		Error ->
		    throw({input_file_error,{'Bad input file',File}})
	    end
    end.

get_file_list(File) ->
    case file:open(File,read) of
	{error,Reason} ->
	    {error,{File,file:format_error(Reason)}};
	{ok,Stream} ->
	    get_file_list1(Stream,[])
    end.

get_file_list1(Stream,Acc) ->
    Ret = io:get_line(Stream,''),
    case Ret of
	eof ->
	    file:close(Stream),
	    lists:reverse(Acc);
	FileName ->
	    PrefixedNameList =
		case (catch input_file_type(lists:delete($\n,FileName))) of
		    {empty_name,[]} -> [];
		    {single_file,Name} -> [Name];
		    {multiple_files_file,Name} ->
			get_file_list(Name);
		    Err = {input_file_error,Reason} ->
			throw(Err)
		end,
	    get_file_list1(Stream,PrefixedNameList++Acc)
    end.

get_rule(Options) ->
    case [Rule ||Rule <-[per,ber,ber_bin,per_bin],% added ber_bin
		 Opt <- Options,
		 Rule==Opt] of
	[Rule] ->
	    Rule;
	[Rule|T] ->
	    Rule;
	[] ->
	    ber
    end.

erl_compile(OutFile,Options) ->
    case lists:member(noobj,Options) of
	true ->
	    ok;
	_ ->
	    ErlOpt = case lists:keysearch(outdir, 1, Options) of
			 {value, {outdir, Odir}} -> [{outdir,Odir}]; 
			 _ -> []
		     end,
	    ErlOpt2 = case lists:member(native,Options) of
			  true -> [native];
			  _ -> []
		      end,
	    ErlOpt3 = case lists:keysearch(hipe,1,Options) of
			  {value, HipeFlag} -> [HipeFlag];
			  _ -> []
		      end,
	    case c:c(OutFile,ErlOpt++ErlOpt2++ErlOpt3) of
		{ok,Module} ->
		    ok;
		_ ->
		    {error,'no_compilation'}
	    end
    end.

debug_on(Options) ->
    case lists:member(debug,Options) of
	true ->
	    put(asndebug,true);
	_ ->
	    true
    end,
    case lists:member(keyed_list,Options) of
	true ->
	    put(asn_keyed_list,true);
	_ ->
	    true
    end.


debug_off(Options) ->
    erase(asndebug),
    erase(asn_keyed_list).


outfile(Base, Ext, Opts) when atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case lists:keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		Other -> Base			% Not found or bad format
	    end,
    case Ext of
	[] ->
	    Obase;
	_ ->
	    Obase++"."++Ext
    end.

%% compile(AbsFileName, Options)
%%   Compile entry point for erl_compile.

compile_asn(File,OutFile,Options) ->
    compile(lists:concat([File,".asn"]),OutFile,Options).

compile_asn1(File,OutFile,Options) ->
    compile(lists:concat([File,".asn1"]),OutFile,Options).

compile_py(File,OutFile,Options) ->
    compile(lists:concat([File,".py"]),OutFile,Options).

compile(File, _OutFile, Options) ->
    case catch compile(File, make_erl_options(Options)) of
	Exit = {'EXIT',Reason} ->
	    io:format("~p~n~s~n",[Exit,"error"]),
	    error;
	{error,Reason} ->
	    %% case occurs due to error in asn1ct_parser2,asn1ct_check
%%	    io:format("~p~n",[Reason]),
%%	    io:format("~p~n~s~n",[Reason,"error"]),
	    error;
	ok -> 
	    io:format("ok~n"),
	    ok;
	ParseRes when tuple(ParseRes) ->
	    io:format("~p~n",[ParseRes]),
	    ok;
	ScanRes when list(ScanRes) ->
	    io:format("~p~n",[ScanRes]),
	    ok;
	Unknown -> 
	    io:format("~p~n~s~n",[Unknown,"error"]),
	    error
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
%%%	case Warning of
%%%	    0 -> [];
%%%	    _ -> [report_warnings]
%%%	end ++
	[] ++
	case Optimize of
	    0 -> [];
	    _ -> [fast]
	end ++
	lists:map(
	  fun ({Name, Value}) ->
		  {d, Name, Value};
	      (Name) ->
		  {d, Name}
	  end,
	  Defines) ++
	case OutputType of
	    undefined -> [ber]; % temporary default (ber when it's ready)
	    ber -> [ber];
	    ber_bin -> [ber_bin];
	    per -> [per];
	    per_bin -> [per_bin]
	end,

    Options++[report_errors, {cwd, Cwd}, {outdir, Outdir}|
	      lists:map(fun(Dir) -> {i, Dir} end, Includes)]++Specific.

pretty2(Module,AbsFile) ->
    start(),
    {ok,F} = file:open(AbsFile,write),
    M = asn1_db:dbget(Module,'MODULE'),
    io:format(F,"%%%%%%%%%%%%%%%%%%%   ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    io:format(F,"~s\n",[asn1ct_pretty_format:term(M#module.defid)]),
    io:format(F,"~s\n",[asn1ct_pretty_format:term(M#module.tagdefault)]),
    io:format(F,"~s\n",[asn1ct_pretty_format:term(M#module.exports)]),
    io:format(F,"~s\n",[asn1ct_pretty_format:term(M#module.imports)]),
    io:format(F,"~s\n\n",[asn1ct_pretty_format:term(M#module.extensiondefault)]),

    {Types,Values,ParameterizedTypes,Classes,Objects,ObjectSets} = M#module.typeorval,
    io:format(F,"%%%%%%%%%%%%%%%%%%% TYPES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Types),
    io:format(F,"%%%%%%%%%%%%%%%%%%% VALUES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Values),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Parameterized Types in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,ParameterizedTypes),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Classes in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,Classes),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Objects in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,Objects),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Object Sets in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])			   
		  end,ObjectSets).
start() ->
    Includes = ["."],
    start(Includes).


start(Includes) when list(Includes) ->
    asn1_db:dbstart(Includes).

stop() ->
    save(),
    asn1_db:stop_server(ns),
    asn1_db:stop_server(rand),
    stopped.

save() ->
    asn1_db:dbstop().

%%clear() ->
%%    asn1_db:dbclear().

encode(Module,Term) ->
    asn1rt:encode(Module,Term).

encode(Module,Type,Term) when list(Module) ->
    asn1rt:encode(list_to_atom(Module),Type,Term);
encode(Module,Type,Term) ->
    asn1rt:encode(Module,Type,Term).

decode(Module,Type,Bytes) when list(Module) ->
    asn1rt:decode(list_to_atom(Module),Type,Bytes);
decode(Module,Type,Bytes) ->
    asn1rt:decode(Module,Type,Bytes).


test(Module) ->
    start(),
    M = asn1_db:dbget(Module,'MODULE'),
    {Types,Values,Ptypes,Classes,Objects,ObjectSets} = M#module.typeorval,
    test_each(Module,Types).

test_each(Module,[Type | Rest]) ->
    case test(Module,Type) of
	{ok,Result} ->
	    test_each(Module,Rest);
	Error ->
	    Error
    end;
test_each(Module,[]) ->
    ok.

test(Module,Type) ->
    io:format("~p:~p~n",[Module,Type]),
    case (catch value(Module,Type)) of 
	{ok,Val} -> 
	    %%	    io:format("asn1ct:test/2: ~w~n",[Val]),
	    test(Module,Type,Val);
	{'EXIT',Reason} -> 
	    {error,{asn1,{value,Reason}}}
    end.


test(Module,Type,Value) ->
    case catch encode(Module,Type,Value) of
	{ok,Bytes} ->
	    %%	    io:format("test 1: ~p~n",[{Bytes}]),
	    M = if 
		    list(Module) ->
			list_to_atom(Module);
		    true ->
			Module
		end,
	    NewBytes = 
		case M:encoding_rule() of
		    ber ->
			lists:flatten(Bytes);
		    ber_bin when binary(Bytes) ->
			Bytes;
		    ber_bin ->
			list_to_binary(Bytes);
		    per ->
			lists:flatten(Bytes);
		    per_bin when binary(Bytes) ->
			Bytes;
		    per_bin ->
			list_to_binary(Bytes)
		end,
	    case decode(Module,Type,NewBytes) of
		{ok,Value} -> 
		    {ok,{Module,Type,Value}};
		{ok,Res} -> 
		    {error,{asn1,{encode_decode_mismatch,
				  {{Module,Type,Value},Res}}}};
		Error -> 
		    {error,{asn1,{{decode,
				   {Module,Type,Value},Error}}}}
	    end;
	Error ->
	    {error,{asn1,{encode,{{Module,Type,Value},Error}}}}
    end.

value(Module) ->
    start(),
    M = asn1_db:dbget(Module,'MODULE'),
    {Types,Values,Ptypes,Classes,Objects,ObjectSets} = M#module.typeorval,
    lists:map(fun(A) ->value(Module,A) end,Types).

value(Module,Type) ->
    start(),
    case catch asn1ct_value:get_type(Module,Type,no) of
	{error,Reason} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,Reason};
	Result ->
	    {ok,Result}
    end.

cmp(Module,InFile) ->
    Base = filename:basename(InFile),
    Dir = filename:dirname(InFile),
    Ext = filename:extension(Base),
    Finfo = file:read_file_info(InFile),
    Minfo = file:read_file_info(filename:join(Dir,lists:concat([Module,Ext]))),
    case Finfo of
	Minfo ->
	    ok;
	_ ->
	    io:format("asn1error: Modulename and filename must be equal~n",[]),
	    throw(error)
    end.

vsn() ->
    ?vsn.

print_error_message([got,H|T]) when list(H) ->
    io:format(" got:"),
    print_listing(H,"and"),
    print_error_message(T);
print_error_message([expected,H|T]) when list(H) ->
    io:format(" expected one of:"),
    print_listing(H,"or"),
    print_error_message(T);
print_error_message([H|T])  ->
    io:format(" ~p",[H]),
    print_error_message(T);
print_error_message([]) ->
    io:format("~n").

print_listing([H1,H2|[]],AndOr) ->
    io:format(" ~p ~s ~p",[H1,AndOr,H2]);
print_listing([H1,H2|T],AndOr) ->
    io:format(" ~p,",[H1]),
    print_listing([H2|T],AndOr);
print_listing([H],AndOr) ->
    io:format(" ~p",[H]);
print_listing([],_) ->
    ok.
