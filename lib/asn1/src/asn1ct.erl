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
-export([compile_asn/3,compile_asn1/3,compile_py/3,compile/3,value/1,vsn/0,
	 create_ets_table/2,get_name_of_def/1,get_pos_of_def/1]).
-include("asn1_records.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(unique_names,0).
-define(dupl_uniquedefs,1).
-define(dupl_equaldefs,2).
-define(dupl_eqdefs_uniquedefs,?dupl_equaldefs bor ?dupl_uniquedefs).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the interface to the compiler
%% 
%% 


compile(File) ->
    compile(File,[]).

compile(File,Options) when list(Options) ->
    Options1 =
	case {lists:member(optimize,Options),lists:member(ber_bin,Options)} of
	    {true,true} -> 
		[ber_bin_v2|Options--[ber_bin]];
	    _ -> Options
	end,
    case (catch input_file_type(File)) of
	{single_file,PrefixedFile} ->
	    (catch compile1(PrefixedFile,Options1));
	{multiple_files_file,SetBase,FileName} ->
	    FileList = get_file_list(FileName),
	    (catch compile_set(SetBase,filename:dirname(FileName),
			       FileList,Options1));
	Err = {input_file_error,_Reason} ->
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
    create_ets_table(asn1_functab,[named_table]),
    Continue1 = scan({true,true},File,Options),
    Continue2 = parse(Continue1,File,Options),
    Continue3 = check(Continue2,File,OutFile,Includes,EncodingRule,
		      DbFile,Options,[]),
    Continue4 = generate(Continue3,OutFile,EncodingRule,Options),
    delete_tables([asn1_functab]),
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
    create_ets_table(asn1_functab,[named_table]),
    ScanRes = scan_set(DirName,Files,Options),
    ParseRes = parse_set(ScanRes,Options),
    Result = 
	case [X||X <- ParseRes,element(1,X)==true] of
	    [] -> %% all were false, time to quit
		lists:map(fun(X)->element(2,X) end,ParseRes);
	    ParseRes -> %% all were true, continue with check
		InputModules = 
		    lists:map(
		      fun(F)->
			      E = filename:extension(F),
			      B = filename:basename(F,E),
			      if
				  list(B) -> list_to_atom(B);
				  true -> B
			      end
		      end,
		      Files),
		check_set(ParseRes,SetBase,OutFile,Includes,
			  EncodingRule,DbFile,Options,InputModules);
	    Other ->
		{error,{'unexpected error in scan/parse phase',
			lists:map(fun(X)->element(3,X) end,Other)}}
	end,
    delete_tables([asn1_functab]),
    Result.

check_set(ParseRes,SetBase,OutFile,Includes,EncRule,DbFile,
	  Options,InputModules) ->
    lists:foreach(fun({_T,M,File})->
			  cmp(M#module.name,File)
		  end,
		  ParseRes),
    MergedModule = merge_modules(ParseRes,SetBase),
    SetM = MergedModule#module{name=SetBase},
    Continue1 = check({true,SetM},SetBase,OutFile,Includes,EncRule,DbFile,
		      Options,InputModules),
    Continue2 = generate(Continue1,OutFile,EncRule,Options),

    delete_tables([renamed_defs,original_imports,automatic_tags]),

    compile_erl(Continue2,OutFile,Options).

delete_tables([Table|Ts]) ->
    case ets:info(Table) of
	undefined -> ok;
	_ -> ets:delete(Table)
    end,
    delete_tables(Ts);
delete_tables([]) ->
    ok.

%% merge_modules/2 -> returns a module record where the typeorval lists are merged,
%% the exports lists are merged, the imports lists are merged when the 
%% elements come from other modules than the merge set, the tagdefault 
%% field gets the shared value if all modules have same tagging scheme,
%% otherwise a tagging_error exception is thrown, 
%% the extensiondefault ...(not handled yet).
merge_modules(ParseRes,CommonName) ->
    ModuleList = lists:map(fun(X)->element(2,X) end,ParseRes),
    NewModuleList = remove_name_collisions(ModuleList),
    case ets:info(renamed_defs,size) of
	0 -> ets:delete(renamed_defs);
	_ -> ok
    end,
    save_imports(NewModuleList),
%    io:format("~p~n~p~n~p~n~n",[ets:lookup(original_imports,'M1'),ets:lookup(original_imports,'M2'),ets:tab2list(original_imports)]),
    TypeOrVal = lists:append(lists:map(fun(X)->X#module.typeorval end,
				       NewModuleList)),
    InputMNameList = lists:map(fun(X)->X#module.name end,
			       NewModuleList),
    CExports = common_exports(NewModuleList),
   
    ImportsModuleNameList = lists:map(fun(X)->
					      {X#module.imports,
					       X#module.name} end,
				      NewModuleList),
    %% ImportsModuleNameList: [{Imports,ModuleName},...]
    %% Imports is a tuple {imports,[#'SymbolsFromModule'{},...]}
    CImports = common_imports(ImportsModuleNameList,InputMNameList),
    TagDefault = check_tagdefault(NewModuleList),
    #module{name=CommonName,tagdefault=TagDefault,exports=CExports,
	    imports=CImports,typeorval=TypeOrVal}.

%% causes an exit if duplicate definition names exist in a module
remove_name_collisions(Modules) ->
    create_ets_table(renamed_defs,[named_table]),
    %% Name duplicates in the same module is not allowed.
    lists:foreach(fun exit_if_nameduplicate/1,Modules),
    %% Then remove duplicates in different modules and return the
    %% new list of modules.
    remove_name_collisions2(Modules,[]).

%% For each definition in the first module in module list, find
%% all definitons with same name and rename both definitions in
%% the first module and in rest of modules
remove_name_collisions2([M|Ms],Acc) ->
    TypeOrVal = M#module.typeorval,
    MName = M#module.name,
    %% Test each name in TypeOrVal on all modules in Ms
    {NewM,NewMs} = remove_name_collisions2(MName,TypeOrVal,Ms,[]),
    remove_name_collisions2(NewMs,[M#module{typeorval=NewM}|Acc]);
remove_name_collisions2([],Acc) ->
    finished_warn_prints(),
    Acc.

%% For each definition in list of defs find definitions in (rest of)
%% modules that have same name. If duplicate was found rename def.
%% Test each name in [T|Ts] on all modules in Ms
remove_name_collisions2(ModName,[T|Ts],Ms,Acc) ->
    Name = get_name_of_def(T),
    case discover_dupl_in_mods(Name,T,Ms,[],?unique_names) of
	{_,?unique_names} -> % there was no name collision
	    remove_name_collisions2(ModName,Ts,Ms,[T|Acc]);
	{NewMs,?dupl_uniquedefs} -> % renamed defs in NewMs
	    %% rename T
	    NewT = set_name_of_def(ModName,Name,T), %rename def
	    warn_renamed_def(ModName,get_name_of_def(NewT),Name),
	    ets:insert(renamed_defs,{get_name_of_def(NewT),Name,ModName}),
	    remove_name_collisions2(ModName,Ts,NewMs,[NewT|Acc]);
	{NewMs,?dupl_equaldefs} -> % name duplicates, but identical defs
	    %% keep name of T
	    warn_kept_def(ModName,Name),
	    remove_name_collisions2(ModName,Ts,NewMs,[T|Acc]);
	{NewMs,?dupl_eqdefs_uniquedefs} ->
	    %% keep name of T, renamed defs in NewMs
	    warn_kept_def(ModName,Name),
	    remove_name_collisions2(ModName,Ts,NewMs,[T|Acc])
    end;
remove_name_collisions2(_,[],Ms,Acc) ->
    {Acc,Ms}.

%% Name is the name of a definition. If a definition with the same name
%% is found in the modules Ms the definition will be renamed and returned.
discover_dupl_in_mods(Name,Def,[M=#module{name=N,typeorval=TorV}|Ms],
			      Acc,AnyRenamed) ->
    Fun = fun(T,RenamedOrDupl)->
		  case {get_name_of_def(T),compare_defs(Def,T)} of
		      {Name,not_equal} ->
			  %% rename def
			  NewT=set_name_of_def(N,Name,T),
			  warn_renamed_def(N,get_name_of_def(NewT),Name),
			  ets:insert(renamed_defs,{get_name_of_def(NewT),
						   Name,N}),
			  {NewT,?dupl_uniquedefs bor RenamedOrDupl};
		      {Name,equal} ->
			  %% delete def
			  warn_deleted_def(N,Name),
			  {[],?dupl_equaldefs bor RenamedOrDupl};
		      _ ->
			  {T,RenamedOrDupl}
		  end
	  end,
    {NewTorV,NewAnyRenamed} = lists:mapfoldl(Fun,AnyRenamed,TorV),
    %% have to flatten the NewTorV to remove any empty list elements
    discover_dupl_in_mods(Name,Def,Ms,
			  [M#module{typeorval=lists:flatten(NewTorV)}|Acc],
			  NewAnyRenamed);
discover_dupl_in_mods(_,_,[],Acc,AnyRenamed) ->
    {Acc,AnyRenamed}.

warn_renamed_def(ModName,NewName,OldName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has been renamed in generated module. New name is ~p.~n",[ModName,OldName,NewName]).

warn_deleted_def(ModName,DefName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has been deleted in generated module.~n",[ModName,DefName]).

warn_kept_def(ModName,DefName) ->
    maybe_first_warn_print(),
    io:format("NOTICE: The ASN.1 definition in module ~p with name ~p has kept its name due to equal definition as duplicate.~n",[ModName,DefName]).

maybe_first_warn_print() ->
    case get(warn_duplicate_defs) of
	undefined ->
	    put(warn_duplicate_defs,true),
	    io:format("~nDue to multiple occurrences of a definition name in "
		      "multi-file compiled files:~n");
	_ ->
	    ok
    end.
finished_warn_prints() ->
    put(warn_duplicate_defs,undefined).


exit_if_nameduplicate(#module{typeorval=TorV}) ->
    exit_if_nameduplicate(TorV);
exit_if_nameduplicate([]) ->
    ok;
exit_if_nameduplicate([Def|Rest]) ->
    Name=get_name_of_def(Def),
    exit_if_nameduplicate2(Name,Rest),
    exit_if_nameduplicate(Rest).

exit_if_nameduplicate2(Name,Rest) ->
    Pred=fun(Def)->
		 case get_name_of_def(Def) of
		     Name -> true;
		     _ -> false
		 end
	 end,
        case lists:any(Pred,Rest) of
	true ->
	    throw({error,{"more than one definition with same name",Name}});
	_ ->
	    ok
    end.

compare_defs(D1,D2) ->
    compare_defs2(unset_pos(D1),unset_pos(D2)).
compare_defs2(D,D) ->
    equal;
compare_defs2(_,_) ->
    not_equal.

unset_pos(Def) when record(Def,typedef) ->
    Def#typedef{pos=undefined};
unset_pos(Def) when record(Def,classdef) ->
    Def#classdef{pos=undefined};
unset_pos(Def) when record(Def,valuedef) ->
    Def#valuedef{pos=undefined};
unset_pos(Def) when record(Def,ptypedef) ->
    Def#ptypedef{pos=undefined};
unset_pos(Def) when record(Def,pvaluedef) ->
    Def#pvaluedef{pos=undefined};
unset_pos(Def) when record(Def,pvaluesetdef) ->
    Def#pvaluesetdef{pos=undefined};
unset_pos(Def) when record(Def,pobjectdef) ->
    Def#pobjectdef{pos=undefined};
unset_pos(Def) when record(Def,pobjectsetdef) ->
    Def#pobjectsetdef{pos=undefined}.

get_pos_of_def(#typedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#classdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#valuedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#ptypedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pvaluedef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pvaluesetdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pobjectdef{pos=Pos}) ->
    Pos;
get_pos_of_def(#pobjectsetdef{pos=Pos}) ->
    Pos.
    
    
get_name_of_def(#typedef{name=Name}) ->
    Name;
get_name_of_def(#classdef{name=Name}) ->
    Name;
get_name_of_def(#valuedef{name=Name}) ->
    Name;
get_name_of_def(#ptypedef{name=Name}) ->
    Name;
get_name_of_def(#pvaluedef{name=Name}) ->
    Name;
get_name_of_def(#pvaluesetdef{name=Name}) ->
    Name;
get_name_of_def(#pobjectdef{name=Name}) ->
    Name;
get_name_of_def(#pobjectsetdef{name=Name}) ->
    Name.

set_name_of_def(ModName,Name,OldDef) ->
    NewName = list_to_atom(lists:concat([Name,ModName])),
    case OldDef of
	#typedef{} -> OldDef#typedef{name=NewName};
	#classdef{} -> OldDef#classdef{name=NewName};
	#valuedef{} -> OldDef#valuedef{name=NewName};
	#ptypedef{} -> OldDef#ptypedef{name=NewName};
	#pvaluedef{} -> OldDef#pvaluedef{name=NewName};
	#pvaluesetdef{} -> OldDef#pvaluesetdef{name=NewName};
	#pobjectdef{} -> OldDef#pobjectdef{name=NewName};
	#pobjectsetdef{} -> OldDef#pobjectsetdef{name=NewName}
    end.

save_imports(ModuleList)->
    Fun = fun(M) ->
		  case M#module.imports of
		      {_,[]} -> [];
		      {_,I} -> 
			  {M#module.name,I}
		  end
	  end,
    ImportsList = lists:map(Fun,ModuleList),
    case lists:flatten(ImportsList) of
	[] ->
	    ok;
	ImportsList2 ->
	    create_ets_table(original_imports,[named_table]),
	    ets:insert(original_imports,ImportsList2)
    end.
				    
	    
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
							     type=P#ptypedef.name};
				PV when record(PV,pvaluesetdef) ->
				    #'Externaltypereference'{pos=0,
							     type=PV#pvaluesetdef.name};
				PO when record(PO,pobjectdef) ->
				    #'Externalvaluereference'{pos=0,
							      value=PO#pobjectdef.name}
			    end
		    end,
		    TorVL)
	  end,
	  ModuleList),
    lists:append(ExpList).

%% common_imports/2
%% IList is a list of tuples, {Imports,MName}, where Imports is the imports of
%% the module with name MName.
%% InputMNameL holds the names of all merged modules.
%% Returns an import tuple with a list of imports that are external the merged
%% set of modules.
common_imports(IList,InputMNameL) ->
    SetExternalImportsList = remove_in_set_imports(IList,InputMNameL,[]),
    {imports,remove_import_doubles(SetExternalImportsList)}.

check_tagdefault(ModList) ->
    case have_same_tagdefault(ModList) of
	{true,TagDefault}  -> TagDefault;
	{false,TagDefault} ->
	    create_ets_table(automatic_tags,[named_table]),
	    save_automatic_tagged_types(ModList),
	    TagDefault
    end.

have_same_tagdefault([#module{tagdefault=T}|Ms]) ->
    have_same_tagdefault(Ms,{true,T}).

have_same_tagdefault([],TagDefault) ->
    TagDefault;
have_same_tagdefault([#module{tagdefault=T}|Ms],TDefault={_,T}) ->
    have_same_tagdefault(Ms,TDefault);
have_same_tagdefault([#module{tagdefault=T1}|Ms],{_,T2}) ->
    have_same_tagdefault(Ms,{false,rank_tagdef([T1,T2])}).

rank_tagdef(L) ->
    case lists:member('EXPLICIT',L) of
	true -> 'EXPLICIT';
	_ -> 'IMPLICIT'
    end.

save_automatic_tagged_types([])->
    done;
save_automatic_tagged_types([#module{tagdefault='AUTOMATIC',
				     typeorval=TorV}|Ms]) ->
    Fun =
	fun(T) ->
		ets:insert(automatic_tags,{get_name_of_def(T)})
	end,
    lists:foreach(Fun,TorV),
    save_automatic_tagged_types(Ms);
save_automatic_tagged_types([_M|Ms]) ->
    save_automatic_tagged_types(Ms).

%% remove_in_set_imports/3 :
%% input: list with tuples of each module's imports and module name 
%% respectively.
%% output: one list with same format but each occured import from a
%% module in the input set (IMNameL) is removed.
remove_in_set_imports([{{imports,ImpL},_ModName}|Rest],InputMNameL,Acc) ->
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
	_ ->
	    remove_in_set_imports1(Is,InputMNameL,[I|Acc])
    end;
remove_in_set_imports1([],_,Acc) ->
    lists:reverse(Acc).

remove_import_doubles([]) ->
    [];
%% If several modules in the merge set imports symbols from
%% the same external module it might be doubled.
%% ImportList has #'SymbolsFromModule' elements
remove_import_doubles(ImportList) ->
    MergedImportList = 
	merge_symbols_from_module(ImportList,[]),
%%    io:format("MergedImportList: ~p~n",[MergedImportList]),
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
%%    io:format("Imp: ~p~nIfromModName: ~p~n",[Imp,IfromModName]),
    NewImp =
	Imp#'SymbolsFromModule'{
	  symbols = lists:append(
		      lists:map(fun(SL)->
					SL#'SymbolsFromModule'.symbols 
				end,[Imp|IfromModName]))},
    merge_symbols_from_module(NewImps,[NewImp|Acc]);
merge_symbols_from_module([],Acc) ->
    lists:reverse(Acc).

delete_double_of_symbol([I|Is],Acc) ->
    SymL=I#'SymbolsFromModule'.symbols,
    NewSymL = delete_double_of_symbol1(SymL,[]),
    delete_double_of_symbol(Is,[I#'SymbolsFromModule'{symbols=NewSymL}|Acc]);
delete_double_of_symbol([],Acc) ->
    Acc.

delete_double_of_symbol1([TRef=#'Externaltypereference'{type=TrefName}|Rest],Acc)->
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
delete_double_of_symbol1([VRef=#'Externalvaluereference'{value=VName}|Rest],Acc) ->
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
	{error,{{Line,_Mod,Message},_TokTup}} ->
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
	{error,{Line,_Mod,[Message,Token]}} ->
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

generate({true,{M,_Module,GenTOrV}},OutFile,EncodingRule,Options) ->
    debug_on(Options),
    case lists:member(compact_bit_string,Options) of
	true -> put(compact_bit_string,true);
	_ -> ok
    end,
    put(encoding_options,Options),
    create_ets_table(check_functions,[named_table]),
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
		{ok,_FileInfo} ->
		    {single_file, lists:concat([File,".asn1"])};
		_Error ->
		    case file:read_file_info(lists:concat([File,".asn"])) of
			{ok,_FileInfo} ->
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
		_Error ->
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
		    Err = {input_file_error,_Reason} ->
			throw(Err)
		end,
	    get_file_list1(Stream,PrefixedNameList++Acc)
    end.

get_rule(Options) ->
    case [Rule ||Rule <-[per,ber,ber_bin,ber_bin_v2,per_bin],
		 Opt <- Options,
		 Rule==Opt] of
	[Rule] ->
	    Rule;
	[Rule|_] ->
	    Rule;
	[] ->
	    ber
    end.

erl_compile(OutFile,Options) ->
%    io:format("Options:~n~p~n",[Options]),
    case lists:member(noobj,Options) of
	true ->
	    ok;
	_ ->
	    ErlOptions = remove_asn_flags(Options),
	    case c:c(OutFile,ErlOptions) of
		{ok,_Module} ->
		    ok;
		_ ->
		    {error,'no_compilation'}
	    end
    end.

remove_asn_flags(Options) ->
    [X || X <- Options,
	  X /= get_rule(Options),
	  X /= optimize,
	  X /= compact_bit_string,
	  X /= debug,
	  X /= keyed_list].
	  
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


debug_off(_Options) ->
    erase(asndebug),
    erase(asn_keyed_list).


outfile(Base, Ext, Opts) when atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case lists:keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		_NotFound -> Base % Not found or bad format
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
	Exit = {'EXIT',_Reason} ->
	    io:format("~p~n~s~n",[Exit,"error"]),
	    error;
	{error,_Reason} ->
	    %% case occurs due to error in asn1ct_parser2,asn1ct_check
%%	    io:format("~p~n",[_Reason]),
%%	    io:format("~p~n~s~n",[_Reason,"error"]),
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

    Includes = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
%%    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    Optimize = Opts#options.optimize,
    OutputType = Opts#options.output_type,
    Cwd = Opts#options.cwd,

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
	    1 -> [optimize];
	    999 -> [];
	    _ -> [{optimize,Optimize}]
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
	    ber_bin_v2 -> [ber_bin_v2];
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
    {Types,_Values,_Ptypes,_Classes,_Objects,_ObjectSets} = M#module.typeorval,
    test_each(Module,Types).

test_each(Module,[Type | Rest]) ->
    case test(Module,Type) of
	{ok,_Result} ->
	    test_each(Module,Rest);
	Error ->
	    Error
    end;
test_each(_,[]) ->
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
		    ber_bin_v2 when binary(Bytes) ->
			Bytes;
		    ber_bin_v2 ->
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
    {Types,_Values,_Ptypes,_Classes,_Objects,_ObjectSets} = M#module.typeorval,
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
print_listing([H],_AndOr) ->
    io:format(" ~p",[H]);
print_listing([],_) ->
    ok.


create_ets_table(Name,Options) when atom(Name) ->
    case ets:info(Name) of
	undefined ->
	    ets:new(Name,Options);
	_  ->
	    ets:delete(Name),
	    ets:new(Name,Options)
    end.
