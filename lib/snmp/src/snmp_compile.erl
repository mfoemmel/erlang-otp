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
-module(snmp_compile).

%%%-----------------------------------------------------------------
%%% Changes:
%%%    981217 - mbj added code to handle forward referneces in OIDs
%%%-----------------------------------------------------------------

%% API
-export([compile/1, compile/2]).

%% Debug
-export([look_at/1]).

%% Internal Exports
-export([init/3]).

-include("snmp_types.hrl").
-include("snmp_generic.hrl").
-include("snmp_compile.hrl").


%% Returns: {ok, File}|{error, Reason}
compile([AtomFilename]) when atom(AtomFilename) ->
    compile(atom_to_list(AtomFilename), []), %from cmd line
    halt();
compile(FileName) -> compile(FileName, []).

look_at(Mib) ->
    io:format("~p", [snmp_compile_lib:look_at(Mib)]).

%%----------------------------------------------------------------------
%% Options: {debug, Bool}                        Default: false
%%          {group_check, Bool}                           true
%%          {db, volatile|persistent|mnesia}              volatile
%%          {i, [import_dir_string()]}                    ["./"]
%%          {il, [import_lib_dir_string()]}               []
%%          {i, [import_dir_string()]}                    [""]
%%          {warnings, Bool}                              true
%%          {outdir, string()}                            "./"
%%----------------------------------------------------------------------

compile(FileName, Options) when list(FileName) ->
    true = snmp_misc:is_string(FileName),
    Opts = insert_default_options(Options, [{debug, false}, 
					    {group_check, true},
					    {i, ["./"]},
					    {db, volatile},
					    {warnings, true},
					    {outdir, "./"},
					    {il,[]}]),
    case check_options(Opts) of
	ok ->
	    Pid=spawn_link(snmp_compile,init,[self(),FileName,Opts]),
	    receive
		{mc_result,R} -> R;
		{'EXIT',Pid, Reason} when Reason =/= normal ->
		    exit(Reason)
	    end;
	{error, Reason} -> {error, Reason}
    end.

insert_default_options(Options, []) -> Options;
insert_default_options(Options, [{Key,DefVal}|DefaultOptions]) ->
    case snmp_misc:assq(Key, Options) of
	false ->
	    [{Key, DefVal} | insert_default_options(Options, DefaultOptions)];
	{value, Val} when Key == i ->
	    [{Key, Val++DefVal}
	     | insert_default_options(Options,DefaultOptions)];
	{value, Val} when Key == il ->
	    [{Key, Val++DefVal}
	     | insert_default_options(Options,DefaultOptions)];
	{value, Val} ->
	    [{Key, Val} | insert_default_options(Options,DefaultOptions)]
    end.

check_options([]) -> ok;
check_options([no_symbolic_info|T]) -> check_options(T);
check_options([{outdir, Str} | T]) when list(Str) ->
    check_options(T);
check_options([{debug, Atom} | T]) when atom(Atom) ->
    check_options(T);
check_options([{group_check, Atom} | T]) when atom(Atom) ->
    check_options(T);
check_options([{warnings, Atom} | T]) when atom(Atom) ->
    check_options(T);
check_options([{db, volatile} | T]) ->
    check_options(T);
check_options([{db, persistent} | T]) ->
    check_options(T);
check_options([{db, mnesia} | T]) ->
    check_options(T);
check_options([{i, [Str|_]} | T]) when list(Str) ->
    check_options(T);
check_options([{il, []} | T]) ->
    check_options(T);
check_options([{il, [Str|_]} | T]) when list(Str) ->
    check_options(T);
check_options([Opt|_]) ->
    {error, {invalid_option, Opt}}.


%%----------------------------------------------------------------------
%% The compile process.
%%----------------------------------------------------------------------
init(From,MibFileName,Options) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    put(options,Options),
    File = filename:rootname(MibFileName, ".mib"),
    put(filename, filename:basename(File ++ ".mib")),
    R = case catch c_impl(File) of
	    {ok, OutFile} -> {ok, OutFile};
	    {'EXIT',error} -> {error, compilation_failed};
	    Error -> exit(Error)
	end,
    From ! {mc_result,R}.


c_impl(File) ->
    P = snmp_parse(File),
    MibName = compile_parsed_data(P),
    save(File, MibName,get(options)).

compile_parsed_data({ok,{MibVer,MibName,{{import,Imports},Line},Definitions}})->
    snmp_compile_lib:import(Imports),
    definitions_loop(Definitions),
    MibName.


definitions_loop([{{object_type,ObjName,_Type,_Access,
		    _Kind,deprecated,_Index},Line}|T]) ->
    %% May be implemented but the compiler chooses not to.
    snmp_compile_lib:warning("~w is deprecated. Ignored.",[ObjName],Line),
    definitions_loop(T);

definitions_loop([{{object_type,ObjName,_Type,_Access,
		    _Kind,obsolete,_Index},Line}|T]) ->
    %% No need to implement a obsolete object
    ensure_macro_imported('OBJECT-TYPE', Line),
    definitions_loop(T);

%% Defining a table
definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline},
		  {{object_type,NameOfEntry,{{type,SeqName},TEline},
		    'not-accessible',
		    {table_entry,IndexingInfo},
		    Estatus,{NameOfTable,[1]}},Eline},
		  {{sequence,SeqName,{fieldList,FieldList}},Sline}|ColsEtc]) ->
    ensure_macro_imported('OBJECT-TYPE', Tline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmp_compile_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    TableME = #me{aliasname = NameOfTable,
		  entrytype = table, access = 'not-accessible'},
    snmp_compile_lib:register_oid(TEline,NameOfEntry,NameOfTable,[1]),
    TableEntryME = #me{aliasname = NameOfEntry, entrytype = table_entry,
		       assocList=[{table_entry_with_sequence,
				   SeqName}], 
		       access = 'not-accessible' },
    {ColMEs,RestObjs} = define_cols(ColsEtc,1,FieldList,NameOfEntry,
				    NameOfTable,[]),
    TableInfo = snmp_compile_lib:make_table_info(Eline, NameOfTable,
						 IndexingInfo,ColMEs),
    snmp_compile_lib:add_cdata(#cdata.mes, [TableEntryME,
			      TableME#me{assocList=[{table_info, TableInfo}]} |
				       ColMEs]),
    definitions_loop(RestObjs);

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline},
		  {{object_type,NameOfEntry,{{type,SeqName},_line},
		    'not-accessible',
		    {table_entry,IndexingInfo},
		    Estatus,BadOID},Eline},
		  {{sequence,SeqName,{fieldList,FieldList}},Sline}|ColsEtc]) ->
    ensure_macro_imported('OBJECT-TYPE', Tline),
    snmp_compile_lib:print_error("Bad TableEntry OID definition (~w)",
				 [BadOID],Eline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmp_compile_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    TableME = #me{aliasname = NameOfTable,
		  entrytype = table, access = 'not-accessible'},
    TableEntryME = #me{aliasname = NameOfEntry, entrytype = table_entry,
		       assocList=[{table_entry_with_sequence,
				   SeqName}], 
		       access = 'not-accessible' },
    {ColMEs,RestObjs} = define_cols(ColsEtc,1,FieldList,NameOfEntry,
				    NameOfTable,[]),
    TableInfo = snmp_compile_lib:make_table_info(Eline, NameOfTable,
						 IndexingInfo,ColMEs),
    snmp_compile_lib:add_cdata(#cdata.mes, [TableEntryME,
			      TableME#me{assocList=[{table_info, TableInfo}]} |
				       ColMEs]),
    definitions_loop(RestObjs);

definitions_loop([{{new_type,Macro,NewTypeName,OldType},Line}|T]) ->
    ensure_macro_imported(Macro,Line),
    Types = (get(cdata))#cdata.asn1_types,
    case lists:keysearch(NewTypeName, #asn1_type.aliasname, Types) of
	{value,_} ->
	    snmp_compile_lib:print_error("Type ~w already defined.",
			[NewTypeName],Line);
	false ->
	    NameOfOldType = element(2,OldType),
	    ASN1 = snmp_compile_lib:make_ASN1type(OldType),
	    snmp_compile_lib:add_cdata(#cdata.asn1_types,
				       [ASN1#asn1_type{aliasname=NewTypeName,
						       imported = false}])
    end,
    definitions_loop(T);

%% Plain variable
definitions_loop([{{object_type,NewVarName,
		    Type,Access,{variable,DefVal},
		    Status,{FatherName,SubIndex}},Line} |T]) ->
    snmp_compile_lib:test_father(FatherName, NewVarName, SubIndex, Line),
    ASN1type = snmp_compile_lib:make_ASN1type(Type),
    snmp_compile_lib:register_oid(Line, NewVarName, FatherName, SubIndex),
    NewME = #me{aliasname = NewVarName, asn1_type = ASN1type,
		entrytype = variable, assocList = DefVal,
		access = Access },  
        NewME2 = snmp_compile_lib:resolve_defval(NewME),
    %% hmm, should this be done in resolve_defval?
    VI=snmp_compile_lib:make_variable_info(NewME2), 
    snmp_compile_lib:add_cdata(#cdata.mes,
			       [NewME2#me{assocList = [{variable_info, VI}]}]),
    definitions_loop(T);

definitions_loop([{{internal,Macro,NewVarName,FatherName,SubIndex},Line}|T]) ->
    ensure_macro_imported(Macro,Line),
    snmp_compile_lib:register_oid(Line,NewVarName,FatherName,SubIndex),
    snmp_compile_lib:add_cdata(
      #cdata.mes,
      [snmp_compile_lib:makeInternalNode2(false, NewVarName)]),
    definitions_loop(T);    

definitions_loop([{{trap,TrapName,EnterPrise,
		    Variables,SpecificCode},Line}|T]) ->
    CDATA = get(cdata),
    snmp_compile_lib:check_trap_name(EnterPrise, Line, CDATA#cdata.mes),
    Trap = #trap{trapname = TrapName, enterpriseoid=EnterPrise,
		 specificcode = SpecificCode,
		 oidobjects = lists:map({snmp_compile_lib,trap_variable_info},
					[Line, CDATA#cdata.mes],
					Variables)},
    lists:map({snmp_compile_lib,check_trap}, [Trap, Line], CDATA#cdata.traps),
    snmp_compile_lib:add_cdata(#cdata.traps, [Trap]),
    definitions_loop(T);    

definitions_loop([{{object_type,NameOfEntry,Type,Eaccess,{table_entry,Index},
		    Estatus,SubIndex},Eline}|T]) ->
    snmp_compile_lib:print_error("Misplaced TableEntry definition (~w)",
				 [NameOfEntry],Eline),
    definitions_loop(T);

definitions_loop([{{notification,TrapName,Variables,Status,
		    {FatherName,SubIndex}},Line}|T]) ->
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    CDATA = get(cdata),
    snmp_compile_lib:register_oid(Line,TrapName,FatherName,SubIndex),
    Notif = #notification{
      trapname = TrapName,
      oidobjects = lists:map({snmp_compile_lib,trap_variable_info},
			     [Line, CDATA#cdata.mes],
			     Variables)},
    snmp_compile_lib:check_notification(Notif, Line, CDATA#cdata.traps),
    snmp_compile_lib:add_cdata(#cdata.traps, [Notif]),
    definitions_loop(T);    

definitions_loop([{{module_compliance,Name},Line}|T]) ->
    ensure_macro_imported('MODULE-COMPLIANCE', Line),
    definitions_loop(T);

definitions_loop([{{object_group,Name,GroupObjects,deprecated},Line}|T]) ->
    GroupBool = hd([Value || {Option,Value} <-get(options), Option==group_check]),
    case GroupBool==true of
	true->
	    %% May be implemented but the compiler chooses not to.
	    snmp_compile_lib:warning("~w is deprecated. Ignored.",[Name],Line),
	    definitions_loop(T);
	false->
	    definitions_loop(T)
    end;

definitions_loop([{{object_group,Name,GroupObjects,obsolete},Line}|T]) ->
    %% No need to implement a obsolete group
    ensure_macro_imported('OBJECT-GROUP', Line),
    definitions_loop(T);

definitions_loop([{{object_group,Name,GroupObjects,_Status},Line}|T]) ->
    ensure_macro_imported('OBJECT-GROUP', Line),
    GroupBool = hd([Value || {Option,Value} <-get(options), Option==group_check]),
    case GroupBool==true of
	true->
	    snmp_compile_lib:add_cdata(#cdata.objectgroups,[{Name,GroupObjects,Line}]),
	    CDATA=get(cdata),
	    Objects=snmp_compile_lib:check_access_group(CDATA#cdata.mes),
	    snmp_compile_lib:check_def(Objects,GroupObjects,Line),
	    definitions_loop(T);
	false->
	    definitions_loop(T)
    end;

definitions_loop([{{notification_group,Name,GroupObjects,deprecated},Line}|T]) ->
    GroupBool = hd([Value || {Option,Value} <-get(options), Option==group_check]),
    case GroupBool==true of
	true->
	    %% May be implemented but the compiler chooses not to.
	    snmp_compile_lib:warning("~w is deprecated. Ignored.",[Name],Line),
	    definitions_loop(T);
	false->
	    definitions_loop(T)
    end;

definitions_loop([{{notification_group,Name,GroupObjects,obsolete},Line}|T]) ->
    %% No need to implement a obsolete object
    ensure_macro_imported('NOTIFICATION-GROUP', Line),
    definitions_loop(T);

definitions_loop([{{notification_group,Name,NotificationObjects,_Status},Line}|T]) ->
    ensure_macro_imported('NOTIFICATION-GROUP', Line),
    GroupBool = hd([Value || {Option,Value} <-get(options), Option==group_check]),
    case GroupBool==true of
	true->
	    snmp_compile_lib:add_cdata(#cdata.notificationgroups,[{Name,NotificationObjects,Line}]),
	    CDATA=get(cdata),
	    Objects = snmp_compile_lib:check_notification_trap(CDATA#cdata.traps),
	    snmp_compile_lib:check_def(Objects,NotificationObjects,Line),
	    definitions_loop(T);
	false->
	    definitions_loop(T)
    end;

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline}, Entry,Seq|T]) ->
    case Entry of
	{{object_type,NameOfEntry,{{type,SeqName},_line},
		    'not-accessible',
		    {table_entry,IndexingInfo},
		    Estatus,{NameOfTable,[1]}},Eline} ->
	    case Seq of
		{{sequence,SeqName,{fieldList,FieldList}},Sline} ->
		    snmp_compile_lib:error("Internal error. Correct incorrect "
					   "table.",[],Tline);
		Else ->
		    snmp_compile_lib:print_error(
		      "Invalid SEQUENCE OF '~p'.",
		      [safe_elem(1,safe_elem(2,Seq))],Tline)
	    end;
	Else ->
	    snmp_compile_lib:print_error(
	      "Invalid TableEntry '~p' (check STATUS, Sequence name, Oid)",
	      [safe_elem(1,safe_elem(2,Entry))],Tline)
    end,
    definitions_loop(T);

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline}|T]) ->
    snmp_compile_lib:print_error("Invalid statements following table ~p.",
				 [NameOfTable],Tline),
    definitions_loop(T);

definitions_loop([{{sequence,SeqName,{fieldList,FieldList}},Line}|T]) ->
    snmp_compile_lib:warning("Unexpected SEQUENCE ~w, ignoring.",
			     [SeqName],Line),
    definitions_loop(T);

definitions_loop([{Obj,Line}|T]) ->
    snmp_compile_lib:print_error("Unknown Error in MIB. "
	 "Can't describe the error better than this: ~999p ignored."
	 " Please send a trouble report to support@erlang.ericsson.se.",
				 [Obj],Line),
    definitions_loop(T);

definitions_loop([]) ->
    ok.

safe_elem(N,T) ->
    case catch(element(N,T)) of
	{'EXIT',_} ->
	    "no more information available";
	X -> X
    end.

%% A correct column
define_cols([{{object_type,NameOfCol,Type1,Access,{variable,Defval},Status,
	    {NameOfEntry,[SubIndex]}},Oline}|Rest],SubIndex,
	 [{NameOfCol,Type2}|Fields],NameOfEntry,TableName,ColMEs) ->
    ASN1type = snmp_compile_lib:make_ASN1type(Type1),
    case (snmp_compile_lib:make_ASN1type(Type2))#asn1_type.bertype of
	T2 when T2 == ASN1type#asn1_type.bertype -> ok;
	Else ->
	    snmp_compile_lib:error(
	      "Types for ~p differs from the SEQUENCE definition. "
	      ,[NameOfCol],Oline)
    end,
    NewAccess = % a simple way to get the obsolete behaviour
	if
	    Status == obsolete ->
		%% Be quiet and don't implement
		'not-accessible';
	    Status == deprecated ->
		%% The compiler chooses not to implement the column.
		snmp_compile_lib:warning("~w is deprecated. Ignored.",
					 [NameOfCol],Oline),
		'not-accessible';
	   true -> Access
	end,
    snmp_compile_lib:register_oid(Oline,NameOfCol,NameOfEntry,[SubIndex]),
    ColumnME = snmp_compile_lib:resolve_defval(
		 #me{oid = SubIndex,
		     aliasname = NameOfCol, asn1_type = ASN1type,
		     entrytype = table_column, access = NewAccess,
		     assocList = [{table_name,TableName} |
				  Defval]}),
    define_cols(Rest,SubIndex+1,Fields,NameOfEntry,TableName,
	     [ColumnME|ColMEs]);

%% A "hole" (non-consecutive columns) in the table.
%% Implemented as a not-accessible column so Col always is index in
%% row tuple.
define_cols([{{object_type,NameOfCol,Type1,Access,Kind,Status,
	    {NameOfEntry,[SubIndex]}},Oline}|Rest],ExpectedSubIndex,
	    Fields,NameOfEntry,TableName,ColMEs) 
  when SubIndex > ExpectedSubIndex ->
    Int = {{type, 'INTEGER'},Oline},
    GeneratedColumn =
	%% be sure to use an invalid column name here!
	{{object_type,'$no_name$',Int,'not-accessible',
	  {variable,[{defval,0}]},current,
	  {NameOfEntry,[ExpectedSubIndex]}},Oline},
    define_cols([GeneratedColumn,
		 {{object_type,NameOfCol,Type1,Access,Kind,Status,
		   {NameOfEntry,[SubIndex]}},Oline}|Rest],ExpectedSubIndex,
		[{'$no_name$',Int}|Fields],NameOfEntry,
		TableName,ColMEs) ;

%% Ok. done. All fields are eaten.
define_cols(Rest,SubIndex,[],NameOfEntry,TableName,ColMEs) ->
    {ColMEs,Rest};
%% Error Handling

%% The name of the field and object is the same
define_cols([{{object_type,NameOfCol,Type1,Access,Kind,Status,
	       SubIndex},Oline}|Rest],SubIndex2,[{NameOfCol,Type2}|Fields],
	    NameOfEntry,TableName,ColMEs) ->
    SIok = case SubIndex of
	       {Parent,[SI]} when Parent =/= NameOfEntry ->
		   snmp_compile_lib:print_error(
		     "Invalid parent ~p for table column ~p (should be ~p).",
		     [Parent,NameOfCol,NameOfEntry],Oline),
		   false;
	       {NameOfEntry,[SI]} when SI =/= SubIndex ->
		   snmp_compile_lib:print_error(
		     "Invalid column number ~p for column ~p.",
		     [SI,NameOfCol],Oline),
		   false;
	       {NameOfEntry,[SubIndex]} ->
		   ok;
	       Q ->
		   snmp_compile_lib:print_error(
		     "Invalid parent for column ~p.",[NameOfCol],Oline)
	   end,
    Kok = case Kind of
	      {variable,_} ->
		  ok;
	      Q2 ->
		  snmp_compile_lib:print_error(
		    "Expected a table column.",[],Oline),
		  false
	  end,
    case {SIok,Kok} of
	{ok,ok} ->
	    snmp_compile_lib:print_error("Invalid table column definition for"
					 " ~p.",[NameOfCol],Oline);
	Q4 ->
	    done
    end,
    define_cols(Rest,SubIndex2+1,Fields,NameOfEntry,TableName,ColMEs);

%% It's an object-type but everything else is wrong
define_cols([{{object_type,NameOfCol,Type1,Access,Kind,Status,
	       SubIndex},Oline}|Rest],SubIndex2,Fields,
	    NameOfEntry,TableName,ColMEs) ->
    snmp_compile_lib:print_error(
      "Number of columns differs from SEQUENCE definition (object:~p).",
      [NameOfCol],Oline),
    define_cols(Rest,SubIndex2+1,Fields,NameOfEntry,TableName,ColMEs);

define_cols([{Obj,Line}|Tl],SubIndex,_,_,_,ColMEs) ->
    snmp_compile_lib:print_error("Corrupt table definition.",[],Line),
    {ColMEs,[{Obj,Line}|Tl]};
define_cols(Rest,SubIndex,_,_,_,ColMEs) ->
    snmp_compile_lib:print_error("Corrupt table definition.",[]),
    {ColMEs,Rest}.

ensure_macro_imported(dummy, Line) -> ok;
ensure_macro_imported(Macro, Line) ->
    Macros = (get(cdata))#cdata.imported_macros,
    case lists:member(Macro, Macros) of
	true -> ok;
	false ->
	    snmp_compile_lib:print_error("Macro ~p not imported.", [Macro],
					 Line)
    end.

test_table(NameOfTable,Taccess,Kind,Tindex,Tline) ->
    if
	Taccess =/= 'not-accessible' ->
	    snmp_compile_lib:print_error(
	      "Table ~w must have STATUS not-accessible",
	      [NameOfTable],Tline),
	    error;
	Kind =/= {variable,[]} ->
	    snmp_compile_lib:print_error(
	      "Bad table definition (~w).",
	      [NameOfTable],Tline),
	    error;
	true ->
	    ok
    end.

save(Filename, MibName,Options) ->
    R = filename:rootname(Filename),
    File1 = filename:basename(R),
    File3 = snmp_misc:to_upper(File1),
    case snmp_misc:to_upper(atom_to_list(MibName)) of
	File3 ->
	    {value, OutDirr} = snmp_misc:assq(outdir, Options),
	    OutDir = snmp_misc:ensure_trailing_dir_delimiter(OutDirr),
	    File2 = (OutDir ++ File1) ++ ".bin",
	    {ok, MIB} = snmp_compile_lib:get_final_mib(File1, Options),
	    case get(errors) of
		undefined ->
		    case file:write_file(File2, term_to_binary(MIB)) of
			ok ->
			    {ok,File2};
			Err ->
			    snmp_compile_lib:error(
			      "Couldn't write file \"~s\".",[File2])
		    end;
		QQ ->
		    {'EXIT',error}
	    end;
	MibNameL ->
	    snmp_compile_lib:error("Mibname (~s) differs from filename (~s).",
				   [MibNameL, File1])
    end.
    



snmp_parse(FileName) ->
    case snmp_tok:start_link(reserved_words(),
		     [{file, FileName ++ ".mib"},{forget_stringdata, true}]) of
	{error,ReasonStr} ->
	    snmp_compile_lib:error(lists:flatten(ReasonStr),[]);
	{ok, TokPid} ->
	    Toks = snmp_tok:get_all_tokens(TokPid),
	    set_version(Toks),
	    put(cdata,snmp_compile_lib:make_cdata(FileName ++ ".funcs")),
	    snmp_tok:stop(TokPid),
	    Res = if list(Toks) ->
			  snmp_mib_gram:parse(Toks);
		     true ->
			  Toks
		  end,
	    case Res of
		{ok, Mib} ->
		    {ok, Mib};
		{error, {LineNbr, Mod, Msg}} ->
		    case catch format_yecc_error(LineNbr, Msg) of
			{Line, Format, Data} -> 
			    snmp_compile_lib:error(Format,Data,Line);
			Q -> % sorry, have to use ugly yecc printouts
			    Str = apply(Mod, format_error, [Msg]),
			    snmp_compile_lib:error("~s",[Str],LineNbr)
		    end
	    end
    end.

set_version(Toks) when list(Toks) ->
%% MODULE-IDENTITY _must_ be invoked in SNMPv2 according to RFC1908
    case lists:keymember('MODULE-IDENTITY',1,Toks) of
	true ->
	    put(snmp_version,2);
	false ->
	    put(snmp_version,1)
    end;
set_version(_) ->
    put(snmp_version,1).


%% YeccGeneratedFile:format_error/1 is bad.
format_yecc_error(Line, [ErrMsg, [${,Category, $,, LineStr,$,, Value, $}]]) ->
    {Line, "~s \"~s\" (~s).", [ErrMsg, Value, Category]}.

%% The same as the (quoted) Terminals in the snmp_mib_gram.yrl
reserved_words() -> [ 'ACCESS', 'BEGIN', 'BIT', 'CONTACT-INFO',
'Counter', 'DEFINITIONS', 'DEFVAL', 'DESCRIPTION', 'DISPLAY-HINT',
'END', 'ENTERPRISE', 'FROM', 'Gauge', 'IDENTIFIER', 'IDENTIFIER',
'IMPORTS', 'INDEX', 'INTEGER', 'IpAddress', 'LAST-UPDATED',
'NetworkAddress', 'OBJECT', 'OBJECT', 'OBJECT-TYPE', 'OCTET', 'OF',
'Opaque', 'REFERENCE', 'SEQUENCE', 'SIZE', 'STATUS', 'STRING',
'SYNTAX', 'TRAP-TYPE', 'TimeTicks', 'VARIABLES', 'deprecated',
'mandatory', 'not-accessible', 'obsolete', 'optional', 'read-only',
'read-write', 'write-only',

%% v2
'LAST-UPDATED',
'ORGANIZATION',
'CONTACT-INFO',
'MODULE-IDENTITY',
'NOTIFICATION-TYPE',
'MODULE-COMPLIANCE',
'OBJECT-GROUP',
'NOTIFICATION-GROUP',
'REVISION',
'OBJECT-IDENTITY',
'current',
'MAX-ACCESS',
'accessible-for-notify',
'read-create',
'UNITS',
'AUGMENTS',
'IMPLIED',
'OBJECTS',
'TEXTUAL-CONVENTION',
'OBJECT-GROUP',
'NOTIFICATION-GROUP',
'NOTIFICATIONS',
'MODULE-COMPLIANCE',
'MODULE',
'MANDATORY-GROUPS',
'GROUP',
'WRITE-SYNTAX',
'MIN-ACCESS',
'BITS'
].
