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
    compile(atom_to_list(AtomFilename), []), % from cmd line
    halt();
compile(FileName) -> compile(FileName, []).

look_at(Mib) ->
    io:format("~p ~n", [snmp_compile_lib:look_at(Mib)]).

%%----------------------------------------------------------------------
%% Options:
%%          {deprecated,  bool()}                         true
%%          {group_check, bool()}                         true
%%          {db,          volatile|persistent|mnesia}     volatile
%%          {i,           [import_dir_string()]}          ["./"]
%%          {il,          [import_lib_dir_string()]}      []
%%          {warnings,    bool()}                         true
%%          {outdir,      string()}                       "./"
%%          {description, bool()}                         false
%% (hidden) {verbosity,   trace|debug|log|info|silence}   silence
%% (hidden) version 
%% (hidden) options 
%%----------------------------------------------------------------------

compile(FileName, Options) when list(FileName) ->
    true = snmp_misc:is_string(FileName),
    Opts = update_options(Options, [{deprecated,  true},
				    {group_check, true},
				    {i,           ["./"]},
				    {db,          volatile},
				    {warnings,    true},
				    {outdir,      "./"},
				    {il,          []}]),
    case check_options(Opts) of
	ok ->
	    maybe_display_version(Opts),
	    maybe_display_options(Opts),
	    Pid = spawn_link(snmp_compile,init,[self(),FileName,Opts]),
	    receive
		{mc_result,R} -> R;
		{'EXIT',Pid, Reason} when Reason =/= normal ->
		    exit(Reason)
	    end;
	{error, Reason} -> {error, Reason}
    end.

maybe_display_version(Opts) ->
    case lists:member(version, Opts) of
	true ->
	    Vsn = (catch get_version()),
	    io:format("version: ~s~n", [Vsn]);
	false ->
	    ok
    end.

get_version() ->
    MI   = ?MODULE:module_info(),
    Attr = get_info(attributes, MI),
    Vsn  = get_info(app_vsn, Attr),
    Comp = get_info(compile, MI),
    Time = get_info(time, Comp),
    {Year, Month, Day, Hour, Min, Sec} = Time,
    io_lib:format("~s [~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w]", 
		  [Vsn, Year, Month, Day, Hour, Min, Sec]).

maybe_display_options(Opts) ->
    case lists:member(options, Opts) of
	true ->
	    {F, A} = get_options(Opts, [], []),
	    io:format("options: " ++ F ++ "~n", A);
	false ->
	    ok
    end.

get_options([], Formats, Args) ->
    {lists:concat(lists:reverse(Formats)), lists:reverse(Args)};
get_options([{deprecated, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   deprecated:  ~w"|Formats], [Val|Args]);
get_options([{group_check, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   group_check: ~w"|Formats], [Val|Args]);
get_options([{db, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   db:          ~w"|Formats], [Val|Args]);
get_options([{i, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   i:           ~p"|Formats], [Val|Args]);
get_options([{il, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   il:          ~p"|Formats], [Val|Args]);
get_options([{outdir, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   outdir:      ~s"|Formats], [Val|Args]);
get_options([{description, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   description: ~w"|Formats], [Val|Args]);
get_options([{warnings, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   warnings:    ~w"|Formats], [Val|Args]);
get_options([{verbosity, Val}|Opts], Formats, Args) ->
    get_options(Opts, ["~n   verbosity:   ~w"|Formats], [Val|Args]);
get_options([_|Opts], Formats, Args) ->
    get_options(Opts, Formats, Args).
    

get_info(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    throw("undefined")
    end.

update_options([], Options) -> 
    Options;
update_options([{Key,DefVal}|DefOpts], Options) ->
    case snmp_misc:assq(Key, Options) of
	false ->
	    update_options(DefOpts, [{Key,DefVal}|Options]);
	{value, Val} when Key == i ->
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, Val++DefVal}),
	    update_options(DefOpts, Options1);
	{value, Val} when Key == il ->
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, Val++DefVal}),
	    update_options(DefOpts, Options1);
	{value, DefVal} -> %% Same value, no need to update
	    update_options(DefOpts, Options);
	{value, Val} ->    %% New value, so update
	    Options1 = 
		lists:keyreplace(Key, 1, Options, {Key, DefVal}),
	    update_options(DefOpts, Options1)
    end;
update_options([Opt|DefOpts], Options) ->
    case lists:member(Opt, Options) of
	true ->
	    update_options(DefOpts, Options);
	false ->
	    update_options(DefOpts, [Opt|Options])
    end.

check_options([]) -> ok;
check_options([no_symbolic_info|T]) -> check_options(T);
check_options([{outdir, Str} | T]) when list(Str) ->
    check_options(T);
check_options([{debug, Atom} | T]) when atom(Atom) ->
    check_options(T);
check_options([{deprecated, Atom} | T]) when atom(Atom) ->
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
check_options([{description, Atom}| T]) when atom(Atom) ->
    check_options(T);
check_options([{verbosity, V} | T]) when atom(V) ->
    snmp_compile_lib:vvalidate(V),
    check_options(T);
check_options([version| T]) ->
    check_options(T);
check_options([options| T]) ->
    check_options(T);
check_options([Opt|_]) ->
    {error, {invalid_option, Opt}}.


get_group_check(Options) ->
    snmp_compile_lib:key1search(group_check,Options,false).

get_deprecated(Options) ->
    snmp_compile_lib:key1search(deprecated,Options,false).

get_description(Options) ->
    snmp_compile_lib:key1search(description,Options,false).

make_description(Message) ->
    case get(description) of
	true ->
	    Message;
	_ -> 
	    undefined
    end.

    
		
%%----------------------------------------------------------------------
%% verbosity stuff
%%----------------------------------------------------------------------

t(F,A)   -> snmp_compile_lib:t(F,A).
d(F,A)   -> snmp_compile_lib:d(F,A).
l(F,A)   -> snmp_compile_lib:l(F,A).
i(F,A)   -> snmp_compile_lib:i(F,A).
i(F,A,L) -> snmp_compile_lib:i(F,A,L).
w(F,A)   -> snmp_compile_lib:w(F,A).
w(F,A,L) -> snmp_compile_lib:w(F,A,L).

%% Verbosity level is selected from three (historical reasons)
%% options: warnings, debug and verbosity
%% - If warnings is true, then verbosity is _atleast_ warning
%%   (even if the verbosity flag is set to silence)
%% - If debug is true, the verbosity is _atleast_ log
%% - Otherwise, verbosity is used as is.
get_verbosity(Options) ->
    WarningsSeverity = 
	case snmp_compile_lib:key1search(warnings,Options) of
	    true ->
		warning;
	    _ ->
		silence
	end,
    case snmp_compile_lib:key1search(verbosity,Options) of
	undefined ->
	    %% Backward compatible: If not defined the try debug and convert
	    case snmp_compile_lib:key1search(debug,Options,false) of
		true ->
		    log;
		false ->
		    WarningsSeverity
	    end;
	silence ->
	    WarningsSeverity;
	Verbosity ->
	    Verbosity
    end.


%%----------------------------------------------------------------------
%% The compile process.
%%----------------------------------------------------------------------

init(From,MibFileName,Options) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    put(options,Options),
    put(verbosity,get_verbosity(Options)),
    put(description, get_description(Options)),
    File = filename:rootname(MibFileName, ".mib"),
    put(filename, filename:basename(File ++ ".mib")),
    R = case catch c_impl(File) of
	    {ok, OutFile} -> {ok, OutFile};
	    {'EXIT',error} -> {error, compilation_failed};
	    Error -> exit(Error)
	end,
    From ! {mc_result,R}.


c_impl(File) ->
    {ok, Mib} = snmp_parse(File),
    t("Syntax analysis:~n"
      "   ~p",[Mib]),
    MibName = compile_parsed_data(Mib),
    CData = get(cdata),
    t("Compiler output:~n"
      "   ~p",[CData]),
    save(File, MibName,get(options)).

compile_parsed_data({MibVer,MibName,{{import,Imports},Line}, Definitions}) ->
    snmp_compile_lib:import(Imports),
    DeprecatedFlag = get_deprecated(get(options)),
    d("compile_parsed_data -> DeprecatedFlag: ~p",[DeprecatedFlag]),
    definitions_loop(Definitions, DeprecatedFlag),
    MibName.


update_status(Name, Status) ->
    #cdata{status_ets = Ets} = get(cdata),
    ets:insert(Ets, {Name, Status}).
    

%% A deprecated object
definitions_loop([{{object_type,ObjName,_Type,_Access,
		    _Kind, deprecated,
		    {'DESCRIPTION', Message1},
		    _Index},Line}|T],
		 false) ->
    %% May be implemented but the compiler chooses not to.
    i("object_type ~w is deprecated => ignored",[ObjName],Line),    
    update_status(ObjName, deprecated), 
    definitions_loop(T, false);

%% A obsolete object
definitions_loop([{{object_type, ObjName, _Type, _Access,
		    _Kind, obsolete,
		    {'DESCRIPTION', Message1},
		    _Index}, Line}|T], 
		 Deprecated) ->
    l("object_type ~w (~w) is obsolete => ignored",[ObjName,Line]),
    %% No need to implement a obsolete object
    update_status(ObjName, obsolete),
    ensure_macro_imported('OBJECT-TYPE', Line),
    definitions_loop(T, Deprecated);

%% Defining a table
definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind, Tstatus,
		    {'DESCRIPTION', Message1},
		    Tindex},Tline},
		  {{object_type,NameOfEntry,{{type,SeqName},TEline},
		    'not-accessible',
		    {table_entry,IndexingInfo},Estatus, 
		    {'DESCRIPTION', Message2},
		    {NameOfTable,[1]}},Eline},
		  {{sequence,SeqName,{fieldList,FieldList}},Sline}|ColsEtc],
		 Deprecated) ->
    l("defloop -> [object_type(sequence_of),object_type(type,[1]),sequence]:~n"
      "   NameOfTable:  ~p~n"
      "   SeqName:      ~p~n"
      "   Taccess:      ~p~n"
      "   Kind:         ~p~n"
      "   Tstatus:      ~p~n"
      "   Tindex:       ~p~n"
      "   Tline:        ~p~n"
      "   NameOfEntry:  ~p~n"
      "   TEline:       ~p~n"
      "   IndexingInfo: ~p~n"
      "   Estatus:      ~p~n"
      "   Eline:        ~p~n"
      "   FieldList:    ~p~n"
      "   Sline:        ~p",
      [NameOfTable,SeqName,Taccess,Kind,Tstatus,
       Tindex,Tline,
       NameOfEntry,TEline,IndexingInfo,Estatus,Eline,
       FieldList,Sline]),
    update_status(NameOfTable, Tstatus),
    update_status(NameOfEntry, Estatus),
    update_status(SeqName,     undefined),
    ensure_macro_imported('OBJECT-TYPE', Tline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmp_compile_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    Description1 = make_description(Message1),
    TableME = #me{aliasname = NameOfTable,
		  entrytype = table, access = 'not-accessible',
		  description = Description1},
    snmp_compile_lib:register_oid(TEline,NameOfEntry,NameOfTable,[1]),
    Description2 = make_description(Message2),
    TableEntryME = #me{aliasname   = NameOfEntry, entrytype = table_entry,
		       assocList   = [{table_entry_with_sequence, SeqName}], 
		       access      = 'not-accessible',
		       description = Description2 },
    {ColMEs,RestObjs} = define_cols(ColsEtc,1,FieldList,NameOfEntry,
				    NameOfTable,[]),
    TableInfo = snmp_compile_lib:make_table_info(Eline, NameOfTable,
						 IndexingInfo,ColMEs),
    snmp_compile_lib:add_cdata(#cdata.mes, 
			       [TableEntryME,
				TableME#me{assocList=[{table_info, 
						       TableInfo}]} |
				ColMEs]),
    definitions_loop(RestObjs, Deprecated);

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind, Tstatus,
		    {'DESCRIPTION', Message1}, 
		    Tindex},Tline},
		  {{object_type,NameOfEntry,{{type,SeqName},_line},
		    'not-accessible',
		    {table_entry,IndexingInfo}, Estatus, 
		    {'DESCRIPTION', Message2},
		    BadOID},Eline},
		  {{sequence,SeqName,{fieldList,FieldList}},Sline}|ColsEtc],
		 Deprecated) ->
    l("defloop -> "
      "[object_type(sequence_of),object_type(type),sequence(fieldList)]:~n"
      "   NameOfTable:  ~p~n"
      "   SeqName:      ~p~n"
      "   Taccess:      ~p~n"
      "   Kind:         ~p~n"
      "   Tstatus:      ~p~n"
      "   Tindex:       ~p~n"
      "   Tline:        ~p~n"
      "   NameOfEntry:  ~p~n"
      "   IndexingInfo: ~p~n"
      "   Estatus:      ~p~n"
      "   BadOID:       ~p~n"
      "   Eline:        ~p~n"
      "   FieldList:    ~p~n"
      "   Sline:        ~p",
      [NameOfTable,SeqName,Taccess,Kind,Tstatus,
       Tindex,Tline,
       NameOfEntry,IndexingInfo,Estatus,BadOID,Eline,
       FieldList,Sline]),
    update_status(NameOfTable, Tstatus),
    update_status(NameOfEntry, Estatus),
    update_status(SeqName,     undefined),
    ensure_macro_imported('OBJECT-TYPE', Tline),
    snmp_compile_lib:print_error("Bad TableEntry OID definition (~w)",
				 [BadOID],Eline),
    test_table(NameOfTable,Taccess,Kind,Tindex,Tline),
    {Tfather,Tsubindex} = Tindex,
    snmp_compile_lib:register_oid(Tline,NameOfTable,Tfather,Tsubindex),
    Description1 = make_description(Message1),
    TableME = #me{aliasname   = NameOfTable,
		  entrytype   = table, 
		  access       = 'not-accessible',
		  description = Description1
		 },
    Description2 = make_description(Message2),
    TableEntryME = #me{aliasname = NameOfEntry, entrytype = table_entry,
		       access      = 'not-accessible',
		       assocList   = [{table_entry_with_sequence,SeqName}],
		       description = Description2 
		      },
    {ColMEs,RestObjs} = define_cols(ColsEtc,1,FieldList,NameOfEntry,
				    NameOfTable,[]),
    TableInfo = snmp_compile_lib:make_table_info(Eline, NameOfTable,
						 IndexingInfo,ColMEs),
    snmp_compile_lib:add_cdata(#cdata.mes, 
			       [TableEntryME,
				TableME#me{assocList=[{table_info, 
						       TableInfo}]} |
				ColMEs]),
    definitions_loop(RestObjs, Deprecated);

definitions_loop([{{new_type,Macro,NewTypeName,OldType},Line}|T],
		 Deprecated) ->
    l("defloop -> new_type:~n"
      "   Macro:       ~p~n"
      "   NewTypeName: ~p~n"
      "   OldType:     ~p~n"
      "   Line:        ~p",[Macro,NewTypeName,OldType,Line]),
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
    definitions_loop(T,	Deprecated);

%% Plain variable
definitions_loop([{{object_type,NewVarName,
		    Type,Access,{variable,DefVal}, Status,
		    {'DESCRIPTION', Message1}, 
		    {FatherName,SubIndex}},Line} |T],
		 Deprecated) ->
    l("defloop -> object_type (variable):~n"
      "   NewVarName: ~p~n"
      "   Type:       ~p~n"
      "   Access:     ~p~n"
      "   DefVal:     ~p~n"
      "   Status:     ~p~n"
      "   FatherName: ~p~n"
      "   SubIndex:   ~p~n"
      "   Line:       ~p",
      [NewVarName,Type,Access,DefVal,Status,
       FatherName,SubIndex,Line]),
    update_status(NewVarName, Status),
    snmp_compile_lib:test_father(FatherName, NewVarName, SubIndex, Line),
    ASN1type = snmp_compile_lib:make_ASN1type(Type),
    snmp_compile_lib:register_oid(Line, NewVarName, FatherName, SubIndex),
    Description1 = make_description(Message1),
    NewME = #me{aliasname   = NewVarName, asn1_type = ASN1type,
		entrytype   = variable, 	access = Access,
		description = Description1, 
		assocList   = DefVal},  
        NewME2 = snmp_compile_lib:resolve_defval(NewME),
    %% hmm, should this be done in resolve_defval?
    VI=snmp_compile_lib:make_variable_info(NewME2), 
    snmp_compile_lib:add_cdata(#cdata.mes,
			       [NewME2#me{assocList = [{variable_info, VI}]}]),
    definitions_loop(T, Deprecated);

definitions_loop([{{internal,Macro,NewVarName,FatherName,SubIndex},Line}|T],
		 Deprecated) ->
    l("defloop -> internal:~n"
      "   Macro:      ~p~n"
      "   NewVarName: ~p~n"
      "   FatherName: ~p~n"
      "   SubIndex:   ~p~n"
      "   Line:       ~p",[Macro,NewVarName,FatherName,SubIndex,Line]),
    ensure_macro_imported(Macro,Line),
    snmp_compile_lib:register_oid(Line,NewVarName,FatherName,SubIndex),
    snmp_compile_lib:add_cdata(
      #cdata.mes,
      [snmp_compile_lib:makeInternalNode2(false, NewVarName)]),
    definitions_loop(T, Deprecated);    

%% A trap message
definitions_loop([{{trap,TrapName,EnterPrise, Variables, 
		    {'DESCRIPTION', Message1},
		    SpecificCode},Line}|T],
		 Deprecated) ->
    l("defloop -> trap:~n"
      "   TrapName:     ~p~n"
      "   EnterPrise:   ~p~n"
      "   Variables:    ~p~n"
      "   SpecificCode: ~p~n"
      "   Line:         ~p",
      [TrapName,EnterPrise,Variables,SpecificCode,Line]),
    update_status(TrapName, undefined),
    CDATA = get(cdata),
    snmp_compile_lib:check_trap_name(EnterPrise, Line, CDATA#cdata.mes),
    Descriptions = make_description(Message1),
    OidObjects   = snmp_misc:map({snmp_compile_lib,trap_variable_info},
				 [Line, CDATA#cdata.mes],
				 Variables), 
    Trap = #trap{trapname      = TrapName, 
		 enterpriseoid = EnterPrise,
		 specificcode  = SpecificCode,
		 oidobjects    = OidObjects,
		 description   = Descriptions},
    snmp_misc:map({snmp_compile_lib,check_trap}, [Trap, Line], 
		  CDATA#cdata.traps),
    snmp_compile_lib:add_cdata(#cdata.traps, [Trap]),
    definitions_loop(T, Deprecated);    

definitions_loop([{{object_type, NameOfEntry, Type, Eaccess,
		    {table_entry,Index},
		    Estatus,SubIndex},Eline}|T], Deprecated) ->
    l("defloop -> object_type (table_entry):~n"
      "   NameOfEntry: ~p~n"
      "   Type:        ~p~n"
      "   Eaccess:     ~p~n"
      "   Index:       ~p~n"
      "   Estatus:     ~p~n"
      "   SubIndex:    ~p~n"
      "   SubIndex:    ~p~n"
      "   Eline:       ~p",
      [NameOfEntry,Type,Eaccess,Index,Estatus,SubIndex,Eline]),
    update_status(NameOfEntry, Estatus),
    snmp_compile_lib:print_error("Misplaced TableEntry definition (~w)",
				 [NameOfEntry],Eline),
    definitions_loop(T, Deprecated);

definitions_loop([{{notification,TrapName,Variables,deprecated,
		    {'DESCRIPTION', Message1},
		    {FatherName,SubIndex}},Line}|T],
		 false) ->
    i("notification ~w is deprecated => ignored",[TrapName],Line),    
    update_status(TrapName, deprecated),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    definitions_loop(T, false);    

definitions_loop([{{notification,TrapName,Variables,obsolete,
		    {'DESCRIPTION', Message1},
		    {FatherName,SubIndex}},Line}|T],
		 Deprecated) ->
    l("notification ~w (~w) is obsolete => ignored", [TrapName,Line]),
    update_status(TrapName, obsolete),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    definitions_loop(T, Deprecated);    

definitions_loop([{{notification,TrapName,Variables,Status,
		    {'DESCRIPTION', Message1},
		    {FatherName,SubIndex}},Line}|T],
		 Deprecated) ->
    l("defloop -> notification:~n"
      "   TrapName:    ~p~n"
      "   Variables:   ~p~n"
      "   Status:      ~p~n"
      "   FartherName: ~p~n"
      "   SubIndex:    ~p~n"
      "   Line:        ~p",
      [TrapName,Variables,Status,FatherName,SubIndex,Line]),
    update_status(TrapName, Status),
    ensure_macro_imported('NOTIFICATION-TYPE', Line),
    CDATA = get(cdata),
    snmp_compile_lib:register_oid(Line,TrapName,FatherName,SubIndex),
    Descriptions = make_description(Message1),
    OidObjects   = snmp_misc:map({snmp_compile_lib,trap_variable_info},
				 [Line, CDATA#cdata.mes],
				 Variables),
    Notif = #notification{trapname    = TrapName,
			  description = Descriptions,
			  oidobjects  = OidObjects},
    snmp_compile_lib:check_notification(Notif, Line, CDATA#cdata.traps),
    snmp_compile_lib:add_cdata(#cdata.traps, [Notif]),
    definitions_loop(T, Deprecated);    

definitions_loop([{{module_compliance,Name},Line}|T], Deprecated) ->
    l("defloop -> module_compliance:~n"
      "   Name: ~p~n"
      "   Line: ~p",[Name,Line]),
    ensure_macro_imported('MODULE-COMPLIANCE', Line),
    definitions_loop(T, Deprecated);

definitions_loop([{{object_group,Name,GroupObjects,Status},Line}|T],
		 Deprecated) ->
    l("defloop -> object_group ~p:~n"
      "   Status:       ~p~n"
      "   GroupObjects: ~p~n"
      "   Line:         ~p",[Name,Status,GroupObjects,Line]),
    ensure_macro_imported('OBJECT-GROUP', Line),
    GroupBool = get_group_check(get(options)),
    case GroupBool of
	true ->
	    snmp_compile_lib:add_cdata(#cdata.objectgroups,
				       [{Name,GroupObjects,Line}]),
	    %% Check that the group members has been defined 
	    %% and that they have the correct status
	    snmp_compile_lib:check_object_group(Name, GroupObjects,
						Line, Status);
	_ ->
	    ok
    end,
    definitions_loop(T, Deprecated);

definitions_loop([{{notification_group,Name,GroupObjects,Status},Line}
		  |T], Deprecated) ->
    l("defloop -> notification_group ~p: ~n"
      "   Status:       ~p~n"
      "   GroupObjects: ~p~n"
      "   Line:         ~p",[Name,Status,GroupObjects,Line]),
    ensure_macro_imported('NOTIFICATION-GROUP', Line),
    GroupBool = get_group_check(get(options)),
    case GroupBool of
	true ->
	    snmp_compile_lib:add_cdata(#cdata.notificationgroups,
				       [{Name,GroupObjects,Line}]),

	    %% Check that the group members has been defined 
	    %% and that they have the correct status
	    snmp_compile_lib:check_notification_group(Name, GroupObjects,
						      Line, Status);
	_ ->
	    ok
    end,
    definitions_loop(T, Deprecated);

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline}, Entry,Seq|T],
		Deprecated) ->
    l("defloop -> object_type (sequence_of)~n"
      "   NameOfTable: ~p~n"
      "   SeqName:     ~p~n"
      "   Tline:       ~p~n"
      "   Entry:       ~p~n"
      "   Seq:         ~p",
      [NameOfTable,SeqName,Tline,Entry,Seq]),
    update_status(NameOfTable, Tstatus),
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
		    i("defloop -> Invalid sequence: Else = ~p",[Else]),
		    snmp_compile_lib:print_error(
		      "Invalid SEQUENCE OF '~p'.",
		      [safe_elem(1,safe_elem(2,Seq))],Tline)
	    end;
	Else ->
	    i("defloop -> Invalid table entry: Else = ~p",[Else]),
	    snmp_compile_lib:print_error(
	      "Invalid TableEntry '~p' (check STATUS, Sequence name, Oid)",
	      [safe_elem(1,safe_elem(2,Entry))],Tline)
    end,
    definitions_loop(T, Deprecated);

definitions_loop([{{object_type,NameOfTable,
		    {{sequence_of, SeqName},_},Taccess,Kind,
		    Tstatus,Tindex},Tline}|T],
		Deprecated) ->
    l("defloop -> object_type (sequence_of):~n"
      "   object_type: ~p~n"
      "   sequence_of: ~p~n"
      "   Tline:       ~p",[NameOfTable,SeqName,Tline]),
    update_status(NameOfTable, Tstatus),
    snmp_compile_lib:print_error("Invalid statements following table ~p.",
				 [NameOfTable],Tline),
    definitions_loop(T, Deprecated);

definitions_loop([{{sequence,SeqName,{fieldList,FieldList}},Line}|T],
		 Deprecated) ->
    l("defloop -> sequence (fieldList):~n"
      "   SeqName: ~p~n"
      "   Line:    ~p",[SeqName,Line]),
    w("Unexpected SEQUENCE ~w, ignoring.",[SeqName],Line),
    definitions_loop(T, Deprecated);

definitions_loop([{Obj,Line}|T], Deprecated) ->
    i("defloop -> unknown Error ~n"
      "   Obj:  ~p~n"
      "   Line: ~p",[Obj,Line]),
    snmp_compile_lib:print_error("Unknown Error in MIB. "
	 "Can't describe the error better than this: ~999p ignored."
	 " Please send a trouble report to support@erlang.ericsson.se.",
				 [Obj],Line),
    definitions_loop(T, Deprecated);

definitions_loop([], _Deprecated) ->
    l("defloop -> done",[]),
    ok.

safe_elem(N,T) ->
    case catch(element(N,T)) of
	{'EXIT',_} ->
	    "no more information available";
	X -> X
    end.

%% A correct column
define_cols([{{object_type,NameOfCol,Type1,Access,{variable,Defval},Status,
	       {'DESCRIPTION', Message1},
	       {NameOfEntry,[SubIndex]}},Oline}|Rest],SubIndex,
	 [{NameOfCol,Type2}|Fields],NameOfEntry,TableName,ColMEs) ->
    l("defcols -> object_type (variable):~n"
      "   NameOfCol:  ~p~n"
      "   Type1:      ~p~n"
      "   Access:     ~p~n"
      "   Status      ~p~n"
      "   NameOfEntry ~p~n"
      "   Oline:      ~p",
      [NameOfCol,Type1,Access,Status,NameOfEntry,Oline]),
    update_status(NameOfCol, Status),
    Deprecated = get_deprecated(get(options)),
    ASN1type = snmp_compile_lib:make_ASN1type(Type1),
    case (snmp_compile_lib:make_ASN1type(Type2))#asn1_type.bertype of
	T2 when T2 == ASN1type#asn1_type.bertype -> ok;
	Else ->
	    snmp_compile_lib:error(
	      "Types for ~p differs from the SEQUENCE definition. ",
	      [NameOfCol],Oline)
    end,
    NewAccess = % a simple way to get the obsolete behaviour
	if
	    Status == obsolete ->
		%% Be quiet and don't implement
		'not-accessible';
	    Status == deprecated, Deprecated == false ->
		%% The compiler chooses not to implement the column.
		i("object_type ~w is deprecated => ignored",
		  [NameOfCol],Oline),
		'not-accessible';
	    true -> Access
	end,
    snmp_compile_lib:register_oid(Oline,NameOfCol,NameOfEntry,[SubIndex]),
    Description1 = make_description(Message1),
    ColumnME = snmp_compile_lib:resolve_defval(
		 #me{oid         = SubIndex,
		     aliasname   = NameOfCol, 
		     asn1_type   = ASN1type,
		     entrytype   = table_column, 
		     access      = NewAccess,
		     description = Description1,
		     assocList   = [{table_name,TableName} | Defval]}),
    define_cols(Rest,SubIndex+1,Fields,NameOfEntry,TableName,
		[ColumnME|ColMEs]);

%% A "hole" (non-consecutive columns) in the table.
%% Implemented as a not-accessible column so Col always is index in
%% row tuple.
define_cols([{{object_type,NameOfCol,Type1,Access,Kind,Status,
	        {'DESCRIPTION', Message},
	    {NameOfEntry,[SubIndex]}},Oline}|Rest],ExpectedSubIndex,
	    Fields,NameOfEntry,TableName,ColMEs) 
  when SubIndex > ExpectedSubIndex ->
    l("defcols -> object_type (non consecutive cols):~n"
      "   NameOfCol:  ~p~n"
      "   Type1:      ~p~n"
      "   Access:     ~p~n"
      "   Status      ~p~n"
      "   NameOfEntry ~p~n"
      "   Oline:      ~p",
      [NameOfCol,Type1,Access,Status,NameOfEntry,Oline]),
    update_status(NameOfCol, Status),
    Int = {{type, 'INTEGER'},Oline},
    GeneratedColumn =  
	%% be sure to use an invalid column name here!
	{{object_type, '$no_name$', Int, 'not-accessible',
	  {variable, [{defval,0}]}, current, {'DESCRIPTION', undefined},
	  {NameOfEntry, [ExpectedSubIndex]}}, Oline},
    define_cols([GeneratedColumn, 
                {{object_type, NameOfCol, Type1, Access, Kind, Status,
                   {'DESCRIPTION', undefined},
		   {NameOfEntry,[SubIndex]}},Oline}|Rest],ExpectedSubIndex,
		[{'$no_name$', Int}|Fields],NameOfEntry,
		TableName,ColMEs) ;

%% Ok. done. All fields are eaten.
define_cols(Rest,SubIndex,[],NameOfEntry,TableName,ColMEs) ->
    {ColMEs,Rest};
%% Error Handling

%% The name of the field and object is the same
define_cols([{{object_type,NameOfCol,Type1,Access,Kind,Status,
	        {'DESCRIPTION', Message},
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
	        {'DESCRIPTION', Message},
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
    
%% Snmp_parse takes a text file as a input and the output is a list of tokens. 
%% Input: FileName (file of mibs)
%% Output: {ok, Mib} where MIB is a tuple of Tokens.
%%         {error, {LineNbr, Mod, Msg} an error on line number LineNb.


snmp_parse(FileName) ->
    case snmp_tok:start_link(reserved_words(),
			     [{file, FileName ++ ".mib"},
			      {forget_stringdata, true}]) of
	{error,ReasonStr} ->
	    snmp_compile_lib:error(lists:flatten(ReasonStr),[]);
	{ok, TokPid} ->
	    Toks = snmp_tok:get_all_tokens(TokPid),
	    set_version(Toks),
%	    debug("Lexical analysis:", Toks),
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

