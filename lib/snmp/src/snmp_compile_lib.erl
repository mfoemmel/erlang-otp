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
-module(snmp_compile_lib).

%% API
-export([test_father/4,make_ASN1type/1,import/1, makeInternalNode2/2,
	 is_consistent/1, resolve_defval/1, make_variable_info/1,
	 check_trap_name/3, make_table_info/4, get_final_mib/2, set_dir/2,
	 look_at/1, add_cdata/2,check_access_group/1,check_def/3,
	 check_notification_trap/1,register_oid/4,error/2,error/3,warning/2,
	 warning/3,print_error/2,print_error/3,make_cdata/1,
	 trap_variable_info/3, check_notification/3,
	 key1search/2,key1search/3]).

%% internal exports
-export([check_of/1, check_trap/2,check_trap/3,get_elem/2]).

%% debug exports
-export([vvalidate/1, i/2, l/2, d/2, t/2]).


-include("snmp_types.hrl").
-include("snmp_generic.hrl").
-include("snmp_compile.hrl").

test_father(FatherName, NewVarName, SubIndex, Line) ->
    CDATA = get(cdata),
    case lists:keysearch(FatherName, #me.aliasname, CDATA#cdata.mes) of
	{value, #me{entrytype = table, aliasname = TableName}} ->
	    print_error("Variable '~w' (sub-index '~w') cannot "
			"be defined under table '~w'.",
			[NewVarName, SubIndex, TableName],Line);
	{value, #me{entrytype = table_entry, aliasname = TableName}} ->
	    print_error("Variable '~w' (sub-index '~w') cannot "
			"be defined under table entry '~w'.",
			[NewVarName, SubIndex, TableName], Line);
	
	X -> %% internal or variable
	    case lists:last(SubIndex) of
		0 ->
		    print_error("'~w'. A zero-valued final subidentifier is reserved for future use. (RFC1902, 7.10)",[NewVarName],Line);
		_ -> ok
	    end
    end.

make_ASN1type({{type,Type},Line}) ->
    case lookup_vartype(Type) of
        {value,ASN1type} ->  ASN1type;
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
	    guess_integer_type()
    end;
make_ASN1type({{type_with_size,Type,{range,Lo,Hi}},Line}) ->
    case lookup_vartype(Type) of
        {value,ASN1type} ->  
	    case allow_size_rfc1902(BaseType = ASN1type#asn1_type.bertype) of
		true ->
		    ok;
		false ->
		    print_error(
		      "Size refinement is not allowed for subclass from ~w.",
		      [BaseType],Line)
	    end,
	    ASN1type#asn1_type{lo = Lo, hi = Hi};
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
	    guess_string_type()
    end;
make_ASN1type({{integer_with_enum,Type,Enums},Line}) ->
    case lookup_vartype(Type) of
        {value,ASN1type} ->  ASN1type#asn1_type{assocList = [{enums, Enums}]};
	false ->
	    print_error("Undefined type '~w'",[Type],Line),
    	    guess_integer_type()
    end;
make_ASN1type({{bits,Kibbles},Line}) ->
    case get(snmp_version) of
	2 ->
	    {value,Bits} = lookup_vartype('BITS'),
	    Kibbles2 = test_kibbles(Kibbles, Line),
	    Bits#asn1_type{assocList = [{kibbles, Kibbles2}]};
	_ ->
	    guess_integer_type()
    end;
make_ASN1type({{sequence_of,Type},Line}) ->
    print_error("Use of SEQUENCE OF in non-table context.",[],Line),
    guess_integer_type().

test_kibbles([], Line) ->
    print_error("No kibbles found.",[],Line),
    [];
test_kibbles(Kibbles,Line) ->
    test_kibbles2(R = lists:keysort(2,Kibbles),0,Line),
    R.

test_kibbles2([],_,_) ->
    ok;
test_kibbles2([{KibbleName,BitNo}|Ks],BitNo,Line) ->
    test_kibbles2(Ks,BitNo+1,Line);
test_kibbles2([{KibbleName,BitNo}|Ks],ExpectBitNo,Line) ->
    print_error("Expected kibble no ~p but got ~p.",[ExpectBitNo,BitNo],Line).
    
    
allow_size_rfc1902('INTEGER') -> true;
allow_size_rfc1902('Integer32') -> true;
allow_size_rfc1902('Unsigned32') -> true;
allow_size_rfc1902('OCTET STRING') -> true;
allow_size_rfc1902('Gauge32') -> true;
allow_size_rfc1902(_) -> false.

guess_integer_type() ->
    {value,ASN1int} = lookup_vartype('INTEGER'),
    ASN1int.

guess_string_type() ->
    {value,ASN1str} = lookup_vartype('OCTET STRING'),
    ASN1str.

lookup_vartype(Type) ->
    CDATA = get(cdata),
    lists:keysearch(Type, #asn1_type.aliasname, CDATA#cdata.asn1_types).




%%--------------------------------------------------
%% Reads the oid-function files.
%% Out: A list of {oid, entry}.
%% oid is here either a Oid with integers, or
%% with symbolic names.
%% entry is {M,F,A}.
%%--------------------------------------------------
read_funcs(FileName) ->
    case snmp_misc:read_noexit(FileName,m(check_of)) of
	{ok, Res} -> Res;
	{error, LineNo, Reason} ->
	    print_error("~p: ~w: Syntax error: ~p", [FileName, LineNo, Reason]),
	    [];
        {error, open_file} -> []
    end.

check_of({Oid, {M, F, A}}) when list(A) ->
    {ok, {Oid, {M, F, A}}};
check_of({Oid, {M, F, A}})  ->
    {invalid_argument, A};
check_of(X) ->
    {invalid_func, X}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for IMPORT implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

import(ImportList) ->
    lists:foreach(fun import_mib/1, ImportList).

%%----------------------------------------------------------------------
%% Returns: <nothing> only side effect stuff.
%%----------------------------------------------------------------------
import_mib({{'SNMPv2-SMI', ImportsFromMib},Line}) ->
    Nodes = [makeInternalNode(internet, [1,3,6,1]),
	     makeInternalNode(directory, [1,3,6,1,1]),
	     makeInternalNode(mgmt, [1,3,6,1,2]),
	     makeInternalNode('mib-2', [1,3,6,1,2,1]),
	     makeInternalNode(transmission, [1,3,6,1,2,1,10]),
	     makeInternalNode(experimental, [1,3,6,1,3]),
	     makeInternalNode(private, [1,3,6,1,4]),
	     makeInternalNode(enterprises, [1,3,6,1,4,1]),
	     makeInternalNode(zeroDotZero, [0,0]),
	     makeInternalNode(security, [1,3,6,1,5]),
	     makeInternalNode(snmpV2, [1,3,6,1,6]),
	     makeInternalNode(snmpDomains, [1,3,6,1,6,1]),
	     makeInternalNode(snmpProxys,[1,3,6,1,6,2]),
	     makeInternalNode(snmpModules, [1,3,6,1,6,3])],
    Types = [#asn1_type{bertype = 'Integer32',
			aliasname = 'Integer32',lo = -2147483648,
			hi = 2147483647},
	     #asn1_type{bertype='IpAddress',aliasname='IpAddress',lo=4,hi=4},
	     #asn1_type{bertype = 'Counter32', aliasname = 'Counter32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype = 'Gauge32', aliasname = 'Gauge32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype = 'Unsigned32', aliasname = 'Unsigned32',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype = 'TimeTicks', aliasname = 'TimeTicks',
			lo = 0, hi=4294967295},
	     #asn1_type{bertype = 'Opaque', aliasname = 'Opaque'},
	     #asn1_type{bertype = 'Counter64', aliasname = 'Counter64',lo = 0,
			hi = 18446744073709551615}],
    Macros = ['MODULE-IDENTITY','OBJECT-IDENTITY','OBJECT-TYPE',
	      'NOTIFICATION-TYPE'],
    import_built_in_loop(ImportsFromMib,Nodes,Types,Macros,'SNMPv2-SMI',Line);
import_mib({{'RFC-1215', ImportsFromMib},Line}) ->
    Macros = ['TRAP-TYPE'],
    import_built_in_loop(ImportsFromMib, [],[],Macros,'RFC-1215', Line);
import_mib({{'RFC-1212', ImportsFromMib},Line}) ->
    Macros = ['OBJECT-TYPE'],
    import_built_in_loop(ImportsFromMib, [],[],Macros,'RFC-1212', Line);
import_mib({{'SNMPv2-TC', ImportsFromMib},Line}) ->
    TC = {builtin,'TEXTUAL-CONVENTION'},
    case lists:member(TC, ImportsFromMib) of
	true ->
	    import_built_in(TC,[],[],['TEXTUAL-CONVENTION'],
			    'SNMPv2-TC',Line);
	false ->
	    ok
    end,
    import_from_file({{'SNMPv2-TC', ImportsFromMib--[TC]},
		      Line});
import_mib({{'SNMPv2-CONF', ImportsFromMib},Line}) ->
    Macros = ['OBJECT-GROUP','NOTIFICATION-GROUP','MODULE-COMPLIANCE'],
    import_built_in_loop(ImportsFromMib,[],[],Macros,'SNMPv2-CONF',Line);
import_mib({{'RFC1155-SMI', ImportsFromMib},Line}) ->
    Nodes = [makeInternalNode(internet, [1,3,6,1]),
	     makeInternalNode(directory, [1,3,6,1,1]),
	     makeInternalNode(mgmt, [1,3,6,1,2]),
	     makeInternalNode(experimental, [1,3,6,1,3]),
	     makeInternalNode(private, [1,3,6,1,4]),
	     makeInternalNode(enterprises, [1,3,6,1,4,1])],
    Types = [#asn1_type{bertype = 'NetworkAddress',
			aliasname = 'NetworkAddress', lo = 4, hi = 4},
	     #asn1_type{bertype='Counter',aliasname='Counter',
			lo=0,hi=4294967295},
	     #asn1_type{bertype='Gauge',aliasname='Gauge',
			lo = 0, hi = 4294967295},
	     #asn1_type{bertype='IpAddress',aliasname='IpAddress',lo=4,hi=4},
	     #asn1_type{bertype = 'TimeTicks', aliasname = 'TimeTicks',
			lo = 0, hi=4294967295},
	     #asn1_type{bertype = 'Opaque', aliasname = 'Opaque'}],
    import_built_in_loop(ImportsFromMib,Nodes,Types,[],'RFC1155-SMI',Line);
import_mib({{MibName, ImportsFromMib},Line}) ->
    import_from_file({{MibName, ImportsFromMib},Line}).

import_built_in_loop(Objs, Nodes, Types, Macros, MibName, Line) ->
    lists:foreach(fun (Obj) ->
			 import_built_in(Obj,Nodes,Types,Macros,MibName,Line)
		 end, Objs).

import_from_file({{MibName, []},Line}) -> done;
import_from_file({{MibName, ImportsFromMib},Line}) ->
    Filename = atom_to_list(MibName) ++ ".bin",
    {value, Path} = snmp_misc:assq(i, get(options)),
    {value, LibPath} = snmp_misc:assq(il,get(options)),
    LibPath2 = include_lib(LibPath),
    Path2 = Path++LibPath2++[filename:join(code:priv_dir(snmp),"mibs"),
			     "./"],
    ImportedMib = case read_mib(Line,Filename, Path2) of
		      error ->
		       error("Could not import ~p from mib ~s. File not found. "
			     "Check that the MIB to be IMPORTED is compiled "
			     "and present in the import path.",
			     [ImportsFromMib,Filename],Line);
		      Mib -> Mib
		  end,
    lists:foreach(fun (ImpObj) -> import(ImpObj,ImportedMib) end,
		  ImportsFromMib).

import_built_in({_tag,Obj}, Nodes, Types, Macros, MibName, Line) ->
    case lookup(Obj, Nodes) of
	{value, ME} ->
	    register_oid(undef, ME#me.aliasname, root, ME#me.oid),
	    add_cdata(#cdata.mes, [ME#me{imported = true, oid = undefined}]);
	false ->
	    case lists:keysearch(Obj, #asn1_type.aliasname, Types) of
		{value, ASN1Type} ->
		    add_cdata(#cdata.asn1_types,
			      [ASN1Type#asn1_type{imported=true}]);
		false ->
		    case lists:member(Obj, Macros) of
			true ->
			    add_cdata(#cdata.imported_macros,[Obj]);
			false ->
			    print_error("Cannot find '~w' in mib '~s'.",
				  [Obj, MibName], Line)
		    end
	    end
    end.

include_lib([]) -> [];
include_lib([Dir|Dirs]) ->
    [Appl|Path] = filename:split(Dir),
    case code:lib_dir(Appl) of
	{error, Reason} ->
	    include_lib(Dirs);
	DirPath ->
	    [filename:join(DirPath,filename:join(Path))|include_lib(Dirs)]
    end.

%%----------------------------------------------------------------------
%% Returns: #mib
%%----------------------------------------------------------------------
read_mib(Line, Filename, []) ->
    error;
read_mib(Line, Filename, [Dir|Path]) ->
    Dir2 = snmp_misc:ensure_trailing_dir_delimiter(Dir),
    case snmp_misc:read_mib(AbsFile=lists:append(Dir2, Filename)) of
	{ok, MIB} -> MIB;
	{error, enoent} ->
	    read_mib(Line, Filename, Path);
	{error, Reason} ->
	    warning("~s found but not imported. Reason: ~p.",[AbsFile,Reason]),
	    read_mib(Line, Filename, Path)
    end.


%%----------------------------------------------------------------------
%% imports ME or Type from other Mib into current compilation data.
%%----------------------------------------------------------------------
import({node, NodeName}, #mib{mes = IMES, name = MibName}) ->
    case lookup(NodeName, IMES) of
	{value, ME} when ME#me.imported == false ->
	    register_oid(undef, ME#me.aliasname, root, ME#me.oid),
	    add_cdata(#cdata.mes, [ME#me{imported = true}]);
	_ ->
	    print_error("Cannot find '~w' among the objects in the mib '~s'.",
			[NodeName, MibName])
    end;
import({type, TypeName}, #mib{asn1_types = Types, name = MibName}) ->
    case lists:keysearch(TypeName, #asn1_type.aliasname, Types) of
	{value, ASN1Type} when record(ASN1Type, asn1_type),
                          ASN1Type#asn1_type.imported == false ->
	    add_cdata(#cdata.asn1_types, [ASN1Type#asn1_type{imported=true,
							aliasname=TypeName}]);
	X ->
	    print_error("Cannot find '~w' among the types in the mib '~s'.",
			[TypeName, MibName])
    end;
import({builtin, Obj}, #mib{}) ->
    print_error("~p should be imported from a standard mib.",[Obj]).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for initialisation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Types defined in rfc1155 (SMI) are hard coded.
init_types() ->
    VerDep = case get(snmp_version) of
	     1 -> [];
	     2 ->
		  [#asn1_type{imported=true,bertype='BITS',aliasname='BITS'}]
	     end,
    [#asn1_type{imported = true, bertype = 'INTEGER', aliasname = 'INTEGER'},
     #asn1_type{imported=true,bertype='OCTET STRING',aliasname='OCTET STRING'},
     #asn1_type{imported=true,bertype='BIT STRING',aliasname='BIT STRING'},
     #asn1_type{imported = true, bertype = 'OBJECT IDENTIFIER',
		aliasname = 'OBJECT IDENTIFIER'} | VerDep].
     
makeInternalNode(Name, Oid) -> 
    makeInternalNode3(false, Name, Oid).

makeInternalNode2(Imported, Name) ->
    #me{imported = Imported, aliasname = Name, entrytype = internal}.

makeInternalNode3(Imported, Name, Oid) ->
    #me{imported = Imported, oid = Oid, aliasname = Name, entrytype = internal}.

make_cdata(MibFuncsFile) ->
    #cdata{mibfuncs = read_funcs(MibFuncsFile), asn1_types = init_types(),
	  oid_ets = ets:new(oid_ets, [set, private])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for Intermib consistency checking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_consistent(Filenames) ->
    case catch check_all_consistency(Filenames) of
	ok -> ok;
	{undef, Format, Data} ->
	    ok = io:format(Format, Data),
	    io:format("~n"),
	    {error, inconsistent}
    end.
    
check_all_consistency(Filenames) ->
    MIBs = lists:map(fun load_mib/1, Filenames),
    check_oid_conflicts(MIBs),
    check_trap_conflicts(MIBs),
    ok.

check_oid_conflicts(MIBs) ->
    MEs = lists:append(lists:map(m(get_elem), [#mib.mes], MIBs)),
    SortedMEs = lists:keysort(#me.oid, MEs),
    search_for_dublettes2(#me{aliasname=dummy_init}, SortedMEs).

check_trap_conflicts(MIBs) ->
    Traps = lists:append(lists:map(m(get_elem), [#mib.traps], MIBs)),
    snmp_misc:foreach(m(check_trap), [Traps], Traps).

check_trap(Trap, Traps) ->
    case lists:member(error,
		      lists:map(m(check_trap),[Trap,undef],
				lists:delete(Trap,Traps))) of
	true ->
	    throw({undef,"",[]});
	false ->
	    ok
    end.

%%----------------------------------------------------------------------
%% Returns: {Oid, ASN1Type}
%%----------------------------------------------------------------------
trap_variable_info(VariableName, Line, MEs) ->
    case lookup(VariableName, MEs) of
	false ->
	    error("Error in trap definition. Cannot find object '~w'.",
		  [VariableName], Line);
	{value, ME} when ME#me.entrytype == variable ->
	    {{variable, ME#me.aliasname}, ME#me.asn1_type};
	{value, ME} ->
	    {{column, ME#me.aliasname}, ME#me.asn1_type}
    end.

get_elem(MIB, Idx) ->
    element(Idx, MIB).

load_mib(Filename) ->
    F1 = snmp_misc:strip_extension_from_filename(Filename, ".mib"),
    F2 = lists:append(F1, ".bin"),
    case snmp_misc:read_mib(F2) of
	{error, Reason} ->
	    throw({undef, "Error reading file: ~w. Reason:~w", [F1, Reason]});
	{ok, Mib} ->
	    Mib
    end.

search_for_dublettes2(_PrevME, []) -> ok;
search_for_dublettes2(PrevME, [ME|MEs])
            when ME#me.imported==true ->
    search_for_dublettes2(PrevME, MEs);
search_for_dublettes2(PrevME, [ME|MEs]) 
    when PrevME#me.oid == ME#me.oid ->
    if PrevME#me.entrytype == internal, ME#me.entrytype == internal,
       PrevME#me.aliasname == ME#me.aliasname ->
	    search_for_dublettes2(ME, MEs);
       true ->
	    throw({undef,"Multiple used object with OBJECT IDENTIFIER '~w'"
			  " Used by '~w' and '~w' ", [PrevME#me.oid,
						      PrevME#me.aliasname, 
						      ME#me.aliasname]})
    end;
search_for_dublettes2(PrevME, [ME|MEs]) ->
    search_for_dublettes2(ME, MEs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for handling of default value resolving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resolve_defval(ME) ->
    case has_complex_defval(ME) of
	true ->
	    CDATA = get(cdata),
	    resolve_complex_defval(ME, CDATA#cdata.mes);
	false -> ME
    end.

has_complex_defval(#me{aliasname=N,assocList=AssocList, asn1_type=ASN1Type}) 
           when list(AssocList) ->
    case snmp_misc:assq(defval, AssocList) of
	{value, Int} when integer(Int) ->
	    false;
	{value, Val}
           when atom(Val), ASN1Type#asn1_type.bertype == 'OBJECT IDENTIFIER' ->
	    false; % resolved in update_me_oids
	{value, Val}
           when atom(Val), ASN1Type#asn1_type.bertype == 'INTEGER' ->
	    true;
	{value,Bits}
           when list(Bits), ASN1Type#asn1_type.bertype == 'BITS' ->
	    true;
	{value,Str} 
	when list(Str), ASN1Type#asn1_type.bertype == 'OCTET STRING' ->
	    false; % but ok
	{value,Str} 
	when list(Str), ASN1Type#asn1_type.bertype == 'Opaque' ->
	    false; % but ok
	{value,Str} when list(Str), length(Str) == 4,
	ASN1Type#asn1_type.bertype == 'IpAddress' ->
	    false; % but ok
	{value,Shit} ->
	    print_error("Bad default value for ~p: ~p.",[N,Shit]),
	    false;
	false -> %% no defval (or strings nyi)
	    false
    end;
has_complex_defval(_) -> false.

resolve_complex_defval(ME, _AllMEs) 
     when (ME#me.asn1_type)#asn1_type.bertype == 'INTEGER' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    {value, DefVal} = snmp_misc:assq(defval, AssocList),
    #asn1_type{bertype = TypeName,
	       assocList = AssocListForASN1Type} = ME#me.asn1_type,
    case snmp_misc:assq(enums, AssocListForASN1Type) of
	false ->
	    print_error("Type '~w' has no defined enums. "
			"Used in DEFVAL for '~w'.", [TypeName, MEName]),
	    ME;
	{value, Enums} ->
	    case snmp_misc:assq(DefVal, Enums) of
		false ->
		    print_error("Enum '~w' not found. "
				"Used in DEFVAL for '~w'.", [DefVal, MEName]),
		    ME;
		{value, IntVal} when integer(IntVal) ->
		    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
						       {defval, IntVal})}
	    end
    end;

resolve_complex_defval(ME, _AllMEs) 
     when (ME#me.asn1_type)#asn1_type.bertype == 'BITS' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    {value, DefVal} = snmp_misc:assq(defval, AssocList),
    #asn1_type{bertype = TypeName,
	       assocList = AssocListForASN1Type} = ME#me.asn1_type,
    {value, Kibbles} = snmp_misc:assq(kibbles, AssocListForASN1Type),
    case snmp_misc:bits_to_int(DefVal,Kibbles) of
	error->
	    print_error("Invalid default value ~w for ~w.",[DefVal, MEName]),
	    ME;
	IntVal when integer(IntVal) ->
	    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
					       {defval, IntVal})}
    end.


make_variable_info(#me{asn1_type = Asn1Type, assocList = Alist}) ->
    Defval = 
	case snmp_misc:assq(defval, Alist) of
	    {value, Val} -> Val;
	    _ -> get_def(Asn1Type#asn1_type.bertype, Asn1Type#asn1_type.lo)
	end,
    #variable_info{defval = Defval}.

get_def('INTEGER', Lo) when integer(Lo) -> Lo;
get_def('INTEGER', _) -> 0;
get_def('Counter', _) -> 0;
get_def('Gauge', _) -> 0;
get_def('TimeTicks', _) -> 0;
get_def('OCTET STRING', _) -> "";
get_def('IpAddress', _) -> [0,0,0,0];
get_def('NetworkAddress', _) -> [0,0,0,0];
get_def('OBJECT IDENTIFIER', _) -> [0, 0];
get_def('Opaque', _) -> "";
%v2
get_def('Integer32',Lo) when integer(Lo) -> Lo;
get_def('Integer32',_) -> 0;
get_def('Counter32',_) -> 0;
get_def('Gauge32',_) -> 0;
get_def('Unsigned32',_) -> 0;
get_def('BITS',_) -> 0;
get_def('Counter64',_) -> 0.

check_trap_name(EnterpriseName, Line, MEs) ->
    case lists:keysearch(EnterpriseName, #me.aliasname, MEs) of
	false -> error("Error in trap definition. Cannot find object '~w'.",
			[EnterpriseName],Line);
	{value, _} -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for table functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% This information is needed to be able to create default instrumentation
%% functions for tables.
%%----------------------------------------------------------------------
make_table_info(Line, TableName, {augments,SrcTableEntry}, ColumnMEs) ->
    ColMEs = lists:keysort(#me.oid, ColumnMEs),
    Nbr_of_Cols = length(ColMEs),
    MEs = ColMEs ++ (get(cdata))#cdata.mes,
    Aug = case lookup(SrcTableEntry,MEs) of
	      false ->
		  print_error("Cannot AUGMENT the non-existing table entry ~p",
			      [SrcTableEntry],Line),
		  {augments, error};
	      {value,ME} ->
		  {augments, {SrcTableEntry,translate_type(ME#me.asn1_type)}}
	  end,
    #table_info{index_types=Aug};
make_table_info(Line, TableName, {indexes,[]}, ColumnMEs) ->
    print_error("Table ~w lacks indexes.", [TableName],Line),
    #table_info{};
make_table_info(Line, TableName, {indexes,Indexes}, ColumnMEs) ->
    ColMEs = lists:keysort(#me.oid, ColumnMEs),
    NonImpliedIndexes = lists:map(fun non_implied_name/1, Indexes),
    test_read_create_access(ColMEs, Line, dummy),
    NonIndexCol = test_index_positions(Line, NonImpliedIndexes, ColMEs),
    Nbr_of_Cols = length(ColMEs),
    ASN1Indexes = find_asn1_types_for_indexes(Indexes, ColMEs, Line),
    FA = first_accessible(TableName, ColMEs),
    StatCol = find_status_col(Line, TableName, ColMEs),
    NoAccs = list_not_accessible(NonIndexCol,ColMEs),
    case lists:member(StatCol,NoAccs) of
	true ->
	    print_error("Status column cannot be not-accessible. In table ~p.",
			[TableName],Line);
	false -> ok
    end,
    #table_info{nbr_of_cols = Nbr_of_Cols,
		first_own_index = find_first_own_index(NonImpliedIndexes,
						       ColMEs, 1),
		status_col = StatCol,
		first_accessible = FA,
		not_accessible = NoAccs,
		index_types = ASN1Indexes}.

%% Perkins p110
test_read_create_access([#me{aliasname = N, access = 'read-create'}|ColMEs],
			Line, 'read-write') ->
    print_error("Column ~p cannot be read-create when another is read-write.",
		[N], Line);
test_read_create_access([#me{aliasname = N, access = 'read-write'}|ColMEs],
		       Line, 'read-create') ->
    print_error("Column ~p cannot be read-write when another is read-create.",
		[N], Line);
test_read_create_access([#me{access = 'read-write'}|ColMEs],Line,OtherStat) ->
    test_read_create_access(ColMEs,Line,'read-write');
test_read_create_access([#me{access = 'read-create'}|ColMEs],Line,OtherStat) ->
    test_read_create_access(ColMEs,Line,'read-create');
test_read_create_access([_ME|ColMEs],Line,OtherStat) ->
    test_read_create_access(ColMEs,Line,OtherStat);
test_read_create_access([], Line, _) -> ok.    

find_status_col(Line, TableName, []) ->
    undefined;
find_status_col(Line, TableName,
		[#me{oid=Oid,
		     asn1_type=#asn1_type{aliasname='RowStatus'}}|_]) ->
    1;
find_status_col(Line, TableName, [_ShitME | MEs]) ->
    case find_status_col(Line, TableName, MEs) of
	undefined -> undefined;
	N -> 1+N
    end.

list_not_accessible(none,_) -> [];
list_not_accessible(NonIndexCol, ColMEs) when integer(NonIndexCol) ->
    list_not_accessible(lists:nthtail(NonIndexCol - 1,ColMEs)).

list_not_accessible([#me{access='not-accessible', oid=Col}|ColMEs]) ->
    [Col | list_not_accessible(ColMEs)];
list_not_accessible([_ColME|ColMEs]) ->
    list_not_accessible(ColMEs);
list_not_accessible([]) ->
    [].

%%----------------------------------------------------------------------
%% See definition of first_own_index in the table_info record definition.
%%----------------------------------------------------------------------
find_first_own_index([], _ColMEs, _FOI) -> 0;
find_first_own_index([NameOfIndex | Indexes], ColMEs, FOI) ->
    case lists:keysearch(NameOfIndex, #me.aliasname, ColMEs) of
	{value, _ME} ->
	    FOI;
	false ->
	    find_first_own_index(Indexes, ColMEs, FOI + 1)
    end.

first_accessible(TableName, []) ->
    error("Table '~w' must have at least one accessible column.",[TableName]);
first_accessible(TableName, [#me{access = 'not-accessible'} | T]) ->
    first_accessible(TableName, T);
first_accessible(TableName, [#me{oid = Col} | _]) ->
    Col.

get_defvals(ColMEs) ->
    lists:keysort(1, 
        lists:filter(fun drop_undefined/1,
		     lists:map(fun column_and_defval/1, ColMEs))).

find_asn1_types_for_indexes(Indexes, ColMEs,Line) ->
    MEs = ColMEs ++ (get(cdata))#cdata.mes,
    test_implied(Indexes, Line),
    lists:map(fun (ColumnName) ->
		      translate_type(get_asn1_type(ColumnName, MEs,Line))
	      end, 
	      Indexes).

test_implied([],_) -> ok;
test_implied([{implied, Type},OtherIndexElem|_], Line) ->
    print_error("Implied must be last.", [], Line);
test_implied([{implied, Type}], Line) -> ok;
test_implied([H|T], Line) -> test_implied(T, Line).

drop_undefined({X, undefined}) -> false;
drop_undefined({X, Y}) -> true;
drop_undefined(undefined) -> false;
drop_undefined(X) -> true.

%% returns: {ColumnNo, Defval}
column_and_defval(#me{oid = Oid, assocList = AssocList}) ->
    ColumnNo = lists:last(Oid),
    case snmp_misc:assq(defval, AssocList) of
	false -> {ColumnNo, undefined};
	{value, DefVal} -> {ColumnNo, DefVal}
    end.

%% returns: an asn1_type if ColME is an indexfield, otherwise undefined.
get_asn1_type({implied,ColumnName}, MEs, Line) ->
    case lookup(ColumnName, MEs) of
	{value,#me{asn1_type=A}} when A#asn1_type.bertype ==
				      'OCTET STRING' ->
	    A#asn1_type{implied = true};
	{value,#me{asn1_type=A}} when A#asn1_type.bertype == 
				      'OBJECT IDENTIFIER' ->
	    A#asn1_type{implied = true};
	Shit ->
	    print_error("Only OCTET STRINGs and OIDs can be IMPLIED.", [], Line)
    end;
get_asn1_type(ColumnName, MEs, Line) ->
    case lookup(ColumnName, MEs) of
	{value,ME} -> ME#me.asn1_type;
	false -> error("Can't find object ~p. Used as INDEX in table.",
		       [ColumnName],Line)
    end.

test_index_positions(Line, Indexes, ColMEs) ->
    TLI = lists:filter(fun (IndexName) ->
			       is_table_local_index(IndexName,ColMEs) end,
		       Indexes),
    NonIndexCol = test_index_positions_impl(Line, TLI, ColMEs).

%% Returns the first non-index column | none
test_index_positions_impl(_Line, [], []) -> none;
test_index_positions_impl(_Line, [], [#me{oid=Col}|ColMEs]) ->
    Col;
test_index_positions_impl(Line, Indexes,
			  [#me{aliasname=Name, asn1_type=Asn1} | ColMEs]) ->
    case lists:member(Name, Indexes) of
	true ->
	    if Asn1#asn1_type.bertype == 'BITS' ->
		    print_error("Invalid data type 'BITS' for index '~w'.",
				[Name],Line);
	       true -> true
	    end,
	    test_index_positions_impl(Line,
					  lists:delete(Name, Indexes), ColMEs);
	false -> warning("Index columns must be first for "
			 "the default functions to work properly. "
			 "~w is no index column.", [Name], Line),
		 none
    end.

is_table_local_index(IndexName, ColMEs) ->
    case lists:keysearch(IndexName, #me.aliasname, ColMEs) of
	false -> false;
	Q -> true
    end.

non_implied_name({implied, IndexColumnName}) -> IndexColumnName;
non_implied_name(IndexColumnName) -> IndexColumnName.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for generationg the final mib
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% returns: {ok, 
%  {snmp_mib, MEs, traps, list of {TrapOid, list of oids (objects)}}}
get_final_mib(Name, Options) ->
    d("get_final_mib",[]),
    CDATA = get(cdata),
    #cdata{mes=MEs,mibfuncs=MibFuncs,asn1_types=Types,
	   traps=Traps,oid_ets=OidEts}=CDATA,
    d("get_final_mib -> resolve oids",[]),
    resolve_oids(OidEts),
    %% Reverse so that we get report on objects earlier in the file
    %% before later objects.
    UMEs = update_me_oids(lists:reverse(MEs), OidEts),
    t("get_final_mib -> "
      "~n   UMEs: ~p",[UMEs]),
    UTraps = update_trap_oids(Traps, OidEts),
    t("get_final_mib -> "
      "~n   UTraps: ~p",[UTraps]),
    SortedMEs = lists:keysort(#me.oid,UMEs),
    d("get_final_mib -> search for dublettes",[]),
    search_for_dublettes(#me{aliasname=dummy_init}, SortedMEs),

    d("get_final_mib -> search for oid conflicts",[]),
    search_for_oid_conflicts(UTraps,SortedMEs),

    d("get_final_mib -> resolve oid",[]),
    MibFs = lists:keysort(1,
      lists:map(fun (MF) -> resolve_oid(MF,SortedMEs) end, MibFuncs)),
    t("get_final_mib -> "
      "~n   MibFs: ~p",[MibFs]),
    {value, DBName} = snmp_misc:assq(db, Options),
    MEsWithMFA = insert_mfa(MibFs, SortedMEs, DBName),
    Misc = [{snmp_version,get(snmp_version)}
	    | case lists:member(no_symbolic_info,get(options)) of
		  true -> [no_symbolic_info];
		  false -> []
	      end],
    GroupBool = hd([Value || {Option,Value} <-get(options), 
			     Option==group_check]),
    case GroupBool==true of
	true->
	    case get(snmp_version)==2 of
		true->
		    d("get_final_mib -> check groups",[]),
		    check_group(CDATA#cdata.mes,CDATA#cdata.objectgroups),
		    d("get_final_mib -> check notifications",[]),
		    check_notification(UTraps,CDATA#cdata.notificationgroups);
		false->
		    ok
	    end;
	false->
	    ok
    end,
    Mib = #mib{name = Name, mes = lists:map(fun translate_me_type/1,
					    MEsWithMFA), misc=Misc,
	       variable_infos = extract_variable_infos(MEsWithMFA),
	       table_infos = extract_table_infos(MEsWithMFA),
	       traps = lists:map(fun translate_trap_type/1, UTraps), 
	       asn1_types = lists:map(fun translate_type/1, Types)},
    {ok, Mib}.
	      
	      

%% We don't want a zillion aliases for INTEGER (etc),
%% and since they are encoded with the same tag we can treat them as
%% equivalent.
%% The reason for having them at compile time is for example that
%% Unsigned32 is allowed as INDEX but not Gauge.
%% The compiler might want to ensure this and more...
translate_me_type(ME) ->
    ME#me{asn1_type = translate_type(ME#me.asn1_type)}.

translate_trap_type(Trap) when record(Trap,notification)->
    translate_trap_type_notif(Trap);
translate_trap_type(Trap) when record(Trap,trap)->
    translate_trap_type_trap(Trap).

translate_trap_type_notif(Trap)->
    NewOidobjects = 
	lists:map(fun({Oid,ASN1type}) ->{Oid,translate_type(ASN1type)} end,
		  Trap#notification.oidobjects),
    Trap#notification{oidobjects=NewOidobjects}.

translate_trap_type_trap(Trap)->
    NewOidobjects = 
	lists:map(fun({Oid,ASN1type}) ->{Oid,translate_type(ASN1type)} end,
		  Trap#trap.oidobjects),
    Trap#trap{oidobjects=NewOidobjects}.
    
translate_type(ASN1type) when ASN1type#asn1_type.bertype == 'NetworkAddress' ->
    ASN1type#asn1_type{bertype = 'IpAddress'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype == 'Integer32' ->
    ASN1type#asn1_type{bertype = 'INTEGER'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype == 'Counter' ->
    ASN1type#asn1_type{bertype = 'Counter32'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype == 'Gauge' ->
    ASN1type#asn1_type{bertype = 'Unsigned32'};
translate_type(ASN1type) when ASN1type#asn1_type.bertype == 'Gauge32' ->
    ASN1type#asn1_type{bertype = 'Unsigned32'};
translate_type(ASN1type) -> ASN1type.

%% Check for both NOTIFICATION-GROUP  and OBJECT-GROUP

check_def(Objects,[GroupObject|GroupObjects],Line)->
    case lists:member(GroupObject,Objects) of
	false->
	    print_error("OBJECT-TYPE definition missing or 'not-accessible' for '~w'",[GroupObject],Line),
	    check_def(Objects,GroupObjects,Line);
	true ->
	    check_def(Objects,GroupObjects,Line)
    end;
check_def(_Objects,[],_Line) ->
    ok.

%%Checking the definition of OBJECT-GROUP

check_access_group([])->[];
check_access_group([#me{access=A,entrytype=T,aliasname=Aliasname}|MEs]) when A=/='not-accessible',T=/='internal' ->
    [Aliasname|check_access_group(MEs)];
check_access_group([ME|Rest]) ->
    check_access_group(Rest).
%%-----------------------------
check_group([#me{imported = true} | T],GroupObjects)->
    check_group(T,GroupObjects);
check_group([],_GroupObjects) ->
    ok;
check_group([#me{access=A,entrytype=T,aliasname=Aliasname}|MEs],GroupObjects) when A=/='not-accessible',T=/='internal' ->
    check_member_group(Aliasname,GroupObjects),
    check_group(MEs,GroupObjects);
check_group([_|MEs],GroupObjects) ->
    check_group(MEs,GroupObjects).

check_member_group(Aliasname,[])->
    print_error("'~w' missing in OBJECT-GROUP",[Aliasname]);
check_member_group(Aliasname,[{Name,GroupObject,Line}|Tl])->
    case lists:member(Aliasname,GroupObject) of
	true->
	    ok;
	false ->
	    check_member_group(Aliasname,Tl)
    end.                                     
%% Checking definition in NOTIFICATION-GROUP

check_notification_trap([])->[];
check_notification_trap([#notification{trapname=Aliasname}|Traps]) ->
    [Aliasname|check_notification_trap(Traps)];
check_notification_trap([Trap|Rest]) ->
    check_notification_trap(Rest).
%%--------------------------
check_notification([],_NotificationObjects) ->
    ok;
check_notification([#notification{trapname=Aliasname}|Traps],NotificationObjects) ->
    check_member_notification(Aliasname,NotificationObjects),
    check_notification(Traps,NotificationObjects);
check_notification([_|Traps],NotificationObjects) ->
    check_notification(Traps,NotificationObjects).

check_member_notification(Aliasname,[])->
    print_error("'~w' missing in NOTIFICATION-GROUP",[Aliasname]);
check_member_notification(Aliasname,[{Name,NotificationObject,Line}|Tl])->
    case lists:member(Aliasname,NotificationObject) of
	true->
	    ok;
	false ->
	    check_member_notification(Aliasname,Tl)
    end.                               


%%----------------------------------------------------------------------
%% Purpose: Resolves oids for aliasnames used in .funcs file.
%% Returns: {Oid, X}
%%----------------------------------------------------------------------
resolve_oid({NameOrOid, X}, MEs) ->
    case lookup(NameOrOid, MEs) of
	{value, #me{oid=Oid,entrytype=variable}} -> {Oid, X};
	{value, #me{oid=Oid,entrytype=table}} -> {Oid, X};
	{value, #me{entrytype=table_entry}} ->
		error("Cannot associate an instrumentation function with a "
		      "Table Entry: ~w (must be table or variable)",
		      [NameOrOid]);
	{value, #me{entrytype=table_column}} ->
		error("Cannot associate an instrumentation function with a "
		      "Table Column: ~w (must be table or variable)",
		      [NameOrOid]);
	Q ->
	    error("Cannot find OBJECT-TYPE definition for '~w'.",
		  [NameOrOid])
    end.

%%----------------------------------------------------------------------
%% Fs is list of {Oid, {M,F,A}}
%% returns: MEs with access-functions.
%% Pre: Fs, MEs are sorted (on Oid) (then we can traverse mib efficiently)
%%----------------------------------------------------------------------
insert_mfa(Fs, [ME | MEs], DBName) when ME#me.imported == true ->
    [ME | insert_mfa(Fs, MEs, DBName)];

insert_mfa(Fs, [ME | MEs], DBName) 
             when ME#me.entrytype == internal ->
    [ME | insert_mfa(Fs, MEs, DBName)];

insert_mfa([X | Fs], [ME | MEs], DBName) 
             when ME#me.entrytype == variable ->
    {Oid, {M,F,A}} = X,
    case ME#me.oid of
	Oid ->
	    [ME#me{mfa = {M,F,A}}
	     | insert_mfa(Fs, MEs, DBName)];
	Q -> [insert_default_mfa(ME,DBName) | insert_mfa([X | Fs],MEs,DBName)]
    end;

insert_mfa([X | Fs], [TableME | MEs], DBName) 
 when TableME#me.entrytype == table ->
    {Oid, {M,F,A}} = X,
    {TableMEs, RestMEs} = collect_mes_for_table(TableME, [TableME | MEs]),
    [TableEntryME | ColMEs] = tl(TableMEs),
    DefVals = get_defvals(ColMEs),
    {value,TableInfo} = snmp_misc:assq(table_info,TableME#me.assocList),
    NAssocList = [{table_info, TableInfo#table_info{defvals = DefVals}} |
		  lists:keydelete(table_info, 1, TableME#me.assocList)],
    NTableME = TableME#me{assocList = NAssocList},
    case is_same_table(Oid, NTableME#me.oid) of
	true ->  % use mfa from .funcs
	    lists:append([NTableME,
			  TableEntryME#me{mfa = {M, F, A}}
			  | ColMEs],
			 insert_mfa(Fs, RestMEs, DBName));
	false ->
	    lists:append(insert_default_mfa([NTableME | tl(TableMEs)], DBName),
			 insert_mfa([X|Fs], RestMEs, DBName))
    end;

insert_mfa([], [ME|MEs], DBName) when ME#me.entrytype == variable ->
    [insert_default_mfa(ME, DBName) | insert_mfa([], MEs, DBName)];

insert_mfa([], [ME|MEs], DBName) when ME#me.entrytype == table ->
    {TableMEs, RestMEs} = collect_mes_for_table(ME, [ME|MEs]),
    [TableME, TableEntryME | ColMEs] = TableMEs,
    DefVals = get_defvals(ColMEs),
    {value,TableInfo} = snmp_misc:assq(table_info,TableME#me.assocList),
    NAssocList = [{table_info, TableInfo#table_info{defvals = DefVals}} |
		  lists:keydelete(table_info, 1, TableME#me.assocList)],
    NTableME = TableME#me{assocList = NAssocList},
    NewTableMEs = insert_default_mfa([NTableME | tl(TableMEs)], DBName),
    lists:append(NewTableMEs, insert_mfa([], RestMEs, DBName));

insert_mfa([], [], DBName) -> [];
insert_mfa([], [ME|MEs], DBName) ->
    error("Missing access-functions for '~w'.",[ME#me.aliasname]).

%%----------------------------------------------------------------------
%% Returns: {[TableME, TableEntryME | ColumnMEs], RestMEs}
%%----------------------------------------------------------------------
collect_mes_for_table(TableME, []) -> {[], []};

collect_mes_for_table(TableME, [ME|MEs]) ->
    case is_same_table(TableME#me.oid, ME#me.oid) of
	true ->
	    {TableMEs, RestMEs} = collect_mes_for_table(TableME, MEs),
	    {[ME | TableMEs], RestMEs};
	false ->
	    {[], [ME | MEs]}
    end.

%% returns: MibEntry with access-functions.
insert_default_mfa(ME, DBName) when record(ME, me)->
    warning("No accessfunction for '~w', using default.",
	    [ME#me.aliasname]),
    set_default_function(ME, DBName);

insert_default_mfa([TableME, EntryME | Columns], DBName) ->
    warning("No accessfunction for '~w', using default.",
	    [TableME#me.aliasname]),
    set_default_function([TableME, EntryME | Columns], DBName).

%% returns boolean.
is_same_table(Oid, TableOid) ->
    lists:prefix(Oid, TableOid).

%% returns false | {value, ME}
lookup(UniqName, MEs) when atom(UniqName) ->
    lists:keysearch(UniqName, #me.aliasname, MEs);
lookup(Oid, MEs) when list(Oid) ->
    lists:keysearch(Oid, #me.oid, MEs).

search_for_dublettes(PrevME, [ME|MEs])
            when PrevME#me.oid==ME#me.oid ->
    error("Multiple used object with OBJECT IDENTIFIER '~w'. "
	  "Used in '~w' and '~w'.", [PrevME#me.oid,
				     PrevME#me.aliasname,
				     ME#me.aliasname]);
search_for_dublettes(PrevME, [ME|MEs]) 
       when PrevME#me.entrytype == variable, ME#me.entrytype == variable ->
    case lists:prefix(PrevME#me.oid, ME#me.oid) of
	true ->
	    error("Variable '~w' (~w) defined below other "
		  "variable '~w' (~w). ",
		  [ME#me.aliasname, ME#me.oid,
		   PrevME#me.aliasname, PrevME#me.oid]);
	false ->
	    search_for_dublettes(ME, MEs)
    end;
search_for_dublettes(PrevME, [ME|MEs]) ->
    search_for_dublettes(ME, MEs);
search_for_dublettes(PrevME, []) -> ok.


search_for_oid_conflicts([Rec|Traps],MEs) when record(Rec,notification) ->
    #notification{oid = Oid, trapname = Name} = Rec,
    case search_for_oid_conflicts1(Oid,MEs) of
	{error,ME} ->
	    error("Notification with OBJECT IDENTIFIER '~w'. "
		  "Used in '~w' and '~w'.", [Oid,Name,ME#me.aliasname]);
	ok ->
	    search_for_oid_conflicts(Traps,MEs)
    end;
search_for_oid_conflicts([_Trap|Traps],MEs) ->
    search_for_oid_conflicts(Traps,MEs);
search_for_oid_conflicts([],MEs) ->
    ok.

search_for_oid_conflicts1(Oid,[]) ->
    ok;
search_for_oid_conflicts1(Oid,[ME|MEs]) when Oid == ME#me.oid ->
    {error,ME};
search_for_oid_conflicts1(Oid,[ME|MEs]) ->
    search_for_oid_conflicts1(Oid,MEs).

set_default_function([TableMe, EntryMe | ColMes], DBName) ->
    #me{aliasname = Name} = TableMe,
    check_rowstatus(TableMe),
    [TableMe,
     EntryMe#me{mfa = {snmp_generic, table_func, [{Name, DBName}]}} |
     ColMes];

set_default_function(MibEntry,DBName) when MibEntry#me.entrytype == variable ->
    #me{aliasname = Aname} = MibEntry,
    MibEntry#me{mfa = {snmp_generic, variable_func, [{Aname, DBName}]}}.

check_rowstatus(TableME) ->
    {value,TableInfo} = snmp_misc:assq(table_info,TableME#me.assocList),
    case TableInfo#table_info.status_col of
	undefined -> 
        warning("No RowStatus column in table ~w. "
		"The default functions won't work properly.",
		[TableME#me.aliasname]);
	Q -> ok
    end.

check_trap(#trap{trapname=N1, specificcode=C, enterpriseoid=E},
	   #trap{trapname=N2,specificcode=C,enterpriseoid=E},Line) ->
    print_error("Trap code collision. Enterprise: ~w. Trapcode: ~w, "
		"Name of traps: ~w, ~w.", [E, C, N1, N2],Line),
    error;
check_trap(#trap{trapname=N, specificcode=C1, enterpriseoid=E1},
	   #trap{trapname=N,specificcode=C2,enterpriseoid=E2},Line) ->
    print_error("Trap name collision. Name: ~w Enterprises: ~w, ~w. "
		"Trapcodes: ~w, ~w", [N, E1, E2, C1, C2],Line),
    error;
check_trap(OldTrap, ThisTrap, Line) ->
    ok.

check_notification(Notif, Line, Notifs) ->
    lists:map(fun (OtherNotif) ->
		      check_notification1(Notifs,OtherNotif,Line) end,
	      lists:delete(Notif,Notifs)).

check_notification1(#notification{trapname=N},#notification{trapname=N},Line)->
    print_error("Trap name collision for '~w.",[N],Line);
check_notification1(#notification{oid=Oid},#notification{oid=Oid},Line)->
    print_error("Trap oid collision for '~w.",[Oid],Line);
check_notification1(T1,T2,L) ->
    ok.

%%----------------------------------------------------------------------
%% Returns: list of {VariableName, variable_info-record}
%%----------------------------------------------------------------------
extract_variable_infos([]) -> [];
extract_variable_infos([#me{entrytype = variable, assocList = AL,
			    aliasname = Name} | T]) ->
    {value, VI} = snmp_misc:assq(variable_info, AL),
    [{Name, VI} | extract_variable_infos(T)];
extract_variable_infos([ME | T]) ->
    extract_variable_infos(T).

%%----------------------------------------------------------------------
%% Returns: list of {TableName, table_info-record}
%%----------------------------------------------------------------------
extract_table_infos([]) -> [];
extract_table_infos([#me{entrytype = table, assocList = AL,
			    aliasname = Name} | T]) ->
    {value, VI} = snmp_misc:assq(table_info, AL),
    [{Name, VI} | extract_table_infos(T)];
extract_table_infos([ME | T]) ->
    extract_table_infos(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for debug functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t(F,A) ->
    vprint(printable(get(verbosity),trace),F,A).

d(F,A) ->
    vprint(printable(get(verbosity),debug),F,A).

l(F,A) ->
    vprint(printable(get(verbosity),log),F,A).

i(F,A) ->
    vprint(printable(get(verbosity),info),F,A).


vprint(false,_F,_A) ->
    ok;
vprint(S,F,A) ->
    io:format(image_of_severity(S) ++ F ++ "~n",A).


printable(silence,_)   -> false;
printable(info,info)   -> info;
printable(log,info)    -> info;
printable(log,log)     -> log;
printable(debug,info)  -> info;
printable(debug,log)   -> log;
printable(debug,debug) -> debug;
printable(trace,S)     -> S;
printable(_V,_S)       -> false.


image_of_severity(info)  -> "I: ";
image_of_severity(log)   -> "L: ";
image_of_severity(debug) -> "D: ";
image_of_severity(trace) -> "T: ";
image_of_severity(_)     -> "".


vvalidate(silence) -> ok;
vvalidate(info)    -> ok;
vvalidate(log)     -> ok;
vvalidate(debug)   -> ok;
vvalidate(trace)   -> ok;
vvalidate(V)       -> exit({invalid_verbosity,V}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Section for misc useful functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m(M) -> {snmp_compile_lib, M}.

set_dir(File, NewDir) ->
    case string:chr(File, $/) of
	0 -> lists:append(NewDir, File);
	N -> set_dir(lists:nthtail(N,File), NewDir)
    end.

%% Look up a key in a list, and if found returns the value
%% or if not found returns the default value
key1search(Key, List) ->
    key1search(Key, List, undefined).

key1search(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {Key, Val}} -> Val;
        _ -> Default
    end.


%% print the compiled mib
look_at(FileName) ->
    case file:read_file(FileName) of
        {ok,Bin} -> 
	    binary_to_term(Bin);     
        {error,Reason} ->
            {error,Reason}
    end.


%% Data is appended to compiler information
add_cdata(OffsetInRecord, ListOfData) ->
    CDATA = get(cdata),
    OldData = element(OffsetInRecord, CDATA),
    put(cdata, setelement(OffsetInRecord, CDATA, lists:append(ListOfData,
							      OldData))),
    undefined.

check_sub_ids([H | T], Name, Line) when H < 0 ->
    error("OBJECT IDENTIFIER must have all sub "
	  "indexes > 0. Name: '~w'. Illegal sub index: ~w.",
	  [Name, H], Line);
check_sub_ids([H | T], Name, Line) when H > 4294967295 ->
    error("OBJECT IDENTIFIER must have all sub "
	  "indexes < 4294967295. Name: '~w'. Illegal sub index: ~w.",
	  [Name, H], Line);
check_sub_ids([H | T], Name, Line) ->
    check_sub_ids(T, Name, Line);
check_sub_ids([], Name, Line) ->
    ok.

%%-----------------------------------------------------------------
%% Handle forward references:
%% This code handles OIDs that are defined in terms of a
%% parent OID that is defined later in the file.  Ex:
%%   x OBJECT IDENTIFIER ::= {y 1}
%%   y OBJECT IDENTIFIER ::= {enterprises 1}
%% The following alg is used to handle this:
%% Datastructure:
%%    An ets table, with one entry for each object in the mib:
%%    {Name, FatherName, Line, SubIndex, Children}
%%       Name : aliasname
%%       FatherName : aliasname of parent object
%%       SubIndex : list of subindexes from parent object
%%       Children : list of aliasnames for all objects registered
%%                  under this one
%%    FatherName == 'root' => top-level object
%% 1) When an OID is found in the mib, it is registered using
%%    register_oid/4.  This function updates the parent entry,
%%    by adding the new name to its Children list.  It also
%%    updates the entry for the object defined.
%% 2) When all objects are registered, the ets table contains
%%    a directed graph of all objects.
%% 3) resolve_oids/1 is called.  This function traverses the
%%    graph, starting at 'root', and changes each entry to
%%    {Name, Line, Oid}, where Oid is a list of integers.
%% 4) The list of MibEntries is traversed.  Each object is
%%    looked up in the ets table, and the correspsonding oid
%%    is updated.  The functions for this is update_me_oids/2
%%    and update_trap_oids/2.
%%-----------------------------------------------------------------
register_oid(Line, Name, FatherName, SubIndex) when Name /= '$no_name$' ->
    check_sub_ids(SubIndex, Name, Line),
    OidEts = (get(cdata))#cdata.oid_ets,
    %% Lookup Father - if it doesn't already exist, create it
    {_FatherName, HisFatherName, HisLine, HisSubIndex, Children} = 
	case ets:lookup(OidEts, FatherName) of
	    [Found] -> Found;
	    [] -> {FatherName, undef, undef, [], []}
	end,
    %% Update Father with a pointer to us
    NChildren = case lists:member(Name, Children) of
		    true -> Children;
		    false -> [Name | Children]
		end,
    NFather = {FatherName, HisFatherName, HisLine, HisSubIndex,NChildren},
    ets:insert(OidEts, NFather),
    %% Lookup ourselves - if we don't exist, create us
    MyChildren =
	case ets:lookup(OidEts, Name) of
	    [Found2] -> element(5, Found2);
	    [] -> []
	end,
    %% Update ourselves
    NSelf = {Name, FatherName, Line, SubIndex, MyChildren},
    ets:insert(OidEts, NSelf);
register_oid(Line, Name, FatherName, SubIndex) ->
    ok.


resolve_oids(OidEts) ->
    [{_, _, _, _, RootChildren}] = ets:lookup(OidEts, root),
    resolve_oids(RootChildren, [], OidEts).

resolve_oids([Name | T], FatherOid, OidEts) ->
    {MyOid, MyChildren, MyLine} =
	case ets:lookup(OidEts, Name) of
	    [{Name, Oid, Line}] ->
		print_error("Circular OBJECT IDENTIFIER definitions "
			    "involving ~w\n", [Name], Line),
		{Oid, [], Line};
	    [{Name, Father, Line, SubIndex, Children}] ->
		{FatherOid ++ SubIndex, Children, Line}
	end,
    ets:insert(OidEts, {Name, MyOid, MyLine}),
    resolve_oids(T, FatherOid, OidEts),
    resolve_oids(MyChildren, MyOid, OidEts);
resolve_oids([], _, _) ->
    ok.
    
		 
update_me_oids([#me{aliasname = '$no_name$'} | Mes], OidEts) ->
    update_me_oids(Mes, OidEts);
update_me_oids([Me | Mes], OidEts) ->
    Oid = tr_oid(OidEts, Me#me.aliasname),
    NMe = resolve_oid_defval(Me, OidEts),
    [NMe#me{oid = Oid} | update_me_oids(Mes, OidEts)];
update_me_oids([], OidEts) ->
    [].

update_trap_oids([Trap | Traps], OidEts) when record(Trap, notification)->
    Oid = tr_oid(OidEts, Trap#notification.trapname),
    OidObjs = tr_oid_objs(Trap#notification.oidobjects, OidEts),
    [Trap#notification{oid = Oid, oidobjects = OidObjs} |
     update_trap_oids(Traps, OidEts)];
update_trap_oids([Trap | Traps], OidEts) ->
    NEnter = tr_oid(OidEts, Trap#trap.enterpriseoid),
    OidObjs = tr_oid_objs(Trap#trap.oidobjects, OidEts),
    [Trap#trap{enterpriseoid = NEnter,
	       oidobjects = OidObjs} | update_trap_oids(Traps, OidEts)];
update_trap_oids([], OidEts) ->
    [].

tr_oid(OidEts, Name) ->
    case ets:lookup(OidEts, Name) of
	[{Name, MyOid, MyLine}] ->
	    MyOid;
	[{Natrap, Father, Line, SubIndex, Children}] ->
	    print_error("OBJECT IDENTIFIER defined in terms "
			"of undefined parent object. Parent: '~w'."
			"(Sub-indexes: ~w.)",
			[Father, SubIndex],Line),
	    rnd_oid()
    end.

tr_oid_objs([{{variable, Name}, Type} | T], OidEts) ->
    Oid = tr_oid(OidEts, Name) ++ [0],
    [{Oid, Type} | tr_oid_objs(T, OidEts)];
tr_oid_objs([{{column, Name}, Type} | T], OidEts) ->
    Oid = tr_oid(OidEts, Name),
    [{Oid, Type} | tr_oid_objs(T, OidEts)];
tr_oid_objs([], _OidEts) ->
    [].
    

resolve_oid_defval(ME, OidEts) 
  when (ME#me.asn1_type)#asn1_type.bertype == 'OBJECT IDENTIFIER' ->
    #me{aliasname = MEName, assocList = AssocList} = ME,
    case snmp_misc:assq(defval, AssocList) of
	{value, DefVal} when atom(DefVal) ->
	    case ets:lookup(OidEts, DefVal) of
		[{_, Oid, _}] ->
		    ME#me{assocList = lists:keyreplace(defval, 1, AssocList,
						       {defval, Oid})};
		_ ->
		    print_error("Can not find OBJECT-TYPE definition for '~w' "
				"Used in DEFVAL for '~w'.", [DefVal, MEName]),
		    ME
	    end;
	_ ->
	    ME
    end;
resolve_oid_defval(ME, OidEts) ->
    ME.

rnd_oid() ->
    [99,99].  %% '99' means "stop computer" in Y2Kish...

error(FormatStr, Data) when list(FormatStr) ->
    print_error(FormatStr,Data),
    exit(error).

error(FormatStr, Data, Line) when list(FormatStr) ->
    print_error(FormatStr,Data,Line),
    exit(error).

warning(FormatStr, Data) when list(FormatStr) ->
    WarningBool=hd([Value || {Option,Value} <-get(options), Option==warnings]),
    case WarningBool==true of
	true->
	    io:format("~s: Warning: ", [get(filename)]),
	    ok = io:format(FormatStr,Data),
	    io:format("~n");
	false->
	    ok
    end.

warning(FormatStr, Data, Line) when list(FormatStr) ->
    WarningBool=hd([Value || {Option,Value} <-get(options), Option==warnings]),
    case WarningBool==true of
	true->
	    io:format("~s: ~w: Warning: ", [get(filename), Line]),
	    ok = io:format(FormatStr,Data),
	    io:format("~n");
	false->
	    ok
    end.

print_error(FormatStr, Data) when list(FormatStr) ->
    ok = io:format("~s: Error: " ++ FormatStr,[get(filename)|Data]),
    put(errors,yes),
    io:format("~n").
    
print_error(FormatStr, Data,Line) when list(FormatStr) ->
    ok = io:format("~s: ~w: Error: " ++ FormatStr,[get(filename), Line |Data]),
    put(errors,yes),
    io:format("~n").
