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

-module(ic_code).


-include_lib("ic/src/ic.hrl").
-include_lib("ic/src/icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([get_basetype/2, insert_typedef/3, codeDirective/2]).
-export([gen_includes/3, gen_includes/4, mk_list/1]).

%%
%% NOC special
%%
-export([type_expand_op/4, type_expand_handle_op/4]).
-export([ type_expand_op_exec/4, type_expand_all/6, type_expand/7]).

-export([type_expand_null/3, type_expand_void/3, type_expand_float/3, type_expand_double/3]).
-export([type_expand_short/3, type_expand_ushort/3, type_expand_long/3, type_expand_ulong/3]).
-export([type_expand_char/3, type_expand_boolean/3, type_expand_octet/3, type_expand_any/3]).
-export([type_expand_object/3, type_expand_string/3, type_expand_struct/7, type_expand_union/7]).
-export([type_expand_enum/4, type_expand_sequence/7, type_expand_array/7, type_expand_error/3]).

-export([type_expand_struct_rule/3, type_expand_union_rule/2, type_expand_enum_rule/4]).
-export([type_expand_enum_elements/3]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-------------------------------------------------------------------------------------
%%
%% Trackrecording of generated sequence type structs, thist is just used for C today.
%%
%%-------------------------------------------------------------------------------------

get_basetype(G, MyId) ->
    case ?lookup(ic_genobj:typedeftab(G), MyId) of
	[] ->
	     MyId;
	X ->
	    get_basetype(G, X)
    end.

insert_typedef(G, "erlang_term", _) ->
    ok;
insert_typedef(G, MyId, DefinedAsId) ->
    ?insert(ic_genobj:typedeftab(G), MyId, DefinedAsId).

codeDirective(G,X) ->
    case produceCode(X) of
        true ->
            case ic_options:get_opt(G, be) of
                c_genserv ->
                    c;
		c_client ->
		    c;
		c_server ->
		    c_server;
                _ ->
                    erlang
            end;
        false ->
            case ic_options:get_opt(G, be) of              
                c_genserv ->
                    c_no_stub;
		c_client ->
		    c_no_stub;
		c_server ->
		    c_server_no_stub;
                _ ->
                    erlang_no_stub
            end
    end.

%% Checks if X should produce code
produceCode(X) when record(X, module) ->
    case ic_forms:get_body(X) of
        [] ->
            true;
        List ->
            produceModuleCode(List)
    end;
produceCode(X) ->    
    false.
    
produceModuleCode([]) ->
    false;
produceModuleCode([X|Xs]) when record(X, const) ->
    true;
produceModuleCode([X|Xs]) ->
    produceModuleCode(Xs).
 



%% Returns the valid code directive
%% for special cases ( = module )
%% Used ONLY to decide file creation.
%codeDirective(G,X) ->
%    case produceCode(G,X) of
%	true ->
%	    case ic_options:get_opt(G, be) of
%		c_genserv ->
%		    c;
%		c_server ->
%		    c_server;
%		_ ->
%		    erlang
%	    end;
%	false ->
%	    case ic_options:get_opt(G, be) of		
%		c_genserv ->
%		    c_no_stub;
%		c_server ->
%		    c_server_no_stub;	
%		_ ->
%		    erlang_no_stub
%	    end
%    end.

%%% Checks if X should produce code
%produceCode(G,X) when record(X, module) ->
%    case ic_forms:get_body(X) of
%	[] ->
%	    true;
%	List ->
%	    produceModuleCode(G,List)
%    end;
%produceCode(G,X) ->    
%    false.
    
%produceModuleCode(_,[]) ->
%    false;
%produceModuleCode(G,[X|Xs]) when record(X, const) ->
%    true;
%produceModuleCode(G,[X|Xs]) when record(X, enum) ->
%    case ic_options:get_opt(G, be) of
%	c_genserv ->
%	    true;
%	c_server ->
%	    true;
%	_ ->
%	    false
%    end;
%produceModuleCode(G,[X|Xs]) ->
%    produceModuleCode(G,Xs).

%% Includes needed c file headers for included idl files
gen_includes(Fd,G,Type) ->
    case Type of
	c_client ->
	    IncludeList = 
		ic_pragma:get_included_c_headers(G),
	    gen_includes_loop(Fd,IncludeList,Type);
	c_server ->
	    IncludeList = 
		ic_pragma:get_included_c_headers(G),
	    gen_includes_loop(Fd,IncludeList,Type);
	_ ->
	    ok
    end,
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "#ifdef __cplusplus\n"),
    ic_codegen:emit(Fd, "extern \"C\" {\n"),
    ic_codegen:emit(Fd, "#endif\n\n").


%% Includes needed c file headers for local interfaces
gen_includes(Fd,G,X,Type) ->
    case Type of
	c_client ->
	    IncludeList = 
		ic_pragma:get_local_c_headers(G,X),
	    gen_includes_loop(Fd,IncludeList,Type);
	c_server ->
	    IncludeList = 
		ic_pragma:get_local_c_headers(G,X),
	    gen_includes_loop(Fd,IncludeList,Type);
	_ ->
	    ok
    end,
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "#ifdef __cplusplus\n"),
    ic_codegen:emit(Fd, "extern \"C\" {\n"),
    ic_codegen:emit(Fd, "#endif\n\n").


gen_includes_loop(_,[],_) ->
    ok;
gen_includes_loop(Fd,[I|Is],Type) ->
    L = string:tokens(I,"/"),
    File = lists:last(L),
    case File of
	"erlang" -> % Erlang is NOT generated that way !
	    gen_includes_loop(Fd,Is,Type);
	"oe_erlang" -> % Erlang is NOT generated that way !
	    gen_includes_loop(Fd,Is,Type);
	_ ->
	    case Type of
		c_client ->
		    ic_codegen:emit(Fd, "#include \"~s.h\"\n", [File]);
		c_server ->
		    ic_codegen:emit(Fd, "#include \"~s__s.h\"\n", [File]);
		_ ->
		    ok
	    end,
	    gen_includes_loop(Fd,Is,Type)
    end.




%%
%% Used in NOC only
%%


%%
%% Type expand on function head comments
%%
type_expand_op(G,N,X,Fd) ->
    case catch type_expand_op_exec(G,N,X,Fd) of
	{'EXIT',_Reason} ->
	    ic_codegen:nl(Fd),
	    ic_codegen:emit(Fd,"%% Error under type expansion, does not affect generated code.~n",[]),
	    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[]);
	_ ->
	    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[])
    end.


type_expand_op_exec(G,N,X,Fd) ->
    InArgs = ic:filter_params([in,inout], X#op.params),
    OutArgs = ic:filter_params([out,inout], X#op.params),
    ParamNr = length(InArgs)+1,
    Tabs = "",
    
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[]),

     case ic_forms:is_oneway(X) of
	false ->
	    ic_codegen:emit(Fd,"%% Operation: ~s/~p~n",[ic_forms:get_id2(X),ParamNr]);
	true ->
	    ic_codegen:emit(Fd,"%% Operation: ~s/~p  (oneway)~n",[ic_forms:get_id2(X),ParamNr])
    end,

    if  X#op.raises == [] -> [];
	true ->
	    ic_codegen:emit(Fd,"%%~n",[]),
	    RaisesList=["%% Raises: " ++ 
			mk_list(lists:map({icgen, to_colon}, X#op.raises))],
	    ic_codegen:emit(Fd,RaisesList,[]),
	    ic_codegen:nl(Fd)
    end,

    %% Print argument names
    ic_codegen:emit(Fd,"%%\n",[]),
    InArgNames = ["OE_Ref"]++[ic_util:mk_var(ic_forms:get_id(InArg#param.id)) || InArg <- InArgs ],
    OutArgNames = ["Ret"]++[ic_util:mk_var(ic_forms:get_id(OutArg#param.id)) || OutArg <- OutArgs ],
    case length(InArgNames) > 1 of
	true ->
	    ic_codegen:emit(Fd,"%% Input value(s)  : ~s~n",[mk_list(InArgNames)]);
	false ->
	    ic_codegen:emit(Fd,"%% Input value     : ~s~n",[mk_list(InArgNames)])
    end,
    case length(OutArgNames) > 1 of
	true ->
	    ic_codegen:emit(Fd,"%% Return value(s) : ~s~n",[mk_list(OutArgNames)]);
	false ->
	    ic_codegen:emit(Fd,"%% Return value    : ~s~n",[mk_list(OutArgNames)])
    end,
    ic_codegen:emit(Fd,"%%\n",[]),

    InArgsTypeList = 
	[{ic_util:mk_var(ic_forms:get_id(InArg#param.id)),ic_forms:get_tk(InArg)} || InArg <- InArgs ],
    case InArgsTypeList of
	[] -> %% no input parameters
	    ok;
	_ ->
	    ic_codegen:emit(Fd,"%%                                             --input-params-~n",[]),
	    type_expand_all(G,N,X,Fd,Tabs,InArgsTypeList)
    end,

    ReturnTypeList =[{"Ret",X#op.tk}],
    ic_codegen:emit(Fd,"%%                                             --return-value-~n",[]),
    type_expand_all(G,N,X,Fd,Tabs,ReturnTypeList),

    OutArgsTypeList = 
	[{ic_util:mk_var(ic_forms:get_id(OutArg#param.id)),ic_forms:get_tk(OutArg)} || OutArg <- OutArgs ], 
    case OutArgsTypeList of
	[] -> %% no input parameters
	    ok;
	_ ->
	    ic_codegen:emit(Fd,"%%                                             -output-values-~n",[]),
	    type_expand_all(G,N,X,Fd,Tabs,OutArgsTypeList)
    end.




type_expand_handle_op(G,N,X,Fd) ->
    case catch type_expand_handle_op_exec(G,N,X,Fd) of
	{'EXIT',_Reason} ->
	    ic_codegen:nl(Fd),
	    ic_codegen:emit(Fd,"%% Error under type expansion, does not affect generated code.~n",[]),
	    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[]);
	_ ->
	    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[])
    end.


type_expand_handle_op_exec(G,N,X,Fd) ->
    InArgs = ic:filter_params([in,inout], X#op.params),
    ParamNr = length(InArgs)+1,
    
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd,"%%------------------------------------------------------------~n",[]),

    case ic_forms:is_oneway(X) of
	false ->
	    ic_codegen:emit(Fd,"%% Handle operation: handle_call/3~n",[]);
	true ->
	    ic_codegen:emit(Fd,"%% Handle operation: handle_cast/3~n",[])
    end,
    ic_codegen:emit(Fd,"%%~n",[]),
    ic_codegen:emit(Fd,"%% Used for operation ~s/~p implementation~n",[ic_forms:get_id2(X),ParamNr]).



type_expand_all(G,N,X,Fd,Tabs,[]) -> 
    ok;
type_expand_all(G,N,X,Fd,Tabs,[{ArgName,Type}|Rest]) ->
    type_expand(G,N,X,Fd,Tabs,ArgName,Type),
    type_expand_all(G,N,X,Fd,Tabs,Rest);
type_expand_all(G,N,X,Fd,Tabs,[{default,ArgName,Type}|Rest]) ->
    type_expand(G,N,X,Fd,Tabs,"Def",Type),
    type_expand_all(G,N,X,Fd,Tabs,Rest);
type_expand_all(G,N,X,Fd,Tabs,[{LabelNr,ArgName,Type}|Rest]) when integer(LabelNr) ->
    type_expand(G,N,X,Fd,Tabs,"V" ++ integer_to_list(LabelNr),Type),
    type_expand_all(G,N,X,Fd,Tabs,Rest);
type_expand_all(G,N,X,Fd,Tabs,[{Label,ArgName,Type}|Rest]) ->
    type_expand(G,N,X,Fd,Tabs,Label,Type),
    type_expand_all(G,N,X,Fd,Tabs,Rest).



type_expand(_G,_N,_X,Fd,Tabs,Name,tk_null) ->
    type_expand_null(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_void) ->
    type_expand_void(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_float) ->
    type_expand_float(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_double) ->
    type_expand_double(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_short) ->
    type_expand_short(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_ushort) ->
    type_expand_ushort(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_long) ->
    type_expand_long(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_ulong) ->
    type_expand_ulong(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_char) ->
    type_expand_char(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_boolean) ->
    type_expand_boolean(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_octet) ->
    type_expand_octet(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,tk_any) ->
    type_expand_any(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,{tk_objref, IFRId, ObjTabs, ObjName}) ->
    type_expand_object(Fd,Tabs,Name);
type_expand(_G,_N,_X,Fd,Tabs,Name,{tk_string, Length}) ->
    type_expand_string(Fd,Tabs,Name);
type_expand(G,N,X,Fd,Tabs,Name,{tk_union, IFRId, UnionName, DTC, DNr, LblList}) ->
    type_expand_union(G,N,X,Fd,Tabs,Name,{tk_union, IFRId, UnionName, DTC, DNr, LblList});
type_expand(_G,_N,_X,Fd,Tabs,Name,{tk_enum, IFRId, EnumName, ElemNameList}) ->
    type_expand_enum(Fd,Tabs,Name,{tk_enum, IFRId, EnumName, ElemNameList});
type_expand(G,N,X,Fd,Tabs,Name,{tk_sequence, ElemTC, Length}) ->
    type_expand_sequence(G,N,X,Fd,Tabs,Name,{tk_sequence, ElemTC, Length});
type_expand(G,N,X,Fd,Tabs,Name,{tk_array, ElemTC, Length}) ->
    type_expand_array(G,N,X,Fd,Tabs,Name,{tk_array, ElemTC, Length});
type_expand(G,N,X,Fd,Tabs,Name,{tk_struct, IFRId, StructName, TcList}) ->
    type_expand_struct(G,N,X,Fd,Tabs,Name,{tk_struct, IFRId, StructName, TcList});
type_expand(_G,_N,_X,Fd,Tabs,Name,_) ->
    type_expand_error(Fd,Tabs,Name).


%% Basic OMG IDL types

type_expand_null(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = null()~n",[Tabs,Name]).

type_expand_void(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = void()~n",[Tabs,Name]).

type_expand_float(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = float()~n",[Tabs,Name]).

type_expand_double(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = double()~n",[Tabs,Name]).

type_expand_short(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = short()~n",[Tabs,Name]).

type_expand_ushort(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = unsigned_Short()~n",[Tabs,Name]).

type_expand_long(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = long()~n",[Tabs,Name]).

type_expand_ulong(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = unsigned_Long()~n",[Tabs,Name]).

type_expand_char(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = char()~n",[Tabs,Name]).

type_expand_boolean(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = boolean()~n",[Tabs,Name]).

type_expand_octet(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = octet()~n",[Tabs,Name]).

type_expand_any(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = any()~n",[Tabs,Name]).

type_expand_object(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = Object_Ref()~n",[Tabs,Name]).


%% Constructed OMG IDL types

type_expand_string(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = String()~n",[Tabs,Name]).

type_expand_struct(G,N,X,Fd,Tabs,Name,{tk_struct, IFRId, StructName, TcList}) ->
    ScopedStructName = getScopedName(G,N,StructName,IFRId),
    ic_codegen:emit(Fd,"%%~s ~s = ",[Tabs,Name]),
    type_expand_struct_rule(Fd,ScopedStructName,TcList),
    type_expand_all(G,N,X,Fd,Tabs,TcList).

type_expand_union(G,N,X,Fd,Tabs,Name,{tk_union, IFRId, UnionName, DTC, DNr, LblList}) ->
    ScopedUnionName = getScopedName(G,N,UnionName,IFRId),
    ic_codegen:emit(Fd,"%%~s ~s = #'~s'{label, value}\n",[Tabs,Name,ScopedUnionName]),
    type_expand(G,N,X,Fd,Tabs,"label",DTC),
    ic_codegen:emit(Fd,"%%~s value = ",[Tabs]),
    type_expand_union_rule(Fd,LblList),
    type_expand_all(G,N,X,Fd,Tabs,LblList).

type_expand_enum(Fd,Tabs,Name,{tk_enum, IFRId, EnumName, ElemNameList}) ->
    ic_codegen:emit(Fd,"%%~s ~s = ~s~n",[Tabs,Name,EnumName]),
    type_expand_enum_rule(Fd,Tabs,EnumName,ElemNameList).

type_expand_sequence(G,N,X,Fd,Tabs,Name,{tk_sequence, ElemTC, Length}) ->
    ic_codegen:emit(Fd,"%%~s ~s = [ ~sElem ]~n",[Tabs,Name,Name]),
    type_expand(G,N,X,Fd,Tabs,Name++"Elem",ElemTC).

type_expand_array(G,N,X,Fd,Tabs,Name,{tk_array, ElemTC, Length}) ->
    ic_codegen:emit(Fd,"%%~s ~s = { ~sElem[,..~sElem] }~n",[Tabs,Name,Name,Name]),
    type_expand(G,N,X,Fd,Tabs,Name++"Elem",ElemTC).

type_expand_error(Fd,Tabs,Name) ->
    ic_codegen:emit(Fd,"%%~s ~s = ????~n",[Tabs,Name]).


type_expand_struct_rule(Fd,_Name,[]) ->
    ic_codegen:emit(Fd," ???? ");
type_expand_struct_rule(Fd,Name,TcList) ->
    ic_codegen:emit(Fd,"#'~s'{",[Name]), 
    type_expand_struct_rule(Fd,TcList).

type_expand_struct_rule(Fd,[{Name,TC}]) ->
    ic_codegen:emit(Fd,"~s}~n",[Name]);
type_expand_struct_rule(Fd,[{Name,TC}|Rest]) ->
    ic_codegen:emit(Fd,"~s,",[Name]),
    type_expand_struct_rule(Fd,Rest).


type_expand_union_rule(Fd,[]) ->
    ic_codegen:emit(Fd," ????");
type_expand_union_rule(Fd,[{default,Name,TC}]) ->
    ic_codegen:emit(Fd,"Def~n",[]);
type_expand_union_rule(Fd,[{LNr,Name,TC}]) when integer(LNr)->
    ic_codegen:emit(Fd,"V~p~n",[LNr]);
type_expand_union_rule(Fd,[{Label,Name,TC}]) ->
    ic_codegen:emit(Fd,"~s~n",[Label]);
type_expand_union_rule(Fd,[{default,Name,TC}|Rest]) ->
    ic_codegen:emit(Fd,"Default | "),
    type_expand_union_rule(Fd,Rest);
type_expand_union_rule(Fd,[{LNr,Name,TC}|Rest]) when integer(LNr) ->
    ic_codegen:emit(Fd,"V~p | ",[LNr]),
    type_expand_union_rule(Fd,Rest);
type_expand_union_rule(Fd,[{Label,Name,TC}|Rest]) ->
    ic_codegen:emit(Fd,"~s | ",[Label]),
    type_expand_union_rule(Fd,Rest).


type_expand_enum_rule(Fd,Tabs,Name,[]) ->
    ic_codegen:emit(Fd,"%%~s ~s = ????",[Tabs,Name]);
type_expand_enum_rule(Fd,Tabs,Name,ElList) ->
    ic_codegen:emit(Fd,"%%~s ~s = ",[Tabs,Name]),
    type_expand_enum_rule(Fd,ElList).

type_expand_enum_rule(Fd,[ElName]) ->
    ic_codegen:emit(Fd,"'~s' ~n",[ElName]);
type_expand_enum_rule(Fd,[First|Rest]) ->
    ic_codegen:emit(Fd,"'~s' | ",[First]),
    type_expand_enum_rule(Fd,Rest).

type_expand_enum_elements(Fd,Tabs,[]) ->
    ok;
type_expand_enum_elements(Fd,Tabs,[Elem|Elems]) ->
    ic_codegen:emit(Fd,"%%~s ~s = Atom()~n",[Tabs,Elem]),
    type_expand_enum_elements(Fd,Tabs,Elems).



%% Returns the right scoped name to be used
%% along with the expansion comments 
getScopedName(G,N,Name,IfrId) ->
    
    PTab = ic_genobj:pragmatab(G),
    
    case ets:match(PTab,{alias,'$0',IfrId}) of
	[] -> %% No Alias - should never happen
	    ic_util:to_undersc(ic_pragma:mk_scope(IfrId));

	[[[S|N]]] -> %% An alias
	    ic_util:to_undersc([Name|N]);

	[[[S|FoundScope]]] -> %% Maybe inherited
	    case ic_pragma:is_inherited_by(FoundScope,N,PTab) of
 
		false -> %% Not inherited
		    ic_util:to_undersc([S|FoundScope]);

		true -> %% inherited
		    ic_util:to_undersc([Name|N])
	    end
    end.


%% mk_list produces a nice comma separated 
%% string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------



