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

-module(ic_java_type).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([getType/3, getHolderType/3, getErlangObjectType/3,
	 getParamType/4, inlinedTypes/2,
	 marshalFun/4, unMarshalFun/4, getFullType/4, 
	 getFullType/3, getMarshalType/4, getUnmarshalType/4,
	 getdim/1]).
-export([isBasicType/3, isBasicType/1]).
-export([isIntegerType/3, isIntegerType/1]).
-export([isTermType/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: getType/3
%%-----------------------------------------------------------------
getType(G, N, T) when record(T, scoped_id) ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "Pid";
	"erlang.port" ->
	    ?ICPACKAGE ++ "Port";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "Ref";
	"erlang.term" ->
	    ?ICPACKAGE ++ "Term";
	{enum, Type} ->
	    getType(G, N, Type);
	Type -> 
	    case TK of
		{tk_array,_,_} ->
		    tk2type(G,N,T,TK);
		{tk_sequence,_,_} ->
		    tk2type(G,N,T,TK);
		tk_any ->
		    ?ICPACKAGE ++ "Any";
		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    tk2type(G,N,T,TK);
			false ->
			    Type %% Other types
		    end
	    end
    end;

getType(G, N, S) when list(S) ->
    S;

getType(G, N, T) when record(T, string) ->
    "java.lang.String";

getType(G, N, T) when record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

getType(G, N, T) when record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

getType(G, N, T) when record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ "[]";

getType(G, N, T) when record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

%% NOTE i am using the new isJavaElementaryType
%% to avoid members declared as keywords (except
%% all java elementary types) to be used as a 
%% class
getType(G, N, T) when record(T, member) ->
    Type = tk2type(G,N,T,ic_forms:get_type_code(G, N, T)),
    case isJavaElementaryType(list_to_atom(Type)) of
	true ->
	    Type;
	false ->
	    Prefix = list_to_atom(lists:flatten(string:tokens(Type,"[]"))),
	    case isJavaElementaryType(Prefix) of %% Checks if Type is an array
		                                 %% of elementary java types
		true ->
		    Type;
		false -> 
		    ic_forms:get_java_id(getType(G,N,ic_forms:get_type(T))) ++
			if record(hd(T#member.id),array) ->
				arrayEmptyDim(hd(T#member.id));
			   true ->
				""
			end
	    end
    end;

getType(G, N, {boolean, _}) ->
    "boolean";

getType(G, N, {octet, _}) ->
    "byte";

getType(G, N, {void, _}) ->
    "void";

getType(G, N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    "short";
	{long,_} ->
	    "int"
    end;

getType(G, N, {char, _}) ->
    "char";

getType(G, N, {short, _}) ->
    "short";

getType(G, N, {long, _}) ->
    "int";

getType(G, N, {float, _}) ->
    "float";

getType(G, N, {double, _}) ->
    "double";
    
getType(G, N, {any, _}) ->
    ?ICPACKAGE ++ "Any".




%%-----------------------------------------------------------------
%% Func: getObjectType/3
%%-----------------------------------------------------------------
getErlangObjectType(G, N, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "Pid";
	"erlang.port" ->
	    ?ICPACKAGE ++ "Port";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "Ref";
	"erlang.term" ->
	    ?ICPACKAGE ++ "Term";
	_ ->
	    tk2ErlangObjectType(TK)
    end;
%getErlangObjectType(G, N, S) when list(S) ->
%    S;
getErlangObjectType(G, N, T) when record(T, string) ->
    ?ERLANGPACKAGE ++ "OtpErlangString";

getErlangObjectType(G, N, T) when record(T, struct) ->
    ?ERLANGPACKAGE ++ "OtpErlangTuple";

getErlangObjectType(G, N, T) when record(T, union) ->
    ?ERLANGPACKAGE ++ "OtpErlangTuple";

getErlangObjectType(G, N, T) when record(T, sequence) ->
    ?ERLANGPACKAGE ++ "OtpErlangList";

getErlangObjectType(G, N, T) when record(T, array) ->
    ?ERLANGPACKAGE ++ "OtpErlangTuple";

getErlangObjectType(G, N, T) when record(T, enum) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getErlangObjectType(G, N, {boolean, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getErlangObjectType(G, N, {octet, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getErlangObjectType(G, N, {void, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getErlangObjectType(G, N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong";
	{long,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong"
    end;

getErlangObjectType(G, N, {char, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getErlangObjectType(G, N, {short, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getErlangObjectType(G, N, {long, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getErlangObjectType(G, N, {float, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";

getErlangObjectType(G, N, {double, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble".





%%-----------------------------------------------------------------
%% Func: getHolderType/3
%%-----------------------------------------------------------------
getHolderType(G, N, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHolder";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHolder";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHolder";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHolder";
	{enum, Type} ->
	    getHolderType(G, N, Type);
	
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Holder";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Holder";

		{'tk_array', _ , _} ->
		    Type ++ "Holder";

		{'tk_sequence', _ , _} ->
		    Type ++ "Holder";

		{'tk_string', _} ->
		    ?ICPACKAGE ++ "StringHolder";

		{'tk_enum', _, _, _} ->
		    Type ++ "Holder";

		'tk_boolean' ->
		    ?ICPACKAGE ++ "BooleanHolder";
		
		'tk_octet' ->
		    ?ICPACKAGE ++ "ByteHolder";
		
		'tk_ushort' ->
		    ?ICPACKAGE ++ "ShortHolder";
		
		'tk_ulong' ->
		    ?ICPACKAGE ++ "IntHolder";
		
		'tk_short' ->
		    ?ICPACKAGE ++ "ShortHolder";
		
		'tk_long' ->
		    ?ICPACKAGE ++ "IntHolder";
		
		'tk_float' ->
		    ?ICPACKAGE ++ "FloatHolder";
		
		'tk_double' ->
		    ?ICPACKAGE ++ "DoubleHolder";
		
		'tk_char' ->
		    ?ICPACKAGE ++ "CharHolder";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHolder";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getHolderType(G, N, {list_to_atom(tk2type(G,N,T,TK)), -1}); 
			false ->
			    %%io:format("TK = ~p, Type = ~p\n",[TK,Type]),
			    ic_util:to_dot(G,FullScopedName) ++ "Holder"
		    end
	    end
    end;

getHolderType(G, N, S) when list(S) ->
    ic_util:to_dot(G,[S|N]) ++ "Holder";

getHolderType(G, N, T) when record(T, string) ->
    ?ICPACKAGE ++"StringHolder";

getHolderType(G, N, T) when record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when record(T, array) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ "Holder[]";

getHolderType(G, N, T) when record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, {boolean, _}) ->
    ?ICPACKAGE ++"BooleanHolder";

getHolderType(G, N, {octet, _}) ->
    ?ICPACKAGE ++"ByteHolder";

getHolderType(G, N, {void, _}) ->
    "void";

getHolderType(G, N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ?ICPACKAGE ++"ShortHolder";
	{long,_} ->
	    ?ICPACKAGE ++"IntHolder"
    end;

getHolderType(G, N, {char, _}) ->
    ?ICPACKAGE ++"CharHolder";

getHolderType(G, N, {short, _}) ->
    ?ICPACKAGE ++"ShortHolder";

getHolderType(G, N, {long, _}) ->
    ?ICPACKAGE ++"IntHolder";

getHolderType(G, N, {float, _}) ->
    ?ICPACKAGE ++"FloatHolder";

getHolderType(G, N, {double, _}) ->
    ?ICPACKAGE ++"DoubleHolder";

getHolderType(G, N, {any,_}) ->
    ?ICPACKAGE ++ "AnyHolder".


%%-----------------------------------------------------------------
%% Func: getParamType/4
%%-----------------------------------------------------------------
getParamType(G, N, S, in) ->
    getType(G, N, S);
getParamType(G, N, S, ret) ->
    getType(G, N, S);
getParamType(G, N, S, out) ->
    getHolderType(G, N, S);
getParamType(G, N, S, inout) ->
    getHolderType(G, N, S).


%%-----------------------------------------------------------------
%% Func: getUnmarshalType/4
%%-----------------------------------------------------------------
getUnmarshalType(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHelper";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHelper";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHelper";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHelper";
	{enum, Type} ->
	    getUnmarshalType(G, N, X, Type);
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Helper";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Helper";

		{'tk_sequence', _ , _} ->
		    Type ++ "Helper";

		{'tk_array', _ , _} ->
		    Type ++ "Helper";

		{'tk_enum', _, _, _} ->
		    Type ++ "Helper";

		{'tk_string',_} ->
		    ?ERLANGPACKAGE ++ "OtpErlangString";

		'tk_char' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_octet' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_ushort' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_ulong' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_short' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_long' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_float' ->
		    ?ERLANGPACKAGE ++ "OtpErlangDouble";
		
		'tk_double' ->
		    ?ERLANGPACKAGE ++ "OtpErlangDouble";
		
		'tk_boolean' ->
		    ?ERLANGPACKAGE ++ "OtpErlangAtom";

		'tk_void' ->
		    ?ERLANGPACKAGE ++ "OtpErlangAtom";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHelper";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getUnmarshalType(G, N, X, {list_to_atom(tk2type(G,N,T,TK)), -1});
			false ->
			    ic_util:to_dot(G,FullScopedName) ++ "Helper"
		    end
	    end
    end;

getUnmarshalType(G, N, X, S) when list(S) ->
    S ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, string) ->
    ?ERLANGPACKAGE ++ "OtpErlangString";

getUnmarshalType(G, N, X, T) when record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, sequence), 
				  record(X, member) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, sequence), 
				  record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, sequence) ->
    getUnmarshalType(G, N, X, ic_forms:get_type(T)) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, array), 
				  record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getUnmarshalType(G, N, X, {boolean, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getUnmarshalType(G, N, X, {octet, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(G, N, X, {void, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getUnmarshalType(G, N, X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong";
	{long,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong"
    end;

getUnmarshalType(G, N, X, {char, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(G, N, X, {short, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(G, N, X, {long, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(G, N, X, {float, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";

getUnmarshalType(G, N, X, {double, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";

getUnmarshalType(G, N, X, {any, _}) ->
    ?ICPACKAGE ++ "AnyHelper".

%%-----------------------------------------------------------------
%% Func: getMarshalType/4
%%-----------------------------------------------------------------
getMarshalType(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHelper";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHelper";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHelper";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHelper";
	{enum, Type} ->
	    getMarshalType(G, N, X, Type);
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Helper";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Helper";

		{'tk_array', _ , _} ->
		    Type ++ "Helper";

		{'tk_sequence', _ , _} ->
		    Type ++ "Helper";
		
		{'tk_enum', _, _, _} ->
		    Type ++ "Helper";

		{'tk_string',_} ->
		    "string";

		'tk_char' ->
		    "char";

		'tk_octet' ->
 		    "byte";
		
		'tk_ushort' ->
		    "short";
		
		'tk_ulong' ->
		    "int";
		
		'tk_short' ->
		    "short";
		
		'tk_long' ->
		    "int";
		
		'tk_float' ->
		    "float";
		
		'tk_double' ->
		    "double";
		
		'tk_boolean' ->
		    "boolean";

		'tk_void' ->
		    "atom";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHelper";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getMarshalType(G, N, X, {list_to_atom(tk2type(G,N,T,TK)), -1}); 
			false ->
			    ic_util:to_dot(G,FullScopedName) ++ "Helper"
		    end
	    end
    end;

getMarshalType(G, N, X, S) when list(S) ->
    S ++ "Helper";

getMarshalType(G, N, X, T) when record(T, string) ->
    "string";

getMarshalType(G, N, X, T) when record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(G, N, X, T) when record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(G, N, X, T) when record(T, array),
				record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ 
	"Helper";

getMarshalType(G, N, X, T) when record(T, sequence),
				record(X, member) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ 
	"Helper";

getMarshalType(G, N, X, T) when record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ 
	"Helper";

getMarshalType(G, N, X, T) when record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(G, N, X, {boolean, _}) ->
    "boolean";

getMarshalType(G, N, X, {octet, _}) ->
    "byte";

getMarshalType(G, N, X, {void, _}) ->
    ""; % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

getMarshalType(G, N, X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    "short";
	{long,_} ->
	    "int"
    end;

getMarshalType(G, N, X, {short, _}) ->
    "short";
getMarshalType(G, N, X, {long, _}) ->
    "int";
getMarshalType(G, N, X, {float, _}) ->
    "float";
getMarshalType(G, N, X, {double, _}) ->
    "double";
getMarshalType(G, N, X, {char, _}) ->
    "char";

getMarshalType(G, N, X, {any, _}) ->
    ?ICPACKAGE ++ "AnyHelper".




%%-----------------------------------------------------------------
%% Func: unMarshalFun/4
%%-----------------------------------------------------------------
unMarshalFun(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ".read_pid()";
	"erlang.port" ->
	    ".read_port()";
	"erlang.ref" ->
	    ".read_ref()";
	"erlang.term" ->
	    ".read_term()";
	{enum, Type} ->
	    unMarshalFun(G, N, X, Type);
	Type ->
	    case isBasicType(G,N,TK) of
		true ->
		    case TK of
			{'tk_string',_} ->
			    ".read_string()";
			
			'tk_boolean' ->
			    ".read_boolean()";
			
			'tk_octet' ->
			    ".read_byte()";
			
			'tk_ushort' ->
			    ".read_short()";

			'tk_ulong' ->
			    ".read_int()";
			 
			'tk_short' ->
			    ".read_short()";

			'tk_long' ->
			    ".read_int()";

			'tk_float' ->
			    ".read_float()";

			'tk_double' ->
			    ".read_double()";

			'tk_char' ->
			    ".read_char()";

			_ ->
			    %% Faked the type !
			    unMarshalFun(G, N, X, {list_to_atom(tk2type(G,N,X,TK)), -1})
		    end;
		false ->
		    ".unmarshal()"
	    end
    end;

unMarshalFun(G, N, X, S) when list(S) ->
    ".unmarshal()";

unMarshalFun(G, N, X, T) when record(T, string) ->
    ".read_string()";

unMarshalFun(G, N, X, T) when record(T, struct) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangTuple)";

unMarshalFun(G, N, X, T) when record(T, union) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangTuple)";

unMarshalFun(G, N, X, T) when record(T, sequence) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlanglist)";

unMarshalFun(G, N, X, T) when record(T, enum) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangAtom)";

unMarshalFun(G, N, X, {boolean, _}) ->
    ".read_boolean()";

unMarshalFun(G, N, X, {octet, _}) ->
    ".read_byte()";

unMarshalFun(G, N, X, {void, _}) ->
    "";

unMarshalFun(G, N, X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ".read_short()";
	{long,_} ->
	    ".read_int()"
    end;

unMarshalFun(G, N, X, {short, _}) ->
    ".read_short()";
unMarshalFun(G, N, X, {long, _}) ->
    ".read_int()";
unMarshalFun(G, N, X, {float, _}) ->
    ".read_float()";
unMarshalFun(G, N, X, {double, _}) ->
    ".read_double()";
unMarshalFun(G, N, X, {char, _}) ->
    ".read_char()".





%%-----------------------------------------------------------------
%% Func: getFullType/4 - /3
%%
%% Note : Similar to the getType/3 with the major difference 
%%        thet on arrays and sequences it will also declare
%%        their sizes. Used for "new" declarations
%%
%%-----------------------------------------------------------------


getFullType(G, N, X, T) when record(X, typedef), record(T, array) -> 
    FullDim = 
	tk2FullType(G,N,X,ic_forms:get_tk(X)) ++
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, X, T) when record(X, member), record(T, array) -> 
    FullDim = 
	getFullType(G, N, ic_forms:get_type(X)) ++ 
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, X, T) when record(X, case_dcl), record(T, array) -> 
    FullDim = 
	getFullType(G, N, ic_forms:get_type(X)) ++ 
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, X, T)  ->
    getFullType(G, N, T).



getFullType(G, N, T) when record(T, scoped_id) ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    case TK of
	{tk_array,_,_} ->
	    tk2FullType(G,N,T,TK);
	{tk_sequence,_,_} ->
	    tk2FullType(G,N,T,TK);
	_ ->
	    case isBasicType(G,N,TK) of
		true ->
		    tk2FullType(G,N,T,TK);
		false ->
		    %% Other types
		    ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)) 
	    end
    end;

getFullType(G, N, T) when record(T, sequence) ->
    fixSeqDims(getType(G,N,T),"_length");

getFullType(G, N, T)  ->
    getType(G, N, T).



%% In order to make a legal declaration 
%% of an assignable array, the dimentions 
%% of empty array sequences are swifted to 
%% the end of the type
fixArrayDims(Cs) ->
    fixArrayDims(Cs,[],[]).

fixArrayDims([],Fulls,Emptys) ->
    lists:reverse(Fulls) ++ Emptys;
fixArrayDims([91,93|Rest],Fulls,Emptys) ->
    fixArrayDims(Rest,Fulls,[91,93|Emptys]);
fixArrayDims([C|Rest],Fulls,Emptys) ->
    fixArrayDims(Rest,[C|Fulls],Emptys).


%% In order to make a legal declaration 
%% of an assignable array, the dimentions 
%% of empty array of sequences are swifted 
%% to the end of the type
fixSeqDims(Cs,Length) ->
    fixSeqDims(Cs,Length,[]).

fixSeqDims([],Length,Found) ->
    lists:reverse(Found);
fixSeqDims([91,93|Rest],Length,Found) when list(Length) ->
    lists:reverse([93|lists:reverse(Length)] ++ 
		  [91|Found]) ++ Rest;
fixSeqDims([91,93|Rest],Length,Found) when integer(Length) ->
    lists:reverse([93|lists:reverse(integer_to_list(Length))] ++ 
		  [91|Found]) ++ Rest;
fixSeqDims([C|Rest],Length,Found) ->
    fixSeqDims(Rest,Length,[C|Found]).


  
%%-----------------------------------------------------------------
%% Func: inlinedTypes/2
%%-----------------------------------------------------------------
inlinedTypes(PkgName, Type) when record(Type, struct) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(PkgName, Type) when record(Type, union) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(PkgName, Type) when record(Type, enum) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(_, _) ->
    "".

%%-----------------------------------------------------------------
%% Func: marshalFun/4
%%-----------------------------------------------------------------
marshalFun(G, N, X, Type) ->
    case isBasicType(G, N, Type) of
	true ->
	    ".write_" ++ getMarshalType(G, N, X, Type);
	_ ->
	    getMarshalType(G, N, X, Type) ++ ".marshal"
    end.


%%-----------------------------------------------------------------
%% Func: isBasicType/3
%%-----------------------------------------------------------------
isBasicType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isBasicType(ictype:fetchType(TK));

isBasicType(G, N, X) when record(X, member) ->
    if record(hd(element(3,X)), array) ->
	    false;
       true ->
	    isBasicType(G, N, element(2,X))
    end;

isBasicType(G, N, {unsigned, {long, _}} ) -> 
    true;

isBasicType(G, N, {unsigned, {short, _}} ) -> 
    true;

isBasicType(G, N, {Type, _} ) -> 
    isBasicType(Type);

isBasicType(_G, _N, Type) ->
    isBasicType(Type).


%%-----------------------------------------------------------------
%% Func: isBasicType/1
%%-----------------------------------------------------------------

isBasicType( Type ) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_float,float,
		  tk_double,double,
		  tk_boolean,boolean,
		  tk_char,char,
		  tk_octet,octet,
		  tk_string,string]).

%% returns true if the Type is a java elementary type
isJavaElementaryType( Type ) ->
    lists:member(Type,
		 [byte, char, boolean, int, short, long, float, double]).

%%-----------------------------------------------------------------
%% Func: isIntegerType/3
%%-----------------------------------------------------------------
isIntegerType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isIntegerType(ictype:fetchType(TK));
isIntegerType(G, N, {unsigned, {long, _}} ) -> 
    true;
isIntegerType(G, N, {unsigned, {short, _}} ) -> 
    true;
isIntegerType(G, N, {Type, _} ) -> 
    isIntegerType(Type);
isIntegerType(_G, _N, Type) ->
    isIntegerType(Type).

%%-----------------------------------------------------------------
%% Func: isIntegerType/1
%%-----------------------------------------------------------------

isIntegerType( Type ) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_char,char,
		  tk_octet,octet]).



%%-----------------------------------------------------------------
%% Func: isTerm/3
%%-----------------------------------------------------------------
isTermType(G, N, T) ->
    case getType(G,N,T) of
	"com.ericsson.otp.ic.Term" ->
	    true;
	_ ->
	    false
    end.




%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------


%% Changes the typecode to the 
%% corresponding "basic" type
tk2type(G,N,X,{'tk_struct', IFRId, "port", ElementList}) ->
    ?ICPACKAGE ++ "Port";
tk2type(G,N,X,{'tk_struct', IFRId, "pid", ElementList}) ->
    ?ICPACKAGE ++ "Pid";
tk2type(G,N,X,{'tk_struct', IFRId, "ref", ElementList}) ->
    ?ICPACKAGE ++ "Ref";
tk2type(G,N,X,{'tk_struct', IFRId, "term", ElementList}) ->
    ?ICPACKAGE ++ "Term";
tk2type(G,N,X,{'tk_string', _}) -> 
    "java.lang.String";
tk2type(G,N,X,{'tk_array', ElemTC, Dim}) -> 
    tkarr2decl(G,N,X,{'tk_array', ElemTC, Dim});
tk2type(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength}) -> 
    tkseq2decl(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength});
tk2type(G,N,X,{'tk_struct', IFRId, Name, ElementList}) ->
    ScopedId=
	lists:reverse(string:tokens(lists:nth(2,string:tokens(IFRId,":")),"/")),

    case ic_forms:clean_up_scope([Name|N]) of
	ScopedId ->
	    %% Right path, use N instead
	    ic_util:to_dot(G,[Name|N]);
	_ ->
	    %% Uggly work arround 
	    ic_util:to_dot(G,ScopedId)
    end;
tk2type(G,N,X,{'tk_union', IFRId, Name, _, _, ElementList}) ->
    ScopedId=
	lists:reverse(string:tokens(lists:nth(2,string:tokens(IFRId,":")),"/")),

    case ic_forms:clean_up_scope([Name|N]) of
	ScopedId ->
	    %% Right path, use N instead
	    ic_util:to_dot(G,[Name|N]);
	_ ->
	    %% Uggly work arround 
	    ic_util:to_dot(G,ScopedId)
    end;
tk2type(G,N,X,{'tk_enum', Id, Name, ElementList}) -> 
    Name;
tk2type(G,N,X,tk_void) ->
    "void";
tk2type(G,N,X,tk_long) ->
    "int";
tk2type(G,N,X,tk_short) ->
    "short";
tk2type(G,N,X,tk_ulong) ->
    "int";
tk2type(G,N,X,tk_ushort) ->
    "short";
tk2type(G,N,X,tk_float) ->
    "float";
tk2type(G,N,X,tk_double) ->
    "double";
tk2type(G,N,X,tk_boolean) ->
    "boolean";
tk2type(G,N,X,tk_char) ->
    "char";
tk2type(G,N,X,tk_octet) ->
    "byte";
tk2type(G,N,X,tk_string) ->
    "java.lang.String";
tk2type(G,N,X,tk_any) ->
    ?ICPACKAGE ++ "Any".

%% Changes the sequence typecode to the 
%% corresponding "basic" structure
tkseq2decl(G,N,X,TKSeq) ->
    tkseq2decl2(G,N,X,TKSeq,[],[]).

tkseq2decl2(G,N,X,{tk_sequence,E,D},[],Ds) ->
    tkseq2decl2(G,N,X,E,[],[D|Ds]);
tkseq2decl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2type(G,N,X,TkEl),
    ElName ++ getdim(Ds).

%% Changes the array typecode to the 
%% corresponding "basic" structure
tkarr2decl(G,N,X,TKArr) ->
    tkarr2decl2(G,N,X,TKArr,[],[]).

tkarr2decl2(G,N,X,{tk_array,E,D},[],Ds) ->
    tkarr2decl2(G,N,X,E,[],[D|Ds]);
tkarr2decl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2type(G,N,X,TkEl),
    ElName ++ getdim(Ds).

getdim([]) ->
    "";
getdim([D|Ds]) ->
    getdim(Ds) ++ "[]".



tk2ErlangObjectType({'tk_struct', IFRId, "port", ElementList}) ->
    ?ICPACKAGE ++ "Port";
tk2ErlangObjectType({'tk_struct', IFRId, "pid", ElementList}) ->
    ?ICPACKAGE ++ "Pid";
tk2ErlangObjectType({'tk_struct', IFRId, "ref", ElementList}) ->
    ?ICPACKAGE ++ "Ref";
tk2ErlangObjectType({'tk_struct', IFRId, "term", ElementList}) ->
    ?ICPACKAGE ++ "Term";
tk2ErlangObjectType({'tk_struct', IFRId, Name, ElementList}) ->
    ?ERLANGPACKAGE ++ "OtpErlangTuple";
tk2ErlangObjectType({'tk_string', _}) -> 
    ?ERLANGPACKAGE ++ "OtpErlangString";
tk2ErlangObjectType({'tk_array', ElemTC, Dim}) -> 
    ?ERLANGPACKAGE ++ "OtpErlangTuple";
tk2ErlangObjectType({'tk_sequence', ElemTC, MaxLsextractength}) -> 
    ?ERLANGPACKAGE ++ "OtpErlangList";
tk2ErlangObjectType({'tk_union', IFRId, Name, _, _, ElementList}) ->
    ?ERLANGPACKAGE ++ "OtpErlangTuple";
tk2ErlangObjectType({'tk_enum', Id, Name, ElementList}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";
tk2ErlangObjectType(tk_long) ->
    ?ERLANGPACKAGE ++ "OtpErlangInt";
tk2ErlangObjectType(tk_short) ->
    ?ERLANGPACKAGE ++ "OtpErlangShort";
tk2ErlangObjectType(tk_ulong) ->
    ?ERLANGPACKAGE ++ "OtpErlangUInt";
tk2ErlangObjectType(tk_ushort) ->
    ?ERLANGPACKAGE ++ "OtpErlangUShort";
tk2ErlangObjectType(tk_float) ->
    ?ERLANGPACKAGE ++ "OtpErlangFloat";
tk2ErlangObjectType(tk_double) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";
tk2ErlangObjectType(tk_boolean) ->
    ?ERLANGPACKAGE ++ "OtpErlangBoolean";
tk2ErlangObjectType(tk_char) ->
    ?ERLANGPACKAGE ++ "OtpErlangChar";
tk2ErlangObjectType(tk_void) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";
tk2ErlangObjectType(tk_octet) ->
    ?ERLANGPACKAGE ++ "OtpErlangChar".





%% Changes the typecode to the corresponding "basic" type
%% used for variable declarations where arrays and sequences 
%% are declared with there full dimensions 
tk2FullType(G,N,X,{'tk_array', ElemTC, Dim}) -> 
    tkarr2FullDecl(G,N,X,{'tk_array', ElemTC, Dim});
tk2FullType(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength}) -> 
    tkseq2FullDecl(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength});
tk2FullType(G,N,X,TK) ->
    tk2type(G,N,X,TK).


%% Changes the sequence typecode to the 
%% corresponding "basic" structure here
%% arrays and sequences are declared with 
%% their full dimensions 
tkseq2FullDecl(G,N,X,TKSeq) ->
    tkseq2FullDecl2(G,N,X,TKSeq,[],[]).

tkseq2FullDecl2(G,N,X,{tk_sequence,E,D},[],Ds) ->
    tkseq2FullDecl2(G,N,X,E,[],[D|Ds]);
tkseq2FullDecl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2FullType(G,N,X,TkEl),
    ElName ++ getdim(Ds).

%% Changes the array typecode to the 
%% corresponding "basic" structure
tkarr2FullDecl(G,N,X,TKArr) ->
    tkarr2FullDecl2(G,N,X,TKArr,[],[]).

tkarr2FullDecl2(G,N,X,{tk_array,E,D},[],Ds) ->
    tkarr2FullDecl2(G,N,X,E,[],[D|Ds]);
tkarr2FullDecl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2FullType(G,N,X,TkEl),
    ElName ++ getFullDim(G,N,Ds).

getFullDim(_G,_N,[]) ->
    "";
getFullDim(G,N,[D|Ds]) when record(D,scoped_id) ->
    {FSN, _, _, _} = ic_symtab:get_full_scoped_name(G, N, D),
    "[" ++ ic_util:to_dot(G,FSN) ++ "]" ++ getFullDim(G,N,Ds);
getFullDim(G,N,[D|Ds]) when integer(D) ->
    "[" ++ integer_to_list(D) ++ "]" ++ getFullDim(G,N,Ds);
getFullDim(G,N,[D|Ds]) when tuple(D) ->
    "[" ++ ic_util:eval_java(G,N,D) ++ "]" ++ getFullDim(G,N,Ds).



%% Constructs an array empty dimension string
%% used for array variable declaration
arrayEmptyDim(X) ->
    arrayEmptyDim2(X#array.size).

arrayEmptyDim2([D]) ->
    "[]";
arrayEmptyDim2([D |Ds]) ->
    "[]" ++ arrayEmptyDim2(Ds).



