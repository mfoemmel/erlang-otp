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

-module(ic_struct_java).

-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([gen/3]).
-compile(export_all).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
gen(G, N, X) when record(X, struct) ->
    StructName = ic_forms:get_java_id(X),
    WireStructName = ic_forms:get_id2(X),
    emit_struct_class(G, N, X, StructName),
    emit_holder_class(G, N, X, StructName),
    emit_helper_class(G, N, X, StructName, WireStructName),
    N2 = [StructName ++ "Package" |N],  
    ic_jbe:gen(G, N2, ic_forms:get_body(X));
gen(G, N, X) -> 
    %%?PRINTDEBUG2("****** IGNORING ******: ~p", [X]),
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func:  emit_struct_class/4
%%-----------------------------------------------------------------
emit_struct_class(G, N, X, StructName) ->
    {Fd, _}= ic_file:open_java_file(G, N, StructName), 
    
    MList = struct_member_list(G, N, X),

    ic_codegen:emit(Fd, "final public class ~s {\n",[StructName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    emit_struct_members_declarations(G, [StructName ++ "Package" |N],
				     X, Fd, MList),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   public ~s() {}\n", [StructName]),

    ArgList = gen_parameter_list(G, [ StructName ++ "Package" |N], X, MList), 
    ic_codegen:emit(Fd, "   public ~s(~s) {\n", [StructName, ArgList]),
    emit_struct_members_initialisation(G, N, X, Fd, MList),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, X, StructName) ->
    SName = string:concat(StructName, "Holder"),
    {Fd, _}= ic_file:open_java_file(G, N, SName), 
    
    ic_codegen:emit(Fd, "final public class ~sHolder {\n",[StructName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    ic_codegen:emit(Fd, "   public ~s value;\n\n", [StructName]),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   public ~sHolder() {}\n", [StructName]),
    ic_codegen:emit(Fd, "   public ~sHolder(~s initial) {\n", [StructName, StructName]),
    ic_codegen:emit(Fd, "      value = initial;\n", []),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   // methods\n", []),

    MList = struct_member_list(G, N, X),

    ic_codegen:emit(Fd, "   public void _marshal(~sOtpOutputStream out) throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      ~sHelper.marshal(out, value);\n", [StructName]),
    ic_codegen:emit(Fd, "   }\n\n", []),


    ic_codegen:emit(Fd, "   public void _unmarshal(~sOtpInputStream in) throws java.lang.Exception {\n", 
		    [?ERLANGPACKAGE]), 
    ic_codegen:emit(Fd, "      value = ~sHelper.unmarshal(in);\n",
		    [StructName]),
    ic_codegen:emit(Fd, "   }\n", []),
    
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_helper_class/5
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, StructName, WireStructName) ->
    SName = string:concat(StructName, "Helper"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),
 
    ic_codegen:emit(Fd, "public class ~sHelper {\n",[StructName]),
    
    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   private ~sHelper() {}\n", [StructName]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n", []),

    MList = struct_member_list(G, N, X),

    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s _value)\n",
		    [?ERLANGPACKAGE, StructName]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),

    emit_struct_marshal_function(G, N, X, Fd, StructName, WireStructName, MList),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   public static ~s unmarshal(~sOtpInputStream _in)\n",
		    [StructName, ?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),

    emit_struct_unmarshal_function(G, N, X, Fd, StructName, WireStructName, MList),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   public static String id() {\n", []), 
    ic_codegen:emit(Fd, "      return ~p;\n",[ictk:get_IR_ID(G, N, X)]),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   public static String name() {\n", []), 
    ic_codegen:emit(Fd, "      return ~p;\n",[StructName]),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_jbe:emit_type_function(G, N, X, Fd),

    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s _this)\n",
		    [?ICPACKAGE,StructName]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static ~s extract(~sAny _any)\n",
		    [StructName,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
    ic_codegen:emit(Fd, "   }\n\n"),


    %% In corba mapping there is also a _type function here.
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

    
%%-----------------------------------------------------------------
%% Func: emit_struct_members_declarations/
%%-----------------------------------------------------------------
emit_struct_members_declarations(_, _, _, _, []) ->
    ok;
emit_struct_members_declarations(G, N, X, Fd, [{Member, Type, Id} | MList]) ->
    ic_codegen:emit(Fd, "   public ~s ~s;\n",
		    [ic_java_type:getType(G, N, Member),
		     Id]),
    emit_struct_members_declarations(G, N, X, Fd, MList).



%%-----------------------------------------------------------------
%% Func: emit_struct_members_initialisation/5
%%-----------------------------------------------------------------
emit_struct_members_initialisation(_, _, _, _, []) ->
    ok;
emit_struct_members_initialisation(G, N, X, Fd, [{Member, Type, Id} | MList]) ->
    ic_codegen:emit(Fd, "     ~s = _~s;\n", [Id, Id]),
    emit_struct_members_initialisation(G, N, X, Fd, MList).



			       
%%-----------------------------------------------------------------
%% Func: emit_struct_marshal_function/7
%%-----------------------------------------------------------------
emit_struct_marshal_function(G, N, X, Fd, StructName, WireStructName, MList) ->

    ic_codegen:emit(Fd, "     _out.write_tuple_head(~p);\n", [length(MList) + 1]),
    ic_codegen:emit(Fd, "     _out.write_atom(~p);\n\n", [ic_util:to_undersc([WireStructName|N])]),

    emit_struct_marshal_function_loop(G, [StructName ++ "Package" |N],
				      X, Fd, MList, 1).

%%-----------------------------------------------------------------
%% Func: emit_struct_marshal_function_loop/6
%%-----------------------------------------------------------------
emit_struct_marshal_function_loop(_, _, _, Fd, [], _) ->
    ic_codegen:nl(Fd);
emit_struct_marshal_function_loop(G, N, X, Fd, [{Member, Type, Id} |MList], Num) ->
    
    case ic_java_type:isBasicType(G, N, Member) of
	true ->
	    ic_codegen:emit(Fd, "     _out~s(_value.~s);\n",
			    [ic_java_type:marshalFun(G, N, Member, Type),
			     Id]);
	_ ->
	    if (element(1,hd(element(3,Member))) == array) ->
		    ic_codegen:emit(Fd, "     ~sHelper.marshal(_out, _value.~s);\n",
				    [ic_util:to_dot(G,[ic_forms:get_id2(Member)|N]),
				     Id]);
	       true ->
		    ic_codegen:emit(Fd, "     ~s(_out, _value.~s);\n",
				    [ic_java_type:marshalFun(G, N, Member, Type),
				     Id])
	    end
    end,
    
    emit_struct_marshal_function_loop(G, N, X, Fd, MList, Num+1).




%%-----------------------------------------------------------------
%% Func: emit_struct_unmarshal_function/7
%%-----------------------------------------------------------------
emit_struct_unmarshal_function(G, N, X, Fd, StructName, WireStructName, MList) ->

    ic_codegen:emit(Fd, "     _in.read_tuple_head();\n\n"),
    
    ic_codegen:emit(Fd, "     if ((_in.read_atom()).compareTo(~p) != 0)\n",
	 	    [ic_util:to_undersc([WireStructName|N])]),
    ic_codegen:emit(Fd, "       throw new ~sOtpErlangDataException(\"\");\n\n",
	 	    [?ERLANGPACKAGE]),

    ic_codegen:emit(Fd, "     ~s _value = new ~s();\n", [StructName, StructName]),
    
    emit_struct_unmarshal_function_loop(G, [StructName ++ "Package"|N],
					X, Fd, MList, 1),
    
    ic_codegen:emit(Fd, "     return _value;\n").

%%-----------------------------------------------------------------
%% Func:  emit_union_unmarshal_function_loop/6
%%-----------------------------------------------------------------
emit_struct_unmarshal_function_loop(_, _, _, Fd, [], _) ->
    ic_codegen:nl(Fd);
emit_struct_unmarshal_function_loop(G, N, X, Fd, [{Member, Type, Id} |MList], Num) ->

    case ic_java_type:isBasicType(G, N, Member) of
	true ->
	    ic_codegen:emit(Fd, "     _value.~s = _in~s;\n",
		    [Id,
		     ic_java_type:unMarshalFun(G, N, Member, Type)]);
	_ ->
	    if (element(1,hd(element(3,Member))) == array) ->
		    ic_codegen:emit(Fd, "     _value.~s = ~sHelper.unmarshal(_in);\n",
				    [Id,
				     ic_util:to_dot(G,[ic_forms:get_id2(Member)|N])]);
	       true ->
		    ic_codegen:emit(Fd, "     _value.~s = ~s.unmarshal(_in);\n",
				    [Id,
				     ic_java_type:getUnmarshalType(G, N, Member, Type)])
	    end
    end,

    emit_struct_unmarshal_function_loop(G, N, X, Fd, MList, Num +1).



%%-----------------------------------------------------------------
%% Func: gen_parameter_list/4
%%-----------------------------------------------------------------
gen_parameter_list(G, N, X, [{Member, Type, Id}]) ->
    ic_java_type:getType(G,N,Member) ++
	" _" ++ 
	ic_util:to_list(Id);
gen_parameter_list(G, N, X, [{Member, Type, Id} | MList]) ->
    ic_java_type:getType(G,N,Member) ++ 
	" _" ++
	ic_util:to_list(Id) ++ 
	", " ++
	gen_parameter_list(G, N, X, MList).


%%-----------------------------------------------------------------
%% Func: struct_member_list/3
%%-----------------------------------------------------------------
struct_member_list(G, N, X) ->
    M = lists:map(
	  fun(Member) -> 
		  lists:map(
		    fun(Id) ->
			    Type = ic_forms:get_type(Member),
			    { Member, Type, ic_forms:get_java_id(Id)}
		    end, 
		    ic_forms:get_idlist(Member))
	  end,
	  ic_forms:get_body(X)),
    lists:flatten(M).


