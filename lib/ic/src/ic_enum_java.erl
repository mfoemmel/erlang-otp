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

-module(ic_enum_java).

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

%%-----------------------------------------------------------------
%% Func: gen/3
%%-----------------------------------------------------------------
gen(G, N, X) when record(X, enum) ->
    %%?PRINTDEBUG2("enum: ~p", [X]),
    EnumName = ic_forms:get_java_id(X),
    N2 = ["_" ++ EnumName |N],  
    ic_jbe:gen(G, N2, ic_forms:get_body(X)),

    emit_enum_class(G, N, X, EnumName),
    emit_holder_class(G, N, X, EnumName),
    emit_helper_class(G, N, X, EnumName);
gen(G, N, X) -> 
    %%?PRINTDEBUG2("****** IGNORING ******: ~p", [X]),
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: emit_enum_class/4
%%-----------------------------------------------------------------
emit_enum_class(G, N, X, EnumName) ->
    {Fd, _} = ic_file:open_java_file(G, N, EnumName), 

    EList = enum_member_name_list(G, N, X),
    %%?PRINTDEBUG2("EList: ~p", [EList]),
    ic_codegen:emit(Fd, "final public class ~s {\n",[EnumName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    emit_enum_member_int_values_initialization(G, N, X, Fd, EList),
    emit_enum_public_instance_variables(G, N, X, Fd, EnumName, EList),
    ic_codegen:emit(Fd, "   private int _value;\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   private ~s(int __value) {\n", [EnumName]),
    ic_codegen:emit(Fd, "      _value = __value;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
 

    ic_codegen:emit(Fd, "   // methods\n", []),

    ic_codegen:emit(Fd, "   public int value() {\n", []),
    ic_codegen:emit(Fd, "      return _value;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
 
    emit_enum_from_int_function(G, N, X, Fd, EnumName, EList),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, X, EnumName) ->
    EName = string:concat(EnumName, "Holder"),
    {Fd, _} = ic_file:open_java_file(G, N, EName), 
    
    ic_codegen:emit(Fd, "final public class ~sHolder {\n",[EnumName]),

    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   // instance variables\n", []),
    ic_codegen:emit(Fd, "   public ~s value;\n", [EnumName]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   public ~sHolder() {}\n", [EnumName]),
    ic_codegen:emit(Fd, "   public ~sHolder(~s initial) {\n", [EnumName, EnumName]),
    ic_codegen:emit(Fd, "      value = initial;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n", []),
    ic_codegen:emit(Fd, "   public void _marshal(~sOtpOutputStream out) throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      ~sHelper.marshal(out, value);\n", [EnumName]),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   public void _unmarshal(~sOtpInputStream in) throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]),  
    ic_codegen:emit(Fd, "      value = ~sHelper.unmarshal(in);\n", [EnumName]),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_helper_class/4
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, EnumName) ->
    EName = string:concat(EnumName, "Helper"),
    WEList = enum_member_atom_list(G, N, X),
    {Fd, _} = ic_file:open_java_file(G, N, EName), 
    
    ic_codegen:emit(Fd, "public class ~sHelper {\n",[EnumName]),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   private ~sHelper() {}\n", [EnumName]),
    ic_codegen:nl(Fd),
    
    ic_codegen:emit(Fd, "   // methods\n", []),

    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s _value)\n",
		    [?ERLANGPACKAGE, EnumName]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"), 
    emit_enum_write_function(G, N, X, Fd, EnumName),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   public static ~s unmarshal(~sOtpInputStream _in)\n",
		    [EnumName, ?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
    emit_enum_read_function(G, N, X, Fd, EnumName),
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   }\n\n", []),

    emit_enum_private_member_variables(Fd, WEList),

    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   // Get integer value of enum from string\n", []),
    ic_codegen:emit(Fd, "   private static int _getIntFromName(String name)"
		    " throws java.lang.Exception {\n",
		    []), 
    ic_codegen:emit(Fd, "      for(int i = 0; i < _memberCount; i++) {\n", []),
    ic_codegen:emit(Fd, "         if (name.equals(_members[i]))\n", []),
    ic_codegen:emit(Fd, "            return i;\n", []),
    ic_codegen:emit(Fd, "      }\n", []),
    ic_codegen:emit(Fd, "      throw new ~sOtpErlangDataException"
		    "(\"\");\n",
		    [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "   }\n\n", []),
    
    ic_codegen:emit(Fd, "   public static String id() {\n", []), 
    ic_codegen:emit(Fd, "      return ~p;\n",[ictk:get_IR_ID(G, N, X)]),
    ic_codegen:emit(Fd, "   }\n\n", []),

    ic_codegen:emit(Fd, "   public static String name() {\n"), 
    ic_codegen:emit(Fd, "      return ~p;\n",[EnumName]),
    ic_codegen:emit(Fd, "   }\n\n"),
    
    ic_jbe:emit_type_function(G, N, X, Fd),

    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s _this)\n",
		    [?ICPACKAGE,EnumName]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static ~s extract(~sAny _any)\n",
		    [EnumName,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_enum_public_instance_variables/6
%%-----------------------------------------------------------------
emit_enum_public_instance_variables(G, N, X, Fd, EnumName, []) ->
    ok;
emit_enum_public_instance_variables(G, N, X, Fd, EnumName, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, "   public static final ~s ~s = new ~s(_~s);\n",
		    [EnumName, Enumerator, EnumName, Enumerator]),
    emit_enum_public_instance_variables(G, N, X, Fd, EnumName, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_member_int_values_initialization/5
%%-----------------------------------------------------------------
emit_enum_member_int_values_initialization(G, N, X, Fd, EList) ->
    InitString = emit_enum_member_int_values_initialization_1(G, N, X, Fd, EList, 0),
    ic_codegen:emit(Fd, "   public static final int ~s;\n", [InitString]).


%%-----------------------------------------------------------------
%% Func:  emit_enum_member_int_values_initialization_1/6
%%-----------------------------------------------------------------
emit_enum_member_int_values_initialization_1(G, N, X, Fd, [Enumerator], Num) ->
    "                           _" ++ Enumerator ++ " = " ++ ic_util:to_list(Num);
emit_enum_member_int_values_initialization_1(G, N, X, Fd, [Enumerator |EList], Num) ->
    Spaces = if
	Num == 0 ->
	    "";
	true ->
	    "                           "
    end,
    Spaces ++ "_" ++ Enumerator ++ " = " ++ ic_util:to_list(Num) ++ ",\n" ++
	emit_enum_member_int_values_initialization_1(G, N, X, Fd, EList, Num + 1).

%%-----------------------------------------------------------------
%% Func:  emit_enum_from_int_function/6
%%-----------------------------------------------------------------
emit_enum_from_int_function(G, N, X, Fd, EnumName, EList) ->
    ic_codegen:emit(Fd, "   public static final ~s from_int(int __value) "
		    " throws java.lang.Exception {\n", [EnumName]),
    ic_codegen:emit(Fd, "      switch (__value) {\n", []),
    emit_enum_from_int_function_switchbody(Fd, EList),
    ic_codegen:emit(Fd, "      }\n", []),
    ic_codegen:emit(Fd, "   }\n", []).
    
%%-----------------------------------------------------------------
%% Func:  emit_enum_from_int_function_switchbody/2
%%-----------------------------------------------------------------
emit_enum_from_int_function_switchbody(Fd, []) ->
    ic_codegen:emit(Fd, "         default:\n", []),
    ic_codegen:emit(Fd, "            throw new ~s"
		    "OtpErlangDataException(\"\");\n",
		    [?ERLANGPACKAGE]);
emit_enum_from_int_function_switchbody(Fd, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, "         case _~s:\n", [Enumerator]),
    ic_codegen:emit(Fd, "            return ~s;\n", [Enumerator]),    
    emit_enum_from_int_function_switchbody(Fd, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_private_member_variables/2
%%-----------------------------------------------------------------
emit_enum_private_member_variables(Fd, EList) ->
    ic_codegen:emit(Fd, "   private static final int _memberCount = ~p;\n",
		    [length(EList)]),
    ic_codegen:emit(Fd, "   private static String[] _members  = {\n", []),
    emit_enum_private_member_variables_1(Fd, EList),
    ic_codegen:emit(Fd, "   };\n", []).

%%-----------------------------------------------------------------
%% Func:  emit_enum_private_member_variables_1/2
%%-----------------------------------------------------------------
emit_enum_private_member_variables_1(Fd, [Enumerator]) ->
    ic_codegen:emit(Fd, "      ~p\n", [Enumerator]);
emit_enum_private_member_variables_1(Fd, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, "      ~p,\n", [Enumerator]),
    emit_enum_private_member_variables_1(Fd, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_read_function/5
%%-----------------------------------------------------------------
emit_enum_read_function(G, N, X, Fd, EnumName) ->
    ic_codegen:emit(Fd, "     return ~s.from_int(_getIntFromName(_in.read_atom()));",
		    [EnumName]).

%%-----------------------------------------------------------------
%% Func:  emit_enum_write_function/5
%%-----------------------------------------------------------------
emit_enum_write_function(G, N, X, Fd, EnumName) ->
    ic_codegen:emit(Fd, "     _out.write_atom(_members[_value.value()]);\n").


%%-----------------------------------------------------------------
%% Func:  enum_member_name_list/3
%%
%% Note: The names generated are checked for name coalition 
%%       with java keywords. If so the name is always prefixed
%%       by "_"
%%-----------------------------------------------------------------
enum_member_name_list(G, N, X) ->
    M = lists:map(
	  fun(Enumerator) -> 
		  ic_forms:get_java_id(Enumerator)
	  end,
	  ic_forms:get_body(X)).

%%-----------------------------------------------------------------
%% Func:  enum_member_atom_list/3
%%
%% Note : Similar to the emit_member_list/3 but does not
%%        solves name coalitions with java keywords.
%%        Used for wire encoding only 
%%-----------------------------------------------------------------
enum_member_atom_list(G, N, X) ->
    M = lists:map(
	  fun(Enumerator) -> 
		  ic_forms:get_id2(Enumerator)
	  end,
	  ic_forms:get_body(X)).


