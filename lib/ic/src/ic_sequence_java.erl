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

-module(ic_sequence_java).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([gen/4]).
-compile(export_all).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: gen/4
%%-----------------------------------------------------------------
gen(G, N, X, SequenceName) when record(X, sequence) ->
    %%?PRINTDEBUG2("******** SEQ ********: \n~p,\n~p", [SequenceName, X]),
    emit_holder_class(G, N, X, SequenceName),
    emit_helper_class(G, N, X, SequenceName);
%gen(G, N, X, SequenceName) when record(X, typedef) -> %% A typedef of a sequence
%                                                      %% Stoped due to compatibility problems
%                                                      %% with erl_genserv backend
%    %%?PRINTDEBUG2("*** TYPEDEF of SEQ **: \n~p,\n~p", [SequenceName, X]),
%    emit_helper_class(G, N, X, SequenceName);
gen(G, N, X, SequenceName) -> 
    %%?PRINTDEBUG2("****** IGNORING ******: \n~p,\n~p", [SequenceName, X]),
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------


%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, X, SequenceName) ->
    SName = string:concat(SequenceName, "Holder"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),

    SequenceType = ic_java_type:getType(G, N, X),

    ic_codegen:emit(Fd, "final public class ~sHolder {\n",[SequenceName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    ic_codegen:emit(Fd, "   public ~s value;\n", [SequenceType]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   public ~sHolder() {}\n", [SequenceName]),
    ic_codegen:emit(Fd, "   public ~sHolder(~s initial) {\n",
		    [SequenceName, SequenceType]),
    ic_codegen:emit(Fd, "      value = initial;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n", []),

    ic_codegen:emit(Fd, "   public void _marshal(~sOtpOutputStream out) "
		    "throws java.lang.Exception{\n",
		    [?ERLANGPACKAGE]),  
    ic_codegen:emit(Fd, "      ~sHelper.marshal(out, value);\n",
		    [SequenceName]),
    ic_codegen:emit(Fd, "   }\n", []),
    
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   public void _unmarshal(~sOtpInputStream in) "
		    "throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]), 
    ic_codegen:emit(Fd, "      value = ~sHelper.unmarshal(in);\n",
		    [SequenceName]),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_helper_class/4
%%-----------------------------------------------------------------

%emit_helper_class(G, N, X, SequenceName) when record(X, typedef) -> 
%%                                %% Stoped due to compatibility problems
%%                                %% with erl_genserv backend
%    SName = string:concat(SequenceName, "Helper"),
%    {Fd, _}= ic_file:open_java_file(G, N, SName),
    
%    SequenceType = ic_java_type:getType(G, N, X#typedef.type),

%    {FullScopedName, _, _, _} = 
%	ic_symtab:get_full_scoped_name(G, N, X#typedef.type),
%    OriginalType = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)),

%    ic_codegen:emit(Fd, "public class ~sHelper {\n",[SequenceName]),
    
%    ic_codegen:emit(Fd, "   // constructors\n", []),
%    ic_codegen:emit(Fd, "   private ~sHelper() {}\n", [SequenceName]),
%    ic_codegen:nl(Fd),

%    ic_codegen:emit(Fd, "   // methods\n", []),
%    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s _value) \n"
%		        "     throws java.lang.Exception {\n\n",
%		    [?ERLANGPACKAGE, SequenceType]), 

%    ic_codegen:emit(Fd, "       ~sHelper.marshal(_out, _value);\n", [OriginalType]), 
    
%    ic_codegen:emit(Fd, "   }\n", []),
%    ic_codegen:nl(Fd),

%    ic_codegen:emit(Fd, "   public static ~s unmarshal(~sOtpInputStream _in) \n"
%		        "     throws java.lang.Exception {\n\n",
%		    [SequenceType,?ERLANGPACKAGE]),

%    ic_codegen:emit(Fd, "       return ~sHelper.unmarshal(_in);\n", [OriginalType]), 

%    ic_codegen:emit(Fd, "   }\n\n"),
    
%    ic_codegen:emit(Fd, "   public static String id() {\n"), 
%    ic_codegen:emit(Fd, "      return ~p;\n",
%		    [ic_pragma:scope2id(G, [SequenceName | N])]),
%    ic_codegen:emit(Fd, "   }\n\n"),

%    ic_codegen:emit(Fd, "   public static String name() {\n"), 
%    ic_codegen:emit(Fd, "      return ~p;\n", [SequenceName]),
%    ic_codegen:emit(Fd, "   }\n\n"),

%    ic_jbe:emit_type_function(G, N, X, Fd),
    
%    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s _this)\n",
%		    [?ICPACKAGE,SequenceType]),
%    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
%    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
%    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
%    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
%    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
%    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
%    ic_codegen:emit(Fd, "   }\n\n"),

%    ic_codegen:emit(Fd, "   public static ~s extract(~sAny _any)\n",
%		    [SequenceType,?ICPACKAGE]),
%    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
%    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
%    ic_codegen:emit(Fd, "   }\n\n"),
    
%    ic_codegen:emit(Fd, "}\n", []),
%    file:close(Fd);

emit_helper_class(G, N, X, SequenceName) ->
    SName = string:concat(SequenceName, "Helper"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),
    
    SequenceType = ic_java_type:getType(G, N, X),
    ElementType = ic_forms:get_type(X),

    ic_codegen:emit(Fd, "public class ~sHelper {\n",[SequenceName]),
    
    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   private ~sHelper() {}\n", [SequenceName]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n", []),
    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s _value) \n"
		        "     throws java.lang.Exception {\n\n",
		    [?ERLANGPACKAGE, SequenceType]), 

    emit_sequence_marshal_function(G, N, X, Fd, SequenceName, ElementType),

    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   public static ~s unmarshal(~sOtpInputStream _in) \n"
		        "     throws java.lang.Exception {\n\n",
		    [SequenceType,?ERLANGPACKAGE]),

    emit_sequence_unmarshal_function(G, N, X, Fd, SequenceName, ElementType),

    ic_codegen:emit(Fd, "   }\n\n"),
    
    ic_codegen:emit(Fd, "   public static String id() {\n"), 
    ic_codegen:emit(Fd, "      return ~p;\n",
		    [ic_pragma:scope2id(G, [SequenceName | N])]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static String name() {\n"), 
    ic_codegen:emit(Fd, "      return ~p;\n", [SequenceName]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_jbe:emit_type_function(G, N, X, Fd),
    
    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s _this)\n",
		    [?ICPACKAGE,SequenceType]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static ~s extract(~sAny _any)\n",
		    [SequenceType,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
    ic_codegen:emit(Fd, "   }\n\n"),
    

    %% In corba mapping there is also a _type function here.
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func: emit_sequence_marshal_function/6
%%-----------------------------------------------------------------
emit_sequence_marshal_function(G, N, X, Fd, SequenceName,
				 ElementType) ->
    ic_codegen:emit(Fd, "    int _length = _value.length;\n\n"),

    ic_codegen:emit(Fd, "    _out.write_list_head(_length);\n\n"),

    ic_codegen:emit(Fd, "    if (_length > 0) {\n"),
    ic_codegen:emit(Fd, "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"),

    case ic_java_type:isBasicType(G, N, ElementType) of 
	true ->
	    ic_codegen:emit(Fd, "        _out~s(_value[_tmp]);\n\n", 
			    [ic_java_type:marshalFun(G, N, X, ElementType)]);
	false ->
	    ic_codegen:emit(Fd, "        ~s(_out, _value[_tmp]);\n\n", 
			    [ic_java_type:marshalFun(G, N, X, ElementType)])
    end,
    
    ic_codegen:emit(Fd, "      _out.write_nil();\n"),
    ic_codegen:emit(Fd, "    }\n\n").




%%-----------------------------------------------------------------
%% Func: emit_sequence_unmarshal_function/6
%%-----------------------------------------------------------------
emit_sequence_unmarshal_function(G, N, X, Fd, SequenceName, ElementType) ->

    SequenceElementType = ic_java_type:getType(G, N, ElementType),

    ic_codegen:emit(Fd, "    int _tag,_length;\n"),
    ic_codegen:emit(Fd, "    ~s _sequence[];\n", [SequenceElementType]),
    ic_codegen:emit(Fd, "    _tag = _in.peek();\n\n"),

    case ic_java_type:isIntegerType(G, N, ElementType) of
	true ->
	    ic_codegen:emit(Fd, "    switch(_tag) {\n"),
	    ic_codegen:emit(Fd, "    case ~sOtpExternal.stringTag:\n", [?ERLANGPACKAGE]),
	    ic_codegen:emit(Fd, "      byte _compressed[] = (_in.read_string()).getBytes();\n"),
	    ic_codegen:emit(Fd, "      _length = _compressed.length;\n"),
	    ic_codegen:emit(Fd, "      _sequence = new ~s;\n\n",
			    [ic_java_type:getFullType(G,N,X)]),
	    
	    ic_codegen:emit(Fd, "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"),
	    ic_codegen:emit(Fd, "        _sequence[_tmp] = (~s)(_compressed[_tmp] & 0xff);\n\n",
			    [ic_java_type:getType(G, N, ElementType)]),
	    
	    ic_codegen:emit(Fd, "      break;\n"),
	    ic_codegen:emit(Fd, "    default:\n"),
	    ic_codegen:emit(Fd, "      _length = _in.read_list_head();\n"),
	    ic_codegen:emit(Fd, "      _sequence = new ~s;\n\n",
			    [ic_java_type:getFullType(G,N,X)]),
	    
	    ic_codegen:emit(Fd, "      if(_length > 0) {\n"),
	    ic_codegen:emit(Fd, "        for(int _tmp = 0; _tmp < _length; _tmp++)\n"),
	    ic_codegen:emit(Fd, "          _sequence[_tmp] = _in~s;\n\n",
			    [ic_java_type:unMarshalFun(G, N, X, ElementType)]),
	    
	    ic_codegen:emit(Fd, "        _in.read_nil();\n"),
	    ic_codegen:emit(Fd, "      }\n"),
	    ic_codegen:emit(Fd, "    }\n");
	false ->
	    ic_codegen:emit(Fd, "    _length = _in.read_list_head();\n"),
	    ic_codegen:emit(Fd, "    _sequence = new ~s;\n\n",
			    [ic_java_type:getFullType(G,N,X)]),
	    
	    ic_codegen:emit(Fd, "    if(_length > 0) {\n"),
	    ic_codegen:emit(Fd, "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"),
	    case ic_java_type:isBasicType(G, N, ElementType) of 
		true ->
		    ic_codegen:emit(Fd, "        _sequence[_tmp] = _in~s;\n\n",
				    [ic_java_type:unMarshalFun(G, N, X, ElementType)]);
		_ ->
		    ic_codegen:emit(Fd, "        _sequence[_tmp] = ~s.unmarshal(_in);\n\n",
				    [ic_java_type:getUnmarshalType(G, N, X, ElementType)])
	    end,
	    
	    ic_codegen:emit(Fd, "      _in.read_nil();\n"),
	    ic_codegen:emit(Fd, "    }\n\n")
    end,
	    
    ic_codegen:emit(Fd, "    return _sequence;\n", []).




%%---------------------------------------------------
%%  Utilities
%%---------------------------------------------------









