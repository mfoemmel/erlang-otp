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
-module(icstruct).


-export([struct_gen/4, except_gen/4, create_c_array_coding_file/5, gen_base_type/3]).

%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------
-include("icforms.hrl").
-include("ic.hrl").

%%------------------------------------------------------------

%%------------------------------------------------------------
%%
%% File handling stuff
%%
%%------------------------------------------------------------



%%------------------------------------------------------------
%%
%% Generation loop
%%
%%	The idea is to traverse everything and find every struct that
%%	may be hiding down in nested types. All structs that are found
%%	are generated to a hrl file.
%%
%%	struct_gen is entry point for structs and types, except_gen is
%%	for exceptions
%%
%%------------------------------------------------------------


except_gen(G, N, X, L) when record(X, except) ->
    N2 = [ic_forms:get_id2(X) | N],
    if
	L == c ->
	    io:format("Warning : Exception not defined for c mapping\n", []);
	true ->
	    emit_struct(G, N, X, L)
    end,
    struct_gen_list(G, N2, ic_forms:get_body(X), L).

struct_gen(G, N, X, L) when record(X, struct) ->
    N2 = [ic_forms:get_id2(X) | N],
    struct_gen_list(G, N2, ic_forms:get_body(X), L),
    emit_struct(G, N, X, L);
struct_gen(G, N, X, L) when record(X, union) ->
    N2 = [ic_forms:get_id2(X) | N],
    if
	L == c ->
	    struct_gen_list(G, N2, ic_forms:get_body(X), L), % Produce the "body" first
	    icunion:union_gen(G, N, X, c);
	true ->
	    struct_gen(G, N, ic_forms:get_type(X), L),
	    struct_gen_list(G, N2, ic_forms:get_body(X), L)
    end,
    emit_union(G, N, X, L);
struct_gen(G, N, X, L) when record(X, member) ->
    struct_gen(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when record(X, typedef) ->
    struct_gen(G, N, ic_forms:get_body(X), L),
    emit_typedef(G, N, X, L);
struct_gen(G, N, X, L) when record(X, type_dcl) ->
    struct_gen_list(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when record(X, case_dcl) ->
    struct_gen(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when record(X, sequence) ->
    struct_gen(G, N, ic_forms:get_type(X), L),
    X;
struct_gen(G, N, X, L) when record(X, enum) -> 
    icenum:enum_gen(G, N, X, L);
struct_gen(G, N, X, L) -> 
    %%io:format("*** IGNORING *** ~p~n", [X]),
    ok.

%% List clause for struct_gen
struct_gen_list(G, N, Xs, L) -> 
    lists:foreach(fun(X) ->
			  R = struct_gen(G, N, X, L),
			  if
			      L == c ->
				  if
				      record(R,sequence) ->
					  emit_sequence_head_def(G,N,X,R,L);
				      true ->
					  ok
				  end;
			      true ->
				  ok
			  end
		  end, Xs).
    

%% emit primitive for structs.
emit_struct(G, N, X, erlang) ->
    case ic_genobj:is_hrlfile_open(G) of
        true ->
            %% Make a straight list of all member ids (this is a
            %% variant of flatten)
            EList = lists:map(fun(XX) -> 
                                  lists:map(fun(XXX) ->
                                                    ic_util:to_atom(ic_forms:get_id2(XXX))
					    end,
                                            ic_forms:get_idlist(XX))
			      end,
			      ic_forms:get_body(X)),
            ic_codegen:record(G, X, ic_util:to_undersc([ic_forms:get_id2(X) | N]), 
                         ictk:get_IR_ID(G, N, X), lists:flatten(EList)),
	    mkFileRecObj(G,N,X,erlang);
	false -> ok
    end;
emit_struct(G, N, X, c) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),

	    N1 = [ic_forms:get_id2(X) | N],
	    StructName = ic_util:to_undersc(N1),
	
	    %% Make a straight list of all member ids (this is a
	    %% variant of flatten)
	    M = lists:map(
		  fun(XX) -> 
			  lists:map(
			    fun(XXX) ->
				    if record(XXX, array) ->
					    Type = ic_forms:get_type(XX),
					    Name = element(3,element(2,XXX)),
					    {_, _, StructTK, _} =
						ic_symtab:get_full_scoped_name(G, N, ic_symtab:scoped_id_new(ic_forms:get_id2(X))),
					    ArrayTK = get_structelement_tk(StructTK, Name),
					    Dim = extract_dim(ArrayTK),
					    %% emit array file
					    ic_codegen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(StructName ++ "_" ++ Name)]),	
					    ic_codegen:emit(Fd, "#define __~s__\n\n",[ic_util:to_uppercase(StructName ++ "_" ++ Name)]),
					    create_c_array_coding_file(G,N,{StructName ++ "_" ++ Name,Dim},Type,
								       no_typedef),
					    ic_codegen:emit(Fd, "\n#endif\n\n"),
					    { {Type, XXX}, ic_forms:get_id2(XXX) };
				       true ->
					    { ic_forms:get_type(XX), ic_forms:get_id2(XXX)}
				    end
			    end,
			    ic_forms:get_idlist(XX))
		  end,
		  ic_forms:get_body(X)),

	    EList = lists:flatten(M),
	    %%io:format("Elist = ~p~n",[EList]),
     
	    ic_codegen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(StructName)]),	
	    ic_codegen:emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(StructName)]),
	    ic_codegen:mcomment_light(Fd,
				 [io_lib:format("Struct definition: ~s",
						[StructName])],
				 c),
	    ic_codegen:emit(Fd, "typedef struct {\n"),
	    lists:foreach(fun({Type, Name}) ->
				  gen_struct_member(Fd, G, N1, X, Name, Type)
			  end,
			  EList),
	    ic_codegen:emit(Fd, "} ~s;\n\n", [StructName]),	
	    create_c_struct_coding_file(G, N, X, StructName, EList, struct),
	    ic_codegen:emit(Fd, "\n#endif\n\n");

	false -> ok
    end.

%% Extracts array dimention(s)

get_structelement_tk({tk_struct, _, _, EList}, EN) ->
    {value, {EN, ArrayTK}} = lists:keysearch(EN, 1, EList),
    ArrayTK.

extract_dim({tk_array, {tk_array, T, D1}, D}) ->
    [integer_to_list(D) | extract_dim({tk_array, T, D1})];
extract_dim({tk_array, _, D}) ->
    [integer_to_list(D)].

%% Makes the array name
mk_array_name(Name,[]) ->
    Name ++ "[]";
mk_array_name(Name,Dim) ->
    Name ++ mk_array_name(Dim).

mk_array_name([]) ->
    "";
mk_array_name([Dim|Dims]) ->
    "[" ++ Dim ++ "]" ++ mk_array_name(Dims).


gen_struct_member(Fd, G, N, X, Name,{Type,Array}) when record(Array, array)->
    {_, _, StructTK, _} = ic_symtab:get_full_scoped_name(G, N, ic_symtab:scoped_id_new(ic_forms:get_id2(X))),
    ArrayTK = get_structelement_tk(StructTK, Name),
    Dim = extract_dim(ArrayTK),
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [ic_cbe:gen_cc_type(G, N, Type),mk_array_name(Name,Dim)]);
gen_struct_member(Fd, G, N, X, Name, Union) when record(Union, union)->
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [ic_util:to_undersc([ic_forms:get_id2(Union) | N]),Name]);
gen_struct_member(Fd, G, N, X, Name, {string, _}) ->
    ic_codegen:emit(Fd, "   CORBA_char *~s;\n",
	       [Name]);
gen_struct_member(Fd, G, N, X, Name, {sequence, Type, Length}) ->
    %% Sequence used as struct
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [ic_util:to_undersc([Name | N]), Name]);
gen_struct_member(Fd, G, N, X, Name, Type) when element(1, Type) == scoped_id ->
    CType = ic_cbe:gen_cc_type(G, N, Type, evaluate_not),
    gen_struct_member(Fd, G, N, X, Name, CType);
gen_struct_member(Fd, G, N, X, Name, {enum, Type}) ->
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [ic_cbe:gen_cc_type(G, N, Type),
		Name]);
gen_struct_member(Fd, G, N, X, Name, "ETERM*") ->
    ic_codegen:emit(Fd, "   ETERM* ~s;\n",
	       [Name]);
gen_struct_member(Fd, G, N, X, Name, Type) when list(Type) ->  
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [Type, Name]);
gen_struct_member(Fd, G, N, X, Name, Type) ->
    ic_codegen:emit(Fd, "   ~s ~s;\n",
	       [ic_cbe:gen_cc_type(G, N, Type),
		Name]).


emit_typedef(G, N, X, erlang) -> 
    ok;
emit_typedef(G, N, X, c) ->
    B = ic_forms:get_body(X),
    if
	record(B, sequence) ->
	    emit_sequence_head_def(G, N, X, B, c);
	true ->
	    lists:foreach(fun(D) ->
				  emit_typedef(G, N, D, B, c)
			  end, 
			  ic_forms:get_idlist(X))
    end.
 
emit_typedef(G, N, D, Type, c) when record(D, array) ->
    emit_array(G, N, D, Type);
emit_typedef(G, N, D, Type, c)  ->
    Name = ic_util:to_undersc([ic_forms:get_id2(D) | N]),
    CType = ic_cbe:gen_cc_type(G, N, Type),
    TDType = gen_base_type(G, N, Type),
    ic_code:insert_typedef(G, Name, TDType),
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    ic_codegen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(Name)]),	
	    ic_codegen:emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(Name)]),
	    ic_codegen:mcomment_light(Fd,
				 [io_lib:format("Type definition ~s for type  ~s",
						[Name, CType])],
				 c),
	    ic_codegen:emit(Fd, "typedef ~s ~s;\n",
		       [CType, Name]),
	    ic_codegen:emit(Fd, "\n#endif\n\n"),
	    ic_codegen:nl(Fd);
	false ->
	    ok
    end.


gen_base_type(G, N, S) when element(1, S) == scoped_id ->
    {FullScopedName, T, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    BT = ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)),
    case BT of
	"erlang_pid" ->
	    "erlang_pid";
	"erlang_port" ->
	    "erlang_port";
	"erlang_ref" ->
	    "erlang_ref";
	"erlang_term" ->
	    "ETERM*";
	Type ->
	    Type
    end;
gen_base_type(G, N, S) ->
    S.

emit_array(G, N, D, Type) -> 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    Name = ic_util:to_undersc([ic_forms:get_id2(D) | N]),
	    {_, _, ArrayTK, _} = ic_symtab:get_full_scoped_name(G, N, ic_symtab:scoped_id_new(ic_forms:get_id(D))),
	    Dim = extract_dim(ArrayTK),
	    CType = ic_cbe:gen_cc_type(G, N, Type),
	    SName = string:concat(ic_util:mk_oe_name(G, "code_"), Name),

	    ic_codegen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(Name)]),	
	    ic_codegen:emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(Name)]),
	    ic_codegen:mcomment_light(Fd,
				 [io_lib:format("Array definition ~s for type  ~s",
						[Name, CType])],
				 c),
	    ic_codegen:emit(Fd, "typedef ~s ~s~s;\n",
		       [CType, Name, ic_cbe:mk_dim(Dim)]),
	    ic_codegen:emit(Fd, "typedef ~s ~s_slice~s;\n",
		       [CType, Name, ic_cbe:mk_slice_dim(Dim)]),
	    ic_codegen:nl(Fd),
	    create_c_array_coding_file(G, N, {Name, Dim}, Type, typedef),
	    ic_codegen:emit(Fd, "\n#endif\n\n");
	false ->
	    ok
    end.

open_c_coding_file(G, Name) ->
    SName = string:concat(ic_util:mk_oe_name(G, "code_"), Name),
    FName =  
        ic_file:join(ic_options:get_opt(G, stubdir),ic_file:add_dot_c(SName)),
    case file:rawopen(FName, {binary, write}) of
        {ok, Fd} ->
            {Fd, SName};
        Other ->
            exit(Other)
    end.



create_c_array_coding_file(G, N, {Name, Dim}, Type, TypeDefFlag) ->

    {Fd , SName} = open_c_coding_file(G, Name), 
    HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    ic_codegen:emit_stub_head(G, Fd, SName, c),
    ic_codegen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),

    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Fd = ic_genobj:stubfiled(G), %% Write on stubfile
    %%  HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    %%  HrlFName = filename:basename(ic_genobj:include_file(G)),
    %%  ic_codegen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	       [ic_util:mk_oe_name(G, "sizecalc_"), Name]),

    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size) {\n",
	       [ic_util:mk_oe_name(G, "sizecalc_"), Name]),

    ic_codegen:emit(Fd, "  int oe_malloc_size = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_type = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_array_size = 0;\n",[]),

    {ok, RamFd} = ram_file:open([], [binary, write]),

    emit_sizecount(array, G, N, RamFd, {Name, Dim}, Type),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data} = ram_file:get_file(RamFd),
    ic_codegen:emit(Fd, Data),
    ram_file:close(RamFd),

    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),
    
    put(op_variable_count, 0),
    put(tmp_declarations, []),

    RefStr = get_refStr(Dim),

    case TypeDefFlag of
	typedef ->
	    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s);\n",
		       [ic_util:mk_oe_name(G, "encode_"), Name, Name]),

	    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec) {\n\n",
		       [ic_util:mk_oe_name(G, "encode_"), Name, Name]);
	no_typedef ->
	    
	    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec~s);\n",
		       [ic_util:mk_oe_name(G, "encode_"), 
			Name,  
			ic_cbe:gen_cc_type(G, N, Type),
			RefStr]),
	    
	    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec~s) {\n\n",
		       [ic_util:mk_oe_name(G, "encode_"), 
			Name,
			ic_cbe:gen_cc_type(G, N, Type),
			RefStr])
    end,

    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd1} = ram_file:open([], [binary, write]),

    case TypeDefFlag of
	typedef ->
	    emit_encode(array, G, N, RamFd1, {Name, Dim}, Type);
	no_typedef ->
	    emit_encode(array_no_typedef, G, N, RamFd1, {Name, Dim}, Type)
    end,

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data1} = ram_file:get_file(RamFd1),
    ic_codegen:emit(Fd, Data1),
    ram_file:close(RamFd1),

    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    case TypeDefFlag of
	typedef ->
	    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s);\n",
		       [ic_util:mk_oe_name(G, "decode_"), Name, Name]), 
	    
	    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_outindex, "
		       "~s oe_out) {\n\n",
		       [ic_util:mk_oe_name(G, "decode_"), Name, Name]);
	no_typedef ->
	    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s oe_rec~s);\n",
		       [ic_util:mk_oe_name(G, "decode_"), 
			Name, 
			ic_cbe:gen_cc_type(G, N, Type),
			RefStr]), 
	    
	    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_outindex, "
		       "~s oe_out~s) {\n\n",
		       [ic_util:mk_oe_name(G, "decode_"), 
			Name, 
			ic_cbe:gen_cc_type(G, N, Type),
			RefStr])
    end,
    

    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_array_size = 0;\n",[]),

    {ok, RamFd2} = ram_file:open([], [binary, write]),

    case TypeDefFlag of
	typedef ->
	    emit_decode(array, G, N, RamFd2, {Name, Dim}, Type);
	no_typedef ->
	    emit_decode(array_no_typedef, G, N, RamFd2, {Name, Dim}, Type)
    end,


    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data2} = ram_file:get_file(RamFd2),
    ic_codegen:emit(Fd, Data2),
    ram_file:close(RamFd2),
    
    ic_codegen:emit(Fd, "  *oe_outindex = ~s;\n\n",[ic_util:mk_align("*oe_outindex")]),

    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),
    file:close(Fd).


get_refStr([]) ->
    "";
get_refStr([X|Xs]) ->
    "[" ++ X ++ "]" ++ get_refStr(Xs).


emit_sequence_head_def(G, N, X, T, c) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    SeqName = ic_util:to_undersc([ic_forms:get_id2(X) | N]),
	    CType = ic_cbe:gen_cc_type(G, N, T#sequence.type),

	    ic_codegen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(SeqName)]),	
	    ic_codegen:emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(SeqName)]),
	    ic_codegen:mcomment_light(Fd,
				 [io_lib:format("Struct definition:  ~s",
						[SeqName])],
				 c),
	    ic_codegen:emit(Fd, "typedef struct {\n"),
	    ic_codegen:emit(Fd, "  CORBA_unsigned_long _maximum;\n"),
	    ic_codegen:emit(Fd, "  CORBA_unsigned_long _length;\n"),
	    gen_seq_buffer(Fd, G, N, T#sequence.type),
	    ic_codegen:emit(Fd, "} ~s;\n\n", [SeqName]),
	    create_c_struct_coding_file(G, N, X, SeqName, 
					T#sequence.type, sequence_head),
	    ic_codegen:emit(Fd, "\n#endif\n\n");

	false ->
	    ok
    end.

gen_seq_buffer(Fd, G, N, Type) ->
    ic_codegen:emit(Fd, "  ~s* _buffer;\n",
	       [ic_cbe:gen_cc_type(G, N, Type)]).

%%------------------------------------------------------------
%%
%% Emit encode/decode functions in C for structs and 
%% sequence header structs
%%
%%------------------------------------------------------------
emit_decode(array, G, N, Fd, {Name, Dim}, Type) ->
    ic_codegen:emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_outindex + ", dim_multiplication(Dim),
			      " * sizeof(", ic_cbe:gen_cc_type(G, N, Type),")"]),
    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n",[ic_util:mk_align(AlignName)]),
    array_decode_dimension_loop(G, N, Fd, Dim, "", Type, array);
emit_decode(array_no_typedef, G, N, Fd, {Name, Dim}, Type) ->
    ic_codegen:emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_outindex + ", dim_multiplication(Dim),
			      " * sizeof(", ic_cbe:gen_cc_type(G, N, Type),")"]),
    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n",[ic_util:mk_align(AlignName)]),
    array_decode_dimension_loop(G, N, Fd, Dim, "", Type, array_no_typedef);
emit_decode(sequence_head, G, N, Fd, StructName, ElType) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    Tname1 = ic_cbe:gen_variable_name(op_variable_count),
    Tname2 = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n", [Tname]),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n", [Tname1]),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n", [Tname2]),

    Tname3 = 
	case ictype:isBasicTypeOrEterm(G, N, ElType) of
	    true ->
		Tmp = ic_cbe:gen_variable_name(op_variable_count),
		ic_cbe:store_tmp_decl("  char* ~s = 0;\n", [Tmp]),
		Tmp;
	    false ->
		"NOT USED"
	end,

    ic_codegen:emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_outindex + sizeof(",StructName,")"]),
    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n",[ic_util:mk_align(AlignName)]),
    ic_codegen:nl(Fd),

    Ctype = ic_cbe:gen_cc_type(G, N, ElType),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, &oe_env->_iin, &~s)) < 0) {\n", [Tname2]),
    case ictype:isBasicTypeOrEterm(G, N, ElType) of
	true ->
	    ic_codegen:emit(Fd, "    int oe_type=0;\n"),

	    ic_codegen:emit(Fd, "    (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, &oe_type, &~s);\n\n", [Tname2]),
	    
	    ic_codegen:emit(Fd, "    oe_out->_length = ~s;\n", [Tname2]),
	    ic_codegen:emit(Fd, "    oe_out->_buffer = (void *) (oe_first + *oe_outindex);\n"),
	    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n",
		       [ic_util:mk_align(io_lib:format("*oe_outindex + (sizeof(~s) * oe_out->_length)",[Ctype]))]),
	    
	    ic_codegen:emit(Fd, "    ~s = (char*) malloc(~s + 1);\n\n", [Tname3, Tname2]),
	    ic_codegen:emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, &oe_env->_iin, ~s)) < 0)\n",[Tname3]),
	    ic_codegen:emit(Fd, "      return oe_error_code;\n\n"),
	    
	    ic_codegen:emit(Fd, "    for(~s = 0; ~s < oe_out->_length; ~s++)\n", [Tname,Tname,Tname]), 
	    
	    case ictype:isBasicType(G, N, ElType) of
		true -> %% BasicType
		    ic_codegen:emit(Fd, "      oe_out->_buffer[~s] = ~s[~s];\n\n",[Tname, Tname3, Tname]);
		false -> %% Term
		    ic_codegen:emit(Fd, "      oe_out->_buffer[~s] = erl_mk_int(~s[~s]);\n\n",[Tname, Tname3, Tname])
	    end,

	    ic_codegen:emit(Fd, "    CORBA_free(~s);\n\n", [Tname3]);
	false ->
	    ic_codegen:emit(Fd, "    return oe_error_code;\n")
    end,
	    
    ic_codegen:emit(Fd, "  } else {\n\n"),

    ic_codegen:emit(Fd, "    oe_out->_length = ~s;\n\n", [Tname2]),

    ic_codegen:emit(Fd, "    oe_out->_buffer = (void *) (oe_first + *oe_outindex);\n\n"),
    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n",
	       [ic_util:mk_align(io_lib:format("*oe_outindex + (sizeof(~s) * oe_out->_length)",[Ctype]))]),
    
    if
	Ctype == "CORBA_char *" ->
	    ic_codegen:emit(Fd, "    for(~s = 0; ~s < oe_out->_length; ~s++) {\n\n",
		       [Tname, Tname, Tname]),

	    ic_codegen:emit(Fd, "      oe_out->_buffer[~s] = (void*) (oe_first + *oe_outindex);\n\n  ",[Tname]),
	    ic_cbe:gen_decoding_fun(G, N, Fd, ElType, "oe_out->_buffer["++Tname++"]", "", "oe_env->_inbuf", 0, "", caller_dyn),
            ic_codegen:emit(Fd, "      *oe_outindex = ~s;",
		       [ic_util:mk_align(io_lib:format("*oe_outindex + strlen(oe_out->_buffer[~s])+1", [Tname]))]);
	true ->
	    ic_codegen:emit(Fd, "    for(~s = 0; ~s < oe_out->_length; ~s++) {\n\n", [Tname, Tname, Tname]),
	    case ictype:isArray(G, N, ElType) of
		true ->
		    ic_cbe:gen_decoding_fun(G, N, Fd, ElType, "oe_out->_buffer["++Tname++"]", "", 
					    "oe_env->_inbuf", 0,"oe_outindex", generator);
		false ->
		    ic_cbe:gen_decoding_fun(G, N, Fd, ElType, "oe_out->_buffer+" ++ Tname, "", 
					    "oe_env->_inbuf", 0,"oe_outindex", generator)
	    end
    end,
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    if (oe_out->_length != 0) {\n"),
    ic_codegen:emit(Fd, "      if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, &oe_env->_iin, &~s)) < 0)\n",
	 [Tname1]),
    ic_codegen:emit(Fd, "        return oe_error_code;\n"),
    ic_codegen:emit(Fd, "    } else\n"),
    ic_codegen:emit(Fd, "        oe_out->_buffer = NULL;\n"),
    ic_codegen:emit(Fd, "  }\n\n");
emit_decode(struct, G, N, Fd, StructName, ElTypes) ->
    Length = length(ElTypes) + 1,
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    Tname1 = ic_cbe:gen_variable_name(op_variable_count),

    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),
    ic_cbe:store_tmp_decl("  char ~s[256];\n\n",[Tname1]),

    ic_codegen:emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_outindex + sizeof(",StructName,")"]),
    ic_codegen:emit(Fd, "    *oe_outindex = ~s;\n\n", [ic_util:mk_align(AlignName)]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &~s)) < 0)\n",
	 [Tname]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  if (~s != ~p)\n",[Tname, Length]),
    ic_codegen:emit(Fd, "    return -1;\n\n"),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, ~s)) < 0)\n", [Tname1]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),
    ic_codegen:emit(Fd, "  if (strcmp(~s, ~p) != 0)\n",[Tname1, StructName]),
    ic_codegen:emit(Fd, "    return -1;\n\n"),
    lists:foreach(fun({ET, EN}) ->
			  case ic_cbe:check_dynamic_size(G, N, ET) of
			      true ->
				  case ET of
				      {sequence, _, _} ->
					  %% Sequence member = a struct
					  ic_cbe:gen_decoding_fun(G, N, Fd, StructName ++ "_" ++ EN, "&oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
				      {_,{array, _, _}} ->
					  ic_codegen:emit(Fd, "  oe_out->~s = (void *) (oe_first+*oe_outindex);\n\n",[EN]),
					  ic_cbe:gen_decoding_fun(G, N, Fd, StructName ++ "_" ++ EN, "oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

				      {union, _, _, _, _} ->
					  %% Sequence member = a union
					  ic_cbe:gen_decoding_fun(G, N, Fd, StructName ++ "_" ++ ic_forms:get_id2(ET), 
								  "&oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

				      {string,_} -> 
					  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator_malloc);

				      {scoped_id,_,_,_} ->
					  case ictype:member2type(G,StructName,EN) of
					      array ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      struct ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      sequence ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      union ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      _ ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator)
					  end;

				      _ ->
					  ic_codegen:emit(Fd, "  oe_out->~s = (void *) (oe_first+*oe_outindex);\n\n",[EN]),
					  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator)
				  end;
			      false ->
				  case ET of

				      {_,{array, _, _}} ->
					  ic_cbe:gen_decoding_fun(G, N, Fd, StructName ++ "_" ++ EN, "oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

				      {union, _, _, _, _} ->
					  %% Sequence member = a union
					  ic_cbe:gen_decoding_fun(G, N, Fd, StructName ++ "_" ++ ic_forms:get_id2(ET), 
								  "&oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

				      {_,_} ->
					  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
								  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
				      {scoped_id,_,_,_} ->
					  case ic_symtab:get_full_scoped_name(G, N, ET) of
					      {FullScopedName, _, {tk_array,_,_}, _} ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      {FullScopedName, _, {tk_string,_}, _} ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);
					      {FullScopedName, _, {tk_struct,_,_,_}, _} ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

					      {FullScopedName, _, {tk_union,_,_,_,_,_}, _} ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator);

					      _ ->
						  ic_cbe:gen_decoding_fun(G, N, Fd, ET, "&oe_out->" ++ EN ,
									  "", "oe_env->_inbuf", 0, "oe_outindex", generator)
					  end
				  end
			  end
		  end,
		  ElTypes).

check_buffer_assign_ref({'tk_sequence', ElemTC, MaxLength}) ->
    "";
check_buffer_assign_ref({'tk_string', MaxLength}) ->
    "";
check_buffer_assign_ref({'tk_array', ElemTC, Length}) ->
    "";
check_buffer_assign_ref(_) ->
    "*".



ref_array_static_dec(array, true) -> % Typedef, Static, Basic Type
    "&(oe_out)";
ref_array_static_dec(array, false) -> % Typedef, Static, Constr Type
    "&(oe_out)";
ref_array_static_dec(array_no_typedef, true) -> % No Typedef, Static, Basic Type
    "&oe_out";
ref_array_static_dec(array_no_typedef, false) -> % No Typedef, Static, Constr Type
    "&oe_out".



ref_array_dynamic_dec(G, N, T, array) ->  
    case ictype:isString(G, N, T) of 
	true ->   % Typedef, Dynamic, String
	    "oe_out";
	false ->  % Typedef, Dynamic, No String
	    "&(oe_out)"
    end;
ref_array_dynamic_dec(G, N, T, array_no_typedef) -> 
    case ictype:isString(G, N, T) of
	true ->   % No Typedef, Dynamic, String
	    "oe_out";
	false ->  % No Typedef, Dynamic, No String
	    "&oe_out"
    end.



array_decode_dimension_loop(G, N, Fd, [Dim], Dimstr, Type, TDFlag) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	       "&oe_env->_iin, &oe_array_size)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    %% This is disabled due to a bug in erl_interface :
    %% tuples inside tuples hae no correct data about the size 
    %% of the tuple........( allways = 0 )
    %%ic_codegen:emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    %%ic_codegen:emit(Fd, "    return -1;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),


    ArrAccess = 
	case ic_cbe:check_dynamic_size(G, N, Type) of
	    true ->
		ref_array_dynamic_dec(G, N, Type, TDFlag) ++ 
		    Dimstr ++ "[" ++ Tname ++ "]";
	    false ->
		ref_array_static_dec(TDFlag, ictype:isBasicType(G,N,Type)) ++
		    Dimstr ++ "[" ++ Tname ++ "]"
	end,
       
    ic_cbe:gen_decoding_fun(G, N, Fd, Type,
			    ArrAccess,
			    "", "oe_env->_inbuf", 0,
			    "oe_outindex", generator),

%    ic_codegen:emit(Fd, "\n  *oe_outindex += sizeof(~s);\n",[ic_cbe:gen_cc_type(G, N, Type)]), 

    ic_codegen:emit(Fd, "  }\n\n");
array_decode_dimension_loop(G, N, Fd, [Dim | Ds], Dimstr, Type, TDFlag) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	       "&oe_env->_iin, &oe_array_size)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    %% This is disabled due to a bug in erl_interface :
    %% tuples inside tuples hae no correct data about the size 
    %% of the tuple........( allways = 0 )
    %%ic_codegen:emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    %%ic_codegen:emit(Fd, "    return -1;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),
    array_decode_dimension_loop(G, N, Fd, Ds, "[" ++ Tname ++ "]" , Type, TDFlag),

    ic_codegen:emit(Fd, "  }\n\n").

dim_multiplication([D]) ->
    D;
dim_multiplication([D |Ds]) ->
    D ++ "*" ++ dim_multiplication(Ds).

emit_encode(array, G, N, Fd, {Name, Dim}, Type) ->
    array_encode_dimension_loop(G, N, Fd, Dim, {"",""}, Type, array);
emit_encode(array_no_typedef, G, N, Fd, {Name, Dim}, Type) ->
    array_encode_dimension_loop(G, N, Fd, Dim, {"",""}, Type, array_no_typedef);
emit_encode(sequence_head, G, N, Fd, StructName, ElType) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n\n",[Tname]),

    ic_codegen:emit(Fd, "  if (oe_rec->_length != 0) {\n\n"),

    ic_codegen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_list_header(oe_env, oe_rec->_length)) < 0)\n",
	 []),
    ic_codegen:emit(Fd, "      return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "    for(~s = 0; ~s < oe_rec->_length; ~s++) {\n\n",
	 [Tname, Tname, Tname]),
    case ElType of 
	{_,_} -> %% ElType = elementary type or pointer type
	    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf");

	{scoped_id,local,_,["term","erlang"]} ->
	    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf");

	{scoped_id,_,_,_} ->
	    case ic_symtab:get_full_scoped_name(G, N, ElType) of
		{_, typedef, TDef, _} ->
		    case TDef of
			{tk_struct,_,_,_} ->
			    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "&oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf");
			{tk_sequence,_,_} ->
			    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "&oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf");
			{tk_union,_,_,_,_,_} ->
			    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "&oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf");
			_ ->
			    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf") 
		    end;
		_ ->
		    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "&oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf")
	    end;
    
	_ ->     %% ElType = structure 
	    ic_cbe:gen_encoding_fun(G, N, Fd, ElType, "&oe_rec->_buffer[" ++ Tname ++ "]", "oe_env->_outbuf")
    end,
    ic_codegen:emit(Fd, "    }\n\n"),
    ic_codegen:emit(Fd, "  }\n\n"),
    ic_codegen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_empty_list(oe_env)) < 0)\n",
	 []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n");
emit_encode(struct, G, N, Fd, StructName, ElTypes) ->
    Length = length(ElTypes) + 1,
    ic_codegen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_tuple_header(oe_env, ~p)) < 0)\n", [Length]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),
    ic_codegen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, ~p)) < 0)\n", [StructName]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),
    lists:foreach(fun({ET, EN}) -> 
			  case ET of
			      {sequence, _, _} ->
				  %% Sequence = struct
				  ic_cbe:gen_encoding_fun(G, N, Fd, StructName ++ "_" ++ EN, "&oe_rec->" ++ EN,
							  "oe_env->_outbuf");
			      {_,{array, _, Dims}} ->
				  ic_cbe:gen_encoding_fun(G, N, Fd, StructName ++ "_" ++ EN, "oe_rec->" ++ EN,
							  "oe_env->_outbuf");

			      {union,_,_,_,_} ->
				  ic_cbe:gen_encoding_fun(G, N, Fd, 
							  StructName ++ "_" ++ ic_forms:get_id2(ET), 
							  "&oe_rec->" ++ EN,
							  "oe_env->_outbuf");

			      {scoped_id,_,_,_} ->
				  case ictype:member2type(G,StructName,EN) of
				      struct ->
					  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "&oe_rec->" ++ EN, "oe_env->_outbuf");
				      sequence ->
					  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "&oe_rec->" ++ EN, "oe_env->_outbuf");
				      union ->
					  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "&oe_rec->" ++ EN, "oe_env->_outbuf");
				      array ->
					  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "oe_rec->" ++ EN, "oe_env->_outbuf");
				      _ ->
					  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "oe_rec->" ++ EN, "oe_env->_outbuf")
				  end;
			      _ ->
				  ic_cbe:gen_encoding_fun(G, N, Fd, ET, "oe_rec->" ++ EN, "oe_env->_outbuf")
			  end
	    end,
	    ElTypes).



ref_if_not_string({string, _}) ->
    "";
ref_if_not_string(X) ->
    "&".


ref_array_static_enc(array, true) -> % Typedef, Static, Basic Type
    "oe_rec";
ref_array_static_enc(array, false) -> % Typedef, Static, Constr Type
    "&(oe_rec)"; 
ref_array_static_enc(array_no_typedef, true) -> % No Typedef, Static, Basic Type
    "oe_rec";
ref_array_static_enc(array_no_typedef, false) -> % No Typedef, Static, Constr Type
    "&oe_rec".


ref_array_dynamic_enc(G, N, T, array) -> 
    case ictype:isString(G, N, T) of
	true ->    % Typedef, Dynamic, String
	    "oe_rec";
	false ->   % Typedef, Dynamic, No String
	    "&(oe_rec)"
    end;
ref_array_dynamic_enc(G, N, T, array_no_typedef) -> 
    case ictype:isString(G, N, T) of
	true ->    % No Typedef, Dynamic, String
	    "oe_rec";
	false ->   % No Typedef, Dynamic, No String
	    "&oe_rec"
    end.



array_encode_dimension_loop(G, N, Fd, [Dim], {Str1,Str2}, Type, TDFlag) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_tuple_header(oe_env, ~s)) < 0)\n",
	       [Dim]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),
    
    ArrAccess = 
	case ic_cbe:check_dynamic_size(G, N, Type) of
	    true ->
		ref_array_dynamic_enc(G, N, Type, TDFlag) ++
		    Str1 ++ "[" ++ Tname ++ "]";
	    false ->
		ref_array_static_enc(TDFlag, ictype:isBasicType(G,N,Type)) ++
		    Str1 ++ "[" ++ Tname ++ "]"
	end,

    ic_cbe:gen_encoding_fun(G, N, Fd, Type, ArrAccess, "oe_env->_outbuf"),
    ic_codegen:emit(Fd, "  }\n\n");
array_encode_dimension_loop(G, N, Fd, [Dim | Ds],{Str1,Str2}, Type, TDFlag) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_tuple_header(oe_env, ~s)) < 0)\n",
	       [Dim]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),
    array_encode_dimension_loop(G, 
				N, 
				Fd, 
				Ds,
				{Str1 ++ "[" ++ Tname ++ "]", Str2},
				Type, 
				TDFlag),
    ic_codegen:emit(Fd, "  }\n\n").





emit_sizecount(array, G, N, Fd, {Name, Dim}, Type) ->
    ic_codegen:emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + ", dim_multiplication(Dim),
			      " * sizeof(", ic_cbe:gen_cc_type(G, N, Type),")"]),
    ic_codegen:emit(Fd, "    *oe_size = ~s;\n\n",[ic_util:mk_align(AlignName)]),
    array_size_dimension_loop(G, N, Fd, Dim, Type),
    ic_codegen:emit(Fd, "  *oe_size = ~s;\n\n", [ic_util:mk_align("*oe_size + oe_malloc_size")]),
    ic_codegen:nl(Fd);
emit_sizecount(sequence_head, G, N, Fd, StructName, ElType) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    Tname1 = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname1]),

    ic_codegen:emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + sizeof(",StructName,")"]),
    ic_codegen:emit(Fd, "    *oe_size = ~s;\n\n",
	 [ic_util:mk_align(AlignName)]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &~s)) < 0)\n",
	       [Tname]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    %%ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
    %%ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    AlignName1 = lists:concat(["sizeof(",ic_cbe:gen_cc_type(G,N, ElType),") * ", Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),

    case ictype:isBasicTypeOrEterm(G, N, ElType) of
	true ->
	    ic_codegen:emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    ic_codegen:emit(Fd, "      return oe_error_code;\n\n"),

	    ic_codegen:emit(Fd, "    oe_malloc_size = ~s;\n\n",[ic_util:mk_align(AlignName1)]);
	false ->
	    ic_codegen:emit(Fd, "    return oe_error_code;\n\n")
    end,
	    
    ic_codegen:emit(Fd, "  } else {\n\n"),

    ic_codegen:emit(Fd, "    oe_malloc_size = ~s;\n\n",
	 [ic_util:mk_align(AlignName1)]),

    ic_codegen:emit(Fd, "    for(~s = 0; ~s < ~s; ~s++) {\n\n",[Tname1, Tname1, Tname, Tname1]),

    ic_cbe:gen_malloc_size_calculation(G, N, Fd, ElType, "oe_env->_inbuf", 0, generator),

    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    if (~s != 0) \n", [Tname]),
    ic_codegen:emit(Fd, "      if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
    ic_codegen:emit(Fd, "        return oe_error_code;\n\n"),
    ic_codegen:emit(Fd, "  }\n\n"),
    ic_codegen:emit(Fd, "  *oe_size = ~s;\n\n", [ic_util:mk_align("*oe_size + oe_malloc_size")]);
emit_sizecount(struct, G, N, Fd, StructName, ElTypes) ->

    Length = length(ElTypes) + 1,
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n\n",[Tname]),

    ic_codegen:emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + sizeof(",StructName,")"]),
    ic_codegen:emit(Fd, "    *oe_size = ~s;\n\n", [ic_util:mk_align(AlignName)]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, "
	       "&~s)) < 0)\n", [Tname]),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  if (~s != ~p)\n",[Tname, Length]),
    ic_codegen:emit(Fd, "    return -1;\n\n"),


    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),
    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n", []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),
    lists:foreach(
      fun({ET, EN}) ->
	      case ic_cbe:check_dynamic_size(G, N, ET) of
		  true ->
		      case ET of
			  {sequence, _, _} ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, StructName ++ "_" ++ EN,
								 "oe_env->_inbuf", 0, generator);
			  {_,{array, _, _}} ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, StructName ++ "_" ++ EN,
								 "oe_env->_inbuf", 0, generator);
			  {union,_,_,_,_} ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, 
								 StructName ++ "_" ++ ic_forms:get_id2(ET),
								 "oe_env->_inbuf", 0, generator);
			  _  ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, ET, "oe_env->_inbuf", 0, generator)
		      end;
		  false ->
		      case ET of
			  {_,{array, _, _}} ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, StructName ++ "_" ++ EN,
								 "oe_env->_inbuf", 0, generator);

			  {union,_,_,_,_} ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, 
								 StructName ++ "_" ++ ic_forms:get_id2(ET),
								 "oe_env->_inbuf", 0, generator);
			  _  ->
			      ic_cbe:gen_malloc_size_calculation(G, N, Fd, ET, "oe_env->_inbuf", 1, generator)
		      end
	      end
      end,
      ElTypes),
    ic_codegen:emit(Fd, "  *oe_size = ~s;\n\n",[ic_util:mk_align("*oe_size + oe_malloc_size")]).


array_size_dimension_loop(G, N, Fd, [Dim], Type) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, "
	       "&oe_type, &oe_array_size)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    ic_codegen:emit(Fd, "    return -1;\n\n"),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	       "oe_size_count_index, 0)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),
    ic_cbe:gen_malloc_size_calculation(G, N, Fd, Type, "oe_env->_inbuf", 0, generator),
    ic_codegen:emit(Fd, "  }\n\n");
array_size_dimension_loop(G, N, Fd, [Dim | Ds], Type) ->
    Tname = ic_cbe:gen_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, "
	       "&oe_type, &oe_array_size)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    ic_codegen:emit(Fd, "    return -1;\n\n"),

    ic_codegen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	       "oe_size_count_index, 0)) < 0)\n",
	       []),
    ic_codegen:emit(Fd, "    return oe_error_code;\n\n"),

    ic_codegen:emit(Fd, "  for(~s = 0; ~s < ~s; ~s++) {\n\n",
	       [Tname, Tname, Dim, Tname]),
    array_size_dimension_loop(G, N, Fd, Ds, Type),
    ic_codegen:emit(Fd, "  }\n\n").





create_c_struct_coding_file(G, N, X, StructName, ElTypes, StructType) ->
    
    {Fd , SName} = open_c_coding_file(G,  StructName),
    HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    
    ic_codegen:emit_stub_head(G, Fd, SName, c),
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    ic_codegen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),


    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Fd = ic_genobj:stubfiled(G), %% Write on stubfile
    %%  HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    %%  HrlFName = filename:basename(ic_genobj:include_file(G)),
    %%  ic_codegen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	       [ic_util:mk_oe_name(G, "sizecalc_"), StructName]),

    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size) {\n\n",
	       [ic_util:mk_oe_name(G, "sizecalc_"), StructName]),

    ic_codegen:emit(Fd, "  int oe_malloc_size = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),
    ic_codegen:emit(Fd, "  int oe_type = 0;\n",[]), 

    {ok, RamFd} = ram_file:open([], [binary, write]),

    emit_sizecount(StructType, G, N, RamFd, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data} = ram_file:get_file(RamFd),
    ic_codegen:emit(Fd, Data),
    ram_file:close(RamFd),

    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),
    
    put(op_variable_count, 0),
    put(tmp_declarations, []),


    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s*);\n",
	       [ic_util:mk_oe_name(G, "encode_"), StructName, StructName]),

    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s* oe_rec) {\n\n",
	       [ic_util:mk_oe_name(G, "encode_"), StructName, StructName]),

    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd1} = ram_file:open([], [binary, write]),

    emit_encode(StructType, G, N, RamFd1, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data1} = ram_file:get_file(RamFd1),
    ic_codegen:emit(Fd, Data1),
    ram_file:close(RamFd1),

    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    ic_codegen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s *);\n",
	       [ic_util:mk_oe_name(G, "decode_"), StructName, StructName]),

    ic_codegen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_outindex, "
	       "~s *oe_out) {\n\n",
	       [ic_util:mk_oe_name(G, "decode_"), StructName, StructName]),

    ic_codegen:emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd2} = ram_file:open([], [binary, write]),

    emit_decode(StructType, G, N, RamFd2, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data2} = ram_file:get_file(RamFd2),
    ic_codegen:emit(Fd, Data2),
    ram_file:close(RamFd2),

    ic_codegen:emit(Fd, "  *oe_outindex = ~s;\n",[ic_util:mk_align("*oe_outindex")]),
    ic_codegen:emit(Fd, "  return 0;\n\n",[]),
    ic_codegen:emit(Fd, "}\n\n",[]),
    file:close(Fd).


%%------------------------------------------------------------
%%
%% emit primitive for unions.
%%
%%------------------------------------------------------------
emit_union(G, N, X, erlang) ->
    case ic_genobj:is_hrlfile_open(G) of
        true ->
            ic_codegen:record(G, X, ic_util:to_undersc([ic_forms:get_id2(X) | N]),nil,nil),
	    mkFileRecObj(G,N,X,erlang);
	false -> ok
    end;
emit_union(G, N, X, c) -> %% Not supported in c backend
    true.


%%------------------------------------------------------------
%%
%% emit erlang modules for objects with record definitions..
%%
%%------------------------------------------------------------
mkFileRecObj(G,N,X,erlang) ->
    SName = 
	ic_util:to_undersc([ic_forms:get_id2(X) | N]),
    FName =  
	ic_file:join(ic_options:get_opt(G, stubdir),ic_file:add_dot_erl(SName)),
    
    case file:rawopen(FName, {binary, write}) of
	{ok, Fd} ->
	    HrlFName = filename:basename(ic_genobj:include_file(G)),

	    ic_codegen:emit_stub_head(G, Fd, SName, erlang),
	    ic_codegen:emit(Fd, "-include(~p).\n\n",[HrlFName]),
	    emit_exports(G,Fd),
	    emit_rec_methods(G,N,X,SName,Fd),  
	    ic_codegen:nl(Fd),
	    ic_codegen:nl(Fd),
	    file:close(Fd);
	Other -> 
	    exit(Other)
    end;
mkFileRecObj(_,_,_,_) ->
    true.



%%------------------------------------------------------------
%%
%% emit exports for erlang modules which represent records.
%%
%%------------------------------------------------------------
emit_exports(G,Fd) ->
    ic_codegen:emit(Fd, "-export([tc/0,id/0,name/0]).\n\n\n\n",[]).


%%------------------------------------------------------------
%%
%% emit erlang module functions which represent records, yields
%% record information such as type code, identity and name.
%%
%%------------------------------------------------------------
emit_rec_methods(G,N,X,Name,Fd) ->

    IR_ID = ictk:get_IR_ID(G, N, X),
    TK = ic_forms:get_tk(X),

    case TK of
	undefined ->
	    STK = ic_forms:search_tk(G,ictk:get_IR_ID(G, N, X)),
	    ic_codegen:emit(Fd, "%% returns type code\n",[]),
	    ic_codegen:emit(Fd, "tc() -> ~p.\n\n",[STK]),
	    ic_codegen:emit(Fd, "%% returns id\n",[]),
	    ic_codegen:emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
	    ic_codegen:emit(Fd, "%% returns name\n",[]),
	    ic_codegen:emit(Fd, "name() -> ~p.\n\n",[Name]);
	_ ->
	    ic_codegen:emit(Fd, "%% returns type code\n",[]),
	    ic_codegen:emit(Fd, "tc() -> ~p.\n\n",[TK]),
	    ic_codegen:emit(Fd, "%% returns id\n",[]),
	    ic_codegen:emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
	    ic_codegen:emit(Fd, "%% returns name\n",[]),
	    ic_codegen:emit(Fd, "name() -> ~p.\n\n",[Name])
    end.
