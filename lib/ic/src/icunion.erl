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

-module(icunion).


-include("icforms.hrl").
-include("ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([union_gen/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

union_gen(G, N, X, c) when record(X, union) ->
    emit_c_union(G, N, X);
union_gen(G, N, X, L) ->
    ok.


%% Emits the union
emit_c_union(G, N, X) ->
    %%io:format("Rec = ~p\n",[X]),
    case icgen:is_hrlfile_open(G) of
	true ->

	    %% Sort Union Default = put it last in case list
	    NewX = #union{ id = X#union.id,
			   type = X#union.type,
			   body = mvDefaultToTail(X#union.body),
			   tk = X#union.tk },

	    UnionScope = [icgen:get_id2(NewX) | N],

	    case ic_pragma:is_local(G,UnionScope) of
		
		true ->

		    HFd = icgen:hrlfiled(G),
		    emit_c_union_values(G, N, NewX, HFd),
		    UnionName = icgen:to_undersc(UnionScope),

		    icgen:emit(HFd, "\n#ifndef __~s__\n",[ictype:to_uppercase(UnionName)]),	
		    icgen:emit(HFd, "#define __~s__\n",[ictype:to_uppercase(UnionName)]),
		    icgen:mcomment_light(HFd,
					 [io_lib:format("Union definition: ~s",
							[UnionName])],
					 c),
		    icgen:emit(HFd, "typedef struct {\n"),
		    icgen:emit(HFd, "  ~s _d;\n", [get_c_union_discriminator(G, N, NewX)]),
		    icgen:emit(HFd, "  union {\n"),
		    emit_c_union_values_decl(G, N, NewX, HFd),
		    icgen:emit(HFd, "  } _u;\n"),
		    icgen:emit(HFd, "} ~s;\n\n", [UnionName]),
		    
		    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
			       [icgen:mk_oe_name(G, "sizecalc_"), UnionName]),
		    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s*);\n",
			       [icgen:mk_oe_name(G, "encode_"), UnionName, UnionName]),
		    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s*);\n",
			       [icgen:mk_oe_name(G, "decode_"), UnionName, UnionName]),
		    icgen:emit(HFd, "\n#endif\n\n"),
		    create_c_union_file(G, N, NewX, UnionName);

		false -> %% Do not generate included types att all.
		    ok
	    end;
	false ->
	    ok
    end.



%% Loops over union members and creates members typedefs
emit_c_union_values(G, N, X, Fd) ->
    emit_c_union_values_loop(G, N, X, Fd, X#union.body).

emit_c_union_values_loop(G, N, X, Fd, [CU]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array,AID,SZ} -> % Check for arrays 	
		    mk_array_file(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    ok
	    end;
	_ ->
	    error
    end;
emit_c_union_values_loop(G, N, X, Fd, [CU |CUs]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array,AID,SZ} -> % Check for arrays	
		    mk_array_file(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    emit_c_union_values_loop(G, N, X, Fd, CUs)
	    end;
	_ ->
	    error
    end.


%% Loops over union members and declares members inside union structure
emit_c_union_values_decl(G, N, X, Fd) ->
    emit_c_union_values_decl_loop(G, N, X, Fd, X#union.body).

emit_c_union_values_decl_loop(G, N, X, Fd, [CU]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array,AID,SZ} -> % Check for arrays 	
		    mk_array_decl(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    mk_union_member_decl(G,N,X,Id,Type,Fd),
		    ok
	    end;
	_ ->
	    error
    end;
emit_c_union_values_decl_loop(G, N, X, Fd, [CU |CUs]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array,AID,SZ} -> % Check for arrays	
		    mk_array_decl(G,N,X,Id,Type,Fd),
		    emit_c_union_values_decl_loop(G, N, X, Fd, CUs);
		_ ->              % Elementary types or seq/struct
		    mk_union_member_decl(G,N,X,Id,Type,Fd),
		    emit_c_union_values_decl_loop(G, N, X, Fd, CUs)
	    end;
	_ ->
	    error
    end.


%% Makes the declaration for the array in union
mk_array_decl(G,N,X,Id,Type,Fd) ->
    icgen:emit(Fd, "    ~s ~s;\n", 
	       [getCaseTypeStr(G,N,X,Id,Type), 
		mk_array_name(Id)]).

mk_array_name({array,Id,D}) ->
    icgen:get_id2(Id) ++ mk_array_dim(D).

mk_array_dim([]) ->
    "";
mk_array_dim([{_,_,Dim}|Dims]) ->
    "[" ++ Dim ++ "]" ++ mk_array_dim(Dims).


%% Creates the array file 
mk_array_file(G,N,X,{array,AID,SZ},Type,HFd) ->
    ArrayName = icgen:to_undersc([icgen:get_id2(AID),icgen:get_id2(X) | N]),
    ArrayDim =  extract_array_dim(SZ),
    icgen:emit(HFd, "\n#ifndef __~s__\n",[ictype:to_uppercase(ArrayName)]),	
    icgen:emit(HFd, "#define __~s__\n\n",[ictype:to_uppercase(ArrayName)]),
    icstruct:create_c_array_coding_file(G,
					N,
					{ArrayName,ArrayDim},
					Type,
					no_typedef),
    icgen:emit(HFd, "\n#endif\n\n").

extract_array_dim([{_,_,Dim}]) ->
    [Dim];
extract_array_dim([{_,_,Dim}|Dims]) ->
    [Dim | extract_array_dim(Dims)].


%% Makes the declaration for the member in union
mk_union_member_decl(G,N,X,Id,Type,Fd) ->
    icgen:emit(Fd, "    ~s ~s;\n", 
	       [getCaseTypeStr(G,N,X,Id,Type), 
		icgen:get_id2(Id)]).

    


%% File utilities
create_c_union_file(G, N, X, UnionName) ->

    {Fd , SName} = open_c_coding_file(G, UnionName),
    HFd = icgen:hrlfiled(G), %% Write on stubfile header
    HrlFName = filename:basename(icgen:include_file(G)),
    icgen:emit_stub_head(G, Fd, SName, c),
    icgen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),

    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Fd = icgen:stubfiled(G), %% Write on stubfile
    %%  HFd = icgen:hrlfiled(G), %% Write on stubfile header
    %%  HrlFName = filename:basename(icgen:include_file(G)),
    %%  icgen:emit(Fd, "#include \"~s\"\n\n",[HrlFName]),
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
    put(op_variable_count, 0),
    put(tmp_declarations, []),

    %% Write generated code on file
    emit_union_sizecount(G, N, X, Fd, UnionName),
    emit_union_encode(G, N, X, Fd, UnionName),
    emit_union_decode(G, N, X, Fd, UnionName),
    file:close(Fd).
 
open_c_coding_file(G, Name) ->
    SName = string:concat(icgen:mk_oe_name(G, "code_"), Name),
    FName =  
        icgen:join(icgen:get_opt(G, stubdir),icgen:add_dot_c(SName)),
    case file:rawopen(FName, {binary, write}) of
        {ok, Fd} ->
            {Fd, SName};
        Other ->
            exit(Other)
    end.




get_c_union_discriminator(G, N, X) ->
    case getDiscrStr(G, N, X#union.type) of
	error ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, X#union.type, N});
	DiscrStr ->
	    case icgen:get_basetype(G, DiscrStr) of
		{short, _} ->
		    "CORBA_short";
		{unsigned,{short, _}} ->
		    "CORBA_unsigned_short";
		{long, _} ->
		    "CORBA_long";
		{unsigned,{long, _}} ->
		    "CORBA_unsigned_long";
		{boolean,_} ->
		    "CORBA_boolean";
		{char,_} ->
		    "CORBA_char";
		{enum, EnumType} ->
		    EnumType;
		_ ->
		    DiscrStr
	    end
    end.

getDiscrStr(G, N, S) when element(1, S) == scoped_id -> 
    case icgen:get_full_scoped_name(G, N, S) of
	{FSN, _, tk_short, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, tk_ushort, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, tk_long, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, tk_ulong, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, tk_boolean, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, tk_char, _} ->
	    icgen:to_undersc(FSN);
	{FSN, _, {tk_enum,_,_,_}, _} ->
	    icgen:to_undersc(FSN);
	_ ->
	    error
    end;
getDiscrStr(G, N, X) ->
    case X of
	{short,_} ->
	    "CORBA_short";
	{unsigned,{short,_}} ->
	    "CORBA_unsigned_short";
	{long, _} ->
	    "CORBA_long";
	{unsigned,{long,_}} ->
	    "CORBA_unsigned_long";
	{boolean,_} ->
	    "CORBA_boolean";
	{char,_} ->
	    "CORBA_char";
	{enum,TID,_,_} ->
	    icgen:to_undersc([icgen:get_id2(TID) | N]);
	_ ->
	    error
    end.




getCaseTypeStr(G, N, X, I, T) when element(1, T) == scoped_id ->
    case catch icgen:get_full_scoped_name(G, N, T) of
	{FSN, _, _, _} ->
	    BT = icgen:get_basetype(G, icgen:to_undersc(FSN)),
	    case isList(BT) of
		true ->
		    BT;
		false ->
		    case BT of
			{short,_} ->
			    "CORBA_short";
			{unsigned,{short,_}} ->
			    "CORBA_unsigned_short";
			{long, _} ->
			    "CORBA_long";
			{unsigned,{long,_}} ->
			    "CORBA_unsigned_long";
			{float,_} ->
			    "CORBA_float";
			{double,_} ->
			    "CORBA_double";
			{boolean,_} ->
			    "CORBA_boolean";
			{char,_} ->
			    "CORBA_char";
			{octet,_} ->
			    "CORBA_octet";
			{string,_} ->
			    "CORBA_char*";
			{sequence,_,_} ->
			    icgen:to_undersc([icgen:get_id2(I), icgen:get_id2(X) | N]);
			{struct,SID,_,_} ->
			    icgen:to_undersc([icgen:get_id2(SID), icgen:get_id2(X) | N]);
			{enum,EID} ->
			    EID;
			{any, _} -> %% Fix for any type
			    "CORBA_long";
			_ ->
			    %%io:format("BT = ~p~n",[BT]),
			    error
		    end
	    end
    end;
getCaseTypeStr(G, N, X, I, T) ->
    case T of
	{short,_} ->
	    "CORBA_short";
	{unsigned,{short,_}} ->
	    "CORBA_unsigned_short";
	{long, _} ->
	    "CORBA_long";
	{unsigned,{long,_}} ->
	    "CORBA_unsigned_long";
	{float,_} ->
	    "CORBA_float";
	{double,_} ->
	    "CORBA_double";
	{boolean,_} ->
	    "CORBA_boolean";
	{char,_} ->
	    "CORBA_char";
	{octet,_} ->
	    "CORBA_octet";
	{string,_} ->
	    "CORBA_char*";
	{sequence,_,_} ->
	    icgen:to_undersc([icgen:get_id2(I), icgen:get_id2(X) | N]);
	{struct,SID,_,_} ->
	    icgen:to_undersc([icgen:get_id2(SID), icgen:get_id2(X) | N]);
	{union,UID,_,_,_} ->
	    icgen:to_undersc([icgen:get_id2(UID), icgen:get_id2(X) | N]);
	{any, _} -> %% Fix for any type
	    "CORBA_long";
	_ ->
	    error
    end.

isList(L) when list(L) ->
    true;
isList(_) ->
    false.
	    
%%
%%  Sizecount facilities
%%
emit_union_sizecount(G, N, X, Fd, UnionName) ->
    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size) {\n\n",
	       [icgen:mk_oe_name(G, "sizecalc_"), UnionName]),
   
    icgen:emit(Fd, "  int oe_malloc_size = 0;\n"),
    icgen:emit(Fd, "  int oe_error_code = 0;\n"),
    icgen:emit(Fd, "  int oe_type = 0;\n"),
    icgen:emit(Fd, "  int oe_tmp = 0;\n"),
    emit_union_discr_var_decl(G, N, X, Fd),
    
    icgen:nl(Fd),
    icgen:emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + sizeof(",UnionName,")"]),
    icgen:emit(Fd, "    *oe_size = ~s;\n\n", [icgen:mk_align(AlignName)]),

    icgen:emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0)\n"),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),

    %%icgen:emit(Fd, "  if (oe_tmp != 3)\n"),
    %%icgen:emit(Fd, "    return -1;\n\n"),

    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n", []),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),

    emit_c_union_discr_sizecount(G, N, X, Fd),
    icgen:emit(Fd, "  /* Calculate union size */\n"),
    icgen:emit(Fd, "  switch(oe_discr) {\n"),
 
    emit_c_union_loop(G, N, X, Fd, X#union.body, sizecalc),
    icgen:emit(Fd, "  }\n\n"),

    icgen:emit(Fd, "  *oe_size = ~s;\n",[icgen:mk_align("*oe_size+oe_malloc_size")]),
    icgen:emit(Fd, "  return 0;\n"),
    icgen:emit(Fd, "}\n\n\n").


emit_union_discr_var_decl(G, N, X, Fd) ->
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    icgen:emit(Fd, "  long oe_discr = 0;\n");
	"CORBA_unsigned_short" ->
	    icgen:emit(Fd, "  unsigned long oe_discr = 0;\n");
	"CORBA_long" ->
	    icgen:emit(Fd, "  long oe_discr = 0;\n");
	"CORBA_unsigned_long" ->
	    icgen:emit(Fd, "  unsigned long oe_discr = 0;\n");
	"CORBA_boolean" ->
	    icgen:emit(Fd, "  int oe_discr = 0;\n"),
	    icgen:emit(Fd, "  char oe_bool[256];\n");
	"CORBA_char" ->
	    icgen:emit(Fd, "  char oe_discr = 0;\n");
	T ->
	    icgen:emit(Fd, "  int oe_dummy = 0;\n"),
	    icgen:emit(Fd, "  ~s oe_discr = 0;\n",[UD])
    end.


emit_c_union_discr_sizecount(G, N, X, Fd) ->
    icgen:emit(Fd, "  /* Calculate discriminator size */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_unsigned_short" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_unsigned_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_boolean" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, oe_bool)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n"),
	    icgen:emit(Fd, "  if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    icgen:emit(Fd, "    oe_discr = 0;\n"), 
	    icgen:emit(Fd, "  }\n"),
	    icgen:emit(Fd, "  else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    icgen:emit(Fd, "    oe_discr = 1;\n"), 
	    icgen:emit(Fd, "  }\n"),
	    icgen:emit(Fd, "  else\n"),
	    icgen:emit(Fd, "    return -1;\n\n");

	"CORBA_char" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	T ->
	    icgen:emit(Fd, "  oe_tmp = *oe_size_count_index;\n"),
	    icgen:emit(Fd, "  if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
		       [T]),
	    icgen:emit(Fd, "    return oe_error_code;\n\n"),

	    icgen:emit(Fd, "  *oe_size_count_index = oe_tmp;\n"),
	    icgen:emit(Fd, "  oe_tmp = oe_env->_iin;\n"),
	    icgen:emit(Fd, "  oe_env->_iin = *oe_size_count_index;\n"),
	    icgen:emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, NULL, &oe_dummy, &oe_discr)) < 0)\n",
                       [T]),
	    icgen:emit(Fd, "    return oe_error_code;\n\n"),

	    icgen:emit(Fd, "  *oe_size_count_index = oe_env->_iin;\n"),
	    icgen:emit(Fd, "  oe_env->_iin = oe_tmp;\n\n")
    end.
				 


emit_c_union_loop(G, N, X, Fd, CaseList, Case) ->
    emit_c_union_loop(G, N, X, Fd, CaseList, false, Case).

emit_c_union_loop(G, N, X, Fd, [], GotDefaultCase, Case) ->
    case GotDefaultCase of
	false ->
	    emit_c_union_valueless_discriminator(G, N, X, Fd, Case);
	_ ->
	    ok
    end;
emit_c_union_loop(G, N, X, Fd, [CU|CUs], GotDefaultCase, Case) ->
    case CU of
	{case_dcl,CaseList,I,T} ->
	    GotDefaultCase = emit_c_union_case(G, N, X, Fd, I, T, CaseList, Case),
	    emit_c_union_loop(G, N, X, Fd, CUs, GotDefaultCase, Case);
	_ ->
	    error
    end.

emit_c_union_valueless_discriminator(G, N, X, Fd, Case) ->  
    icgen:emit(Fd, "  default:\n"),
    case Case of
	sizecalc ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      char oe_undefined[15];\n\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, "
		       "oe_size_count_index, oe_undefined)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    }\n");
	encode ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"undefined\")) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n"),
	    icgen:emit(Fd, "    break;\n");
	decode ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      char oe_undefined[15];\n\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, "
		       "oe_undefined)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "      if (strcmp(oe_undefined, \"undefined\") != 0)\n"),
	    icgen:emit(Fd, "        return -1;\n"),
	    icgen:emit(Fd, "    }\n")
    end.


emit_c_union_case(G, N, X, Fd, I, T, [{default,_}], Case) -> 
    icgen:emit(Fd, "  default:\n"),
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end, 
    true;
emit_c_union_case(G, N, X, Fd, I, T, [{Bool,_}], Case) -> %% Boolean discriminator
    case Bool of
	'TRUE' ->
	    icgen:emit(Fd, "  case 1:\n");
	'FALSE' ->
	    icgen:emit(Fd, "  case 0:\n")
    end,
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    icgen:emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{Bool,_}|Rest], Case) -> %% Boolean discriminator
    case Bool of
	'TRUE' ->
	    icgen:emit(Fd, "  case 1:\n");
	'FALSE' ->
	    icgen:emit(Fd, "  case 0:\n")
    end,
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{_,_,NrStr}], Case) -> %% Integer type discriminator
    case get_c_union_discriminator(G, N, X) of
	"CORBA_char" ->
	    icgen:emit(Fd, "  case \'~s\':\n",[NrStr]);
	_ ->
	    icgen:emit(Fd, "  case ~s:\n",[NrStr])
    end,
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    icgen:emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{_,_,NrStr}|Rest], Case) -> %% Integer type discriminator
    icgen:emit(Fd, "  case ~s:\n",[NrStr]),
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{scoped_id,_,_,[EID]}], Case) -> %% Enumerant type discriminator
    SID = ic_util:to_undersc([EID|get_c_union_discriminator_scope(G, N, X)]),
    %%io:format("SID = ~p~n",[SID]),
    icgen:emit(Fd, "  case ~s:\n",[SID]),
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    icgen:emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{scoped_id,_,_,[EID]}|Rest], Case) -> %% Enumerant type discriminator
    SID = ic_util:to_undersc([EID|get_c_union_discriminator_scope(G, N, X)]),
    %%io:format("SID = ~p~n",[SID]),
    icgen:emit(Fd, "  case ~s:\n",[SID]),
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false.


%%
%% Returns the enumerant discriminator scope
%%
get_c_union_discriminator_scope(G, N, X) ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, X#union.type),
    BT = case ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)) of
	     {enum,ST} ->
		 ST;
	     Other ->
		 Other
	 end,
    tl(lists:reverse(string:tokens(BT,"_"))). %% Uggly work arround





getCaseTypeSizecalc(G, N, X, Fd, I, T) when element(1, T) == scoped_id ->
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	ulong -> 
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	short ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n"); 	
	long ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	float ->
	    icgen:emit(Fd, "   if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");     
	double ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");       
	boolean ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	char ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	octet ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	string ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    oe_malloc_size = ~s;\n",[icgen:mk_align("oe_malloc_size+oe_tmp+1")]);
	any -> %% Fix for any type
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n");

	_ ->
	    case getCaseTypeStr(G, N, X, I, T) of
		"erlang_pid" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_pid(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n",
			       []),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"erlang_port" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_port(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n",
			       []),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"erlang_ref" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n",
			       []),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"erlang_term" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_term(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n",
			       []),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		
		Other ->

		    icgen:emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			       [Other]),
		    icgen:emit(Fd, "      return oe_error_code;\n")
	    end
    end;
getCaseTypeSizecalc(G, N, X, Fd, I, T) ->
    case I of 
	{array,_,_}  ->
	    ArrayName = icgen:to_undersc([icgen:get_id2(I),icgen:get_id2(X) | N]),
	    icgen:emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
		       [ArrayName]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	_ ->
	    case T of
		{short,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{unsigned,{short,_}} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{long, _} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{unsigned,{long,_}} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{float,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{double,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{boolean,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{char,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{octet,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{string,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n\n"),
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n\n"),
		    icgen:emit(Fd, "    oe_malloc_size = ~s;\n",[icgen:mk_align("oe_malloc_size+oe_tmp+1")]);
		{sequence,_,_} ->
		    SeqName = icgen:to_undersc([icgen:get_id2(I), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			       [SeqName]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{struct,SID,_,_} ->
		    StructName = icgen:to_undersc([icgen:get_id2(SID), icgen:get_id2(X) | N]),
			    
		    icgen:emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			       [StructName]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{union,UID,_,_,_} ->
		    UnionName = icgen:to_undersc([icgen:get_id2(UID), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			       [UnionName]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{any, _} -> %% Fix for any type
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n"),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		_ ->
		    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.





%%
%% Encode facilities
%%
emit_union_encode(G, N, X, Fd, UnionName) ->
    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s* oe_rec) {\n\n",
	       [icgen:mk_oe_name(G, "encode_"), UnionName, UnionName]),
    
    icgen:emit(Fd, "  int oe_error_code = 0;\n\n"),

    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_tuple_header(oe_env, 3)) < 0)\n"),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    
    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, \"~s\")) < 0)\n", 
	       [UnionName]),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    
    emit_c_union_discr_encode(G, N, X, Fd),  
    icgen:emit(Fd, "  /* Encode union */\n"),
    icgen:emit(Fd, "  switch(oe_rec->_d) {\n"),
    emit_c_union_loop(G, N, X, Fd, X#union.body, encode),
    icgen:emit(Fd, "  }\n\n"),
    icgen:emit(Fd, "  return 0;\n"),
    icgen:emit(Fd, "}\n\n\n").


emit_c_union_discr_encode(G, N, X, Fd) ->
    icgen:emit(Fd, "  /* Encode descriminator */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_unsigned_short" ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_unsigned_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_boolean" ->
	    icgen:emit(Fd, "  switch(oe_rec->_d) {\n"),
	    icgen:emit(Fd, "  case 0:\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n"),
	    icgen:emit(Fd, "    break;\n"),
	    icgen:emit(Fd, "  case 1:\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n"),
	    icgen:emit(Fd, "    break;\n"),
	    icgen:emit(Fd, "  default:\n"),
	    icgen:emit(Fd, "    return -1;\n"),
	    icgen:emit(Fd, "  }\n\n");
	"CORBA_char" ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	T ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_d)) < 0)\n",
		       [T]),
	    icgen:emit(Fd, "    return oe_error_code;\n\n")
    end.


getCaseTypeEncode(G, N, X, Fd, I, T) when element(1, T) == scoped_id -> 
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	ulong -> 
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	short ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n"); 	
	long ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	float ->
	    icgen:emit(Fd, "   if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");     
	double ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");       
	boolean ->
	    icgen:emit(Fd, "    switch(oe_rec->_u.~s) {\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "    case 0:\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n"),
	    icgen:emit(Fd, "      break;\n"),
	    icgen:emit(Fd, "    case 1:\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n"),
	    icgen:emit(Fd, "      break;\n"),
	    icgen:emit(Fd, "    default:\n"),
	    icgen:emit(Fd, "      return -1;\n"),
	    icgen:emit(Fd, "    }\n");
	char ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	octet ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	string ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_string(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	struct ->
	    case ic_cbe:gen_cc_type(G, N, T, evaluate_not) of
		"erlang_pid" ->
		    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, &oe_rec->_u.~s)) < 0)\n",
			 [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n");
		"erlang_port" ->
		    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_port(oe_env, &oe_rec->_u.~s)) < 0)\n",
			 [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n");
		"erlang_ref" ->
		    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, &oe_rec->_u.~s)) < 0)\n",
			 [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n");
		"ETERM*" ->
		    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_term(oe_env, &oe_rec->_u.~s)) < 0)\n",
			 [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n");
		_ ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
			       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n")
	    end;
	sequence ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	array ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	union ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	enum ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	any -> %% Fix for any type
	    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	_ ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end;
getCaseTypeEncode(G, N, X, Fd, I, T) ->
    case I of
	{array,AID,_} ->
	    ArrayName = icgen:to_undersc([icgen:get_id2(AID),icgen:get_id2(X) | N]),
	    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0)\n",
		       [ArrayName,icgen:get_id2(AID)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	_ ->
	    case T of
		{short,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{unsigned,{short,_}} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{long, _} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{unsigned,{long,_}} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{float,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{double,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{boolean,_} ->
		    icgen:emit(Fd, "    switch(oe_rec->_u.~s) {\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "    case 0:\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n"),
		    icgen:emit(Fd, "      break;\n"),
		    icgen:emit(Fd, "    case 1:\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n"),
		    icgen:emit(Fd, "      break;\n"),
		    icgen:emit(Fd, "    default:\n"),
		    icgen:emit(Fd, "      return -1;\n"),
		    icgen:emit(Fd, "    }\n");
		{char,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{octet,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{string,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_ei_encode_string(oe_env, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{sequence,_,_} ->
		    SeqName = icgen:to_undersc([icgen:get_id2(I), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
			       [SeqName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{struct,SID,_,_} ->
		    StructName = icgen:to_undersc([icgen:get_id2(SID), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
			       [StructName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{union,UID,_,_,_} ->
		    UnionName = icgen:to_undersc([icgen:get_id2(UID), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0)\n",
			       [UnionName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		_ ->
		    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.




%%
%% Decode facilities
%%
emit_union_decode(G, N, X, Fd, UnionName) ->
    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_index, ~s* oe_rec) {\n\n",
	       [icgen:mk_oe_name(G, "decode_"), UnionName, UnionName]),
    
    icgen:emit(Fd, "  int oe_error_code = 0;\n"),
    icgen:emit(Fd, "  int oe_tmp = 0;\n"),
    icgen:emit(Fd, "  char oe_union_name[256];\n\n"),

    icgen:emit(Fd, "  if((char*) oe_rec == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_index + sizeof(",UnionName,")"]),
    icgen:emit(Fd, "    *oe_index = ~s;\n\n", [icgen:mk_align(AlignName)]),

    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_tmp)) < 0)\n"),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    
    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_union_name)) < 0)\n"),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    
    emit_c_union_discr_decode(G, N, X, Fd),
    icgen:emit(Fd, "  /* Decode union */\n"),
    icgen:emit(Fd, "  switch(oe_rec->_d) {\n"),
    emit_c_union_loop(G, N, X, Fd, X#union.body, decode),
    icgen:emit(Fd, "  }\n\n"),

    icgen:emit(Fd, "  *oe_index = ~s;\n", [icgen:mk_align("*oe_index")]),
    icgen:emit(Fd, "  return 0;\n"),
    icgen:emit(Fd, "}\n\n\n").


emit_c_union_discr_decode(G, N, X, Fd) ->
    icgen:emit(Fd, "  /* Decode descriminator */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    icgen:emit(Fd, "  {\n"),
	    icgen:emit(Fd, "    long oe_long;\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    oe_rec->_d = (short) oe_long;\n\n"),
	    icgen:emit(Fd, "    if (oe_rec->_d !=  oe_long)\n      return -1;\n"),
	    icgen:emit(Fd, "  }\n\n");
	"CORBA_unsigned_short" ->
	    icgen:emit(Fd, "  {\n"),
	    icgen:emit(Fd, "    unsigned long oe_ulong;\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    oe_rec->_d = (unsigned short) oe_ulong;\n\n"),
	    icgen:emit(Fd, "    if (oe_rec->_d !=  oe_ulong)\n      return -1;\n"),
	    icgen:emit(Fd, "  }\n\n");
	"CORBA_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_unsigned_long" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	"CORBA_boolean" ->
	    icgen:emit(Fd, "  {\n"),
	    icgen:emit(Fd, "    char oe_bool[25];\n\n"),
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0)\n"),
	    icgen:emit(Fd, "      return oe_error_code;\n\n"),
	    icgen:emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    icgen:emit(Fd, "      oe_rec->_d = 0;\n"), 
	    icgen:emit(Fd, "    }\n"),
	    icgen:emit(Fd, "    else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    icgen:emit(Fd, "      oe_rec->_d = 1;\n"), 
	    icgen:emit(Fd, "    }\n"),
	    icgen:emit(Fd, "    else\n"),
	    icgen:emit(Fd, "      return -1;\n"),
	    icgen:emit(Fd, "  }\n\n");
	"CORBA_char" ->
	    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0)\n"),
	    icgen:emit(Fd, "    return oe_error_code;\n\n");
	T ->
	    icgen:emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_d)) < 0)\n",
		       [T]),
	    icgen:emit(Fd, "    return oe_error_code;\n\n")
    end.



getCaseTypeDecode(G, N, X, Fd, I, T) when element(1, T) == scoped_id -> 
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      unsigned long oe_ulong;\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "      oe_rec->_u.~s = (unsigned short) oe_ulong;\n\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "      if (oe_rec->_u.~s !=  oe_ulong)\n        return -1;\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "    }\n");
	ulong -> 
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	short ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      long oe_long;\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "      oe_rec->_u.~s = (short) oe_long;\n\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "      if (oe_rec->_u.~s !=  oe_long)\n        return -1;\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "    }\n");
	long ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	float ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      double oe_double;\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_double)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "      oe_rec->_u.~s = (float) oe_double;\n",[icgen:get_id2(I)]),
	    icgen:emit(Fd, "    }\n");
	double ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");       
	boolean ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      char oe_bool[25];\n\n"),
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0)\n"),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),
	    icgen:emit(Fd, "      if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    icgen:emit(Fd, "        oe_rec->_u.~s = 0;\n",[icgen:get_id2(I)]), 
	    icgen:emit(Fd, "      }\n"),
	    icgen:emit(Fd, "      else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    icgen:emit(Fd, "        oe_rec->_u.~s = 1;\n",[icgen:get_id2(I)]), 
	    icgen:emit(Fd, "      }\n"),
	    icgen:emit(Fd, "      else\n"),
	    icgen:emit(Fd, "        return -1;\n"),
	    icgen:emit(Fd, "    }\n");
	char ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	octet ->
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	string ->
	    icgen:emit(Fd, "    {\n"),
	    icgen:emit(Fd, "      int oe_type = 0;\n"),
	    icgen:emit(Fd, "      int oe_string_ctr = 0;\n\n"),
	    
	    icgen:emit(Fd, "      (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n"),

	    icgen:emit(Fd, "      oe_rec->_u.~s = (void *) (oe_first + *oe_index);\n\n",[icgen:get_id2(I)]),
	    
	    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_string(oe_env->_inbuf, &oe_env->_iin, oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "        return oe_error_code;\n\n"),

	    icgen:emit(Fd, "      *oe_index = ~s;\n",[icgen:mk_align("*oe_index+oe_string_ctr+1")]),
	    icgen:emit(Fd, "    }\n");
	struct ->
	    case ic_cbe:gen_cc_type(G, N, T, evaluate_not) of
		"erlang_pid" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_pid(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"erlang_port" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_port(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"erlang_ref" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");
		"ETERM*" ->
		    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_term(oe_env->_inbuf, &oe_env->_iin, (void **)&oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n\n");

		_ ->
		    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
			       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
		    icgen:emit(Fd, "    return oe_error_code;\n")
	    end;
	sequence ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	array ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	union ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	enum ->
	    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
		       [getCaseTypeStr(G, N, X, I, T),icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	any -> %% Fix for any type
	    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
		       [icgen:get_id2(I)]),
	    icgen:emit(Fd, "    return oe_error_code;\n");
	_ ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end;
getCaseTypeDecode(G, N, X, Fd, I, T) ->
    case I of
	{array,AID,_} ->
	    ArrayName = icgen:to_undersc([icgen:get_id2(AID),icgen:get_id2(X) | N]),
	    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, oe_rec->_u.~s)) < 0)\n",
		       [ArrayName,icgen:get_id2(AID)]),
	    icgen:emit(Fd, "      return oe_error_code;\n");
	_ ->
	    case T of
		{short,_} ->
		    icgen:emit(Fd, "    {\n"),
		    icgen:emit(Fd, "      long oe_long;\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n\n"),
		    icgen:emit(Fd, "      oe_rec->_u.~s = (short) oe_long;\n\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "      if (oe_rec->_u.~s !=  oe_long)\n        return -1;\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "    }\n");
		{unsigned,{short,_}} ->
		    icgen:emit(Fd, "    {\n"),
		    icgen:emit(Fd, "      unsigned long oe_ulong;\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n\n"),
		    icgen:emit(Fd, "      oe_rec->_u.~s = (unsigned short) oe_ulong;\n\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "      if (oe_rec->_u.~s !=  oe_ulong)\n        return -1;\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "    }\n");
		{long, _} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{unsigned,{long,_}} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{float,_} ->
		    icgen:emit(Fd, "    {\n"),
		    icgen:emit(Fd, "      double oe_double;\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_double)) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n\n"),
		    icgen:emit(Fd, "      oe_rec->_u.~s = (float) oe_double;\n",[icgen:get_id2(I)]),
		    icgen:emit(Fd, "    }\n");
		{double,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{boolean,_} ->
		    icgen:emit(Fd, "    {\n"),
		    icgen:emit(Fd, "      char oe_bool[25];\n\n"),
		    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0)\n"),
		    icgen:emit(Fd, "        return oe_error_code;\n\n"),
		    icgen:emit(Fd, "      if (strcmp(oe_bool, \"false\") == 0) {\n"),
		    icgen:emit(Fd, "        oe_rec->_u.~s = 0;\n",[icgen:get_id2(I)]), 
		    icgen:emit(Fd, "      }\n"),
		    icgen:emit(Fd, "      else if (strcmp(oe_bool, \"true\") == 0) {\n"),
		    icgen:emit(Fd, "        oe_rec->_u.~s = 1;\n",[icgen:get_id2(I)]), 
		    icgen:emit(Fd, "      }\n"),
		    icgen:emit(Fd, "      else\n"),
		    icgen:emit(Fd, "        return -1;\n"),
		    icgen:emit(Fd, "    }\n");
		{char,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{octet,_} ->
		    icgen:emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{string,_} ->
		    icgen:emit(Fd, "    {\n"),
		    icgen:emit(Fd, "      int oe_type = 0;\n"),
		    icgen:emit(Fd, "      int oe_string_ctr = 0;\n\n"),
		    
		    icgen:emit(Fd, "      (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n"),

		    icgen:emit(Fd, "      oe_rec->_u.~s = (void *) (oe_first + *oe_index);\n\n",[icgen:get_id2(I)]),
		    
		    icgen:emit(Fd, "      if ((oe_error_code = ei_decode_string(oe_env->_inbuf, &oe_env->_iin, oe_rec->_u.~s)) < 0)\n",
			       [icgen:get_id2(I)]),
		    icgen:emit(Fd, "        return oe_error_code;\n\n"),
		    
		    icgen:emit(Fd, "      *oe_index = ~s;\n",[icgen:mk_align("*oe_index+oe_string_ctr+1")]),
		    icgen:emit(Fd, "    }\n");
		{sequence,_,_} ->
		    SeqName = icgen:to_undersc([icgen:get_id2(I), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
			       [SeqName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{struct,SID,_,_} ->
		    StructName = icgen:to_undersc([icgen:get_id2(SID), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
			       [StructName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		{union,UID,_,_,_} ->
		    UnionName = icgen:to_undersc([icgen:get_id2(UID), icgen:get_id2(X) | N]),
		    icgen:emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0)\n",
			       [UnionName,icgen:get_id2(I)]),
		    icgen:emit(Fd, "      return oe_error_code;\n");
		_ ->
		    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.






mvDefaultToTail(CDclL) ->
    mvDefaultToTail(CDclL,[],[]).
    

mvDefaultToTail([], F, FD) ->
    lists:reverse(F) ++ FD;
mvDefaultToTail([{case_dcl,CaseList,I,T}|Rest], Found, FoundDefault) ->
    case lists:keysearch(default, 1, CaseList) of
	{value,Default} ->
	    NewCaseList = lists:delete(Default, CaseList) ++ [Default],
	    mvDefaultToTail(Rest, Found, [{case_dcl,NewCaseList,I,T}|FoundDefault]);
	false ->
	    mvDefaultToTail(Rest, [{case_dcl,CaseList,I,T}|Found], FoundDefault)
    end.


    
















