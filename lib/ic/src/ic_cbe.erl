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
-module(ic_cbe).


-export([do_gen/3]).
-export([gen_malloc_size_calculation/7, gen_malloc_size_calculation_rec/6,
	 gen_encoding_fun/6, gen_decoding_fun/10,
	 gen_variable_name/1, gen_cc_type/3, gen_cc_type/4, 
	 check_dynamic_size/1, check_dynamic_size/3, mk_dim/1, mk_slice_dim/1,
	 emit_tmp_variables/1, store_tmp_decl/2, extract_info/3]).

%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------
-export([unfold/1, mk_attr_func_names/2]).

-import(icgen, [mk_name/2, mk_oe_name/2, mk_var/1, get_id/1, mk_list/1,
		emit/3, emit/2,
		get_opt/2, 
		nl/1, is_oneway/1,
		to_list/1, to_atom/1, get_id2/1, get_type/1, get_body/1,
		skelfiled/1, stubfiled/1,
		push_file/2, pop_file/2, sys_file/2]).


-import(lists, [foreach/2, foldr/3, map/2]).


-include("icforms.hrl").
-include("ic.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(IC_HEADER, "ic.h").
-define(ERL_INTERFACEHEADER, "erl_interface.h").
-define(EICONVHEADER, "ei.h").
-define(ERLANGATOMLENGTH, "256").

%%------------------------------------------------------------
%%
%% Entry point
%%
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    G2 = icgen:filename_push(G, [], mk_oe_name(G, remove_ext(to_list(File))), c),
    gen_head(G2, [], Form),
    R = gen(G2, [], Form),
    icgen:filename_pop(G2, c),
    R.

remove_ext(File) ->
    filename:rootname(filename:basename(File)).

%%------------------------------------------------------------
%%
%% Generate the client side C stubs.
%%
%% Each module is generated to a separate file.
%%
%% Each function needs to generate a function head and
%% a body. IDL parameters must be converted into C parameters.
%%
%%------------------------------------------------------------

gen(G, N, [X|Xs]) when record(X, preproc) ->
    NewG = handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);

gen(G, N, [X|Xs]) when record(X, module) ->
    CD = icgen:codeDirective(G, X), 
    G2 = icgen:filename_push(G, N, X, CD),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    G3 = icgen:filename_pop(G2, CD),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, interface) ->
    G2 = icgen:filename_push(G, N, X, c),
    N2 = [get_id2(X) | N],
    %% Sets the temporary variable counter.
    put(op_variable_count, 0),
    put(tmp_declarations, []),
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    foreach(fun({Name, Body}) -> gen(G2, N2, Body) end, 
	    X#interface.inherit_body), 

    %% New Adds

    %% Generate Prototypes
    gen_prototypes(G2,N2,X),

    %% Generic functions
    emit_client_generic_decoding(G2, N2, X),

    G3 = icgen:filename_pop(G2, c),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, const) ->
    emit_constant(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, op) ->
    {Name, ArgNames, TypeList} = extract_info(G, N, X),
    gen_sync_client_func(G, N, X, Name, ArgNames, TypeList),
    gen_client_enc_func(G, N, X, Name, ArgNames, TypeList),
    gen_client_dec_func(G, N, X, Name, ArgNames, TypeList),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, attr) ->
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, except) ->
    icstruct:except_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, enum) ->
    icenum:enum_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, c);  %% create h - file
	false -> ok
    end,
    gen(G, N, Xs);

gen(G, N, []) -> ok.


may_contain_structs(X) when record(X, typedef) -> true;
may_contain_structs(X) when record(X, struct) -> true;
may_contain_structs(X) when record(X, union) -> true;
may_contain_structs(X) -> false.

handle_preproc(G, N, line_nr, X) ->
    Id = get_id2(X),
    Flags = X#preproc.aux,
    case Flags of
	[] -> push_file(G, Id);
	_ ->
	    foldr(fun({_, _, "1"}, Gprim) -> push_file(Gprim, Id);
		     ({_, _, "2"}, Gprim) -> pop_file(Gprim, Id);
		     ({_, _, "3"}, Gprim) -> sys_file(Gprim, Id) end,
		  G, Flags)
    end;
handle_preproc(G, N, Other, X) ->
    G.







%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when record(X, interface) ->
    case icgen:is_hrlfile_open(G) of
	true ->
	    %% Sets the temporary variable counter.
	    put(op_variable_count, 0),
	    put(tmp_declarations, []),
	    HFd = icgen:hrlfiled(G),
	    IncludeFileStack = icgen:include_file_stack(G),
	    L = length(N),
	    Filename =
		if
		    L < 2 ->
			lists:nth(L + 1, IncludeFileStack);
		    true ->
			lists:nth(2, IncludeFileStack)
		end,
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]),
	    icgen:gen_includes(HFd,G,X,c_client),

	    IName = icgen:to_undersc(N),
	    icgen:emit(HFd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(IName)]),	
	    icgen:emit(HFd, "#define __~s__\n",[ic_util:to_uppercase(IName)]),
	    icgen:mcomment_light(HFd,
				 [io_lib:format("Interface object definition: ~s",
						[IName])],
				 c),
	    icgen:emit(HFd, "typedef CORBA_Object ~s;\n\n", [IName]),	

	    icgen:emit(HFd, "#endif\n\n");

	false -> ok
    end,
    case icgen:is_stubfile_open(G) of
	true ->
	    Fd = icgen:stubfiled(G),
	    nl(Fd),
	    emit(Fd, "#include <stdlib.h>\n"),
	    emit(Fd, "#include <string.h>\n"),
	    emit(Fd, "#include \"~s\"\n", [?IC_HEADER]),
	    emit(Fd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]),
	    emit(Fd, "#include \"~s\"\n", [?EICONVHEADER]),
	    emit(Fd, "#include \"~s\"\n", [filename:basename(icgen:include_file(G))]),
	    nl(Fd), nl(Fd),
	    Fd;
	false ->
	    ok
    end;

%% Some items have extra includes
gen_head_special(G, N, X) when record(X, module) ->
    case icgen:is_hrlfile_open(G) of
	true ->
	    HFd = icgen:hrlfiled(G),
	    IncludeFileStack = icgen:include_file_stack(G),
	    Filename = lists:nth(length(N) + 1, IncludeFileStack),
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]),
	    icgen:gen_includes(HFd,G,X,c_client);
	false -> ok
    end;
gen_head_special(G, [], X) -> 
    case icgen:is_hrlfile_open(G) of
	true ->
	    HFd = icgen:hrlfiled(G),
	    emit(HFd, "#include \"~s\"\n", [?IC_HEADER]),
	    emit(HFd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]),
	    emit(HFd, "#include \"~s\"\n", [?EICONVHEADER]),
	    icgen:gen_includes(HFd,G,c_client);
	false -> ok
    end;
gen_head_special(G, N, X) -> ok.

%% Open stubfile
gen_head(G, N, X) -> 
    gen_head_special(G, N, X).


gen_variable_name(Var) ->
    Nr = get(Var),
    put(Var, Nr + 1),
    "oe_tmp" ++ integer_to_list(Nr).

store_tmp_decl(Format, Args) ->
    Decl = io_lib:format(Format, Args),
    DeclList = get(tmp_declarations),
    put(tmp_declarations, [Decl |DeclList]).

emit_tmp_variables(Fd) ->
    DeclList = get(tmp_declarations),
    emit_tmp_variables(Fd, DeclList),
    ok.

emit_tmp_variables(Fd, []) ->
    ok;
emit_tmp_variables(Fd, [Decl |Rest]) ->
    emit_tmp_variables(Fd, Rest),
    emit(Fd, "~s", [Decl]).
    

gen_type_arg_list([], []) ->
    [];
gen_type_arg_list([Type |Types], [{Attr, Arg}|Args]) ->
    [{Type, Attr, Arg}| gen_type_arg_list(Types, Args)].

check_refval(in) ->
    "";
check_refval(inout) ->
    error;
check_refval(_) ->
    "*".

gen_result_par(G, N, Type) ->
    Ctype = gen_cc_type(G, N, Type),
    Dyn = case check_dynamic_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  %% WSTRING
			  "";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  "*"
		  end;
	      false ->
		  ""
	  end,
    Ctype ++ Dyn ++ check_refval(out) ++ " " ++ "oe_result".


gen_par_list(_, _, _, [], []) ->
    [];
gen_par_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    IsArray = ictype:isArray(G, N, Type),
	    IsStruct = ictype:isStruct(G, N, Type),
	    IsUnion = ictype:isUnion(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case IsArray of
				      true ->
					  if Attr == out ->
						  "_slice**";
					     true ->
						  ""
					  end;
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  if 
			      Attr == in, Ctype == "erlang_pid" ->
				  "*";
			      Attr == in, Ctype == "erlang_port" ->
				  "*";
			      Attr == in, Ctype == "erlang_ref" ->
				  "*";
			      Attr == in, IsStruct == true ->
				  "*";
			      Attr == in, IsUnion == true ->
				  "*";
			      true ->
				  ""
			  end
		  end,
	    
	    if Attr == out, IsArray == true ->
		    [Ctype ++ Dyn ++ " " ++ Arg | 
		     gen_par_list(G, N, X, Types, Args)];
	       true ->
		    [Ctype ++ Dyn ++ RefVal ++ " " ++ Arg | 
		     gen_par_list(G, N, X, Types, Args)]
	    end
    end.

emit_stub_func_decl(G, Fd, N, X, R) ->
    case is_oneway(X) of
	true ->
	    ok;
	false ->
	    case ictype:isArray(G, N, R) of
		true ->
		    emit(Fd, "  ~s oe_result = NULL;\n",[gen_ret_type(G, N, R)]);
		false ->
		    if element(1,R) /= 'void' ->
			    emit(Fd, "  ~s oe_result;\n",[gen_ret_type(G, N, R)]);
		       true ->
			    true
		    end
	    end,
	    emit(Fd, "  int oe_msgType = 0;\n"),
	    emit(Fd, "  erlang_msg oe_msg;\n")
    end,
    emit(Fd, "\n").


emit_ref_init(G, Fd, X) ->
    case is_oneway(X) of
	true ->
	    ok;
	false ->
	    emit(Fd, "  /* Initiating the message reference */\n"),
	    emit(Fd, "  ic_init_ref(oe_env,&oe_env->_unique);\n\n")
    end.

	    

emit_encoding(G, N, Fd, X, TAlist, false) ->
    emit(Fd, "  oe_ei_encode_version(oe_env);\n"),
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 3);\n"),
    %% Call or Cast
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n", ["$gen_call"]),
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 2);\n\n"),
    %% Unique ref. field
    %% From pid
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, oe_env->_from_pid)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, &oe_env->_unique)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),
    emit_encoding_1(G, N, Fd, X, TAlist);
emit_encoding(G, N, Fd, X, TAlist, true) ->
    emit(Fd, "  oe_ei_encode_version(oe_env);\n"),
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 2);\n\n"),
    %% Call or Cast
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n", ["$gen_cast"]),
    emit_encoding_1(G, N, Fd, X, TAlist).
    
emit_encoding_1(G, N, Fd, X, []) ->
    {ScopedName, _, _} = extract_info(G, N, X),
    Name = case icgen:get_opt(G, scoped_op_calls) of 
	       true -> 
		   ScopedName;
	       false ->
		   icgen:get_id2(X)
	   end,
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n\n", [Name]);
emit_encoding_1(G, N, Fd, X, TAlist) ->
    {ScopedName, _, _} = extract_info(G, N, X),
    Name = case icgen:get_opt(G, scoped_op_calls) of 
	       true -> 
		   ScopedName;
	       false ->
		   icgen:get_id2(X)
	   end,
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, ~p);\n",[length(TAlist) + 1]),
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n\n", [Name]),

    foreach(fun({T1, A1, N1}) ->
		    case T1 of
			{'void', _} ->
			    ok;
			_ ->
			    Refstring = check_refval(A1),
			    emit_encoding_comment(G, N, Fd, "Encode", Refstring, T1, N1),
			    gen_encoding_fun(G, N, X, Fd, T1, Refstring ++ N1, "oe_env->_outbuf")
		    end
	    end, TAlist),
    ok.

emit_send(Fd, FdVar, FromPidVar, ToPidVar, RegVar) ->
    emit(Fd, "  if (strlen(~s) == 0) {\n", [RegVar]),
    emit(Fd, "    if ((oe_error_code = ei_send_encoded(~s, ~s, oe_env->_outbuf, oe_env->_iout)) < 0) {\n",
	 [FdVar, ToPidVar]),
    emit(Fd, "        return oe_error_code;\n"),
    emit(Fd, "      }\n"),
    emit(Fd, "    }\n"),
    emit(Fd, "    else if ((oe_error_code = ei_send_reg_encoded(~s, ~s, ~s, oe_env->_outbuf, oe_env->_iout))< 0) {\n",
	 [FdVar, FromPidVar, RegVar]),
    emit(Fd, "      return oe_error_code;\n"),
    emit(Fd, "    }\n\n"),
    ok.

emit_receive(Fd, FdVar) ->
    emit(Fd, "  do {\n"),
    emit(Fd, "   if((oe_msgType = ei_receive_encoded(~s, &oe_env->_inbuf, &oe_env->_inbufsz, &oe_msg, &oe_env->_iin)) < 0)\n", 
	 [FdVar]), 
    emit(Fd, "     return -1;\n"),
    emit(Fd, "  } while (oe_msgType != ERL_SEND && oe_msgType != ERL_REG_SEND);\n\n"),
    emit(Fd, "  oe_env->_iin = 0;\n\n"),
    emit(Fd, "  if ((oe_error_code = ei_decode_version(oe_env->_inbuf, &oe_env->_iin, &oe_rec_version)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),
    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),
    emit(Fd, "  if (oe_env->_received != 2)\n"),
    emit(Fd, "    return -1;\n\n"),
    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, &oe_env->_iin, &oe_env->_unique)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),
    ok.

emit_decoding(G, N, Fd, Result, []) ->
    %% Fetching the result value
    emit(Fd,"  /* Decode result value: ~s* oe_result */\n", [gen_cc_type(G, N, Result)]),

    case check_dynamic_size(G, N, Result) of
	false ->
	    case ictype:isArray(G, N, Result) of
		true ->
		    emit(Fd, 
			 "  {\n"
			 "    int oe_size_count_index = oe_env->_iin;\n"
			 "    int oe_malloc_size = 0;\n"
			 "    char *oe_first = 0;\n\n"),
		    gen_malloc_size_calculation(G, N, Fd, Result, "oe_env->_inbuf", 1, caller),
		    emit(Fd, 
			 "    if (oe_malloc_size > 0) {\n"
			 "      oe_first = malloc(oe_malloc_size);\n"
			 "      (*oe_result) = (void *) oe_first;\n"
			 "    }\n\n"),
		    gen_decoding_fun(G, N, Fd, Result, "oe_result", "", "oe_env->_inbuf", 1, "&oe_outindex", array_fix_ret),
		    emit(Fd, "   }\n\n");
		false ->
		    %% The last parameter "oe_outindex" is not interesting 
		    %% in the static case.
		    gen_decoding_fun(G, N, Fd, Result, "oe_result", "", "oe_env->_inbuf", 1, "&oe_outindex", caller),
		    nl(Fd)
	    end;
	true ->
	    emit(Fd, 
		 "  {\n"
		 "    int oe_size_count_index = oe_env->_iin;\n"
		 "    int oe_malloc_size = 0;\n"
		 "    char *oe_first = 0;\n\n"),
	    gen_malloc_size_calculation(G, N, Fd, Result, "oe_env->_inbuf", 1, caller),
	    emit(Fd, 
		 "    if (oe_malloc_size > 0) {\n"
		 "      oe_first = malloc(oe_malloc_size);\n"
		 "      (*oe_result) = (void *) oe_first;\n"
		 "    }\n\n"),

	    case ictype:isArray(G, N, Result) of
		true ->
                    %%%%%%%%%%%%%%%%%%%%%%%%
		    %% Array of dynamic type
		    %%%%%%%%%%%%%%%%%%%%%%%%
		    gen_decoding_fun(G, N, Fd, Result, "(*oe_result)", "", "oe_env->_inbuf", 1, "&oe_outindex", array_dyn),
		    emit(Fd, "   }\n\n");
		false ->
		    gen_decoding_fun(G, N, Fd, Result, "(*oe_result)", "", "oe_env->_inbuf", 1, "&oe_outindex", caller_dyn),
		    emit(Fd, "   }\n\n")
	    end

    end,
    ok;
emit_decoding(G, N, Fd, Result, TAlist) ->
    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received)) < 0)\n"),
    emit(Fd, "    return oe_error_code;\n\n"),

    emit(Fd, "  if (oe_env->_received != ~p)\n", 
	 [length(TAlist) + 1]),
    emit(Fd, "    return -1;\n\n"),

    %% Fetching the result value
    emit(Fd, "  /* Decode result value: ~s *oe_result */\n",[gen_cc_type(G, N, Result)]),
    case check_dynamic_size(G, N, Result) of
	false ->
	    case ictype:isArray(G, N, Result) of
		true ->
		    emit(Fd, 
			 "  {\n"
			 "    int oe_size_count_index = oe_env->_iin;\n"
			 "    int oe_malloc_size = 0;\n"
			 "    char *oe_first = 0;\n\n"),
		    gen_malloc_size_calculation(G, N, Fd, Result, "oe_env->_inbuf", 1, caller),
		    emit(Fd, 
			 "    if (oe_malloc_size > 0) {\n"
			 "      oe_first = malloc(oe_malloc_size);\n"
			 "      *oe_result = (void *) oe_first;\n"
			 "    }\n\n"),
		    gen_decoding_fun(G, N, Fd, Result, "oe_result", "", "oe_env->_inbuf", 1, "&oe_outindex", array_fix_ret),
		    emit(Fd, "  }\n\n");
		false ->
		    %% The last parameter "oe_outindex" is not interesting 
		    %% in the static case.
		    gen_decoding_fun(G, N, Fd, Result, "oe_result", "", "oe_env->_inbuf", 1, "&oe_outindex", caller),
		    nl(Fd)
	    end;
	true ->
	    emit(Fd, 
		 "  {\n"
		 "    int oe_size_count_index = oe_env->_iin;\n"
		 "    int oe_malloc_size = 0;\n"
		 "    char *oe_first = 0;\n\n"),
	    gen_malloc_size_calculation(G, N, Fd, Result, "oe_env->_inbuf", 1, caller),
	    emit(Fd, 
		 "    if (oe_malloc_size > 0) {\n"
		 "      oe_first = malloc(oe_malloc_size);\n"
		 "      (*oe_result) = (void *) oe_first;\n"
		 "    }\n\n"),
	    case ictype:isArray(G, N, Result) of
		true ->
                    %%%%%%%%%%%%%%%%%%%%%%%%
		    %% Array of dynamic type
		    %%%%%%%%%%%%%%%%%%%%%%%%
		    gen_decoding_fun(G, N, Fd, Result, "(*oe_result)", "", "oe_env->_inbuf", 1, "&oe_outindex", array_dyn),
		    emit(Fd, "  }\n\n");
		false ->
		    gen_decoding_fun(G, N, Fd, Result, "(*oe_result)", "", "oe_env->_inbuf", 1, "&oe_outindex", caller_dyn),
		    emit(Fd, "  }\n\n")
	    end
    end,

    foreach(fun({T, A, N1}) ->
		    case T of
			{'void', _} ->
			    ok;
			_ ->
			    Refstring = check_refval(A),
			    case check_dynamic_size(G, N, T) of
				false ->
				    case ictype:isArray(G, N, T) of
					true ->
					    emit_encoding_comment(G, N, Fd, "Decode", "", T, N1),
					    gen_decoding_fun(G, N, Fd, T, N1, "",
							     "oe_env->_inbuf", 1, "&oe_outindex", array_fix_out),
					    nl(Fd);
					false ->
					    %% The last parameter "oe_outindex" is not interesting 
					    %% in the static case.
					    emit_encoding_comment(G, N, Fd, "Decode", Refstring, T, N1),
					    gen_decoding_fun(G, N, Fd, T,  N1, "",
							     "oe_env->_inbuf", 1, "&oe_outindex", caller), 
					    nl(Fd)
				    end;
				true ->
				    emit_encoding_comment(G, N, Fd, "Decode", Refstring, T, N1),
				    emit(Fd, 
					 "  {\n"
					 "    int oe_size_count_index = oe_env->_iin;\n"
					 "    int oe_malloc_size = 0;\n"
					 "    char *oe_first = 0;\n\n"),
				    gen_malloc_size_calculation(G, N, Fd, T, "oe_env->_inbuf", 1, caller),
				    emit(Fd, 
					 "    if (oe_malloc_size > 0) {\n"
					 "      oe_first = malloc(oe_malloc_size);\n"
					 "      (~s~s) = (void *) oe_first;\n"
					 "    }\n\n",
					 [Refstring, N1]),
				    case ictype:isArray(G,N,T) of
					true ->
					    %%%%%%%%%%%%%%%%%%%%%%%%
					    %% Array of dynamic type
					    %%%%%%%%%%%%%%%%%%%%%%%%
					    gen_decoding_fun(G, N, Fd, T,  "(" ++ Refstring ++ N1 ++ ")", "",
							     "oe_env->_inbuf", 1, "&oe_outindex", array_dyn),
					    emit(Fd, "  }\n\n");
					false ->
					    gen_decoding_fun(G, N, Fd, T,  "(" ++ Refstring ++ N1 ++ ")", "",
							     "oe_env->_inbuf", 1, "&oe_outindex", caller_dyn),
					    emit(Fd, "  }\n\n")
				    end
			    end;
			_ ->
			    ok
		    end
	    end, TAlist),
    ok.


emit_constant(G, N, ConstRecord) ->
    case icgen:is_hrlfile_open(G) of
	false -> ok;
	true ->
	    Fd = icgen:hrlfiled(G),
	    CName = ic_util:to_undersc([get_id(ConstRecord#const.id)|N]),
	    UCName = ic_util:to_uppercase(CName),

	    emit(Fd, "\n#ifndef __~s__\n",[UCName]),
	    emit(Fd, "#define __~s__\n\n", [UCName]),

	    emit(Fd, "/* Constant: ~s */\n", [CName]),
	    
	    if record(ConstRecord#const.type,wstring) -> %% If wstring, add 'L' 
		    emit(Fd, "#define ~s L~p\n\n", [CName, ConstRecord#const.val]);
	       true ->
		    emit(Fd, "#define ~s ~p\n\n", [CName, ConstRecord#const.val])
	    end,

	    emit(Fd, "#endif\n\n")
    end.



%%------------------------------------------------------------
%%    Special comment functions
%%------------------------------------------------------------

emit_encoding_comment(G, N, F, String, RefOrVal, Type, Name) ->
    emit(F,"  /* ~s parameter: ~s~s ~s */\n",
	 [String, gen_cc_type(G, N, Type),RefOrVal, Name]).


emit_op_comment(G, F, X, Name, InP) ->
    icgen:mcomment_light(F,
			 [io_lib:format("~s: ~s", [get_title(X), Name]),
			  "",
			  get_returns(G, X, InP, []) |
			  get_raises(X)], c).

get_title(X) when record(X, attr) -> "Attribute Operation";
get_title(X) -> "Operation".

get_raises(X) when record(X, op) ->
    if  X#op.raises == [] -> [];
	true ->
	    ["  Raises:  " ++ 
	     mk_list(map({icgen, to_colon}, X#op.raises))]
    end;
get_raises(X) -> [].

get_returns(G, X, InP, []) ->
    "  Returns: RetVal";
get_returns(G, X, InP, OutP) ->
    "  Returns: "++mk_list(["RetVal" | mk_c_vars(OutP)]).




%%------------------------------------------------------------
%%
%% Utilities
%%
%% Convenient little go-get functions
%%
%%------------------------------------------------------------

%% The automaticly generated get and set operation names for an
%% attribute.
mk_attr_func_names(Scope, Name) ->
    {icgen:to_undersc(["_get_" ++ Name| Scope]), icgen:to_undersc(["_set_" ++ Name|Scope])}.

%% Returns TK of the Get and Set attribute functions.
mk_attr_func_types(N, X) ->
    TK = ic_forms:get_tk(X),
    Type = get_type(X),
    {{Type, [], []}, {{void, 0}, [Type], []}}.
        


%%------------------------------------------------------------
%%
%% Generation utilities and common stuff
%%
%% Convenient stuff for generation
%%
%%------------------------------------------------------------


%% Input is a list of parameters (in parse form) and output is a list
%% of parameter attribute and varaible names.
mk_c_vars(Params) ->
    map(fun(P) -> {A, _} = P#param.inout,
		  {A, get_id(P#param.id)}
	end,
	Params).


%% Filters parameters so that only those with certain attributes are
%% seen. The filter parameter is a list of attributes that will be
%% seen, ex. [in] or [inout, out]
filter_params(Filter, Params) ->
    lists:filter(fun(P) ->
		    lists:member(get_param_attr(P#param.inout), Filter) end,
		 Params).


%% Access primitive to get the attribute name (and discard the line
%% number).
get_param_attr({A, N}) -> A.



%%------------------------------------------------------------
%%
%% Parser utilities
%%
%% Called from the yecc parser. Expands the identifier list of an
%% attribute so that the attribute generator never has to handle
%% lists.
%%
%%------------------------------------------------------------


%% Unfold identifier lists or nested lists. Note that many records
%% contain an entry named id that is a list before unfold and a single
%% id afterwards.
unfold(L) when list(L) ->
    lists:flatten(map(fun(X) -> unfold2(X) end, L));
unfold(X) -> unfold2(X).
    
unfold2(A) when record(A, attr) ->
    map(fun(Id) -> A#attr{id=Id} end, A#attr.id);
unfold2(M) when record(M, member) ->
    map(fun(Id) -> M#member{id=Id} end, M#member.id);
unfold2(M) when record(M, case_dcl) ->
    map(fun(Id) -> M#case_dcl{label=Id} end, M#case_dcl.label);
unfold2(T) when record(T, typedef) ->
    map(fun(Id) -> T#typedef{id=Id} end, T#typedef.id).


emit_attr(G, N, X, F) ->
    XX = #id_of{type=X},
    {GetType, SetType} = mk_attr_func_types(N, X),
    foreach(fun(Id) ->
		    X2 = XX#id_of{id=Id},
		    {Get, Set} = mk_attr_func_names(N, get_id(Id)),
		    F(G, N, X2, Get, [], GetType),
		    case X#attr.readonly of
			{readonly, _} -> ok;
			_ -> 
			    F(G, N, X2, Set, [{in, mk_name(G, "Value")}], SetType)
		    end
	    end, icgen:get_idlist(X)),
    ok.

extract_info(G, N, X) when record(X, op) ->
    Name	=  icgen:to_undersc([get_id2(X) | N]),
    Args	= X#op.params,
    ArgNames	= mk_c_vars(Args),
    TypeList	= {get_type(X),
		   map(fun(Y) -> get_type(Y) end, Args),
		   []
		  },
    {Name, ArgNames, TypeList};
extract_info(G, N, X) ->
    Name	=  icgen:to_undersc([get_id2(X) | N]),
    {Name, [], []}.



%%------------------------------------------------------------
%%    IDL to C type conversion
%%------------------------------------------------------------
gen_malloc_c_type(G, N, S) ->
    case gen_cc_type(G, N, S) of
	"CORBA_char *" ->
	    "CORBA_char";
	T ->
	    T
    end.

gen_cc_type(G, N, S) ->
    gen_cc_type(G, N, S, evaluate).

gen_cc_type(G, N, S, evaluate) when element(1, S) == scoped_id ->
    {FullScopedName, T, TK, _} = icgen:get_full_scoped_name(G, N, S),
    BT = icgen:get_basetype(G, icgen:to_undersc(FullScopedName)),
    case BT of
	"erlang_pid" ->
	    "erlang_pid";
	"erlang_port" ->
	    "erlang_port";
	"erlang_ref" ->
	    "erlang_ref";
	"erlang_term" ->
	    "ETERM*";
	{enum, Type} ->
	    gen_cc_type(G, N, Type, evaluate);
	Type ->
	    gen_cc_type(G, N, Type, evaluate)
    end;
gen_cc_type(G, N, S, evaluate_not) when element(1, S) == scoped_id ->
    {FullScopedName, T, TK, _} = icgen:get_full_scoped_name(G, N, S),
    BT = icgen:get_basetype(G, icgen:to_undersc(FullScopedName)),
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
gen_cc_type(G, N, S, _) when list(S) ->
    S;
gen_cc_type(G, N, S, _) when record(S, string) ->
    "CORBA_char *";
gen_cc_type(G, N, S, _) when record(S, wstring) ->  %% WSTRING
    "CORBA_wchar *";
gen_cc_type(G, N, {boolean, _}, _) ->
    "CORBA_boolean";
gen_cc_type(G, N, {octet, _}, _) ->
    "CORBA_octet";
gen_cc_type(G, N, {void, _}, _) ->
    "void";
gen_cc_type(G, N, {unsigned, U}, _) ->
    case U of
	{short,_} ->
	    "CORBA_unsigned_short";
	{long,_} ->
	    "CORBA_unsigned_long";
	{'long long',_} ->
	    "CORBA_unsigned_long_long"
    end;
gen_cc_type(G, N, {'long long', _}, _) ->
    "CORBA_long_long";
gen_cc_type(G, N, S, _) when record(S, union)->
    icgen:get_id2(S);
gen_cc_type(G, N, {T, _}, _) ->
    "CORBA_" ++ atom_to_list(T).


%%------------------------------------------------------------
%%    C to erlang type conversion
%%------------------------------------------------------------

%% Used for malloc_size_calc
gen_encoding_fun(G, N, Fd, T, LName, OutBuffer) when element(1, T) == scoped_id ->
    case gen_cc_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_port" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_port(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_ref" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"ETERM*" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_term(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{enum, FSN} ->
	    gen_encoding_fun(G, N, Fd, FSN, LName, OutBuffer);
	FSN ->
	    gen_encoding_fun(G, N, Fd, FSN, LName, OutBuffer)
    end;
gen_encoding_fun(G, N, Fd, T, LName, OutBuffer)  when list(T) -> %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    case Type of
		ushort ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0)\n", 
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		ulong -> 
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0)\n", 
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		short ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		long ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		float ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		double ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		boolean ->
		    emit(Fd, "  switch(~s) {\n",[LName]),
		    emit(Fd, "    case 0 :\n"),
		    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      break;\n"),
		    emit(Fd, "    case 1 :\n"),
		    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      break;\n"),
		    emit(Fd, "    default :\n"),
		    emit(Fd, "      return -1;\n"),
		    emit(Fd, "  }\n\n");
		char ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		wchar ->  %% WCHAR
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		
		octet ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
			 [LName]),
		    emit(Fd, "    return oe_error_code;\n\n")
	    end;
	false ->
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
		 [icgen:mk_oe_name(G, "encode_"), T, LName]),
	    emit(Fd, "    return oe_error_code;\n\n")
    end;
gen_encoding_fun(G, N, Fd, T, LName, OutBuffer)  when record(T, string) ->
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_string(oe_env, ~s)) < 0)\n", 
	 [LName]),
    emit(Fd, "    return oe_error_code;\n\n");
gen_encoding_fun(G, N, Fd, T, LName, OutBuffer)  when record(T, wstring) ->  %% WSTRING
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wstring(oe_env, ~s)) < 0)\n", 
	 [LName]),
    emit(Fd, "    return oe_error_code;\n\n");
gen_encoding_fun(G, N, Fd, T, LName, OutBuffer) ->
    case T of
	{unsigned, {short, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{unsigned, {long, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{unsigned, {'long long', _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulonglong(oe_env, ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{short, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{long, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{'long long', _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_longlong(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{float,_} ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{double, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{boolean, _} ->
	    emit(Fd, "  switch(~s) {\n",[LName]),
	    emit(Fd, "    case 0 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
	    emit(Fd, "        return oe_error_code;\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    case 1 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
	    emit(Fd, "        return oe_error_code;\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    default :\n"),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "  }\n\n");
	
	{char, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{wchar, _} ->  %% WCHAR
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{octet, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{void, _} ->
	    emit(Fd, "    return oe_error_code;\n\n");

	{sequence, _, _} ->
	    emit(Fd, "    return oe_error_code;\n\n");

	{ArrayType, {array, _, _}} ->
	    emit(Fd, "    return oe_error_code;\n\n");

	{union, _, _, _, _} -> %% Union as a member in struct !  
	    emit(Fd, "    return oe_error_code;\n\n");
	_ ->  
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.


%% Used for encoding
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer) when element(1, T) == scoped_id ->
    case gen_cc_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_port" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_port(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_ref" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"ETERM*" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_term(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{enum, FSN} ->
	    gen_encoding_fun(G, N, X, Fd, FSN, LName, OutBuffer);
	FSN ->
	    gen_encoding_fun(G, N, X, Fd, FSN, LName, OutBuffer)
    end;
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer)  when list(T) -> %% Already a fullscoped name
    case get_param_tk(LName,X) of
	error ->
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
		 [icgen:mk_oe_name(G, "encode_"), T, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	ParamTK ->
	    case check_dynamic_size(ParamTK) of
		true ->
		    if tuple(ParamTK) ->
			    case element(1,ParamTK) of
				tk_array ->
				    %%%%%%%%%%%%%%%%%%%%%%%%
				    %% Array of dynamic data
				    %%%%%%%%%%%%%%%%%%%%%%%%
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				_ ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n")
			    end;
		       true ->
			    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
				 [icgen:mk_oe_name(G, "encode_"), T, LName]),
			    emit(Fd, "    return oe_error_code;\n\n")
		    end;
		false ->
		    if atom(ParamTK) ->
			    case ParamTK of
				tk_ushort -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0)\n", 
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_ulong -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0)\n", 
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_ulonglong -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulonglong(oe_env, ~s)) < 0)\n", 
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_short ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_long ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_longlong ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_longlong(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_float ->
				    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_double ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_boolean ->
				    emit(Fd, "  switch(~s) {\n",[LName]),
				    emit(Fd, "    case 0 :\n"),
				    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
				    emit(Fd, "        return oe_error_code;\n"),
				    emit(Fd, "      break;\n"),
				    emit(Fd, "    case 1 :\n"),
				    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
				    emit(Fd, "        return oe_error_code;\n"),
				    emit(Fd, "      break;\n"),
				    emit(Fd, "    default :\n"),
				    emit(Fd, "      return -1;\n"),
				    emit(Fd, "  }\n\n");
				tk_char ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_wchar ->  %% WCHAR
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_octet ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
					 [LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				_ ->
				    emit(Fd, "    return oe_error_code;\n\n"),
				    ok
			    end;
		       true ->
			    case element(1,ParamTK) of
				tk_enum ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_array ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_struct ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				tk_union ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n");
				_ ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, &~s)) < 0)\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]),
				    emit(Fd, "    return oe_error_code;\n\n")
			    end
		    end
	    end
    end;
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer)  when record(T, string) ->
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_string(oe_env, ~s)) < 0)\n", 
	 [LName]),
    emit(Fd, "    return oe_error_code;\n\n");
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer)  when record(T, wstring) ->  %% WSTRING
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wstring(oe_env, ~s)) < 0)\n", 
	 [LName]),
    emit(Fd, "    return oe_error_code;\n\n");
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer) ->
    case T of
	{unsigned, {short, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{unsigned, {long, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{unsigned, {'long long', _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulonglong(oe_env, ~s)) < 0)\n", 
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{short, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{long, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{'long long', _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_longlong(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{float,_} ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{double, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{boolean, _} ->
	    emit(Fd, "  switch(~s) {\n",[LName]),
	    emit(Fd, "    case 0 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0)\n"),
	    emit(Fd, "        return oe_error_code;\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    case 1 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0)\n"),
	    emit(Fd, "        return oe_error_code;\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    default :\n"),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "  }\n\n");
	{char, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{wchar, _} -> %% WCHAR
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{octet, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0)\n",
		 [LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{void, _} ->
	    emit(Fd, "    return oe_error_code;\n\n"),
	    ok;
	{sequence, _, _} ->
	    emit(Fd, "    return oe_error_code;\n\n"),
	    ok;
	{ArrayType, {array, _, _}} ->
	    emit(Fd, "    return oe_error_code;\n\n"),
	    ok;
	_ ->
	    %%io:format("2 ------------> ~p~n", [T]),
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.




%% Usefull functions
get_param_tk(Name,Op) ->
    case get_param(Name,Op) of
	error ->
	    error;
	Param ->
	    ic_forms:get_tk(Param)
    end.

get_param(Name,Op) when record(Op, op) ->
    get_param_loop(Name,Op#op.params);
get_param(_Name,_Op) ->
    error.

get_param_loop(Name,[]) ->
    error;
get_param_loop(Name,[Param|Params]) ->
    case get_id2(Param) of
	Name ->
	    Param;
	_ ->
	    get_param_loop(Name,Params)
    end.
	    

    

%%------------------------------------------------------------
%%    Erlang to C type conversion
%%------------------------------------------------------------
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		  InBuffer, Align, NextPos, DecType) when element(1, T) == scoped_id ->
    case gen_cc_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_pid(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_port" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_port(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_ref" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_ref(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"ETERM*" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_term(~s, &oe_env->_iin, (void**)~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{enum, FSN} ->
	    gen_decoding_fun(G, N, Fd, FSN, LName, Refstring,
			     InBuffer, Align, NextPos, DecType);
	FSN ->
	    gen_decoding_fun(G, N, Fd, FSN, LName, Refstring,
			     InBuffer, Align, NextPos, DecType) 
    end;
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		 InBuffer, Align, NextPos, DecType)  when list(T) -> %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    case Type of
		ushort ->
		    emit(Fd, "  {\n"),
		    emit(Fd, "    unsigned long oe_ulong;\n"),
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, &oe_env->_iin, &oe_ulong)) < 0)\n",
			 [InBuffer]),
		    emit(Fd, "      return oe_error_code;\n\n"),
		    emit(Fd, "    *(~s) = (unsigned short) oe_ulong;\n\n",[LName]),
		    emit(Fd, "    if (*(~s) !=  oe_ulong)\n      return -1;\n",[LName]),
		    emit(Fd, "  }\n\n");
		
		ulong -> 
		    emit(Fd, "  if ((oe_error_code = ei_decode_ulong(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");

		ulonglong -> 
		    emit(Fd, "  if ((oe_error_code = oe_ei_decode_ulonglong(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		
		short ->
		    emit(Fd, "  {\n"),
		    emit(Fd, "    long oe_long;\n"),
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, &oe_env->_iin, &oe_long)) < 0)\n",
			 [InBuffer]),
		    emit(Fd, "      return oe_error_code;\n\n"),
		    emit(Fd, "    *(~s) = (short) oe_long;\n\n",[LName]),
		    emit(Fd, "    if (*(~s) !=  oe_long)\n      return -1;\n",[LName]),
		    emit(Fd, "  }\n\n");
		
		long ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_long(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");

		longlong ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_decode_longlong(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");

		float ->
		    emit(Fd, "  {\n"),
		    emit(Fd, "    double oe_double;\n"),
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, &oe_env->_iin, &oe_double)) < 0)\n",
			 [InBuffer]),
		    emit(Fd, "      return oe_error_code;\n\n"),
		    emit(Fd, "    *(~s) = (float) oe_double;\n",[LName]),
		    %%emit(Fd, "    if (*(~s) !=  oe_double)\n      return -1;\n",[LName]), Does not work on floats !
		    emit(Fd, "  }\n\n");
		
		double ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_double(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		
		boolean ->
		    emit(Fd, "  {\n"),
		    emit(Fd, "    char oe_bool[25];\n\n"),
		    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, &oe_env->_iin, oe_bool)) < 0)\n",[InBuffer]),
		    emit(Fd, "      return oe_error_code;\n\n"),
		    emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"),
		    emit(Fd, "      *(~s) = 0;\n",[LName]), 
		    emit(Fd, "    }\n"),
		    emit(Fd, "    else if (strcmp(oe_bool, \"true\") == 0) {\n"),
		    emit(Fd, "      *(~s) = 1;\n",[LName]), 
		    emit(Fd, "    }\n"),
		    emit(Fd, "    else\n"),
		    emit(Fd, "      return -1;\n"),
		    emit(Fd, "  }\n\n");	
		
		char ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_char(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");

		wchar ->  %% WCHAR
		    emit(Fd, "  if ((oe_error_code = oe_ei_decode_wchar(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		
		octet ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_char(~s, &oe_env->_iin, ~s~s)) < 0)\n",
			 [InBuffer, Refstring, LName]),
		    emit(Fd, "    return oe_error_code;\n\n")
	    
	    end;
	false ->
	    case DecType of
		generator ->
		    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "    return oe_error_code;\n\n");
		caller -> %% No malloc used, define oe_first
		    emit(Fd, "    {\n"),
		    emit(Fd, "      char *oe_first = 0;\n"), 
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "    }\n");
		caller_dyn ->  %% Malloc used
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "    }\n");
		array_dyn ->  %% Malloc used
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
%			 [icgen:mk_align(io_lib:format("sizeof(~s)",[T]))]),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "    }\n");
		array_fix_ret ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s,*~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "    }\n");
		array_fix_out -> %% No malloc used, define oe_first
		    emit(Fd, "    {\n"),
		    emit(Fd, "      char *oe_first = 0;\n"), 
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
			 [icgen:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "    }\n")
	    end
    end;
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		 InBuffer, Align, NextPos, DecType)  when record(T, string) ->
    case DecType of
	caller_dyn ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_string(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	_ ->
	    emit(Fd, "  ~s~s = (void *)(oe_first + *oe_outindex);\n\n", 
		 [Refstring, LName]),
	    
	    emit(Fd, "  {\n"),
	    emit(Fd, "    int oe_type=0;\n"),
	    emit(Fd, "    int oe_string_ctr=0;\n\n"),
	    
	    emit(Fd, "    (int) ei_get_type(~s, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n",
		 [InBuffer]),
	    
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    
	    icgen:emit(Fd, "  *oe_outindex = ~s;\n",
		       [icgen:mk_align("*oe_outindex+oe_string_ctr+1")]),

	    emit(Fd, "  }\n\n")	
    end;
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		 InBuffer, Align, NextPos, DecType)  when record(T, wstring) ->  %% WSTRING
    case DecType of
	caller_dyn ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_decode_wstring(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	_ ->
	    emit(Fd, "  ~s~s = (void *)(oe_first + *oe_outindex);\n\n", 
		 [Refstring, LName]),
	    
	    emit(Fd, "  {\n"),
	    emit(Fd, "    int oe_type=0;\n"),
	    emit(Fd, "    int oe_string_ctr=0;\n\n"),
	    
	    emit(Fd, "    (int) ei_get_type(~s, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n",
		 [InBuffer]),
	    
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    
	    icgen:emit(Fd, "  *oe_outindex = ~s;\n",
		       [icgen:mk_align("*oe_outindex+oe_string_ctr+1")]),

	    emit(Fd, "  }\n\n")	
    end;
gen_decoding_fun(G, N, Fd, T, LName, Refstring, InBuffer, Align, NextPos, DecType) ->
    case T of
	{void, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_atom(~s, &oe_env->_iin, 0)) < 0)\n", 
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{unsigned, {short, _}} ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    unsigned long oe_ulong;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, &oe_env->_iin, &oe_ulong)) < 0)\n",
		 [InBuffer]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *(~s) = (unsigned short) oe_ulong;\n\n",[LName]),
	    emit(Fd, "    if (*(~s) !=  oe_ulong)\n      return -1;\n",[LName]),
	    emit(Fd, "  }\n\n");

	{unsigned, {long, _}} -> 
	    emit(Fd, "  if ((oe_error_code = ei_decode_ulong(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{unsigned, {'long long', _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_decode_ulonglong(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{short,_} ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    long oe_long;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, &oe_env->_iin, &oe_long)) < 0)\n",
		 [InBuffer]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *(~s) = (short) oe_long;\n\n",[LName]),
	    emit(Fd, "    if (*(~s) !=  oe_long)\n      return -1;\n",[LName]),
	    emit(Fd, "  }\n\n");


	{long, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_long(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{'long long', _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_decode_longlong(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{float,_} ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    double oe_double;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, &oe_env->_iin, &oe_double)) < 0)\n",
		 [InBuffer]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *(~s) = (float) oe_double;\n",[LName]),
	    %%emit(Fd, "    if (*(~s) !=  oe_double)\n      return -1;\n",[LName]), Does not work on floats !
	    emit(Fd, "  }\n\n");

	{double, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_double(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{boolean, _} ->
	     emit(Fd, "  {\n"),
	    emit(Fd, "    char oe_bool[25];\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, &oe_env->_iin, oe_bool)) < 0)\n",[InBuffer]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    emit(Fd, "      *(~s) = 0;\n",[LName]), 
	    emit(Fd, "    }\n"),
	    emit(Fd, "    else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    emit(Fd, "      *(~s) = 1;\n",[LName]), 
	    emit(Fd, "    }\n"),
	    emit(Fd, "    else\n"),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "  }\n\n");

	{char, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_char(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{wchar, _} ->  %% WCHAR
	    emit(Fd, "  if ((oe_error_code = oe_ei_decode_wchar(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{octet, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_char(~s, &oe_env->_iin, ~s~s)) < 0)\n",
		 [InBuffer, Refstring, LName]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{void, _} ->
	    emit(Fd, "    return oe_error_code;\n\n");

	{sequence, _, _} ->
	    ok;

	{_, {array, SId, Dims}} ->
	    AName = get_id2({array, SId, Dims}),
	    Ptr = "oe_out->"++AName,
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
		 [icgen:mk_oe_name(G, "decode_"),get_id2(SId), NextPos, Ptr]),
	    emit(Fd, "    return oe_error_code;\n\n");

	_ ->
	    %%io:format("3 ------------> ~p~n", [T]),
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.







%%------------------------------------------------------------
%%    Calculation of the malloc size for different types.
%%------------------------------------------------------------

gen_malloc_size_calculation_rec(G, N, Fd, [], InBuffer, Align) -> 
    ok;
gen_malloc_size_calculation_rec(G, N, Fd, [{_, T} |Es], InBuffer, Align) -> 
    gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, caller),
    gen_malloc_size_calculation_rec(G, N, Fd, Es, InBuffer, Align).

gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, CalcType) when element(1, T) == scoped_id ->
    case gen_cc_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_pid);\n\n", []),
	    emit(Fd, "  if ((oe_error_code = ei_decode_pid(~s, oe_size_count_index, 0))"
		 " < 0)\n",
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_port" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_port);\n\n", []),
	    emit(Fd, "  if ((oe_error_code = ei_decode_port(~s, oe_size_count_index, 0))"
		 " < 0)\n",
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"erlang_ref" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_ref);\n\n", []),
	    emit(Fd, "  if ((oe_error_code = ei_decode_ref(~s, oe_size_count_index, 0))"
		 " < 0)\n",
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");
	"ETERM*" ->
	    emit(Fd, "  oe_malloc_size += sizeof(char*);\n\n", []),
	    emit(Fd, "  if ((oe_error_code = ei_decode_term(~s, oe_size_count_index, 0))"
		 " < 0)\n",
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");
	{enum, FSN} ->
	    gen_malloc_size_calculation(G, N, Fd, FSN, InBuffer, Align, CalcType);
	FSN ->
	    %%io:format("gen_malloc_size_calculation: ~p ~p~n",[FSN, CalcType]),
	    gen_malloc_size_calculation(G, N, Fd, FSN, InBuffer, Align, CalcType)
    end;
gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, CalcType)  when list(T) -> %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    case Type of
		ushort -> 
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, oe_size_count_index, 0)) < 0)\n", 
			 [InBuffer]);
		ulong -> 
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, oe_size_count_index, 0)) < 0)\n", 
			 [InBuffer]);
		ulonglong -> 
		    emit(Fd, "    if ((oe_error_code = oe_ei_decode_ulonglong(~s, oe_size_count_index, 0)) < 0)\n", 
			 [InBuffer]);
		short ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		long ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		longlong ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_decode_longlong(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		float ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		double ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		boolean ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		char ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		wchar ->  %% WCHAR
		    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wchar(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer]);
		octet ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(~s, oe_size_count_index, 0)) < 0)\n",
			 [InBuffer])
	    end,
	    emit(Fd, "      return oe_error_code;\n\n");
	false ->
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), T]),
		    emit(Fd, "      return oe_error_code;\n\n");
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, &oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), T]),
		    emit(Fd, "      return oe_error_code;\n\n")
	    end
    end;
gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, CalcType)  when record(T, string) ->
    Tname = gen_variable_name(op_variable_count),
    store_tmp_decl("    int ~s = 0;\n",[Tname]),
    
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, oe_size_count_index, &oe_type, &~s)) < 0)\n",
		 [InBuffer, Tname]);
	_ ->
	    emit(Fd, "    int oe_type = 0;\n"),
	    emit(Fd, "    int oe_temp = 0;\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, &oe_size_count_index, &oe_type, &oe_temp)) < 0)\n",
		 [InBuffer])
    end,

    emit(Fd, "      return oe_error_code;\n\n"),

    if
	T#string.length == 0 ->
	    ok;
	true ->
	    Length = ic_util:eval_c(G, N, T#string.length), 
	    case CalcType of
		generator ->
		    emit(Fd, "  if (~s > ~s)\n",[Tname, Length]),
		    emit(Fd, "    return -1;\n\n");
		 _ ->
		    emit(Fd, "  if (oe_temp > ~s)\n",[Length]),
		    emit(Fd, "    return -1;\n\n")
	    end		    
    end,
    
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, oe_size_count_index, 0)) < 0)\n", [InBuffer]);
	_ ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, &oe_size_count_index, 0)) < 0)\n", [InBuffer])
    end,

    emit(Fd, "      return oe_error_code;\n\n"),
    
    case CalcType of
	generator ->
	    emit(Fd, "    oe_malloc_size = ~s;\n\n", 
		 [icgen:mk_align("oe_malloc_size + " ++ Tname ++"+1")]);
	_ ->
	    emit(Fd, "    oe_malloc_size = ~s;\n\n", 
		 [icgen:mk_align("oe_malloc_size + oe_temp+1")])
    end;

gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, CalcType)  when record(T, wstring) ->  %% WSTRING
    Tname = gen_variable_name(op_variable_count),
    store_tmp_decl("    int ~s = 0;\n",[Tname]),
    
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, oe_size_count_index, &oe_type, &~s)) < 0)\n",
		 [InBuffer, Tname]);
	_ ->
	    emit(Fd, "    int oe_type = 0;\n"),
	    emit(Fd, "    int oe_temp = 0;\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, &oe_size_count_index, &oe_type, &oe_temp)) < 0)\n",
		 [InBuffer])
    end,

    emit(Fd, "      return oe_error_code;\n\n"),

    if
	T#wstring.length == 0 ->
	    ok;
	true ->
	    Length = ic_util:eval_c(G, N, T#wstring.length), 
	    case CalcType of
		generator ->
		    emit(Fd, "  if (~s > ~s)\n",[Tname, Length]),
		    emit(Fd, "    return -1;\n\n");
		 _ ->
		    emit(Fd, "  if (oe_temp > ~s)\n",[Length]),
		    emit(Fd, "    return -1;\n\n")
	    end		    
    end,
    
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, oe_size_count_index, 0)) < 0)\n", [InBuffer]);
	_ ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, &oe_size_count_index, 0)) < 0)\n", [InBuffer])
    end,

    emit(Fd, "      return oe_error_code;\n\n"),
    
    case CalcType of
	generator ->
	    emit(Fd, "    oe_malloc_size =\n      ~s;\n\n", 
		 [icgen:mk_align("oe_malloc_size + ((" ++ Tname ++"+ 1) * __OE_WCHAR_SIZE_OF__)")]);
	_ ->
	    emit(Fd, "    oe_malloc_size =\n      ~s;\n\n", 
		 [icgen:mk_align("oe_malloc_size + ((oe_temp + 1) * __OE_WCHAR_SIZE_OF__)")])
    end;
 

gen_malloc_size_calculation(G, N, Fd, T, InBuffer, Align, CalcType) ->
    emit_size(G, N, Fd, T, Align),
    case T of
	{unsigned, {short, _}} -> 
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, oe_size_count_index, 0)) < 0)\n", 
		 [InBuffer]);
	{unsigned, {long, _}} -> 
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, oe_size_count_index, 0)) < 0)\n", 
		 [InBuffer]);
	{unsigned, {'long long', _}} -> 
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_ulonglong(~s, oe_size_count_index, 0)) < 0)\n", 
		 [InBuffer]);
	{short, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{long, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{'long long', _} ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_longlong(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{float,_} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{double, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{boolean, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{char, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{wchar, _} ->  %% WCHAR
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wchar(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{octet, _} ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(~s, oe_size_count_index, 0)) < 0)\n",
		 [InBuffer]);
	{void, _} ->
	    ok;
	{sequence, _, _} ->
	    ok;
	{_, {array, SId, _}} ->
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), get_id2(SId)]);
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, &oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), get_id2(SId)])
	    end;
	{union, UId, _, _, _} ->                                                        
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), get_id2(UId)]);
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, &oe_size_count_index, &oe_malloc_size)) < 0)\n",
			 [icgen:mk_oe_name(G, "sizecalc_"), get_id2(UId)])
	    end;
	_ ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end,
    emit(Fd, "      return oe_error_code;\n\n").



emit_size(G, N, Fd, T, 0) ->
    emit(Fd, "  oe_malloc_size += sizeof(~s);\n\n", [gen_cc_type(G, N, T)]);
emit_size(G, N, Fd, T, _) ->
    ok.



    
check_dynamic_size_rec([]) ->
    false;
check_dynamic_size_rec([{N, T} |Es]) ->
    case check_dynamic_size(T) of
	true ->
	    true;
	false ->
	    check_dynamic_size_rec(Es)
    end;
check_dynamic_size_rec([{_, N, T} |Es]) -> 
    case check_dynamic_size(T) of
	true ->
	    true;
	false ->
	    check_dynamic_size_rec(Es)
    end.



check_dynamic_size({'tk_struct', IFRId, "port", ElementList}) ->
    false;
check_dynamic_size({'tk_struct', IFRId, "pid", ElementList}) ->
    false;
check_dynamic_size({'tk_struct', IFRId, "ref", ElementList}) ->
    false;
check_dynamic_size({'tk_struct', IFRId, "term", ElementList}) ->
    false;
check_dynamic_size({'tk_struct', IFRId, Name, ElementList}) ->
    check_dynamic_size_rec(ElementList);
check_dynamic_size({'tk_array', ElemTC, Length}) ->
    check_dynamic_size(ElemTC);
check_dynamic_size({'tk_string', _}) -> 
    true;
check_dynamic_size({'tk_wstring', _}) ->  %% WSTRING 
    true;
check_dynamic_size({'tk_sequence', ElemTC, MaxLsextractength}) ->
    true;
check_dynamic_size({'tk_union', IFRId, Name, _, _, ElementList}) ->
    check_dynamic_size_rec(ElementList);
check_dynamic_size(Other) ->
    false.


check_dynamic_size(G, N, T)  when record(T, string) ->
    true;
check_dynamic_size(G, N, T)  when record(T, wstring) ->  %% WSTRING
    true;
check_dynamic_size(G, N, T)  when record(T, sequence) ->
    true;
check_dynamic_size(G, N, T)  when record(T, union) ->
    %%io:format("~n~p = ~p~n",[icgen:get_id2(T),ictype:fetchTk(G, N, T)]),
    check_dynamic_size(ictype:fetchTk(G, N, T));
check_dynamic_size(G, N, T)  when record(T, struct) ->
    check_dynamic_size(ictype:fetchTk(G, N, T));
check_dynamic_size(G, N, T) when element(1, T) == scoped_id ->
    case icgen:get_full_scoped_name(G, N, T) of
	{FullScopedName, _, TK, _} ->
	    check_dynamic_size(TK);
	_ ->
	    icgen:fatal_error(G, {name_not_found, T})
    end;
check_dynamic_size(G, N, Other) ->
    false.    



%% mk_dim produces 
mk_dim([]) -> [];
mk_dim([Arg | Args]) ->
    "[" ++ Arg ++ "]" ++ mk_dim(Args).
 
mk_slice_dim(Args) ->
    mk_dim(tl(Args)).




%%%------------------------------------------------------------
%%%
%%% Generates the generic part of c-client. 
%%%
%%%------------------------------------------------------------

emit_client_generic_decoding(G, N, X) ->
    case icgen:is_stubfile_open(G) of
	true ->
	    Fd = icgen:stubfiled(G),
	    
	    Code = 
"
/*
 *  Generic function, used to return received message information.
 *  Not used by oneways. Allways generated. 
 */

int ~s__receive_info(~s oe_obj, CORBA_Environment *oe_env) {

  int oe_error_code = 0;
  int oe_rec_version = 0;
  erlang_ref oe_unq;
  oe_env->_iin = 0;
  oe_env->_received = 0;

  if ((oe_error_code = ei_decode_version(oe_env->_inbuf, &oe_env->_iin, &oe_rec_version)) < 0)
    return oe_error_code;

  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received)) < 0)
    return oe_error_code;

  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, &oe_env->_iin, &oe_unq)) < 0)
    return oe_error_code;

  /* Checking message reference*/
  return ic_compare_refs(&oe_env->_unique,&oe_unq);   

}


",
       
        IName = icgen:to_undersc(N),
        emit(Fd,Code,[IName,IName]);

     false ->
         ok
end.





gen_sync_client_func(G, N, X, Name, ArgNames, TypeList) ->
    case icgen:is_stubfile_open(G) of
	true ->
	    Fd = icgen:stubfiled(G),
	    IName = icgen:to_undersc(N),
	    {R, ParameterTypes, _} = TypeList,
       

	    emit(Fd,"\n/*\n *  Object interface function \"~s\"\n */\n\n",[Name]),

	    RV = element(1, R),

    	    case is_oneway(X) of 
		false ->
		    if RV /= 'void' ->
			    emit(Fd, "~s ~s(~s, ~s) {\n\n",
				 [gen_ret_type(G, N, R),
				  Name,
				  mk_list([IName ++ " oe_obj" |
					   gen_par_list(G, N, X, ParameterTypes,ArgNames)]),
				  "CORBA_Environment *oe_env"]);
		       true ->
			    emit(Fd, "void ~s(~s, ~s) {\n\n",
				 [Name,
				  mk_list([IName ++ " oe_obj" |
					   gen_par_list(G, N, X, ParameterTypes, ArgNames)]),
				  "CORBA_Environment *oe_env"])
		    end;
		
		true ->
		    emit(Fd, "void ~s(~s, ~s) {\n\n",
			 [Name,
			  mk_list([IName ++ " oe_obj" |
				   gen_par_list(G, N, X, ParameterTypes,
						ArgNames)]),
			  "CORBA_Environment *oe_env"])
	    end,

	    emit_stub_func_decl(G, Fd, N, X, R), 
	    emit_ref_init(G, Fd, X),

	    emit(Fd,"  /* Initiating exception indicator */ \n"),
	    emit(Fd,"  oe_env->_major = CORBA_NO_EXCEPTION;\n\n"),

	    emit(Fd,"  /* Creating call message */ \n"),
	    case  mk_list(gen_par_list_for_client_enc_func_call(G, N, X, ParameterTypes,ArgNames)) of
		"" ->
		    case is_oneway(X) of
			true ->
			    emit(Fd,"  if (~s__client_enc(oe_obj, oe_env) < 0) {\n",
				 [Name]);
			false ->
			    emit(Fd,"  if (~s__client_enc(oe_obj, oe_env) < 0) {\n",
				 [Name])
		    end;
		PLFCEC ->
		    case is_oneway(X) of
			true ->
			    emit(Fd,"  if (~s__client_enc(oe_obj, ~s, oe_env) < 0) {\n",
				 [Name,PLFCEC]);
			false ->
			    emit(Fd,"  if (~s__client_enc(oe_obj, ~s, oe_env) < 0) {\n",
				 [Name,PLFCEC])
		    end
	    end, 
	    emit(Fd,"    if (oe_env->_major == CORBA_NO_EXCEPTION)\n"),
	    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Cannot encode message\");\n"),

	    if RV /= void ->
		    emit(Fd, "    return oe_result;\n");
	       true ->
		    true
	    end,
	    
	    emit(Fd,"  }\n\n"),

	    emit(Fd,"  /* Sending call request */ \n"),

	    if RV /= void ->
		    emit(Fd,"  if (strlen(oe_env->_regname) == 0) {\n"),
		    emit(Fd,"    if (ei_send_encoded(oe_env->_fd, oe_env->_to_pid, oe_env->_outbuf, oe_env->_iout) < 0) {\n"), 
		    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, \"Cannot connect to server\");\n"),
		    emit(Fd,"      return oe_result;\n"),
		    emit(Fd,"    }\n"),
		    emit(Fd,"  }\n"),
		    emit(Fd,"  else if (ei_send_reg_encoded(oe_env->_fd, oe_env->_from_pid, oe_env->_regname, oe_env->_outbuf, oe_env->_iout) < 0) {\n"), 
		    emit(Fd,"    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, \"Cannot connect to server\");\n"),
		    emit(Fd,"    return oe_result;\n"),
		    emit(Fd,"  }\n\n");
	       true ->
		    emit(Fd,"  if (oe_env->_major == CORBA_NO_EXCEPTION) {\n"),
		    emit(Fd,"    if (strlen(oe_env->_regname) == 0) {\n"),
		    emit(Fd,"      if (ei_send_encoded(oe_env->_fd, oe_env->_to_pid, oe_env->_outbuf, oe_env->_iout) < 0) {\n"), 
		    emit(Fd,"        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, \"Cannot connect to server\");\n"),
		    emit(Fd,"      }\n"),
		    emit(Fd,"    }\n"),
		    emit(Fd,"    else if (ei_send_reg_encoded(oe_env->_fd, oe_env->_from_pid, oe_env->_regname, oe_env->_outbuf, oe_env->_iout) < 0) {\n"), 
		    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, \"Cannot connect to server\");\n"),
		    emit(Fd,"    }\n"),
		    emit(Fd,"  }\n\n")
	    end,
	    
	    case is_oneway(X) of
		true ->
		    emit(Fd,"}\n\n");
		false ->
		    emit(Fd,"  /* Receiving reply message */\n"),
		    if RV /= void ->
			    emit(Fd,"  do {\n"),
			    emit(Fd,"    if ((oe_msgType = ei_receive_encoded(oe_env->_fd, &oe_env->_inbuf, &oe_env->_inbufsz, &oe_msg, &oe_env->_iin)) < 0) {\n"),
			    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Cannot decode message\");\n"),
			    emit(Fd,"      return oe_result;\n"),
			    emit(Fd,"    }\n"),
			    emit(Fd,"  } while (oe_msgType != ERL_SEND && oe_msgType != ERL_REG_SEND);\n\n"),

			    emit(Fd,"  /* Extracting message header */ \n"),
			    emit(Fd,"  if (~s__receive_info(oe_obj, oe_env) < 0) {\n",[IName]),
			    emit(Fd,"    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad message\");\n"),
			    emit(Fd,"    return oe_result;\n"),
			    emit(Fd,"  }\n\n");
		       true ->
			    emit(Fd,"  if (oe_env->_major == CORBA_NO_EXCEPTION)\n"),
			    emit(Fd,"    do {\n"),
			    emit(Fd,"      if ((oe_msgType = ei_receive_encoded(oe_env->_fd, &oe_env->_inbuf, &oe_env->_inbufsz, &oe_msg, &oe_env->_iin)) < 0) {\n"),
			    emit(Fd,"        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Cannot decode message\");\n"),
			    emit(Fd,"        break;\n"),
			    emit(Fd,"      }\n"),
			    emit(Fd,"    } while (oe_msgType != ERL_SEND && oe_msgType != ERL_REG_SEND);\n\n"),

			    emit(Fd,"  /* Extracting message header */ \n"),
			    emit(Fd,"  if (oe_env->_major == CORBA_NO_EXCEPTION)\n"),
			    emit(Fd,"    if (~s__receive_info(oe_obj, oe_env) < 0) {\n",[IName]),
			    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad message\");\n"),
			    emit(Fd,"  }\n\n")
		    end,
		    
		    DECPARLIST = case gen_ret_type(G, N, R) of
				     "void" ->
					 gen_par_list_for_client_dec_func_call(G, N, X, ParameterTypes,ArgNames);
				     Else ->
					 ["&oe_result" |
					  gen_par_list_for_client_dec_func_call(G, N, X, ParameterTypes,ArgNames)]
				 end,
		    
		    if RV /= void ->
			    emit(Fd,"  /* Extracting return value(s) */ \n"),
			    emit(Fd,"  if (~s__client_dec(oe_obj, ~s, oe_env) < 0) {\n",[Name, mk_list(DECPARLIST)]),
			    emit(Fd,"    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, DATA_CONVERSION, \"Bad return/out value(s)\");\n"),
			    emit(Fd,"  }\n"),
			    emit(Fd, "\n  return oe_result;\n}\n\n\n");		       
		       true ->
			    case  mk_list(DECPARLIST) of
				"" ->
				    emit(Fd,"  /* Extracting message tail */ \n"),
				    emit(Fd,"  if (oe_env->_major == CORBA_NO_EXCEPTION)\n"),
				    emit(Fd,"    if (~s__client_dec(oe_obj, oe_env) < 0) {\n",
					 [Name]);
				PLFCDC ->
				    emit(Fd,"  /* Extracting return value(s) */ \n"),
				    emit(Fd,"  if (oe_env->_major == CORBA_NO_EXCEPTION)\n"),
				    emit(Fd,"    if (~s__client_dec(oe_obj, ~s, oe_env) < 0) {\n",
					 [Name,PLFCDC])
			    end,
			    emit(Fd,"      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad message tail\");\n"),
			    emit(Fd,"    }\n}\n\n\n")
		    end
	    end,
	    ok;
	
	false -> 
	    ok
    end.




%%-----------------------------------------------------
%% Generates the specific encoder function definition 
%%-----------------------------------------------------
gen_client_enc_func(G, N, X, Name, ArgNames, TypeList)->
    case icgen:is_stubfile_open(G) of
	true ->
	    Fd = icgen:stubfiled(G),
	    IName = icgen:to_undersc(N),
	    {R, ParameterTypes, _} = TypeList,
	    TAlist = gen_type_arg_list(ParameterTypes, ArgNames),

	    emit(Fd,"/*\n *  Encodes the function call for \"~s\"\n */\n\n",[Name]),
	    case  mk_list(gen_par_list_for_client_enc_func(G, N, X, ParameterTypes,ArgNames)) of
		"" ->
		    case is_oneway(X) of
			true ->
			    emit(Fd,"int ~s__client_enc(~s oe_obj, CORBA_Environment *oe_env) {\n\n",[Name,IName]);
			false ->
			    emit(Fd,"int ~s__client_enc(~s oe_obj, CORBA_Environment *oe_env) {\n\n",[Name,IName])
		    end;
		PLFCE ->
		    case is_oneway(X) of
			true ->
			    emit(Fd,"int ~s__client_enc(~s oe_obj, ~s, CORBA_Environment *oe_env) {\n\n",[Name,IName,PLFCE]);
			false ->
			    emit(Fd,"int ~s__client_enc(~s oe_obj, ~s, CORBA_Environment *oe_env) {\n\n",[Name,IName,PLFCE])
		    end
	    end,

	    emit(Fd, "  int oe_error_code = 0;\n  oe_env->_iout = 0;\n\n"),

	    emit_encoding(G, N, Fd, X, lists:filter(fun({_, A, _}) ->
								     case A of
									 out ->
									     false;
									 inout ->
									     false;
									 _ ->
									     true
								     end
							     end,
							     TAlist),
			  is_oneway(X)),
 
	    emit(Fd, "  return 0;\n}\n\n\n"),
	    ok;
	
	false -> 
	    ok
    end.


%%-----------------------------------------------------
%% Generates the specific decoder function definition 
%%-----------------------------------------------------
gen_client_dec_func(G, N, X, Name, ArgNames, TypeList)->
    case is_oneway(X) of
	true ->
	    ok;
	false ->
	    case icgen:is_stubfile_open(G) of
		true ->
		    Fd = icgen:stubfiled(G),
		    IName = icgen:to_undersc(N),
		    {R, ParameterTypes, _} = TypeList,
		    TAlist = gen_type_arg_list(ParameterTypes, ArgNames),

		    
		    emit(Fd,"/*\n *  Decodes the return value for \"~s\"\n */\n\n",[Name]),
		    
		    PARLIST = case gen_ret_type(G, N, R) of
				  "void" ->
				      gen_par_list_for_client_dec_func(G, N, X, ParameterTypes,ArgNames);
				  Else ->    
				      [Else++ "* oe_result"|
				       gen_par_list_for_client_dec_func(G, N, X, ParameterTypes,ArgNames)]
			      end,

		    case  mk_list(PARLIST) of
			"" ->
			    emit(Fd,"int ~s__client_dec(~s oe_obj, CORBA_Environment *oe_env) {\n\n",
				 [Name,IName]);
			PLFCD ->
			    emit(Fd,"int ~s__client_dec(~s oe_obj, ~s, CORBA_Environment *oe_env) {\n\n",
				 [Name,IName,PLFCD])
		    end, 
		    
		    emit(Fd, "  int oe_error_code = 0;\n\n"),
		    
		    %%emit(Fd, "  if (oe_env->_received != ~p)\n",[length(TAlist) + 1]), DOES NOT WORK !
		    %%emit(Fd, "    return -1;\n\n"),
		    
		    emit_decoding(G, N, Fd, R,
				  lists:filter(fun({_, A, _}) ->
						       case A of
							   in ->
							       false;
							   inout ->
							       false;
							   _ ->
							       true
						       end
					       end,
					       TAlist)),
		    
		    emit(Fd, "  return 0;\n}\n\n\n"),
		    ok;
		
		false -> 
		    ok
	    end
    end.

		




%%-------------------------------------------------
%% Generates all the prototypes for the interface
%%-------------------------------------------------
gen_prototypes(G,N,X) ->
    case icgen:is_hrlfile_open(G) of
	true ->
	    HFd = icgen:hrlfiled(G),
	    IName = icgen:to_undersc(N),

	    %% Emit generated function prototypes
	    emit(HFd,"\n/* Composit functions  */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_client_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),

	    %% Emit encoding function prototypes
	    emit(HFd,"\n/* Call message encoders */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_encoder_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),


	    %% Emit generic function prototypes
	    emit(HFd,"\n/* Generic decoder */\n",[]),
	    emit(HFd,"int ~s__receive_info(~s oe_obj, CORBA_Environment *oe_env);\n",[IName,IName]),

	    %% Emit decode function prototypes
	    emit(HFd,"\n/* Return value decoders */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_decoder_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),
	    
	    ok;

	false -> 
	    ok
    end.



%%----------------------------------------------------------
%% Generates the prototype for synchronous client functions 
%%----------------------------------------------------------
gen_client_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,
    IName = icgen:to_undersc(N),
    RT = gen_ret_type(G, N, R),
    PARLIST = gen_par_list_for_client_prototypes(G, N, X, ParameterTypes,ArgNames),

    case mk_list(PARLIST) of
	"" ->
	    case RT of
		"void" ->
		    emit(Fd,"void ~s(~s oe_obj, CORBA_Environment *oe_env);\n",[ScopedName,IName]);
		_ ->
		    emit(Fd,"~s ~s(~s oe_obj, CORBA_Environment *oe_env);\n",[RT,ScopedName,IName])
	    end;
	PLFCP ->
	    case RT of
		"void" ->
		    emit(Fd,"void ~s(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",[ScopedName,IName,PLFCP]);
		_ ->
		    emit(Fd,"~s ~s(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",[RT,ScopedName,IName,PLFCP])
	     end
    end,
    gen_client_prototypes(G, Fd, N, Xs);
gen_client_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_client_prototypes(G, Fd, N, Xs);
gen_client_prototypes(G, Fd, N, [X|Xs]) ->
    gen_client_prototypes(G, Fd, N, Xs);
gen_client_prototypes(G, Fd, N, []) -> ok.




%%---------------------------------------------------------------
%% Generates the prototype for specific client encoder functions 
%%---------------------------------------------------------------
gen_encoder_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,
    IName = icgen:to_undersc(N),

    case  mk_list(gen_par_list_for_client_enc_prototypes(G, N, X, ParameterTypes,ArgNames)) of
	"" ->
	    case is_oneway(X) of
		true ->
		    emit(Fd,"int ~s__client_enc(~s oe_obj, CORBA_Environment *oe_env);\n",[ScopedName,IName]);
		false ->
		    emit(Fd,"int ~s__client_enc(~s oe_obj, CORBA_Environment *oe_env);\n",[ScopedName,IName])
	    end;
	PLFCP ->
	    case is_oneway(X) of
		true ->
		    emit(Fd,"int ~s__client_enc(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
			 [ScopedName,IName,PLFCP]);
		false ->
		    emit(Fd,"int ~s__client_enc(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
			 [ScopedName,IName,PLFCP])
	    end
    end,
    gen_encoder_prototypes(G, Fd, N, Xs);
gen_encoder_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_encoder_prototypes(G, Fd, N, Xs);
gen_encoder_prototypes(G, Fd, N, [X|Xs]) ->
    gen_encoder_prototypes(G, Fd, N, Xs);
gen_encoder_prototypes(G, Fd, N, []) -> ok.




%%---------------------------------------------------------------
%% Generates the prototype for specific client decoder functions 
%%---------------------------------------------------------------
gen_decoder_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    case is_oneway(X) of
	true ->
	    true;
	false ->
	    IName = icgen:to_undersc(N),
	    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
	    {R, ParameterTypes, _} = TypeList,
	    
	    PARLIST = case gen_ret_type(G, N, R) of
			  "void" ->
			      gen_par_list_for_client_dec_prototypes(G, N, X, ParameterTypes,ArgNames);
			  Else ->
			      [Else++"*"|
			       gen_par_list_for_client_dec_prototypes(G, N, X, ParameterTypes,ArgNames)]
		      end,
	    
	    case  mk_list(PARLIST) of
		"" ->
		    emit(Fd,"int ~s__client_dec(~s oe_obj, CORBA_Environment *oe_env);\n",
			 [ScopedName,IName]);
		PLFDP ->
		    emit(Fd,"int ~s__client_dec(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
			 [ScopedName,IName,PLFDP])
	    end
    end,
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, [X|Xs]) ->
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, []) -> ok.






%%---------------------------------------------------------------------
%% Generates a parameter list for specific encoder function definition 
%%---------------------------------------------------------------------
gen_par_list_for_client_enc_func(_, _, _, [], []) ->
    [];
gen_par_list_for_client_enc_func(G, N, X, [Type |Types], [{out, Arg}|Args]) ->
    gen_par_list_for_client_enc_func(G, N, X, Types, Args);
gen_par_list_for_client_enc_func(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_enc_func(G, N, X, Types, Args);

	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    IsStruct = ictype:isStruct(G, N, Type),
	    IsUnion = ictype:isUnion(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  "";
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  if 
			      Attr == in, Ctype == "erlang_pid" ->
				  "*";
			      Attr == in, Ctype == "erlang_port" ->
				  "*";
			      Attr == in, Ctype == "erlang_ref" ->
				  "*";
			      Attr == in, IsStruct == true ->
				  "*";
			      Attr == in, IsUnion == true ->
				  "*";
			       true ->
				  ""
			  end
		   end,
	    [Ctype ++ Dyn ++ RefVal ++ " " ++ Arg |
	     gen_par_list_for_client_enc_func(G, N, X, Types, Args)]
    end.






%%---------------------------------------------------------------------
%% Generates a parameter list for specific decoder function definition 
%%---------------------------------------------------------------------
gen_par_list_for_client_dec_func(_, _, _, [], []) ->
    [];
gen_par_list_for_client_dec_func(G, N, X, [Type |Types], [{in, Arg}|Args]) ->
    gen_par_list_for_client_dec_func(G, N, X, Types, Args);
gen_par_list_for_client_dec_func(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_dec_func(G, N, X, Types, Args);

	RefVal ->
	    IsArray = ictype:isArray(G, N, Type),
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case IsArray of
				      true ->
					  "_slice**";
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  ""
		  end,
	    case IsArray of
		true ->
		    [Ctype ++ Dyn ++ " " ++ Arg | 
		     gen_par_list_for_client_dec_func(G, N, X, Types, Args)];
		false ->
		    [Ctype ++ Dyn ++ RefVal ++ " " ++ Arg | 
		     gen_par_list_for_client_dec_func(G, N, X, Types, Args)]
	    end
    end.



%%----------------------------------------------------------------
%% Generates a parameter list for specific encoder function calls 
%%----------------------------------------------------------------
gen_par_list_for_client_enc_func_call(_, _, _, [], []) ->
    [];
gen_par_list_for_client_enc_func_call(G, N, X, [Type |Types], [{out, Arg}|Args]) ->
    gen_par_list_for_client_enc_func_call(G, N, X, Types, Args);
gen_par_list_for_client_enc_func_call(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_enc_func_call(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    [Arg | 
	     gen_par_list_for_client_enc_func_call(G, N, X, Types, Args)]
    end.


%%----------------------------------------------------------
%% Generates a parameter list for specific decoder function 
%%----------------------------------------------------------
gen_par_list_for_client_dec_func_call(_, _, _, [], []) ->
    [];
gen_par_list_for_client_dec_func_call(G, N, X, [Type |Types], [{in, Arg}|Args]) ->
    gen_par_list_for_client_dec_func_call(G, N, X, Types, Args);
gen_par_list_for_client_dec_func_call(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_dec_func_call(G, N, X, Types, Args);

	RefVal ->
	    [Arg | 
	     gen_par_list_for_client_dec_func_call(G, N, X, Types, Args)]
    end.



%%------------------------------------------------------------------------
%% Generates a parameter list for synchronous client function prototypes 
%%------------------------------------------------------------------------
gen_par_list_for_client_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_client_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_prototypes(G, N, X, Types, Args);

	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    IsArray = ictype:isArray(G, N, Type),
	    IsStruct = ictype:isStruct(G, N, Type),
	    IsUnion = ictype:isUnion(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case IsArray of
				      true ->
					  if Attr == out ->
						  "_slice**";
					     true ->
						  ""
					  end;
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  if 
			      Attr == in, Ctype == "erlang_pid" ->
				  "*";
			      Attr == in, Ctype == "erlang_port" ->
				  "*";
			      Attr == in, Ctype == "erlang_ref" ->
				  "*";
			      Attr == in, IsStruct == true ->
				  "*";
			      Attr == in, IsUnion == true ->
				  "*";
			      true ->
				  ""
			  end
		  end,
	    
	    if Attr == out, IsArray == true ->
		    [Ctype ++ Dyn | 
		     gen_par_list_for_client_prototypes(G, N, X, Types, Args)];
	       true ->
		    [Ctype ++ Dyn ++ RefVal | 
		     gen_par_list_for_client_prototypes(G, N, X, Types, Args)]
	    end
    end.



gen_ret_type(G, N, Type) ->
    Ctype = gen_cc_type(G, N, Type),
    Dyn = case check_dynamic_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  %% WSTRING
			  "";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "_slice*";
			      false ->
				  "*"
			  end
		  end;
	      false ->
		  case ictype:isArray(G, N, Type) of
		      true ->
			  "_slice*";
		      false ->
			  ""
		  end
	  end,
    Ctype ++ Dyn.



%%---------------------------------------------------------------------
%% Generates a parameter list for specific encoder function prototype 
%%---------------------------------------------------------------------
gen_par_list_for_client_enc_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_client_enc_prototypes(G, N, X, [Type |Types], [{out, Arg}|Args]) ->
    gen_par_list_for_client_enc_prototypes(G, N, X, Types, Args);
gen_par_list_for_client_enc_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_enc_prototypes(G, N, X, Types, Args);
	
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    IsStruct = ictype:isStruct(G, N, Type),
	    IsUnion = ictype:isUnion(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  "";
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  if 
			      Attr == in, Ctype == "erlang_pid" ->
				  "*";
			      Attr == in, Ctype == "erlang_port" ->
				  "*";
			      Attr == in, Ctype == "erlang_ref" ->
				  "*";
			      Attr == in, IsStruct == true ->
				  "*";
			      Attr == in, IsUnion == true ->
				  "*";
			      true ->
				  ""
			  end
		  end,
	    [Ctype++ Dyn ++ RefVal | gen_par_list_for_client_enc_prototypes(G, N, X, Types, Args)]
    end.







%%---------------------------------------------------------------------
%% Generates a parameter list for specific decoder function prototype 
%%---------------------------------------------------------------------
gen_par_list_for_client_dec_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_client_dec_prototypes(G, N, X, [Type |Types], [{in, Arg}|Args]) ->
    gen_par_list_for_client_dec_prototypes(G, N, X, Types, Args);
gen_par_list_for_client_dec_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_client_dec_prototypes(G, N, X, Types, Args);

	RefVal ->
	    IsArray = ictype:isArray(G, N, Type),
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  "_slice**";
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  ""
		  end,
	    case IsArray of
		true ->
		    [Ctype ++ Dyn | 
		     gen_par_list_for_client_dec_prototypes(G, N, X, Types, Args)];
		false ->
		    [Ctype ++ Dyn ++ RefVal | 
		     gen_par_list_for_client_dec_prototypes(G, N, X, Types, Args)]
	    end
    end.


















