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
-module(ic_cclient).

%% This module implements generation of C client code, where the
%% client acts as an Erlang C-node, and where the communication thus
%% is according to the Erlang distribution protocol.
%%
%% TODO:
%% 1. ic_cbe:extract_info/3 is a silly name. 
%% 2. ParList, ParTypes, TypeList etc. what are they?
%% 3. Zip is merging two lists (of the same length).
%%

-export([do_gen/3]).

%%------------------------------------------------------------
%% IMPLEMENTATION CONVENTIONS
%%------------------------------------------------------------
%% Functions:
%%
%% mk_*       returns things to be used. No side effects.
%% emit_*     Writes to file. Has Fd in arguments.
%% gen_*      Same, but has no Fd. Usually for larger things.
%%
%% Terminology for generating C:
%%
%% par_list   list of identifiers with types, types only, or with 
%%            parameters (arguments) only. 
%% arg_list   list of identifiers only (for function calls)
%%

%%------------------------------------------------------------
%% Internal stuff
%%------------------------------------------------------------

-import(lists, [foreach/2, foldl/3, foldr/3]).
-import(ic_codegen, [emit/2, emit/3]).

-include("icforms.hrl").
-include("ic.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(IC_HEADER, "ic.h").
-define(ERL_INTERFACEHEADER, "erl_interface.h").
-define(EICONVHEADER, "ei.h").
-define(ERLANGATOMLENGTH, "256").

%%------------------------------------------------------------
%% ENTRY POINT
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    OeName = ic_util:mk_oe_name(G, remove_ext(ic_util:to_list(File))), 
    G2 = ic_file:filename_push(G, [], OeName, c), 
    gen_headers(G2, [], Form), 
    R = gen(G2, [], Form), 
    ic_file:filename_pop(G2, c), 
    R.

remove_ext(File) ->
    filename:rootname(filename:basename(File)).

%%------------------------------------------------------------
%%
%% Generate client side C stubs. 
%%
%% - each module definition results in a separate file.
%% - each interface definition results in a separate file.
%%
%% XXX Constructed types get their own files. True? 
%%
%%------------------------------------------------------------

gen(G, N, [X| Xs]) when record(X, preproc) ->
    G1 = change_file_stack(G, N, X), 
    gen(G1, N, Xs);

gen(G, N, [X| Xs]) when record(X, module) ->
    CD = ic_code:codeDirective(G, X), 
    G2 = ic_file:filename_push(G, N, X, CD), 
    N2 = [ic_forms:get_id2(X)| N], 
    gen_headers(G2, N2, X), 
    gen(G2, N2, ic_forms:get_body(X)), 
    G3 = ic_file:filename_pop(G2, CD), 
    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when record(X, interface) ->

    G2 = ic_file:filename_push(G, N, X, c), 
    N2 = [ic_forms:get_id2(X)| N], 

    %% Sets the temporary variable counter.
    put(op_variable_count, 0), 
    put(tmp_declarations, []), 

    gen_headers(G2, N2, X), 

    gen(G2, N2, ic_forms:get_body(X)), 

    lists:foreach(
      fun({Name, Body}) -> 
	      gen(G2, N2, Body) end, 
      X#interface.inherit_body), 

    %% Generate Prototypes
    gen_prototypes(G2, N2, X), 

    %% Generate generic receive
    gen_receive_info(G2, N2, X), 

    G3 = ic_file:filename_pop(G2, c), 

    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when record(X, const) ->
    emit_constant(G, N, X), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, op) ->
    {Name, ArgNames, RetParTypes} = ic_cbe:extract_info(G, N, X), 
    gen_operation(G, N, X, Name, ArgNames, RetParTypes), 
    gen_encoder(G, N, X, Name, ArgNames, RetParTypes), 
    gen_decoder(G, N, X, Name, ArgNames, RetParTypes), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, attr) ->
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, except) ->
    icstruct:except_gen(G, N, X, c), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, enum) ->
    icenum:enum_gen(G, N, X, c), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, typedef) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, struct) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when record(X, union) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X| Xs]) ->
    %% XXX Should have debug message here.
    gen(G, N, Xs);

gen(G, N, []) -> 
    ok.

%%------------------------------------------------------------
%% Change file stack
%%------------------------------------------------------------

change_file_stack(G, N, X) when X#preproc.cat == line_nr ->
    Id = ic_forms:get_id2(X), 
    Flags = X#preproc.aux, 
    case Flags of
	[] -> 
	    ic_genobj:push_file(G, Id);
	_ ->
	    foldr(
	      fun({_, _, "1"}, G1) -> 
		      ic_genobj:push_file(G1, Id);
		 ({_, _, "2"}, G1) -> 
		      ic_genobj:pop_file(G1, Id);
		 ({_, _, "3"}, G1) -> 
		      ic_genobj:sys_file(G1, Id) 
	      end, G, Flags)
    end;
change_file_stack(G, N, X) ->
    G.

%%------------------------------------------------------------
%% Generate headers in stubfiles and header files 
%%------------------------------------------------------------

gen_headers(G, N, X) when record(X, interface) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    %% Set the temporary variable counter
	    put(op_variable_count, 0), 
	    put(tmp_declarations, []), 
	    HFd = ic_genobj:hrlfiled(G), 
	    IncludeFileStack = ic_genobj:include_file_stack(G), 
	    L = length(N), 
	    Filename =
		if
		    L < 2 ->
			lists:nth(L + 1, IncludeFileStack);
		    true ->
			lists:nth(2, IncludeFileStack)
		end, 
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_client), 

	    IName = ic_util:to_undersc(N), 
	    emit(HFd, "\n#ifndef __~s__\n", [ic_util:to_uppercase(IName)]), 
	    emit(HFd, "#define __~s__\n", 
		 [ic_util:to_uppercase(IName)]), 
	    LCmt = io_lib:format("Interface object definition: ~s", [IName]), 
	    ic_codegen:mcomment_light(HFd, [LCmt], c), 
	    emit(HFd, "typedef CORBA_Object ~s;\n", [IName]), 
	    emit(HFd, "#endif\n\n");

	false -> ok
    end, 
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    ic_codegen:nl(Fd), 
	    emit(Fd, "#include <stdlib.h>\n"), 
	    emit(Fd, "#include <string.h>\n"), 
	    emit(Fd, "#include \"~s\"\n", [?IC_HEADER]), 
	    emit(Fd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]), 
	    emit(Fd, "#include \"~s\"\n", [?EICONVHEADER]), 
	    emit(Fd, "#include \"~s\"\n", 
		 [filename:basename(ic_genobj:include_file(G))]), 
	    ic_codegen:nl(Fd), ic_codegen:nl(Fd), 
	    Fd;
	false ->
	    ok
    end;

%% Some items have extra includes
gen_headers(G, N, X) when record(X, module) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    IncludeFileStack = ic_genobj:include_file_stack(G), 
	    Filename = lists:nth(length(N) + 1, IncludeFileStack), 
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_client);
	false -> ok
    end;
gen_headers(G, [], X) -> 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    emit(HFd, "#include \"~s\"\n", [?IC_HEADER]), 
	    emit(HFd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]), 
	    emit(HFd, "#include \"~s\"\n", [?EICONVHEADER]), 
	    ic_code:gen_includes(HFd, G, c_client);
	false -> ok
    end;
gen_headers(G, N, X) -> 
    ok.


%%------------------------------------------------------------
%% Generate all prototypes (for interface)
%%------------------------------------------------------------
gen_prototypes(G, N, X) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    IName = ic_util:to_undersc(N), 

	    %% Emit generated function prototypes
	    emit(HFd, "\n/* Operation functions  */\n"), 
	    lists:foreach(fun({Name, Body}) ->
				  emit_operation_prototypes(G, HFd, N, Body)
			  end, [{x, ic_forms:get_body(X)}| 
				X#interface.inherit_body]), 

	    %% Emit encoding function prototypes
	    emit(HFd, "\n/* Input encoders */\n"), 
	    lists:foreach(fun({Name, Body}) ->
				  emit_encoder_prototypes(G, HFd, N, Body) 
			  end, 
			  [{x, ic_forms:get_body(X)}| 
			   X#interface.inherit_body]), 

	    %% Emit generic function prototype
	    emit(HFd, "\n/* Generic decoder */\n"), 
	    emit(HFd, "int ~s__receive_info(~s, CORBA_Environment*);\n", 
		 [IName, IName]), 
	    %% Emit decode function prototypes
	    emit(HFd, "\n/* Result decoders */\n"), 
	    lists:foreach(fun({Name, Body}) ->
				  emit_decoder_prototypes(G, HFd, N, Body) 
			  end, [{x, ic_forms:get_body(X)}| 
				X#interface.inherit_body]), 
	    ok;

	false -> 
	    ok
    end.


%%------------------------------------------------------------
%% Generate receive_info (generic part for message reception) 
%% (for interface).
%%------------------------------------------------------------

gen_receive_info(G, N, X) ->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 

	    Code = 
		"
/*
 *  Generic function, used to return received message information.
 *  Not used by oneways. Always generated. 
 */

int ~s__receive_info(~s oe_obj, CORBA_Environment *oe_env)
{
    int oe_error_code = 0;
    int oe_rec_version = 0;
    erlang_ref oe_unq;
    oe_env->_iin = 0;
    oe_env->_received = 0;

    if ((oe_error_code = ei_decode_version(oe_env->_inbuf, "
"&oe_env->_iin, &oe_rec_version)) < 0)
        return oe_error_code;

    if ((oe_error_code = ei_decode_tuple_header("
"oe_env->_inbuf, &oe_env->_iin, &oe_env->_received)) < 0)
        return oe_error_code;

    if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, "
"&oe_env->_iin, &oe_unq)) < 0)
        return oe_error_code;
    /* Checking message reference*/
    return ic_compare_refs(&oe_env->_unique, &oe_unq);
}   
", 
        IName = ic_util:to_undersc(N), 
        emit(Fd, Code, [IName, IName]);

     false ->
         ok
end.


%%------------------------------------------------------------
%% Emit constant
%%------------------------------------------------------------

emit_constant(G, N, ConstRecord) ->
    case ic_genobj:is_hrlfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:hrlfiled(G), 
	    CName = ic_util:to_undersc(
		      [ic_forms:get_id(ConstRecord#const.id)| N]), 
	    UCName = ic_util:to_uppercase(CName), 

	    emit(Fd, "\n#ifndef __~s__\n", [UCName]), 
	    emit(Fd, "#define __~s__\n", [UCName]), 

	    emit(Fd, "/* Constant: ~s */\n", [CName]), 

	    if record(ConstRecord#const.type, wstring) -> 
		    %% If wstring, add 'L' 
		    emit(Fd, "#define ~s L~p\n", 
			 [CName, ConstRecord#const.val]);
	       true ->
		    emit(Fd, "#define ~s ~p\n", 
			 [CName, ConstRecord#const.val])
	    end, 
	    emit(Fd, "#endif\n\n")
    end.

%%------------------------------------------------------------
%% Generate operation (for interface)
%%------------------------------------------------------------

gen_operation(G, N, X, Name, ArgNames, RetParTypes) ->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    IName = ic_util:to_undersc(N), 
	    {R, ParTypes, _} = RetParTypes, 

	    emit(Fd, "\n/*\n *  Operation function \"~s\"\n */\n\n", 
		 [Name]), 
	    RV = element(1, R), 
	    Ret = case ic_forms:is_oneway(X) of 
		      false ->
			  if RV /= 'void' -> 
				  mk_ret_type(G, N, R);
			     true -> 
				  "void"
			  end;
		      true ->
			  "void"
		  end, 
	    ParList = ic_util:chain(
			mk_par_type_list(G, N, X, [in, out], [types, args], 
					 ParTypes, ArgNames), ", "),
	    emit(Fd, 
		 "~s ~s(~s, ~sCORBA_Environment *oe_env)\n{\n",
		 [Ret, Name, [IName, " ", "oe_obj"], ParList]), 

	    case ic_forms:is_oneway(X) of
		true ->
		    ok;
		false ->
		    case ictype:isArray(G, N, R) of
			true ->
			    emit(Fd, "  ~s oe_return = NULL;\n", 
				 [mk_ret_type(G, N, R)]);
			false ->
			    if RV /= 'void' ->
				    emit(Fd, "  ~s oe_return;\n", 
					 [Ret]);
			       true ->
				    ok
			    end
		    end, 
		    emit(Fd, 
			 "  int oe_msgType = 0;\n"
			 "  erlang_msg oe_msg;\n"
			 "\n"
			 "  /* Initiating the message reference */\n" 
			 "  ic_init_ref(oe_env, &oe_env->_unique);\n\n")
	    end,

	    emit(Fd, 
		 "  /* Initiating exception indicator */ \n"
		 "  oe_env->_major = CORBA_NO_EXCEPTION;\n"
		 "\n"),

	    %% XXX Add pointer checks: checks of in-parameter
	    %% pointers, and non-variable out-parameter pointers.

	    emit(Fd,
		 "  /* Creating call message */ \n"), 
	    GenParList = 
		ic_util:chain(
		  mk_arg_list_for_encoder(G, N, X, ParTypes, ArgNames), 
		  ", "),
	    emit(Fd, 
		 "  if (~s__client_enc(oe_obj, ~s""oe_env) < 0) {\n", 
		 [Name, GenParList]),
	    emit(Fd, 
		 "    if (oe_env->_major == CORBA_NO_EXCEPTION)\n" 
		 "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "MARSHAL, \"Cannot encode message\");\n"), 

	    if RV /= void ->
		    emit(Fd, "    return oe_return;\n"
			 "  }\n");
	       true ->
		    emit(Fd, "  }\n")
	    end, 

	    emit(Fd, "  /* Sending call request */ \n"), 

	    if RV /= void ->
		    emit(Fd, 
			 "  if (strlen(oe_env->_regname) == 0) {\n" 
			 "    if (ei_send_encoded(oe_env->_fd, "
			 "oe_env->_to_pid, oe_env->_outbuf, "
			 "oe_env->_iout) < 0) {\n" 
			 "      CORBA_exc_set(oe_env, "
			 "CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, "
			 "\"Cannot connect to server\");\n" 
			 "      return oe_return;\n" 
			 "    }\n"
			 "  }\n" 
			 "  else if (ei_send_reg_encoded(oe_env->_fd, "
			 "oe_env->_from_pid, oe_env->_regname, "
			 "oe_env->_outbuf, oe_env->_iout) < 0) {\n"
			 "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
			 " NO_RESPONSE, \"Cannot connect to server\");\n" 
			 "    return oe_return;\n" 
			 "  }\n");
	       true ->
		    emit(Fd, 
			 "  if (oe_env->_major == CORBA_NO_EXCEPTION) {\n" 
			 "    if (strlen(oe_env->_regname) == 0) {\n" 
			 "      if (ei_send_encoded(oe_env->_fd, "
			 "oe_env->_to_pid, oe_env->_outbuf, "
			 "oe_env->_iout) < 0) {\n" 
			 "        CORBA_exc_set(oe_env, "
			 "CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, "
			 "\"Cannot connect to server\");\n" 
			 "      }\n"
			 "    }\n" 
			 "    else if (ei_send_reg_encoded(oe_env->_fd, "
			 "oe_env->_from_pid, oe_env->_regname, "
			 "oe_env->_outbuf, oe_env->_iout) < 0) {\n" 
			 "      CORBA_exc_set(oe_env, "
			 "CORBA_SYSTEM_EXCEPTION, NO_RESPONSE, "
			 "\"Cannot connect to server\");\n" 
			 "    }\n" 
			 "  }\n")
	    end, 
	    case ic_forms:is_oneway(X) of
		true ->
		    emit(Fd, "}\n");
		false ->
		    emit(Fd, "  /* Receiving reply message */\n"), 
		    if RV /= void ->
			    emit(Fd, 
				 "  do {\n"
				 "    if ((oe_msgType = ei_receive_encoded("
				 "oe_env->_fd, "
				 "&oe_env->_inbuf, &oe_env->_inbufsz, "
				 "&oe_msg, &oe_env->_iin)) < 0) {\n" 
			         "CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, MARSHAL, "
				 "\"Cannot decode message\");\n" 
				 "      return oe_return;\n" 
				 "    }\n" 
				 "  } while (oe_msgType != ERL_SEND "
				 "&& oe_msgType != ERL_REG_SEND);\n" 

				 "  /* Extracting message header */ \n"), 
			    emit(Fd, 
				 "  if (~s__receive_info(oe_obj, "
				 "oe_env) < 0) {\n", [IName]), 
			    emit(Fd, 
				 "    CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, MARSHAL, "
				 "\"Bad message\");\n" 
				 "    return oe_return;\n" 
				 "  }\n\n");
		       true ->
			    emit(Fd, 
				 "  if (oe_env->_major == "
				 "CORBA_NO_EXCEPTION)\n" 
				 "    do {\n" 
				 "      if ((oe_msgType = "
				 "ei_receive_encoded(oe_env->_fd, "
				 "&oe_env->_inbuf, &oe_env->_inbufsz, "
				 "&oe_msg, &oe_env->_iin)) < 0) {\n" 
				 "        CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, MARSHAL, "
				 "\"Cannot decode message\");\n" 
				 "        break;\n" 
				 "      }\n" 
				 "    } while (oe_msgType != ERL_SEND "
				 "&& oe_msgType != ERL_REG_SEND);\n" 

				 "  /* Extracting message header */ \n" 
				 "  if (oe_env->_major == "
				 "CORBA_NO_EXCEPTION)\n"), 
			    emit(Fd, 
				 "    if (~s__receive_info(oe_obj, "
				 "oe_env) < 0) {\n", [IName]), 
			    emit(Fd, 
				 "      CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, MARSHAL, "
				 "\"Bad message\");\n" 
				 "  }\n")
		    end, 

		    DecParList = mk_arg_list_for_decoder(G, N, X, 
						      ParTypes, 
						      ArgNames), 
		    DECPARLIST = 
			case mk_ret_type(G, N, R) of
			    "void" ->
				DecParList;
			    Else ->
				["&oe_return"| DecParList]
			end, 

		    if RV /= void ->
			    emit(Fd, 
				 "  /* Extracting return value(s) */ \n" 
				 "  if (~s__client_dec(oe_obj, ~s, "
				 "oe_env) < 0) {\n", 
				 [Name, ic_util:join(DECPARLIST, ", ")]), 
			    emit(Fd, 
				 "    if (oe_env->_major == "
				 "CORBA_NO_EXCEPTION)\n" 
				 "      CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, DATA_CONVERSION, "
				 "\"Bad return/out value(s)\");\n" 
				 "  }\n" 
				 "\n  return oe_return;\n}\n");
		       true ->
			    case ic_util:join(DECPARLIST, ", ") of
				"" ->
				    emit(Fd, 
					 "  /* Extracting message tail */ \n" 
					 "  if (oe_env->_major == "
					 "CORBA_NO_EXCEPTION)\n" 
					 "    if (~s__client_dec(oe_obj, "
					 "oe_env) < 0) {\n", 
					 [Name]);
				PLFCDC ->
				    emit(Fd, 
					 "  /* Extracting return value(s) "
					 "*/ \n" 
					 "  if (oe_env->_major == "
					 "CORBA_NO_EXCEPTION)\n" 
					 "    if (~s__client_dec(oe_obj, "
					 "~s, oe_env) < 0) {\n", 
					 [Name, PLFCDC])
			    end, 
			    emit(Fd, 
				 "      if (oe_env->_major == "
				 "CORBA_NO_EXCEPTION)\n" 
				 "        CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, MARSHAL, "
				 "\"Bad message tail\");\n" 
				 "    }\n}\n\n")
		    end
	    end, 
	    ok;

	false -> 
	    ok
    end.

%%------------------------------------------------------------
%% Generate encoder 
%%------------------------------------------------------------
gen_encoder(G, N, X, Name, ArgNames, RetParTypes)->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    IName = ic_util:to_undersc(N), 
	    {R, ParTypes, _} = RetParTypes, 
	    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames), 
	    emit(Fd, "/*\n *  Encode operation input for \"~s\"\n */\n\n", 
		 [Name]), 
	    ParList = ic_util:chain(
			mk_par_type_list(G, N, X, [in], [types, args], 
					 ParTypes, ArgNames), ", "), 
	    emit(Fd, 
		 "int ~s__client_enc(~s oe_obj, ~s"
		 "CORBA_Environment *oe_env)\n{\n", 
		 [Name, IName, ParList]),

	    InTypeAttrArgs = lists:filter(fun({_, in, _}) -> true;
					     ({_, _, _}) -> false
					  end, TypeAttrArgs), 
	    case InTypeAttrArgs of 
		[] ->
		    case ic_forms:is_oneway(X) of
			true ->
			    emit(Fd, 
				 "  oe_env->_iout = 0;\n");
			false ->
			    emit(Fd, 
				 "  int oe_error_code = 0;\n"
				 "  oe_env->_iout = 0;\n")
		    end;
		_ ->
		    emit(Fd, 
			 "  int oe_error_code = 0;\n"
			 "  oe_env->_iout = 0;\n")
	    end, 

	    emit_encodings(G, N, Fd, X, InTypeAttrArgs, 
			   ic_forms:is_oneway(X)), 
 	    emit(Fd, "  return 0;\n}\n\n"), 
	    ok;
	
	false -> 
	    ok
    end.

%%------------------------------------------------------------
%% Generate decoder
%%------------------------------------------------------------
gen_decoder(G, N, X, Name, ArgNames, RetParTypes)->
    case ic_forms:is_oneway(X) of
	true ->
	    ok;
	false ->
	    case ic_genobj:is_stubfile_open(G) of
		true ->
		    Fd = ic_genobj:stubfiled(G), 
		    IName = ic_util:to_undersc(N), 
		    {R, ParTypes, _} = RetParTypes, 
		    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames), 
		    emit(Fd, "/*\n *  Decode operation results for "
			 "\"~s\"\n */\n\n", [Name]), 
		    ParList0 = mk_par_type_list(G, N, X, [out],
						[types, args], 
						ParTypes, ArgNames),
		    PARLIST = case mk_ret_type(G, N, R) of
				  "void" ->
				      ParList0;
				  Else ->    
				      [Else ++ "* oe_return"| ParList0]
			      end, 
		    PLFCD = ic_util:chain(PARLIST, ", "), 
		    emit(Fd, 
			 "int ~s__client_dec(~s oe_obj, ~s"
			 "CORBA_Environment *oe_env)\n{\n", 
			 [Name, IName, PLFCD]),
		    emit(Fd, "  int oe_error_code = 0;\n"), 
		    OutTypeAttrArgs = lists:filter(fun({_, out, _}) -> true;
						      ({_, _, _}) -> false
						   end, TypeAttrArgs), 
		    emit_decodings(G, N, Fd, R, OutTypeAttrArgs),
		    emit(Fd, "  return 0;\n}\n\n"), 
		    ok;
		
		false -> 
		    ok
	    end
    end.

%%------------------------------------------------------------
%% EMIT ENCODINGS/DECODINGS
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Emit encodings
%%------------------------------------------------------------
%% emit_encodings(G, N, Fd, X, TypeAttrArgs, IsOneWay) 
%%
emit_encodings(G, N, Fd, X, TypeAttrArgs, true) ->
    emit(Fd, "  oe_ei_encode_version(oe_env);\n"), 
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 2);\n"), 
    %% Call or Cast
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n", ["$gen_cast"]), 
    emit_encodings_1(G, N, Fd, X, TypeAttrArgs);
emit_encodings(G, N, Fd, X, TypeAttrArgs, false) ->
    emit(Fd, "  oe_ei_encode_version(oe_env);\n"), 
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 3);\n"), 
    %% Call or Cast
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n", ["$gen_call"]), 
    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 2);\n"), 
    %% Unique ref. field
    %% From pid
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, "
	 "oe_env->_from_pid)) < 0)\n"), 
    emit(Fd, "    return oe_error_code;\n"), 
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, "
	 "&oe_env->_unique)) < 0)\n"), 
    emit(Fd, "    return oe_error_code;\n"), 
    emit_encodings_1(G, N, Fd, X, TypeAttrArgs).

emit_encodings_1(G, N, Fd, X, TypeAttrArgs) ->
    {ScopedName, _, _} = ic_cbe:extract_info(G, N, X), 
    Name = case ic_options:get_opt(G, scoped_op_calls) of 
	       true -> 
		   ScopedName;
	       false ->
		   ic_forms:get_id2(X)
	   end, 
    if 
	TypeAttrArgs /= [] -> 
	    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, ~p);\n", 
		 [length(TypeAttrArgs) + 1]); 
	true ->
	    ok
    end,
    emit(Fd, "  oe_ei_encode_atom(oe_env, ~p);\n", [Name]), 

    foreach(fun({{'void', _}, _, _}) ->
		    ok;
		({T1, A1, N1}) ->
		    IndOp  = mk_ind_op(A1), 
		    emit_coding_comment(G, N, Fd, "Encode", IndOp, 
					  T1, N1), 
		    ic_cbe:emit_encoding_stmt(G, N, X, Fd, T1, IndOp ++ N1,
					      "oe_env->_outbuf")
	    end, TypeAttrArgs), 
    ok.

%%------------------------------------------------------------
%% Emit dedodings
%%------------------------------------------------------------
%% XXX Unfortunately we have to retain the silly `oe_first' variable,
%% since its name is hardcoded in other modules (icstruct, icunion,
%% etc).
%%
emit_decodings(G, N, Fd, RetType, TypeAttrArgs) ->
    if 
	TypeAttrArgs /= [] ->
	    %% Only if there are out parameters
	    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header("
		 "oe_env->_inbuf, &oe_env->_iin, "
		 "&oe_env->_received)) < 0)\n"), 
	    emit(Fd, "    return oe_error_code;\n"), 
	    emit(Fd, "  if (oe_env->_received != ~p)\n", 
		 [length(TypeAttrArgs) + 1]), 
	    emit(Fd, "    return -1;\n"); 
	true  ->
	    ok
    end,

    %% Fetch the return value
    emit(Fd, "  /* Decode return value: ~s *oe_return */\n", 
	 [ic_cbe:mk_c_type(G, N, RetType)]), 
    APars =
	case ic_cbe:is_variable_size(G, N, RetType) of
	    true ->
		emit(Fd, 
		     "  {\n"
		     "    int oe_size_count_index = oe_env->_iin;\n"
		     "    int oe_malloc_size = 0;\n"
		     "    void *oe_first = NULL;\n"),
		ic_cbe:emit_malloc_size_stmt(G, N, Fd, RetType, 
					     "oe_env->_inbuf", 
					     1, caller), 
		%% XXX Add malloc prefix from option
		emit(Fd, 
		     "    OE_MALLOC_SIZE_CHECK(oe_env, oe_malloc_size);\n" 
		     "    if ((*oe_return = oe_first = "
		     "malloc(oe_malloc_size)) == NULL) {\n"
		     "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		     "NO_MEMORY, \"Cannot malloc\");\n" 
		     "      return -1;\n"
		     "    }\n"),
		Pars = ["*oe_return"],
		DecType = case ictype:isArray(G, N, RetType) of
			      true -> array_dyn;
			      false -> caller_dyn
			  end,
		ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
					  "(*oe_return)", 
					  "", "oe_env->_inbuf", 1, 
					  "&oe_outindex", DecType,
					  Pars), 
		emit(Fd, "  }\n"),
		Pars;
	    false ->
		case ictype:isArray(G, N, RetType) of
		    true ->
			Pars = ["*oe_return"],
			emit(Fd, 
			     "  {\n"
			     "    int oe_size_count_index = oe_env->_iin;\n"
			     "    int oe_malloc_size = 0;\n"
			     "    void *oe_first = NULL;\n"),
			ic_cbe:emit_malloc_size_stmt(G, N, Fd, RetType, 
						     "oe_env->_inbuf", 
						     1, caller), 
			%% XXX Add malloc prefix from option
			emit(Fd, 
			     "    OE_MALLOC_SIZE_CHECK(oe_env, "
			     "oe_malloc_size);\n" 
			     "    if ((*oe_return = oe_first = "
			     "malloc(oe_malloc_size)) == NULL) {\n"
			     "      CORBA_exc_set(oe_env, "
			     "CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "
			     "\"Cannot malloc\");\n" 
			     "        return -1;"
			     "    }\n"),
			ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
						  "oe_return", "", 
						  "oe_env->_inbuf", 1, 
						  "&oe_outindex", 
						  array_fix_ret, 
						  Pars), 
			emit(Fd, "  }\n"),
			Pars;
		    false ->
			Pars = [],
			%% The last parameter "oe_outindex" is not interesting 
			%% in the static case.
			ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
						  "oe_return", "", 
						  "oe_env->_inbuf", 1, 
						  "&oe_outindex", 
						  caller, Pars), 
			ic_codegen:nl(Fd),
			Pars
		end
	end, 

    foldl(fun({{'void', _}, _, _}, Acc) ->
		  Acc;
	     ({T, A, N1}, Acc) ->
		  emit_one_decoding(G, N, Fd, T, A, N1, Acc)
	  end, APars, TypeAttrArgs), 
    ok.

emit_one_decoding(G, N, Fd, T, A, N1, Acc) ->
    IndOp = mk_ind_op(A), 
    case ic_cbe:is_variable_size(G, N, T) of
	true ->
	    emit_coding_comment(G, N, Fd, "Decode", IndOp, 
				  T, N1), 
	    emit(Fd, 
		 "  {\n"
		 "    int oe_size_count_index = oe_env->_iin;\n"
		 "    int oe_malloc_size = 0;\n"
		 "    void *oe_first = NULL;\n"),
	    ic_cbe:emit_malloc_size_stmt(G, N, Fd, T, 
					 "oe_env->_inbuf", 
					 1, caller), 
	    %% XXX Add malloc prefix from option
	    emit(Fd, 
		 "    OE_MALLOC_SIZE_CHECK(oe_env, oe_malloc_size);\n" 
		 "    if ((~s~s = oe_first = "
		 "malloc(oe_malloc_size)) == NULL) {\n", [IndOp, N1]),
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", Acc),
	    emit(Fd,
		 "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "NO_MEMORY, \"Cannot malloc\");\n" 
		 "      return -1;\n"
		 "    }\n"),
	    NAcc = [IndOp ++ N1| Acc],  
	    DecType = case ictype:isArray(G, N, T) of
			  true ->
			      array_dyn;
			  false ->
			      caller_dyn
		      end,
	    ic_cbe:emit_decoding_stmt(G, N, Fd, T,  
				      "(" ++ IndOp
				      ++ N1 ++ ")", "", 
				      "oe_env->_inbuf", 1, 
				      "&oe_outindex", 
				      DecType, NAcc), 
	    emit(Fd, "  }\n"),
	    NAcc;
	false ->
	    case ictype:isArray(G, N, T) of
		true ->
		    emit_coding_comment(G, N, Fd, "Decode", "", 
					  T, N1), 
		    ic_cbe:emit_decoding_stmt(G, N, Fd, T, N1, 
					      "", "oe_env->_inbuf", 
					      1, "&oe_outindex", 
					      array_fix_out, Acc), 
		    ic_codegen:nl(Fd),
		    [N1| Acc]; 
		false ->
		    %% The last parameter "oe_outindex" is
		    %% not interesting in the static case, but
		    %% must be present anyhow.
		    emit_coding_comment(G, N, Fd, "Decode", 
					  IndOp, T, N1), 
		    ic_cbe:emit_decoding_stmt(G, N, Fd, T,  N1, 
					      "", "oe_env->_inbuf", 
					      1, "&oe_outindex", 
					      caller, Acc), 
		    ic_codegen:nl(Fd),
		    Acc
	    end
    end.

%%------------------------------------------------------------
%% GENERATE PROTOTYPES
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Generate operation prototypes
%%------------------------------------------------------------
emit_operation_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when record(X, op) ->
	      {ScopedName, ArgNames, RetParTypes} = 
		  ic_cbe:extract_info(G, N, X), 
	      {R, ParTypes, _} = RetParTypes, 
	      IName = ic_util:to_undersc(N), 
	      RT = mk_ret_type(G, N, R), 
	      ParList = 
		  ic_util:chain(
		    mk_par_type_list(G, N, X, [in, out], [types], 
				     ParTypes, ArgNames), 
		    ", "), 
	      emit(Fd, "~s ~s(~s, ~sCORBA_Environment*);\n", 
		   [RT, ScopedName, IName, ParList]);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Generate encoder prototypes
%%------------------------------------------------------------
emit_encoder_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when record(X, op) ->
	      {ScopedName, ArgNames, RetParTypes} = 
		  ic_cbe:extract_info(G, N, X), 
	      {R, ParTypes, _} = RetParTypes, 
	      IName = ic_util:to_undersc(N), 
	      ParList = ic_util:chain(
			  mk_par_type_list(G, N, X, [in], [types], 
					   ParTypes, ArgNames), 
			  ", "),
	    emit(Fd, "int ~s__client_enc(~s, ~sCORBA_Environment*);\n", 
		 [ScopedName, IName, ParList]);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Generate decoder prototypes
%%------------------------------------------------------------
emit_decoder_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when record(X, op) ->
	      case ic_forms:is_oneway(X) of
		  true ->
		      true;
		  false ->
		      IName = ic_util:to_undersc(N), 
		      {ScopedName, ArgNames, RetParTypes} = 
			  ic_cbe:extract_info(G, N, X), 
		      {R, ParTypes, _} = RetParTypes, 
		      ParList0 = 
			  mk_par_type_list(G, N, X, [out], [types], 
					   ParTypes, ArgNames), 
		      PARLIST = case mk_ret_type(G, N, R) of
				    "void" ->
					ParList0;
				    Else ->
					[Else ++ "*"| ParList0]
				end, 
		      ParList = ic_util:chain(PARLIST, ", "),
		      emit(Fd, "int ~s__client_dec(~s, ~s"
			   "CORBA_Environment*);\n", 
			   [ScopedName, IName, ParList])
	      end;
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% PARAMETER TYPE LISTS
%%------------------------------------------------------------
%%------------------------------------------------------------
%%  Make parameter type list
%%
%%  InOrOut = in | out | [in | out]
%%  TypesOrArgs = types | args | [types | args]
%%------------------------------------------------------------
mk_par_type_list(G, N, X, 
		 InOrOut, TypesOrArgs, Types, Args) when atom(InOrOut) ->
    mk_par_type_list(G, N, X, [InOrOut], TypesOrArgs, Types, Args);
mk_par_type_list(G, N, X, InOrOut, 
		 TypesOrArgs, Types, Args) when atom(TypesOrArgs) ->
    mk_par_type_list(G, N, X, InOrOut, [TypesOrArgs], Types, Args);
mk_par_type_list(G, N, X, InOrOut, TypesOrArgs, Types, Args) ->
    TypeAttrArgs = 
	filterzip(
	  fun(_, {inout, Arg}) ->
		  ic_error:error(G, {inout_spec_for_c, X, Arg}), 
		  false;
	     (Type, {Attr, Arg}) ->
		  case lists:member(Attr, InOrOut) of
		      true ->
			  {true, {Type, Attr, Arg}}; 
		      false ->
			  false
		  end
	  end, Types, Args),
    lists:map(
      fun({Type, Attr, Arg}) ->
	      Ctype = ic_cbe:mk_c_type(G, N, Type), 
	      IsArray = ictype:isArray(G, N, Type), 
	      IsStruct = ictype:isStruct(G, N, Type), 
	      IsUnion = ictype:isUnion(G, N, Type), 
	      Dyn = 
		  case ic_cbe:is_variable_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->		"";
			      Ctype == "CORBA_char *" -> 	"";
			      record(Type, wstring) ->		"";
			      Ctype == "CORBA_wchar *" ->	"";
			      true ->
				  case IsArray of
				      true ->
					  "_slice*";
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
			      Attr == in, IsArray == true ->
				  "_slice*";
			      Attr == out, IsArray == true ->
				  "_slice";
			      true ->
				  ""
			  end
		  end, 
	      IndOp = mk_ind_op(Attr),
	      case {lists:member(types, TypesOrArgs), 
		    lists:member(args, TypesOrArgs)} of
		  {true, true} ->
		      Ctype ++ Dyn ++ IndOp ++ " " ++ Arg; 
		  {true, false} ->
		      Ctype ++ Dyn ++ IndOp;
		  {false, true} ->
		      Arg;
		  {false, false} ->
		      ""
	      end
      end, TypeAttrArgs).

%%------------------------------------------------------------
%% ENCODER ARG LIST
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make encoder argument list XXX
%%------------------------------------------------------------
mk_arg_list_for_encoder(G, N, X, Types, Args) ->
    filterzip(
      fun(_, {out, _}) ->
	      false;
	 (_, {inout, Arg}) ->
	      ic_error:error(G, {inout_spec_for_c, X, Arg}), 
	      false;
	 (Type, {in, Arg}) ->
	      {true, Arg}
      end, Types, Args).

%%------------------------------------------------------------
%% DECODER ARG LIST
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make decoder argument list XXX
%%------------------------------------------------------------
mk_arg_list_for_decoder(G, N, X, Types, Args) ->
    filterzip(fun(_, {in, _}) ->
		      false;
		 (_, {inout, Arg}) -> 
		      ic_error:error(G, {inout_spec_for_c, X, Arg}), 
		      false;
		 (_, {out, Arg}) ->
		      {true, Arg}
	      end, Types, Args).

%%------------------------------------------------------------
%% MISC
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make list of {Type, Attr, Arg}
%%------------------------------------------------------------
mk_type_attr_arg_list(Types, Args) ->
    filterzip(fun(Type, {Attr, Arg}) ->
		      {true, {Type, Attr, Arg}}
	      end, Types, Args).

%%------------------------------------------------------------
%% Make return type
%%------------------------------------------------------------
mk_ret_type(G, N, Type) ->
    Ctype = ic_cbe:mk_c_type(G, N, Type), 
    Dyn = case ic_cbe:is_variable_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  
			  "";
		      Ctype == "CORBA_wchar *" ->  
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


%%------------------------------------------------------------
%% Make indirection operator (to "*" or not to "*").
%%------------------------------------------------------------
mk_ind_op(in) ->
    "";
mk_ind_op(inout) ->
    error;
mk_ind_op(out) ->
    "*".

%%------------------------------------------------------------
%% Emit encoding/decoding comment
%%------------------------------------------------------------
emit_coding_comment(G, N, F, String, RefOrVal, Type, Name) ->
    emit(F, "  /* ~s parameter: ~s~s ~s */\n", 
	 [String, ic_cbe:mk_c_type(G, N, Type), RefOrVal, Name]).

%%------------------------------------------------------------
%% ZIPPERS (merging of successive elements of two lists).
%%------------------------------------------------------------

%% zip([H1| T1], [H2| T2]) ->
%%     [{H1, H2}| zip(T1, T2)];
%% zip([], []) ->
%%     [].

filterzip(F, [H1| T1], [H2| T2]) ->
    case F(H1, H2) of
	false ->
	    filterzip(F, T1, T2);
	{true, Val} ->
	    [Val| filterzip(F, T1, T2)]
    end;
filterzip(_, [], []) ->
    [].
    

