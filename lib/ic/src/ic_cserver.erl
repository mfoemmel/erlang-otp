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
-module(ic_cserver).


%%    ---------------------
%%
%%    The IDL to C code generator.
%%
%%
%%------------------------------------------------------------
-export([do_gen/3]).


%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-import(icgen, [mk_oe_name/2, get_id/1, mk_list/1,
		emit/3, emit/2, 
		nl/1, is_oneway/1,
		to_list/1, get_id2/1, get_body/1,
		stubfiled/1,
		push_file/2, pop_file/2, sys_file/2]).

-import(ic_cbe, [gen_malloc_size_calculation/7,
		 check_dynamic_size/1, 
		 check_dynamic_size/3, extract_info/3]).

-import(lists, [foreach/2, foldr/3, map/2]).


-include("icforms.hrl").
-include("ic.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(IC_HEADER, "ic.h").
-define(ERL_INTERFACEHEADER, "erl_interface.h").
-define(EICONVHEADER, "ei.h").
-define(OE_MSGBUFSIZE, "OE_MSGBUFSIZE").
-define(ERLANGATOMLENGTH, "256").

%%------------------------------------------------------------
%%
%% Entry point
%%
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    G2 = icgen:filename_push(G, [], mk_oe_name(G, remove_ext(to_list(File))), c_server),
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
    G2 = icgen:filename_push(G, N, X, c_server),
    N2 = [get_id2(X) | N],
    gen_prototypes(G2,N2,X),
    gen_serv(G2, N2, X), 
    G3 = icgen:filename_pop(G2, c),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, const) ->
    emit_constant(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, op) ->
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, attr) ->
    %emit_attr(G, N, X, fun emit_stub_func/6),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, except) ->
    icstruct:except_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, enum) ->
    icenum:enum_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, c); %% create h - file
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


%%--------------------------------------------------------------------
%%
%% Generate the server encoding/decoding function
%%

gen_serv(G, N, X) ->
    case icgen:is_stubfile_open(G) of
	true ->
	    Fd = stubfiled(G), 

	    emit_switch(G, Fd, N, X),
	    emit_server_generic_decoding(G, Fd, N),

	    %% Sets the temporary variable counter.
	    put(op_variable_count, 0),
	    put(tmp_declarations, []),

	    %% Generate the operation switch part
	    lists:foreach(fun({Name, Body}) ->
				  gen_dispatch(G, Fd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]);

	false ->
	    ok
    end.


   
gen_prototypes(G,N,X) ->
    case icgen:is_hrlfile_open(G) of
	true ->
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

	    IName = icgen:to_undersc(N),

	    emit(HFd,"#include \"~s\"\n", [filename:basename(Filename)]),
	    icgen:gen_includes(HFd,G,X,c_server),
	    nl(HFd),

	    icgen:emit(HFd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(IName)]),	
	    icgen:emit(HFd, "#define __~s__\n",[ic_util:to_uppercase(IName)]),
	    icgen:mcomment_light(HFd,
				 [io_lib:format("Interface object definition: ~s",
						[IName])],
				 c),
	    icgen:emit(HFd, "typedef CORBA_Object ~s;\n\n", [IName]),	

	    icgen:emit(HFd, "#endif\n\n"),

	    %% Emit structures inside the module
	    emit(HFd,"\n/* Structure definitions  */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_structs_inside_module(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),

	    %% Emit generated function prototypes
	    emit(HFd,"\n/* Switch and composit functions  */\n",[]),
	    emit(HFd,"int ~s__switch(~s oe_obj, CORBA_Environment *oe_env);\n",[IName,IName]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),

	     %% Emit generic function prototypes
	    emit(HFd,"\n/* Generic decoder */\n",[]),
	    emit(HFd,"int ~s__call_info(~s oe_obj, CORBA_Environment *oe_env);\n",[IName,IName]),

	    %% Emit restore function typedefs
	    emit(HFd,"\n/* Restore function typedefs */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_restore_typedefs(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),

	    %% Emit callback function prototypes
	    emit(HFd,"\n/* Callback functions */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_callback_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),
	    	    
	    %% Emit callback function prototypes
	    emit(HFd,"\n/* Parameter decoders */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_decoder_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),
	    
	    %% Emit callback function prototypes
	    emit(HFd,"\n/* Message encoders */\n",[]),
	    lists:foreach(fun({Name, Body}) ->
				  gen_encoder_prototypes(G, HFd, N, Body) end,
			  [{x, get_body(X)} | X#interface.inherit_body]),
	    
	    ok;

	false -> 
	    ok
    end.


gen_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    {ScopedName, _, _} = extract_info(G, N, X),
    emit(Fd,"int ~s__exec(~s oe_obj, CORBA_Environment *oe_env);\n",[ScopedName,icgen:to_undersc(N)]),
    gen_prototypes(G, Fd, N, Xs);
gen_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_prototypes(G, Fd, N, Xs);
gen_prototypes(G, Fd, N, [X|Xs]) when record(X, const) ->
    emit_constant(G, N, X),
    gen_prototypes(G, Fd, N, Xs);
gen_prototypes(G, Fd, N, [X|Xs]) ->
    gen_prototypes(G, Fd, N, Xs);
gen_prototypes(G, Fd, N, []) -> ok.


gen_structs_inside_module(G, Fd, N, [X|Xs]) when record(X, enum) ->
    icenum:enum_gen(G, N, X, c),
    gen_structs_inside_module(G, Fd, N, Xs);
gen_structs_inside_module(G, Fd, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, c);  %% create h - file
	false -> ok
    end,
    gen_structs_inside_module(G, Fd, N, Xs);
gen_structs_inside_module(G, Fd, N, []) -> ok.



gen_callback_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    % Check if to use scoped call names
    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,
    RT = gen_ret_type(G,N,R),

    PL = mk_list(gen_par_list_for_callback_prototypes(G, N, X, ParameterTypes,ArgNames)),
   
    CBPL = case PL of
	       "" ->
		   "";
	       _PL ->
		   ", " ++ PL
	   end,
    
    case RT of
	"void" ->
	    case PL of
		"" ->
		    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, CORBA_Environment *oe_env);\n",
			 [ScopedName, ScopedName, icgen:to_undersc(N)]);
		_ ->
		    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
			 [ScopedName, ScopedName, icgen:to_undersc(N), PL])
	    end;
	
	"erlang_port*" ->
	    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s~s, CORBA_Environment *oe_env);\n",
		 [ScopedName, ScopedName, icgen:to_undersc(N), RT, CBPL]);
		
	"erlang_pid*" ->
	    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s~s, CORBA_Environment *oe_env);\n",
		 [ScopedName, ScopedName, icgen:to_undersc(N), RT, CBPL]);
		
	"erlang_ref*" ->
	    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s~s, CORBA_Environment *oe_env);\n",
		 [ScopedName, ScopedName, icgen:to_undersc(N), RT, CBPL]);
		
	_ ->
	    case ictype:isArray(G,N,R) of
		true ->
		    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s~s, CORBA_Environment *oe_env);\n",
			 [ScopedName, ScopedName, icgen:to_undersc(N), RT, CBPL]);
		false ->
		    emit(Fd,"extern ~s__rs* ~s__cb(~s oe_obj, ~s*~s, CORBA_Environment *oe_env);\n",
			 [ScopedName, ScopedName, icgen:to_undersc(N), RT, CBPL])
	    end
    end,
    gen_callback_prototypes(G, Fd, N, Xs);
gen_callback_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_callback_prototypes(G, Fd, N, Xs);
gen_callback_prototypes(G, Fd, N, [X|Xs]) ->
    gen_callback_prototypes(G, Fd, N, Xs);
gen_callback_prototypes(G, Fd, N, []) -> ok.


gen_restore_typedefs(G, Fd, N, [X |Xs]) when record(X, op) ->
    % Check if to use scoped call names
    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,
    RT = gen_ret_type(G,N,R),

    PL = mk_list(gen_par_list_for_callback_prototypes(G, N, X, ParameterTypes,ArgNames)),
    
    RPL = case PL of
	       "" ->
		   "";
	       _PL ->
		   ", " ++ PL
	   end,

    case RT of
	"void" ->
	    case PL of 
		"" ->
		    emit(Fd,"typedef void (*~s__rs(~s oe_obj, CORBA_Environment *oe_env));\n",
			 [ScopedName,icgen:to_undersc(N)]);
		_ ->
		    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s, CORBA_Environment *oe_env));\n",
			 [ScopedName,icgen:to_undersc(N),PL])
	    end;
	
	"erlang_port*" ->
	    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s~s, CORBA_Environment *oe_env));\n",
		 [ScopedName,icgen:to_undersc(N),RT,RPL]);
		
	"erlang_pid*" ->
	    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s~s, CORBA_Environment *oe_env));\n",
		 [ScopedName,icgen:to_undersc(N),RT,RPL]);
	
	"erlang_ref*" ->
	    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s~s, CORBA_Environment *oe_env));\n",
		 [ScopedName,icgen:to_undersc(N),RT,RPL]);
		
	_ ->
	    case ictype:isArray(G, N, R) of
		true ->
		    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s~s, CORBA_Environment *oe_env));\n",
			 [ScopedName,icgen:to_undersc(N),RT,RPL]);
		false ->
		    emit(Fd,"typedef void (*~s__rs(~s oe_obj, ~s*~s, CORBA_Environment *oe_env));\n",
			 [ScopedName,icgen:to_undersc(N),RT,RPL])
	    end
    end,
    gen_restore_typedefs(G, Fd, N, Xs);
gen_restore_typedefs(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_restore_typedefs(G, Fd, N, Xs);
gen_restore_typedefs(G, Fd, N, [X|Xs]) ->
    gen_restore_typedefs(G, Fd, N, Xs);
gen_restore_typedefs(G, Fd, N, []) -> ok.


gen_decoder_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    % Check if to use scoped call names
    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,
    case mk_list(gen_par_list_for_decoder_prototypes(G, N, X, ParameterTypes,ArgNames)) of
	"" ->
	    ok;
	PLFDP ->
	    emit(Fd,"int ~s__dec(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
		 [ScopedName,icgen:to_undersc(N),PLFDP])
    end,
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, [X|Xs]) ->
    gen_decoder_prototypes(G, Fd, N, Xs);
gen_decoder_prototypes(G, Fd, N, []) -> ok.



gen_encoder_prototypes(G, Fd, N, [X |Xs]) when record(X, op) ->
    case is_oneway(X) of
	true ->
	    gen_encoder_prototypes(G, Fd, N, Xs);
	false ->
	    %% Check if to use scoped call names
	    {ScopedName, ArgNames, TypeList} = extract_info(G, N, X),
	    {R, ParameterTypes, _} = TypeList,
	    RType = gen_ret_type(G, N, R),
	    case mk_list(gen_par_list_for_encoder_prototypes(G, N, X, ParameterTypes,ArgNames)) of
		"" ->
		    case RType of
			"void" ->
			    emit(Fd,"int ~s__enc(~s oe_obj, CORBA_Environment *oe_env);\n",
				 [ScopedName,icgen:to_undersc(N)]);
			_ ->
			    emit(Fd,"int ~s__enc(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
				 [ScopedName,icgen:to_undersc(N),RType])
		    end;
		PLFEP ->
		    case RType of
			"void" ->
			    emit(Fd,"int ~s__enc(~s oe_obj, ~s, CORBA_Environment *oe_env);\n",
				 [ScopedName,icgen:to_undersc(N),PLFEP]);
			_ ->
			    emit(Fd,"int ~s__enc(~s oe_obj, ~s, ~s, CORBA_Environment *oe_env);\n",
				 [ScopedName,icgen:to_undersc(N),RType,PLFEP])
		    end
	    end,
	    gen_encoder_prototypes(G, Fd, N, Xs)
    end;
gen_encoder_prototypes(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_encoder_prototypes(G, Fd, N, Xs);
gen_encoder_prototypes(G, Fd, N, [X|Xs]) ->
    gen_encoder_prototypes(G, Fd, N, Xs);
gen_encoder_prototypes(G, Fd, N, []) -> ok.




%%%------------------------------------------------------------
%%%
%%% Generates the generic part of c-server. 
%%%
%%%------------------------------------------------------------

emit_server_generic_decoding(G, Fd, N) ->

    Code = 

"
/*
 * Returns call identity
 */

int ~s__call_info(~s oe_obj, CORBA_Environment *oe_env) {

  char gencall_atom[10];
  int error_code = 0;
  int rec_version = 0;
  oe_env->_iin = 0;
  oe_env->_received = 0;

  memset(gencall_atom, 0, 10);
  ei_decode_version(oe_env->_inbuf, &oe_env->_iin, &rec_version);
  ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received);
  ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, gencall_atom);

  if (strcmp(gencall_atom, \"$gen_cast\") == 0) {

    if ((error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_env->_operation)) < 0) {
      ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received);
      if ((error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_env->_operation)) < 0) { 
        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, \"Bad Message, cannot extract operation\");
        return error_code;
      }
      oe_env->_received -= 1;
    } else
      oe_env->_received -= 2;

    return 0;
  }

  if (strcmp(gencall_atom, \"$gen_call\") == 0) {

    ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received);

    if ((error_code = ei_decode_pid(oe_env->_inbuf, &oe_env->_iin, &oe_env->_caller)) < 0) {
      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad Message, bad caller identity\");
      return error_code;
    }

    if ((error_code = ei_decode_ref(oe_env->_inbuf, &oe_env->_iin, &oe_env->_unique)) < 0) {
      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad Message, bad message reference\");
      return error_code;
    }

    if ((error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_env->_operation)) < 0) {

      ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_env->_received);

      if ((error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_env->_operation)) < 0) { 
        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, \"Bad Message, cannot extract operation\");
        return error_code;
      }

      oe_env->_received -= 1;
      return 0;	  
    }
    else {
      oe_env->_received -= 2;
      return 0;
    }
  }

  CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, MARSHAL, \"Bad message, neither cast nor call\");
  return -1;
}


",

IName = icgen:to_undersc(N),

emit(Fd,Code,[IName,IName]).



emit_switch(G, Fd, N, X) ->

    StartCode =

"
#include <string.h> 
#include \"ic.h\"
#include \"erl_interface.h\"
#include \"ei.h\"
#include \"~s__s.h\"


/*
 * Main switch
 */

int ~s__switch(~s oe_obj, CORBA_Environment *oe_env) {

  int status=0;

  /* Initiating exception indicator */
  oe_env->_major = CORBA_NO_EXCEPTION;

  /* Call switch */
  if ((status = ~s__call_info(oe_obj, oe_env)) >= 0) {\n",
            
    ScopedName = icgen:to_undersc(N),

    emit(Fd,StartCode,[ScopedName,ScopedName,ScopedName,ScopedName]),

    %% Generate the server switch part
    lists:foreach(fun({Name, Body}) ->
			  gen_switch(G, Fd, N, Body) end,
		  [{x, get_body(X)} | X#interface.inherit_body]),

    emit(Fd,"     \n"),
    emit(Fd,"    /* Bad call */\n"),
    emit(Fd,"    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_OPERATION, \"Invalid operation\");\n"),
    emit(Fd,"    return -1;\n"),
    emit(Fd,"  }\n\n\n"),
    emit(Fd,"  /* Exit */\n"),
    emit(Fd,"  return status;\n}\n\n\n").



gen_switch(G, Fd, N, [X |Xs]) when record(X, op) ->
    % Check if to use scoped call names
    {ScopedName, _, _} = extract_info(G, N, X),
    Name = case icgen:get_opt(G, scoped_op_calls) of 
	       true -> 
		   ScopedName;
	       false ->
		   icgen:get_id(X#op.id)
	   end,
    
    emit(Fd,"\n    if (strcmp(oe_env->_operation, ~p) == 0)\n",[Name]),
    emit(Fd,"      return ~s__exec(oe_obj, oe_env);\n",[ScopedName]),
    gen_switch(G, Fd, N, Xs);
gen_switch(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_switch(G, Fd, N, Xs);
gen_switch(G, Fd, N, [X|Xs]) ->
    gen_switch(G, Fd, N, Xs);
gen_switch(G, Fd, N, []) -> ok.




gen_dispatch(G, Fd, N, [X |Xs]) when record(X, op) ->
    {Name, ArgNames, TypeList} = extract_info(G, N, X),
    emit_composit(G, Fd, N, X, Name, ArgNames, TypeList),
    emit_parameter_decoder(G, Fd, N, X, Name, ArgNames, TypeList),
    emit_message_encoder(G, Fd, N, X, Name, ArgNames, TypeList),
    gen_dispatch(G, Fd, N, Xs);
gen_dispatch(G, Fd, N, [X |Xs]) when record(X, attr) ->
    gen_dispatch(G, Fd, N, Xs);
gen_dispatch(G, Fd, N, [X|Xs]) ->
    gen_dispatch(G, Fd, N, Xs);
gen_dispatch(G, Fd, N, []) -> ok.



emit_composit(G, Fd, N, X, Name, ArgNames, TypeList) ->
    {R, ParameterTypes, _} = TypeList,
    TAlist = gen_type_arg_list(ParameterTypes, ArgNames),  

    %% Decoding operation specific part

    InParList = lists:filter(fun({_, A, _}) ->
				 case A of
				     out ->
					 false;
				     inout ->
					 false;
				     _ ->
					 true
				 end
			       end, TAlist),
    nl(Fd),

    emit(Fd,
	 "int ~s__exec(~s oe_obj, CORBA_Environment *oe_env) {\n\n",
	 [Name,icgen:to_undersc(N)]),
    
    emit(Fd,"  if (oe_env->_received != ~p) {\n",[length(InParList)]),
    emit(Fd,"    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Wrong number of operation parameters\");\n"),
    emit(Fd,"    return -1;\n",[]),
    emit(Fd,"  }\n"),
    emit(Fd,"  else {\n",[]),

    case InParList of
	[] ->
	    true;
%	    case is_oneway(X) of
%		true ->
%		    ok; %% Special case, no ret value && no parameters
%		false ->
%		    emit(Fd,"    int oe_error_code;\n")
%	    end;
	_ ->
	    emit(Fd,"    int oe_error_code = 0;\n")
    end,
    
    %% Callback variable definition
    emit_variable_defs(G, Fd, N, X, Name, R, ArgNames, ParameterTypes),  

    %% Call to parameter decoder
    emit_parameter_decoder_call(G, Fd, N, X, Name, R, ArgNames, ParameterTypes),   

    %% Callback to user code
    emit_callback(G, Fd, N, X, Name, R, ArgNames, ParameterTypes),
 
    %% Call to return message encoder 
    case is_oneway(X) of 
	true ->
           true;
        false ->
           emit_message_encoder_call(G, Fd, N, X, Name, R, ArgNames, ParameterTypes)
    end,

    %% Restore function call
    emit_restore(G, Fd, N, X, Name, R, ArgNames, ParameterTypes),

    emit(Fd, "  }\n  return 0;\n}\n\n").




emit_parameter_decoder(G, Fd, N, X, Name, ArgNames, TypeList) ->
    {R, ParameterTypes, _} = TypeList,
    TAlist = gen_type_arg_list(ParameterTypes, ArgNames),  

    %% Decoding operation specific part
    InParList = lists:filter(fun({_, A, _}) ->
				 case A of
				     out ->
					 false;
				     inout ->
					 false;
				     _ ->
					 true
				 end
			       end, TAlist),


    case InParList of
	[] ->
	    ok;
	_ ->
	    case mk_list(gen_par_list_for_decoder(G, N, X, ParameterTypes,ArgNames)) of
		"" ->
		    emit(Fd,"int ~s__dec(~s oe_obj, CORBA_Environment *oe_env) {\n\n  int oe_error_code;\n\n",
			 [Name,icgen:to_undersc(N)]);
		PLFD ->
		    emit(Fd,
			 "int ~s__dec(~s oe_obj, ~s, CORBA_Environment *oe_env) {\n\n",
			 [Name,icgen:to_undersc(N),PLFD]),

		    emit(Fd,"  int oe_error_code;\n\n")
	    end,
	    
	    
	    foreach(fun({T1, A1, N1}) ->
			    case T1 of
				{'void', _} ->
				    ok;
				_ ->
				    Refstring = check_refval(A1),
				    case check_dynamic_size(G, N, T1) of
					false ->

					    %% The last parameter "oe_outindex" is not interesting 
					    %% in the static case.
					    gen_decoding_fun(G, N, Fd, T1,  N1, "",
							     "oe_env->_inbuf", 1, "&oe_outindex", caller), 
					    nl(Fd);
					true ->
					    emit_encoding_comment(G, N, Fd, "Decode", Refstring, T1, N1),
					    emit(Fd, "  {\n"),
					    emit(Fd, "    int oe_size_count_index = oe_env->_iin;\n"),
					    emit(Fd, "    int oe_malloc_size = 0;\n"),
					    emit(Fd, "    char *oe_first = 0;\n"),

					    gen_malloc_size_calculation(G, N, Fd, T1, "oe_env->_inbuf", 1, caller),
					    emit(Fd, "    if (oe_malloc_size > 0)  { \n"
						     "      oe_first = malloc(oe_malloc_size);\n"
					             "      (*~s) = (void *)oe_first;\n"
						     "    }\n\n",[N1]),
					    case ictype:isArray(G, N, T1) of
						true ->
						    gen_decoding_fun(G, N, Fd, T1,  "(*" ++ Refstring ++ N1 ++ ")", "",
								     "oe_env->_inbuf", 1, "&oe_outindex", array_dyn);
						false ->
						    gen_decoding_fun(G, N, Fd, T1,  "(*" ++ Refstring ++ N1 ++ ")", "",
								     "oe_env->_inbuf", 1, "&oe_outindex", caller_dyn)
					    end,
                                            emit(Fd, "  }\n\n")

				    end;
				_ ->
				    ok
			    end
		    end, InParList),        
	    emit(Fd, "  return 0;\n}\n\n"),
	    ok
    end.



emit_message_encoder(G, Fd, N, X, Name, ArgNames, TypeList) ->
    case is_oneway(X) of
	false ->
	    {R, ParameterTypes, _} = TypeList,
	    TAlist = gen_type_arg_list(ParameterTypes, ArgNames),  
	    
	    %% Decoding operation specific part
	    
	    InParList = lists:filter(fun({_, A, _}) ->
					     case A of
						 out ->
						     true;
						 inout ->
						     true;
						 _ ->
						     false
					     end
				     end, TAlist),
	    
	    emit(Fd, 
		 "\nint ~s__enc(~s oe_obj",
		 [Name,icgen:to_undersc(N)]),
	    
	    RType = gen_ret_type(G, N, R),
	    case mk_list(gen_par_list_for_encoder(G, N, X, ParameterTypes,ArgNames)) of
		"" ->
		    case RType of
			"void" ->
			    emit(Fd,", CORBA_Environment *oe_env) {");
			_ ->
			    emit(Fd,", ~s oe_result, CORBA_Environment *oe_env) {",[RType])
		    end;
		PLFD ->
		    case RType of
			"void" ->
			    emit(Fd,", ~s, CORBA_Environment *oe_env) {",[ PLFD ]);
			_ ->
			    emit(Fd,", ~s oe_result~s, CORBA_Environment *oe_env) {",[RType, ", " ++ PLFD ])
		    end
	    end, 
	    
	    emit(Fd, "\n  int oe_error_code;    \n  oe_env->_iout = 0;\n\n"),
	    
	    %% Encoding
	    emit(Fd, "  oe_ei_encode_version(oe_env);\n"),
	    
	    OutParList = lists:filter(fun({_, A, _}) ->
					      case A of
						  in ->
						      false;
						  inout ->
						      false;
						  _ ->
						      true
					      end
				      end, TAlist),     

	    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, 2);\n"),
	    emit(Fd, "  oe_ei_encode_ref(oe_env, &oe_env->_unique);\n"),

	    OutLength = length(OutParList),

	    case OutLength > 0 of
		false ->
		    nl(Fd);
		true ->
		    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, ~p);\n\n",
			 [OutLength+1])
		    
	    end,

	    %%     		RetRefstring = check_refval(R),
	    emit_encoding_comment(G, N, Fd, "Encode", "", R, "oe_result"),
	    gen_encoding_fun(G, N, X, Fd, R, "oe_result", "oe_env->_outbuf"),

	    foreach(fun({T1, A1, N1}) ->
			    case T1 of
				{'void', _} ->
				    ok;
				_ ->
				    %%Refstring = check_refval(A1),
				    %% Refstring is removed from the following two calls and it 
				    %% must be checked that all parameers have correct type.
				    %% But i think it's ok
				    emit_encoding_comment(G, N, Fd, "Encode", "", T1, N1),
				    gen_encoding_fun(G, N, X, Fd, T1, N1, "oe_env->_outbuf")
			    end
		    end, OutParList),
	    
	    emit(Fd, "  return 0;\n}\n\n");
	
	_ ->
	    %% Oneway, Produce no code !  
	    ok
    end.





emit_message_encoder_call(G, Fd, N, X, Name, R, ArgNames, ParameterTypes) ->
    emit(Fd, "    /* Encoding reply message */\n"),
    RType =  gen_ret_type(G, N, R),
    case  mk_list(gen_enc_par_list(G, N, X, ArgNames, ParameterTypes)) of
	"" ->
	    case RType of
		"void" ->
		    emit(Fd,"    ~s(oe_obj, oe_env);\n",
			 [Name ++ "__enc"]);
		"erlang_pid*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, oe_env);\n",
			 [Name ++ "__enc"]);
		"erlang_port*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, oe_env);\n",
			 [Name ++ "__enc"]);
		"erlang_ref*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, oe_env);\n",
			 [Name ++ "__enc"]);
		_ ->
		    emit(Fd,"    ~s(oe_obj, oe_result, oe_env);\n",
			 [Name ++ "__enc"])
	    end;
	
	PLFE ->
	    case RType of
		"void" ->
		    emit(Fd,"    ~s(oe_obj, ~s, oe_env);\n",
			 [Name ++ "__enc",PLFE]);
		"erlang_pid*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, ~s, oe_env);\n",
			 [Name ++ "__enc",PLFE]);
		"erlang_port*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, ~s, oe_env);\n",
			 [Name ++ "__enc",PLFE]);
		"erlang_ref*" ->
		    emit(Fd,"    ~s(oe_obj, &oe_result, ~s, oe_env);\n",
			 [Name ++ "__enc",PLFE]);
		_ ->
		    emit(Fd,"    ~s(oe_obj, oe_result, ~s, oe_env);\n",
			 [Name ++ "__enc",PLFE])
	    end
    end,
    nl(Fd).



emit_parameter_decoder_call(G, Fd, N, X, Name, R, ArgNames, ParameterTypes) ->
    case mk_list(gen_dec_par_list(G, N, X, ArgNames,ParameterTypes))  of
	"" -> %% No parameters ! skip it !
	    ok;
	PLFDC ->
	    ParDecName = Name ++ "__dec",
	    emit(Fd, "    /* Decode parameters */\n"),
	    emit(Fd, "    if((oe_error_code = ~s(oe_obj, ~s, oe_env)) < 0) {\n",
		 [ParDecName, PLFDC]),
	    emit(Fd, "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad parameter on decode\");\n"),
	    emit(Fd, "      return oe_error_code;\n  }\n\n")
    end.


emit_callback(G, Fd, N, X, Name, R, ArgNames, ParameterTypes) ->
    CallBackName = Name ++ "__cb",
    emit(Fd, "    /* Callback function call */\n"),
    PL = mk_list(gen_cb_par_list(G, N, X, ArgNames, ParameterTypes)),
    case is_oneway(X) of
	true ->
	    case PL of
		"" ->
		    emit(Fd, "    oe_restore = ~s(oe_obj, oe_env);\n\n",
			 [CallBackName]);
		_ ->
		    emit(Fd, "    oe_restore = ~s(oe_obj, ~s, oe_env);\n\n",
			 [CallBackName,PL])
	    end;
	false ->
	    CBPL = case PL of
		       "" ->
			   "";
		       _PL ->
			   ", " ++ PL
		   end,
	    case gen_ret_type(G, N, R) of
		"void" ->
		    case PL of 
			"" ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, oe_env);\n\n",
				 [CallBackName]);
			_ ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, ~s, oe_env);\n\n",
				 [CallBackName,PL])
		    end;
		_ ->
		    case ictype:isArray(G, N, R) of
			true ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, oe_result~s, oe_env);\n\n",
			 [CallBackName,CBPL]);
			false ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, &oe_result~s, oe_env);\n\n",
				 [CallBackName,CBPL])
		    end
	    end
    end.


emit_restore(G, Fd, N, X, Name, R, ArgNames, ParameterTypes) ->
    RestoreName = Name ++ "__rs",
    emit(Fd, "    /* Restore function call */\n"),
    emit(Fd, "    if (oe_restore != NULL)\n"),
    PL = mk_list(gen_cb_par_list(G, N, X, ArgNames, ParameterTypes)),
    case is_oneway(X) of
	true ->
	    case PL of
		"" ->
		    emit(Fd, "      (*oe_restore)(oe_obj, oe_env);\n\n");
		_ ->
		    emit(Fd, "      (*oe_restore)(oe_obj, ~s, oe_env);\n\n",[PL])
	    end;
	false ->
	    RPL = case PL of
		       "" ->
			   "";
		       _PL ->
			   ", " ++ PL
		   end,
	    case gen_ret_type(G, N, R) of
		"void" ->
		    case PL of
			"" ->
			    emit(Fd, "      (*oe_restore)(oe_obj, oe_env);\n\n");
			_ ->
			    emit(Fd, "      (*oe_restore)(oe_obj, ~s, oe_env);\n\n",[PL])
		    end;
		_ ->
		    case ictype:isArray(G, N, R) of
			true ->
			    emit(Fd, "      (*oe_restore)(oe_obj, oe_result~s, oe_env);\n\n",[RPL]);
			false ->
			    emit(Fd, "      (*oe_restore)(oe_obj, &oe_result~s, oe_env);\n\n",[RPL])
		    end
	    end
    end.



emit_variable_defs(G, Fd, N, X, Name, R, ArgNames, ParameterTypes) ->
    {ScopedName, _, _} = extract_info(G, N, X),
    emit(Fd,"    ~s__rs* oe_restore = NULL;\n",[ScopedName]),
    RestVars = mk_var_list(gen_var_decl_list(G, N, X, ParameterTypes,ArgNames)),
    case is_oneway(X) of
	true ->
	    emit(Fd,"~s\n\n",[RestVars]);
	false ->
	    RType =  gen_ret_type(G, N, R),
	    case RType of
		"void" ->
		    emit(Fd,"~s\n\n",[RestVars]);
		"CORBA_unsigned_long" -> 
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_unsigned_long_long" -> 
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_unsigned_short" -> 
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);		
		"CORBA_short" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_long" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_long_long" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_float" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_double" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_char" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_wchar" ->  %% WCHAR
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_boolean" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		"CORBA_octet" ->
		    emit(Fd,"~s    ~s oe_result = 0;\n\n",[RestVars,RType]);
		_ ->
		    case check_dynamic_size(G, N, R) of
			true ->
			    emit(Fd,"~s    ~s oe_result;\n\n",[RestVars,RType]);
			false ->
			    TK = ic_forms:get_tk(X), 
			    case TK of
				{tk_enum,_,_,List} ->
				    %% Enumerants are initiated to the 
				    %% first element in their definition
				    %% emit(Fd,"~s    ~s oe_result = ~s;\n\n",[RestVars,RType,hd(List)]);
				    emit(Fd,"~s    ~s oe_result;\n\n",[RestVars,RType]);
				_ ->
				    case RType of
					"erlang_pid*" ->
					    emit(Fd,"~s    erlang_pid oe_result;\n\n",[RestVars]);
					"erlang_port*" ->
					    emit(Fd,"~s    erlang_port oe_result;\n\n",[RestVars]);
					"erlang_ref*" ->
					    emit(Fd,"~s    erlang_ref oe_result;\n\n",[RestVars]);
					_ ->
					    %% Structures are initiated by memset
					    emit(Fd,"~s    ~s oe_result;\n\n",[RestVars,RType])
				    end,
				    emit(Fd,"    memset(&oe_result, 0, sizeof(oe_result));\n\n")
			    end
		    end
	    end
    end.



mk_var_list([]) -> 
    "";
mk_var_list([Arg | Args]) ->
    "    " ++ Arg ++ ";\n" ++ mk_var_list(Args).


gen_ret_type(G, N, Type) ->
    Ctype = gen_cc_type(G, N, Type),
    Dyn = case check_dynamic_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "*";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  %% WSTRING
			  "*";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end;
	      false ->
		  if 
		      Ctype == "erlang_pid" ->
			  "*";
		      Ctype == "erlang_port" ->
			  "*";
		      Ctype == "erlang_ref" ->
			  "*";
		      true ->
			  ""
		  end
	  end,
    Ctype ++ Dyn.

gen_cb_par_list(_G,_N, _X, [], _) ->
    [];
gen_cb_par_list(G, N, X, [{inout, Arg} |Args], [Type|Types]) ->
    icgen:error(G, {inout_spec_for_c, X, Arg}), 
    gen_cb_par_list(G, N, X, Args, Types);
gen_cb_par_list(G, N, X, [{Attr, Arg} |Args], [Type|Types]) ->
    case check_dynamic_size(G, N, Type) of
	true ->
	    case Attr of
		in ->
		    [ Arg | gen_cb_par_list(G, N, X, Args, Types)];
		out ->
		    case ictype:isArray(G,N,Type) of
			true ->
			    [ Arg | gen_cb_par_list(G, N, X, Args, Types)];
			_ ->
			    [ "&" ++ Arg | gen_cb_par_list(G, N, X, Args, Types)]
		    end
	    end;
	false ->
	    case ictype:isArray(G,N,Type) of
		true ->
		    [ Arg | gen_cb_par_list(G, N, X, Args, Types)];
		_ ->
		    [ "&" ++ Arg | gen_cb_par_list(G, N, X, Args, Types)]
	    end
    end.



gen_dec_par_list(_G, _N, _X, [], _) -> [];
gen_dec_par_list(G, N, X, [{out, Arg} |Args], [Type|Types]) ->
gen_dec_par_list(G, N, X, Args, Types);
gen_dec_par_list(G, N, X, [{inout, Arg} |Args], [Type|Types]) ->
gen_dec_par_list(G, N, X, Args, Types);
gen_dec_par_list(G, N, X, [{in, Arg} |Args], [Type|Types]) ->
    Ctype = gen_cc_type(G, N, Type),
    case check_dynamic_size(G, N, Type) of
	true ->
	    if 
		record(Type, string) ->
		    [ "&" ++ Arg | gen_dec_par_list(G, N, X, Args, Types)];
		Ctype == "CORBA_char *" ->
		    [ Arg | gen_dec_par_list(G, N, X, Args, Types)];
		record(Type, wstring) ->  %% WSTRING
		    [ "&" ++ Arg | gen_dec_par_list(G, N, X, Args, Types)];
		Ctype == "CORBA_wchar *" ->  %% WSTRING
		    [ Arg | gen_dec_par_list(G, N, X, Args, Types)];
		true ->
		    [ "&" ++ Arg | gen_dec_par_list(G, N, X, Args, Types)]
	    end;
	false ->
	    case ictype:isArray(G,N,Type) of
		true ->
		    [ Arg | gen_dec_par_list(G, N, X, Args, Types)];
		_ ->
		    [ "&" ++ Arg | gen_dec_par_list(G, N, X, Args, Types)]
	    end
    end.


gen_enc_par_list(_G, _N, _X, [], _) ->
    [];
gen_enc_par_list(G, N, X, [{inout, Arg} |Args], [Type|Types]) ->
    icgen:error(G, {inout_spec_for_c, X, Arg}), 
    gen_enc_par_list(G, N, X, Args, Types);
gen_enc_par_list(G, N, X, [{in, Arg} |Args], [Type|Types]) ->
    gen_enc_par_list(G, N, X, Args, Types);
gen_enc_par_list(G, N, X, [{out, Arg} |Args], [Type|Types]) ->
    Ctype = gen_cc_type(G, N, Type),
    case Ctype of
	"erlang_pid" ->
	    [ "&" ++ Arg | gen_enc_par_list(G, N, X, Args, Types)];
	"erlang_port" ->
	    [ "&" ++ Arg | gen_enc_par_list(G, N, X, Args, Types)];
	"erlang_ref" ->
	    [ "&" ++ Arg | gen_enc_par_list(G, N, X, Args, Types)];
	_ ->
	    [ Arg | gen_enc_par_list(G, N, X, Args, Types)]
    end.



%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%

%% Some items have extra includes
gen_head_special(G, N, X) when record(X, module) ->
    case icgen:is_hrlfile_open(G) of
	true ->
	    HFd = icgen:hrlfiled(G),
	    IncludeFileStack = icgen:include_file_stack(G),
	    Filename = lists:nth(length(N) + 1, IncludeFileStack),
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]),
	    icgen:gen_includes(HFd,G,X,c_server);
	false -> ok
    end;
gen_head_special(G, [], X) -> 
    case icgen:is_hrlfile_open(G) of
	true ->
	    HFd = icgen:hrlfiled(G),
	    emit(HFd, "#include <stdlib.h>\n"),
	    emit(HFd, "#include \"~s\"\n", [?IC_HEADER]),
	    emit(HFd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]),
	    emit(HFd, "#include \"~s\"\n", [?EICONVHEADER]),
	    icgen:gen_includes(HFd,G,c_server);
	false -> ok
    end;
gen_head_special(G, N, X) -> ok.

%% Open stubfile
gen_head(G, N, X) -> 
    gen_head_special(G, N, X).

    

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



gen_par_list(_, _, _, [], []) ->
    [];
gen_par_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "*";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "*";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  "*"
			  end;
		      false ->
			  ""
		  end,
	    [Ctype ++ Dyn ++ RefVal ++ " " ++ Arg | 
	     gen_par_list(G, N, X, Types, Args)]
    end.



gen_par_list_for_decoder(_, _, _, [], []) ->
    [];
gen_par_list_for_decoder(G, N, X, [Type |Types], [{out, Arg}|Args]) ->
    gen_par_list_for_decoder(G, N, X, Types, Args);
gen_par_list_for_decoder(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_decoder(G, N, X, Types, Args);
	out ->
	    gen_par_list_for_decoder(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "**";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "**";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  slice(Attr) ++ "*";
				      _ ->
					  "**"
				  end
			  end;
		      false ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end,
	    [Ctype ++ Dyn ++ " " ++ Arg | 
	     gen_par_list_for_decoder(G, N, X, Types, Args)]
    end.



gen_par_list_for_encoder(_, _, _, [], []) ->
    [];
gen_par_list_for_encoder(G, N, X, [Type |Types], [{in, Arg}|Args]) ->
    gen_par_list_for_encoder(G, N, X, Types, Args);
gen_par_list_for_encoder(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    Ctype = gen_cc_type(G, N, Type),
    Dyn = case check_dynamic_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "*";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  %% WSTRING
			  "*";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end;
	      false ->
		  if 
		      Ctype == "erlang_pid" ->
			  "*";
		      Ctype == "erlang_port" ->
			  "*";
		      Ctype == "erlang_ref" ->
			  "*";
		      true ->
			  ""
		  end
	  end,
    [Ctype ++ " " ++ Dyn ++ Arg | gen_par_list_for_encoder(G, N, X, Types, Args)].


gen_par_list_for_decoder_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_decoder_prototypes(G, N, X, [Type |Types], [{out, Arg}|Args]) ->
    gen_par_list_for_decoder_prototypes(G, N, X, Types, Args);
gen_par_list_for_decoder_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_decoder_prototypes(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "**";
			      Ctype == "CORBA_char *" ->
				  "";
			      record(Type, wstring) ->  %% WSTRING
				  "**";
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "";
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  slice(Attr) ++ "*";
				      _ ->
					  "**"
				  end
			  end;
		      false ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end,
	    [Ctype ++ Dyn | 
	     gen_par_list_for_decoder_prototypes(G, N, X, Types, Args)]
    end.




gen_par_list_for_encoder_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_encoder_prototypes(G, N, X, [Type |Types], [{in, Arg}|Args]) ->
    gen_par_list_for_encoder_prototypes(G, N, X, Types, Args);
gen_par_list_for_encoder_prototypes(G, N, X, [Type |Types], [{inout, Arg}|Args]) ->
    icgen:error(G, {inout_spec_for_c, X, Arg}), 
    gen_par_list_for_encoder_prototypes(G, N, X, Types, Args);
gen_par_list_for_encoder_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    Ctype = gen_cc_type(G, N, Type),
    Dyn = case check_dynamic_size(G, N, Type) of
	      true ->
		  if 
		      record(Type, string) ->
			  "*";
		      Ctype == "CORBA_char *" ->
			  "";
		      record(Type, wstring) ->  %% WSTRING
			  "*";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end;
	      false ->
		  if 
		      Ctype == "erlang_pid" ->
			  "*";
		      Ctype == "erlang_port" ->
			  "*";
		      Ctype == "erlang_ref" ->
			  "*";
		      true ->
			  ""
		  end
	  end,
    [Ctype ++ Dyn| gen_par_list_for_encoder_prototypes(G, N, X, Types, Args)].


gen_par_list_for_callback_prototypes(_, _, _, [], []) ->
    [];
gen_par_list_for_callback_prototypes(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_par_list_for_callback_prototypes(G, N, X, Types, Args);
	RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    Dyn = case check_dynamic_size(G, N, Type) of
		      true ->
			  if 
			      record(Type, string) ->
				  "*" ++ RefVal;
			      Ctype == "CORBA_char *" ->
				  "" ++ RefVal;
			      record(Type, wstring) ->  %% WSTRING
				  "*" ++ RefVal;
			      Ctype == "CORBA_wchar *" ->  %% WSTRING
				  "" ++ RefVal;
			      true ->
				  case ictype:isArray(G, N, Type) of
				      true ->
					  "";
				      _ ->
					  "*" ++ RefVal
				  end
			  end;
		      false ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  case Attr of  %% Should just be RefVal
				      in ->
					  "*" ++ RefVal;
				      out ->
					  RefVal
				  end
			  end
		  end,
	    [Ctype ++ Dyn | 
	     gen_par_list_for_callback_prototypes(G, N, X, Types, Args)]
    end.


gen_var_decl_list(_, _, _, [], []) ->
    []; 
gen_var_decl_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    case check_refval(Attr) of
	error ->
	    icgen:error(G, {inout_spec_for_c, X, Arg}), 
	    gen_var_decl_list(G, N, X, Types, Args);
	_RefVal ->
	    Ctype = gen_cc_type(G, N, Type),
	    VarDecl = case check_dynamic_size(G, N, Type) of
			  true ->
			      if 
				  record(Type, string) ->
				      Ctype ++ "* " ++ Arg ++ " = NULL";
				  Ctype == "CORBA_char *" ->
				      Ctype ++ " " ++ Arg ++ " = NULL";
				  record(Type, wstring) ->  %% WSTRING
				      Ctype ++ "* " ++ Arg ++ " = NULL";
				  Ctype == "CORBA_wchar *" ->  %% WSTRING
				      Ctype ++ " " ++ Arg ++ " = NULL";
				  true ->
				      case ictype:isArray(G, N, Type) of
					  true ->
					      Ctype ++ slice(Attr) ++ " " ++ Arg;
					  _ ->
					      Ctype ++ "* " ++ Arg
				      end
			      end;
			  false ->
			      Ctype ++ " " ++ Arg
		      end,
	    
	    [VarDecl | gen_var_decl_list(G, N, X, Types, Args)]
    end.


slice(in) ->
    "_slice*";
slice(_) ->
    "".


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
    emit(F,[io_lib:format("  /* ~s parameter: ~s~s ~s */\n",
			  [String, gen_cc_type(G, N, Type),
			   RefOrVal, Name])]).

emit_op_comment(G, F, X, Name, InP) ->
    emit(F,[io_lib:format("    /* ~s: ~s */\n", [get_title(X), Name]),
	    "",
	    get_returns(G, X, InP, []) |
	    get_raises(X)]).

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


%% Warning this is NOT identical to gen_cc_type on ic_cbe.erl
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
    "CORBA_char";
gen_cc_type(G, N, S, _) when record(S, wstring) ->  %% WSTRING
    "CORBA_wchar";
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
gen_cc_type(G, N, {T, _}, _) ->
    "CORBA_" ++ atom_to_list(T).


%%------------------------------------------------------------
%%    C to erlang type conversion
%%------------------------------------------------------------

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
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0) {\n",
		 [icgen:mk_oe_name(G, "encode_"), T, LName]);
	ParamTK ->
	    case check_dynamic_size(ParamTK) of
		true ->
		    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0) {\n",
			 [icgen:mk_oe_name(G, "encode_"), T, LName]),
		    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
		    emit(Fd, "    return oe_error_code;\n  }\n\n");
		false ->
		    if atom(ParamTK) ->
			    case ParamTK of
				
				tk_ushort -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0) {\n", 
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_ulong -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0) {\n", 
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_ulonglong -> 
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulonglong(oe_env, ~s)) < 0) {\n", 
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_short ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_longlong ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_longlong(oe_env, ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_float ->
				    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_double ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_boolean ->
				    emit(Fd, "  switch(~s) {\n",[LName]),
				    emit(Fd, "    case 0 :\n"),
				    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"),
				    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "        return oe_error_code;\n      }\n"),
				    emit(Fd, "      break;\n"),
				    emit(Fd, "    case 1 :\n"),
				    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"),
				    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "        return oe_error_code;\n      }\n"),
				    emit(Fd, "      break;\n"),
				    emit(Fd, "    default :\n"),
				    emit(Fd, "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "      return -1;\n"),
				    emit(Fd, "  }\n\n");
				tk_char ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_wchar ->  %% WCHAR
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				tk_octet ->
				    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0) {\n",
					 [LName]),
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n");
				_ ->
				    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
				    emit(Fd, "    return oe_error_code;\n  }\n\n"),
				    ok
			    end;
		       true ->
			    case element(1,ParamTK) of
				tk_enum ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0) {\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]);
				tk_array ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0) {\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName]);
				_ ->
				    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, &~s)) < 0) {\n",
					 [icgen:mk_oe_name(G, "encode_"), T, LName])
			    end,
			    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
			    emit(Fd, "    return oe_error_code;\n  }\n\n")
		    end
	    end
    end;
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer)  when record(T, string) ->
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_string(oe_env,(const char*) ~s)) < 0) {\n", 
	 [LName]),
    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Cannot encode string\");\n"),
    emit(Fd, "    return oe_error_code;\n  }\n\n");
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer)  when record(T, wstring) ->  %% WSTRING
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wstring(oe_env, ~s)) < 0) {\n", 
	 [LName]),
    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Cannot encode string\");\n"),
    emit(Fd, "    return oe_error_code;\n  }\n\n");
gen_encoding_fun(G, N, X, Fd, T, LName, OutBuffer) ->
    case T of
	{unsigned, {short, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0) {\n", 
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{unsigned, {long, _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, ~s)) < 0) {\n", 
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{unsigned, {'long long', _}} -> 
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulonglong(oe_env, ~s)) < 0) {\n", 
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{short, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, (long) ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{long, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{'long long', _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_longlong(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{float,_} ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, (double) ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{double, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_double(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{boolean, _} ->
	    emit(Fd, "  switch(~s) {\n",[LName]),
	    emit(Fd, "    case 0 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"),
	    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    case 1 :\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"),
	    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    default :\n"),
	    emit(Fd, "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "  }\n\n");
	{char, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{wchar, _} ->  %% WCHAR
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wchar(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{octet, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{void, _} ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, \"void\")) < 0) {\n"),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{sequence, _, _} ->
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, \"Bad operation parameter on encode\");\n"),
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	_ ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.



%% Usefull functions
get_param_tk("oe_result",Op) ->
    ic_forms:get_tk(Op);
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
		    emit(Fd, "    *~s = (unsigned short) oe_ulong;\n",[LName]),
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
		    emit(Fd, "    *~s = (short) oe_long;\n",[LName]),
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
		    emit(Fd, "    *~s = (float) oe_double;\n",[LName]),
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
	    emit(Fd, "    {\n"),
	    case DecType of
		generator ->
		    true;
		caller -> %% No malloc used, define oe_first
		    emit(Fd, "      char *oe_first = 0;\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n");
		array_dyn ->  %% Malloc used 
		    emit(Fd, "      int oe_outindex = 0;\n\n");
%			 [icgen:mk_align(io_lib:format("sizeof(~s)",[T]))]);
		caller_dyn ->  %% Malloc used 
		    emit(Fd, "      int oe_outindex = 0;\n\n")
	    end,
	    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, ~s, ~s)) < 0)\n",
		 [icgen:mk_oe_name(G, "decode_"),
		  T, NextPos, LName]),
	    emit(Fd, "        return oe_error_code;\n"),
	    emit(Fd, "    }\n")
    end;
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		 InBuffer, Align, NextPos, DecType)  when record(T, string) ->
    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
	 [InBuffer, Refstring, LName]), 
    emit(Fd, "      return oe_error_code;\n\n");
gen_decoding_fun(G, N, Fd, T, LName, Refstring,
		 InBuffer, Align, NextPos, DecType)  when record(T, wstring) ->  %% WSTRING
    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, &oe_env->_iin, ~s~s)) < 0)\n", 
	 [InBuffer, Refstring, LName]), 
    emit(Fd, "      return oe_error_code;\n\n");
gen_decoding_fun(G, N, Fd, T, LName, Refstring, InBuffer, Align, NextPos, DecType) ->
    case T of
	{void, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_atom(~s, &oe_env->_iin, 0)) < 0)\n", 
		 [InBuffer]),
	    emit(Fd, "    return oe_error_code;\n\n");

	{unsigned, {short, _}} ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    unsigned long oe_~s;\n",[LName]),
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, &oe_env->_iin, &oe_~s)) < 0)\n",
		 [InBuffer, LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *~s = (unsigned short) oe_~s;\n",[LName,LName]),
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
	    emit(Fd, "    long oe_~s;\n",[LName]),
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, &oe_env->_iin, &oe_~s)) < 0)\n",
		 [InBuffer, LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *~s = (short) oe_~s;\n",[LName,LName]),
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
	    emit(Fd, "    double oe_~s;\n",[LName]),
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, &oe_env->_iin, &oe_~s)) < 0)\n",
		 [InBuffer, LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "    *~s = (float) oe_~s;\n",[LName,LName]),
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
	    emit(Fd, "    return oe_error_code;\n\n");

	_ ->
	    icgen:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.


















