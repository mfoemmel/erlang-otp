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
%%-----------------------------------------------------------------
%% File: icenum.erl
%% Author: Lars Thorsen
%% 
%% Creation date: 980429
%% Modified:
%%
%%-----------------------------------------------------------------
%%
%% Code generation for enum's.
%%-----------------------------------------------------------------
-module(icenum).


-include("icforms.hrl").
-include("ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([enum_gen/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

enum_gen(G, N, X, c) when record(X, enum) ->
    emit_c_enum(G, N, X);
enum_gen(G, N, X, L) ->
    ok.


emit_c_enum(G, N, X) ->
    case icgen:is_hrlfile_open(G) of
	true ->
	    Fd = icgen:hrlfiled(G),
	    EnumName = [icgen:get_id2(X) | N],
	    EnumNameStr = icgen:to_undersc(EnumName),
	    icgen:insert_typedef(G, EnumNameStr, {enum, EnumNameStr}),
	    {tk_enum,_,_,EList} = ic_forms:get_tk(X),
	    icgen:emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(EnumNameStr)]),	
	    icgen:emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(EnumNameStr)]),
	    icgen:mcomment_light(Fd,
				 [io_lib:format("Enum definition: ~s",
						[EnumNameStr])],
				 c),
	    icgen:emit(Fd, "typedef CORBA_enum {", []),
	    emit_c_enum_values(G, N, Fd, EList),
	    icgen:emit(Fd, "} ~s ;\n\n", [EnumNameStr]),
	    create_c_enum_file(G, N, EnumNameStr, EList),
	    icgen:emit(Fd, "\n#endif\n\n");
	false ->
	    ok
    end.


emit_c_enum_values(G, N, Fd, [E]) ->
    icgen:emit(Fd, "~s", [icgen:to_undersc([E| N])]);
emit_c_enum_values(G, N, Fd, [E |Es]) ->
    icgen:emit(Fd, "~s, ", [icgen:to_undersc([E| N])]),
    emit_c_enum_values(G, N, Fd, Es).


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


create_c_enum_file(G, N, Name, Elist) ->

    {Fd , SName} = open_c_coding_file(G, Name),
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

    icgen:emit(Fd, "char* ~s[~p] = {\n", [icgen:mk_oe_name(G, Name),
					      length(Elist)]),
    emit_c_enum_array_values(Fd, Elist),
    icgen:emit(Fd, "};\n\n",[]),
    emit_sizecount(G, N, Fd, HFd, Name, Elist),
    emit_encode(G, N, Fd, HFd, Name, Elist),
    emit_decode(G, N, Fd, HFd, Name, Elist),
    file:close(Fd).
 
emit_c_enum_array_values(Fd, [E]) ->
    icgen:emit(Fd, "  ~p\n", [E]);
emit_c_enum_array_values(Fd, [E |Es]) ->
    icgen:emit(Fd, "  ~p,\n", [E]),
    emit_c_enum_array_values(Fd, Es).


emit_sizecount(G, N, Fd, HFd, Name, Elist) ->
    
    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	       [icgen:mk_oe_name(G, "sizecalc_"), Name]),

    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size) {\n",
	       [icgen:mk_oe_name(G, "sizecalc_"), Name]),
    icgen:emit(Fd, "  int oe_error_code = 0;\n\n",[]),

    AlignName = lists:concat(["*oe_size + sizeof(",Name,")"]),
    icgen:emit(Fd, "  *oe_size = ~s;\n\n",[icgen:mk_align(AlignName)]),

    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0)\n",
	 []),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),

    icgen:emit(Fd, "  return 0;\n\n",[]),
    icgen:emit(Fd, "}\n\n",[]).


emit_encode(G, N, Fd, HFd, Name, Elist) ->

    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s);\n",
	       [icgen:mk_oe_name(G, "encode_"), Name, Name]),

    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec) {\n",
	       [icgen:mk_oe_name(G, "encode_"), Name, Name]),
    icgen:emit(Fd, "  int oe_error_code = 0;\n\n",[]),

    icgen:emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, ~s[oe_rec])) < 0)\n", 
	 [icgen:mk_oe_name(G, Name)]),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),

    icgen:emit(Fd, "  return 0;\n\n",[]),
    icgen:emit(Fd, "}\n\n",[]).

emit_decode(G, N, Fd, HFd, Name, Elist) ->

    icgen:emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s *);\n",
	       [icgen:mk_oe_name(G, "decode_"), Name, Name]),

    icgen:emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_outindex, "
	       "~s *oe_out) {\n\n",
	       [icgen:mk_oe_name(G, "decode_"), Name, Name]),
    icgen:emit(Fd, "  int oe_error_code = 0;\n",[]),
    icgen:emit(Fd, "  int oe_i;\n",[]),
    icgen:emit(Fd, "  char oe_atom[256];\n\n",[]),

    AlignName = lists:concat(["*oe_outindex + sizeof(",Name,")"]),
    icgen:emit(Fd, "  *oe_outindex = ~s;\n\n",[icgen:mk_align(AlignName)]),

    icgen:emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_atom)) < 0)\n",
	 []),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),

    Len = length(Elist),
    icgen:emit(Fd, "  for(oe_i = 0; oe_i < ~p && strcmp(oe_atom, ~s[oe_i]); oe_i++);\n",
	 [Len, icgen:mk_oe_name(G, Name)]),
    icgen:emit(Fd, "    *oe_out = oe_i;\n\n", []),

    icgen:emit(Fd, "  if (oe_i == ~p)\n",[Len]),
    icgen:emit(Fd, "    return oe_error_code;\n\n"),
    
    icgen:emit(Fd, "  return 0;\n",[]),
    icgen:emit(Fd, "}\n\n",[]).





