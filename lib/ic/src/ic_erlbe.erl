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
-module(ic_erlbe).


-export([do_gen/3]).
%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-export([unfold/1, mk_attr_func_names/2]).

-import(icgen, [mk_name/2, mk_oe_name/2, 
		mk_var/1, get_id/1, 
		emit/3, emit/2,
		get_opt/2, 
		nl/1, is_oneway/1,
		to_list/1, to_atom/1, get_id2/1, 
		get_type/1, get_body/1,
		skelfiled/1, stubfiled/1,
		push_file/2, pop_file/2, sys_file/2]).

-import(lists, [foreach/2, foldr/3, map/2]).


-include("icforms.hrl").
-include("ic.hrl").

-include_lib("stdlib/include/erl_compile.hrl").

%%------------------------------------------------------------
%%
%% Generate the client side Erlang stubs.
%%
%% Each module is generated to a separate file.
%%
%% Export declarations for all interface functions must be
%% generated. Each function then needs to generate a function head and
%% a body. IDL parameters must be converted into Erlang parameters
%% (variables, capitalised) and a type signature list must be
%% generated (for later encode/decode).
%%
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    GT = get_opt(G, be),
    G2 = icgen:filename_push(G, [], mk_oe_name(G, 
					       icgen:remove_ext(to_list(File))),
			     erlang),
    R = if
	    GT == erl_corba ->
		case icgen:is_stubfile_open(G2) of
		    true ->
			emit(stubfiled(G2), "-include_lib(\"~s/include/~s\").\n\n", 
			     [?ORBNAME, ?IFRTYPESHRL]);
		    false -> ok
		end,
		gen_head(G2, [], Form),
		icgen:export(icgen:stubfiled(G2), [{ictk:register_name(G2), 0},
						   {ictk:unregister_name(G2), 0},
						   {oe_dependency,0}]),
		R0= gen(G2, [], Form),
		ictk:reg_gen(G2, [], Form),
		ictk:unreg_gen(G2, [], Form), % "new" unreg_gen/3
		genDependency(G2), % creates code for dependency list
		R0;
	    true ->
		gen_head(G2, [], Form),
		gen(G2, [], Form)
	end,
    icgen:filename_pop(G2, erlang),
    R.


gen(G, N, [X|Xs]) when record(X, preproc) ->
    NewG = ic:handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);

gen(G, N, [X|Xs]) when record(X, module) ->
    CD = icgen:codeDirective(G,X),
    G2 = icgen:filename_push(G, N, X, CD),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    G3 = icgen:filename_pop(G2, CD),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, interface) ->
    G2 = icgen:filename_push(G, N, X, erlang),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    foreach(fun({Name, Body}) -> gen(G2, N2, Body) end, 
	    X#interface.inherit_body),
    gen_serv(G2, N, X), 
    G3 = icgen:filename_pop(G2, erlang),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when record(X, const) ->
    N2 = [get_id2(X) | N],
%%    icstruct:struct_gen(G, N2, get_type(X)),
    emit_constant_func(G, X#const.id, X#const.val),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, op) ->
    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
    emit_stub_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, attr) ->
    emit_attr(G, N, X, fun emit_stub_func/7),
    gen(G, N, Xs);

%%    XX = #id_of{type=X},
%%    {GetType, SetType} = mk_attr_func_types(N, X),
%%    lists:foreach(fun(Id) ->
%%			  X2 = XX#id_of{id=Id},
%%			  {Get, Set} = mk_attr_func_names(N, get_id(Id)),
%%			  emit_stub_func(G, N, X2, Get, [], GetType, []),
%%			  case X#attr.readonly of
%%			      {readonly, _} -> ok;
%%			      _ -> 
%%				  emit_stub_func(G, N, X2, Set, 
%%						 [mk_name(G, "Value")], 
%%						 SetType, [])
%%			  end end, icgen:get_idlist(X)),
%%    gen(G, N, Xs);

gen(G, N, [X|Xs]) when record(X, except) ->
    icstruct:except_gen(G, N, X, erlang),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, erlang);
	false -> ok
    end,
    gen(G, N, Xs);

gen(G, N, []) -> ok.


may_contain_structs(X) when record(X, typedef) -> true;
may_contain_structs(X) when record(X, struct) -> true;
may_contain_structs(X) when record(X, union) -> true;
may_contain_structs(X) -> false.



%%--------------------------------------------------------------------
%%
%% Generate the server side (handle_call and handle_cast)
%%

gen_serv(G, N, X) ->
    case icgen:is_stubfile_open(G) of
	true ->
	    GT = get_opt(G, be),
	    emit_serv_std(GT, G, N, X),
	    N2 = [get_id2(X) | N], 
	    gen_calls(G, N2, get_body(X)),
	    lists:foreach(fun({Name, Body}) ->
				  gen_calls(G, N2, Body) end,
			  X#interface.inherit_body),
	    get_if_gen(G, N2, X),
	    gen_end_of_call(GT, G, N, X),		% Note N instead of N2

	    gen_casts(G, N2, get_body(X)),
	    lists:foreach(fun({Name, Body}) ->
				  gen_casts(G, N2, Body) end,
			  X#interface.inherit_body),
	    gen_end_of_cast(GT, G, N, X),		% Note N instead of N2
	    emit_skel_footer(GT, G, N, X);		% Note N instead of N2
	false ->
	    ok
    end.

gen_calls(G, N, [X|Xs]) when record(X, op) ->
    case is_oneway(X) of
	false ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
	    gen_calls(G, N, Xs);
	true ->
	    gen_calls(G, N, Xs)
    end;

gen_calls(G, N, [X|Xs]) when record(X, attr) ->
    emit_attr(G, N, X, fun emit_skel_func/7),
    gen_calls(G, N, Xs);

gen_calls(G, N, [X|Xs]) -> gen_calls(G, N, Xs);
gen_calls(G, N, []) -> ok.

gen_casts(G, N, [X|Xs]) when record(X, op) ->
    case is_oneway(X) of
	true ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
%%	    io:format("Generateing oneway ~p~n", [Name]),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
	    gen_casts(G, N, Xs);
	false ->
	    gen_casts(G, N, Xs)
    end;

gen_casts(G, N, [X|Xs]) -> gen_casts(G, N, Xs);
gen_casts(G, N, []) -> ok.

emit_attr(G, N, X, F) ->
    XX = #id_of{type=X},
    {GetType, SetType} = mk_attr_func_types(N, X),
    lists:foreach(fun(Id) ->
			  X2 = XX#id_of{id=Id},
			  {Get, Set} = mk_attr_func_names(N, get_id(Id)),
			  F(G, N, X2, Get, [], GetType, []),
			  case X#attr.readonly of
			      {readonly, _} -> ok;
			      _ -> 
				  F(G, N, X2, Set, [mk_name(G, "Value")], 
				    SetType, [])
			  end end, icgen:get_idlist(X)).


extract_info(G, N, X) when record(X, op) ->
    Name	= get_id2(X),
    InArgs	= ic:filter_params([in,inout], X#op.params),
    OutArgs	= ic:filter_params([out,inout], X#op.params),
    ArgNames	= mk_erl_vars(G, InArgs),
    S = icgen:tktab(G),
    TypeList	= {ic_forms:get_tk(X),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, InArgs),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, OutArgs)
		  },
    {Name, ArgNames, TypeList, OutArgs}.



%% This function generates the standard functions of an object
%% gen_server
emit_serv_std(erl_corba, G, N, X) ->
    Fd		= icgen:stubfiled(G),
    Reply	= mk_name(G, "Reply"),
    State	= mk_name(G, "State"),
    NewState	= mk_name(G, "NewState"),
    Reason	= mk_name(G, "Reason"),
    Timeout	= mk_name(G, "Timeout"),

    Var1	= mk_name(G, "1"),
    Var2	= mk_name(G, "2"),
    Var3	= mk_name(G, "3"),
    Var4	= mk_name(G, "4"),

    Impl	= icgen:impl(G),
    TypeID = ictk:get_IR_ID(G, N, X),

    nl(Fd), nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Object server implementation."]),
    nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Function for fetching the interface type ID."]),
    nl(Fd), 
    emit(Fd, "typeID() ->\n"),
    emit(Fd, "    \"~s\".\n", [TypeID]),
    nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Object creation functions."]),
    nl(Fd), 
    emit(Fd, "oe_create() ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\").\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link() ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\").\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create(Env) ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\", Env).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link(Env) ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\", Env).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create(Env, RegName) ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\", Env, RegName).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link(Env, RegName) ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\", Env, RegName).\n", [TypeID]),
    nl(Fd),
      icgen:mcomment(Fd, ["Start functions."]),
    nl(Fd), 
    emit(Fd, "start(Env) ->\n"),
    emit(Fd, "    gen_server:start(?MODULE, Env, []).\n"),
    nl(Fd),
    emit(Fd, "start_link(Env) ->\n"),
    emit(Fd, "    gen_server:start_link(?MODULE, Env, []).\n"),
    nl(Fd),
    icgen:mcomment(Fd, ["Init & terminate functions."]),
    nl(Fd), 
    emit(Fd, "init(Env) ->\n"),
    icgen:comment(Fd, "Call to implementation init"),
    emit(Fd, "    ~p:~p(Env).\n", [to_atom(Impl), init]),
    nl(Fd),
    emit(Fd, "terminate(Reason, State) ->\n"),
    emit(Fd, "    catch (~p:~p(Reason, State)).\n", 
	 [to_atom(Impl), terminate]),
    nl(Fd),
    icgen:mcomment(Fd, ["Call & Cast macros."]),
    nl(Fd), 

    %%
    %% gen_server's requirements on return values from handle_call:
    %%
    %% {reply, Reply, State} 
    %% {reply, Reply, State,  Timeout}
    %% {noreply,  State}
    %% {noreply,  State, Timeout} 
    %% {stop, StopReason, Reply, State}
    %% {stop, StopReason, State}
    %%
    emit(Fd, "-define(~s(Mod, Fun, Args),\n", [mk_name(G, "CALL")]),
    emit(Fd, "        Result = case catch apply(Mod, Fun, Args) of\n"),
    emit(Fd, "            {'EXIT', OE} -> exit(OE);\n"),
    emit(Fd, "            ~s when element(1, ~s)=='EXCEPTION' ->\n",
	 [Var1, Var1]),
    emit(Fd, "                {reply, ~s, ~s};\n", [Var1, State]),

    emit(Fd, "            ~s when element(1, ~s)=='reply' ->\n",
	 [Var2, Var2]),
    emit(Fd, "                ~s;~n", [Var2]),

    emit(Fd, "            ~s when element(1, ~s)=='noreply' ->\n",
	 [Var3, Var3]),
    emit(Fd, "                ~s;~n", [Var3]),

    emit(Fd, "            ~s when element(1, ~s)=='stop' ->\n",
	 [Var4, Var4]),
    emit(Fd, "                ~s;~n", [Var4]),


    %% Old format for return values (this line is just for backward compatability)
    emit(Fd, "            {~s, ~s} -> {reply, ~s, ~s}~n", 
	 [Reply, NewState, Reply, NewState]),
    emit(Fd, "        end).", []),
    nl(Fd), 

    %%
    %% gen_server's requirements on return values from handle_cast:
    %%
    %% {noreply,  State}
    %% {noreply,  State, Timeout} 
    %% {stop, StopReason, State}
    %%
    emit(Fd, "-define(~s(Mod, Fun, Args),\n", [mk_name(G, "CAST")]),
    emit(Fd, "        Result = case catch apply(Mod, Fun, Args) of\n"),
    emit(Fd, "            {'EXIT', OE} -> exit(OE);\n"),
    emit(Fd, "            ~s when element(1, ~s)=='EXCEPTION' ->\n",
	 [Var1, Var1]),
    emit(Fd, "                {noreply, ~s};~n", [State]),

    emit(Fd, "            ~s when element(1, ~s)=='noreply' ->\n",
	 [Var2, Var2]),
    emit(Fd, "                ~s;~n", [Var2]),

    emit(Fd, "            ~s when element(1, ~s)=='stop' ->\n",
	 [Var3, Var3]),
    emit(Fd, "                ~s;~n", [Var3]),

    %% Old format for return values (this line is just for backward compatability)
    emit(Fd, "            ~s -> {noreply, ~s}~n", [NewState, NewState]),

    emit(Fd, "        end).\n"),

    nl(Fd), 
    nl(Fd), nl(Fd),
    Fd;
emit_serv_std(erl_genserv, G, N, X) ->
    Fd		= icgen:stubfiled(G),
    Ret		= mk_name(G, "Ret"),
    State	= mk_name(G, "State"),
    NewState	= mk_name(G, "NewState"),
    Var		= mk_name(G, ""),
    Reason	= mk_name(G, "Reason"),
    Impl	= icgen:impl(G),
    TypeID = ictk:get_IR_ID(G, N, X),

    nl(Fd), nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Server implementation."]),
    nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Function for fetching the interface type ID."]),
    nl(Fd), 
    emit(Fd, "typeID() ->\n"),
    emit(Fd, "    \"~s\".\n", [TypeID]),
    nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Server creation functions."]),
    nl(Fd), 
    emit(Fd, "oe_create() ->\n"),
    emit(Fd, "    start([], []).\n", []),
    nl(Fd),
    emit(Fd, "oe_create_link() ->\n"),
    emit(Fd, "    start_link([], []).\n", []),
    nl(Fd),
    emit(Fd, "oe_create(Env) ->\n"),
    emit(Fd, "    start(Env, []).\n", []),
    nl(Fd),
    emit(Fd, "oe_create_link(Env) ->\n"),
    emit(Fd, "    start_link(Env, []).\n", []),
    nl(Fd),
    emit(Fd, "oe_create(Env, RegName) ->\n"),
    emit(Fd, "    start(RegName, Env, []).\n", []),
    nl(Fd),
    emit(Fd, "oe_create_link(Env, RegName) ->\n"),
    emit(Fd, "    start_link(RegName, Env, []).\n", []),
    nl(Fd),
    icgen:mcomment(Fd, ["Start functions."]),
    nl(Fd), 
    emit(Fd, "start(Env, Opt) ->\n"),
    emit(Fd, "    gen_server:start(?MODULE, Env, Opt).\n"),
    nl(Fd),
    emit(Fd, "start_link(Env, Opt) ->\n"),
    emit(Fd, "    gen_server:start_link(?MODULE, Env, Opt).\n"),
    nl(Fd),
    emit(Fd, "start(RegName, Env, Opt) ->\n"),
    emit(Fd, "    gen_server:start(RegName, ?MODULE, Env, Opt).\n"),
    nl(Fd),
    emit(Fd, "start_link(RegName, Env, Opt) ->\n"),
    emit(Fd, "    gen_server:start_link(RegName, ?MODULE, Env, Opt).\n"),
    nl(Fd),
    icgen:comment(Fd, "Standard gen_server termination"),
    emit(Fd, "stop(OE_THIS) ->\n"),
    emit(Fd, "    gen_server:cast(OE_THIS,stop).\n"),
    nl(Fd),
    icgen:comment(Fd, "Call to implementation init"),
    emit(Fd, "init(Env) ->\n"),
    emit(Fd, "    ~p:~p(Env).\n", [to_atom(Impl), init]),
    nl(Fd),
    emit(Fd, "terminate(Reason, State) ->\n"),
    emit(Fd, "    ~p:~p(Reason, State).\n", 
	 [to_atom(Impl), terminate]),
    nl(Fd), nl(Fd),
    Fd.

gen_end_of_call(erl_corba, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server call handle"]),
    emit(Fd, "handle_call(stop, From, State) ->\n"),
    emit(Fd, "    {stop, normal, ok, State}"),
    case get_opt(G, serv_last_call) of
	exception ->
	    emit(Fd, ";\n"),
	    nl(Fd),
	    emit(Fd, "handle_call(Req, From, State) ->\n"),
	    emit(Fd, "    {reply, catch corba:raise(#'BAD_OPERATION'{completion_status='COMPLETED_NO'}), State}.\n");
	exit ->
	    emit(Fd, ".\n"),
	    nl(Fd),
	    nl(Fd)
    end,
    ok;
gen_end_of_call(erl_genserv, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server call handle"]),
    emit(Fd, "handle_call(stop, From, State) ->\n"),
    emit(Fd, "    {stop, normal, ok, State}"),
    emit(Fd, ".\n"),
    nl(Fd), nl(Fd),
    ok.

gen_end_of_cast(erl_corba, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server cast handle"]),
    emit(Fd, "handle_cast(stop, State) ->\n"),
    emit(Fd, "    {stop, normal, State}"),
    case get_opt(G, serv_last_call) of
	exception ->
	    emit(Fd, ";\n"),
	    nl(Fd),
	    emit(Fd, "handle_cast(Req, State) ->\n"),
	    emit(Fd, "    {reply, catch corba:raise(#'BAD_OPERATION'{completion_status='COMPLETED_NO'}), State}.\n");
	exit ->
	    emit(Fd, ".\n"),
	    nl(Fd), nl(Fd)
    end,
    ok;
gen_end_of_cast(erl_genserv, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server cast handle"]),
    emit(Fd, "handle_cast(stop, State) ->\n"),
    emit(Fd, "    {stop, normal, State}"),
    emit(Fd, ".\n"),
    nl(Fd), nl(Fd),   
    ok.

emit_skel_footer(erl_corba, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server handles"]),
    emit(Fd, "handle_info(Info, State) ->\n"),
    case use_impl_handle_info(G, N, X) of
	true ->
	    emit(Fd, "    ~p:handle_info(Info, State).\n\n", 
		 [list_to_atom(icgen:impl(G))]);
	false ->
	    emit(Fd, "    {reply, catch corba:raise(#'BAD_OPERATION'{completion_status='COMPLETED_NO'}), State}.\n\n")
    end,
    nl(Fd),
    emit(Fd, "code_change(OldVsn, State, Extra) ->\n"),    
    case get_opt(G, no_codechange) of
	false ->
	    emit(Fd, "    ~p:code_change(OldVsn, State, Extra).\n\n", 
		 [list_to_atom(icgen:impl(G))]);
	true ->
	    emit(Fd, "    {ok, State}.\n\n")
    end,
    ok;
emit_skel_footer(erl_genserv, G, N, X) ->
    Fd = icgen:stubfiled(G),
    nl(Fd), nl(Fd),
    icgen:mcomment_light(Fd, ["Standard gen_server handles"]),
    emit(Fd, "handle_info(Info, State) ->\n"),
    case use_impl_handle_info(G, N, X) of
	true ->
	    emit(Fd, "    ~p:handle_info(Info, State).\n\n", 
		 [list_to_atom(icgen:impl(G))]);
	false ->
	    emit(Fd, "    {noreply, State}.\n\n")
    end,
    nl(Fd), nl(Fd),
    emit(Fd, "code_change(OldVsn, State, Extra) ->\n"),    
    case get_opt(G, no_codechange) of
	false ->
	    emit(Fd, "    ~p:code_change(OldVsn, State, Extra).\n\n", 
		 [list_to_atom(icgen:impl(G))]);
	true ->
	    emit(Fd, "    {ok, State}.\n\n")
    end,
    ok.


use_impl_handle_info(G, N, X) ->
    FullName = icgen:to_colon([get_id2(X) | N]),
    case {get_opt(G, handle_info), get_opt(G, {handle_info, FullName})} of
	{false, false} -> false;
	_ -> true
    end.

use_timeout(G, N, X) ->
    FullName = icgen:to_colon(N),
    case {get_opt(G, timeout), get_opt(G, {timeout, FullName})} of
	{false, false} -> false;
	_ -> true
    end.

use_precond(G, N, X) ->
    FullName = icgen:to_colon([get_id2(X) | N]),
    case get_opt(G, {precond, FullName}) of
	false -> 
	    InterfaceName = icgen:to_colon(N),
	    case get_opt(G, {precond, InterfaceName}) of
		false ->
		    case get_opt(G, precond) of
			false -> false;
			V2 -> V2
		    end;
		V2 -> V2
	    end;
	V1 -> V1
    end.

use_postcond(G, N, X) ->
    FullName = icgen:to_colon([get_id2(X) | N]),
    case get_opt(G, {postcond, FullName}) of
	false -> 
	    InterfaceName = icgen:to_colon(N),
	    case get_opt(G, {postcond, InterfaceName}) of
		false ->
		    case get_opt(G, postcond) of
			false -> false;
			V3 -> V3
		    end;
		V2 -> V2
	    end;
	V1 -> V1
    end.

get_if_name(G) -> mk_oe_name(G, "get_interface").

%% Generates the get_interface function (for Lars)
get_if_gen(G, N, X) ->
%%    ?DBG("Get I/F gen~n", []),
%%    S = icgen:tktab(G),
    Backend = get_opt(G, be),
    case {icgen:is_stubfile_open(G), Backend} of
	{true, erl_corba} ->
	    B = 
		foldr(fun({Name, Body}, Acc) ->
			      get_if(G,N,Body,Backend)++Acc end,
		      get_if(G,N,get_body(X),Backend),
		      X#interface.inherit_body),
	    Fd = icgen:stubfiled(G),
	    Name = to_atom(get_if_name(G)),
	    icgen:mcomment_light(Fd, 
				 [io_lib:format("Standard Operation: ~p",
						[Name])]),
	    emit(Fd, "handle_call({~s, ~p, []}, From, State) ->~n",
		 [mk_name(G, "THIS"), Name]),
	    emit(Fd, "    {reply, ~p, State};~n", [B]),
	    nl(Fd),
	    ok;
	_ -> ok
    end.


get_if(G,N,[X|Rest],erl_corba) when record(X, op) ->
    R = ic_forms:get_tk(X),
    IN = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		   ic:filter_params([in, inout], X#op.params)),
    OUT = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		    ic:filter_params([out, inout], X#op.params)),
    [{get_id2(X), {R, IN, OUT}} | get_if(G,N,Rest,erl_corba)];

get_if(G,N,[X|Rest],erl_corba) when record(X, attr) ->
    {GetT, SetT} = mk_attr_func_types([], X),
    AList = lists:map(fun(Id) -> 
			      {Get, Set} = mk_attr_func_names([], get_id(Id)),
			      case X#attr.readonly of
				  {readonly, _} -> 
				      {Get, GetT};
				  _ -> 
				      [{Set, SetT}, {Get, GetT}]
			      end end, icgen:get_idlist(X)),
    lists:flatten(AList) ++ get_if(G,N,Rest,erl_corba);

get_if(G,N,[X|Rest],erl_genserv) when record(X, op) ->
    FunName = case icgen:get_opt(G, scoped_op_calls) of 
		  true -> 
		      icgen:to_undersc([get_id2(X) | N]);
		  false ->
		      get_id2(X)
	      end,
    R = ic_forms:get_tk(X),
    IN = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		   ic:filter_params([in, inout], X#op.params)),
    OUT = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		    ic:filter_params([out, inout], X#op.params)),
    [{FunName, {R, IN, OUT}} | get_if(G,N,Rest,erl_genserv)];

get_if(G,N,[X|Rest],erl_genserv) when record(X, attr) ->
    {GetT, SetT} = mk_attr_func_types([], X),
    AList = lists:map(fun(Id) -> 
			      {Get, Set} = mk_attr_func_names([], get_id(Id)),
			      case X#attr.readonly of
				  {readonly, _} -> 
				      {Get, GetT};
				  _ -> 
				      [{Set, SetT}, {Get, GetT}]
			      end end, icgen:get_idlist(X)),
    lists:flatten(AList) ++ get_if(G,N,Rest,erl_genserv);

get_if(G,N,[X|Rest],Backend) -> get_if(G,N,Rest,Backend);
get_if(_,_,[],_) -> [].



%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when record(X, interface) ->
    Fd = icgen:stubfiled(G),

    foreach(fun({Name, Body}) ->
		    icgen:comment(Fd, "Exports from ~p", 
				  [icgen:to_colon(Name)]),
		    icgen:export(Fd, exp_top(G, N, Body, [])),
		    nl(Fd)
	    end, X#interface.inherit_body),
    %% nl(Fd), nl(Fd),
    icgen:comment(Fd, "Type identification function"),
    icgen:export(Fd, [{typeID, 0}]), %%%, {to_atom(get_if_name(G)), 1}]),
    nl(Fd),
    icgen:comment(Fd, "Used to start server"),
    icgen:export(Fd, [{oe_create, 0}, {oe_create_link, 0}, {oe_create, 1}, {oe_create_link, 1},
		      {oe_create, 2}, {oe_create_link, 2}]),
    nl(Fd),
    case get_opt(G, be) of
	erl_corba ->
	    icgen:export(Fd, [{start, 1}, {start_link, 1}]);
	_ ->
	    icgen:export(Fd, [{start, 2}, {start_link, 3}])
    end,
    nl(Fd),
    icgen:comment(Fd, "gen server export stuff"),
    emit(Fd, "-behaviour(gen_server).\n"),

    case get_opt(G, be) of
	erl_genserv -> %% stop/1 is only for erl_genserv backend
	    icgen:export(Fd, [{stop, 1}, {init, 1}, {terminate, 2}, {handle_call, 3}, 
			      {handle_cast, 2}, {handle_info, 2}, {code_change, 3}]);
	_ ->
	     icgen:export(Fd, [{init, 1}, {terminate, 2}, {handle_call, 3}, 
			      {handle_cast, 2}, {handle_info, 2}, {code_change, 3}])
    end,

    case get_opt(G, be) of
	erl_corba ->
	    nl(Fd),
	    emit(Fd, "-include_lib(\"~s/include/~s\").\n", [?ORBNAME, ?CORBAHRL]);
	_ ->
	    ok
    end,
    nl(Fd), nl(Fd),
    icgen:mcomment(Fd, ["Object interface functions."]),
    nl(Fd), nl(Fd), nl(Fd),
    Fd;

%% Some items have extra exports
%%gen_head_special(G, N, X) when record(X, module) ->
%%    Fd = icgen:stubfiled(G),
%%    nl(Fd),
%%    icgen:comment(Fd, "Specific exports for modules\n"),
%%    icgen:export(Fd, [{ictk:register_name(G), 0},
%%		      {ictk:unregister_name(G), 0}]);

gen_head_special(G, N, X) -> ok.

    

%% Shall generate all export declarations
gen_head(G, N, X) -> 
    case icgen:is_stubfile_open(G) of
	true -> 
	    F = stubfiled(G),
	    icgen:comment(F, "Interface functions"),
	    icgen:export(F, exp_top(G, N, X, [])),
	    nl(F),
	    gen_head_special(G, N, X);
	false -> ok
    end.

exp_top(G, N, X, Acc)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, Acc)  when list(L) ->
    exp_list(G, N, L, Acc);
exp_top(G, N, M, Acc)  when record(M, module) ->
    exp_list(G, N, get_body(M), Acc);
exp_top(G, N, I, Acc)  when record(I, interface) ->
    exp_list(G, N, get_body(I), Acc);
exp_top(G, N, X, Acc) ->
    exp3(G, N, X, Acc).

exp3(G, N, C, Acc)  when record(C, const) ->
    [{get_id(C#const.id), 0} | Acc];
exp3(G, N, Op, Acc)  when record(Op, op) ->
    FuncName = get_id(Op#op.id),

    Arity = 
	case use_timeout(G,N,Op) of
	    true ->
		 %% NO TimeOut on ONEWAYS here !!!!
		case is_oneway(Op) of 
		    true ->
			length(ic:filter_params([in, inout], Op#op.params)) + 1;
		    false ->
			length(ic:filter_params([in, inout], Op#op.params)) + 2
		end;
	    false ->
		length(ic:filter_params([in, inout], Op#op.params)) + 1
	end,
    [{FuncName, Arity} | Acc];

exp3(G, N, A, Acc)  when record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1} | Acc2];
			    _ ->             [{Get, 1}, {Set, 2} | Acc2]
			end end, Acc, icgen:get_idlist(A));

exp3(G, N, X, Acc) -> Acc.

exp_list(G, N, L, OrigAcc) -> 
    lists:foldr(fun(X, Acc) -> exp3(G, N, X, Acc) end, OrigAcc, L).




%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%

emit_stub_func(G, N, X, Name, ArgNames, TypeList, OutArgs) ->
    case icgen:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = icgen:stubfiled(G),
%%%	    io:format("Stub: n: ~p filescope: ~p~n", [N, stubscope(G)]),
	    StubName = list_to_atom(Name),
	    UsingTimeout = use_timeout(G, N, X),
	    Timeout = case UsingTimeout of
			  true ->
			      mk_name(G, "Timeout");
			  false ->
			      "infinity"
		      end,
	    This = mk_name(G, "THIS"),
	    CallOrCast = 
		case is_oneway(X) of 
		    true -> ?CAST; 
		    _ -> ?CALL
		end,
	    emit_op_comment(G, Fd, X, StubName, ArgNames, OutArgs),
	    case get_opt(G, be) of
		erl_corba ->
		    %% NO TimeOut on ONEWAYS here !!!!
		    case is_oneway(X) of 
			true ->
			    emit(Fd, "~p(~s) ->\n", 
				 [StubName, mk_list([This | ArgNames])]);
			false ->
			    case UsingTimeout of
				true ->
				    emit(Fd, "~p(~s) ->\n", 
					 [StubName, mk_list([This, Timeout| ArgNames])]);
				false ->
				    emit(Fd, "~p(~s) ->\n", 
					 [StubName, mk_list([This | ArgNames])])
			    end
		    end,
			    
		    %% NO TimeOut on ONEWAYS here !!!!
		    case is_oneway(X) of 
			true ->
			    emit(Fd, "    ~s:~s(~s, ~p, [~s], ~p).\n\n", 
				 [?CORBAMOD, CallOrCast, This, StubName, mk_list(ArgNames),
				  TypeList]);
			false ->
			    emit(Fd, "    ~s:~s(~s, ~p, [~s], ~p, ~s).\n\n", 
				 [?CORBAMOD, CallOrCast, This, StubName, mk_list(ArgNames),
				  TypeList, Timeout])
		    end;
		_  ->
		    FunName = case icgen:get_opt(G, scoped_op_calls) of 
				  true -> 
				      list_to_atom(icgen:to_undersc([Name | N]));
				  false ->
				      StubName
			      end,
		    %% NO TimeOut on ONEWAYS here !!!!
		    case is_oneway(X) of 
			true ->
			    emit(Fd, "~p(~s) ->\n", 
				 [StubName, mk_list([This | ArgNames])]);
			false ->
			    case UsingTimeout of
				true ->
				    emit(Fd, "~p(~s) ->\n",
					 [StubName, mk_list([This, Timeout| ArgNames])]);
				false ->
				    emit(Fd, "~p(~s) ->\n", 
					 [StubName, mk_list([This | ArgNames])])
			    end
		    end,

		    %% NO TimeOut on ONEWAYS here !!!!
		    if 
			length(ArgNames) == 0 ->
			    case is_oneway(X) of 
				true ->
				    emit(Fd, "    ~s:~s(~s, ~p).\n\n",
					 [?GENSERVMOD, CallOrCast, This, FunName]);
				false ->
				    emit(Fd, "    ~s:~s(~s, ~p, ~s).\n\n",
					 [?GENSERVMOD, CallOrCast, This, FunName, Timeout])
			    end;
			true ->
			    case is_oneway(X) of 
				true ->
				    emit(Fd, "    ~s:~s(~s, {~p, ~s}).\n\n", 
					 [?GENSERVMOD, CallOrCast, This, FunName,
					  mk_list(ArgNames)]);
				false ->
				    emit(Fd, "    ~s:~s(~s, {~p, ~s}, ~s).\n\n", 
					 [?GENSERVMOD, CallOrCast, This, FunName,
					  mk_list(ArgNames), Timeout])
			    end
		    end
	    end
    end.

emit_skel_func(G, N, X, OpName, ArgNames, TypeList, OutArgs) ->
    case icgen:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = stubfiled(G),
	    Name	= list_to_atom(OpName),
	    ImplF	= Name,
	    ImplM	= list_to_atom(icgen:impl(G)),
	    This	= mk_name(G, "THIS"),
	    TL		= mk_name(G, "Types"),
	    From	= mk_name(G, "From"),
	    State	= mk_name(G, "State"),

	    %% Create argument list
	    CallArgs1 = [State | ArgNames],
	    CallArgs2 =
		case is_oneway(X) of
		    false ->
			case use_from(G, N, OpName) of
			    true ->
				[From | CallArgs1];
			    false ->
				CallArgs1
			end;
		    true ->
			CallArgs1
		end,
	    CallArgs3 =
		case use_this(G, N, OpName) of
		    true ->
			case get_opt(G, be) of
			    erl_corba ->
				[This | CallArgs2];
			    erl_genserv ->
				CallArgs2
			end;
		    false ->
			CallArgs2
		end,
	    %% Create argument list string
	    CallArgs = mk_list(CallArgs3),
	    emit_op_comment(G, Fd, X, Name, ArgNames, OutArgs),
	    case get_opt(G, be) of
		erl_corba ->
		    VarPrecond	= mk_name(G, "Precond"),
		    VarPostcond	= mk_name(G, "Postcond"),

		    %% Check if pre and post conditions are specified for this operation
		    Precond = use_precond(G, N, X),
		    Postcond = use_postcond(G, N, X),

		    case is_oneway(X) of
			true ->
			    emit(Fd, "handle_cast({~s, ~p, [~s]}, ~s) ->\n",
				 [This, Name, mk_list(ArgNames), State]),
			    emit_precond(Fd, true, Precond, VarPrecond, 
					 Name, CallArgs, State),
			    emit(Fd, "    ?~s(~p, ~p, [~s])", 
				 [mk_name(G, "CAST"), ImplM, ImplF, CallArgs]),
			    emit_postcond(Fd, true, Precond, Postcond, VarPostcond,
					  Name, CallArgs, State);
			false ->
			    emit(Fd, "handle_call({~s, ~p, [~s]}, ~s, ~s) ->\n",
				 [This, Name, mk_list(ArgNames), From, State]),
			    emit_precond(Fd, false, Precond, VarPrecond,  Name, 
					 CallArgs, State),
			    emit(Fd, "  ?~s(~p, ~p, [~s])", 
				 [mk_name(G, "CALL"), ImplM, ImplF, CallArgs]),
			    emit_postcond(Fd, false, Precond, Postcond, VarPostcond,
					  Name, CallArgs, State)
		    end;
		erl_genserv ->
		    FunName = case icgen:get_opt(G, scoped_op_calls) of 
				  true -> 
				      icgen:to_undersc([OpName | N]);
				  false ->
				      OpName
			      end,
		    case is_oneway(X) of
			true ->
			    if
				length(ArgNames) == 0 ->
				    emit(Fd, "handle_cast(~p, ~s) ->\n",
					 [list_to_atom(FunName), State]);
				true ->
				    emit(Fd, "handle_cast({~s}, ~s) ->\n",
					 [ mk_list([FunName | ArgNames]), State])
			    end,
			    emit(Fd, "    ~p:~p(~s);\n\n", 
				 [ImplM, ImplF, CallArgs]);
			false ->
			    if
				length(ArgNames) == 0 ->
				    emit(Fd, "handle_call(~p, ~s, ~s) ->\n",
					 [list_to_atom(FunName), From, State]);
				true ->
				    emit(Fd, "handle_call({~p, ~s}, ~s, ~s) ->\n",  
					 [list_to_atom(FunName), 
					  mk_list(ArgNames), 
					  From, State])
			    end,
			    emit(Fd, "    ~p:~p(~s);\n\n", 
				 [ImplM, ImplF, CallArgs])
		    end
	    end
%%	    emit(Fd, "~p({~s, ~p, [~s], ~s},",
%%		 [CallOrCast1, This, Name, mk_list(ArgNames), TL]),
%%	    emit(Fd, " ~s, ~s) ->\n", [From, State]),
%%	    emit(Fd, "    ?~s(~p:~p(~s));\n\n", 
%%		 [CallOrCast2, ImplM, ImplF, mk_list(CallArgs)])
    end.

emit_precond(Fd, CallOrCast, Precond, VarPrecond, F, A, State) ->
    case Precond of 
	{PreM, PreF} ->
	    emit(Fd, "  case catch ~p:~p(?MODULE, ~s, [~s]) of\n",
		 [PreM, PreF, F, A]),
	    emit(Fd, "    {'EXIT', OE_precond_exit} -> exit(OE_precond_exit);\n"),
	    emit(Fd, "    ~s when element(1, ~s)=='EXCEPTION' ->\n",
		 [VarPrecond, VarPrecond]),
	    case CallOrCast of
		true ->
		    emit(Fd, "          {noreply, ~s};~n", [State]);
		false ->
		    emit(Fd, "         {reply, ~s, ~s};\n", [VarPrecond, State])
	    end,
	    emit(Fd, "    ok ->\n      ");
	_ ->
	    ok
    end.

emit_postcond(Fd, CallOrCast, Precond, Postcond, VarPostcond, F, A, State) ->
    case Postcond of 
	{PostM, PostF} ->
	    emit(Fd, ",\n"),
	    emit(Fd, "        case catch ~p:~p(?MODULE, ~s, [~s], Result) of\n",
		 [PostM, PostF, F, A]),
	    emit(Fd, "          {'EXIT', OE_postcond_exit} -> exit(OE_postcond_exit);\n"),
	    emit(Fd, "          ~s when element(1, ~s)=='EXCEPTION' ->\n",
		 [VarPostcond, VarPostcond]),

	    case CallOrCast of
		true ->
		    emit(Fd, "             {noreply, ~s};~n", [State]);
		false ->
		    emit(Fd, "             {reply, ~s, ~s};\n", [VarPostcond, State])
	    end,

	    emit(Fd, "          ok ->\n"),
	    emit(Fd, "             Result\n"),
	    case Precond of 
		{_, _} ->
		    emit(Fd, "        end\n"),
		    emit(Fd, "  end;\n\n");
		_ ->
		    emit(Fd, "        end;\n\n")
	    end;
	_ ->	    
	    case Precond of 
		{_, _} ->
		    emit(Fd, "\n"),
		    emit(Fd, "  end;\n\n");
		_ ->
		    emit(Fd, ";\n\n")
	    end
    end.

use_this(G, N, OpName) ->
    FullOp = icgen:to_colon([OpName|N]),
    FullIntf = icgen:to_colon(N),
%%%    io:format("Use this: ~p and ~p~n", [FullOp, FullIntf]),
    case {get_opt(G, {this, FullIntf}), get_opt(G, {this, FullOp})} of
	{_, force_false} -> false;
	{false, false} -> false;
	_ -> true
    end.

use_from(G, N, OpName) ->
    FullOp = icgen:to_colon([OpName|N]),
    FullIntf = icgen:to_colon(N),
%%%    io:format("Use from: ~p and ~p~n", [FullOp, FullIntf]),
    case {get_opt(G, {from, FullIntf}), get_opt(G, {from, FullOp})} of
	{_, force_false} -> false;
	{false, false} -> false;
	_ -> true
    end.


emit_constant_func(G, Id, Val) ->
    case icgen:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = stubfiled(G),
	    N = list_to_atom(get_id(Id)),
	    emit_const_comment(G, Fd, Id, N),
	    emit(Fd, "~p() -> ~p.\n\n", [N, Val])
    end.



emit_const_comment(G, F, X, Name) ->
    icgen:mcomment_light(F,
			 [io_lib:format("Constant: ~p", [Name])]).


emit_op_comment(G, F, X, Name, InP, OutP) ->
    icgen:mcomment_light(F,
			 [io_lib:format("~s: ~p", [get_title(X), Name]),
			  "",
			  get_returns(G, X, InP, OutP) |
			  get_raises(X)]).

get_title(X) when record(X, attr) -> "Attribute Operation";
get_title(X) -> "Operation".

get_raises(X) when record(X, op) ->
    if  X#op.raises == [] -> [];
	true ->
	    ["  Raises:  " ++ 
	     mk_list(lists:map({icgen, to_colon}, X#op.raises))]
    end;
get_raises(X) -> [].

get_returns(G, X, InP, []) ->
    "  Returns: RetVal";
get_returns(G, X, InP, OutP) ->
    "  Returns: "++mk_list(["RetVal" | mk_erl_vars(G, OutP)]).




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
    {"_get_" ++ Name, "_set_" ++ Name}.
%%    {scoped_name(Scope, "_get_"++Name), scoped_name(Scope, "_set_"++Name)}.

%% Returns TK of the Get and Set attribute functions.
mk_attr_func_types(N, X) ->
    TK = ic_forms:get_tk(X),
    {{TK, [], []}, {tk_void, [TK], []}}.
        


%%------------------------------------------------------------
%%
%% Generation utilities and common stuff
%%
%% Convenient stuff for generation
%%
%%------------------------------------------------------------


%% Input is a list of parameters (in parse form) and output is a list
%% of capitalised variable names. mk_var is in icgen
mk_erl_vars(G, Params) ->
    map(fun(P) -> mk_var(get_id(P#param.id)) end, Params).


%% mk_list produces a nice comma separated string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].


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




%% Code produce for dependency function
genDependency(G) ->
    Fd = stubfiled(G),
    nl(Fd),nl(Fd),
    icgen:comment(Fd, "Idl file dependency list function"), 
    emit(Fd, "oe_dependency() ->\n\n", []),
    emit(Fd, "    ~p.\n\n", [ic_pragma:get_dependencies(G)]).   
