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

%% The com_gen module generates stubs for com-interfaces, to make life
%% easier for the erlang programmer. Each function in an interface is
%% called through a stub in the corresponding generated
%% erlang-file. Stubs can be generated both for Dispatch-interfaces
%% and Virtual interfaces. This is the only practical way to use the
%% Virtual interface from COM. For more information, consult the
%% User's Guide.

-module(com_gen).
-author('jakob@gandalf').

-include("com_gen.hrl").

-compile(export_all).

%% ----------------------------------------------------------------------
%% options record, user-settable options, sent to most functions
%% ----------------------------------------------------------------------
-record(options, {names, prefix, prefix_these, suffix, class_suffix, verbose, multi_optional, dispatch_only,
		  include_base_virtual}).

%% ----------------------------------------------------------------------
%% get erlang module statement, according to virtual flag and options
%% ----------------------------------------------------------------------
get_module(O, Iname, virtual) ->
    get_module(O, Iname);
get_module(O, Iname, dispatch) ->
    get_module(O, get_dispatch_name(Iname, O#options.dispatch_only)).

get_dispatch_name(Iname, true) ->
    Iname;
get_dispatch_name(Iname, false) ->
    Iname ++ "_disp".

get_module(O, Iname) ->
    ["-module(", make_erl_name(O, Iname, ticks), ").\n\n"].

%% ----------------------------------------------------------------------
%% get erlang export statement
%% has to check for optional in parameters (see get_functions below)
%% ----------------------------------------------------------------------
get_export(O, coclass) ->
    "-export([iid/0, create_object/1, create_object/2, create_object/3, create_dispatch/1, create_dispatch/2]).\n\n".

get_export(O, enum, Enums) ->
    ["-export([name/0, com_name/0",
     get_exports(O, Enums),
     "]).\n\n"];
get_export(O, interface, Fns) ->
    ["-export([iid/0, query_interface/1", 
     get_exports(O, Fns),
     "]).\n\n"].

get_exports(O, E) ->
    [get_1_exports(O, Fn) || Fn <- E].

get_1_exports(O, {Fname, Ename, InvKinds, _FuncKind, _Offs, Params, _Retval, {Lo, Hi}}) ->
    [[", ", Ename, "/", integer_to_list(Nparams)] || Nparams <- lists:seq(Lo, Hi)];
get_1_exports(O, {Enumname, _Val}) ->
    [", ", make_erl_name(O, Enumname, ticks), "/0"].

commas([]) -> [];
commas([L]) -> L;
commas([[] | R]) -> commas(R);
commas([L | R]) ->
    [L | ([[", ", A] || A <- R, A =/= "" ])].

%% ----------------------------------------------------------------------
%% add two elements to the func tuple:
%% a parameter count tuple and a unique erlang name
%% ----------------------------------------------------------------------

add_parcounts_and_name(O, Ftuples) ->
    F= add_parcounts(Ftuples, O#options.multi_optional),
    add_names(O, F).

add_parcounts(Ftuples, Multipleinopts) ->
    [{F, I, K, O, P, R, count_params(P, Multipleinopts)} || {F, I, K, O, P, R} <- Ftuples].

check_clash(N, {Lo, Hi}, Sofar) ->
    case lists:keysearch(N, 1, Sofar) of
	{value, {_N, {Lo1, Hi1}}} ->
	    ((Lo >= Lo1) and (Lo =< Hi1)) or ((Lo1 >= Lo) and (Lo1 =< Hi));
	_ -> false
    end.

check_prop_name(O, {F, I, K, Offs, P, R, C}, Sofar) ->
    N0= make_erl_name(O, F, ticks),
    N= change_name(N0, check_clash(N0, C, Sofar), I),
    {{F, N, I, K, Offs, P, R, C}, [{N, C} | Sofar]}.


add_names(O, Ftuples) ->
    {L, _Sofar}=
	lists:mapfoldl(
	  fun(Ftuple, Sofar) ->
		  check_prop_name(O, Ftuple, Sofar) end,
	  [], Ftuples),
    L.

warn_change_name(Old, New) ->
    io:format("Warning: renaming ~s to ~s.\n", [Old, New]).

get_new_name(Fname, property_put) -> ["put_", Fname];
get_new_name(Fname, property_put_ref) -> ["putref_", Fname];
get_new_name(Fname, property_get) -> ["get_", Fname];
get_new_name(Fname, func) -> [Fname, "_"].

change_name(Fname, true, [M]) ->
    N= get_new_name(Fname, M),
    warn_change_name(Fname, N),
    N;
change_name(Fname, _, _) -> Fname.

in_optional(Flags) ->  lists:member(optional, Flags) and lists:member(in, Flags).

count_params(Params, Multipleinopts) ->
    Noouts= lists:filter(fun({_Name, Flags, _Type, _DefaultVal}) -> not lists:member(out, Flags) end,
			    Params),
    Hi= length(Noouts) + 1,
    case Multipleinopts of
	true ->
	    Lo= Hi - length(lists:filter(fun({_Name, Flags, _Type, _DefaultVal}) -> in_optional(Flags) end,
					 Noouts)),
	    {Lo, Hi};
	false ->
	    {Hi, Hi}
    end.

%% ----------------------------------------------------------------------
%% get functions in interface
%% generates stub code for functions
%% a lot is handled here:
%%  - type tuples for in and out parametes              (get_call_params)
%%  - unlistifying a single return value   		(get_result1 and get_result2)
%%  - packaging interfaces returned			( -"- )
%%  - handling return values from get_call as out parameter ( -"- )
%%  - each function is generated with different parameter count according to
%%    the number of optional parameters specified in the COM idl
%%
%% each stub generated looks like:
%% A   fn(I_, P1, P2) ->
%% B       [Result]=
%% C           erl_com:com_call(I_, 44,
%% D                            [{enum, P1}, {vt_str, P2}, {vt_str, out}]),
%% E       Result.


%% iterate over function list
get_functions(O, F) ->
    [[get_1_functions(O, Fn), "\n\n"] || Fn <- F].

%% generate several functions, add optional-count for each optional parameter, and
%% iterate over # of optional in parameters
get_1_functions(O, {Fname, Ename, InvKinds, FuncKind, Offs, Params, Retval, {Lo, Hi}}) ->
    progress(O, 3, "function ~w ~s to ~s\n", [FuncKind, Fname, Ename]),
    ParamsWithOptCount0= add_opt_param_count(Params),
    ParamsWithOptCount= fix_optional_outs(ParamsWithOptCount0, FuncKind),
    [[get_1_function(O, Fname, Ename, InvKinds, FuncKind, Offs, ParamsWithOptCount, Retval, Nopts),
      "\n\n"] || Nopts <- lists:seq(0, Hi-Lo)].

%% stub code
get_1_function(O, Fname, Ename, InvKinds, FuncKind, Offs, Params, Retval, Nopts) ->
    StubParams= get_stub_params(Params, Nopts, Retval),
    {HasIntfOut, InterfaceOuts}= get_interface_outs(Params, Nopts),
    OneOut= length(InterfaceOuts)==1,
    RetvIntf= check_interface_retval(Retval),
    [Ename,
     "(I_", get_parameters(StubParams),
     ") ->\n    ",
     get_intf_match(RetvIntf or HasIntfOut),
     get_result1(OneOut, Retval, RetvIntf, HasIntfOut, InterfaceOuts),
     "erl_com:", atom_to_list(call_or_invoke(FuncKind, InvKinds)),
     "(I_, ", integer_to_list(Offs),
     ", [", get_callparams(O, Params, FuncKind, Nopts),
     "])",
     get_result2(OneOut, Retval, RetvIntf, HasIntfOut, InterfaceOuts),
     "."].

%% get result assignment (B)
%% get_result1(OneOut, Retval, RetvIntf, HasIntfOut, InterfaceOuts),
get_result1(_, _, false, true, InterfaceOuts) ->
    ["[",
     commas([initial_upper(Name) || {_Intf, Name} <- InterfaceOuts]),
     "]= "];
get_result1(_, Retval, RetvIntf, true, InterfaceOuts) ->
    get_result1(false, Retval, true, false, InterfaceOuts++[{RetvIntf, "Result"}]);
get_result1(true, _, false, false, _) ->
    "[Result] = ";
get_result1(false, _, _, false, _) ->
    "Result = ";
get_result1(false, void, _, _, _) ->
    [].

%% package if interface outs
pack_if(false, Name) -> Name;
pack_if(true, Name) -> ["{com_interface, P_, T_, ", Name, "}"].
%pack_if(true, Name) -> ["erl_com:package_interface(I_, ", Name, ")"].

%% get result return (E)
get_result2(false, _, false, true, InterfaceOuts) ->
    [",\n    [",
     commas([pack_if(Intf, initial_upper(Name)) || {Intf, Name} <- InterfaceOuts]),
     "]"];
get_result2(true, _, false, true, InterfaceOuts) ->
    [",\n    ",
     commas([pack_if(Intf, initial_upper(Name)) || {Intf, Name} <- InterfaceOuts])];
get_result2(_Oneout, Retval, RetvIntf, true, InterfaceOuts) ->
    get_result2(false, Retval, false, true, InterfaceOuts++[{RetvIntf, "Result"}]);
get_result2(_, _, RetvIntf, false, _) ->
    [",\n    ", pack_if(RetvIntf, "Result")];
get_result2(false, void, false, false, _) ->
    "".

%% add optional parameter count to parameter tuple from TypeLib
add_opt_param_count(Params) ->
    {L, Oc}=
	lists:mapfoldl(fun({N, F, T, D}, Oc) ->
			       case in_optional(F) of
				   true -> {{N, F, T, D, Oc}, Oc+1};
				   false -> {{N, F, T, D, -1}, Oc}
			       end
		       end, 0, Params),
    L.
    
%% get stub parameter names, while checking for optionals and return values
get_stub_params(Params, Nopts, Retval) ->
    StubParams= [N || {N, F, T, D, Optn} <- Params,
		      not lists:member(out, F), Optn < Nopts],
    StubParams.

%% check parameter for interface
check_interface_out({pointer, T}) -> check_interface_retval(T);
check_interface_out({pointer, {pointer, {virtual, Typename}}}) -> true;
check_interface_out(_) -> false.

check_interface_retval({pointer, {dispatch, _Typename}}) -> true;
check_interface_retval({pointer, {coclass, _Typename}}) -> true;
check_interface_retval({pointer, {interface, _Typename}}) -> true;
check_interface_retval({pointer, {virtual, _Typename}}) -> true;
check_interface_retval(_) -> false.

%% get parameters, while checking for interface returned
get_interface_outs(Params, Nopts) ->
    R= [{check_interface_out(T), N} || {N, F, T, D, Optn} <- Params,
				       lists:member(out, F), Optn < Nopts], % NB currently we don't support optional out
    {lists:any(fun({A, _}) -> A end, R), R}.

%% get erl_com-function to call
call_or_invoke(virtual, _) ->
    com_call;
call_or_invoke(purevirtual, _) ->
    com_call;
call_or_invoke(dispatch, [func]) ->
    invoke;
call_or_invoke(dispatch, [property_get]) ->
    property_get;
call_or_invoke(dispatch, [property_put]) ->
    property_put;
call_or_invoke(dispatch, [property_put_ref]) ->
    property_put.

%% get interface match on returned interfaces
get_intf_match(true) ->
    "{com_interface, P_, T_, N_}= I_,\n    ";
get_intf_match(false) ->
    "".

%% fix optional out parameters
%%  dispatch: simply remove them
%%  virtual: put in vt_i4, 0 as default value (NULL)
fix_optional_outs(Params, dispatch) ->
    lists:filter(fun({N, F, T, D, O}) ->
			 not (lists:member(out, F)
			      and lists:member(optional, F)) end,
		 Params);
fix_optional_outs(Params, virtual) ->
    fix_virtual_opt_outs(Params);
fix_optional_outs(Params, purevirtual) -> 
    fix_virtual_opt_outs(Params).

fix_virtual_opt_outs(Params) ->
    lists:map(fun({N, F, T, D, O}) ->
		      fix_virtual_opt_out({N, F, T, D, O},
					  lists:member(optional, F)
					  and lists:member(out, F)) end,
	      Params).

fix_virtual_opt_out({N, F, T, {}, O}, true) ->
    {N, F, optional_null, 0, 99};	  % enforce a NULL pointer
fix_virtual_opt_out({N, F, T, D, O}, true) ->
    {N, F, T, D, 99};
fix_virtual_opt_out(P, false) ->
    P.
	
%% get parameters (A)
get_parameters(Params) ->    
    [[", " | initial_upper(Param)] || Param <- Params].

%% generate default value (used when the Typelib doesn't specify one)
get_default_val({pointer, _}, {}) ->    0;
get_default_val(vt_variant, {}) ->    error;
get_default_val(vt_i4, {}) ->    0;
get_default_val(vt_u4, {}) ->    0;
get_default_val(vt_i2, {}) ->    0;
get_default_val(vt_u2, {}) ->    0;
get_default_val(vt_i1, {}) ->    0;
get_default_val(vt_u1, {}) ->    0;
get_default_val(vt_str, {}) ->    "";
get_default_val(vt_date, {}) ->    {0,0,0};
get_default_val(vt_cy, {}) ->    0;
get_default_val(vt_r8, {}) ->    0;
get_default_val(vt_r4, {}) ->    0;
get_default_val(Ptype, {}) ->    0;
get_default_val(Ptype, DefaultVal) ->    DefaultVal.

%% get type and value or name or out for parameter
%% callparam_name_type(Pname, Outflag, Type)
callparam_name_type(Pname, true, {pointer, {pointer, {dispatch, Type}}}) ->
    {vt_dispatch, "out"};
callparam_name_type(Pname, true, {pointer, {pointer, {coclass, Type}}}) ->
    {vt_unknown, "out"};
callparam_name_type(Pname, true, {pointer, {pointer, {interface, Type}}}) ->
    {vt_unknown, "out"};
callparam_name_type(Pname, true, {pointer, {pointer, {virtual, Type}}}) ->
    {vt_unknown, "out"};
callparam_name_type(Pname, true, {pointer, {pointer, Type}}) ->
    {Type, "out, pointer"};
callparam_name_type(Pname, true, {pointer, {enum, Type}}) ->
    {enum, "out"};
callparam_name_type(Pname, true, {pointer, Type}) ->
    {Type, "out"};
callparam_name_type(Pname, true, optional_null) ->
    {vt_i4, "0"};		  % a kind of NULL ptr
callparam_name_type(Pname, false, {pointer, {pointer, {pointer, Type}}}) ->
    {Type, "pointer, pointer, pointer "};
callparam_name_type(Pname, false, {pointer, {pointer, Type}}) ->
    {Type, "pointer, pointer"};
callparam_name_type(Pname, false, {pointer, {dispatch, Typename}}) ->
    {vt_dispatch, initial_upper(Pname)};
callparam_name_type(Pname, false, {pointer, {virtual, Typename}}) ->
    {vt_unknown, initial_upper(Pname)};
callparam_name_type(Pname, false, {pointer, {coclass, Typename}}) ->
    {vt_unknown, initial_upper(Pname)};
callparam_name_type(Pname, false, {pointer, {interface, Typename}}) ->
    {vt_unknown, initial_upper(Pname)};
callparam_name_type(Pname, false, {pointer, Type}) ->
    {Type, "pointer"};
callparam_name_type(Pname, false, {enum, Type}) ->
    {enum, initial_upper(Pname)};
callparam_name_type(Pname, false, Type) ->
    {Type, initial_upper(Pname)}.

%% listify atoms and ints
listify_atom(A) when atom(A) -> atom_to_list(A);
listify_atom(L) when list(L) -> L.

listify_value(I) when integer(I) -> integer_to_list(I);
listify_value(N) when float(N) -> float_to_list(N);
listify_value(L) when list(L) -> [$", L, $"];
listify_value(L) -> listify_atom(L).

unalias_type({{alias, T1}, T2}) -> T1;
unalias_type(T) -> T.

%% get call parameters (D)
get_1_callparam(O, {Pname, Pflags, Ptype, DefaultVal, Optn}, _FuncKind, Nopts) when Optn < Nopts ->
    Out= lists:member(out, Pflags),
    {T0, N}= callparam_name_type(Pname, Out, Ptype),
    T= unalias_type(T0),
    [" {", listify_atom(T), ", ", N, "}"];
get_1_callparam(O, {Pname, Pflags, Ptype, {}, Optn}, dispatch, Nopts) ->
    "";
get_1_callparam(O, {Pname, Pflags, Ptype, DefaultVal0, Optn}, _FuncKind, Nopts) ->
    Out= lists:member(out, Pflags),
    {T0, N}= callparam_name_type(Pname, Out, Ptype),
    T= unalias_type(T0),
    DefaultVal= get_default_val(Ptype, DefaultVal0),
    [" {", listify_atom(T), ", ", listify_value(DefaultVal), "}"].

get_callparams(O, Params, FuncKind, Nopts) when integer(Nopts) ->
    P= commas([get_1_callparam(O, Param, FuncKind, Nopts) || Param <- Params]).

%% make initial upper-case of a string (to get a erlang variable name)
initial_upper([Char | Rest]) when Char >= $a, Char =< $z ->
    [Char - $a + $A | Rest];
initial_upper(A) when atom(A) ->
    initial_upper(atom_to_list(A));
initial_upper(S) ->
    S.

%% make a decent erlang name from a COM symbol (according to options)
%% remove initial _, make first letter lowercase, add prefix if specified

make_erl_name(O, A, Ticks) when atom(A) ->
    make_erl_name(O, atom_to_list(A), Ticks);
make_erl_name(O, S, ticks) ->
    P= make_erl_name(O, S, no_ticks),
    add_ticks(P);
make_erl_name(O, S, no_ticks) ->
    N= fix_erl_name(O#options.names, S),
    P= prefix_name(N, O#options.prefix, O#options.prefix_these),
    suffix_name(P, O#options.suffix).

add_ticks([Char | Rest]) when Char >= $a, Char =< $z ->
    [Char | Rest];
add_ticks([[Char| A] | B]) when Char >= $a, Char =< $z ->
    [[Char| A] | B];
add_ticks([[[Char| A] | B] | C]) when Char >= $a, Char =< $z ->
    [[[Char| A] | B] | C];
add_ticks(N) ->
    ["'", N , "'"].

fix_erl_name(keep_names, S) ->
    S;
fix_erl_name(fix_names, S) ->
    lower_case_initial(without_(S)).

prefix_name(S, Prefix, []) ->
    [Prefix, S];
prefix_name(S, Prefix, Prefixthese) ->
    case lists:member(S, Prefixthese) of
	true ->
	    [Prefix, S];
	_ ->
	    S
    end.

suffix_name(S, Suffix) ->
    [S, Suffix].

without_([$_ | Rest]) ->
    without_(Rest);
without_(S) ->
    S.

lower_case(S) when list(S) ->
    lists:map(fun lower_case_c/1, S).    

lower_case_initial([C | S]) ->
    [lower_case_c(C) | S].

lower_case_c(Char) when Char >= $A, Char =< $Z ->
    Char - $A + $a;
lower_case_c(Char) ->
    Char.


%% open a module file, given name and options
open_module(O, Iname, virtual) ->
    open_module(O, Iname);
open_module(O, Iname, dispatch) ->
    open_module(O, get_dispatch_name(Iname, O#options.dispatch_only)).

open_module(O, Iname) ->
    Fname= Iname++".erl",
    {ok, F}= file:open(Fname, [write]),
    {ok, F, Fname}.

%% open header file
open_header(Iname) ->
    Fname= Iname++".hrl",
    {ok, F}= file:open(Fname, [write]),
    {ok, F, Fname}.

%% get erlang IID definition and function
get_iid_def(Iname, IID) ->
    ["-define(", Iname, "_iid, \"", IID, "\").\n\n",
     "iid() ->\n    ?", Iname, "_iid.\n\n"].

%% get erlang query_interface function
get_query_interface(Iname, IntfID) ->
    ["query_interface(I) ->\n",
     "	erl_com:query_interface(I, \"",
     IntfID, "\").\n\n"].

%% get erlang create_object function
get_create_object(dispatch) ->
    ["create_dispatch(T) ->\n    erl_com:create_dispatch(T, iid()).\n\n",
     "create_dispatch(T, Ctx) ->\n    erl_com:create_dispatch(T, iid(), Ctx).\n\n",
     get_create_object(virtual)];
get_create_object(_) ->
    ["create_object(T) ->\n    erl_com:create_object(T, iid()).\n\n",
     "create_object(T, X) ->\n    erl_com:create_object(T, iid(), X).\n\n",
     "create_object(T, I, Ctx) ->\n    erl_com:create_object(T, iid(), I, Ctx).\n\n"].

%% ----------------------------------------------------------------------
%% generate an interface, dispatch or virtual and dispatch
%% either a dispatch-only interface is generated, or
%% both the dispatch and virtual interfaces are generated,
%% the dispatch-interface suffixed by _disp
%% ----------------------------------------------------------------------

gen_interface(Obj, V) when tuple(Obj), atom(V) ->
    gen_interface_wrapper(default_options(), Obj, V).

gen_interface(Obj, V, Options) when tuple(Obj), atom(V), list(Options) ->
    gen_interface_wrapper(make_options(Options), Obj, V);
gen_interface(Obj, IntfName, V) when tuple(Obj), list(IntfName), atom(V) ->
    gen_interface_wrapper(default_options(), Obj, IntfName, V).

gen_interface(Obj, IntfName, V, Options) when tuple(Obj), list(IntfName), atom(V), list(Options) ->
    gen_interface_wrapper(make_options(Options), Obj, IntfName, V).


gen_interface_wrapper(Obj, V) when tuple(Obj) ->
    gen_interface_wrapper(default_options(), Obj, V).

gen_interface_wrapper(O, Obj, Intname, V) when tuple(Obj) ->
    {_, Dualflag, {Iname, IntfID}, Fns0, Impltypes} = erl_com:get_interface_info(Obj, Intname, V),
    gen_intf_wrapper_aux(O, Obj, V, Dualflag, Iname, IntfID, Fns0, Impltypes).

gen_interface_wrapper(O, Obj, V) when tuple(Obj) ->
    {_, Dualflag, {Iname, IntfID}, Fns0, Impltypes} = erl_com:get_interface_info(Obj, V),
    gen_intf_wrapper_aux(O, Obj, V, Dualflag, Iname, IntfID, Fns0, Impltypes).

gen_intf_wrapper_aux(O, Obj, V, Dualflag, Iname, IntfID, Fns0, Impltypes) ->
    Fns= get_virt_base_fns(Obj, Impltypes, V, Fns0, O#options.include_base_virtual),
    Mname= make_erl_name(O, Iname, no_ticks),
    {ok, F, Name}= open_module(O, Mname, V),
    progress(O, 2, "~w interface ~s to ~s\n", [V, Iname, Name]),
    io:put_chars(F, get_interface_wrapper(O, Iname, IntfID, Fns, V)),
    file:close(F),
    {ok, Name},
    case {Dualflag, V, O#options.dispatch_only} of
	{dual, dispatch, false} -> gen_interface_wrapper(O, Obj, Iname, virtual);
	_ -> ok
    end.
	
get_virt(T, {value, {_, Intfname, _}}, Fns) when Intfname =/= "IDispatch" -> 
    {_, Dualflag, {Iname, IntfID}, Fns0, Impltypes} = erl_com:get_interface_info(T, Intfname, virtual),
    Basetype= lists:keysearch(0, 1, Impltypes),
    get_virt(T, Basetype, [Fns0 | Fns]);
get_virt(T, _, Fns) ->
    lists:flatten(Fns).

get_virt_base_fns(T, Impltypes, virtual, Fns0, true) ->
    get_virt(T, lists:keysearch(0, 1, Impltypes), Fns0);
get_virt_base_fns(T, Impltypes, _, Fns0, _) ->
    Fns0.

%% get the interface
get_interface_wrapper(O, Iname, IntfID, Fns, V) ->
    Ftuples= add_parcounts_and_name(O, Fns),
    [get_module(O, Iname, V),
     get_export(O, interface, Ftuples),
     get_iid_def(Iname, IntfID),
     get_query_interface(Iname, IntfID),
     get_functions(O, Ftuples)].

%% ----------------------------------------------------------------------
%% generate coclass, just ID, name and creation
%% ----------------------------------------------------------------------

gen_coclass(Cname, IID, Options) when list(Cname), list(IID), list(Options) ->
    gen_coclass1(make_options(Options), Cname, IID).

gen_coclass1(O, Cname0, IID) when record(O, options) ->
    Cname= Cname0++O#options.class_suffix,
    Mname= make_erl_name(O, Cname, no_ticks),
    {ok, F, Name}= open_module(O, Mname),
    progress(O, 2, "coclass ~s to ~s\n", [Cname, Name]),
    io:put_chars(F,
		 [get_module(O, Cname),
		  get_export(O, coclass),
		  get_iid_def(Cname, IID),
		  get_create_object(dispatch)]),
    file:close(F),
    {ok, Name}.    

%% ----------------------------------------------------------------------
%% generate types, match and call the appropriate gen_<type>
%% ----------------------------------------------------------------------

gen_1_type(O, Obj, {enum, Name, IID}) ->
    gen_enum1(O, Obj, Name);
gen_1_type(O, Obj, {interface, Name, IID}) ->
    gen_interface_wrapper(O, Obj, Name, virtual);
gen_1_type(O, Obj, {dispatch, Name, IID}) ->
%    io:format("~s~n", [Name]),
    gen_interface_wrapper(O, Obj, Name, dispatch);
gen_1_type(O, Obj, {coclass, Name, IID}) ->
    gen_coclass1(O, Name, IID);
gen_1_type(O, Obj, {Other, Name, IID}) ->
    progress(O, 2, "Unknown type ~w ~s\n", [Other, Name]),
    io:format("Warning: won't generate anything for ~w ~s.\n", [Other, Name]).

gen_types(_O, _Obj, []) ->
    ok;
gen_types(O, Obj, [Atype | Rest]) ->
    {Type, Origname, _IID}= Atype,
    gen_1_type(O, Obj, Atype),
    gen_types(O, Obj, Rest).

%% ----------------------------------------------------------------------
%% show progress, according to verbose level
%% ----------------------------------------------------------------------

progress(O, Vlvl, S, P) ->
    progress1(O#options.verbose, Vlvl, S, P).

progress1(Vopt, Vlvl, S, P) when Vlvl > Vopt ->
    ok;
progress1(Vopt, Vlvl, S, P) ->
    io:format("~s", [string:copies("   ", Vlvl)]),
    io:format(S, P).

%% ----------------------------------------------------------------------
%% generate typelibs
%% iterates over each type and generates it
%% ----------------------------------------------------------------------

gen_typelib(Obj) ->
    gen_typelib1(default_options(), Obj).

gen_typelib(Obj, Options) when list(Options) ->
    gen_typelib1(make_options(Options), Obj).

gen_typelib(Obj, Libpath, Options) when list(Options)->
    gen_typelib1(make_options(Options), Obj, Libpath).

gen_typelib1(O, Obj) ->
    {Libname, Types}= erl_com:get_typelib_info(Obj),
    progress(O, 1, "Generating typelib ~s\n", [Libname]),
    gen_types(O, Obj, Types).

gen_typelib1(O, T, Libpath) ->
    {Libname, Types}= erl_com:get_typelib_info(T, Libpath),
    progress(O, 1, "Generating typelib ~s\n", [Libname]),
    gen_types(O, {T, Libpath}, Types).


%% ----------------------------------------------------------------------
%% handle options for code generation
%%
%% all options in the option record are user-settable
%% ----------------------------------------------------------------------

default_options() ->
    #options{names=fix_names, prefix="c_", prefix_these=["end", "fun", "when"], suffix="",
	     class_suffix="_class", verbose=1, multi_optional=true,
	     dispatch_only=true, include_base_virtual=true}.

make_options(L) ->
    make_options(L, default_options()).

make_options(Opts, O) ->
    lists:foldl(fun(Opt, O2) -> make_opt(Opt, O2) end, O, Opts).

make_opt(fix_names, O) ->    	O#options{names= fix_names};
make_opt(keep_names, O) ->	O#options{names= keep_names};
make_opt({prefix, Prefix}, O) -> O#options{prefix= Prefix};
make_opt({prefix_these, These}, O) -> O#options{prefix_these= These};
make_opt({also_prefix_these, These}, O) -> O#options{prefix_these= O#options.prefix_these++These};
make_opt({dont_prefix_these, These}, O) -> O#options{prefix_these= O#options.prefix_these--These};
make_opt({verbose, Lvl}, O) -> 	O#options{verbose= Lvl};
make_opt(verbose, O) ->    	O#options{verbose= 1};
make_opt({suffix, Suffix}, O) -> O#options{suffix= Suffix};
make_opt({class_suffix, Suffix}, O) -> O#options{class_suffix= Suffix};
make_opt(no_optional, O) ->	O#options{multi_optional=false};
make_opt(virtual, O) ->    O#options{dispatch_only=false};
make_opt(no_base_virtual, O) -> O#options{include_base_virtual=false};
make_opt(Other, O) ->
    io:format("Warning: unknown option ~w~n", [Other]),
    O.
    

%% ----------------------------------------------------------------------
%% generate enums
%% ----------------------------------------------------------------------

gen_enum(Obj, Enumname, Options) ->
    gen_enum1(make_options(Options), Obj, Enumname).

gen_enum(Obj, Enumname) ->
    gen_enum1(default_options(), Obj, Enumname).

gen_enum1(O, Obj, Enumname) when atom(Enumname) ->
    gen_enum(O, Obj, atom_to_list(Enumname));
gen_enum1(O, Obj, Enumname) ->
    {_, _, {Iname, IntfID}, Enums, _}=
	erl_com:get_interface_info(Obj, Enumname, dispatch), %%
    Enumerlname = make_erl_name(O, Enumname, ticks),
    Fname= make_erl_name(O, Enumname, no_ticks),
    Name= make_erl_name(O, Fname, no_ticks),
    {ok, Fm, Mname}= open_module(O, Name),
    {ok, Fh, Hname}= open_header(Name),
    io:put_chars(Fh, get_enum_header_comment(Enumname)),
    io:put_chars(Fm,
		 [get_module(O, Enumerlname),
		  get_export(O, enum, Enums),
		  get_enum_name(Enumname, Enumerlname)]),
    emit_enums(Fm, Fh, O, Enums),
    file:close(Fm),
    file:close(Fh),
    {ok, Mname, Hname}.


%% gen_enum helpers

get_enum_name(Enumname, Enumerlname) ->
    ["com_name() ->\n    \"", Enumname, "\".\n\n",
     "name() ->\n    \"", Enumerlname, "\".\n\n"].

emit_1_enum(Fm, Fh, O, {Name, Value}) ->
    Lname= make_erl_name(O, Name, ticks),
    io:format(Fm, "~s() ->\n    ~w.\n\n", [Lname, Value]),
    io:format(Fh, "-define(~s, ~w).\n\n", [Lname, Value]).

emit_enums(Fm, Fh, O, []) ->
    ok;
emit_enums(Fm, Fh, O, [Enum | Rest]) ->
    {Name, Value}=Enum,
    emit_1_enum(Fm, Fh, O, Enum),
    emit_enums(Fm, Fh, O, Rest).

get_enum_header_comment(Enumname) ->
    ["%% values for enum ", Enumname, "\n\n"].

