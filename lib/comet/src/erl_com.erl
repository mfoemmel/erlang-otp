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

%% The erl_com module provides a low-level interface to
%% COM-libraries. It is a gen_server and has API-function for all
%% features in COM supported by comet.

-module(erl_com).
-author('jakob@GAMGI').

-behaviour(gen_server).

-include("erl_com.hrl").

%% External exports (API)
-export([start_driver/0, start_program/0,
	 start_driver/1, start_program/1,
	 get_driver/1, get_program/1,
	 get_or_start/2,
	 driver_or_program/1,
	 create_dispatch/3, create_object/4, create_object/3, 
	 create_dispatch/2, create_object/2, 
	 get_object/3, get_object/2, get_dispatch/2,
	 query_interface/2, release/1,
	 invoke/3, invoke/2, com_call/3, com_call/2,
	 property_put/3, property_get/2, property_get/3, property_put_ref/3,
	 new_thread/1, end_thread/1, current_thread/1,
	 stop/1,
	 get_method_id/2,
	 get_interface_info/2, get_interface_info/3,
	 get_typelib_info/1, get_typelib_info/2,
	 package_interface/2,
	 test/1, idispatch/0,
	 reset/1, next/1, enumintf_next/1, nexti/1, intfnexti/1,
	 value/1, enum/1, evaluate/1,
	 map_enum/2, map_enumi/2, map_intfenumi/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% state record, portid and pending procs and kind
-record(state, {portid, procs, kind, current_thread}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% start as a port-driver (use with care)
start_driver() ->
    gen_server:start_link(?MODULE, driver, []).

%% start as a port-program
start_program() ->
    gen_server:start_link(?MODULE, program, []).

%% start as a registered port-driver (use with care)
start_driver(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, driver, []).

%% start as a named port-program
start_program(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, program, []).

get_or_start(undefined, Name, ProcFlag) ->
    gen_server:start_link({local, Name}, ?MODULE, ProcFlag, []);
get_or_start(Pid, Name, ProcFlag) ->
    case driver_or_program(Pid) of
	ProcFlag ->
	    {ok, Pid};
	_ ->
	    {error, already_other_kind}
    end.

get_or_start(Name, ProcFlag) ->
    get_or_start(whereis(Name), Name, ProcFlag).

%% get or start as a registered port-driver (use with care)
get_driver(Name) ->
    get_or_start(Name, driver).

%% get or start as a named port-program
get_program(Name) ->
    get_or_start(Name, program).

%% check whether a process is a port driver or port program
driver_or_program(Pid) ->
    gen_server:call(Pid, driver_or_program, infinity).

%% create a thread for com
new_thread(Pid) ->
    gen_server:call(Pid, new_thread, infinity).

%% end thread
end_thread(T) ->
    {com_thread, Pid, Threadn}= get_thread(T),
    gen_server:call(Pid, {end_thread, Threadn}).

%% current thread
current_thread(Pid) ->
    gen_server:call(Pid, current_thread, infinity).

%% create a com object with default interface
create_object(Thread, Clsid, Ctx) when integer(Ctx) ->
    create_object(get_thread(Thread), Clsid, "", Ctx);
create_object(Thread, Clsid, Refiid) when list(Refiid) ->
    {com_thread, Pid, Threadn}= get_thread(Thread),
    gen_server:call(Pid, {create_object, Threadn, Clsid, Refiid, 15}, infinity).

create_object(Thread, Clsid) ->
    create_object(Thread, Clsid, 15).

%% create a com object with dispatch interface
create_dispatch(Thread, Clsid, Ctx) when integer(Ctx) ->
    {com_thread, Pid, Threadn}= get_thread(Thread),
    gen_server:call(Pid, {create_object, Threadn, Clsid,
			  ?IID_IDispatch, Ctx}, infinity).
create_dispatch(Thread, Clsid) ->
    create_dispatch(Thread, Clsid, 15).

%% create a com object with given interface (Refiid)
create_object(Thread, Clsid, Refiid, Ctx) ->
    {com_thread, Pid, Threadn}= get_thread(Thread),
    gen_server:call(Pid, {create_object, Threadn, Clsid, Refiid, Ctx}, infinity).

%% get a com object with given name and interface
get_object(Thread, Name, Refiid) ->
    {com_thread, Pid, Threadn}= get_thread(Thread),
    gen_server:call(Pid, {get_object, Threadn, Name, Refiid}, infinity).

get_object(Thread, Name) ->
    get_object(Thread, Name, "").

get_dispatch(Thread, Name) ->
    get_object(Thread, Name, ?IID_IDispatch).

%% package an interface
package_interface({com_thread, Pid, Threadn}, I) when integer(I) ->
    {com_interface, Pid, Threadn, I};
package_interface({com_interface, Pid, Threadn, _}, I) when integer(I) ->
    {com_interface, Pid, Threadn, I};
package_interface(_, I) ->			% to get errors through
    I.

%% get thread from interface (or thread)
get_thread({com_interface, Pid, Threadn, _}) ->
    {com_thread, Pid, Threadn};
get_thread(Pid) when pid(Pid) ->
    {com_thread, Pid, 0};
get_thread(undefined) ->
    {com_error};
get_thread(Pid) when atom(Pid) ->
    {com_thread, whereis(Pid), 0};
get_thread(T) ->
    T.

%% query_interface, get a com interface from another
query_interface({com_interface, Pid, Threadn, Inum}, Iid) ->
    gen_server:call(Pid, {query_interface, Threadn, Inum, Iid}, infinity).

%% release, release a com interface
release({com_interface, Pid, Threadn, Inum}) ->
    gen_server:call(Pid, {release, Threadn, Inum}, infinity).

%% invoke a com method through the dispatch interface
invoke(Comint, Mid) ->
    invoke(Comint, Mid, []).
invoke(Comint, Mid, Pars) when list(Mid) ->
    M= get_method_id(Comint, Mid),
    invoke(Comint, M, Pars);
invoke({com_interface, Pid, Threadn, Inum}, Mid, Pars) when Inum >=0 ->
    gen_server:call(Pid, {invoke, Threadn, Inum, Mid, Pars}, infinity).

%% get type information, methods names, ids and offsets etc.
get_interface_info({com_interface, Pid, Threadn, Inum}, dispatch) ->
    gen_server:call(Pid, {get_interface_info, Threadn, Inum, ?ERLCOM_DispatchIntf}, infinity);
get_interface_info({com_interface, Pid, Threadn, Inum}, virtual) ->
    gen_server:call(Pid, {get_interface_info, Threadn, Inum, ?ERLCOM_VirtualIntf}, infinity).

get_interface_info({com_interface, Pid, Threadn, Inum}, Tname, dispatch) ->
    gen_server:call(Pid, {get_interface_info, Threadn, Tname, Inum, ?ERLCOM_DispatchIntf}, infinity);
get_interface_info({com_interface, Pid, Threadn, Inum}, Tname, virtual) ->
    gen_server:call(Pid, {get_interface_info, Threadn, Tname, Inum, ?ERLCOM_VirtualIntf}, infinity);
get_interface_info({T, Tlib}, Tname, dispatch) ->
    {com_thread, Pid, Threadn}= get_thread(T),
    gen_server:call(Pid, {get_interface_info, Threadn, Tname, Tlib, ?ERLCOM_DispatchIntf}, infinity);
get_interface_info({T, Tlib}, Tname, virtual) ->
    {com_thread, Pid, Threadn}= get_thread(T),
    gen_server:call(Pid, {get_interface_info, Threadn, Tname, Tlib, ?ERLCOM_VirtualIntf}, infinity).

%% get type information, methods names, ids and offsets etc.
get_typelib_info({com_interface, Pid, Threadn, Inum}) ->
    gen_server:call(Pid, {get_typelib_info, Threadn, Inum}, infinity).

get_typelib_info({com_interface, P, T, I}, Libname) ->
    get_typelib_info({com_thread, P, T}, Libname);
get_typelib_info(T, Libname) ->
    {com_thread, Pid, Threadn}= get_thread(T),
    gen_server:call(Pid, {get_typelib_info, Threadn, Libname}, infinity).

%% call a com method with stdcall (needs offset)
com_call(ComInt, Moffs) ->
    com_call(ComInt, Moffs, []).
com_call({com_interface, Pid, Threadn, Inum}, Moffs, Pars) ->
    gen_server:call(Pid, {call, Threadn, Inum, Moffs, Pars}, infinity).

%% put a com property through the dispatch interface
property_put(Comint, Prop, Par) when list(Prop) ->
    P= get_method_id(Comint, Prop),
    property_put(Comint, P, Par);
property_put({com_interface, Pid, Threadn, Inum}, Prop, Par) ->
    gen_server:call(Pid, {property_put, Threadn, Inum, Prop, Par}, infinity).

%% put a com ref property through the dispatch interface
property_put_ref(Comint, Prop, Par) when list(Prop) ->
    P= get_method_id(Comint, Prop),
    property_put_ref(Comint, P, Par);
property_put_ref({com_interface, Pid, Threadn, Inum}, Prop, Par) ->
    gen_server:call(Pid, {property_put, Threadn, Inum, Prop, Par}, infinity).

%% get a com property through the dispatch interface
%property_get(Comint, Prop) when list(Prop) ->
%    P= get_method_id(Comint, Prop),
%    property_get(Comint, P);
%property_get({com_interface, Pid, Threadn, Inum}, Prop) ->
%    gen_server:call(Pid, {property_get, Threadn, Inum, Prop}, infinity).
property_get(Comint, Mid) ->
    property_get(Comint, Mid, []).
property_get(Comint, Mid, Pars) when list(Mid) ->
    M= get_method_id(Comint, Mid),
    property_get(Comint, M, Pars);
property_get({com_interface, Pid, Threadn, Inum}, Mid, Pars) ->
    gen_server:call(Pid, {property_get, Threadn, Inum, Mid, Pars}, infinity).


%% get method id from name
get_method_id({com_interface, Pid, Threadn, Inum}, Method) ->
    gen_server:call(Pid, {get_method_id, Threadn, Inum, Method}, infinity).

%% terminate all com threads and close port
stop(Pid) when pid(Pid) ->
    unlink(Pid),
    gen_server:call(Pid, stop, infinity);
stop(Pid) ->
    stop(whereis(Pid)).

%% get next from IEnumVARIANT or IEnumUnknown
next({com_interface, Pid, Threadn, Inum}) ->
    gen_server:call(Pid, {next, Threadn, Inum}, infinity).

enumintf_next({com_interface, Pid, Threadn, Inum}) ->
    gen_server:call(Pid, {enumintf_next, Threadn, Inum}, infinity).

%% reset IEnumVARIANT or IEnumUnknown
reset({com_interface, Pid, Threadn, Inum}) ->
    gen_server:call(Pid, {reset, Threadn, Inum}, infinity).

%% some util functions

nexti(I) -> package_interface(I, next(I)).
intfnexti(I) -> package_interface(I, enumintf_next(I)).
value(I) -> property_get(I, ?DISPID_VALUE).
enum(I) -> package_interface(I, property_get(I, ?DISPID_NEWENUM)).
evaluate(I) -> property_get(I, ?DISPID_EVALUATE).

map_enum_aux(Enum, Next, F, A) ->
    reset(Enum),
    case Next(Enum) of
	{com_error, _, _}= E -> E;
	{} -> lists:reverse(A);
	N -> V= F(N),
	     map_enum_aux(Enum, Next, F, [V | A])
    end.

map_enum(Enum, F) ->
    map_enum_aux(Enum, {?MODULE, next}, F, []).

map_enumi(Enum, F) ->
    map_enum_aux(Enum, {?MODULE, nexti}, F, []).

map_intfenumi(Enum, F) ->
    map_enum_aux(Enum, {?MODULE, intfenum_next}, F, []).    

%% the test function just do a DebugBreak in the driver/port-program
test(Thread) ->
    {com_thread, Pid, Threadn}= get_thread(Thread),
    gen_server:call(Pid, {test, Threadn}, infinity).




idispatch() ->
    ?IID_IDispatch.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init(driver) ->
    case 
	erl_ddll:load_driver(
		 filename:join(code:priv_dir(comet), "lib"),
		 "erl_com_drv") of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    Portid= open_port({spawn, erl_com_drv}, [binary]),
    {ok, #state{portid= Portid, procs= [], kind=driver}};
init(program) ->
    ErlProg= filename:join(code:priv_dir(comet), "lib/erl_com_prog"),
    Portid= open_port({spawn, ErlProg}, [binary, {packet, 4}]),
    {ok, #state{portid= Portid, procs= [], kind=program}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(new_thread, From, State) ->
    do_call(?ERLCOM_NewThread, new_thread, 255, From, State);
handle_call(driver_or_program, From, State) ->
    {reply, State#state.kind, State};
handle_call(current_thread, From, State) ->
    do_call(?ERLCOM_CurrentThread, current_thread, 0, From, State);
handle_call({end_thread, Threadn}, From, State) ->
    do_call(?ERLCOM_EndThread, end_thread, Threadn, From, State);
handle_call(stop, From, State) ->
    State#state.portid ! {self(), {command, [?ERLCOM_Quit]}},
    {stop, normal, ok, State};
handle_call({create_object, Threadn, Clsid, Refiid, Ctx}, From, State) ->
    do_call(?ERLCOM_CreateObject, create_object, Threadn,
	    {Clsid, Refiid, Ctx}, From, State);
handle_call({get_object, Threadn, Name, Refiid}, From, State) ->
    do_call(?ERLCOM_GetObject, create_object, Threadn,
	    {Name, Refiid}, From, State);
handle_call({query_interface, Threadn, Inum, Iid}, From, State) ->
    do_call(?ERLCOM_QueryInterface, query_interface, Threadn,
	    {Inum, Iid}, From, State);
handle_call({release, Threadn, Inum}, From, State) ->
    do_call(?ERLCOM_Release, release, Threadn, Inum, From, State);
handle_call({invoke, Threadn, Inum, Mid, Pars}, From, State) ->
    do_call(?ERLCOM_Invoke, invoke, Threadn,
	    {Inum, Mid, Pars}, From, State);
handle_call({call, Threadn, Inum, Moffs, Pars}, From, State) ->
    do_call(?ERLCOM_Call, call, Threadn, {Inum, Moffs, Pars}, From, State);
handle_call({property_put_ref, Threadn, Inum, Pid, Par}, From, State) ->
    do_call(?ERLCOM_PropertyPutRef, property_put_ref, Threadn,
	    {Inum, Pid, Par}, From, State);
handle_call({property_put, Threadn, Inum, Pid, Par}, From, State) ->
    do_call(?ERLCOM_PropertyPut, property_put, Threadn,
	    {Inum, Pid, Par}, From, State);
handle_call({property_get, Threadn, Inum, Mid, Pars}, From, State) ->
    do_call(?ERLCOM_PropertyGet, property_get, Threadn,
	    {Inum, Mid, Pars}, From, State);

handle_call({get_method_id, Threadn, Inum, Method}, From, State) ->
    do_call(?ERLCOM_GetMethodID, get_method_id, Threadn,
	    {Inum, Method}, From, State);
handle_call({get_interface_info, Threadn, Inum, Dispflag}, From, State) ->
    do_call(?ERLCOM_GetInterfaceInfo, get_interface_info, Threadn,
	   {Inum, Dispflag}, From, State);
handle_call({get_interface_info, Threadn, Tname, Inum, Dispflag}, From, State) ->
    do_call(?ERLCOM_GetInterfaceInfo, get_interface_info, Threadn,
	   {Tname, Inum, Dispflag}, From, State);

handle_call({get_typelib_info, Threadn, Inum}, From, State) ->
    do_call(?ERLCOM_GetTypeLibInfo, get_typelib_info, Threadn,
	   Inum, From, State);
handle_call({next, Threadn, Inum}, From, State) ->
    do_call(?ERLCOM_Next, next, Threadn,
	    Inum, From, State);
handle_call({enumintf_next, Threadn, Inum}, From, State) ->
    do_call(?ERLCOM_NextIntf, enumintf_next, Threadn,
	    Inum, From, State);
handle_call({reset, Threadn, Inum}, From, State) ->
    do_call(?ERLCOM_Reset, reset, Threadn,
	    Inum, From, State);

handle_call({test, Threadn}, From, State) ->
    do_call(?ERLCOM_Test, test, Threadn, {}, From, State);

handle_call(Request, From, State) ->
    Reply = false,
    {reply, Reply, State}.
%%!! vad ska vi göra här!

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%---------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({_, {data, Bin}}, State) ->
    {Threadn, Result}= binary_to_term(Bin),
    {Proc, Op, Newstate}= extract_op(Threadn, State),
    Reply= package_result(Op, Threadn, Result),
    gen_server:reply(Proc, Reply),
    {noreply, Newstate}.
%%!! övriga fall!


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    State#state.portid ! {self(), close},
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
%% get an operation from the procs and remove it
extract_op(Id, State) ->
    {value, {_, Proc, Op}}= lists:keysearch(Id, 1, State#state.procs),
    Newstate= State#state{procs= lists:keydelete(Id, 1, State#state.procs)},
    {Proc, Op, Newstate}.

%% add an operation to the procs
add_op(Threadn, Proc, Op, State) ->
    Nprocs= [{Threadn, Proc, Op} | State#state.procs],
    State#state{procs= Nprocs}.

%% convert result from port to expected format
package_result(create_object, Threadn, Result) when integer(Result) ->
    {com_interface, self(), Threadn, Result};
package_result(query_interface, Threadn, Result) when integer(Result) ->
    {com_interface, self(), Threadn, Result};
package_result(new_thread, Threadn, Result) when integer(Result) ->
    {com_thread, self(), Result};
package_result(current_thread, Threadn, Result) when integer(Result) ->
    {com_thread, self(), Result};
package_result(_Op, _Threadn, Result) ->
    Result.

%% helper for handle_call
do_call(Call, Op, Threadn, From, State) ->
    Newstate= add_op(Threadn, From, Op, State),
    State#state.portid ! {self(), {command, [Call, Threadn]}},
    {noreply, Newstate}.

do_call(Call, Op, Threadn, Pars, From, State) ->
    Newstate= add_op(Threadn, From, Op, State),
    State#state.portid ! {self(), {command, [Call, Threadn
					     | term_to_binary(Pars)]}},
    {noreply, Newstate}.
