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

-module(gtk).

-compile(export_all).

-include("gtk.hrl").

start_link(GsId,FrontendNode,Owner,Options) ->
    case gs:assq(node,Options) of
	false ->
	    Gtk = spawn_link(gtk, init,[{GsId, FrontendNode, Owner, Options}]),
	    receive
		{ok, PortHandler} ->
		    {ok, Gtk};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{value, Node} ->
	    rpc:call(Node,gen_server,start_link,[gtk, {Owner,Options},[]])
    end.
	    
stop(BackendServ) -> 
    request(BackendServ,stop).

create(BackendServ,Args) ->
    request(BackendServ,{create,Args}).

config(BackendServ,Args) ->
    request(BackendServ,{config,Args}).

read(BackendServ,Args) ->
    request(BackendServ,{read,Args}).

destroy(BackendServ,Args) ->
    request(BackendServ,{destroy,Args}).

pid_died(BackendServ,Pid) ->
    request(BackendServ,{pid_died,Pid}).

call(Cmd) ->
    %%io:format("Call:~p~n",[Cmd]),
    gtk_port_handler:call(get(port_handler),Cmd).

exec(Cmd) ->
    gtk_port_handler:exec(Cmd). 
   
%exec(Cmd) ->
%    gtk_port_handler:exec(get(port_handler),Cmd). % too expensive 

make_extern_id(IntId, DB) ->
    [{_,Node}] = ets:lookup(DB,frontend_node),
    {IntId,Node}.

event(BackendServ,Event) ->
    BackendServ!{event,Event}.

%% -----------------------------------------------------------------------------

request(Who,Msg) ->
    Who ! {self(),Msg},
    receive
	{gtk_reply,R} -> R;
	{'EXIT',Who,Reason} ->
	    self() ! {'EXIT',Who,Reason},
	    {error,Reason}
    end.


-record(state,{db,frontendnode,port_handler}).

%% ------------------------------------------------------------
%% Initialize
%%
init({GsId,FrontendNode,Owner,Opts}) ->
    put(gs_frontend,Owner),
    case gtk_port_handler:start_link(self()) of
	{error, Reason} ->
	    FrontendNode ! {error, Reason},
	    exit(normal);
	{ok, PortHandler} ->
	    FrontendNode ! {ok, PortHandler},
	    put(port_handler,PortHandler),
	    {ok,Port} = gtk_port_handler:ping(PortHandler),
	    put(port,Port),
	    exec("wm withdraw ."),
	    DB = gtk_db:init(Opts),
	    ets:insert(DB,{frontend_node,FrontendNode}),
	    put(worker,spawn_link(gtk,worker_init,[0])),
	    Gtkid = #gtkid{id=GsId,widget="",owner=Owner,objtype=gs},
	    gtk_db:insert_gs(DB,Gtkid),
	    gtk_font:init(),
	    exec(["option add *font ",gtk_font:choose_ascii(DB,{screen,12})]),
	    loop(#state{db=DB,frontendnode=FrontendNode})
    end.

loop(State) ->
    receive
	X ->
%	    io:format("GS backend. received: ~p~n",[X]),
	    case (doit(X,State)) of
		done -> loop(State);
		 NewState when record(NewState,state) ->
		    loop(NewState);
		stop -> bye;
		EXIT ->
		    gs:error("GS backend. Last msg in: ~p~n",[X]),
		    io:format("db:~p~nexit:~p",
			      [ets:tab2list(State#state.db),EXIT]),
		    exit(EXIT)
	    end
    end.

reply(To,Msg) ->
    To ! {gtk_reply,Msg},
    done.

doit({From,{config, {Id, Opts}}},#state{db=DB}) ->
    reply(From,config_impl(DB,Id,Opts));
doit({From,{create, Args}}, #state{db=DB}) ->
    reply(From,create_impl(DB,Args));
doit({From,{read,{Id,Opt}}},#state{db=DB}) ->
    reply(From,read_impl(DB,Id,Opt));
doit({From,{pid_died, Pid}}, #state{db=DB}) ->
    pid_died_impl(DB, Pid),
    reply(From,gtk_db:get_deleted(DB));
doit({From,{destroy, Id}}, #state{db=DB}) ->
    destroy_impl(DB, gtk_db:lookup_gtkid(DB,Id)),
    reply(From,gtk_db:get_deleted(DB));

doit({From,dump_db},State) ->
    io:format("gtk_db:~p~n",[ets:tab2list(State#state.db)]),
    io:format("events:~p~n",[ets:tab2list(get(events))]),
    io:format("options:~p~n",[ets:tab2list(get(options))]),
    io:format("defaults:~p~n",[ets:tab2list(get(defaults))]),
    io:format("kids:~p~n",[ets:tab2list(get(kids))]),
    reply(From,State);

doit({From,stop},S) ->
    gtk_port_handler:stop(get(port_handler)),
    exit(get(worker),kill),
    reply(From,stopped),
    stop;

doit({event,{Id, Etag, Args}},#state{db=DB}) ->
    case gtk_db:lookup_event(DB, Id, Etag) of
        {Etype, Edata} ->
            Gtkid = gtk_db:lookup_gtkid(DB, Id),
            apply(gtk_widgets:objmod(Gtkid),event,[DB,Gtkid,Etype,Edata,Args]);
        Other -> true
    end,
    done.


%%----------------------------------------------------------------------
%% Implementation of create,config,read,destroy
%% Comment: In the gtk process there is not concept call 'name', only
%%          pure oids. Names are stripped of by 'gs' and this simplifies
%%          gtk a lot.
%% Comment: For performance reasons gtk.erl ans gs.erl communicats through
%%          tuples. This is unfortunate but we don't want to pack the same
%%          thing too many times.
%% Pre (for all functions): GS guarantees that the object (and parent if
%%          necessary) exists.
%%----------------------------------------------------------------------


create_impl(DB, {Owner, {Objtype, Id, Parent, Opts}}) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent),
    GtkId=#gtkid{id=Id,owner=Owner,parent=Parent,objtype=Objtype},
    gtk_db:insert_opt(DB,Id,{data,[]}),
    RealOpts=apply(gtk_widgets:objmod(Pgtkid),
		   mk_create_opts_for_child,[DB,GtkId,Pgtkid,Opts]),
    case gtk_widgets:type2mod(Objtype) of
	{error,Reason} -> {error,Reason};
	ObjMod ->
	    case apply(ObjMod, create, [DB, GtkId, RealOpts]) of
		{bad_result, BR} ->
		    gtk_db:delete_gtkid(DB,GtkId),
		    gs:creation_error(GtkId,{bad_result, BR});
		Ngtkid when record(Ngtkid,gtkid) ->
		    gtk_db:insert_widget(DB, Ngtkid),
		    ok;
		{error,Reason} -> {error,Reason};
		ok -> ok
	    end
    end.

config_impl(DB,Id,Opts) ->
    Gtkid = gtk_db:lookup_gtkid(DB, Id), 
    case apply(gtk_widgets:objmod(Gtkid), config, [DB, Gtkid, Opts]) of
	ok -> ok;
	{bad_result,R} -> {error,R};
	{error,Reason} -> {error,Reason};
	Q -> {error,Q}
    end.
    

read_impl(DB,Id,Opt) ->
    Gtkid = gtk_db:lookup_gtkid(DB, Id), 
    case apply(gtk_widgets:objmod(Gtkid), read, [DB, Gtkid, Opt]) of
	{bad_result,R} -> {error,R};
	{error,R} -> {error,R};
	Res -> Res
    end.



%%-----------------------------------------------------------------------------
%%			DESTROYING A WIDGET
%%-----------------------------------------------------------------------------

destroy_impl(DB, Gtkid) ->
    worker_do({delay_is,50}),
    Widget = delete_only_this_widget(DB,Gtkid),
    destroy_widgets([Widget], DB),
    worker_do({delay_is,5}),
    true.

delete_only_this_widget(DB,Gtkid) ->
    #gtkid{id=ID,objtype=OT,parent=P} = Gtkid,
    delete_widgets(gtk_db:lookup_kids(DB, ID), DB),
    Widget = apply(gtk_widgets:type2mod(OT), delete, [DB, Gtkid]),
    gtk_db:delete_kid(DB, P, ID),
    Widget.


pid_died_impl(DB, Pid) ->
    case lists:sort(gtk_db:lookup_ids(DB, Pid)) of
	[ID | IDs] ->
	    Gtkid = gtk_db:lookup_gtkid(DB, ID),
	    destroy_impl(DB, Gtkid),
	    Tops = get_tops(IDs, DB),
	    destroy_widgets(Tops, DB);
	Other ->
	    true
    end.


get_tops([ID | IDs], DB) ->
    case gtk_db:lookup_gtkid(DB, ID) of
	undefined ->
	    get_tops(IDs, DB);
	Gtkid -> 
	    Parent = Gtkid#gtkid.parent,
	    case lists:member(Parent, IDs) of
		true  ->
		    delete_widgets([ID], DB),
		    get_tops(IDs, DB);
		false ->
		    Widget = delete_only_this_widget(DB,Gtkid),
		    [Widget | get_tops(IDs, DB)]
	    end
    end;
get_tops([], DB) -> [].


delete_widgets([ID | Rest], DB) ->
    delete_widgets(gtk_db:lookup_kids(DB, ID), DB),
    case gtk_db:lookup_gtkid(DB, ID) of
	undefined ->
	    delete_widgets(Rest, DB);
	Gtkid ->
	    apply(gtk_widgets:objmod(Gtkid), delete, [DB, Gtkid]),
	    delete_widgets(Rest, DB)
    end;
delete_widgets([], _) -> true.



destroy_widgets(Widgets, DB) ->
    case destroy_wids(Widgets, DB) of
	[]       -> true;
	Destroys -> exec(["catch {destroy ", Destroys, "}"])
    end.


destroy_wids([{Parent, ID, Objmod, Args} | Rest], DB) ->
    gtk_db:delete_kid(DB, Parent, ID),
    apply(Objmod, destroy, [DB | Args]),
    destroy_wids(Rest, DB);

destroy_wids([W | Rest], DB) ->
    [W, " "| destroy_wids(Rest, DB)];

destroy_wids([], DB) -> [].


%% ----- The Color Model -----

to_color({R,G,B}) ->
    [$#,dec2hex(2,R),dec2hex(2,G),dec2hex(2,B)];
to_color(Color) when atom(Color) -> atom_to_list(Color).

%% ------------------------------------------------------------
%% Decimal to Hex converter
%% M is number of digits we want
%% N is the decimal to be converted

dec2hex(M,N) -> dec2hex(M,N,[]).

dec2hex(0,N,Ack) -> Ack;
dec2hex(M,N,Ack) -> dec2hex(M-1,N bsr 4,[d2h(N band 15)|Ack]).

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.

		
%% ----- Value to String -----

to_ascii(V) when list(V)    -> [$",to_ascii(V,[],[]),$"];  %% it's a string
to_ascii(V) when integer(V) -> integer_to_list(V);
to_ascii(V) when float(V)   -> float_to_list(V);
to_ascii(V) when atom(V)    -> to_ascii( atom_to_list(V));
to_ascii(V) when tuple(V)   -> to_ascii(lists:flatten(io_lib:format("~w",[V])));
to_ascii(V) when pid(V)     -> pid_to_list(V).


to_ascii([$[|R], Y, X) ->  to_ascii(R, Y, [$[, $\\ | X]);
to_ascii([$]|R], Y, X) ->  to_ascii(R, Y, [$], $\\ | X]);
to_ascii([${|R], Y, X) ->  to_ascii(R, Y, [${, $\\ | X]);
to_ascii([$}|R], Y, X) ->  to_ascii(R, Y, [$}, $\\ | X]);
to_ascii([$"|R], Y, X) ->  to_ascii(R, Y, [$", $\\ | X]);
to_ascii([$$|R], Y, X) ->  to_ascii(R, Y, [$$, $\\ | X]);
to_ascii([$\\|R], Y, X) ->  to_ascii(R, Y, [$\\, $\\ | X]);
to_ascii([C|R], Y, X) when list(C) -> to_ascii(C, [R|Y], X);
to_ascii([C|R], Y, X) -> to_ascii(R, Y, [C|X]);
to_ascii([], [Y1|Y], X) -> to_ascii(Y1, Y, X);
to_ascii([], [], X) -> lists:reverse(X).

worker_do(Msg) ->
    get(worker) ! Msg.

worker_init(Delay) ->
    receive
	{delay_is,D} ->
	    worker_init(D);
	{match_delete,DBExprs} ->
	    worker_match(DBExprs),
	    if Delay > 0 ->
		    receive
			{delay_is,D} ->
			    worker_init(D)
		    after Delay ->
			    worker_init(Delay)
		    end;
	       true -> 
		    worker_init(Delay)
	    end
    end.

worker_match([{DB,[Expr|Exprs]}|DbExprs]) ->
    ets:match_delete(DB,Expr),
    worker_match([{DB,Exprs}|DbExprs]);
worker_match([{DB,[]}|DbExprs]) ->
    worker_match(DbExprs);
worker_match([]) -> done.
