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
-module(mnemosyne_catalog).

%%%================================================================
%%%
%%% INTRODUCTION
%%%
%%% This is the catalog function for the database query language
%%% Mnemosyne in conjunction with the DBMS Mnesia.
%%%
%%% The purpose of the catalog is to maintain statistics about the
%%% database.  This statistics is used by the query optimizer when
%%% reordering the query goals at query setup.
%%%
%%% 
%%% ABOUT THE CATALOG SERVER
%%%
%%% The catalog server is supervised by the mnesia application.
%%%
%%% The statistics is in form of one so called image per table which
%%% is returned to the asker on request. The image "appears" in the
%%% server in either of two ways:
%%% 
%%%     1) When there is a request from the optimizer but there is no
%%%        image available for that table.  In this case we will wait
%%%        maximum "?max_wait" ms for the image to be produced.  It
%%%        this time limit fails, the "null_image" is returned but the
%%%        image will be entered in the catalog when it is available.
%%%
%%%     2) When a mnesia node has produced an image it is broadcasted
%%%        to all other running mnesia nodes.
%%% 
%%%  When there is a request for an image, the catalog subscribes on
%%%  table events for that table from mnesia.  There is a counter per
%%%  table which is decremented by one for each update event that is
%%%  received from mnesia.  When the counter reaches 0, a new image is
%%%  produced unless there have passed less than "?min_upd_interval"
%%%  seconds since the last update (either localy or broadcasted from
%%%  an other node).  The counter is initialised to "?upd_limit" percent
%%%  of the number of records in the table.
%%%
%%%  The updating is performed in low process priority.
%%%
%%%  If a table has no local copy (neither disk nor ram) there is no
%%%  image produced, instead the updating is handed over to the node
%%%  given by "mnesia:table_info(Table,where_to_read)" unless that
%%%  node has a disk_only copy of the table. In that case we try to
%%%  find (in order) a node with disc_copies, ram_copies and finally
%%%  the disk_only copy.
%%%
%%%  If the schema is changed in some serious way (for example the
%%%  arity of a table), the image is removed for that table.
%%%  
%%%  ERRORS
%%%
%%%  On errors usually the "null_image" is returned since an absent or
%%%  even erroneous image does not infuence the correctness nor the
%%%  completness of a query, just the execution time.
%%%
%%%================================================================

%%% external interface:

-export([start/0,
	 image/1, 
	 clear_statistics/1, clear_statistics/0,
	 set_parameter/3, get_parameter/2
	]).

%%% Administrative functions
-export([is_running/0,
	 info/0, infoS/0
	 ]).

-behaviour(gen_server).
%%% gen_server callbacks:
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%% doing apply on these:
-export([mk_image/1,
	 mk_insert_image/1,
	 mk_insert_image_send/2
	]).

-define(SERVER_NAME, ?MODULE).  %% The registered name of the catalog server
-define(CATALOG, ?SERVER_NAME). %% The name of the named ets catalog table

%%%----------------------------------------------------------------
%%% DEFAULT VALUES of parameters setable by users
%%%
 
-define(default_values,
	[
	 %% If we shall update this table on this node (yes|no)
	 {do_local_upd, yes, ""},

	 %% Seconds after an update before next one is allowed:
	 {min_upd_interval, 10, "s"},

	 %% Percentage of each table which may be updated without any
	 %% recalculation: 
	 {upd_limit, 10, "%"},

	 %% Max time to wait for an image to be produced (milli seconds):
	 {max_wait, 1000, "ms"}
	]).

%%%----------------------------------------------------------------

%%-define(debug,1).
-include("mnemosyne_debug.hrl").

%%%================================================================
%%% 		User Exports


start() -> gen_server:start_link({local,?SERVER_NAME}, ?MODULE, [], []).


%% returns {Cardinality, {I1,I2...Iarity}, I1*I2*...*Iarity}

image(Table) -> 
    WhereToRead = mnesia:table_info(Table,where_to_read),

    case catch read_image(Table) of
	none when WhereToRead==node() ->
	    impatient_mk_insert_image(Table, read_parameter(Table,max_wait));
	
	none when WhereToRead=/=nowhere ->	
	    %% Not local, ask someone else
	    RemoteNode =
		case lists:member(WhereToRead, 
				  mnesia:table_info(Table,disc_only_copies))of
		    true ->
			%% WhereToRead has a slow copy, try to find another
			select_remote_node(Table, [WhereToRead]);
		    false ->
			WhereToRead
		end,
	    case rpc:call(RemoteNode, ?MODULE, image, [Table]) of
		{badrpc, Reason} -> 
		    %% Serious problems, but not for the catalog!
		    null_image(Table);
		I ->
		    I
	    end;
	
	none when WhereToRead==nowhere ->	
	    %% The returned null_image is the callers least problem now...
	    null_image(Table);
	
	{'EXIT', _} -> 
	    exit(mnemosyne_not_running);

	I when WhereToRead==node() ->
	    subscribe(Table),
	    I;
	
	I ->
	    I
    end.


set_parameter(Table, Name, Value) ->
    case lists:keymember(Name,1,?default_values) of
	false ->
	    {error, {no_such_parameter,Name}};
	_ ->
	    case catch ets:first(?CATALOG) of
		{'EXIT',_} -> {error, {node_not_running,node()}};
		_ ->
		    OldValue = read_parameter(Table, Name),
		    insert_parameter(Table, Name, Value),
		    %% Actions when updated
		    case Name of
			upd_limit ->
			    NewUpdLimit = update_limit(Table),
			    Cntr = read_counter(Table,limit),
			    if 
				Cntr<NewUpdLimit -> 
				    ?SERVER_NAME ! 
					{mnesia_table_event,{'',{Table},''}};
			       true ->
				    %% should reduce NewUpdLimit by
				    %% decremented amount
				    insert_counter(Table,limit,NewUpdLimit)
			    end;

			do_local_upd when Value==no ->
			    gen_server:cast(?SERVER_NAME,{unsubscribe,Table});

			do_local_upd when Value==yes ->
			    gen_server:cast(?SERVER_NAME,{subscribe,Table});

			_ -> ok
		    end,
		    OldValue
	    end
    end.

get_parameter(Table, Name) ->
    case lists:keymember(Name,1,?default_values) of
	false ->
	    {error, {no_such_parameter,Name}};
	_ ->
	    case catch ets:first(?CATALOG) of
		{'EXIT',_} -> parameter_default_value(Name);
		_ -> read_parameter(Table, Name)
	    end
    end.

%%%----------------------------------------------------------------
%%%		Local
subscribe(Table) ->
    case read_parameter(Table,do_local_upd) of
	yes -> gen_server:cast(?SERVER_NAME, {subscribe,Table});
	no -> ok
    end.

%%%----------------------------------------------------------------
%%% T is table or a list of tables
clear_statistics(Tab) when atom(Tab) ->
    ets:match_delete(?CATALOG, {{'_',Tab},'_'});

clear_statistics(Tabs) when list(Tabs) ->
    lists:foreach(fun(Tab) -> clear_statistics(Tab) end, Tabs).

clear_statistics() ->
    ets:match_delete(?CATALOG, '_').
    
%%%---------------- Adm ----------------

is_running() -> whereis(?SERVER_NAME) =/= undefined.

info() ->
    io:format('~s\n', [infoS()]).

infoS() ->
    lists:flatten([
		   io_lib:format("Mnemosyne catalog on ~w ", [node()]),
		   case length(element(2,element(2,lists:keysearch(messages,1,process_info(whereis(?SERVER_NAME)))))) of
		       N when integer(N), N>0 -> 
			   io_lib:format(', ~w messages in queue\n', [N]);
		       _ -> 
			   io_lib:nl()
		   end,

		   case mnemosyne_catalog:is_running() of
		       true ->  tbl_info();
		       false -> io_lib:format(" *** Not running!\n", [])
		   end
		  ]).
    


%%%================================================================
%%% 		gen_server callbacks

-record(state,
	{
	}).

-record(stat, %% statistics
	{latest = 0,
	 sum = 0,
	 n = 0
	}).

-record(stats,
	{time,
	 gc,
	 recl
	}).

init(_) ->
    ets:new(?CATALOG, [set,public,named_table]),
    tell_friends({hi,node()}, nodes()),
    mnesia:subscribe({table,schema}),
    process_flag(priority, low),
    {ok, #state{}}.


handle_call(_,_,State) ->
    {noreply,State}.


handle_cast({hi,RemoteNode}, State) ->
    %% A new node has joined us. Welcome him/her by sending the images
    lists:foreach(
      fun(Tab) -> 
	      gen_server:cast({?SERVER_NAME,RemoteNode},
			      {remote_data, Tab, read_image(Tab),
			       read_time_stamp(Tab)})
      end, checked_tables()),
    {noreply, State};

handle_cast({remote_data,Table,RemoteImage,RemoteTimeStamp}, State) ->
    %% An image from an other node
    case {RemoteTimeStamp,read_time_stamp(Table)} of
	{{_,_,_,Remote},{_,_,_,Local}} when Remote<Local ->
	    %% We received a older image than our own
	    do_nothing;

	{_,LocalTimeStamp} ->
	    %% Assuming that the received TimeStamp is =/= undefined.
	    update_counter(Table, n_upd_remote, 1, 1),
	    insert_image(Table, RemoteImage),
	    insert_time_stamp(Table, RemoteTimeStamp),
	    insert_counter(Table, limit, update_limit(Table,RemoteImage))
    end,
    {noreply, State};

handle_cast({subscribe,Table}, State) ->
    mnesia:subscribe({table,Table}),
    {noreply,State};

handle_cast({unsubscribe,Table}, State) ->
    mnesia:unsubscribe({table,Table}),
    {noreply,State};

handle_cast(_,State) ->
    {noreply,State}.



handle_info({mnesia_table_event,{delete,R,_}}, State) when element(1,R)==schema,
							   atom(element(2,R))  ->
    clear_statistics (element (2,R)),
    {noreply,State};

handle_info({mnesia_table_event,{_,R,_}}, State) when element(1,R)==schema ->
    %% Remove data for any of our checked tables that are changed
    %% in any important way
    lists:foreach(
      fun(Tab) ->
	      OldArity = size(element(2,read_image(Tab))),
	      NewArity = mnesia:table_info(Tab,arity) - 1,
	      if
		  OldArity =/= NewArity -> clear_statistics(Tab);
		  true -> ok
	      end
      end, checked_tables()),
    {noreply,State};

handle_info({mnesia_table_event,{_,R,_}}, State) when atom(element(1,R)) ->
    Table = element(1,R),
    %% Updating a Mnesia table.  Shall we re-calculate?
    case read_parameter(Table,do_local_upd) of
	yes ->
	    case update_counter(Table,limit,-1) of
		N when integer(N),N=<0 -> %% Passed the update limit
		    NowSecs = now_in_secs(),
		    MinInterv = read_parameter(Table,min_upd_interval),
		    case read_time_stamp(Table) of
			{_,_,_,Tupd} when (NowSecs-Tupd)<MinInterv ->
			    %% Too early, but do not forget this
			    SecsLeft = MinInterv - (NowSecs-Tupd),
			    timer:send_after(SecsLeft*1000,
					     {delayed_upd,Table});
			_ -> %% enough time has passed since last update
			    mk_insert_image(Table)
		    end;
		_ ->
		    no
	    end;

	no -> 
	    mnesia:unsubscribe({table,Table})
    end,
    {noreply,State};

handle_info({delayed_upd,Table}, State) ->
    NowSecs = now_in_secs(),
    MinInterv = read_parameter(Table,min_upd_interval),
    case read_time_stamp(Table) of
	{_,_,_,Tupd} when (NowSecs-Tupd)<MinInterv ->
	    %% Too early. Someone else must have updated this. Just skip it.
	    skip;
	_ ->
	    mk_insert_image(Table)
    end,
    {noreply,State};

handle_info(_,State) ->
    {noreply,State}.


terminate(_,State) -> ok.


code_change(_,State,_) -> {ok,State}.


%%%----------------------------------------------------------------
checked_tables() ->
    lists:foldl(
      fun({{image,Table},_},Acc) -> ordsets:add_element(Table,Acc);
	 (_,Acc) -> Acc
      end, ordsets:new_set(), ets:match_object(?CATALOG,{{image,'_'}, '_'})).

%%%----------------------------------------------------------------

-define(read(What, Table, DefaultValue),
	case ets:lookup(?CATALOG, {What,Table}) of
	    [{_,Value}] -> Value;
	    [] -> DefaultValue
	end
       ).

-define(insert(What, Table, Value),
	ets:insert(?CATALOG, {{What,Table},Value}),Value
       ).



read_image(Table) ->
    ?read(image, Table, none).

insert_image(Table, Image) -> 
    ?insert(image, Table, Image).


read_time_stamp(Table) ->
    ?read(time_stamp, Table, undefined).

insert_time_stamp(Table, TimeStamp) ->
    case read_time_stamp(Table) of
	{_,_,_,Tupd} ->
	    Tpassed = element(4,TimeStamp) - Tupd,
	    insert_stat(Table, upd_stat, Tpassed);
	_ ->
	    ok
    end,
    ?insert(time_stamp, Table, TimeStamp).


read_counter(Table, Name) ->
    ?read({counter,Name}, Table, undefined).

insert_counter(Table, Name, Value) ->
    ?insert({counter,Name}, Table, Value).

update_counter(Table, Name, Amount) ->
    case catch ets:update_counter(?CATALOG,{{counter,Name},Table},Amount) of
	{'EXIT',{badarg,{ets,update_counter,_}}} -> undefined;
	Val -> Val
    end.

update_counter(Table, Name, Amount, Val_if_undefined) ->
    case update_counter(Table, Name, Amount) of
	undefined -> insert_counter(Table, Name, Val_if_undefined);
	N -> N
    end.

read_parameter(Table, Name) ->
    ?read({parameter,Name}, Table, parameter_default_value(Name)).

insert_parameter(Table, Name, Value) ->
    ?insert({parameter,Name}, Table, Value).


read_exec_stat(Table) ->
    ?read(exec_stat, Table, #stats{}).

insert_exec_stat(Table, Time, Ngc, Recl) ->
    S0 = read_exec_stat(Table),
    S = S0#stats{time = upd_stat(S0#stats.time, Time),
		 gc = upd_stat(S0#stats.gc, Ngc),
		 recl = upd_stat(S0#stats.recl, Recl)
		},
    ?insert(exec_stat, Table, S).


read_stat(Table, Name) ->
    ?read(Name, Table, #stat{}).

insert_stat(Table, Name, Value) ->
    S = upd_stat(read_stat(Table,Name), Value),
    ?insert(Name, Table, S).


%%%----
parameter_default_value(Name) ->
    case lists:keysearch(Name,1,?default_values) of
	{value, {Name,Value,Unit}} -> Value;
	false -> undefined
    end.

%%%----
upd_stat(S, V) when record(S,stat) ->
    S#stat{latest = V,
	   sum = S#stat.sum+V,
	   n = S#stat.n+1};

upd_stat(undefined, V) ->
    #stat{latest = V,
	  sum = V,
	  n = 1}.

now_in_secs() ->
    {MS,S,_} = now(),
    MS*1000000 + S.

%%%----------------------------------------------------------------
select_remote_node(Table, LastResort) ->
    select_remote_node1([], [disc_copies,ram_copies], Table, LastResort).

select_remote_node1([N|Ns], Types, Table, LastResort) ->
    case lists:member(N, mnesia:system_info(running_db_nodes)) of
	true -> N;
	false -> select_remote_node1(Ns, Types, Table, LastResort)
    end;

select_remote_node1([], [Type|Types], Table, LastResort) ->
    select_remote_node1(mnesia:table_info(Table,Type),
			Types, Table, LastResort);

select_remote_node1([], [], Table, []) ->
    none;
    
select_remote_node1([], [], Table, LastResort) ->
    select_remote_node1(LastResort, [], Table, []).
    
%%%----------------------------------------------------------------
tell_friends(What) ->
    tell_friends(What, mnesia:system_info(running_db_nodes)).

tell_friends(What, Nodes) ->
    gen_server:abcast(lists:delete(node(),Nodes), ?SERVER_NAME, What).

%%%----------------------------------------------------------------
null_image(Table) -> {0,0,0}.

%%%----------------------------------------------------------------

mk_insert_time_stamp(Table) ->
    insert_time_stamp(Table, {date(),time(),node(),now_in_secs()}).

mk_insert_image(Table) ->
    NewImage = mk_image_stat(Table),
    insert_image(Table, NewImage),
    mk_insert_time_stamp(Table),
    insert_counter(Table, limit, update_limit(Table,NewImage)),
    update_counter(Table, n_upd_local, 1, 1),
    tell_friends({remote_data,Table,NewImage,read_time_stamp(Table)}),
    NewImage.



impatient_mk_insert_image(Table, MaxWait_ms) ->
    Pid = spawn(?MODULE,mk_insert_image_send,[Table,self()]),
    receive
	{image,Image,Pid} ->
	    Image
    after MaxWait_ms ->
	    Pid ! no_msg, % Skip result, but keep calculating an image
	    null_image(Table)
    end.



mk_insert_image_send(Table, Pid) ->
    CatalogPid = whereis(?SERVER_NAME),
    link(CatalogPid),
    Image = mk_insert_image(Table),
    receive
	no_msg -> ok
    after 0 -> Pid ! {image,Image,self()}
    end,
    subscribe(Table),
    unlink(CatalogPid).


update_limit(Table) ->
    update_limit(Table,read_image(Table)).

update_limit(Table,{Nobjs,Sizes,_}) ->
    (Nobjs * read_parameter(Table,upd_limit)) div 100;
update_limit(Table, none) ->
    0.
    

mk_image_stat(Table) ->
    {Ngc0,Recl0,_} = erlang:statistics(garbage_collection),
    {Time,NewImage} = timer:tc(?MODULE,mk_image,[Table]),
    {Ngc1,Recl1,_} = erlang:statistics(garbage_collection),
    insert_exec_stat(Table, Time, Ngc1-Ngc0, Recl1-Recl0),
    NewImage.
    

mk_image(Table) ->
    case mnesia:table_info(Table, storage_type) of
	disc_only_copies -> mk_image_dets(Table);
	unknown -> null_image(Table);
	_ -> mk_image_ets(Table)
    end.

%%%---- ets
mk_image_ets(Tab) ->
    SizesList = 
	lists:map(
	  fun(Pos) ->
		  one_col(Tab, Pos, ets:info(Tab,type))
	  end, lists:seq(2,mnesia:table_info(Tab,arity))),
    {ets:info(Tab,size), list_to_tuple(SizesList), product(SizesList)}.
    

%% TODO: catch exceptions (for example when Key is deleted meanwhile)
%% Until we can use fix_table,
%% we use match 
%one_col(Tab, Pos, Type) ->
%    TmpTab = ets:new(mnemosyne_tmp,[set,public]),
%    fix_table(Tab, true),
%    one_col_ets(Type, Tab, ets:first(Tab), Pos, TmpTab),
%    fix_table(Tab, false),
%    N_objs = ets:info(TmpTab, size),
%    ets:delete(TmpTab),
%    N_objs.

%one_col_ets(_, Table, '$end_of_table', Pos, TmpTab) -> ready;
%one_col_ets(Type, Table, Key, Pos, TmpTab) ->
%    one_col_ets_elms(Type, ets:lookup_element(Table,Key,Pos), TmpTab),
%    one_col_ets(Type, Table, ets:next(Table,Key), Pos, TmpTab).

%one_col_ets_elms(set, E, TmpTab) -> ets:insert(TmpTab, {E});
%one_col_ets_elms(bag, Es, TmpTab) -> ins_elems(Es, TmpTab).

%ins_elems([E|Es], TmpTab) -> ets:insert(TmpTab, {E}), ins_elems(Es,TmpTab);
%ins_elems(_, _) -> ready.

one_col(Tab, Pos, Type) ->
    TmpTab = ets:new(mnemosyne_tmp,[set,public]),
    Blank  = mnesia:table_info(Tab, wild_pattern),
    Patt   = setelement(Pos, Blank, '$1'),
    ResList = ets:match(Tab, Patt),
    insert_ets(ResList, TmpTab),
    N_objs = ets:info(TmpTab, size),
    ets:delete(TmpTab),
    N_objs.

insert_ets([H|R], Tab) ->
    ets:insert(Tab, {H}),
    insert_ets(R, Tab);
insert_ets([], _) ->
    ok.
%%%---- dets
mk_image_dets(Table) ->
    TmpTabs = lists:map(fun(N) -> 
				ets:new(tmp,[set,public])
			end, lists:seq(2,mnesia:table_info(Table,arity))),
    traverse_dets(Table, 0, TmpTabs),
    SizesList = lists:map(fun(TmpTab) ->
				  S = ets:info(TmpTab,size),
				  ets:delete(TmpTab),
				  S
			  end, TmpTabs),
    {dets:info(Table,size), list_to_tuple(SizesList), product(SizesList)}.
    
    
traverse_dets(Table, Islot, TmpTabs) ->
    case dets:slot(Table,Islot) of
	'$end_of_table' -> ready;
	Objs -> traverse_dets_objs(Objs, TmpTabs),
		traverse_dets(Table, Islot+1, TmpTabs)
    end.
   
traverse_dets_objs([Obj|Objs], TmpTabs) ->
    traverse_dets_obj(tl(tuple_to_list(Obj)), TmpTabs),
    traverse_dets_objs(Objs, TmpTabs);
traverse_dets_objs(_, _) ->
    ready.

traverse_dets_obj([Val|Vals], [TmpTab|TmpTabs]) ->
    ets:insert(TmpTab, {Val}),
    traverse_dets_obj(Vals, TmpTabs);
traverse_dets_obj(_,_) ->
    ready.

%%%---- common
product(L) when list(L) -> lists:foldl(fun(E,Acc) -> E*Acc end, 1, L).

%%%================================================================
-record(inf,
	{name = "",
	 image = "",
	 limit = "",
	 n_upd_local = "",
	 n_upd_remote = "",
	 subscr = "",
	 origin = "",
	 latest_upd_sec = "",
	 latest_upd_sec_mean = "",
	 stat_time_latest = "",
	 stat_time_mean = "",
	 stat_time_N = ""
%%%	 stat_gc_latest = "",
%%%	 stat_gc_mean = "",
%%%	 stat_gc_N = "",
%%%	 stat_recl_latest = "",
%%%	 stat_recl_mean = "",
%%%	 stat_recl_N = ""
	}).
	 

-define(str(F,A), lists:flatten(io_lib:format(F,A))).


tbl_info() ->
    Tables = checked_tables(),
    Info = collect_info(Tables),
    {Hdr1,Hdr2,Fmt,Line,Note} = 
	mk_hdr(Info,
	       [{1,"Table"}, {3,"Updates"}, {7,"Update History"},
		{10,"Calc. Cost"}, 
%%%		{10,"Number of GCs"},{13,"GC:Reclaimed"},
		{13,"Parameters"}],
	       #inf{name=" name",
		    image="image",
		    limit="limit",
		    subscr="subscr",
		    n_upd_local="Nloc",
		    n_upd_remote="Nrem",
		    origin="origin",
		    latest_upd_sec = "age",
		    latest_upd_sec_mean = "mean",
		    stat_time_latest="latest",
		    stat_time_mean="mean",
		    stat_time_N="N"
%%%		    stat_gc_latest="latest",
%%%		    stat_gc_mean="mean",
%%%		    stat_gc_N="N",
%%%		    stat_recl_latest="latest",
%%%		    stat_recl_mean="mean",
%%%		    stat_recl_N="N"
		   }),
    [io_lib:format("~s~s~s~s", [Line,Hdr1,Hdr2,Line]),
     write_info(Info, Fmt),
     io_lib:format("~s~s\n", [Line,Note])
    ].


mk_hdr(Data, Hdr1L, Hdr2Rec) ->
    Hdr2L = 
	tl(tuple_to_list(Hdr2Rec)) ++ 
	element(1, lists:mapfoldl(
		     fun(_,N) ->
			     {?str("~w)",[N]),N+1}
		     end, 1, lists:seq(1,length(?default_values)))),
    List_of_lengths =
	lists:map(fun(D) -> lists:map(fun(E) -> length(E) end, D)
		  end, [Hdr2L|Data]),
    MaxL = max_l(tl(List_of_lengths), hd(List_of_lengths)),
    Fmt = mk_format(MaxL, Hdr1L),
    Hdr1 = mk_hdr1(MaxL,Hdr1L),
    TblWidth = length(Hdr1),
    Line = lists:map(fun($|) -> $+;
			($\n) -> $\n; 
			(_) -> $-
		     end, Hdr1), 
    MaxNoteWidth = TblWidth - 20,
    Note =
	lists:flatten(
	  element(1,
		  lists:mapfoldl(
		    fun({Name,Default,Unit},{N,W}) -> 
			    Str = ?str("~w) ~w[~w ~s]   ",
				       [N,Name,Default,Unit]),
			    Wnew = W+length(Str),
			    {if Wnew>MaxNoteWidth -> Str++["\n"];
				true -> Str
			     end, {N+1,Wnew}}
		    end, {1,0}, ?default_values))),
    {Hdr1, ?str(Fmt,Hdr2L), Fmt, Line, Note}.

mk_format(MaxL, Hdr1Def) -> 
    case Hdr1Def of
	[{N,_}|_] when N>1 -> "| ";
	[] -> "| ";
	_ -> ""
    end ++ lists:concat(mk_format(MaxL,Hdr1Def,1))++"\n".


mk_format([W|Ws], [{N1,S}|Ds], N) when N==N1 ->
    ["| ~",W,"s "] ++ mk_format(Ws, Ds, N+1);

mk_format([W|Ws], [{N1,S}|Ds], N) when N<N1 ->
    ["~",W,"s "]  ++ mk_format(Ws, [{N1,S}|Ds], N+1);

mk_format([W|Ws], [], N) ->
    ["~",W,"s "]  ++ mk_format(Ws, [], N+1);

mk_format([], _, _) ->
    ["|"].



mk_hdr1(Maxs, []) -> 
    "| " ++ lists:duplicate(lists:sum(Maxs)+length(Maxs),$ ) ++ "|\n";
mk_hdr1(Maxs, Def) ->
    mk_hdr1(Maxs, Def, 1) ++ "\n".


mk_hdr1([W|Ws], [{N1,S1},{N2,S2}|Ds], N) when N==N1 ->
    {W1,WsT} = mk_hdr1_split([W|Ws],N2-N,0),
    "| " ++ mk_hdr1_s(W1,S1) ++ mk_hdr1(WsT, [{N2,S2}|Ds], N2);

mk_hdr1([W|Ws], [{N1,S}], N) when N==N1 ->
    {W1,WsT} = mk_hdr1_split([W|Ws],100,0),
    "| " ++ mk_hdr1_s(W1,S) ++ "|";

mk_hdr1([W|Ws], [{N1,S}|Ds], N) when N<N1 ->
    "|  " ++ lists:duplicate(W,$ ) ++ mk_hdr1(Ws, [{N1,S}|Ds], N+1);

mk_hdr1(_, [], _) ->
    "|".

mk_hdr1_split([W|Ws], I, Acc) when I>0 -> mk_hdr1_split(Ws,I-1,Acc+W+1);
mk_hdr1_split(Ws, _, Acc) -> {Acc,Ws}.

mk_hdr1_s(W,S) when length(S)>W ->
    lists:duplicate(W, $*);
mk_hdr1_s(W,S) ->
    Nspaces = W - length(S),
    Npfx = Nspaces div 2,
    lists:duplicate(Npfx, $ ) ++ S ++ lists:duplicate(Nspaces-Npfx,$ ).
    



max_l([L|Ls], MaxL) ->
    max_l(Ls,
	  lists:map(fun(N) ->
			    lists:max([lists:nth(N,L), lists:nth(N,MaxL)])
		    end, lists:seq(1,length(MaxL))));
max_l([], MaxL) ->
    MaxL.



write_info(Info, Fmt) ->
    lists:map(
      fun(L) when list(L) -> io_lib:format(Fmt, L);
	 (X) -> io_lib:format(" ???: ~w\n", [X])
      end, Info).
	      

cntr_info(Tab, Name) ->
    case read_counter(Tab,Name) of
	undefined -> "";
	Cnt -> string(int,Cnt)
    end.

collect_info(Tables) ->
    NowSecs = now_in_secs(),
    lists:map(
      fun(Tab) ->
	      S = read_exec_stat(Tab),
	      R = #inf{name = atom_to_list(Tab),
		       image = case read_image(Tab) of
				   none -> "";
				   {Ar,F1,Prod} -> ?str('~W', [{Ar,F1,'_'},8])
			       end,
		       limit = cntr_info(Tab,limit),
		       n_upd_local = cntr_info(Tab,n_upd_local),
		       n_upd_remote = cntr_info(Tab,n_upd_remote),
		       subscr = case
				 lists:member(whereis(?SERVER_NAME),
					      mnesia:table_info(Tab,
								subscribers))
				    of
				    true -> "Yes";
				    false -> "No"
				end,
		       origin = case read_time_stamp(Tab) of
				    {_,_,Node,_} -> atom_to_list(Node);
				    _ -> ""
				end,
		       
		       latest_upd_sec = 
		       begin
			   MinInterv=read_parameter(Tab,min_upd_interval),
			   case read_time_stamp(Tab) of
			       {_,_,_,Sec} when Sec+MinInterv>NowSecs -> 
				   "("++string(time_s,(NowSecs-Sec))++")";
			       {_,_,_,Sec} -> 
				   string(time_s,(NowSecs-Sec))++" ";
			       _ -> ""
			   end
		       end,
		       latest_upd_sec_mean =string(time_s, 
						   stat(mean,
							read_stat(Tab,
								  upd_stat))),
		       stat_time_latest=string(time_us, 
					       stat(latest,S#stats.time)),
		       stat_time_mean = string(time_us,
					       stat(mean,S#stats.time)),
		       stat_time_N = string(int, 
					    stat(n,S#stats.time))

%%%		       stat_gc_latest = string(int, stat(latest,S#stats.gc)),
%%%		       stat_gc_mean = string(int, stat(mean,S#stats.gc)),
%%%		       stat_gc_N = string(int, stat(n,S#stats.gc)),
		       
%%%		       stat_recl_latest = string(int,
%%%						 stat(latest,S#stats.recl)),
%%%		       stat_recl_mean = string(int, stat(mean,S#stats.recl)),
%%%		       stat_recl_N = string(int, stat(n,S#stats.recl))
		      },
	      tl(tuple_to_list(R)) ++ 
		  lists:map(
		    fun({Name,_,_}) ->
			    io_lib:write(read_parameter(Tab,Name))
		    end, ?default_values)
      end, Tables).


stat(latest, S) when record(S,stat), integer(S#stat.latest) ->
    S#stat.latest;

stat(mean, S) when record(S,stat), integer(S#stat.sum), S#stat.n=/=0 ->
    S#stat.sum div S#stat.n;

stat(n, S) when record(S,stat), integer(S#stat.n) -> 
    S#stat.n;

stat(_, _) ->
    undefined.


string(clock, {H,M,S}) -> ?str("~2..0w:~2..0w:~2..0w", [H,M,S]);
string(date, {Y,M,D}) -> ?str("~w-~2..0w-~2..0w", [Y,M,D]);
string(time_us, MicroSecs) when integer(MicroSecs) -> 
    if
	MicroSecs < 1000 -> ?str("~wus",[MicroSecs]);
	MicroSecs < 3000 -> ?str("~4.2fms",[MicroSecs/1000]);
	true -> string(time_ms, MicroSecs div 1000)
    end;
string(time_ms, MilliSecs) when integer(MilliSecs) ->
    if
	MilliSecs < 1000 -> ?str("~wms",[MilliSecs]);
	MilliSecs < 9000 -> ?str("~4.2fs",[MilliSecs/1000]);
	true -> string(time_s, MilliSecs div 1000)
    end;
string(time_s, TotSecs) when integer(TotSecs) ->
    Hrs = TotSecs div 3600,
    Mins= (TotSecs div 60) rem 60,
    Secs= TotSecs rem 60,
    if 
	Hrs>24 -> ?str("~wd~2..0wh~2..0wm~2..0ws",
		       [Hrs div 24,Hrs rem 24,Mins,Secs]);
	Hrs >0 -> ?str("~wh~2..0wm~2..0ws",[Hrs,Mins,Secs]);
	Mins>0 -> ?str("~wm~2..0ws",[Mins,Secs]);
	true -> ?str("~ws",[Secs])
    end;
string(int, I) when integer(I) -> integer_to_list(I);
string(_,_) -> "".

