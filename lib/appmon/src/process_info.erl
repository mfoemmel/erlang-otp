-module(process_info).
-behavior(gen_server).

-export([start/0,stop/0,start_link/0]).
-export([get_host/0,get_applications/1,get_processes/3,send_trace/1]).
-export([get_nodes/0,is_node/1,get_process_data/2,get_application_keys/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).
-export([code_change/3]).
-record(data,{que=undef,procs=undef,links=undef,links2=undef}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%Public functions to retrieve information about which application    %%
%% at the node                                                        %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
    gen_server:start({local,proc_info},process_info,[],[]).

start_link()->
    gen_server:start_link({local,proc_info},process_info,[],[]).

stop()->
    gen_server:call(proc_info,stop,1000).

get_host()->
    gen_server:call(proc_info,{host_name}).
    
get_applications(Node)->
    gen_server:call(proc_info,{applications,Node}).

get_application_keys(App,Node)->
    gen_server:call(proc_info,{application_keys,App,Node}).

get_processes(App,Mode,Node)->
    gen_server:call(proc_info,{procs,App,Mode,Node}).

get_process_data(Pid,Node)->
    gen_server:call(proc_info,{process_data,Pid,Node}).
    
send_trace(Proc)->
    gen_server:call(proc_info,{send_trace,Proc}).

get_nodes()->
    gen_server:call(proc_info,nodes).

is_node(Node)->
        gen_server:call(proc_info,{is_node,list_to_atom(Node)}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%Callback funktions used by the genserver                            %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Arg)->
    {ok,ets:new(procs,[])}.


handle_call({host_name},From,State)->
    {reply,get_host_name(),State};

handle_call({applications,Node},From,State)->
    {reply,get_name_of_applications(Node),State};

handle_call({application_keys,App,Node},From,State)->
    {reply,retrieve_application_keys(App,Node),State};

handle_call({procs,App,Mode,Node},From,State) ->
    {reply,{get_application_info(App,Mode,Node),State},State};

handle_call({process_data,Pid,Node},From,State) ->
    {reply,get_process_info(Pid,Node),State};
  
handle_call({send_trace,Proc},From,State) ->
    New_state=send_trace_to_proc(Proc,State),
    {reply,ok,New_state};

handle_call(nodes,From,State) ->
    {reply,[node()|nodes()],State};

handle_call({is_node,Node},From,State) ->
    {reply,control_node(Node),State};


handle_call(stop,From,State) ->
    {stop,normal,ok,State}.


handle_cast(_,State)->
    {noreply,State}.

handle_info(_,State)->
    {noreply,State}.

terminate(Reason,State)->
    ok.
code_change(_,State,_)->
    {ok,State}.


%%Returns the hosts name as as a string
get_host_name()->
    atom_to_list(node()).

%%returns a the current running applications in a list of atoms 
get_name_of_applications(Node)->
    Get_elem_fun=fun(Process)->
			 element(1,Process)
		 end,
    Del_not_run_fun=fun(Name)->
			    case Name of
				webappmon->
				    false;
				_->
				    case catch rpc:call(Node,application_controller,get_master,[Name]) of
					Pid when pid(Pid)->
					    true;
					_ ->
					    false
				    end
			    end
		    end,
    lists:filter(Del_not_run_fun,(lists:map(Get_elem_fun,(rpc:call(Node,application,which_applications,[]))))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%Public functions to retrieve the processes in an applications       %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%The public function to call to retrieve the processes in a application
%%App:  Atom that represent the application ex. mnesia
%%Mode: Atom all|sup|sup_childs
get_application_info(App,Mode,Node)->
    case rpc:call(Node,application_controller,get_master,[App]) of
	Pid when pid(Pid) ->
	    start_collecting_data(Pid,Mode,Node);
	_Other ->
	    unknown
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%%Private functions to retrieve the processes in an application       %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Initiate the database, get the processes and the links
%%Then move the data to lists and return them
start_collecting_data(Pid,Mode,Node)->
    Db=get_database(),
    {Db2,Tree}=build_graph({master,Pid},Db,Pid,Mode,Node),
    {Q,P,L1,L2}=get_data(Db2),
    delete_database(Db2),
    Tree.
   %% Link.
    %%{P,L1,L2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%When all processes in the application is collected 
%%Fix secondary links and return the tree
build_graph(finish,Db,Grp,Mode,Node)->
    Db=fix_links(Db,Grp,Node), 
    delete_unwanted(Db,Mode,Grp),
    Tree=start_tree(Db,Node),
    {Db,Tree};


%%The tought is 1. Get the processes that Pid links to Pid. Pid is the application_master the first time
%%              2. Add the Process to the database and clear the list of 
%%                 children from processes Which for some resason not shuold 
%%                 be there.
%%              3. Que the children, so we later can se if they has any links 
%%              4. Add links to the childrens
%%              5. When the whole tree is retreived remove the unnecessary processes depending on the mode
%%              6. Take all links that points to the same Pid and sort out the primary and secondary relations
%%                 If more than one process links to the same process the relation between a supervisor and a
%%                 process is primary the rest is secondary, there is no different in real world just in logic
%%                 between a secondary and a primary relation   





%%Pid :The process that we shall work with 
%%Db  :A data record that has referenses to the ets:tables
%%Grp :The groupleader of the app
%%Mode:Mode is which processes to retrieve all|sup|sup_child
build_graph(Pid,Db,Grp,Mode,Node)->
    Childs=get_childs(Pid,Mode,Node),
    Childs2=add_and_remove(Childs,Pid,Db,Grp,Node),
    Q2=queue_childs(Db#data.que,Childs2),
    add_childs(Pid,Db,Grp,Mode,Childs2,1),
    case queue:out(Q2) of
	{empty,_}->
	    build_graph(finish,Db,Grp,Mode,Node);
	{{value,NPid},Q3}->
	    Db2=#data{que=Q3,procs=Db#data.procs,links=Db#data.links,links2=Db#data.links2},
	    build_graph(NPid,Db2,Grp,Mode,Node)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Collect the processes which the current process has a link to
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%The Pid is now the application_master and the application masters child is the 
%%Application supervisor but in reality there are 2 application master processes 
%%fix this by reorder the processes a little.
get_childs({master,Pid},_Mode,Node)when pid(Pid)->
    %%Get the master pid
    MPid= case application_master:get_child(Pid) of
	{Pid1,Name}->
		 Pid1;
	Pid2->
		 Pid2
    end,
    %%Get the second masterpid and order them correct
    case
	rpc:call(Node,erlang,process_info,[MPid,links])of
	{links, [H|T]}-> 
	    Pids=[MPid|T],
	    [H|Pids];
	_ ->
	    MPid
    end;


get_childs({Pid,_Name},_Mode,Node) when pid(Pid),Node==node(Pid)->
    {links,Links}=rpc:call(Node,erlang,process_info,[Pid,links]),
    Links;

get_childs(Pid,_Mode,Node)when pid(Pid),Node==node(Pid)->
    {links,Links}=rpc:call(Node,erlang,process_info,[Pid,links]),
    Links; 

get_childs(Pid,_Mode,Node)when pid(Pid),Node/=node(Pid)->
    [];

get_childs(Port,_Mode,Node) when port(Port)-> 
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Add the links to the database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%The first cassse when its the master process there are only one real child
%% even thogh thre arer more links
add_childs({master,Pid},Db,Grp,Mode,[Child|Rest],N)->
    add_child(Pid,Db,Grp,Mode,Child,N);



add_childs(Pid,Db,Grp,Mode,[],N)->
    ok;
add_childs(Pid,Db,Grp,Mode,[Child|Rest],N)->
    add_child(Pid,Db,Grp,Mode,Child,N),
    add_childs(Pid,Db,Grp,Mode,Rest,N+1).

add_child(Pid,Db,Grp,Mode,Child,N)->
    case ets:match_object(Db#data.links,{Pid,Child,'_'}) of
	[]->
	    ets:insert(Db#data.links,{Pid,Child,N});
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Add the list of processes to the que
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

queue_childs(Que,[])->
    Que;
queue_childs(Que,[H|T])->
    Q=queue:in(H,Que),
    queue_childs(Q,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Create the ets:tables and return them in a data record
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_database()->
    P=ets:new(procs,[]),
    L=ets:new(link,[bag]),
    L2=ets:new(link2,[bag]),
    Q=queue:new(),
    ets:insert(P,{whereis(application_controller),crap}),
    ets:insert(P,{whereis(gs),crap}),
    #data{que=Q,procs=P,links=L,links2=L2}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Free the ets:tables that we no longer need
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_database(Db)->
    ets:delete(Db#data.procs),
    ets:delete(Db#data.links),
    ets:delete(Db#data.links2).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Take out the data from the ets:tables and return them as lists 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_data(Db)->
    P = ets:tab2list(Db#data.procs),
    L1= ets:tab2list(Db#data.links),
    L2= ets:tab2list(Db#data.links2),
    {queue:to_list(Db#data.que),P,L1,L2}. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%The processess that we already has added to the database is 
%%not child to the current process so we dont need to add them a
%%second time. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_used_childs([],Db,New_list)->
    lists:reverse(New_list);
remove_used_childs([Child|Rest],Db,New)->
    case ets:lookup(Db#data.procs,Child) of
	[]->
	    remove_used_childs(Rest,Db,[Child|New]);
	_ ->
	    remove_used_childs(Rest,Db,New)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%take the list of links and separate it to a list with ports and a list with 
%%the processes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
separate_ports([],Pids,Ports)->
    {Pids,Ports};

separate_ports([Child|Rest],Pids,Ports)->
    case Child of
	P when port(P)->
	    separate_ports(Rest,Pids,[Child|Ports]);
	_Ohter ->
	    separate_ports(Rest,[Child|Pids],Ports)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Add the current pid to the ets:table with processes and clear the list of
%%childs from processes that not shuold be there
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%This is the first case no childs are used so thats not necessary
add_and_remove(Childs,{master,Pid},Db,Grp,Node)when pid(Pid), Node==node(Pid)->
    ets:insert(Db#data.procs,{Pid,{master,master},controller}),
    {Pids,Ports}=separate_ports(Childs,[],[]),
    Ports++Childs;


%%This clause is removable when using only link as retrieving mode  
add_and_remove(Childs,{Pid,Name},Db,Grp,Node)when pid(Pid),Node==node(Pid)->
    ets:insert(Db#data.procs,{Pid,rpc:call(Node,erlang,process_info,[Pid,registered_name])}), 
    {Pids,Ports}=separate_ports(Childs,[],[]),
    Childs1=remove_used_childs(Pids,Db,[]),
    Childs2=remove_others_childs(Childs1,Grp,Node),
    Ports++Childs2;


add_and_remove(Childs,Pid,Db,Grp,Node)when pid(Pid),Node==node(Pid)->
    ets:insert(Db#data.procs,{Pid,rpc:call(Node,erlang,process_info,[Pid,registered_name])}), 
    {Pids,Ports}=separate_ports(Childs,[],[]),
    Childs1=remove_used_childs(Pids,Db,[]),
    Childs2=remove_others_childs(Childs1,Grp,Node),
    Ports++Childs2;

add_and_remove(Childs,Pid,Db,Grp,Node)when pid(Pid),Node/=node(Pid)->
    [];

%%take care of the ports donmt add them 
%%to the table with processes 
add_and_remove(Childs,Pid,Db,Grp,Node)when port(Pid)->
    [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Control that the applications groupleader is the process Pids groupleader
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
group_leader_check({Pid,Name},Grp,Node)->
    group_leader_check(Pid,Grp,Node);

group_leader_check(Pid,Grp,Node)->
    case  rpc:call(Node,erlang,process_info,[Pid,group_leader]) of
	{_Item,Grp}->
    	    yes;
	_ ->
	    no
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Take the list of childs and remove the ones with other groupleaader
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_others_childs(Childs,Grp,Node)->
    lists:filter(fun(Child)->
		   case group_leader_check(Child,Grp,Node) of
		       yes->
			   true;
		       no->
			   false
		   end
	   end,Childs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%These methods are for stage to when we has retrieved all the pids      %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Mark the processes in the procs table as that is supervised as either 
%%supervisor or worker
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fix_links(Db,Leader,Node)->
    {Sup,Work}=mark_supervisors_workers(Db,Leader,Node),
    ets:match_delete(Db#data.procs,{'_',crap}),
    [Pid|Procs]=ets:tab2list(Db#data.procs),
    N_links=get_n_links(Procs,Db#data.links,[]),
    N_links2=take_sup_links(Sup,Db#data.links,N_links),
    add_shared_links(N_links2,Db#data.links2),
    Db.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Add the links that points to the same child and not child to a supervisor
%%to the shared links table 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_shared_links(N_links,Links2)->
    Insert_fun=fun(Link)->
		       ets:insert(Links2,Link)
	       end,
    lists:map(fun(List)->
		      lists:map(Insert_fun,List)
	end,N_links).
		    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%take the list of links that point to the same children and remove the 
%%ones that is child to supervisors

%%The first argument is a list of the supervisors
%%N_links contains the a list of lists where each list is a list of the links
%% that points to the same child
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
take_sup_links([],Db,N_links)->
    N_links;

take_sup_links([H|Supervised],Links_table,N_links)->
   N_list_fun=fun(Link)->
		      insert_sup_links(Link,H,Links_table)
	      end, 
    N_links2=lists:map(fun(Link_list)->
		      lists:filter(N_list_fun,Link_list)
	      end,N_links),

    take_sup_links(Supervised,Links_table,N_links2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Insert the supervised links in the primary links list
%%This method should be used as a fun to the filterfunction in take_sup_links.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insert_sup_links({From,To,N},Sup,Links_table)->
    case From of
	Sup -> 
	    ets:insert(Links_table,{From,To,N}),
	    false;
	_ ->
	    true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Get the links which points to the same children
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_n_links([],Links,N_link)->
    N_link;

get_n_links([{Pid,_,_}|Procs],Links,N_link)->
    case ets:match_object(Links,{'_',Pid,'_'}) of
	L when length(L)>1 ->
	    ets:match_delete(Links,{'_',Pid,'_'}),
	    get_n_links(Procs,Links,[L|N_link]);
	L ->
	    get_n_links(Procs,Links,N_link)
    end;
get_n_links([{Pid,_}|Procs],Links,N_link)->
    case ets:match_object(Links,{'_',Pid,'_'}) of
	L when length(L)>1 ->
	    ets:match_delete(Links,{'_',Pid,'_'}),
	    get_n_links(Procs,Links,[L|N_link]);
	L ->
	    get_n_links(Procs,Links,N_link)
    end.

	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Mark the processes that is in the supervisor tree as either worker or supervisor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mark_supervisors_workers(Db,Leader,Node)->
    %%Get the Supervisors and workers
    {Sup_list,Worker_list}=get_by_supervisors1(Leader),
    %%Update the supervisor pids
    lists:map(fun(Pid)->
	      ets:insert(Db#data.procs,{Pid,rpc:call(Node,erlang,process_info,[Pid,registered_name]),supervisor})
	end,Sup_list),
    %%update the worker pids.
     lists:map(fun(Pid)->
	      ets:insert(Db#data.procs,{Pid,rpc:call(Node,erlang,process_info,[Pid,registered_name]),worker})
	 end,Worker_list),
    {lists:reverse(Sup_list),Worker_list}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%sThe second way to retrieve the applications processes is to go by the 
%%supervisr tree, get the processes in the supervisortree.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_by_supervisors1(Leader)->
    case application_master:get_child(Leader) of
	{Pid,Name}->
	    get_by_supervisors([{namn,Pid,supervisor,list_of_mods}],[],[]);
	Pid ->
	    get_by_supervisors([{namn,Pid,supervisor,list_of_mods}],[],[])
    end.

get_by_supervisors([],Sup,Work)->
    {Sup,Work};

get_by_supervisors([{_,Pid,supervisor,_}|Rest],Sup,Work)when pid(Pid)->
    Childs=supervisor:which_children(Pid),
    Childs2=lists:append(Childs,Rest),
    get_by_supervisors(Childs2,[Pid|Sup],Work);

get_by_supervisors([{_,Pid,_,_}|Rest],Sup,Work)when pid(Pid) ->
    get_by_supervisors(Rest,Sup,[Pid|Work]);

get_by_supervisors([Whatever|Rest],Sup,Work)->
    get_by_supervisors(Rest,Sup,Work).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%These methods are for stage to when we has retrieved all the pids      %%
%%And fixed secondary and primary links                                  %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Use pattern matching to select mode and delete the unneccesary Pids    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


delete_unwanted(Db,sup_child,App_pid)->
    delete_not_in_supervisor_tree(Db),
    add_main_link(Db,App_pid),
    Db;

delete_unwanted(Db,all,App_pid)->
    Db;
delete_unwanted(Db,sup,App_pid)->
    delete_workers(Db),
    delete_not_in_supervisor_tree(Db),
    add_main_link(Db,App_pid),
    Db.

add_main_link(Db,App_pid)->
    case application_master:get_child(App_pid) of
	{Pid,Name} when pid(Pid)-> 
	    ets:insert(Db#data.links,{App_pid,Pid,1});
	Pid when pid(Pid)->
	    ets:insert(Db#data.links,{App_pid,Pid,1});
	_ ->
	    false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Delete the processes that is in the supervisortree but workers and their
%% links
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_workers(Db) ->
    Pids=ets:match_object(Db#data.procs,{'_','_',worker}),
    lists:map(fun({Pid,_,_})->
		%%Remove the unwanted from the process table
		ets:match_delete(Db#data.procs,{Pid,'_','_'}),
		%%Remove the links to and from the pid
		ets:match_delete(Db#data.links,{Pid,'_','_'}),
		ets:match_delete(Db#data.links,{'_',Pid,'_'}),
		ets:match_delete(Db#data.links2,{Pid,'_','_'}),
		ets:match_delete(Db#data.links2,{'_',Pid,'_'})	
	end,Pids),
    Pids.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Delete the processes thats not in the supervisor tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete_not_in_supervisor_tree(Db)->
    Pids=ets:match_object(Db#data.procs,{'_','_'}),
    lists:map(fun({Pid,_})->
		%%Remove the unwanted from the process table
		ets:match_delete(Db#data.procs,{Pid,'_'}),
		%%Remove the links to and from the pid
		ets:match_delete(Db#data.links,{Pid,'_','_'}),
		ets:match_delete(Db#data.links,{'_',Pid,'_'}),
		ets:match_delete(Db#data.links2,{Pid,'_','_'}),
		ets:match_delete(Db#data.links2,{'_',Pid,'_'})
	end,Pids),
    Pids.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Start generating the tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_tree(Db,Node)->
    case get_master(Db)of
	no->
	    false;
	Pid->
	    build_node(Pid,Db,Node)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Build a node and then its runs itself on every child to the current pid
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_node(Pid,Db,Node)when pid(Pid),Node==node(Pid)->
    Sort_fun=fun sort_order/2,
    Fix_sec_name_fun=fun(Pid2)->
			     get_link_name(Pid2,Db)
		     end,
    Build_tree_fun=fun({_,Pid1,_})->
			   build_node(Pid1,Db,Node)
		   end,
    Childs=ets:match_object(Db#data.links,{Pid,'_','_'}),
    Childs1=lists:sort(Sort_fun,Childs),
    Sec_childs=ets:match_object(Db#data.links2,{Pid,'_','_'}),
   {get_name(Pid,Db),lists:map(Build_tree_fun,Childs1) , lists:map(Fix_sec_name_fun,Sec_childs) };
    %%{Pid ,lists:map(Build_tree_fun,Childs1) , Sec_childs }; 

build_node(Pid,Db,Node)when pid(Pid),Node/=node(Pid)->
    {"Runs on another node:" ++ erlang:pid_to_list(Pid) ,[],[]};
build_node(Pid,Db,Node)when port(Pid)->
    {"Port :" ++ erlang:port_to_list(Pid) ,[],[]}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Select the name of the Pid frpm the database where we previosly added  %%
%% it                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_name(Pid,Db)->    
    case ets:lookup(Db#data.procs,Pid) of
	[{_,{_,master},_}]->
	    pid_to_list(Pid);
	[{_,{_,Name}}]->
	    atom_to_list(Name) ++ " : " ++ pid_to_list(Pid);
	[{_,{_,Name},_}] ->
	    atom_to_list(Name) ++ " : " ++ pid_to_list(Pid);
	_ ->
	    pid_to_list(Pid)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Select the name of hte process which we have a link to                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_link_name({_,Pid,_},Db)when pid(Pid)->    
    case ets:lookup(Db#data.procs,Pid) of
	[{_,{_,Name}}]->
	    atom_to_list(Name) ++ " : " ++ pid_to_list(Pid);
	[{_,{_,Name},_}] ->
	    atom_to_list(Name) ++ " : " ++ pid_to_list(Pid);
	 Other->
	    pid_to_list(Pid)
    end;
%%When port..
get_link_name({_,Pid,_},Db)when port(Pid)->    
    "Port :" ++ " : ";% ++ erlang:port_to_list(Pid);
get_link_name(_,_)->
    "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Used by the BIF that sorts lists to  sort the links in the order       %%
%%they where added                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Sort in ascending order
sort_order({_,_,N1},{_,_,N2})when N1>N2->
    true;
sort_order(N1,N2)->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Select the Pid of the application master                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_master(Db)->
    case ets:match_object(Db#data.procs,{'_',{master,master},controller}) of
	[{Pid,_,_}|Rest]->
	    Pid;
	_ ->
	    no
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%These methods are used to handle the trace signal                      %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%These methods are used to handle the distributed part                  %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%The main function to handle the trace signal, Controls if the Pricess  %%
%%is in teh table  whit traced processes if so it stop the trace otherwise %%
%5it starts trace the process                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_trace_to_proc(Proc,Traced_tab)->
    Pid=list_to_pid(Proc),
    Key=get_key(Pid),
    New_dict = case catch ets:lookup(Traced_tab,Key) of
		   []->
		       trace_process(Pid,Key,true,Traced_tab);
		   [Object]->
		       trace_process(Pid,Key,false,Traced_tab);
		   Error ->
                       io:fwrite("MegaError= Key = ~w  ~n",[Key]),
		       %%This is an error do nothing....
		       Traced_tab
	       end,
     filter_procs(Traced_tab,ets:tab2list(Traced_tab)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Control that the processess in the ets:table with traced processes is %%
%%alive if not remove them                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filter_procs(Tab,Tab_list)->
    lists:foreach(fun({Key,Val})->
			  is_alive(Key,Tab)
		  end,Tab_list),
    Tab.


is_alive(Key,Tab)->
    case get_pid(Key) of
	nopid->
	    false;
	Pid->
	    is_alive2(Pid,Key,Tab)
    end.

is_alive2(Pid,Key,Tab)->
    case catch rpc:call(node(Pid),erlang,is_process_alive,[Pid]) of
	true->
	    true;
	_ ->
	    delete_dead_proc(Tab,Key)
    end.

delete_dead_proc(Tab,Key)->
    case catch ets:delete(Tab,Key) of
	_->
	    ok
    end.

	    
%%Key is either a pid in list form or  Pidname:Pid in listform
get_pid(Key)->
    case list_to_pid(string:substr(Key,string:rchr(Key,$<))) of
	Pid when pid(Pid)->
	    Pid;
	_->
	    nopid
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Tries to toggle the trace flag for the process                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trace_process(Pid,Key,On_or_off,Procs_tab)when pid(Pid)->
    case rpc:call(node(Pid),sys,trace,[Pid,On_or_off,1000]) of
	timeout->
	    Nod=node(Pid),
	    io:fwrite("timeout node= ~w, Pid= ~w mode= ~w ~n",[Nod,Pid,On_or_off]),
	    Procs_tab;
	{badrpc,_}->
	    Nod=node(Pid),
	    io:fwrite("badrpc  node= ~w, Pid= ~w mode= ~w ~n",[Nod,Pid,On_or_off]),
	    Procs_tab;
	Res->
	    Nod=node(Pid),
	    io:fwrite("anymode ~w  node= ~w, Pid= ~w mode= ~w ~n",[Res,Nod,Pid,On_or_off]),
	    store_key(Key,On_or_off,Procs_tab)
    end;

trace_process(_,_,_,Proc_tab)->
    Proc_tab.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Stores the pid in the dictionary of traced procs                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_key(Key,On_or_off,Proc_tab)->
    case On_or_off of
	true->
	    ets:insert(Proc_tab,{Key,On_or_off}),
	    Proc_tab;
	_ ->
	    ets:delete(Proc_tab,Key),
	    Proc_tab
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Tries to select the name of the process from the process dict          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_key(Pid)->
    case rpc:call(node(Pid),erlang,process_info,[Pid,registered_name]) of
	[]->
	    pid_to_list(Pid);
	{registered_name,Name}->
	    atom_to_list(Name) ++ " : " ++ pid_to_list(Pid)
    end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                       %%
%%These methods are used to handle the distributed part                  %%
%%                                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Controls wheather the current Node is really a node                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
control_node(Node)->
    case lists:member(Node,[node()|nodes()]) of
	true->
	    {true,Node,atom_to_list(Node)};
	false ->
	    {false,node(),atom_to_list(node())}
    end.



get_process_info(Pid,Node)->
    case rpc:call(Node,erlang,process_info,[Pid]) of
	{badrpc,_}->
	    [{error,"Please try again"}];
	Res ->
	    Res
    end.


retrieve_application_keys(App,Node)->
    case rpc:call(Node,application,get_all_key,[App]) of
	{badrpc,_} ->
	    {error,badrpc};
	undefined ->
	    {error,badapp};
	{ok,Keys} ->
	    {ok,Keys};
	_->
	    {error,general_error}
    end.




























































