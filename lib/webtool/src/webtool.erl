                                                                        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                     %%
%%The general idea is:                                                 %%
%%                                                                     %%
%%                                                                     %%
%% 1. Scan through the  path for *.tool files and find all the         %%
%%    web based tools. Query each tool for configuration data          %%
%% 2. Add Alias for Erlscript and html to the httpd.conf file          %%
%%    for each tool.                                                   %%
%%                                                                     %%
%% 3. Start the webbserver                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(webtool).
%Export the api functions
-export([start/0,start/2,stop/0]).

%The Web api
-export([started_tools/2,toolbar/2,start_tools/2,stop_tools/2]).

%API against other tools
-export([is_localhost/0]).
%Debug export 
-export([get_tools1/1]).


%Export the callback functions for the webTool
-export([init/1,handle_call/3,handle_cast/2]).
-export([handle_info/2,terminate/2,code_change/3]).

-include_lib("kernel/include/file.hrl").



-behaviour(gen_server).
-record(state,{priv_dir,app_data,supvis,web_data,started=[]}).

-define(WEBTOOL_ALIAS,{webtool,{alias,{erl_alias,"/webtool",[webtool]}}}).
-define(HEADER,"Pragma:no-cache\r\n Content-type: text/html\r\n\r\n").
-define(HTML_HEADER,"<HTML>\r\n<HEAD>\r\n<TITLE>WebTool</TITLE>\r\n</HEAD>\r\n<BODY BGCOLOR=\"#FFFFFF\">\r\n").
-define(HTML_HEADER_RELOAD,"<HTML>\r\n<HEAD>\r\n<TITLE>WebTool
                             </TITLE>\r\n</HEAD>\r\n
                             <BODY BGCOLOR=\"#FFFFFF\" onLoad=reloadCompiledList()>\r\n").

-define(HTML_END,"</BODY></HTML>").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Api functions to the genserver.                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
%
%----------------------------------------------------------------------

start()->
    start(get_path(),get_standard_data()).
	  

start(Path,standard_data)->
    Data=get_standard_data(),
    start(Path,Data);
    
start(standard_path,Port) when integer(Port)->
    Path=get_path(),
    case get_standard_data(Port)of
	error->
	    {error,"unable to get ipaddress"};
	Data ->
	    start(Path,Data)
    end;

start(standard_path,Data)->
    Path=get_path(),
    start(Path,Data);
    
start(Path,Port) when integer(Port)->
    case get_standard_data(Port)of
	error->
	    {error,"unable to get ipaddress"};
	Data ->
	    start(Path,Data)
    end;
	
start(Path,Data)->
    gen_server:start({local,web_tool},webtool,{Path,Data},[]).

stop()->
    gen_server:call(web_tool,stoppit).

%----------------------------------------------------------------------
%Web Api functions called by the web
%----------------------------------------------------------------------
started_tools(Env,Input)->
    gen_server:call(web_tool,{started_tools,Env,Input}).

toolbar(Env,Input)->    
    gen_server:call(web_tool,{toolbar,Env,Input}).

start_tools(Env,Input)->
    gen_server:call(web_tool,{start_tools,Env,Input}).

stop_tools(Env,Input)->
    gen_server:call(web_tool,{stop_tools,Env,Input}).
%----------------------------------------------------------------------
%Support API for other tools
%----------------------------------------------------------------------

is_localhost()->
    gen_server:call(web_tool,is_localhost).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%The gen_server callback functions that builds the webbpages       %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({started_tools,Env,Input},_,State)->
    {reply,started_tools_page(State),State};

handle_call({toolbar,Env,Input},_,State)->
    {reply,toolbar(),State};

handle_call({start_tools,Env,Input},_,State)->
    {NewState,Page}=start_tools_page(Env,Input,State),
    {reply,Page,NewState};

handle_call({stop_tools,Env,Input},_,State)->
    {NewState,Page}=stop_tools_page(Env,Input,State),
    {reply,Page,NewState};

handle_call(stoppit,_From,Data)->
    {stop,normal,ok,Data};

handle_call(is_localhost,_From,Data)->
    Result=case httpd_util:key1search(Data#state.web_data,"BindAddress",localhost) of
	"127.0.0.1" ->
	    true;
	_IpNumber ->
	    false 
    end,
    {reply,Result,Data}.


handle_info(Message,State)->
    {noreply,State}.

handle_cast(Request,State)->
    {noreply,State}.

code_change(_,State,_)->
    {ok,State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The other functions needed by the gen_server behaviour 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------------------------------------
% Start the gen_server
%----------------------------------------------------------------------
init({Path,Config_data})->
    case is_dir(Path) of
	true->
	    case get_data() of
		{ok,Data}->
		    ets:insert(Data,?WEBTOOL_ALIAS),
		    case webtool_sup:start_link() of
			{ok, Pid} ->
			    case start_webserver(Data,Path,Config_data,Pid) of
				{ok,_}->
				    print_url(Config_data),		      
				    {ok,#state{priv_dir=Path,
					       app_data=Data,
					       supvis=Pid,
					       web_data=Config_data}};
				{error,Error} ->
				    {stop,{error,Error}}
			    end;
			Error ->
			    {stop,Error}
		    end;
	       {error,Error}->
		    {stop,{error,Error}}
	    end;
	_ ->
	   {stop,{error,error_dir}}
    end.


terminate(Reason,Data)->
    %%shut down the webbserver
    shutdown_server(Data),
    %%Shutdown the different tools that are started with application:start
    shutdown_apps(Data),
    %%Shutdown the supervisor and its children will die
    shutdown_supervisor(Data),
    ok.

print_url(ConfigData)->
    Server=httpd_util:key1search(ConfigData,"ServerName","undefined"),
    Port=httpd_util:key1search(ConfigData,"Port","undefined"),
    Address=httpd_util:key1search(ConfigData,"BindAddress","undefined"),
    io:format("WebTool is availible at http://~s:~s/~n",[Server,Port]),
    io:format("Or  http://~s:~s/~n",[Address,Port]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% begin build the pages
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
%The page that shows the started tools
%----------------------------------------------------------------------
started_tools_page(State)->
    [?HEADER,?HTML_HEADER,started_tools(State),?HTML_END].

toolbar()->
    [?HEADER,?HTML_HEADER,toolbar_page(),?HTML_END].

               
start_tools_page(Env,Input,State)->
    %%io:format("~n======= ~n ~p ~n============~n",[Input]),
    case get_tools(Input) of
	{tools,Tools}->
	    %%io:format("~n======= ~n ~p ~n============~n",[Tools]),
	    NewState=handle_apps(Tools,State,start),
	    {NewState,[?HEADER,?HTML_HEADER_RELOAD,reload_started_apps(),show_unstarted_apps(NewState),?HTML_END]};
	_ ->
	    {State,[?HEADER,?HTML_HEADER,show_unstarted_apps(State),?HTML_END]}
    end.

stop_tools_page(Env,Input,State)->
    case get_tools(Input) of
	{tools,Tools}->
	    NewState=handle_apps(Tools,State,stop),
	    {NewState,[?HEADER,?HTML_HEADER_RELOAD,reload_started_apps(),show_started_apps(NewState),?HTML_END]};
	_ ->
	    {State,[?HEADER,?HTML_HEADER,show_started_apps(State),?HTML_END]}
    end.
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Functions that start and config the webserver
%% 1. Collect the data
%% 2. Config Webbserver
%% 3. Start webbserver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
%Collects the data from the config file 
%----------------------------------------------------------------------
get_data()->
    case get_tool_files_data() of
	{error,Reason}->
	    {error,Reason};
	{ok,Ets_table} ->
	    {ok,Ets_table}
    end.

%----------------------------------------------------------------------
%When we gathered the data then this function starts the 4 other tasks
%----------------------------------------------------------------------
start_webserver(Data,Path,Config_data,Pid)->
    update_conf_file(Data,Path,Config_data).

%----------------------------------------------------------------------
% Update the file that configure the webbserver 
%----------------------------------------------------------------------
update_conf_file(Data,Path,Config_data)->
    case update_conf_file2(Data,Path,Config_data) of
	{ok,Data}->
	        start_server(Path);
	Error ->
	    {error,error_server_conf_file}
    end.
%----------------------------------------------------------------------
% Start the webbserver
%----------------------------------------------------------------------
start_server(Path)->
    case httpd:start(filename:join([Path,"root/conf/httpd.conf"])) of
	{ok,Pid}->
	    {ok,Pid};
	Error->
	    {error,{server_error,Error}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The functions that collects the configuration data from          %%
%% The tools and inserts them into a ets table                      %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Control the path for *.tools files 
%----------------------------------------------------------------------
get_tool_files_data()->
    Tools=get_tools1(code:get_path()),
    %%io:format("Data : ~p ~n",[Tools]),
    get_file_content(Tools).

%----------------------------------------------------------------------
%Control that the data in the file really is erlang terms
%---------------------------------------------------------------------- 
get_file_content(Tools)->
    Get_data=fun({tool,ToolData}) ->
		     %%io:format("Data : ~p ~n",[ToolData]),
		     case httpd_util:key1search(ToolData,config_func) of
			 {M,F,A}->
			     case catch apply(M,F,A) of
				 {'EXIT',_} ->
				     bad_data;
				 Data when tuple(Data) ->
				     Data;
				 _->
				     bad_data  
			     end;
			 _ ->
				bad_data
		     end
	     end,
    insert_file_content([X ||X<-lists:map(Get_data,Tools),X/=bad_data]).

%----------------------------------------------------------------------
%Insert the data from the file in to the ets:table
%----------------------------------------------------------------------
insert_file_content(Content)->
    Table=ets:new(app_data,[bag]),
    lists:foreach(fun(X)->
			  insert_app(X,Table)
		  end,Content),
    {ok,Table}.

%----------------------------------------------------------------------
%Control that we got a a tuple of a atom and a list if so add the 
%elements in the list to the ets:table
%----------------------------------------------------------------------
insert_app({Name,Key_val_list},Table)when list(Key_val_list),atom(Name)->
    lists:foreach(fun(Key_val_pair)->
			  ets:insert(Table,{Name,Key_val_pair})
		  end,Key_val_list);

insert_app(_,_)->
    ok.
   
%----------------------------------------------------------------------
%Control that the string file really is a file name
%----------------------------------------------------------------------
%is_file(File_name)->
%    control_type(File_name,regular).

is_dir(File_name)->
    control_type(File_name,directory).
%----------------------------------------------------------------------
% control_type(Dir_or_file,Type)->true|false
% Dir_or_file a string that shall be controlled if its a plain file or a dir
%----------------------------------------------------------------------

control_type(Dir_or_file,Type)->
    case file:read_file_info(Dir_or_file) of
	{ok,Info}->
	    case Info#file_info.type of
		Type->
		    true;
		_->
		    false
	    end;
	_->
	    false
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% The functions that edit the config file for the server           %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_conf_file2(Data,Path,Config_data)->
    Alias=get_aliases(Data),
    %Erl_alias=get_erlscriptalias(Data),
    Backup_file=filename:join([Path,"root/conf/conf.backup"]),
    Conf_file=filename:join([Path,"root/conf/httpd.conf"]),
    case file:rename(Conf_file,Backup_file) of
	ok->
	    case create_file_handles(Conf_file,Backup_file) of
		{ok,{Backup_handle,Conf_handle}}->
		    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle),
		    {ok,Data};
		{error,Reason}->
		    {error,Reason}
	    end;
	Error ->
	    {error,Error}
    end.

%----------------------------------------------------------------------
%Select all the alias in the database
%----------------------------------------------------------------------
get_aliases(Data)->
    lists:map(fun({_,{_,Alias}})->
		      Alias
	      end,ets:match_object(Data,{'_',{alias,'_'}})).

%----------------------------------------------------------------------
% Create the file handles
%----------------------------------------------------------------------
create_file_handles(Conf_file,Backup_file)->
    case file:open(Conf_file,[write]) of
	{ok,Config_handle}->
	    case file:open(Backup_file,[read]) of
		{ok,Backup_handle}->
		    {ok,{Backup_handle,Config_handle}};
		{error,Reason} ->
		    {error,"Cant create backup handle"}
	    end;
	{error,Reason} ->
	    {error,"Can not create httpd.conf handle"}
    end.


%----------------------------------------------------------------------
%Copy the lines from the old config file, to the new config file
%---------------------------------------------------------------------- 
copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle)->
    case io:get_line(Backup_handle,[]) of
	eof->
	    file:close(Backup_handle),
	    file:close(Conf_handle),
	    {ok,eof};
	[$#,$S,$t,$a,$r,$t,$# |Whatever] ->
	    add_dynamic_aliases(Alias,Path,Backup_handle,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	[$S,$e,$r,$v,$e,$r,$R,$o,$o,$t|Whatever]->
	    add_server_root(Alias,Path,Backup_handle,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	[$D,$o,$c,$u,$m,$e,$n,$t,$R,$o,$o,$t|Whatever]->
	    add_document_root(Alias,Path,Backup_handle,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	[$P,$o,$r,$t|Old_data]->
	    add_configuration("Port",Old_data,Config_data,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	[$B,$i,$n,$d,$A,$d,$d,$r,$e,$s,$s|Old_data]->
	    add_configuration("BindAddress",Old_data,Config_data,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	[$S,$e,$r,$v,$e,$r,$N,$a,$m,$e|Old_data]->
	    add_configuration("ServerName",Old_data,Config_data,Conf_handle),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle);
	Line->
	    io:fwrite(Conf_handle,"~s",[Line]),
	    copy_and_add_data(Alias,Config_data,Path,Backup_handle,Conf_handle)
    end.

add_configuration(Which_data,Old_data,Config_data,Conf_handle)->
    case lists:keymember(Which_data,1,Config_data) of
	true->
	    {value,{Name,Data}}=lists:keysearch(Which_data,1,Config_data),
	    io:fwrite(Conf_handle,"~s \n",[Name ++ " " ++ Data]);
	false->
	    io:fwrite(Conf_handle,"~s  \n",[Which_data ++ Old_data])
	end.
	    
add_server_root(Alias,Path,Backup_handle,Conf_handle)->
    io:fwrite(Conf_handle,"ServerRoot ~s \n",[filename:join([Path,"root/"])]).

add_document_root(Alias,Path,Backup_handle,Conf_handle)->
    io:fwrite(Conf_handle,"DocumentRoot ~s \n",[filename:join([Path,"root/doc"])]).

add_dynamic_aliases(Alias,Path,Backup_handle,Conf_handle)->
    insert_the_data(Conf_handle,Alias),
    step_by_old_alias(Backup_handle).

    
insert_the_data(Conf_handle,Alias)->
    io:fwrite(Conf_handle,"~s",["#Start# Please do not remove this line\n"]),
    insert_alias(Alias,Conf_handle),
    io:fwrite(Conf_handle,"~s",["#Stop# Please do not remove this line \n"]).


%----------------------------------------------------------------------
%Scan through the old conf file until we find #Stop#
%----------------------------------------------------------------------
step_by_old_alias(Backup_handle)->
    case io:get_line(Backup_handle,[]) of
	eof->
	    {ok,eof};
	[$#,$S,$t,$o,$p,$# |Whatever] ->
	    {ok,stepped_by};
	Line->
	    step_by_old_alias(Backup_handle)
    end.

%----------------------------------------------------------------------
%The function that do the actual writing of aliases to the conf file.
%----------------------------------------------------------------------
    
insert_alias([],Conf_handle)->
    ok;
insert_alias([Alias|Rest],Conf_handle) ->
    io:fwrite(Conf_handle,"~s",[format_alias(Alias)]),    
    insert_alias(Rest,Conf_handle).

%----------------------------------------------------------------------
% There are two kind of aliases, to handle this we use pattern matching
% to create the corresponding alias row that we write to the file
%----------------------------------------------------------------------
format_alias({Alias,Dir})->
    "Alias "++ Alias ++ " "  ++ Dir ++ "\n";
format_alias({erl_alias,Alias,Mods}) ->
    "ErlScriptAlias " ++ Alias ++ " " ++ format_modules(Mods) ++ "\n".
%----------------------------------------------------------------------
% When we has am Erlscript alias there is a list of modules and they are
% Atom so the must be formatted to be correectly written to the file
%----------------------------------------------------------------------

format_modules([])->
    [];
format_modules([Mod|Rest]) ->
    atom_to_list(Mod) ++ " " ++format_modules(Rest).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Helper functions                                                 %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_standard_data(Port)->
    {ok,HostName}=inet:gethostname(),
    case format_ip_num(inet:getaddr(HostName,inet)) of
	error ->
	    error;
	IpNum ->
	    [{"Port",integer_to_list(Port)},
	     {"BindAddress",IpNum},
	     {"ServerName",HostName}]
    end.

format_ip_num({ok,{N1,N2,N3,N4}})->
    integer_to_list(N1)++"."++integer_to_list(N2)++"."++integer_to_list(N3)++"."++integer_to_list(N4);
format_ip_num(_)->
    error.


get_standard_data()->
    [{"Port","8888"},
     {"BindAddress","127.0.0.1"},
     {"ServerName","localhost"}].

get_path()->
    code:priv_dir(webtool).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These functionss is used to shutdown the webserver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% Shut down the webbserver 
%----------------------------------------------------------------------
shutdown_server(State)->
    httpd:stop(filename:join([State#state.priv_dir,"root/conf/httpd.conf"])).
    %httpd:stop(list_to_integer(PortNr)).

%----------------------------------------------------------------------
% Select all apps in the table and close them
%----------------------------------------------------------------------
shutdown_apps(State)->
    Data=State#state.app_data,
    lists:foreach(fun(Start_app)->
			  stop_app(Start_app,Data)
	     end,ets:match_object(Data,{'_',{start,'_'}})),
    {ok,Data}.
%----------------------------------------------------------------------
%Shuts down the supervisor that supervises tools that is not
%Designed as applications
%----------------------------------------------------------------------
shutdown_supervisor(State)->
    %io:format("~n==================~n"),
    webtool_sup:stop(State#state.supvis).
    %io:format("~n==================~n").
%----------------------------------------------------------------------
%close the individual apps.
%----------------------------------------------------------------------  
stop_app({Name,{start,{child,Real_name}}},Data)->
    ok;

stop_app({Name,{start,{app,Real_name}}},Data)->
    application:stop(Real_name);

stop_app({Name,{start,{func,Start,Stop}}},Data)->    
    case Stop of
	{M,F,A} ->
	    catch apply(M,F,A);
	_NoStop ->
	    ok
    end.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% These functions creates the webpage where the user can select if 
%% to start apps or to stop apps
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toolbar_page()->
    "<TABLE>
       <TR>
         <TD>
             <B>Select Action</B>
         </TD>
       </TR>
       <TR>
         <TD>
            <A HREF=\"./start_tools\" TARGET=right> Start Tools</A>
         </TD>
       </TR>
       <TR>
         <TD>
            <A HREF=\"./stop_tools\" TARGET=right> Stop Tools</A>
	 </TD> 
      </TR> 
    </TABLE>".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% These functions creates the webbpage that  shows the started apps
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------------
% started_tools(State)->String (html table)
% State is a record of type state
%----------------------------------------------------------------------
started_tools(State)->
    Names=get_started_apps(State#state.app_data,State#state.started),
    "<TABLE BORDER=1 WIDTH=100%>
	"++ make_rows(Names,[],0) ++"
     </TABLE>".
%----------------------------------------------------------------------
%get_started_apps(Data,Started)-> [{web_name,link}]
%selects the started apps from the ets table of apps.
%----------------------------------------------------------------------
    
get_started_apps(Data,Started)->
    SelectData=fun({App,{web_data,{Name,Link}}})->
		       {Name,Link}
	       end,
    SelectStart=fun({App,{web_data,{Name,Link}}})->
			lists:member(App,Started)
		end,
    [SelectData(X)||X<-ets:match_object(Data,{'_',{web_data,'_'}}),SelectStart(X)]++[{"WebTool","/tool_management.html"}].


%----------------------------------------------------------------------
% make_rows(List,Result,Fields)-> String (The rows of a htmltable
% List a list of tupler discibed above
% Result an accumulator for the result
% Field, counter that counts the number of cols in each row.
%----------------------------------------------------------------------
make_rows([],Result,Fields)->
    Result ++ fill_out(Fields);
make_rows([Data|Paths],Result,Field)when Field==0->
   make_rows(Paths,Result ++ "<TR>" ++ make_field(Data),Field+1);

make_rows([Path|Paths],Result,Field)when Field==4->
   make_rows(Paths,Result ++ make_field(Path) ++ "</TR>",0);

make_rows([Path|Paths],Result,Field)->
   make_rows(Paths,Result ++ make_field(Path),Field+1).

%----------------------------------------------------------------------
% make_fields(Path)-> String that is a field i a html table
% Path is a name url tuple {Name,url}
%----------------------------------------------------------------------
make_field(Path)->
    "<TD WIDTH=20%>" ++ get_name(Path) ++ "</TD>".


%----------------------------------------------------------------------
%get_name({Nae,Url})->String that represents a <A> tag in html. 
%----------------------------------------------------------------------
get_name({Name,Url})->
    "<A HREF=\"" ++ Url ++ "\" TARGET=app_frame>" ++ Name ++ "</A>".


%----------------------------------------------------------------------
% fill_out(Nr)-> String, that represent Nr fields in a html-table.
%----------------------------------------------------------------------
fill_out(Nr)when Nr==0->
    [];
fill_out(Nr)when Nr==4->
    "<TD WIDTH=\"20%\" >&nbsp</TD></TR>";

fill_out(Nr)->
    "<TD WIDTH=\"20%\">&nbsp</TD>" ++ fill_out(Nr+1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%These functions starts applicatons and builds the page showing tools
%%to start
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------------- 
%Controls whether the user selected a tool to start
%----------------------------------------------------------------------
get_tools(Input)->
    case httpd:parse_query(Input) of
	[]->
	    no_tools;
	 Tools->
	    FormatData=fun({Name,Data})->list_to_atom(Data) end,
	    SelectData=fun({Name,Data})->string:sub_string(Name,1,3)=="app"end,
	    {tools,[FormatData(X)||X<-Tools,SelectData(X)]}
    end.

%----------------------------------------------------------------------
% Selects the data to start  the applications the user has ordered 
% starting of 
%----------------------------------------------------------------------
handle_apps([],State,Cmd)->
    State;

handle_apps([Tool|Tools],State,Cmd)->
    case ets:match_object(State#state.app_data,{Tool,{start,'_'}}) of
	[]->
	    Started = case Cmd of
			  start ->
			      [Tool|State#state.started];
			  stop ->
			      
			      lists:delete(Tool,State#state.started)
		      end,
	    {ok,#state{priv_dir=State#state.priv_dir,
		       app_data=State#state.app_data,
		       supvis=State#state.supvis,
		       web_data=State#state.web_data,
		       started=Started}};
	ToStart ->
	    case handle_apps2(ToStart,State,Cmd) of
		{ok,NewState}->
		    handle_apps(Tools,NewState,Cmd);
		_->
		    handle_apps(Tools,State,Cmd)
	    end
    end.

%----------------------------------------------------------------------
%execute every start or stop data about a tool.
%----------------------------------------------------------------------
handle_apps2([{Name,Start_data}],State,Cmd)->
    case handle_app({Name,Start_data},State#state.app_data,State#state.supvis,Cmd)of
	ok->
	    Started = case Cmd of
			  start ->
			      [Name|State#state.started];
			  stop ->
			      
			      lists:delete(Name,State#state.started)
		      end,
	    {ok,#state{priv_dir=State#state.priv_dir,
		       app_data=State#state.app_data,
		       supvis=State#state.supvis,
		       web_data=State#state.web_data,
		       started=Started}};
	_->
	    error
    end;

handle_apps2([{Name,Start_data}|Rest],State,Cmd)->
    case handle_app({Name,Start_data},State#state.app_data,State#state.supvis,Cmd)of
	ok->
	    handle_apps2(Rest,State,Cmd);
	_->
	    error
    end.


%----------------------------------------------------------------------
% Handle th start and stop of availible applications  the application 
%---------------------------------------------------------------------- 

handle_app({Name,{start,{func,Start,Stop}}},Data,Pid,Cmd)->
    Action = case Cmd of
		 start ->
		     Start;
		 _ ->
		     Stop
	     end,    
    case Action of
	{M,F,A} ->
	    case catch apply(M,F,A) of
		{'EXIT',_}->
		    ets:delete(Data,Name);
		_OK->
		    ok
	    end;
	_NoStart ->
	    ok
    end;
	    

handle_app({Name,{start,{child,ChildSpec}}},Data,Pid,Cmd)->
    case Cmd of
	start ->
	    case catch supervisor:start_child(Pid,ChildSpec) of
		{ok,_}->
		    ok;
		{ok,_,_}->
		    ok;
		{error,Reason}->
		    ets:delete(Data,Name);
		_ ->
		    ets:delete(Data,Name)
	    end;
	stop ->
	    case catch supervisor:terminate_child(websup,element(1,ChildSpec)) of
		ok ->
		    supervisor:delete_child(websup,element(1,ChildSpec));
		_ ->
		    error
	    end
    end;



handle_app({Name,{start,{app,Real_name}}},Data,Pid,Cmd)->
    case Cmd of
	start ->
	    case application:start(Real_name,temporary) of
		ok->
		    io:write(Name),
		    ok;
		{error,{already_started,_}}->
		    %% Remove it from the database so we dont stop anything already started
		    ets:match_delete(Data,{Name,{start,{app,Real_name}}}),
		    ok;
		{error,Reason}->
		    ets:delete(Data,Name)
	    end;
	
	stop ->
	    application:stop(Real_name)
    end;

%----------------------------------------------------------------------
% If the data is incorrect delete the app
%----------------------------------------------------------------------
handle_app({Name,_},Data,Pid,Cmd)->
    ets:delete(Data,Name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% this functions creates the page that shows the unstarted tools   %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reload_started_apps()->
    "<script>
        function reloadCompiledList()
        {
          parent.parent.top1.document.location.href=\"/webtool/webtool/started_tools\";
        }
     </script>".

show_unstarted_apps(State)->
  "<TABLE HEIGHT=100%  WIDTH=100% BORDER=0> 
    <TR HEIGHT=80%><TD ALIGN=\"center\" VALIGN=\"middle\"> 
      <FORM NAME=\"stop_apps\" ACTION=\"/webtool/webtool/start_tools\" >
       <TABLE BORDER=1 WIDTH=60%>
	 <TR BGCOLOR=\"#8899AA\">
	   <TD ALIGN=CENTER COLSPAN=2><FONT SIZE=4>Availible Tools<FONT></TD>
	 </TR>
 	<TR>
	   <TD WIDTH=50%>
	       <TABLE BORDER=0>
	           "++ list_availible_apps(State)++"
                   <TR><TD COLSPAN=2>&nbsp;</TD></TR>
                   <TR>
                      <TD COLSPAN=2 ALIGN=\"center\">
                         <INPUT TYPE=submit VALUE=\"Start\">
                      </TD>
                   </TR>
                </TABLE>
            </TD>
           <TD>   
             To Start a Tool:
             <UL>
             <LI>Select the
             checkbox for each tool to
             start.</LI>
             <LI>Click on the 
             button marked <EM>Start</EM>.</LI></UL>
            </TD>         
         </TR>
      </TABLE> 
    </FORM>
   </TD></TR>
   <TR><TD>&nbsp;</TD></TR>
   </TABLE>".



list_availible_apps(State)->
    Unstarted_apps=lists:filter(fun({Tool,Web_data})->
					  false==lists:member(Tool,State#state.started)
				  end,ets:match_object(State#state.app_data,{'_',{web_data,'_'}})),
    case Unstarted_apps of
	[]->
	    "<TR><TD>All tools are started</TD></TR>";
	_->
	    lists:flatten(
	      lists:map(fun({Tool,{web_data,{Name,Link}}})->
				"<TR><TD>
                                    <INPUT TYPE=\"checkbox\" NAME=\"app\" VALUE=\""  ++ atom_to_list(Tool) ++"\">
                                    "++Name++"     
                                 </TR></TD>"  
			end,Unstarted_apps))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% these functions creates the page that shows the started apps     %%
%% the user can select to shutdown                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_started_apps(State)->
  "<TABLE HEIGHT=100%  WIDTH=100% BORDER=0> 
    <TR HEIGHT=80%><TD ALIGN=\"center\" VALIGN=\"middle\"> 
      <FORM NAME=\"stop_apps\" ACTION=\"/webtool/webtool/stop_tools\" >
       <TABLE BORDER=1 WIDTH=60%>
	 <TR BGCOLOR=\"#8899AA\">
	   <TD ALIGN=CENTER COLSPAN=2><FONT SIZE=4>Started Tools<FONT></TD>
	 </TR>
 	<TR>
	   <TD WIDTH=50%>
	       <TABLE BORDER=0>
	           "++ list_started_apps(State)++"
                   <TR><TD COLSPAN=2>&nbsp;</TD></TR>
                   <TR>
                      <TD COLSPAN=2 ALIGN=\"center\">
                         <INPUT TYPE=submit VALUE=\"Stop\">
                      </TD>
                   </TR>
                </TABLE>
            </TD>
           <TD>   
             Stop a Tool:
             <UL>
             <LI>Select the
             checkbox for each tool to
             stop.</LI>
             <LI>Click on the 
             button marked <EM>Stop</EM>.</LI></UL>
            </TD>         
         </TR>
      </TABLE> 
    </FORM>
   </TD></TR>
   <TR><TD>&nbsp;</TD></TR>
   </TABLE>".

list_started_apps(State)->
    Started_apps=lists:filter(fun({Tool,Web_data})->
					  true==lists:member(Tool,State#state.started)
				  end,ets:match_object(State#state.app_data,{'_',{web_data,'_'}})),
    case Started_apps of
	[]->
	    "<TR><TD>No tool is started yet.</TD></TR>";
	_->
	    lists:flatten(
	      lists:map(fun({Tool,{web_data,{Name,Link}}})->
				"<TR><TD>
                                    <INPUT TYPE=\"checkbox\" NAME=\"app\" VALUE=\""  ++ atom_to_list(Tool) ++"\">
                                    "++Name++"     
                                 </TD></TR>"  
			end,Started_apps))
    end.

			 
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                   %%
%% Collecting the data from the  *.tool files                        %%
%%                                                                   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------------------
% get_tools(Dirs) => [{M,F,A},{M,F,A}...{M,F,A}]
%   Dirs - [string()] Directory names
% Calls get_tools2/2 recursively for a number of directories
% to retireve the configuration data for the web based tools.
%----------------------------------------
get_tools1(Dirs)->
    get_tools1(Dirs,[]).

get_tools1([Dir|Rest],Data) when list(Dir) ->
    Tools=case filename:basename(Dir) of
	      %% Dir is an 'ebin' directory, check in '../priv' as well
	      "ebin" ->
		  get_tools2(filename:join(filename:dirname(Dir),"priv"))++
		  get_tools2(Dir);
	      _ ->
		  get_tools2(Dir)
	  end,
    get_tools1(Rest,[Tools|Data]);

get_tools1([],Data) ->
  lists:flatten(Data).

%----------------------------------------
% get_tools2(Directory) => DataList
%   DataList : [WebTuple]|[]
%   WebTuple: {tool,[{web,M,F,A}]}
%
%----------------------------------------
get_tools2(Dir)->
    get_tools2(tool_files(Dir),[]).

get_tools2([ToolFile|Rest],Data) ->
    case get_tools3(ToolFile) of
	{tool,WebData} ->
	    get_tools2(Rest,[{tool,WebData}|Data]);
	{error,Reason} ->
	    get_tools2(Rest,Data);
	nodata ->
	    get_tools2(Rest,Data)
    end;

get_tools2([],Data) ->
    Data.

%----------------------------------------
% get_tools3(ToolFile) => {ok,Tool}|{error,Reason}|nodata 
%   Tool: {tool,[KeyValTuple]}
%   ToolFile - string() A .tool file
%   Now we have the file get the data and sort it out
%----------------------------------------
get_tools3(ToolFile) ->
    case file:consult(ToolFile) of
	{error,open} ->
	    {error,nofile};
	{error,read} ->
	    {error,format};
	{ok,[{version,Vsn},InfoTuple]} when tuple(InfoTuple)->
	    {error,old_version};
	{ok,[{version,"1.2"},ToolInfo]} when list(ToolInfo)->
	    webdata(ToolInfo);
	{ok,_Other} ->
	    {error,format}
    end.


%----------------------------------------------------------------------
% webdata(TupleList)-> ToolTuple| nodata
% ToolTuple: {tool,[{config_func,{M,F,A}}]}
%
% There are a little unneccesary work in this format but it is extendable
%----------------------------------------------------------------------
webdata(TupleList)-> 
    case httpd_util:key1search(TupleList,config_func,nodata) of
	{M,F,A} ->
	    {tool,[{config_func,{M,F,A}}]};
	_ ->
	   nodata
    end.


%=============================================================================
% Functions for getting *.tool configuration files
%=============================================================================

%----------------------------------------
% tool_files(Dir) => ToolFiles
%   Dir - string() Directory name
%   ToolFiles - [string()]
% Return the list of all files in Dir ending with .tool (appended to Dir)
%----------------------------------------
tool_files(Dir) ->
    case file:list_dir(Dir) of
	{ok,Files} ->
	    filter_tool_files(Dir,Files);
	{error,_Reason} ->
	    []
    end.

%----------------------------------------
% filter_tool_files(Dir,Files) => ToolFiles
%   Dir - string() Directory name
%   Files, ToolFiles - [string()] File names
% Filters out the files in Files ending with .tool and append them to Dir
%----------------------------------------
filter_tool_files(_Dir,[]) ->
    [];
filter_tool_files(Dir,[File|Rest]) ->
    case filename:extension(File) of
	".tool" ->
	    [filename:join(Dir,File)|filter_tool_files(Dir,Rest)];
	_ ->
	    filter_tool_files(Dir,Rest)
    end.













