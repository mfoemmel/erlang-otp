%%<copyright>
%% <year>2003-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% @doc Common Test Framework Utilities.
%%%
%%% <p>This is a support module for the Common Test Framework. It
%%% implements the process ct_util_server which acts like a data
%%% holder for suite, configuration and connection data.</p>
%%%
-module(ct_util).

-export([start/0,start/1,start/2,stop/1,update_last_run_index/0]).

-export([register_connection/4,unregister_connection/1,
	 does_connection_exist/3,get_key_from_name/1]).

-export([require/1,require/2,get_config/1,get_config/2,get_all_config/0,
	 set_default_config/3, delete_default_config/1,
	 update_config/2, release_allocated/0, close_connections/0]).

-export([save_suite_data/3, save_suite_data/2, read_suite_data/1, 
	 delete_suite_data/0, delete_suite_data/1, match_delete_suite_data/1,
	 delete_testdata/0, delete_testdata/1, set_testdata/1, get_testdata/1]).

-export([override_silence_all_connections/0, override_silence_connections/1, 
	 get_overridden_silenced_connections/0, 
	 delete_overridden_silenced_connections/0, 
	 silence_all_connections/0, silence_connections/1, is_silenced/1, 
	 reset_silent_connections/0]).

-export([set_cwd/1, reset_cwd/0]).

-export([parse_table/1]).

-export([listenv/1]).

-export([get_target_name/1, get_connections/2]).

-export([is_test_dir/1, get_testdir/1]).

-include("ct_event.hrl").
-include("ct_util.hrl").

-record(ct_conf,{key,value,ref,name='_UNDEF',default=false}).  
%% default = {true,suite} | {true,testcase} | false

-record(suite_data, {key,name,value}).

%%%-----------------------------------------------------------------
%%% @spec start(Mode) -> Pid | exit(Error)
%%%       Mode = normal | interactive
%%%       Pid = pid()
%%%
%%% @doc Start start the ct_util_server process 
%%% (tool-internal use only).
%%%
%%% <p>This function is called from ct_run.erl. It starts and initiates
%%% the <code>ct_util_server</code></p>
%%% 
%%% <p>Returns the process identity of the
%%% <code>ct_util_server</code>.</p>
%%%
%%% @see ct
start() ->
    start(normal,".").

start(LogDir) when is_list(LogDir) ->
    start(normal,LogDir);
start(Mode) ->
    start(Mode,".").

start(Mode,LogDir) ->
    case whereis(ct_util_server) of
	undefined ->
	    S = self(),
	    Pid = spawn_link(fun() -> do_start(S,Mode,LogDir) end),
	    receive 
		{Pid,started} -> Pid;
		{Pid,Error} -> exit(Error)
	    end;
	Pid ->
	    case get_mode() of
		interactive when Mode==interactive ->
		    Pid;
		interactive ->
		    {error,interactive_mode};
		_OtherMode ->
		    Pid
	    end
    end.

do_start(Parent,Mode,LogDir) ->
    process_flag(trap_exit,true),
    register(ct_util_server,self()),
    create_table(?attr_table,bag,#ct_conf.key),
    create_table(?conn_table,#conn.handle),
    create_table(?board_table,2),
    create_table(?suite_table,#suite_data.key),
    {ok,StartDir} = file:get_cwd(),
    case file:set_cwd(LogDir) of
	ok -> ok;
	E -> exit(E)
    end,
    Opts = case read_opts() of
	       {ok,Opts1} ->
		   Opts1;
	       Error ->
		   Parent ! {self(),Error},
		   exit(Error)
	   end,

    %% start an event manager (if not already started by master)
    case ct_event:start_link() of
	{error,{already_started,_}} ->
	    ok;
	_ ->
	    case whereis(vts) of
		undefined ->
		    ct_event:add_handler();
		VtsPid ->
		    ct_event:add_handler([{vts,VtsPid}])
	    end
    end,
    case read_config_files(Opts) of
	ok ->
	    %% add user handlers
	    case lists:keysearch(event_handler,1,Opts) of
		{value,{_,Handlers}} ->
		    Add = fun({H,Args}) ->
				  case catch gen_event:add_handler(?CT_EVMGR_REF,H,Args) of
				      ok -> ok;
				      {'EXIT',Why} -> exit(Why);
				      Other -> exit({event_handler,Other})
				  end
			  end,
		    case catch lists:foreach(Add,Handlers) of
			{'EXIT',Reason} ->
			    Parent ! {self(),Reason};
			_ ->
			    ok
		    end;
		false ->
		    ok
	    end,
	    {StartTime,TestLogDir} = ct_logs:init(Mode),
	    ct_event:notify(#event{name=test_start,
				   node=node(),
				   data={StartTime,
					 lists:flatten(TestLogDir)}}),
	    Parent ! {self(),started},
	    loop(Mode,[],StartDir);
	ReadError ->
	    Parent ! {self(),ReadError},
	    exit(ReadError)
    end.

create_table(TableName,KeyPos) ->
    create_table(TableName,set,KeyPos).
create_table(TableName,Type,KeyPos) ->
    catch ets:delete(TableName),
    ets:new(TableName,[Type,named_table,public,{keypos,KeyPos}]).

read_opts() ->
    case file:consult(ct_run:variables_file_name("./")) of
	{ok,Opts} ->
	    {ok,Opts};
	{error,enoent} ->
	    {error,not_installed};
	Error ->
	    {error,{bad_installation,Error}}
    end.

read_config_files(Opts) ->
    ConfigFiles =
	lists:foldl(fun({config,Files},Acc) ->
			    Acc ++ Files;
		       (_,Acc) ->
			    Acc
		    end,[],Opts),
    read_config_files1(ConfigFiles).

read_config_files1([ConfigFile|Files]) ->
    case file:consult(ConfigFile) of
	{ok,Config} -> 
	    set_config(Config),
	    read_config_files1(Files);
	{error,Reason} ->
	    {user_error,{config_file_error,ConfigFile,Reason}}
    end;
read_config_files1([]) ->
    ok.

set_default_config(Name, NewConfig, Scope) ->
    call({set_default_config, {Name, NewConfig, Scope}}).

delete_default_config(Scope) ->
    call({delete_default_config, Scope}).

update_config(Name, Config) ->
    call({update_config, {Name, Config}}).

save_suite_data(Key, Value) ->
    call({save_suite_data, {Key, undefined, Value}}).

save_suite_data(Key, Name, Value) ->
    call({save_suite_data, {Key, Name, Value}}).

read_suite_data(Key) ->
    call({read_suite_data, Key}).

delete_suite_data() ->
    call({delete_suite_data, all}).

delete_suite_data(Key) ->
    call({delete_suite_data, Key}).

match_delete_suite_data(KeyPat) ->
    call({match_delete_suite_data, KeyPat}).

delete_testdata() ->
    call(delete_testdata).

delete_testdata(Key) ->
    call({delete_testdata, Key}).

set_testdata(TestData) ->
    call({set_testdata, TestData}).

get_testdata(Key) ->
    call({get_testdata, Key}).

set_cwd(Dir) ->
    call({set_cwd,Dir}).

reset_cwd() ->
    call(reset_cwd).

loop(Mode,TestData,StartDir) ->
    receive 
	{update_last_run_index,From} ->
	    ct_logs:make_last_run_index(),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{require,Name,Tag,SubTags},From} ->
	    Result = do_require(Name,Tag,SubTags),
	    return(From,Result),
	    loop(Mode,TestData,StartDir);
	{{set_default_config,{Name,Config,Scope}},From} ->
	    set_config(Name,Config,{true,Scope}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{delete_default_config,Scope},From} ->
	    delete_config({true,Scope}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{update_config,{Name,NewConfig}},From} ->
	    update_conf(Name,NewConfig),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{save_suite_data,{Key,Name,Value}},From} ->
	    ets:insert(?suite_table, #suite_data{key=Key,
						 name=Name,
						 value=Value}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{read_suite_data,Key},From} ->
	    case ets:lookup(?suite_table, Key) of
		[#suite_data{key=Key,name=undefined,value=Value}] ->
		    return(From,Value);
		[#suite_data{key=Key,name=Name,value=Value}] ->
		    return(From,{Name,Value});
		_ ->
		    return(From,undefined)
	    end,
	    loop(Mode,TestData,StartDir);
	{{delete_suite_data,Key},From} ->
	    if Key == all ->
		    ets:delete_all_objects(?suite_table);
	       true ->
		    ets:delete(?suite_table, Key)
	    end,
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{{match_delete_suite_data,KeyPat},From} ->
	    ets:match_delete(?suite_table, #suite_data{key=KeyPat,
						       name='_',
						       value='_'}),
	    return(From,ok),
	    loop(Mode,TestData,StartDir);
	{delete_testdata,From} ->
	    return(From,ok),
	    loop(From,[],StartDir);	
	{{delete_testdata,Key},From} ->
	    TestData1 = lists:keydelete(Key,1,TestData),
	    return(From,ok),
	    loop(From,TestData1,StartDir);	
	{{set_testdata,New = {Key,_Val}},From} ->
	    TestData1 = lists:keydelete(Key,1,TestData),
	    return(From,ok),
	    loop(Mode,[New|TestData1],StartDir);
	{{get_testdata,Key},From} ->
	    case lists:keysearch(Key,1,TestData) of
		{value,{Key,Val}} ->
		    return(From,Val);
		_ ->
		    return(From,undefined)
	    end,
	    loop(From,TestData,StartDir);
	{{set_cwd,Dir},From} ->
	    return(From,file:set_cwd(Dir)),
	    loop(From,TestData,StartDir);
	{reset_cwd,From} ->
	    return(From,file:set_cwd(StartDir)),
	    loop(From,TestData,StartDir);
	{{stop,How},From} ->
	    ets:delete(?attr_table),
	    close_connections(ets:tab2list(?conn_table)),
	    ets:delete(?conn_table),
	    ets:delete(?board_table),
	    ets:delete(?suite_table),
	    ct_logs:close(How),
	    file:set_cwd(StartDir),
	    ct_event:stop(),
	    return(From,ok);
	{get_mode,From} ->
	    return(From,Mode),
	    loop(Mode,TestData,StartDir);
	{'EXIT',_Pid,normal} ->
	    loop(Mode,TestData,StartDir);
	{'EXIT',Pid,Reason} ->
	    %% Let process crash in case of error, this shouldn't happen!
	    io:format("\n\nct_util_server got EXIT from ~p: ~p\n\n",
		      [Pid,Reason]),
	    file:set_cwd(StartDir),
	    exit(Reason)
    end.


close_connections([#conn{handle=Handle,callback=CB}|Conns]) ->
    CB:close(Handle),
    close_connections(Conns);
close_connections([]) ->
    ok.


%%%-----------------------------------------------------------------
%%% @spec register_connection(TargetName,Address,Callback,Handle) -> 
%%%                                              ok | {error,Reason}
%%%      TargetName = ct:target_name()
%%%      Address = term()
%%%      Callback = atom()
%%%      Handle = term
%%%
%%% @doc Register a new connection (tool-internal use only).
%%%
%%% <p>This function can be called when a new connection is
%%% established. The connection data is stored in the connection
%%% table, and ct_util will close all registered connections when the
%%% test is finished by calling <code>Callback:close/1</code>.</p>
register_connection(TargetName,Address,Callback,Handle) ->
    {ok,TargetRef} = get_ref_from_name(TargetName),
    ets:insert(?conn_table,#conn{handle=Handle,
				 targetref=TargetRef,
				 address=Address,
				 callback=Callback}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec unregister_connection(Handle) -> ok
%%%      Handle = term
%%%
%%% @doc Unregister a connection (tool-internal use only).
%%%
%%% <p>This function should be called when a registered connection is
%%% closed. It removes the connection data from the connection
%%% table.</p>
unregister_connection(Handle) ->
    ets:delete(?conn_table,Handle),
    ok.


%%%-----------------------------------------------------------------
%%% @spec does_connection_exist(TargetName,Address,Callback) -> 
%%%                                              {ok,Handle} | false
%%%      TargetName = ct:target_name()
%%%      Address = address
%%%      Callback = atom()
%%%      Handle = term()
%%%
%%% @doc Check if a connection already exists.
does_connection_exist(TargetName,Address,Callback) ->
    {ok,TargetRef} = get_ref_from_name(TargetName),
    case ets:select(?conn_table,[{#conn{handle='$1',
					targetref=TargetRef,
					address=Address,
					callback=Callback},
				  [],
				  ['$1']}]) of
	[Handle] ->
	    {ok,Handle};
	[] ->
	    false
    end.

%%%-----------------------------------------------------------------
%%% @spec get_connections(TargetName,Callback) -> 
%%%                                {ok,Connections} | {error,Reason}
%%%      TargetName = ct:target_name()
%%%      Callback = atom()
%%%      Connections = [Connection]
%%%      Connection = {Handle,Address}
%%%      Handle = term()
%%%      Address = term()
%%%
%%% @doc Return all connections for the <code>Callback</code> on the
%%% given target (<code>TargetName</code>).
get_connections(TargetName,Callback) ->
    case get_ref_from_name(TargetName) of
	{ok,Ref} ->
	    {ok,ets:select(?conn_table,[{#conn{handle='$1',
					       address='$2',
					       targetref=Ref,
					       callback=Callback},
					 [],
					 [{{'$1','$2'}}]}])};
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_target_name/1
get_target_name(ConnPid) ->
    case ets:select(?conn_table,[{#conn{handle=ConnPid,targetref='$1',_='_'},
				  [],
				  ['$1']}]) of
	[TargetRef] ->
	    get_name_from_ref(TargetRef);
	[] ->
	    {error,{unknown_connection,ConnPid}}
    end.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:require/1
require(Key) when is_atom(Key) ->
    require({Key,[]});
require({Key,SubKeys}) when is_atom(Key) ->
    allocate('_UNDEF',Key,to_list(SubKeys));
require(Key) ->
    {error,{invalid,Key}}.


%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:require/2
require(Name,Key) when is_atom(Key) ->
    require(Name,{Key,[]});
require(Name,{Key,SubKeys}) when is_atom(Name), is_atom(Key) ->
    call({require,Name,Key,to_list(SubKeys)});
require(Name,Keys) ->
    {error,{invalid,{Name,Keys}}}.

to_list(X) when is_list(X) -> X;
to_list(X) -> [X].

do_require(Name,Key,SubKeys) when is_list(SubKeys) ->
    case get_key_from_name(Name) of
	{error,_} ->
	    allocate(Name,Key,SubKeys);		    
	{ok,Key} ->
	    %% Already allocated - check that it has all required subkeys
	    case get_subconfig(SubKeys,lookup_name(Name)) of
		{ok,_SubMapped} ->
		    ok;
		Error ->
		    Error
	    end;
	{ok,OtherKey} ->
	    {error,{name_in_use,Name,OtherKey}}
    end.

allocate(Name,Key,SubKeys) ->
    case ets:match_object(?attr_table,#ct_conf{key=Key,name='_UNDEF',_='_'}) of
	[] ->
	    {error,{not_available,Key}};
	Available ->
	    case allocate_subconfig(Name,SubKeys,Available) of
		ok ->
		    ok;
		Error ->
		    Error
	    end
    end.

allocate_subconfig(Name,SubKeys,[C=#ct_conf{value=Value}|Rest]) ->
    case do_get_config(SubKeys,Value,[]) of
	{ok,_SubMapped} ->
	    ets:delete_object(?attr_table,C),
	    ets:insert(?attr_table,C#ct_conf{name=Name}),
	    ok;
	_Error ->
	    allocate_subconfig(Name,SubKeys,Rest)
    end;
allocate_subconfig(_Name,SubKeys,[]) ->
    {error,{not_available,SubKeys}}.



%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_config/2
get_config(What,Default) ->
    case get_config(What) of
	undefined -> Default;
	Value -> Value
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:get_config/1
get_config(KeyOrName) when is_atom(KeyOrName) ->
    case lookup_config(KeyOrName) of
	[] -> undefined;
	[Value|_] -> Value
    end;
get_config({KeyOrName,SubKey}) ->
    case lookup_config(KeyOrName) of
	[] ->
	    undefined;
	Values ->
	    case get_subconfig([SubKey],Values) of
		{ok,[{_SubKey,SubVal}]} ->
		    SubVal;
		_Error ->
		    undefined
	    end
    end.

get_subconfig(SubKeys,[Value|Rest]) ->
    case do_get_config(SubKeys,Value,[]) of
	{ok,SubMapped} ->
	    {ok,SubMapped};
	_Error ->
	    get_subconfig(SubKeys,Rest)
    end;
get_subconfig(SubKeys,[]) ->
    {error,{not_available,SubKeys}}.


do_get_config([Key|Required],Available,Mapped) ->
    case lists:keysearch(Key,1,Available) of
	{value,{Key,Value}} ->
	    NewAvailable = lists:keydelete(Key,1,Available),
	    NewMapped = [{Key,Value}|Mapped],
	    do_get_config(Required,NewAvailable,NewMapped);
	false ->
	    {error,{not_available,Key}}
    end;
do_get_config([],_Available,Mapped) ->
    {ok,lists:reverse(Mapped)}.

get_all_config() ->
    ets:select(?attr_table,[{#ct_conf{name='$1',key='$2',value='$3',
				      default='$4',_='_'},
			     [],
			     [{{'$1','$2','$3','$4'}}]}]).

lookup_config(KeyOrName) ->
    case lookup_name(KeyOrName) of
	[] ->
	    lookup_key(KeyOrName);
	Values ->
	    Values
    end.

lookup_name(Name) ->
    ets:select(?attr_table,[{#ct_conf{value='$1',name=Name,_='_'},[],['$1']}]).
lookup_key(Key) ->
    ets:select(?attr_table,[{#ct_conf{key=Key,value='$1',name='_UNDEF',_='_'},
			     [],
			     ['$1']}]).

set_config(Config) ->
    set_config('_UNDEF', Config, false).

set_config(Name,Config,Default) ->
    [ets:insert(?attr_table,
		#ct_conf{key=Key,value=Val,ref=make_ref(),
			 name=Name,default=Default}) ||
	{Key,Val} <- Config].

delete_config(Default) ->
    ets:match_delete(?attr_table,#ct_conf{default=Default,_='_'}),
    ok.


%%%-----------------------------------------------------------------
%%% @spec release_allocated() -> ok
%%%
%%% @doc Release all allocated resources, but don't take down any
%%% connections.
release_allocated() ->
    Allocated = ets:select(?attr_table,[{#ct_conf{name='$1',_='_'},
					 [{'=/=','$1','_UNDEF'}],
					 ['$_']}]),
    release_allocated(Allocated).
release_allocated([H|T]) ->
    ets:delete_object(?attr_table,H),
    ets:insert(?attr_table,H#ct_conf{name='_UNDEF'}),
    release_allocated(T);
release_allocated([]) ->
    ok.

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc 
update_conf(Name, NewConfig) ->
    Old = ets:select(?attr_table,[{#ct_conf{name=Name,_='_'},[],['$_']}]),
    lists:foreach(fun(OldElem) ->
			  NewElem = OldElem#ct_conf{value=NewConfig},
			  ets:delete_object(?attr_table, OldElem),
			  ets:insert(?attr_table, NewElem)
		  end, Old),
    ok.
				       
%%%-----------------------------------------------------------------
%%% @spec close_connections() -> ok
%%%
%%% @doc Close all open connections.
close_connections() ->
    close_connections(ets:tab2list(?conn_table)),
    ok.

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc 
override_silence_all_connections() ->
    Protocols = [telnet,ftp,rpc,snmp],
    override_silence_connections(Protocols),
    Protocols.

override_silence_connections(Conns) when is_list(Conns) ->
    Conns1 = lists:map(fun({C,B}) -> {C,B};
			  (C)     -> {C,true}
		       end, Conns),
    set_testdata({override_silent_connections,Conns1}).

get_overridden_silenced_connections() ->
    case get_testdata(override_silent_connections) of
	{error,_} ->
	    undefined;
	Conns ->      % list() or undefined
	    Conns
    end.

delete_overridden_silenced_connections() ->    
    delete_testdata(override_silent_connections).

silence_all_connections() ->
    Protocols = [telnet,ftp,rpc,snmp],
    silence_connections(Protocols),
    Protocols.

silence_connections(Conn) when is_tuple(Conn) ->
    silence_connections([Conn]);
silence_connections(Conn) when is_atom(Conn) ->
    silence_connections([{Conn,true}]);
silence_connections(Conns) when is_list(Conns) ->
    Conns1 = lists:map(fun({C,B}) -> {C,B};
			  (C)     -> {C,true}
		       end, Conns),
    set_testdata({silent_connections,Conns1}).

is_silenced(Conn) ->
    case get_testdata(silent_connections) of
	Conns when is_list(Conns) ->
	    case lists:keysearch(Conn,1,Conns) of
		{value,{Conn,true}} ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.
    
reset_silent_connections() ->
    delete_testdata(silent_connections).
    

%%%-----------------------------------------------------------------
%%% @spec stop(How) -> ok
%%%
%%% @doc Stop the ct_util_server and close all existing connections
%%% (tool-internal use only).
%%%
%%% @see ct
stop(How) ->
    case whereis(ct_util_server) of
	undefined -> ok;
	_ -> call({stop,How})
    end.

%%%-----------------------------------------------------------------
%%% @spec update_last_run_index() -> ok
%%%
%%% @doc Update <code>ct_run.&lt;timestamp&gt;/index.html</code> 
%%% (tool-internal use only).
update_last_run_index() ->
    call(update_last_run_index).


%%%-----------------------------------------------------------------
%%% @spec get_mode() -> Mode
%%%   Mode = normal | interactive
%%%
%%% @doc Return the current mode of the ct_util_server
%%% (tool-internal use only).
get_mode() ->
    call(get_mode).

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:listenv/1
listenv(Telnet) ->
    case ct_telnet:send(Telnet,"listenv") of
	ok ->
	    {ok,Data,_} = ct_telnet:expect(Telnet,
					   ["(^.+)=(.*$)"],
					   [{timeout,seconds(3)},
					    repeat]),
	    {ok,[{Name,Val} || [_,Name,Val] <- Data]};
	{error,Reason} ->
	    {error,{could_not_send_command,Telnet,"listenv",Reason}}
    end.

%%%-----------------------------------------------------------------
%%% @hidden
%%% @equiv ct:parse_table/1
parse_table(Data) ->
    [Heading|Lines]=
	[remove_space(string:tokens(L, "|"),[]) || L <- Data, hd(L)==$|],
    {Heading,Lines}.

remove_space([Str|Rest],Acc) ->
    remove_space(Rest,[string:strip(string:strip(Str),both,$')|Acc]);
remove_space([],Acc) ->
    list_to_tuple(lists:reverse(Acc)).


%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
is_test_dir(Dir) ->
    lists:last(string:tokens(filename:basename(Dir), "_")) == "test".

%%%-----------------------------------------------------------------
%%% @spec 
%%%
%%% @doc
get_testdir(Dir) ->
    Abs = abs_name(Dir),
    case is_test_dir(filename:basename(Abs)) of
	true ->
	    Abs;
	false ->
	    filename:join(Abs,"test")
    end.


%%%-----------------------------------------------------------------
%%% Internal functions
call(Msg) ->
    MRef = erlang:monitor(process,whereis(ct_util_server)),
    Ref = make_ref(),
    ct_util_server ! {Msg,{self(),Ref}},
    receive
	{Ref, Result} -> 
	    erlang:demonitor(MRef),
	    Result;
	{'DOWN',MRef,process,_,Reason}  -> 
	    {error,{ct_util_server_down,Reason}}
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.

seconds(T) ->
    test_server:seconds(T).

get_ref_from_name(Name) ->
    case ets:select(?attr_table,[{#ct_conf{name=Name,ref='$1',_='_'},
				  [],
				  ['$1']}]) of
	[Ref] ->
	    {ok,Ref};
	_ ->
	    {error,{no_such_name,Name}}
    end.
    
get_name_from_ref(Ref) ->
    case ets:select(?attr_table,[{#ct_conf{name='$1',ref=Ref,_='_'},
				  [],
				  ['$1']}]) of
	[Name] ->
	    {ok,Name};
	_ ->
	    {error,{no_such_ref,Ref}}
    end.
    
get_key_from_name(Name) ->
    case ets:select(?attr_table,[{#ct_conf{name=Name,key='$1',_='_'},
				  [],
				  ['$1']}]) of
	[Key] ->
	    {ok,Key};
	_ ->
	    {error,{no_such_name,Name}}
    end.
 
   
abs_name(Dir0) ->
    Abs = filename:absname(Dir0),
    Dir = case lists:reverse(Abs) of
	      [$/|Rest] -> lists:reverse(Rest);
	      _ -> Abs
	  end,
    abs_name1(Dir,[]).

abs_name1([Drv,$:,$/],Acc) ->
    Split = [[Drv,$:,$/]|Acc],
    abs_name2(Split,[]);
abs_name1("/",Acc) ->
    Split = ["/"|Acc],
    abs_name2(Split,[]);
abs_name1(Dir,Acc) ->
    abs_name1(filename:dirname(Dir),[filename:basename(Dir)|Acc]).

abs_name2([".."|T],[_|Acc]) ->
    abs_name2(T,Acc);
abs_name2(["."|T],Acc) ->
    abs_name2(T,Acc);
abs_name2([H|T],Acc) ->
    abs_name2(T,[H|Acc]);
abs_name2([],Acc) ->
    filename:join(lists:reverse(Acc)).
