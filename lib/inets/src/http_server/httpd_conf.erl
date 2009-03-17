%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
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
%% %CopyrightEnd%
%%
%%
-module(httpd_conf).

%% EWSAPI 
-export([is_directory/1, is_file/1, make_integer/1, clean/1, 
	 custom_clean/3, check_enum/2]).

%% Application internal API
-export([load/1, load/2, load_mime_types/1, store/1, store/2,
	remove/1, remove_all/1, config/1, get_config/2, get_config/3]).

-define(VMODULE,"CONF").
-include("httpd.hrl").

%%%=========================================================================
%%%  EWSAPI
%%%=========================================================================
%%-------------------------------------------------------------------------
%%  is_directory(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,Directory} | {error,Reason}
%%      Directory = string()
%%      Reason = string() | enoent | eaccess | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a directory in which case it is
%% returned. 
%%-------------------------------------------------------------------------
is_directory(Directory) ->
    case file:read_file_info(Directory) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_directory(Type,Access,FileInfo,Directory);
	{error,Reason} ->
	    {error,Reason}
    end.
is_directory(directory,read,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(directory,read_write,_FileInfo,Directory) ->
    {ok,Directory};
is_directory(_Type,_Access,FileInfo,_Directory) ->
    {error,FileInfo}.
%%-------------------------------------------------------------------------
%% is_file(FilePath) -> Result
%%	FilePath = string()
%%      Result = {ok,File} | {error,Reason}
%%      File = string()
%%      Reason = string() | enoent | eaccess | enotdir | FileInfo
%%      FileInfo = File info record
%%
%% Description: Checks if FilePath is a regular file in which case it
%% is returned.
%%-------------------------------------------------------------------------
is_file(File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    #file_info{type = Type, access = Access} = FileInfo,
	    is_file(Type,Access,FileInfo,File);
	{error,Reason} ->
	    {error,Reason}
    end.
is_file(regular,read,_FileInfo,File) ->
    {ok,File};
is_file(regular,read_write,_FileInfo,File) ->
    {ok,File};
is_file(_Type,_Access,FileInfo,_File) ->
    {error,FileInfo}.
%%-------------------------------------------------------------------------
%% make_integer(String) -> Result
%% String = string()
%% Result = {ok,integer()} | {error,nomatch}
%%
%% Description: make_integer/1 returns an integer representation of String. 
%%-------------------------------------------------------------------------
make_integer(String) ->
    case regexp:match(clean(String),"[0-9]+") of
	{match, _, _} ->
	    {ok, list_to_integer(clean(String))};
	nomatch ->
	    {error, nomatch}
    end.
%%-------------------------------------------------------------------------
%% clean(String) -> Stripped
%% String = Stripped = string()
%%
%% Description:clean/1 removes leading and/or trailing white spaces
%% from String.
%%-------------------------------------------------------------------------
clean(String) ->
    {ok,CleanedString,_} = 
	regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.
%%-------------------------------------------------------------------------
%% custom_clean(String,Before,After) -> Stripped
%% Before = After = regexp()
%% String = Stripped = string()
%%
%% Description: custom_clean/3 removes leading and/or trailing white
%% spaces and custom characters from String. 
%%-------------------------------------------------------------------------
custom_clean(String,MoreBefore,MoreAfter) ->
    {ok,CleanedString,_} = regexp:gsub(String,"^[ \t\n\r\f"++MoreBefore++
				       "]*|[ \t\n\r\f"++MoreAfter++"]*\$",""),
    CleanedString.
%%-------------------------------------------------------------------------
%% check_enum(EnumString,ValidEnumStrings) -> Result
%%	EnumString = string()
%%      ValidEnumStrings = [string()]
%%      Result = {ok,atom()} | {error,not_valid}
%%
%% Description: check_enum/2 checks if EnumString is a valid
%% enumeration of ValidEnumStrings in which case it is returned as an
%% atom.
%%-------------------------------------------------------------------------
check_enum(_Enum,[]) ->
    {error, not_valid};
check_enum(Enum,[Enum|_Rest]) ->
    {ok, list_to_atom(Enum)};
check_enum(Enum, [_NotValid|Rest]) ->
    check_enum(Enum, Rest).

get_config(Address, Port) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port),
    Properties =  ets:tab2list(Tab),
    MimeTab = proplists:get_value(mime_types, Properties),
    NewProperties = proplists:delete(mime_types, Properties),
    [{mime_types, ets:tab2list(MimeTab)} | NewProperties].
     
get_config(Address, Port, Properties) ->    
    Tab = httpd_util:make_name("httpd_conf", Address, Port),
    Config = 
	lists:map(fun(Prop) -> {Prop, httpd_util:lookup(Tab, Prop)} end,
		  Properties),
    [{Proporty, Value} || {Proporty, Value} <- Config, Value =/= undefined].  
			   
%%%=========================================================================
%%%  Application internal API
%%%=========================================================================
%% The configuration data is handled in three (3) phases:
%% 1. Parse the config file and put all directives into a key-vale
%%    tuple list (load/1). 
%% 2. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

%% Phase 1: Load
load(ConfigFile) ->
    case read_config_file(ConfigFile) of
	{ok, Config} ->
	    case bootstrap(Config) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Modules} ->
		    load_config(Config, lists:append(Modules, [?MODULE]))
	    end;
	{error, Reason} ->
	    {error, ?NICE("Error while reading config file: "++Reason)}
    end.

load(eof, []) ->
    eof;
load("MaxHeaderSize " ++ MaxHeaderSize, []) ->
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_header_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;

load("MaxURISize " ++ MaxHeaderSize, []) ->
    case make_integer(MaxHeaderSize) of
        {ok, Integer} ->
            {ok, [], {max_uri_size, Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxHeaderSize)++
                          " is an invalid number of MaxHeaderSize")}
    end;

load("MaxBodySize " ++ MaxBodySize, []) ->
    case make_integer(MaxBodySize) of
        {ok, Integer} ->
            {ok, [], {max_body_size,Integer}};
        {error, _} ->
            {error, ?NICE(clean(MaxBodySize)++
                          " is an invalid number of MaxBodySize")}
    end;

load("ServerName " ++ ServerName, []) ->
    {ok,[],{server_name,clean(ServerName)}};
load("SocketType " ++ SocketType, []) ->
    case check_enum(clean(SocketType),["ssl","ip_comm"]) of
	{ok, ValidSocketType} ->
	    {ok, [], {socket_type,ValidSocketType}};
	{error,_} ->
	    {error, ?NICE(clean(SocketType) ++ " is an invalid SocketType")}
    end;
load("Port " ++ Port, []) ->
    case make_integer(Port) of
	{ok, Integer} ->
	    {ok, [], {port,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Port)++" is an invalid Port")}
    end;
load("BindAddress " ++ Address, []) ->
    %% If an ipv6 address is provided in URL-syntax strip the
    %% url specific part e.i. "[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]"
    %% -> "FEDC:BA98:7654:3210:FEDC:BA98:7654:3210"
    NewAddress = string:strip(string:strip(clean(Address), 
					   left, $[), 
			      right, $]),
    case NewAddress of
	"*" ->
	    {ok, [], {bind_address,any}};
	CAddress ->
	    case httpd_util:ip_address(CAddress) of
		{ok, IPAddr} ->
		    {ok, [], {bind_address,IPAddr}};
		{error, _} ->
		    {error, ?NICE(CAddress++" is an invalid address")}
	    end
    end;
load("KeepAlive " ++ OnorOff, []) ->
    case list_to_atom(clean(OnorOff)) of
	off ->
	    {ok, [], {keep_alive, false}};
	_ ->
	    {ok, [], {keep_alive, true}}
    end;
load("MaxKeepAliveRequests " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequests")}
    end;
%% This clause is keept for backwards compability 
load("MaxKeepAliveRequest " ++  MaxRequests, []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {max_keep_alive_request, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests) ++
			  " is an invalid MaxKeepAliveRequest")}
    end;
load("KeepAliveTimeout " ++ Timeout, []) ->
    case make_integer(Timeout) of
	{ok, Integer} ->
	    {ok, [], {keep_alive_timeout, Integer*1000}};
	{error, _} ->
	    {error, ?NICE(clean(Timeout)++" is an invalid KeepAliveTimeout")}
    end;
load("Modules " ++ Modules, []) ->
    {ok, ModuleList} = regexp:split(Modules," "),
    {ok, [], {modules,[list_to_atom(X) || X <- ModuleList]}};
load("ServerAdmin " ++ ServerAdmin, []) ->
    {ok, [], {server_admin,clean(ServerAdmin)}};
load("ServerRoot " ++ ServerRoot, []) ->
    case is_directory(clean(ServerRoot)) of
	{ok, Directory} ->
	    {ok, [], [{server_root,string:strip(Directory,right,$/)}]};
	{error, _} ->
	    {error, ?NICE(clean(ServerRoot)++" is an invalid ServerRoot")}
    end;

load("MimeTypes " ++ MimeTypes, []) ->
    case load_mime_types(clean(MimeTypes)) of
	{ok, MimeTypesList} ->
	    {ok, [], [{mime_types, MimeTypesList}]};
	{error, Reason} ->
	    {error, Reason}
    end;

load("MaxClients " ++ MaxClients, []) ->
    case make_integer(MaxClients) of
	{ok, Integer} ->
	    {ok, [], {max_clients,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxClients) ++
			  " is an invalid number of MaxClients")}
    end;
load("DocumentRoot " ++ DocumentRoot,[]) ->
    case is_directory(clean(DocumentRoot)) of
	{ok, Directory} ->
	    {ok, [], {document_root,string:strip(Directory,right,$/)}};
	{error, _} ->
	    {error, ?NICE(clean(DocumentRoot)++"is an invalid DocumentRoot")}
    end;
load("DefaultType " ++ DefaultType, []) ->
    {ok, [], {default_type,clean(DefaultType)}};
load("SSLCertificateFile " ++ SSLCertificateFile, []) ->
    case is_file(clean(SSLCertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_file,File}};
    {error, _} ->
	    {error, ?NICE(clean(SSLCertificateFile)++
			  " is an invalid SSLCertificateFile")}
    end;
load("SSLCertificateKeyFile " ++ SSLCertificateKeyFile, []) ->
    case is_file(clean(SSLCertificateKeyFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_key_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCertificateKeyFile)++
			  " is an invalid SSLCertificateKeyFile")}
    end;
load("SSLVerifyClient " ++ SSLVerifyClient, []) ->
    case make_integer(clean(SSLVerifyClient)) of
	{ok, Integer} when Integer >=0,Integer =< 2 ->
	    {ok, [], {ssl_verify_client,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyClient) ++
			 " is an invalid SSLVerifyClient")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyClient) ++ 
			 " is an invalid SSLVerifyClient")}
    end;
load("SSLVerifyDepth " ++ SSLVerifyDepth, []) ->
    case make_integer(clean(SSLVerifyDepth)) of
	{ok, Integer} when Integer > 0 ->
	    {ok, [], {ssl_verify_client_depth,Integer}};
	{ok, _Integer} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyDepth) ++
			 " is an invalid SSLVerifyDepth")}
    end;
load("SSLCiphers " ++ SSLCiphers, []) ->
    {ok, [], {ssl_ciphers, clean(SSLCiphers)}};
load("SSLCACertificateFile " ++ SSLCACertificateFile, []) ->
    case is_file(clean(SSLCACertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_ca_certificate_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCACertificateFile)++
			  " is an invalid SSLCACertificateFile")}
    end;
load("SSLPasswordCallbackModule " ++ SSLPasswordCallbackModule, []) ->
    {ok, [], {ssl_password_callback_module,
	      list_to_atom(clean(SSLPasswordCallbackModule))}};
load("SSLPasswordCallbackFunction " ++ SSLPasswordCallbackFunction, []) ->
    {ok, [], {ssl_password_callback_function,
	      list_to_atom(clean(SSLPasswordCallbackFunction))}};
load("SSLPasswordCallbackArguments " ++ SSLPasswordCallbackArguments, []) ->
    {ok, [], {ssl_password_callback_arguments, 
	                         SSLPasswordCallbackArguments}};
load("DisableChunkedTransferEncodingSend " ++ TrueOrFalse, []) ->
    case list_to_atom(clean(TrueOrFalse)) of
	true ->
	    {ok, [], {disable_chunked_transfer_encoding_send, true}};
	_ ->
	    {ok, [], {disable_chunked_transfer_encoding_send, false}}
    end;
load("LogFormat " ++ LogFormat, []) ->
    {ok,[],{log_format, list_to_atom(httpd_conf:clean(LogFormat))}};
load("ErrorLogFormat " ++ LogFormat, []) ->
    {ok,[],{error_log_format, list_to_atom(httpd_conf:clean(LogFormat))}}.

%%
%% load_mime_types/1 -> {ok, MimeTypes} | {error, Reason}
%%
load_mime_types(MimeTypesFile) ->
    case file:open(MimeTypesFile, [read]) of
	{ok, Stream} ->
	    parse_mime_types(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open " ++ MimeTypesFile)}
    end.

validate_config_params([]) ->
    ok;
validate_config_params([{max_header_size, Value} | Rest]) 
  when is_integer(Value), Value > 0 ->
    validate_config_params(Rest);
validate_config_params([{max_header_size, Value} | _]) ->
    throw({max_header_size, Value});

validate_config_params([{max_body_size, Value} | Rest]) 
  when is_integer(Value), Value > 0 ->
    validate_config_params(Rest);
validate_config_params([{max_body_size, Value} | _]) -> 
    throw({max_body_size, Value});

validate_config_params([{server_name, Value} | Rest])  
  when is_list(Value)->
    validate_config_params(Rest);
validate_config_params([{server_name, Value} | _]) ->
    throw({server_name, Value});

validate_config_params([{socket_type, Value} | Rest]) 
  when Value == ip_comm;
       Value == ssl ->
    validate_config_params(Rest);
validate_config_params([{socket_type, Value} | _]) ->
    throw({socket_type, Value});

validate_config_params([{port, Value} | Rest]) 
  when is_integer(Value), Value >= 0 ->
    validate_config_params(Rest);
validate_config_params([{port, Value} | _]) -> 
    throw({port, Value});

validate_config_params([{bind_address, Value} | Rest])  ->
    case is_bind_address(Value) of
	true ->
	    validate_config_params(Rest);
	false ->
	    throw({bind_address, Value})
    end;

validate_config_params([{keep_alive, Value} | Rest])  
  when Value == true;
       Value == false ->
    validate_config_params(Rest);
validate_config_params([{keep_alive, Value} | _]) ->
    throw({keep_alive, Value});

validate_config_params([{max_keep_alive_request, Value} | Rest]) 
  when is_integer(Value), Value > 0 ->
    validate_config_params(Rest);
validate_config_params([{max_keep_alive_request, Value} | _]) ->
    throw({max_header_size, Value});

validate_config_params([{keep_alive_timeout, Value} | Rest]) 
  when is_integer(Value), Value >= 0 ->
    validate_config_params(Rest);
validate_config_params([{keep_alive_timeout, Value} | _]) ->
    throw({keep_alive_timeout, Value});

validate_config_params([{modules, Value} | Rest]) ->
    ok = httpd_util:modules_validate(Value),
    validate_config_params(Rest);
	  
validate_config_params([{server_admin, Value} | Rest]) when is_list(Value)->
    validate_config_params(Rest);
validate_config_params([{server_admin, Value} | _]) ->
    throw({server_admin, Value});

validate_config_params([{server_root, Value} | Rest]) ->
    ok = httpd_util:dir_validate(server_root, Value),
    validate_config_params(Rest);

validate_config_params([{mime_types, Value} | Rest]) ->
    ok = httpd_util:mime_types_validate(Value),
    validate_config_params(Rest);

validate_config_params([{max_clients, Value} | Rest]) 
  when is_integer(Value), Value > 0 ->
    validate_config_params(Rest);
validate_config_params([{max_clients, Value} | _]) ->
    throw({max_clients, Value});

validate_config_params([{document_root, Value} | Rest]) ->
    ok = httpd_util:dir_validate(document_root, Value),
    validate_config_params(Rest);

validate_config_params([{default_type, Value} | Rest]) when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{default_type, Value} | _]) ->
    throw({default_type, Value});

validate_config_params([{ssl_certificate_file, Value} | Rest]) ->
    ok = httpd_util:file_validate(ssl_certificate_file, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_certificate_key_file, Value} | Rest]) ->
    ok = httpd_util:file_validate(ssl_certificate_file, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_verify_client, Value} | Rest]) when 
  Value == 0; Value == 1; Value == 2 ->
    validate_config_params(Rest);

validate_config_params([{ssl_verify_client_depth, Value} | Rest]) 
  when is_integer(Value), Value >= 0 ->
    validate_config_params(Rest);
validate_config_params([{ssl_verify_client_depth, Value} | _]) ->
    throw({ssl_verify_client_depth, Value});

validate_config_params([{ssl_ciphers, Value} | Rest]) when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_ciphers, Value} | _]) ->
    throw({ssl_ciphers, Value});

validate_config_params([{ssl_ca_certificate_file, Value} | Rest]) ->
    ok = httpd_util:file_validate(ssl_certificate_file, Value),
    validate_config_params(Rest);

validate_config_params([{ssl_password_callback_module, Value} | Rest]) 
  when is_atom(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_module, Value} | _]) ->
    throw({ssl_password_callback_module, Value});

validate_config_params([{ssl_password_callback_function, Value} | Rest]) 
  when is_atom(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_function, Value} | _]) ->
    throw({ssl_password_callback_function, Value});

validate_config_params([{ssl_password_callback_arguments, Value} | Rest]) 
  when is_list(Value) ->
    validate_config_params(Rest);
validate_config_params([{ssl_password_callback_arguments, Value} | _]) ->
    throw({ssl_password_callback_arguments, Value});

validate_config_params([{disable_chunked_transfer_encoding_send, Value} |
			Rest])  when Value == true; Value == false ->
    validate_config_params(Rest);
validate_config_params([{disable_chunked_transfer_encoding_send, Value} |
			_ ]) ->
    throw({disable_chunked_transfer_encoding_send, Value});
validate_config_params([_| Rest]) ->
    validate_config_params(Rest).

is_bind_address(any) ->
    true;
is_bind_address(Value) ->
    case httpd_util:ip_address(Value) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

store(ConfigList0) -> 
    try validate_config_params(ConfigList0) of
	ok ->
	    Modules = 
		proplists:get_value(modules, ConfigList0, ?DEFAULT_MODS),
	    Port = proplists:get_value(port, ConfigList0),
	    Addr = proplists:get_value(bind_address, ConfigList0, any),
	    ConfigList = fix_mime_types(ConfigList0),
	    Name = httpd_util:make_name("httpd_conf",Addr,Port),
	    ConfigDB = ets:new(Name, [named_table, bag, protected]),
	    store(ConfigDB, ConfigList, 
		  lists:append(Modules,[?MODULE]), ConfigList)
    catch
	throw:Error ->
	    {error, {invalid_option, Error}}
    end.

fix_mime_types(ConfigList0) ->
    case proplists:get_value(mime_types, ConfigList0) of
	undefined ->
	    ServerRoot = proplists:get_value(server_root, ConfigList0),
		MimeTypesFile = 
		filename:join([ServerRoot,"conf", "mime.types"]),
		case filelib:is_file(MimeTypesFile) of
		    true ->
			{ok, MimeTypesList} = load_mime_types(MimeTypesFile),
			[{mime_types, MimeTypesList} | ConfigList0];
		    false ->
			[{mime_types,
			  [{"html","text/html"},{"htm","text/html"}]} 
			 | ConfigList0]
		end;
	_ ->
	    ConfigList0
    end.

store({mime_types,MimeTypesList},ConfigList) ->
    Port = proplists:get_value(port, ConfigList),
    Addr = proplists:get_value(bind_address, ConfigList),
    Name = httpd_util:make_name("httpd_mime",Addr,Port),
    {ok, MimeTypesDB} = store_mime_types(Name,MimeTypesList),
    {ok, {mime_types,MimeTypesDB}};
store({log_format, LogFormat}, _ConfigList) when LogFormat == common; 
						 LogFormat == combined ->
    {ok,{log_format, LogFormat}};
store({log_format, LogFormat}, _ConfigList) when LogFormat == compact; 
						 LogFormat == pretty ->
    {ok,{log_format, LogFormat}};
store(ConfigListEntry, _ConfigList) ->
    {ok, ConfigListEntry}.

%% Phase 3: Remove
remove_all(ConfigDB) ->
    Modules = httpd_util:lookup(ConfigDB,modules,[]),
    remove_traverse(ConfigDB, lists:append(Modules,[?MODULE])).

remove(ConfigDB) ->
    ets:delete(ConfigDB),
    ok.

config(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, socket_type,ip_comm) of
	ssl ->
	    case ssl_certificate_file(ConfigDB) of
		undefined ->
		    {error,
		     "Directive SSLCertificateFile "
		     "not found in the config file"};
		SSLCertificateFile ->
		    {ssl,
		     SSLCertificateFile++
		     ssl_certificate_key_file(ConfigDB)++
		     ssl_verify_client(ConfigDB)++
		     ssl_ciphers(ConfigDB)++
		     ssl_password(ConfigDB)++
		     ssl_verify_depth(ConfigDB)++
		     ssl_ca_certificate_file(ConfigDB)}
	    end;
	ip_comm ->
	    ip_comm
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% Phase 1 Load:
bootstrap([]) ->
    {ok, ?DEFAULT_MODS};
bootstrap([Line|Config]) ->
    case Line of
	"Modules " ++ Modules ->
	    {ok, ModuleList} = regexp:split(Modules," "),
	    TheMods = [list_to_atom(X) || X <- ModuleList],
	    case verify_modules(TheMods) of
		ok ->
		    {ok, TheMods};
		{error, Reason} ->
		    {error, Reason}
	    end;
	_ ->
	    bootstrap(Config)
    end.

load_config(Config, Modules) ->
    %% Create default contexts for all modules
    Contexts = lists:duplicate(length(Modules), []),
    load_config(Config, Modules, Contexts, []).
load_config([], _Modules, _Contexts, ConfigList) ->
    {ok, ConfigList};
	
load_config([Line|Config], Modules, Contexts, ConfigList) ->
    case load_traverse(Line, Contexts, Modules, [], ConfigList, no) of
	{ok, NewContexts, NewConfigList} ->
	    load_config(Config, Modules, NewContexts, NewConfigList);
	{error, Reason} -> 
	    {error, Reason}
    end.


%% This loads the config file into each module specified by Modules
%% Each module has its own context that is passed to and (optionally)
%% returned by the modules load function. The module can also return
%% a ConfigEntry, which will be added to the global configuration
%% list.
%% All configuration directives are guaranteed to be passed to all
%% modules. Each module only implements the function clauses of
%% the load function for the configuration directives it supports,
%% it's ok if an apply returns {'EXIT', {function_clause, ..}}.
load_traverse(Line, [], [], _NewContexts, _ConfigList, no) ->
    {error, ?NICE("Configuration directive not recognized: "++Line)};
load_traverse(_Line, [], [], NewContexts, ConfigList, yes) ->
    {ok, lists:reverse(NewContexts), ConfigList};
load_traverse(Line, [Context|Contexts], [Module|Modules], NewContexts,
	      ConfigList, State) ->
    case catch apply(Module, load, [Line, Context]) of
	{'EXIT', {function_clause, _}} ->
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);
	{'EXIT',{undef, _}} ->
	    load_traverse(Line, Contexts, Modules,
			  [Context|NewContexts], ConfigList,yes);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT', Reason}),
	    load_traverse(Line, Contexts, Modules, 
			  [Context|NewContexts], ConfigList, State);
	{ok, NewContext} ->
	    load_traverse(Line, Contexts, Modules, 
			  [NewContext|NewContexts], ConfigList,yes);
	{ok, NewContext, ConfigEntry} when tuple(ConfigEntry) ->
	  load_traverse(Line, Contexts, 
			Modules, [NewContext|NewContexts],
			  [ConfigEntry|ConfigList], yes);
	{ok, NewContext, ConfigEntry} when list(ConfigEntry) ->
	    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
			  lists:append(ConfigEntry, ConfigList), yes);
	{error, Reason} ->
	    {error, Reason}
    end.
	
%% Verifies that all specified modules are available.
verify_modules([]) ->
    ok;
verify_modules([Mod|Rest]) ->
    case code:which(Mod) of
	non_existing ->
	    {error, ?NICE(atom_to_list(Mod)++" does not exist")};
	_Path ->
	    verify_modules(Rest)
    end.

%% Reads the entire configuration file and returns list of strings or
%% and error.
read_config_file(FileName) ->
    case file:open(FileName, [read]) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, _Reason} ->
	    {error, ?NICE("Cannot open "++FileName)}
    end.
read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    file:close(Stream),
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    file:close(Stream),
	    {error, Reason};
	[$#|_Rest] ->
	    %% Ignore commented lines for efficiency later ..
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    %% Also ignore empty lines ..
		    read_config_file(Stream, SoFar);
		_Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

parse_mime_types(Stream,MimeTypesList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		clean(String)
	end,
    parse_mime_types(Stream, MimeTypesList, Line).
parse_mime_types(Stream, MimeTypesList, eof) ->
    file:close(Stream),
    {ok, MimeTypesList};
parse_mime_types(Stream, MimeTypesList, "") ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, [$#|_]) ->
    parse_mime_types(Stream, MimeTypesList);
parse_mime_types(Stream, MimeTypesList, Line) ->
    case regexp:split(Line, " ") of
	{ok, [NewMimeType|Suffixes]} ->
	    parse_mime_types(Stream,
			     lists:append(suffixes(NewMimeType,Suffixes),
					  MimeTypesList));
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.

suffixes(_MimeType,[]) ->
    [];
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].


%% Pahse 2: store
store(ConfigDB, _ConfigList, _Modules,[]) ->
    {ok, ConfigDB};
store(ConfigDB, ConfigList, Modules, [ConfigListEntry|Rest]) ->
    case store_traverse(ConfigListEntry,ConfigList,Modules) of
	{ok, ConfigDBEntry} when tuple(ConfigDBEntry) ->
	    ets:insert(ConfigDB,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{ok, ConfigDBEntry} when list(ConfigDBEntry) ->
	    lists:foreach(fun(Entry) ->
				  ets:insert(ConfigDB,Entry)
			  end,ConfigDBEntry),
	    store(ConfigDB,ConfigList,Modules,Rest);
	{error, Reason} ->
	    {error,Reason}
    end.

store_traverse(_ConfigListEntry, _ConfigList,[]) ->
    {error,?NICE("Unable to store configuration...")};
store_traverse(ConfigListEntry, ConfigList, [Module|Rest]) ->
    case catch apply(Module,store,[ConfigListEntry, ConfigList]) of
	{'EXIT',{function_clause,_}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT',{undef, _}} ->
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	{'EXIT', Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    store_traverse(ConfigListEntry,ConfigList,Rest);
	Result ->
	    Result
    end.

store_mime_types(Name,MimeTypesList) ->
    %% Make sure that the ets table is not duplicated
    %% when reloading configuration
    catch ets:delete(Name),
    MimeTypesDB = ets:new(Name, [named_table, set, protected]),
    store_mime_types1(MimeTypesDB, MimeTypesList).
store_mime_types1(MimeTypesDB,[]) ->
    {ok, MimeTypesDB};
store_mime_types1(MimeTypesDB,[Type|Rest]) ->
    ets:insert(MimeTypesDB, Type),
    store_mime_types1(MimeTypesDB, Rest).


%% Phase 3: remove
remove_traverse(_ConfigDB,[]) ->
    ok;
remove_traverse(ConfigDB,[Module|Rest]) ->
    case (catch apply(Module,remove,[ConfigDB])) of
	{'EXIT',{undef,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',{function_clause,_}} ->
	    remove_traverse(ConfigDB,Rest);
	{'EXIT',Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    remove_traverse(ConfigDB,Rest);
	{error,Reason} ->
	    error_logger:error_report(Reason),
	    remove_traverse(ConfigDB,Rest);
	_ ->
	    remove_traverse(ConfigDB,Rest)
    end.

ssl_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_file) of
	undefined ->
	    undefined;
	SSLCertificateFile ->
	    [{certfile,SSLCertificateFile}]
    end.

ssl_certificate_key_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_key_file) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_verify_client(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_verify_client) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_ciphers) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_password_callback_module) of
	undefined ->
	    [];
	Module ->
	    case httpd_util:lookup(ConfigDB, 
				   ssl_password_callback_function) of
		undefined ->
		    [];
		Function ->
		    Args = case httpd_util:lookup(ConfigDB, 
					   ssl_password_callback_arguments) of
			       undefined ->
				   [];
			       Arguments  ->
				   [Arguments]
			   end,
	       
		    case catch apply(Module, Function, Args) of
			Password when list(Password) ->
			    [{password, Password}];
			Error ->
			    error_report(ssl_password,Module,Function,Error),
			    []
		    end
	    end
    end.

ssl_verify_depth(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_verify_client_depth) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_ca_certificate_file) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.

error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, 
			       {apply, {M, F, []}}, Error]).
