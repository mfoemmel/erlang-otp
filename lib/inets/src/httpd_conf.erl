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
-module(httpd_conf).
-export([load/1, load/2, store/1, store/2, remove_all/1, remove/1, is_directory/1,
	 is_file/1, make_integer/1, clean/1, custom_clean/3, check_enum/2]).

%% The configuration data is handled in three (3) phases:
%% 1. Parse the config file and put all directives into a key-vale
%%    tuple list (load/1). 
%% 2. Traverse the key-value tuple list store it into an ETS table.
%%    Directives depending on other directives are taken care of here
%%    (store/1).
%% 3. Traverse the ETS table and do a complete clean-up (remove/1).

-include("httpd.hrl").

%%
%% Phase 1: Load
%%

%% load

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

bootstrap([]) ->
    {error, ?NICE("Modules must be specified in the config file")};
bootstrap([Line|Config]) ->
    case Line of
	[$M,$o,$d,$u,$l,$e,$s,$ |Modules] ->
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


%%
%% verify_modules/1 -> ok | {error, Reason}
%%
%% Verifies that all specified modules are available.
%%
verify_modules([]) ->
    ok;
verify_modules([Mod|Rest]) ->
    case code:which(Mod) of
	non_existing ->
	    {error, ?NICE(atom_to_list(Mod)++" does not exist")};
	Path ->
	    verify_modules(Rest)
    end.

%%
%% read_config_file/1 -> {ok, [line(), line()..]} | {error, Reason}
%%
%% Reads the entire configuration file and returns list of strings or
%% and error.
%%
read_config_file(FileName) ->
    case file:open(FileName, read) of
	{ok, Stream} ->
	    read_config_file(Stream, []);
	{error, Reason} ->
	    {error, ?NICE("Cannot open "++FileName)}
    end.

read_config_file(Stream, SoFar) ->
    case io:get_line(Stream, []) of
	eof ->
	    {ok, lists:reverse(SoFar)};
	{error, Reason} ->
	    {error, Reason};
	[$#|Rest] ->
	    %% Ignore commented lines for efficiency later ..
	    read_config_file(Stream, SoFar);
	Line ->
	    {ok, NewLine, _}=regexp:sub(clean(Line),"[\t\r\f ]"," "),
	    case NewLine of
		[] ->
		    %% Also ignore empty lines ..
		    read_config_file(Stream, SoFar);
		Other ->
		    read_config_file(Stream, [NewLine|SoFar])
	    end
    end.

is_exported(Module, ToFind) ->
    Exports = Module:module_info(exports),
    lists:member(ToFind, Exports).

%%
%% load/4 -> {ok, ConfigList} | {error, Reason}
%%
%% This loads the config file into each module specified by Modules
%% Each module has its own context that is passed to and (optionally)
%% returned by the modules load function. The module can also return
%% a ConfigEntry, which will be added to the global configuration
%% list.
%% All configuration directives are guaranteed to be passed to all
%% modules. Each module only implements the function clauses of
%% the load function for the configuration directives it supports,
%% it's ok if an apply returns {'EXIT', {function_clause, ..}}.
%%
load_config(Config, Modules) ->
    %% Create default contexts for all modules
    Contexts = lists:duplicate(length(Modules), []),
    load_config(Config, Modules, Contexts, []).


load_config([], _Modules, _Contexts, ConfigList) ->
    case a_must(ConfigList, [server_name, port, server_root, document_root]) of
	ok ->
	    {ok, ConfigList};
	{missing, Directive} ->
	    {error, ?NICE(atom_to_list(Directive)++
			  " must be specified in the config file")}
    end;

load_config([Line|Config], Modules, Contexts, ConfigList) ->
    case load_traverse(Line, Contexts, Modules, [], ConfigList, no) of
	{ok, NewContexts, NewConfigList} ->
	    load_config(Config, Modules, NewContexts, NewConfigList);
	{error, Reason} -> 
	    {error, Reason}
    end.


load_traverse(Line, [], [], NewContexts, ConfigList, no) ->
    {error, ?NICE("Configuration directive not recognized: "++Line)};
load_traverse(Line, [], [], NewContexts, ConfigList, yes) ->
    {ok, lists:reverse(NewContexts), ConfigList};
load_traverse(Line, [Context|Contexts], [Module|Modules], NewContexts, ConfigList, State) ->
    case is_exported(Module, {load, 2}) of
	true ->
	    case catch apply(Module, load, [Line, Context]) of
		{'EXIT', {function_clause, _}} ->
		    load_traverse(Line, Contexts, Modules, [Context|NewContexts], ConfigList, State);
		{'EXIT', Reason} ->
		    error_logger:error_report({'EXIT', Reason}),
		    load_traverse(Line, Contexts, Modules, [Context|NewContexts], ConfigList, State);
		{ok, NewContext} ->
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts], ConfigList,yes);
		{ok, NewContext, ConfigEntry} when tuple(ConfigEntry) ->
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
				  [ConfigEntry|ConfigList], yes);
		{ok, NewContext, ConfigEntry} when list(ConfigEntry) ->
		    load_traverse(Line, Contexts, Modules, [NewContext|NewContexts],
				  lists:append(ConfigEntry, ConfigList), yes);
		{error, Reason} ->
		    {error, Reason}
	    end;
	false ->
	    load_traverse(Line, Contexts, Modules, [Context|NewContexts],
			  ConfigList,yes)
    end.
	

load(eof, []) ->
    eof;
load([$S,$e,$r,$v,$e,$r,$N,$a,$m,$e,$ |ServerName], []) ->
    {ok,[],{server_name,clean(ServerName)}};
load([$S,$o,$c,$k,$e,$t,$T,$y,$p,$e,$ |SocketType], []) ->
    case check_enum(clean(SocketType),["ssl","ip_comm"]) of
	{ok, ValidSocketType} ->
	    {ok, [], {com_type,ValidSocketType}};
	{error,_} ->
	    {error, ?NICE(clean(SocketType)++" is an invalid SocketType")}
    end;
load([$P,$o,$r,$t,$ |Port], []) ->
    case make_integer(Port) of
	{ok, Integer} ->
	    {ok, [], {port,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(Port)++" is an invalid Port")}
    end;
load([$B,$i,$n,$d,$A,$d,$d,$r,$e,$s,$s,$ |Address], []) ->
    case clean(Address) of
	"*" ->
	    {ok, [], {bind_address,any}};
	CAddress ->
	    case inet:getaddr(CAddress) of
		{ok, IPAddr} ->
		    {ok, [], {bind_address,IPAddr}};
		{error, _} ->
		    {error, ?NICE(CAddress++" is an invalid address")}
	    end
    end;
load([$K,$e,$e,$p,$A,$l,$i,$v,$e,$ |MaxRequests], []) ->
    case make_integer(MaxRequests) of
	{ok, Integer} ->
	    {ok, [], {keep_alive, Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxRequests)++" is an invalid KeepAlive")}
    end;
load([$K,$e,$e,$p,$A,$l,$i,$v,$e,$T,$i,$m,$e,$o,$u,$t,$ |Timeout], []) ->
    case make_integer(Timeout) of
	{ok, Integer} ->
	    {ok, [], {keep_alive_timeout, Integer*1000}};
	{error, _} ->
	    {error, ?NICE(clean(Timeout)++" is an invalid KeepAliveTimeout")}
    end;
load([$M,$o,$d,$u,$l,$e,$s,$ |Modules], []) ->
    {ok, ModuleList} = regexp:split(Modules," "),
    {ok, [], {modules,[list_to_atom(X) || X <- ModuleList]}};
load([$S,$e,$r,$v,$e,$r,$A,$d,$m,$i,$n,$ |ServerAdmin], []) ->
    {ok, [], {server_admin,clean(ServerAdmin)}};
load([$S,$e,$r,$v,$e,$r,$R,$o,$o,$t,$ |ServerRoot], []) ->
    case is_directory(clean(ServerRoot)) of
	{ok, Directory} ->
      case load_mime_types(clean(ServerRoot)) of
	  {ok, MimeTypesList} ->
	      {ok, [], [{server_root,string:strip(Directory,right,$/)},
			{mime_types,MimeTypesList}]};
	  {error, Reason} ->
	      {error, Reason}
      end;
	{error, _} ->
	    {error, ?NICE(clean(ServerRoot)++" is an invalid ServerRoot")}
    end;
load([$M,$a,$x,$C,$l,$i,$e,$n,$t,$s,$ |MaxClients], []) ->
    case make_integer(MaxClients) of
	{ok, Integer} ->
	    {ok, [], {max_clients,Integer}};
	{error, _} ->
	    {error, ?NICE(clean(MaxClients)++" is an invalid number of MaxClients")}
    end;
load([$D,$o,$c,$u,$m,$e,$n,$t,$R,$o,$o,$t,$ |DocumentRoot],[]) ->
    case is_directory(clean(DocumentRoot)) of
	{ok, Directory} ->
	    {ok, [], {document_root,string:strip(Directory,right,$/)}};
	{error, _} ->
	    {error, ?NICE(clean(DocumentRoot)++"is an invalid DocumentRoot")}
    end;
load([$D,$e,$f,$a,$u,$l,$t,$T,$y,$p,$e,$ |DefaultType], []) ->
    {ok, [], {default_type,clean(DefaultType)}};
load([$S,$S,$L,$C,$e,$r,$t,$i,$f,$i,$c,$a,$t,$e,$F,$i,$l,$e,$ | SSLCertificateFile], []) ->
    case is_file(clean(SSLCertificateFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_file,File}};
    {error, _} ->
	    {error, ?NICE(clean(SSLCertificateFile)++
			  " is an invalid SSLCertificateFile")}
    end;
load([$S,$S,$L,$C,$e,$r,$t,$i,$f,$i,$c,$a,$t,$e,$K,$e,$y,$F,$i,$l,$e,$ |
      SSLCertificateKeyFile], []) ->
    case is_file(clean(SSLCertificateKeyFile)) of
	{ok, File} ->
	    {ok, [], {ssl_certificate_key_file,File}};
	{error, _} ->
	    {error, ?NICE(clean(SSLCertificateKeyFile)++
			  " is an invalid SSLCertificateKeyFile")}
    end;
load([$S,$S,$L,$V,$e,$r,$i,$f,$y,$C,$l,$i,$e,$n,$t,$ |SSLVerifyClient], []) ->
    case make_integer(clean(SSLVerifyClient)) of
	{ok, Integer} when Integer >=0,Integer =< 2 ->
	    {ok, [], {ssl_verify_client,Integer}};
	{ok, Integer} ->
	    {error,?NICE(clean(SSLVerifyClient)++" is an invalid SSLVerifyClient")};
	{error, nomatch} ->
	    {error,?NICE(clean(SSLVerifyClient)++" is an invalid SSLVerifyClient")}
    end.

%%
%% load_mime_types/1 -> {ok, MimeTypes} | {error, Reason}
%%
load_mime_types(ServerRoot) ->
    case file:open(filename:join([ServerRoot,"conf", "mime.types"]), read) of
	{ok, Stream} ->
	    parse_mime_types(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open "++filename:join([ServerRoot,"conf", "mime.types"]))}
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
	    parse_mime_types(Stream,lists:append(suffixes(NewMimeType,Suffixes),
						 MimeTypesList));
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.

suffixes(MimeType,[]) ->
    [];
suffixes(MimeType,[Suffix|Rest]) ->
    [{Suffix,MimeType}|suffixes(MimeType,Rest)].

%%
%% Phase 2: Store
%%

%% store

store(ConfigList) ->
    Modules = httpd_util:key1search(ConfigList, modules, []),
    Port = httpd_util:key1search(ConfigList, port),
    Name = list_to_atom(lists:flatten(io_lib:format("httpd_conf~w",[Port]))),
    ConfigDB = ets:new(Name, [named_table, bag, protected]),
    store(ConfigDB, ConfigList, lists:append(Modules,[?MODULE]),ConfigList).

store(ConfigDB, ConfigList, Modules,[]) ->
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

store_traverse(ConfigListEntry,ConfigList,[]) ->
    {error,?NICE("Unable to store configuration...")};
store_traverse(ConfigListEntry, ConfigList, [Module|Rest]) ->
    case is_exported(Module, {store, 2}) of
	true ->
	    case catch apply(Module,store,[ConfigListEntry, ConfigList]) of
		{'EXIT',{function_clause,_}} ->
		    store_traverse(ConfigListEntry,ConfigList,Rest);
		{'EXIT',Reason} ->
		    error_logger:error_report({'EXIT',Reason}),
		    store_traverse(ConfigListEntry,ConfigList,Rest);
		Result ->
		    Result
	    end;
	false ->
	    store_traverse(ConfigListEntry,ConfigList,Rest)
    end.

store({mime_types,MimeTypesList},ConfigList) ->
    {ok, MimeTypesDB}=store_mime_types(MimeTypesList),
    {ok, {mime_types,MimeTypesDB}};
store(ConfigListEntry,ConfigList) ->
    {ok, ConfigListEntry}.


%% store_mime_types
store_mime_types(MimeTypesList) ->
    MimeTypesDB = ets:new(httpd, [set, protected]),
    store_mime_types(MimeTypesDB, MimeTypesList).

store_mime_types(MimeTypesDB,[]) ->
    {ok, MimeTypesDB};
store_mime_types(MimeTypesDB,[Type|Rest]) ->
    ets:insert(MimeTypesDB, Type),
    store_mime_types(MimeTypesDB, Rest).


%%
%% Phase 3: Remove
%%

remove_all(ConfigDB) ->
    Modules = httpd_util:lookup(ConfigDB,modules,[]),
    remove_traverse(ConfigDB, lists:append(Modules,[?MODULE])).

remove_traverse(ConfigDB,[]) ->
    ok;
remove_traverse(ConfigDB,[Module|Rest]) ->
    case catch apply(Module,remove,[ConfigDB]) of
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

remove(ConfigDB) ->
    ets:delete(ConfigDB),
    ok.

%%
%% Utility functions
%%

%% is_directory

is_directory(Directory) ->
    case file:file_info(Directory) of
	{ok,{Size,directory,read,AccessTime,ModifyTime,UnUsed1,UnUsed2}} ->
	    {ok,Directory};
	{ok,{Size,directory,read_write,AccessTime,ModifyTime,UnUsed1,UnUsed2}} ->
	    {ok,Directory};
	{ok,FileInfo} ->
	    {error,FileInfo};
	{error,Reason} ->
	    {error,Reason}
    end.

%% is_file

is_file(File) ->
    case file:file_info(File) of
	{ok,{Size,regular,read,AccessTime,ModifyTime,UnUsed1,UnUsed2}} ->
	    {ok,File};
	{ok,{Size,regular,read_write,AccessTime,ModifyTime,UnUsed1,UnUsed2}} ->
	    {ok,File};
	{ok,FileInfo} ->
	    {error,FileInfo};
	{error,Reason} ->
	    {error,Reason}
    end.

%% make_integer

make_integer(String) ->
    case regexp:match(clean(String),"[0-9]+") of
	{match, _, _} ->
	    {ok, list_to_integer(clean(String))};
	nomatch ->
	    {error, nomatch}
    end.

%% clean

clean(String) ->
    {ok,CleanedString,_} = regexp:gsub(String, "^[ \t\n\r\f]*|[ \t\n\r\f]*\$",""),
    CleanedString.

%% custom_clean

custom_clean(String,MoreBefore,MoreAfter) ->
    {ok,CleanedString,_}=regexp:gsub(String,"^[ \t\n\r\f"++MoreBefore++
				     "]*|[ \t\n\r\f"++MoreAfter++"]*\$",""),
    CleanedString.

%% check_enum

check_enum(Enum,[]) ->
    {error, not_valid};
check_enum(Enum,[Enum|Rest]) ->
    {ok, list_to_atom(Enum)};
check_enum(Enum, [NotValid|Rest]) ->
    check_enum(Enum, Rest).

%% a_must

a_must(ConfigList,[]) ->
    ok;
a_must(ConfigList,[Directive|Rest]) ->
    case httpd_util:key1search(ConfigList,Directive) of
	undefined ->
	    {missing,Directive};
	_ ->
	    a_must(ConfigList,Rest)
    end.
