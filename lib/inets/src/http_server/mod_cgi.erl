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
%% Implements  The WWW Common Gateway Interface Version 1.1

-module(mod_cgi).

%%% API
-export([do/1, env/3, load/2]).

%%% For apply
-export([parse_headers/1]).

-include("http.hrl").
-include("httpd.hrl").

-define(VMODULE,"CGI").
-include("httpd_verbosity.hrl").

-define(GATEWAY_INTERFACE, "CGI/1.1").
-define(DEFAULT_CGI_TIMEOUT, 15000).

%%%=========================================================================
%%%  API
%%%=========================================================================
do(ModData) ->
    ?vtrace("do",[]),
    case httpd_util:key1search(ModData#mod.data, status) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, ModData#mod.data};
	%% No status code has been generated!
	undefined ->
	    ?vtrace("do -> no status code has been generated", []),
	    case httpd_util:key1search(ModData#mod.data, response) of
		%% No response has been generated!
		undefined ->
		    ?vtrace("do -> no response has been generated", []),
		    RequestURI =
			case httpd_util:key1search(ModData#mod.data,
						   new_request_uri) of
			    undefined ->
				ModData#mod.request_uri;
			    Value ->
				Value
			end,
		    ?vtrace("do -> RequestURI: ~p", [RequestURI]),
		    ScriptAliases =
			httpd_util:multi_lookup(ModData#mod.config_db,
						script_alias),
		    ?vtrace("do -> ScriptAliases: ~p", [ScriptAliases]),
		    case mod_alias:real_script_name(ModData#mod.config_db,
						    RequestURI,
						    ScriptAliases) of
			{Script, AfterScript} ->
			    exec_script(ModData, Script, AfterScript, 
					RequestURI);
			not_a_script ->
			    {proceed, ModData#mod.data}
		    end;
		%% A response has been generated or sent!
		_Response ->
		    {proceed, ModData#mod.data}
	    end
    end.

env(VarName, Value) ->
    {VarName, Value}.

env(ModData, Script, AfterScript) ->
    ?vtrace("env -> entry with"
	    "~n   Script:      ~p"
	    "~n   AfterScript: ~p",
	    [Script, AfterScript]),
    {_, RemoteAddr} = (ModData#mod.init_data)#init_data.peername,
    ServerName = (ModData#mod.init_data)#init_data.resolve,
    PH = parsed_header(ModData#mod.parsed_header),
    Env =
	[env("SERVER_SOFTWARE",?SERVER_SOFTWARE),
	 env("SERVER_NAME",ServerName),
	 env("GATEWAY_INTERFACE",?GATEWAY_INTERFACE),
	 env("SERVER_PROTOCOL",?SERVER_PROTOCOL),
	 env("SERVER_PORT", 
	     integer_to_list(httpd_util:lookup(
			       ModData#mod.config_db,port,80))),
	 env("REQUEST_METHOD",ModData#mod.method),
	 env("REMOTE_ADDR",RemoteAddr),
	 env("SCRIPT_NAME",Script)],
    Env1 =
        case ModData#mod.method of
            "GET" ->
                case AfterScript of
		    {[], QueryString} ->
                        [env("QUERY_STRING", QueryString)|Env];
		    {PathInfo, []} ->
                        Aliases = httpd_util:multi_lookup(
				    ModData#mod.config_db,alias),
                        {_, PathTranslated, _} =
                            mod_alias:real_name(
                              ModData#mod.config_db, PathInfo, Aliases),
                        [Env|
                         [env("PATH_INFO","/" ++ 
			      httpd_util:decode_hex(PathInfo)),
                          env("PATH_TRANSLATED",PathTranslated)]];
		    {PathInfo, QueryString} ->
                        Aliases = httpd_util:multi_lookup(
				    ModData#mod.config_db,alias),
                        {_, PathTranslated, _} =
                            mod_alias:real_name(
                              ModData#mod.config_db, PathInfo, Aliases),
                        [Env|
                         [env("PATH_INFO",
			      httpd_util:decode_hex(PathInfo)),
                          env("PATH_TRANSLATED", PathTranslated),
			  env("QUERY_STRING", QueryString)]];
		    [] ->
			Env
                end;
            "POST" ->
                [env("CONTENT_LENGTH",
                     integer_to_list(httpd_util:flatlength(
                                       ModData#mod.entity_body))) | Env];
            _ ->
                Env
        end,
    Env2 =
        case httpd_util:key1search(ModData#mod.data, remote_user) of
            undefined ->
                Env1;
            RemoteUser ->
                [env("REMOTE_USER",RemoteUser) | Env1] %% OTP-4416
        end,
    lists:flatten([Env2 | PH]).


%% There are 2 config directives for mod_cgi:                         
%% ScriptNoCache true|false, defines whether the server shall add     
%%                           header fields to stop proxies and          
%%                           clients from saving the page in history  
%%                           or cache                                 
%%                                                                    
%% ScriptTimeout Seconds, The number of seconds that the server       
%%                        maximum will wait for the script to         
%%                        generate a part of the document             
load("ScriptNoCache " ++ CacheArg, [])->
    case catch list_to_atom(httpd_conf:clean(CacheArg)) of
        true ->
	    {ok, [], {script_nocache, true}};
	false ->
	   {ok, [], {script_nocache, false}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(CacheArg)++
			 " is an invalid ScriptNoCache directive")}
    end;

load("ScriptTimeout " ++ Timeout, [])->
    case catch list_to_integer(httpd_conf:clean(Timeout)) of
	TimeoutSec when integer(TimeoutSec)  ->
	   {ok, [], {script_timeout,TimeoutSec*1000}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(Timeout)++
			 " is an invalid ScriptTimeout")}
    end.
	
%%%========================================================================
%%% Internal functions
%%%========================================================================

%% is_executable(File) ->
%%    ?DEBUG("is_executable -> entry with~n"
%%	   "   File: ~s",[File]),
%%    Dir      = filename:dirname(File),
%%    FileName = filename:basename(File),
%%    is_executable(FileName,Dir).
%%
%% is_executable(FileName,Dir) ->
%%    ?DEBUG("is_executable -> entry with~n"
%%	   "   Dir:      ~s~n"
%%	   "   FileName: ~s",[Dir,FileName]),
%%    case os:find_executable(FileName, Dir) of
%%	false ->
%%	    false;
%%	_ ->
%%	    true
%%    end.


%% -------------------------
%% Start temporary (hopefully) fix for win32
%% OTP-3627
%%

is_executable(File) ->
    Dir      = filename:dirname(File),
    FileName = filename:basename(File),
    case os:type() of
	{win32,_} ->
	    is_win32_executable(Dir,FileName);
	_ ->
	    is_other_executable(Dir,FileName) 
    end.


is_win32_executable(D,F) ->
    case ends_with(F,[".bat",".exe",".com"]) of
	false ->
	    %% This is why we cant use 'os:find_executable' directly.
	    %% It assumes that executable files is given without extension
	    case os:find_executable(F,D) of
		false ->
		    false;
		_ ->
		    true
	    end;
	true ->
	    case file:read_file_info(D ++ "/" ++ F) of
		{ok,_} ->
		    true;
		_ ->
		    false
	    end
    end.
	    

is_other_executable(D,F) ->
    case os:find_executable(F,D) of
	false ->
	    false;
	_ ->
	    true
    end.


ends_with(_File,[]) ->
    false;
ends_with(File,[Ext|Rest]) ->
    case ends_with1(File,Ext) of
	true ->
	    true;
	false ->
	    ends_with(File,Rest)
    end.
    
ends_with1(S,E) when length(S) >= length(E) ->
    case httpd_util:to_lower(string:right(S,length(E))) of
	E ->
	    true;
	_ ->
	    false
    end;
ends_with1(_S,_E) ->
    false.
    
%%
%% End fix
%% ---------------------------------

%% Request
parsed_header(List) ->
    parsed_header(List, []).
 
parsed_header([], SoFar) ->
    SoFar;
parsed_header([{Name, [Value | R1]} | R2], SoFar) when list(Value)->
    NewName =lists:map(fun(X) -> if X == $- -> $_; true -> X end end, Name),
    Env = env("HTTP_"++httpd_util:to_upper(NewName),
	      multi_value([Value | R1])),
    parsed_header(R2, [Env | SoFar]);
 
parsed_header([{Name, Value} | Rest], SoFar) ->
    {ok, NewName,_} = regexp:gsub(Name, "-", "_"),
    Env = env("HTTP_"++ httpd_util:to_upper(NewName), Value),
    parsed_header(Rest, [Env | SoFar]).
 
multi_value([]) ->
  [];
multi_value([Value]) ->
  Value;
multi_value([Value | Rest]) ->
  Value ++ ", " ++ multi_value(Rest).

exec_script(ModData, Script, AfterScript, RequestURI) ->
    ?vdebug("exec_script -> entry with"
	    "~n   Script:      ~p"
	    "~n   AfterScript: ~p",
	    [Script,AfterScript]),
    exec_script(is_executable(Script), ModData, Script, 
		AfterScript, RequestURI).

exec_script(true, ModData, Script, AfterScript, RequestURI) ->
    ?vtrace("exec_script -> entry when script is executable",[]),
    process_flag(trap_exit,true),
    Dir  = filename:dirname(Script),
    [Script_Name|_] = string:tokens(RequestURI, "?"),
    Env  = env(ModData, Script_Name, AfterScript),
    Port = (catch open_port({spawn, Script},[binary, stream,
					     {cd, Dir}, {env, Env}])),
    ?vtrace("exec_script -> Port: ~w",[Port]),
    case Port of
	P when port(P) ->
	    %% Send entity_body to port.
	    Res = case ModData#mod.entity_body of
		      [] ->
			  true;
		      EntityBody ->
			  (catch port_command(Port, EntityBody))
		  end,
	    case Res of
		{'EXIT',Reason} ->
		    ?vlog("port send failed:"
			  "~n   Port:   ~p"
			  "~n   URI:    ~p"
			  "~n   Reason: ~p",
			  [Port,ModData#mod.request_uri,Reason]),
		    exit({open_cmd_failed, Reason,
			  [{mod, ?MODULE}, {port,Port},
			   {uri, ModData#mod.request_uri},
			   {script, Script},{env,Env},{dir,Dir},
			   {ebody_size,sz(ModData#mod.entity_body)}]});
		true ->
		    proxy(ModData, Port)
	    end;
	{'EXIT',Reason} ->
	    ?vlog("open port failed: exit"
		  "~n   URI:    ~p"
		  "~n   Reason: ~p",
		  [ModData#mod.request_uri, Reason]),
	    exit({open_port_failed, Reason,
		  [{mod,?MODULE},
		   {uri,ModData#mod.request_uri}, {script,Script},
		   {env,Env},{dir,Dir}]});
	O ->
	    ?vlog("open port failed: unknown result"
		  "~n   URI: ~p"
		  "~n   O:   ~p",
		  [ModData#mod.request_uri,O]),
	    exit({open_port_failed,O,
		  [{mod,?MODULE}, {uri,ModData#mod.request_uri},
		   {script,Script}, {env,Env}, {dir,Dir}]})
    end;

exec_script(false, ModData, Script, _AfterScript, _RequestURI) ->
    ?vlog("script ~s not executable",[Script]),
    {proceed,
     [{status,
       {404,ModData#mod.request_uri,
	?NICE("You don't have permission to execute " ++
	      ModData#mod.request_uri ++ " on this server")}}|
      ModData#mod.data]}.
    
	   
%%
%% Socket <-> Port communication
%%

cgi_timeout(Db) ->
    httpd_util:lookup(Db, cgi_timeout, ?DEFAULT_CGI_TIMEOUT).

proxy(#mod{config_db = Db} = ModData, Port) ->
    ?vdebug("proxy -> entry with"
	    "~n   ModData:      ~p"
	    "~n   Port ~p", [ModData, Port]),
    
    Timeout = cgi_timeout(Db),    
    case receive_headers(Port, ?MODULE, parse_headers, 
			 [<<>>, [], []], Timeout) of
	{Headers, Body} ->
	    {CGIHeaders, HTTPHeaders} = devide_and_reverse_headers(Headers),
	    case handle_cgi_headers(CGIHeaders, [], {200, "ok"}) of	
		{proceed, AbsPath} ->
		    {proceed, [{real_name, 
				httpd_util:split_path(AbsPath)} | 
			       ModData#mod.data]};
		{ok, ExtraHTTPHeaders, Status} ->
		    IsDisableChunkedSend = 
			httpd_response:is_disable_chunked_send(Db),
		    case (ModData#mod.http_version =/= "HTTP/1.1") or 
			(IsDisableChunkedSend) of
			true ->
			    send_headers(ModData, Status,
					 ExtraHTTPHeaders ++ HTTPHeaders);
			false ->
			    send_headers(ModData, Status,
					 ExtraHTTPHeaders 
					 ++ [{"transfer-encoding",
					      "chunked"} | HTTPHeaders])
		    end,
		    handle_body(Port, ModData, Body, Timeout, size(Body),
				IsDisableChunkedSend);
		Other ->
		    exit(Other)
	    end;
	{'EXIT', Port, Reason} ->
	    process_flag(trap_exit, false),
	    {proceed, [{status, {400, none, reason(Reason)}} |
		       ModData#mod.data]};
	timeout ->
	    (catch port_close(Port)), % KILL the port !!!!
	    send_headers(ModData, {504, "Timeout"}, []),
	    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket),
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, 0}} | ModData#mod.data]}
    end.
	    
receive_headers(Port, Module, Function, Args, Timeout) ->
      receive
	  {Port, {data, Response}} when is_port(Port) ->
	      case Module:Function([Response | Args]) of
		  {NewModule, NewFunction, NewArgs} ->
		      receive_headers(Port, NewModule, 
				      NewFunction, NewArgs, Timeout);
		  {Headers, Body} ->
		      {Headers, Body}
	      end;
	  {'EXIT', Port, Reason} when is_port(Port) ->
	      {'EXIT', Port, Reason};
	  {'EXIT', Pid, Reason} when is_pid(Pid) ->
	      exit({linked_process_died, Pid, Reason})
      after Timeout ->
	      timeout
      end.

%% Parse response headers returned from the cgi script
%% Returns {RevHeaderList, BodyData} | {Module, Function, ArgList} 	
parse_headers([Bin, Data, Header, Headers]) ->
    parse_headers(<<Bin/binary, Data/binary>>, Header, Headers).

parse_headers(<<>>, Header, Headers) ->
    {?MODULE, parse_headers, [<<>>, Header, Headers]};
parse_headers(<<?LF>>, Header, Headers) ->
    {?MODULE, parse_headers, [<<?LF>>, Header, Headers]};
parse_headers(<<?LF, ?LF, Rest/binary>>, Header, Headers) ->
    {[[?LF | Header] | Headers], Rest};
parse_headers(<<?LF, Rest/binary>>, Header, Headers) ->
    {?MODULE, parse_headers, [Rest, [],
			      [[?LF | Header]| Headers]]};
parse_headers(<<Octet, Rest/binary>>, Header, Headers) ->
    parse_headers(Rest, [Octet | Header], Headers).


devide_and_reverse_headers(RevHeaders) ->
    devide_and_reverse_headers(RevHeaders, [], []).

devide_and_reverse_headers([], RevCGI, RevHTTP) ->
    RevFun = fun(Rev) -> lists:reverse(Rev) end,
    RevAndFormat = fun(RevHeader) ->
			   httpd_response:split_header(
			     lists:reverse(RevHeader), [])
		   end,
    {lists:map(RevFun, RevCGI), lists:map(RevAndFormat, RevHTTP)};
devide_and_reverse_headers([RevHeader | RevHeaders], RevCGI, RevHTTP) ->
    case RevHeader of
	[?LF, ?CR | _] ->
	    devide_and_reverse_headers(RevHeaders, RevCGI, 
				       [RevHeader | RevHTTP]);
	[?LF | NewRevHeader] ->
	    devide_and_reverse_headers(RevHeaders, [NewRevHeader | RevCGI], 
				       RevHTTP)
    end.

handle_cgi_headers([], HTTPHeaders, Status) ->
    {ok, HTTPHeaders, Status};

handle_cgi_headers([CGIHeader | CGIHeaders], HTTPHeaders, Status) ->
    
    {FieldName, FieldValue} = httpd_response:split_header(CGIHeader, []),
   
    case FieldName of
	"content-type" ->
	    handle_cgi_headers(CGIHeaders,
			       [{FieldName, FieldValue} | HTTPHeaders], 
			       Status);
	"location" ->
	    case http_request:is_absolut_uri(FieldValue) of
		true ->
		    handle_cgi_headers(CGIHeaders, 
				       [{FieldName, FieldValue} | 
					HTTPHeaders], {302, "Redirect"});
		false ->
		    {proceed, FieldValue}
	    end;
	"status" ->
	    [StatusCode, ReasonPhrase] = string:tokens(FieldValue, " "),
	    handle_cgi_headers(CGIHeaders, HTTPHeaders,
			       {StatusCode, ReasonPhrase});
	_ -> %%Extnsion headers
	    handle_cgi_headers(CGIHeaders,
			       [{FieldName, FieldValue} | HTTPHeaders], Status)
    end.	

send_headers(ModData, {StatusCode, _}, HTTPHeaders) ->
    ExtraHeaders = httpd_response:cache_headers(ModData),
    httpd_response:send_header(ModData, StatusCode, 
			       ExtraHeaders ++ HTTPHeaders).

handle_body(Port, #mod{method = "HEAD"} = ModData, _, _, Size, _) ->
    (catch port_close(Port)), % KILL the port !!!!
    process_flag(trap_exit,false),
    {proceed, [{response, {already_sent, 200, Size}} | ModData#mod.data]};

handle_body(Port, ModData, Body, Timeout, Size, IsDisableChunkedSend) ->
    httpd_response:send_chunk(ModData, Body, IsDisableChunkedSend),
    receive 
	{Port, {data, Data}} when port(Port) ->
	    handle_body(Port, ModData, Data, Timeout, Size + size(Data),
			IsDisableChunkedSend);
	{'EXIT', Port, normal} when is_port(Port) ->
	    ?vtrace("mod_cgi:handle_body -> exit signal from port: normal",[]),
	    httpd_response:send_final_chunk(ModData, IsDisableChunkedSend),
	    process_flag(trap_exit,false),
	    {proceed, [{response, {already_sent, 200, Size}} |
		       ModData#mod.data]};
	{'EXIT', Port, Reason} when is_port(Port) ->
	    process_flag(trap_exit, false),
	    {proceed, [{status, {400, none, reason(Reason)}} | 
		       ModData#mod.data]};
	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    exit({mod_cgi_linked_process_died, Pid, Reason})
    after Timeout ->
	    (catch port_close(Port)), % KILL the port !!!!
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, Size}} |
		      ModData#mod.data]}
    end.

sz(B) when binary(B) -> {binary,size(B)};
sz(L) when list(L)   -> {list,length(L)};
sz(_)                -> undefined.

%% Convert error to printable string
%%
reason({error,emfile})     -> ": To many open files";
reason({error,{enfile,_}}) -> ": File/port table overflow";
reason({error,enomem})     -> ": Not enough memory";
reason({error,eagain})     -> ": No more available OS processes";
reason(_)                  -> "".
