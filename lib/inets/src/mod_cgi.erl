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
-module(mod_cgi).
-export([do/1,env/3,status_code/1]).

-include("httpd.hrl").

-define(GATEWAY_INTERFACE,"CGI/1.1").

%% do

do(Info) ->
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		%% No response has been generated!
		undefined ->
		    RequestURI=
			case httpd_util:key1search(Info#mod.data,new_request_uri) of
			    undefined ->
				Info#mod.request_uri;
			    Value ->
				Value
			end,
		    ScriptAliases=
			httpd_util:multi_lookup(Info#mod.config_db,script_alias),
		    case mod_alias:real_script_name(Info#mod.config_db,
						    RequestURI,
						    ScriptAliases) of
			{Script, AfterScript} ->
			    case is_executable(Script) of
				true ->
				    Dir = filename:dirname(Script),
				    [Script_Name|_] = string:tokens(RequestURI, "?"),
				    Env = env(Info, Script_Name, AfterScript),
				    Port = (catch open_port({spawn, Script}, [stream,
									      {cd, Dir},
									      {env, Env}])),
				    case Port of
					P when port(P) ->
					    %% Send entity_body to port.
					    if
						Info#mod.entity_body == [] ->
						    no_entity_body;
						true ->
						    Port ! {self(),
							    {command,
							     Info#mod.entity_body}}
					    end,
					    proxy(Info,Port);
					_ ->
					    {proceed,[{status,{404,Info#mod.request_uri,
							       ?NICE("You don't have permission to execute "++Info#mod.request_uri++" on this server")}}|
						      Info#mod.data]}
				    end;
				false ->
				    {proceed,[{status,{404,Info#mod.request_uri,
						       ?NICE("You don't have permission to execute "++Info#mod.request_uri++" on this server")}}|
					      Info#mod.data]}
			    end;
			not_a_script ->
			    {proceed,Info#mod.data}
		    end;
		%% A response has been generated or sent!
		Response ->
		    {proceed,Info#mod.data}
	    end
    end.


is_executable(File) ->
    Dir = filename:dirname(File),
    FileName = filename:basename(File),
    case os:find_executable(FileName, Dir) of
	false ->
	    false;
	_ ->
	    true
    end.

env(VarName, Value) ->
    {VarName, Value}.

env(Info, Script, AfterScript) ->
    {_, RemoteAddr} = (Info#mod.init_data)#init_data.peername,
    ServerName = (Info#mod.init_data)#init_data.resolve,
    PH = parsed_header(Info#mod.parsed_header),
    Env=
	[env("SERVER_SOFTWARE",?SERVER_SOFTWARE),
	 env("SERVER_NAME",ServerName),
	 env("GATEWAY_INTERFACE",?GATEWAY_INTERFACE),
	 env("SERVER_PROTOCOL",?SERVER_PROTOCOL),
	 env("SERVER_PORT", integer_to_list(httpd_util:lookup(Info#mod.config_db,port,80))),
	 env("REQUEST_METHOD",Info#mod.method),
	 env("REMOTE_ADDR",RemoteAddr),
	 env("SCRIPT_NAME",Script)],
    Env1=
        case Info#mod.method of
            "GET" ->
		?DEBUG("AfterScript: ~p~n", [AfterScript]),
                case AfterScript of
		    {[], QueryString} ->
                        [env("QUERY_STRING", QueryString)|Env];
		    {PathInfo, []} ->
                        Aliases = httpd_util:multi_lookup(
				    Info#mod.config_db,alias),
                        {_, PathTranslated, _} =
                            mod_alias:real_name(
                              Info#mod.config_db, PathInfo, Aliases),
                        [Env|
                         [env("PATH_INFO","/"++httpd_util:decode_hex(PathInfo)),
                          env("PATH_TRANSLATED",PathTranslated)]];
		    {PathInfo, QueryString} ->
                        Aliases = httpd_util:multi_lookup(
				    Info#mod.config_db,alias),
                        {_, PathTranslated, _} =
                            mod_alias:real_name(
                              Info#mod.config_db, PathInfo, Aliases),
                        [Env|
                         [env("PATH_INFO",
			      httpd_util:decode_hex(PathInfo)),
                          env("PATH_TRANSLATED",PathTranslated),
			  env("QUERY_STRING", QueryString)]];
		    [] ->
			Env
                end;
            "POST" ->
                [env("CONTENT_LENGTH",
                     integer_to_list(httpd_util:flatlength(
                                       Info#mod.entity_body)))|Env];
            _ ->
                Env
        end,
    Env2=
        case httpd_util:key1search(Info#mod.data,remote_user) of
            undefined ->
                Env1;
            RemoteUser ->
                [Env1|env("REMOTE_USER",RemoteUser)]
        end,
    lists:flatten([Env2|PH]).


parsed_header(List) ->
    parsed_header(List, []).
 
parsed_header([], SoFar) ->
    SoFar;
parsed_header([{Name,[Value|R1]}|R2], SoFar) when list(Value)->
    NewName=lists:map(fun(X) -> if X == $- -> $_; true -> X end end,Name),
    Env = env("HTTP_"++httpd_util:to_upper(NewName),
	      multi_value([Value|R1])),
    parsed_header(R2, [Env|SoFar]);
 
parsed_header([{Name,Value}|Rest], SoFar) ->
    {ok,NewName,_} = regexp:gsub(Name, "-", "_"),
    Env=env("HTTP_"++httpd_util:to_upper(NewName),Value),
    parsed_header(Rest, [Env|SoFar]).
 

multi_value([]) ->
  [];
multi_value([Value]) ->
  Value;
multi_value([Value|Rest]) ->
  Value++", "++multi_value(Rest).

%%
%% Socket <-> Port communication
%%

proxy(Info,Port) ->
  process_flag(trap_exit,true),
  proxy(Info,Port,Info#mod.socket,0, undefined).

proxy(Info,Port,Socket,Size, StatusCode) ->
    receive
	{tcp_closed, Socket} ->
	    Port ! {self(),close},
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
	{tcp_error, Socket, Reason} ->
	    Port ! {self(),close},
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
	{ssl_closed, Socket} ->
	    Port ! {self(),close},
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
	{ssl_error, Socket, Reason} ->
	    Port ! {self(),close},
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]};
	{Port, {data, Response}} when port(Port) ->
	    NewStatusCode =
		case StatusCode of 
		    undefined ->
			case status_code(Response) of
			    {ok,StatusCode1} ->
				StatusCode1;
			    {error, Reason} ->
				StatusCode
			end;
		    StatusCode1 ->
			StatusCode1
		end,
	    case send(Info#mod.socket_type,
		      Socket, NewStatusCode, 
		      Response, Size) of
		socket_closed ->
		    Port ! {self(), close}, % KILL the port !!!!
		    process_flag(trap_exit,false),
		    {proceed,
		     [{response,
		       {already_sent,200,Size}}|Info#mod.data]};
		_ ->
		    NewSize = Size+length(Response),
		    proxy(Info,Port,Socket,NewSize,NewStatusCode)
	    end;
	{'EXIT', Port, normal} when port(Port) ->
	    process_flag(trap_exit,false),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]};
	{'EXIT', Port, Reason} when port(Port) ->
	    process_flag(trap_exit,false),
	    {proceed, [{status,{400,none,Reason}}|Info#mod.data]};
	{'EXIT', Pid, Reason} when pid(Pid) ->
	    process_flag(trap_exit,false),
	    {'EXIT', Pid, Reason};
	%% This should not happen!
	WhatEver ->
	    process_flag(trap_exit,false),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]}
    end.

%% send

send(SocketType, Socket, StatusCode, Response, 0) ->
    Header = httpd_util:header(StatusCode),
    httpd_socket:deliver(SocketType,Socket,[Header, Response]);
send(SocketType, Socket, StatusCode, Response, Size) ->
    httpd_socket:deliver(SocketType,Socket, Response).

%% status_code

status_code(Response) ->
  case httpd_util:split(Response,"\n\n|\r\n\r\n",2) of
    {ok,[Header,Body]} ->
      case regexp:split(Header,"\n|\r\n") of
	{ok,HeaderFields} ->
	  {ok,extract_status_code(HeaderFields)};
	{ok,_} ->
	  {error,lists:flatten(io_lib:format("Bad script output ~s",
					     [Response]))}
      end;
    {ok,[Header]} ->
      case regexp:split(Header,"\n|\r\n") of
	{ok,HeaderFields} ->
	  {ok,extract_status_code(HeaderFields)};
	{ok,_} ->
	  {error,lists:flatten(io_lib:format("Bad script output ~s",
					     [Response]))}
      end;
    {ok,BadScriptOutput} ->
      %%io:format("--> ~p~n",[BadScriptOutput]),
      {error,lists:flatten(io_lib:format("Bad script output ~s",[Response]))}
  end.

extract_status_code([]) ->
  200;
extract_status_code([[$L,$o,$c,$a,$t,$i,$o,$n,$:,$ |_]|_]) ->
  302;
extract_status_code([[$S,$t,$a,$t,$u,$s,$:,$ |CodeAndReason]|_]) ->
  case httpd_util:split(CodeAndReason," ",2) of
    {ok,[Code,_]} ->
      list_to_integer(Code);
    {ok,_} ->
      200
  end;
extract_status_code([_|Rest]) ->
  extract_status_code(Rest).
