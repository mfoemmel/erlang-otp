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
-export([do/1,env/3,status_code/1,load/2]).

%% Exports to the interface for sending chunked data 
%% to http/1.1 users and full responses to http/1.0
-export([send/5, send/6, 
	 final_send/4, final_send/5, 
	 update_status_code/2, get_new_size/2]).

-include("httpd.hrl").

-define(VMODULE,"CGI").
-include("httpd_verbosity.hrl").

-define(GATEWAY_INTERFACE,"CGI/1.1").
-define(DEFAULT_CGI_TIMEOUT,15000).

%% do

do(Info) ->
    ?vtrace("do",[]),
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    ?vtrace("do -> no status code has been generated", []),
	    case httpd_util:key1search(Info#mod.data,response) of
		%% No response has been generated!
		undefined ->
		    ?vtrace("do -> no response has been generated", []),
		    RequestURI =
			case httpd_util:key1search(Info#mod.data,
						   new_request_uri) of
			    undefined ->
				Info#mod.request_uri;
			    Value ->
				Value
			end,
		    ?vtrace("do -> RequestURI: ~p", [RequestURI]),
		    ScriptAliases =
			httpd_util:multi_lookup(Info#mod.config_db,
						script_alias),
		    ?vtrace("do -> ScriptAliases: ~p", [ScriptAliases]),
		    case mod_alias:real_script_name(Info#mod.config_db,
						    RequestURI,
						    ScriptAliases) of
			{Script, AfterScript} ->
			    exec_script(Info, Script, AfterScript, RequestURI);
			not_a_script ->
			    {proceed,Info#mod.data}
		    end;
		%% A response has been generated or sent!
		_Response ->
		    {proceed,Info#mod.data}
	    end
    end.


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
    case to_lower(string:right(S,length(E))) of
	E ->
	    true;
	_ ->
	    false
    end;
ends_with1(_S,_E) ->
    false.
    

to_lower(S)       -> to_lower(S,[]).

to_lower([],L)    -> lists:reverse(L);
to_lower([H|T],L) -> to_lower(T,[to_lower1(H)|L]).

to_lower1(C) when C >= $A, C =< $Z ->
    C + ($a - $A);
to_lower1(C) ->
    C.

%%
%% End fix
%% ---------------------------------
    

env(VarName, Value) ->
    {VarName, Value}.

env(Info, Script, AfterScript) ->
    ?vtrace("env -> entry with"
	    "~n   Script:      ~p"
	    "~n   AfterScript: ~p",
	    [Script, AfterScript]),
    {_, RemoteAddr} = (Info#mod.init_data)#init_data.peername,
    ServerName = (Info#mod.init_data)#init_data.resolve,
    PH = parsed_header(Info#mod.parsed_header),
    Env =
	[env("SERVER_SOFTWARE",?SERVER_SOFTWARE),
	 env("SERVER_NAME",ServerName),
	 env("GATEWAY_INTERFACE",?GATEWAY_INTERFACE),
	 env("SERVER_PROTOCOL",?SERVER_PROTOCOL),
	 env("SERVER_PORT", 
	     integer_to_list(httpd_util:lookup(Info#mod.config_db,port,80))),
	 env("REQUEST_METHOD",Info#mod.method),
	 env("REMOTE_ADDR",RemoteAddr),
	 env("SCRIPT_NAME",Script)],
    Env1 =
        case Info#mod.method of
            "GET" ->
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
    Env2 =
        case httpd_util:key1search(Info#mod.data,remote_user) of
            undefined ->
                Env1;
            RemoteUser ->
                [env("REMOTE_USER",RemoteUser)|Env1] %% OTP-4416
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


exec_script(Info, Script, AfterScript, RequestURI) ->
    ?vdebug("exec_script -> entry with"
	    "~n   Script:      ~p"
	    "~n   AfterScript: ~p",
	    [Script,AfterScript]),
    exec_script(is_executable(Script),Info,Script,AfterScript,RequestURI).

exec_script(true, Info, Script, AfterScript, RequestURI) ->
    ?vtrace("exec_script -> entry when script is executable",[]),
    process_flag(trap_exit,true),
    Dir  = filename:dirname(Script),
    [Script_Name|_] = string:tokens(RequestURI, "?"),
    Env  = env(Info, Script_Name, AfterScript),
    Port = (catch open_port({spawn,Script},[stream,{cd, Dir},{env, Env}])),
    ?vtrace("exec_script -> Port: ~w",[Port]),
    case Port of
	P when port(P) ->
	    %% Send entity_body to port.
	    Res = case Info#mod.entity_body of
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
			  [Port,Info#mod.request_uri,Reason]),
		    exit({open_cmd_failed,Reason,
			  [{mod,?MODULE},{port,Port},
			   {uri,Info#mod.request_uri},
			   {script,Script},{env,Env},{dir,Dir},
			   {ebody_size,sz(Info#mod.entity_body)}]});
		 true ->
		    proxy(Info, Port)
	    end;
	{'EXIT',Reason} ->
	    ?vlog("open port failed: exit"
		  "~n   URI:    ~p"
		  "~n   Reason: ~p",
		  [Info#mod.request_uri,Reason]),
	    exit({open_port_failed,Reason,
		  [{mod,?MODULE},{uri,Info#mod.request_uri},{script,Script},
		   {env,Env},{dir,Dir}]});
	O ->
	    ?vlog("open port failed: unknown result"
		  "~n   URI: ~p"
		  "~n   O:   ~p",
		  [Info#mod.request_uri,O]),
	    exit({open_port_failed,O,
		  [{mod,?MODULE},{uri,Info#mod.request_uri},{script,Script},
		   {env,Env},{dir,Dir}]})
    end;

exec_script(false,Info,Script,_AfterScript,_RequestURI) ->
    ?vlog("script ~s not executable",[Script]),
    {proceed,
     [{status,
       {404,Info#mod.request_uri,
	?NICE("You don't have permission to execute " ++
	      Info#mod.request_uri ++ " on this server")}}|
      Info#mod.data]}.
    
	    

%%
%% Socket <-> Port communication
%%

disable_chunked_send(Db) ->
    httpd_util:lookup(Db, disable_chunked_transfer_encoding_send, false).
cgi_timeout(Db) ->
    httpd_util:lookup(Db, cgi_timeout, ?DEFAULT_CGI_TIMEOUT).

proxy(#mod{config_db = Db} = Info, Port) ->
    ?vtrace("proxy -> entry with~n   Port: ~p", [Port]),
    Timeout            = cgi_timeout(Db),
    DisableChunkedSend = disable_chunked_send(Db),
    ?vtrace("proxy -> "
	"~n   Timeout:            ~p"
	"~n   DisableChunkedSend: ~p", [Timeout, DisableChunkedSend]),
    proxy(Info, Port, 0, undefined, [], Timeout, DisableChunkedSend).

proxy(Info, Port, Size, StatusCode, AccResponse, 
      Timeout, DisableChunkedSend) ->
    ?vdebug("proxy -> entry with"
	    "~n   Size:      ~p"
	    "~n   StatusCode ~p", [Size, StatusCode]),
    receive
	{Port, {data, Response}} when port(Port) ->
	    ?vtrace("proxy -> got some data from the port",[]),
	    
	    NewStatusCode = update_status_code(StatusCode, Response),
				  
	    ?vtrace("proxy -> NewStatusCode: ~p",[NewStatusCode]),
	    case send(Info, NewStatusCode, Response, Size, 
		      AccResponse, DisableChunkedSend) of
		socket_closed ->
		    ?vtrace("proxy -> socket closed: kill port",[]),
		    (catch port_close(Port)), % KILL the port !!!!
		    process_flag(trap_exit,false),
		    {proceed,
		     [{response,{already_sent,200,Size}}|Info#mod.data]};

		head_sent ->
		    ?vtrace("proxy -> head sent: kill port",[]),
		    (catch port_close(Port)), % KILL the port !!!!
		    process_flag(trap_exit,false),
		    {proceed,
		     [{response,{already_sent,200,Size}}|Info#mod.data]};

		_ ->
		    ?vtrace("proxy -> continue",[]),
		    %% The data is sent and the socket is not closed, continue
		    NewSize = get_new_size(Size, Response),
		    proxy(Info, Port, NewSize, NewStatusCode,
			  "nonempty", Timeout, DisableChunkedSend)
	    end;

	{'EXIT', Port, normal} when port(Port) ->
	    ?vtrace("proxy -> exit signal from port: normal",[]),
	    NewStatusCode = update_status_code(StatusCode,AccResponse),
	    final_send(Info,NewStatusCode,Size,AccResponse),
	    process_flag(trap_exit,false),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]};

	{'EXIT', Port, Reason} when port(Port) ->
	    ?vtrace("proxy -> exit signal from port: ~p",[Reason]),
	    process_flag(trap_exit, false),
	    {proceed, [{status,{400,none,reason(Reason)}}|Info#mod.data]};

	{'EXIT', Pid, Reason} when pid(Pid) ->
	    %% This is the case that a linked process has died, 
	    %% It would be nice to response with a server error 
	    %% but since the head alredy is sent...
	    ?vtrace("proxy -> exit signal from ~p: ~p",[Pid, Reason]),
	    proxy(Info, Port, Size, StatusCode, AccResponse, 
		  Timeout, DisableChunkedSend);

	%% This should not happen
	WhatEver ->
	    ?vinfo("proxy -> received garbage: ~n~p", [WhatEver]),
	    NewStatusCode = update_status_code(StatusCode, AccResponse),
	    final_send(Info, NewStatusCode, Size, AccResponse),
	    process_flag(trap_exit, false),
	    {proceed, [{response,{already_sent,200,Size}}|Info#mod.data]}

    after Timeout ->
	    ?vlog("proxy -> timeout",[]),
	    (catch port_close(Port)), % KILL the port !!!!
	    httpd_socket:close(Info#mod.socket_type, Info#mod.socket),
	    process_flag(trap_exit,false),
	    {proceed,[{response,{already_sent,200,Size}}|Info#mod.data]}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The functions that handles the sending of the data to the client   %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% Send the header the first time the size of the body is Zero
%%----------------------------------------------------------------------
send(#mod{config_db = Db} = Info, StatusCode, Response, Size, AccResponse) ->
    DisableChunkedSend = disable_chunked_send(Db),
    send(Info, StatusCode, Response, Size, AccResponse, DisableChunkedSend).

send(#mod{method = "HEAD"} = Info, StatusCode, Response, 0, [], 
     DisableChunkedSend) ->
    send_head_first(Info, StatusCode, Response, DisableChunkedSend),
    head_sent;
send(Info, StatusCode, Response, 0, [], DisableChunkedSend) ->
    send_chunk_first(Info, StatusCode, Response, DisableChunkedSend);

%%----------------------------------------------------------------------
%% The size of the body is bigger than zero => 
%% we have a part of the body to send
%%----------------------------------------------------------------------
send(Info, StatusCode, Response, _Size, _AccResponse, DisableChunkedSend) ->
    send_chunk(Info, StatusCode, Response, DisableChunkedSend).


%%----------------------------------------------------------------------
%% The function is called the last time when the port has closed 
%%----------------------------------------------------------------------

final_send(#mod{config_db = Db} = Info, StatusCode, Size, AccResponse) ->
    DisableChunkedSend = disable_chunked_send(Db),
    final_send(Info, StatusCode, Size, AccResponse, DisableChunkedSend).

final_send(Info, StatusCode, _Size, _AccResponse, DisableChunkedSend) ->
    send_final_chunk(Info, StatusCode, DisableChunkedSend).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %% 
%% The code that handles the head requests                            %%
%%                                                                    %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%----------------------------------------------------------------------
%% The request is a head request if its a HTPT/1.1 request answer to it
%% otherwise we must collect the size of hte body before we can answer.
%% Return Values:
%% head_sent
%%----------------------------------------------------------------------
send_head_first(#mod{http_version = "HTTP/1.1", 
		     socket_type  = Type, 
		     socket       = Sock} = Info,
		StatusCode, Response, DisableChunkedSend) ->
    ?vtrace("send_head_first(HTTP/1.1) -> entry with"
	"~n   DisableChunkedSend: ~p",[DisableChunkedSend]),
    %% Since we have all we need to create the header: 
    %% create and send it
    case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
	{ok, [HeadEnd, _Rest]} ->
	    Resp = [create_header(Info, StatusCode, DisableChunkedSend),
		    removeStatus(HeadEnd),
		    "\r\n\r\n"],
	    httpd_socket:deliver(Type, Sock, Resp);
	_ ->
	    Resp = [create_header(Info, StatusCode, DisableChunkedSend),
		    "Content-Type:text/html\r\n\r\n"],
	    httpd_socket:deliver(Type, Sock, Resp)

    end;
send_head_first(#mod{socket_type = Type, socket = Sock} = Info,
		StatusCode, Response, _) ->
    ?vtrace("send_head_first -> entry", []),
    Response1 = case regexp:split(Response,"\r\n\r\n|\n\n") of
		    {ok,[HeadEnd|_Rest]} ->
			removeStatus(HeadEnd);
		    _ ->
			["Content-Type:text/html"]
		end,
    H1 = httpd_util:header(StatusCode,Info#mod.connection),
    httpd_socket:deliver(Type, Sock, [H1,Response1,"\r\n\r\n"]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Handle the requests that is to the other methods                   %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%----------------------------------------------------------------------
%% Create the http-response header and send it to the user if it is
%% a http/1.1 request otherwise we must accumulate it 
%%----------------------------------------------------------------------

send_chunk_first(#mod{http_version = "HTTP/1.1", 
		      socket_type  = Type, 
		      socket       = Sock} = Info,
		 StatusCode, Response, DisableChunkedSend) ->
    ?vtrace("send_chunk_first(HTTP/1.1) -> entry with"
	"~n   DisableChunkedSend: ~p",[DisableChunkedSend]),
    Header = create_header(Info, StatusCode, DisableChunkedSend),
    Response1 = 
	case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
	    {ok,[HeadPart,[]]} ->
		?vtrace("send_chunk_first -> only head", []),
		[Header, removeStatus(HeadPart),"\r\n\r\n"];

	    {ok,[HeadPart,BodyPart]} ->
		?vtrace("send_chunk_first -> head and body", []),
		[Header, removeStatus(HeadPart), "\r\n\r\n",
		 create_first_chunk(BodyPart, DisableChunkedSend)];

	    _WhatEver ->
		%% No response header field from the cgi-script, 
		%% Just a body
		?vtrace("send_chunk_first -> only body", []),
		[Header, "Content-Type:text/html","\r\n\r\n",
		 create_first_chunk(Response, DisableChunkedSend)]
	end,
    httpd_socket:deliver(Type, Sock, Response1);
% send_chunk_first(#mod{http_version = "HTTP/1.1", 
% 		      socket_type  = Type, 
% 		      socket       = Sock} = Info,
% 		 StatusCode, Response, false) ->
%     ?vtrace("send_chunk_first(HTTP/1.1,false) -> entry",[]),
%     Header = create_header(Info, StatusCode, false),
%     Response1 = 
% 	case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
% 	    {ok,[HeadPart,[]]} ->
% 		[Header, removeStatus(HeadPart),"\r\n\r\n"];

% 	    {ok,[HeadPart,BodyPart]} ->
% 		[Header, removeStatus(HeadPart), "\r\n\r\n",
% 		 httpd_util:integer_to_hexlist(length(BodyPart)),
% 		 "\r\n", BodyPart];

% 	    _WhatEver ->
% 		%% No response header field from the cgi-script, 
% 		%% Just a body
% 		[Header, "Content-Type:text/html","\r\n\r\n",
% 		 httpd_util:integer_to_hexlist(length(Response)),
% 		 "\r\n", Response]
% 	end,
%     httpd_socket:deliver(Type, Sock, Response1);
% send_chunk_first(#mod{http_version = "HTTP/1.1", 
% 		      socket_type  = Type, 
% 		      socket       = Sock} = Info,
% 		 StatusCode, Response, true) ->
%     ?vtrace("send_chunk_first(HTTP/1.1,true) -> entry",[]),
%     Header = create_header(Info, StatusCode, true),
%     Response1 = 
% 	case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
% 	    {ok,[HeadPart,[]]} ->
% 		[Header, removeStatus(HeadPart),"\r\n\r\n"];

% 	    {ok,[HeadPart,BodyPart]} ->
% 		[Header, removeStatus(HeadPart), "\r\n\r\n", BodyPart];

% 	    _WhatEver ->
% 		%% No response header field from the cgi-script, 
% 		%% Just a body
% 		[Header, "Content-Type:text/html","\r\n\r\n", Response]
% 	end,
%     httpd_socket:deliver(Type, Sock, Response1);
send_chunk_first(#mod{socket_type = Type, socket = Sock} = Info,
		 StatusCode, Response, DisableChunkedSend) ->
    ?vtrace("send_chunk_first -> entry",[]),
    Header = create_header(Info, StatusCode, DisableChunkedSend),
    httpd_socket:deliver(Type, Sock, [Header, Response]).


send_chunk(#mod{http_version = "HTTP/1.1", 
		socket_type = Type, socket = Sock} = Info, 
	   _StatusCode, Response0, false) ->
    ?vtrace("send_chunk(HTTP/1.1,false) -> entry with"
	"~n   length(Response0): ~p", [length(Response0)]),
    Response = create_chunk(Info, Response0),
    httpd_socket:deliver(Type, Sock, Response);
send_chunk(#mod{socket_type = Type, socket = Sock} = _Info, 
	   _StatusCode, Response, _) ->
    ?vtrace("send_chunk -> entry with"
	"~n   length(Response): ~p", [length(Response)]),
    httpd_socket:deliver(Type, Sock, Response).


send_final_chunk(#mod{http_version = "HTTP/1.1", 
		      socket_type = Type, socket = Sock}, 
		 _StatusCode, false) ->
    ?vtrace("send_final_chunk(HTTP/1.1,false) -> entry", []),
    httpd_socket:deliver(Type, Sock, "0\r\n");
send_final_chunk(#mod{socket_type = Type, socket = Sock}, 
		 _StatusCode, _) ->
    ?vtrace("send_final_chunk -> entry", []),
    httpd_socket:close(Type, Sock),
    socket_closed.


create_first_chunk(Response, false) ->
    ?vtrace("create_first_chunk -> chunked", []),
    HEXSize = httpd_util:integer_to_hexlist(length(lists:flatten(Response))),
    HEXSize++"\r\n"++Response;
create_first_chunk(Response, true) ->
    ?vtrace("create_first_chunk -> not chunked", []),
    Response.


create_chunk(_Info, Response) ->
    ?vtrace("create_chunk -> entry with"
	"~n   length(Response): ~p", [length(Response)]),
    HEXSize = httpd_util:integer_to_hexlist(length(lists:flatten(Response))),
    HEXSize++"\r\n"++Response++"\r\n".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% The various helper functions                                       %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
update_status_code(undefined, Response) ->
    case status_code(Response) of
	{ok, StatusCode1} ->
	    StatusCode1;
	_ ->	    
	    ?vlog("invalid response from script:~n~p", [Response]),
	    500
    end;
update_status_code(StatusCode,_Response)->
    StatusCode.


get_new_size(0,Response)->
    case httpd_util:split(Response,"\r\n\r\n|\n\n",2) of
	{ok,[_Head,Body]}->
	    length(lists:flatten(Body));
	_ ->
	    %%No header in the respone 
	    length(lists:flatten(Response))
    end;

get_new_size(Size,Response)->
    Size+length(lists:flatten(Response)).

%%----------------------------------------------------------------------
%% Creates the http-header for a response
%%----------------------------------------------------------------------
create_header(#mod{http_version = Version,
		   config_db    = DB,
		   connection   = Connection},
	      StatusCode, DisableChunkedSend) ->
    Cache = case httpd_util:lookup(DB,script_nocache,false) of
		true ->
		    Date = httpd_util:rfc1123_date(),
		    "Cache-Control:no-cache\r\n"
			"Pragma:no-cache\r\n"
			"Expires:" ++ Date ++ "\r\n";
		false ->
		    []
	    end,
    create_header(Version, Connection, Cache, StatusCode, DisableChunkedSend).

create_header("HTTP/1.1", Connection, Cache, StatusCode, false) ->
    Header = httpd_util:header(StatusCode, Connection),
    Header ++ "Transfer-encoding:chunked\r\n" ++ Cache;
create_header(_, Connection, Cache, StatusCode, _) ->
    httpd_util:header(StatusCode, Connection) ++ Cache.


%% status_code

status_code(Response) ->
  case httpd_util:split(Response,"\n\n|\r\n\r\n",2) of
    {ok,[Header,_Body]} ->
      case regexp:split(Header,"\n|\r\n") of
	{ok,HeaderFields} ->
	  {ok,extract_status_code(HeaderFields)};
	{error,_} ->
	      {error, bad_script_output(Response)}
      end;
    _ ->
	  %% No header field in the returned data return 200 the standard code
	  {ok, 200}
  end.

bad_script_output(Bad) ->
    lists:flatten(io_lib:format("Bad script output ~s",[Bad])).


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

removeStatus(Head)->
    case httpd_util:split(Head,"Status:.\r\n",2) of
	{ok,[HeadPart,HeadEnd]}->
	    HeadPart++HeadEnd;
	_ ->
	    Head
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% There are 2 config directives for mod_cgi:                         %%
%% ScriptNoCache true|false, defines whether the server shall add     %%
%%                           header fields to stop proxies and        %%  
%%                           clients from saving the page in history  %%
%%                           or cache                                 %%
%%                                                                    %%
%% ScriptTimeout Seconds, The number of seconds that the server       %%
%%                        maximum will wait for the script to         %%
%%                        generate a part of the document             %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load([$S,$c,$r,$i,$p,$t,$N,$o,$C,$a,$c,$h,$e |CacheArg],[])->
    case catch list_to_atom(httpd_conf:clean(CacheArg)) of
        true ->
	    {ok, [], {script_nocache,true}};
	false ->
	   {ok, [], {script_nocache,false}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(CacheArg)++
			 " is an invalid ScriptNoCache directive")}
    end;

load([$S,$c,$r,$i,$p,$t,$T,$i,$m,$e,$o,$u,$t,$ |Timeout],[])->
    case catch list_to_integer(httpd_conf:clean(Timeout)) of
	TimeoutSec when integer(TimeoutSec)  ->
	   {ok, [], {script_timeout,TimeoutSec*1000}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(Timeout)++
			 " is an invalid ScriptTimeout")}
    end.
	
	
