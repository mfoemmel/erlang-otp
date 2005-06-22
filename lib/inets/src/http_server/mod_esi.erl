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
-module(mod_esi).

-export([do/1, load/2]).

-export([parse_headers/1]).

%% Functions provided to help erl scheme alias programmer to 
%% Create dynamic webpages that are sent back to the user during 
%% Generation
-export([deliver/2]).

-include("httpd.hrl").
-include("http.hrl").

-define(VMODULE,"ESI").
-include("httpd_verbosity.hrl").

-define(GATEWAY_INTERFACE,"CGI/1.1").
-define(DEFAULT_ERL_TIMEOUT,15000).


%% do

do(ModData) ->
    ?vtrace("do",[]),
    case httpd_util:key1search(ModData#mod.data, status) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, ModData#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(ModData#mod.data, response) of
		%% No response has been generated!
		undefined ->
		    case erl_or_eval(ModData#mod.request_uri,
				     ModData#mod.config_db) of
			{eval,CGIBody, Modules} ->
			    eval(ModData, ModData#mod.method,CGIBody,Modules);
			{erl,CGIBody, Modules} ->
			    erl(ModData, ModData#mod.method,CGIBody,Modules);
			proceed ->
			    {proceed, ModData#mod.data}
		    end;
		%% A response has been generated or sent!
		_Response ->
		    {proceed, ModData#mod.data}
	    end
    end.

	    

%% erl_or_eval

erl_or_eval(RequestURI, ConfigDB) ->
    case erlp(RequestURI, ConfigDB) of
	false ->
	    case evalp(RequestURI, ConfigDB) of
		false ->
		    ?vtrace("neither erl nor eval",[]),
		    proceed;
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

erlp(RequestURI, ConfigDB) ->
    case httpd_util:multi_lookup(ConfigDB, erl_script_alias) of
	[] ->
	    false;
	AliasMods ->
	    erlp_find_alias(RequestURI, AliasMods)
    end.

erlp_find_alias(_RequestURI,[]) ->
    ?vtrace("erlp_find_alias -> no match",[]),
    false;
erlp_find_alias(RequestURI, [{Alias,Modules} | Rest]) ->
    case regexp:first_match(RequestURI, "^"++Alias++"/") of
	{match,1,Length} ->
	    ?vtrace("erlp -> match with Length: ~p", [Length]),
	    {erl,string:substr(RequestURI,Length+1), Modules};
	nomatch ->
	    erlp_find_alias(RequestURI, Rest)
    end.

evalp(RequestURI, ConfigDB) ->
    case httpd_util:multi_lookup(ConfigDB, eval_script_alias) of
	[] ->
	    false;
	AliasMods ->
	    evalp_find_alias(RequestURI, AliasMods)
    end.

evalp_find_alias(_RequestURI,[]) ->
    ?vtrace("evalp_find_alias -> no match",[]),
    false;
evalp_find_alias(RequestURI, [{Alias, Modules} | Rest]) ->
    case regexp:first_match(RequestURI, "^"++Alias++"\\?") of
	{match, 1, Length} ->
	    ?vtrace("evalp_find_alias -> match with Length: ~p",[Length]),
	    {eval, string:substr(RequestURI, Length + 1), Modules};
	nomatch ->
	    evalp_find_alias(RequestURI, Rest)
    end.


%%
%% Erl mechanism
%% 

%% This is exactly the same as the GET method the difference is that
%% The response must not contain any data expect the response header


erl(ModData,"HEAD",CGIBody,Modules) ->
    erl(ModData,"GET",CGIBody,Modules);	
		
erl(ModData,"GET",CGIBody,Modules) ->
    ?vtrace("erl GET request",[]),
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok, [Mod,FuncAndInput]} ->
	    ?vtrace("~n   Mod:          ~p"
		    "~n   FuncAndInput: ~p",[Mod,FuncAndInput]),
	    case httpd_util:split(FuncAndInput,"[\?/]",2) of
		{ok, [Func,Input]} ->
		    ?vtrace("~n   Func:  ~p"
			    "~n   Input: ~p",[Func,Input]),
		    exec(ModData,"GET",CGIBody,Modules,Mod,Func,
			 {input_type(FuncAndInput),Input});
		{ok, [Func]} ->
		    exec(ModData,"GET",CGIBody,Modules,Mod,Func,{no_input,""});
		{ok, BadRequest} ->
		    {proceed,[{status,{400,none,BadRequest}}|ModData#mod.data]}
	    end;
	{ok, BadRequest} ->
	    ?vlog("erl BAD (GET-) request",[]),
	    {proceed, [{status,{400,none,BadRequest}}|ModData#mod.data]}
    end;

erl(ModData, "POST", CGIBody, Modules) ->
    ?vtrace("erl POST request",[]),
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok,[Mod,Func]} ->
	    ?vtrace("~n   Mod:  ~p"
		    "~n   Func: ~p",[Mod, Func]),
	    exec(ModData, "POST", CGIBody, Modules, Mod, Func,
		 {entity_body,  ModData#mod.entity_body});
	{ok, BadRequest} ->
	    ?vlog("erl BAD (POST-) request",[]),
	    {proceed,[{status, {400, none, BadRequest}} | ModData#mod.data]}
    end.

input_type([]) ->
    no_input;
input_type([$/|_Rest]) ->
    path_info;
input_type([$?|_Rest]) ->
    query_string;
input_type([_First|Rest]) ->
    input_type(Rest).


%% exec

exec(ModData, Method, CGIBody, ["all"], Mod, Func, {Type,  Input}) ->
    ?vtrace("exec ~s 'all'",[Method]),
    exec(ModData, Method, CGIBody, [Mod], Mod, Func, {Type, Input});
exec(ModData, Method, CGIBody, Modules, Mod, Func, {Type, Input}) ->
    ?vtrace("exec ~s request with:"
	    "~n   Modules: ~p"
	    "~n   Mod:     ~p"
	    "~n   Func:    ~p"
	    "~n   Type:    ~p"
	    "~n   Input:   ~p",
	    [Method,Modules,Mod,Func,Type,Input]),
    case lists:member(Mod,Modules) of
	true ->
	    {_,RemoteAddr} = (ModData#mod.init_data)#init_data.peername,
	    ServerName     = (ModData#mod.init_data)#init_data.resolve,
	    Env = get_environment(ModData, ServerName, Method, 
				  RemoteAddr, Type, Input),
	    ?vtrace("and now call the module",[]),
	    case try_new_erl_scheme_method(ModData, Env, Input, 
					   list_to_atom(Mod),
					   list_to_atom(Func)) of
		{error, not_new_method} ->
		    ?vtrace("new method failed, so try old method",[]),
		    Module = list_to_atom(Mod),
		    Function = list_to_atom(Func),
		    case catch Module:Function(Env,Input) of
			{'EXIT',Reason} ->
			    ?vlog("old method failed, exit with Reason: ~p",
				[Reason]),
			    {proceed, [{status, {500,none,Reason}} |
				       ModData#mod.data]};
			Response ->
			    control_response_header(ModData,Mod,Func,Response)
		    end;
		ResponseResult ->
		    ResponseResult
	    end;
	false ->
	    ?vlog("unknown module",[]),
	    {proceed,[{status,{403,ModData#mod.request_uri,
			       ?NICE("Client not authorized to evaluate: "
				     ++ CGIBody)}} | ModData#mod.data]}
    end.

control_response_header(ModData, Mod, Func, Response)->
    case control_response(Response, ModData, Mod, Func) of
	{proceed,[{response,{StatusCode, Response}} | Rest]} ->
	    case httpd_util:lookup(ModData#mod.config_db,
				   erl_script_nocache,false) of
		true ->
		    case httpd_util:split(Response, [?CR, ?LF, ?CR, ?LF], 2) of
			{ok,[Head, Body]}->
			    Date  = httpd_util:rfc1123_date(),
			    Cache = "Cache-Control:no-cache\r\nPragma:"
				"no-cache\r\nExpires:"++ Date ++ "\r\n",
			    {proceed,[{response,{StatusCode,
						 [Head,  ?CRLF, Cache,
						  ?CRLF, Body]}} | Rest]};
			_->    
			    {proceed, 
			    [{response, {StatusCode, Response}} | Rest]}
		    end;
		_WhatEver ->
		    {proceed, [{response, {StatusCode, Response}} | Rest]}
	    end;
	WhatEver ->
	    WhatEver
    end.

control_response(Response, ModData, Mod, Func)->
    ?vdebug("Response: ~n~p",[Response]),
    case handle_esi_headers(lists:flatten(Response)) of
	{ok, _, StatusCode} ->	    
	    {proceed,[{response,{StatusCode, Response}} | ModData#mod.data]};
	{proceed, AbsPath} ->
	      {proceed, [{real_name, httpd_util:split_path(AbsPath)} 
			 | ModData#mod.data]};
	{error,Reason} ->
	    {proceed,
	     [{status,{400,none,
		       ?NICE("Error in "++ Mod ++ ":" ++ Func ++ "/2: " ++
			     lists:flatten(io_lib:format("~p",[Reason])))}}|
	      ModData#mod.data]}
    end.

parsed_header([]) ->
    [];
parsed_header([{Name,[Value|R1]}|R2]) when list(Value) ->
    NewName = lists:map(fun(X) -> if X == $- -> $_; true -> X end end,Name),
    [{list_to_atom("http_"++httpd_util:to_lower(NewName)),
      multi_value([Value|R1])}|parsed_header(R2)];
parsed_header([{Name,Value}|Rest]) when list(Value)->
    {ok,NewName,_} = regexp:gsub(Name,"-","_"),
    [{list_to_atom("http_"++httpd_util:to_lower(NewName)),Value}|
     parsed_header(Rest)].

multi_value([]) ->
    [];
multi_value([Value]) ->
    Value;
multi_value([Value|Rest]) ->
    Value++", "++multi_value(Rest).

%%
%% Eval mechanism
%% 
eval(#mod{request_uri = ReqUri, http_version = Version, data = Data}, 
     "POST", _CGIBody, _Modules) ->
    ?vtrace("eval(POST) -> method not supported",[]),	    
    {proceed,[{status,{501,{"POST", ReqUri, Version},
		       ?NICE("Eval mechanism doesn't support method POST")}}|
	      Data]};

eval(ModData,"HEAD",CGIBody,Modules) ->
    %% The function that sends the data in httpd_response handles HEAD 
    %% reqest by not sending the body
    eval(ModData,"GET",CGIBody,Modules);


eval(ModData,"GET",CGIBody,Modules) ->
    ?vtrace("eval(GET) -> entry when"
	    "~n   Modules: ~p",[Modules]),	    
    case auth(CGIBody,Modules) of
	true ->
	    case lib:eval_str(string:concat(CGIBody,". ")) of
		{error,Reason} ->
		    ?vlog("eval -> error:"
			  "~n   Reason: ~p",[Reason]),	    
		    {proceed,[{status,{500,none,Reason}}|ModData#mod.data]};
		{ok,Response} ->
		    ?vtrace("eval -> ok:"
			    "~n   Response: ~p",[Response]),	    
		    case handle_esi_headers(lists:flatten(Response)) of
			{ok, _, StatusCode} ->
			    {proceed,[{response, {StatusCode, Response}} | 
				      ModData#mod.data]};
			{proceed, AbsPath} ->
			    {proceed, [{cgi_abs_path, AbsPath} | 
				       ModData#mod.data]};
			{error,Reason} ->
			    {proceed, [{status,{400,none,Reason}} | 
				      ModData#mod.data]}
		    end
	    end;
	false ->
	    ?vlog("eval -> auth failed",[]),	    
	    {proceed,[{status,
		       {403,ModData#mod.request_uri,
			?NICE("Client not authorized to evaluate: "
			      ++ CGIBody)}} | ModData#mod.data]}
    end.

auth(_CGIBody, ["all"]) ->
    true;
auth(CGIBody, Modules) ->
    case regexp:match(CGIBody,"^[^\:(%3A)]*") of
	{match,Start,Length} ->
	    lists:member(string:substr(CGIBody,Start,Length),Modules);
	nomatch ->
	    false
    end.

%%----------------------------------------------------------------------
%% Creates the environment list that will be the first arg to the 
%% Functions that is called through the ErlScript Schema
%%----------------------------------------------------------------------

get_environment(ModData,ServerName,Method,RemoteAddr,Type,Input)->
    Env = [{server_software, ?SERVER_SOFTWARE},
	   {server_name, ServerName},
	   {gateway_interface,?GATEWAY_INTERFACE},
	   {server_protocol, ?SERVER_PROTOCOL},
	   {server_port, httpd_util:lookup(ModData#mod.config_db,port,80)},
	   {request_method, Method},
	   {remote_addr, RemoteAddr},
	   {script_name, ModData#mod.request_uri} |
	   parsed_header(ModData#mod.parsed_header)],
    get_environment(Type,Input,Env,ModData).


get_environment(Type,Input,Env,ModData)->
    Env1 = 
	case Type of
	    query_string ->
		[{query_string,Input}|Env];

	    path_info ->
		Aliases = httpd_util:multi_lookup(ModData#mod.config_db,alias),
		{_,PathTranslated,_} = 
		    mod_alias:real_name(ModData#mod.config_db,[$/|Input],
					Aliases),
		[{path_info,"/"++httpd_util:decode_hex(Input)},
		 {path_translated, PathTranslated} | Env];

	    entity_body ->
		[{content_length, httpd_util:flatlength(Input)} | Env];

	    no_input ->
		Env
	end,
    get_environment(ModData,Env1).

get_environment(ModData, Env)->
    case httpd_util:key1search(ModData#mod.data,remote_user) of
	undefined ->
	    Env;
	RemoteUser ->
	    [{remote_user, RemoteUser} | Env]
    end.


%%
%% Configuration
%%

%% load

load("ErlScriptAlias " ++ ErlScriptAlias, []) ->
    case regexp:split(ErlScriptAlias," ") of
	{ok, [ErlName|Modules]} ->
	    {ok, [], {erl_script_alias, {ErlName,Modules}}};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(ErlScriptAlias)++
			 " is an invalid ErlScriptAlias")}
    end;
load("EvalScriptAlias " ++ EvalScriptAlias,[]) ->
    case regexp:split(EvalScriptAlias, " ") of
	{ok, [EvalName|Modules]} ->
	    {ok, [], {eval_script_alias, {EvalName,Modules}}};
	{ok, _} ->
	    {error, ?NICE(httpd_conf:clean(EvalScriptAlias)++
			  " is an invalid EvalScriptAlias")}
    end;
load("ErlScriptTimeout " ++ Timeout, [])->
    case catch list_to_integer(httpd_conf:clean(Timeout)) of
	TimeoutSec when integer(TimeoutSec)  ->
	   {ok, [], {erl_script_timeout,TimeoutSec*1000}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(Timeout)++
			 " is an invalid ErlScriptTimeout")}
    end;
load("ErlScriptNoCache " ++ CacheArg, [])->
    case catch list_to_atom(httpd_conf:clean(CacheArg)) of
        true ->
	    {ok, [], {erl_script_nocache,true}};
	false ->
	   {ok, [], {erl_script_nocache,false}};
	_ ->
	   {error, ?NICE(httpd_conf:clean(CacheArg)++
			 " is an invalid ErlScriptNoCache directive")}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                    %%
%% Functions below handles the data from the dynamic webpages         %%
%% That sends data back to the user part by part                      %%
%%                                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% Deliver is the callback function users can call to deliver back data 
%% to the client
%%----------------------------------------------------------------------

deliver(SessionID,Data)when pid(SessionID) ->
    SessionID ! {ok,Data},
    ok;
deliver(_SessionID, _Data) ->
    {error,bad_sessionID}.


%%----------------------------------------------------------------------
%% The method that tries to execute the new format 
%%----------------------------------------------------------------------

%% It would be nicer to use erlang:function_exported/3 but if the 
%% Module isn't loaded the function says that it is not loaded
try_new_erl_scheme_method(ModData, Env, Input, Mod, Func) -> 
    process_flag(trap_exit, true),
    Self = self(),
    Pid = spawn_link(
	    fun() ->
		    case catch Mod:Func(Self, Env, Input) of
			{'EXIT',{undef,_}} ->
			    exit(not_new_method);
			_ ->
			    ok  
		    end
	    end),
 
    RetVal = proxy(ModData, Pid), 
  
    process_flag(trap_exit,false),
    RetVal.


erl_script_timeout(Db) ->
    httpd_util:lookup(Db, erl_script_timeout, ?DEFAULT_ERL_TIMEOUT).

proxy(#mod{config_db = Db} = ModData, Pid) ->
    ?vtrace("proxy -> entry with~n   Pid: ~p", [Pid]),
    Timeout            = erl_script_timeout(Db),
    proxy(ModData, Pid, Timeout).

proxy(#mod{config_db = Db} = ModData, Pid, Timeout) ->
    case receive_headers(?MODULE, parse_headers, [[], []], Timeout) of
	{error, Reason} ->
	    {error, Reason};
	{proceed, AbsPath} ->
	    {proceed, [{get_abs_path, AbsPath} | ModData#mod.data]};
	{Headers, Body} ->
	    {ok, NewHeaders, StatusCode} = handle_esi_headers(Headers),
	    IsDisableChunkedSend = httpd_response:is_disable_chunked_send(Db),
	    case (ModData#mod.http_version =/= "HTTP/1.1") or
		(IsDisableChunkedSend) of
		true ->
		    send_headers(ModData, StatusCode, NewHeaders);
		false ->
		    send_headers(ModData, StatusCode, [{"transfer-encoding", 
						     "chunked"} | NewHeaders])
	    end,    
	    handle_body(Pid, ModData, Body, Timeout, length(Body), 
			IsDisableChunkedSend);
	timeout ->
	    send_headers(ModData, {504, "Timeout"}, []),
	    httpd_socket:close(ModData#mod.socket_type, ModData#mod.socket),
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, 0}} | ModData#mod.data]}
    end.

handle_esi_headers(Response) ->
    case httpd_util:split(Response, [?CR, ?LF, ?CR, ?LF], 2) of
	{ok,[Headers,_Body]} ->
	    {ok, NewHeaders} = regexp:split(Headers, ?CRLF),
	    handle_headers(NewHeaders);
	_ ->
	    %% No header field in the returned data return 200 the
	    %% standard code
	    {ok, [], 200}
   end.
    
handle_headers(Headers) ->
    handle_headers(Headers, [], 200).

handle_headers([], NewHeaders, StatusCode) ->
    {ok, NewHeaders, StatusCode};

handle_headers([Header | Headers], NewHeaders, StatusCode) -> 
    {FieldName, FieldValue} = httpd_response:split_header(Header, []),

    case FieldName of
	"location" ->
	    case http_request:is_absolut_uri(FieldValue) of
		true ->
		    handle_headers(Headers, 
				   [{FieldName, FieldValue} | NewHeaders], 
				   302);
		false ->
		    {proceed, FieldValue}
	    end;
	_ -> 
	    handle_headers(Headers, 
			   [{FieldName, FieldValue}| NewHeaders], StatusCode)
    end.	

receive_headers(Module, Function, Args, Timeout) ->
    receive
	  {ok, Response} ->
	      case Module:Function([Response | Args]) of
		  {NewModule, NewFunction, NewArgs} ->
		      receive_headers(NewModule, NewFunction, 
				      NewArgs, Timeout);
		  {Headers, Body} ->
		      {Headers, Body}
	      end;
	  {'EXIT', Pid, normal} when is_pid(Pid) ->
	      [_, Body] = Args,
	      %% So that httpd_response:send_final_chunk will be 
	      %% called in handle body as there where no headers
	      self() !  {'EXIT', Pid, normal},
	      {[], Body};
	  {'EXIT', Pid, not_new_method} when is_pid(Pid) ->
	      {error, not_new_method};
	  {'EXIT', Pid, Reason} when is_pid(Pid) ->
	      exit({mod_cgi_linked_process_died, Pid, Reason})
      after Timeout ->
	      timeout
      end.

parse_headers([Data, List, Acc]) ->
    parse_headers(Data ++ List, Acc).

parse_headers([], Acc) ->
    {?MODULE, parse_headers, [[], Acc]};
parse_headers([?CR], Acc) ->
    {?MODULE, parse_headers, [[?CR], Acc]};
parse_headers([?CR, ?LF], Acc) ->
    {?MODULE, parse_headers, [[?CR, ?LF], Acc]};
parse_headers([?CR, ?LF, ?CR], Acc) ->
    {?MODULE, parse_headers, [?CR, ?LF, ?CR, Acc]};
parse_headers([?CR, ?LF, ?CR, ?LF], Acc) ->
    {lists:reverse(Acc) ++ [?CR, ?LF, ?CR, ?LF], []};
parse_headers([?CR, ?LF, ?CR, ?LF | Rest], Acc) ->
    {lists:reverse(Acc) ++ [?CR, ?LF, ?CR, ?LF], Rest};
parse_headers([Char | Rest], Acc) ->
    parse_headers(Rest, [Char | Acc]).

send_headers(ModData, StatusCode, HTTPHeaders) ->
    ExtraHeaders = httpd_response:cache_headers(ModData),
    httpd_response:send_header(ModData, StatusCode, 
			       ExtraHeaders ++ HTTPHeaders).

handle_body(_, #mod{method = "HEAD"} = ModData, _, _, Size, _) ->
    process_flag(trap_exit,false),
    {proceed, [{response, {already_sent, 200, Size}} | ModData#mod.data]};

handle_body(Pid, ModData, Body, Timeout, Size, IsDisableChunkedSend) ->
    httpd_response:send_chunk(ModData, Body, IsDisableChunkedSend),
    receive 
	{ok, Data} ->
	    handle_body(Pid, ModData, Data, Timeout, Size + length(Data),
			IsDisableChunkedSend);
	{'EXIT', Pid, normal} when is_pid(Pid) ->
	    httpd_response:send_final_chunk(ModData, IsDisableChunkedSend),
	    {proceed, [{response, {already_sent, 200, Size}} | 
		       ModData#mod.data]};
	{'EXIT', Pid, Reason} when is_pid(Pid) ->
	    exit({mod_esi_linked_process_died, Pid, Reason})
    after Timeout ->
	    process_flag(trap_exit,false),
	    {proceed,[{response, {already_sent, 200, Size}} | 
		      ModData#mod.data]}  
    end.

