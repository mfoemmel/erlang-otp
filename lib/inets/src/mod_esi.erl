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
-export([do/1,load/2]).

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
		    case erl_or_eval(Info#mod.request_uri,Info#mod.config_db) of
			{eval,CGIBody,Modules} ->
			    eval(Info,Info#mod.method,CGIBody,Modules);
			{erl,CGIBody,Modules} ->
			    erl(Info,Info#mod.method,CGIBody,Modules);
			proceed ->
			    {proceed,Info#mod.data}
		    end;
		%% A response has been generated or sent!
		Response ->
		    {proceed,Info#mod.data}
	    end
    end.

	    

%% erl_or_eval

erl_or_eval(RequestURI, ConfigDB) ->
    case erlp(RequestURI, ConfigDB) of
	false ->
      case evalp(RequestURI, ConfigDB) of
	  false ->
	      proceed;
	  Other ->
	      Other
      end;
	Other ->
	    Other
    end.

erlp(RequestURI, ConfigDB) ->
    case httpd_util:lookup(ConfigDB, erl_script_alias) of
	undefined ->
	    false;
	{Alias, Modules} ->
	    case regexp:first_match(RequestURI,"^"++Alias++"/") of
		{match,1,Length} ->
		    {erl,string:substr(RequestURI,Length+1),Modules};
		nomatch ->
		    false
	    end
    end.

evalp(RequestURI, ConfigDB) ->
    case httpd_util:lookup(ConfigDB, eval_script_alias) of
	undefined ->
	    false;
	{Alias, Modules} ->
	    case regexp:first_match(RequestURI,"^"++Alias++"\\?") of
		{match, 1, Length} ->
		    {eval, string:substr(RequestURI,Length+1),Modules};
		nomatch ->
		    false
	    end
    end.


%%
%% Erl mechanism
%% 

erl(Info,"GET",CGIBody,Modules) ->
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok, [Mod,FuncAndInput]} ->
	    case httpd_util:split(FuncAndInput,"[\?/]",2) of
		{ok, [Func,Input]} ->
		    exec(Info,"GET",CGIBody,Modules,Mod,Func,{input_type(FuncAndInput),
							      Input});
		{ok, [Func]} ->
		    exec(Info,"GET",CGIBody,Modules,Mod,Func,{no_input,""});
		{ok, BadRequest} ->
		    {proceed,[{status,{400,none,BadRequest}}|Info#mod.data]}
	    end;
	{ok, BadRequest} ->
	    {proceed, [{status,{400,none,BadRequest}}|Info#mod.data]}
    end;
erl(Info, "POST", CGIBody, Modules) ->
    case httpd_util:split(CGIBody,":|%3A|/",2) of
	{ok,[Mod,Func]} ->
	    exec(Info,"POST",CGIBody,Modules,Mod,Func,{entity_body,
						       Info#mod.entity_body});
	{ok,BadRequest} ->
	    {proceed,[{status,{400,none,BadRequest}}|Info#mod.data]}
    end.

input_type([]) ->
    no_input;
input_type([$/|Rest]) ->
    path_info;
input_type([$?|Rest]) ->
    query_string;
input_type([First|Rest]) ->
    input_type(Rest).


%% exec

exec(Info,Method,CGIBody,["all"],Mod,Func,{Type,Input}) ->
    exec(Info,Method,CGIBody,[Mod],Mod,Func,{Type,Input});
exec(Info,Method,CGIBody,Modules,Mod,Func,{Type,Input}) ->
    case lists:member(Mod,Modules) of
	true ->
	    {_,RemoteAddr}=(Info#mod.init_data)#init_data.peername,
	    ServerName=(Info#mod.init_data)#init_data.resolve,
	    Env=[{server_software,?SERVER_SOFTWARE},
		 {server_name,ServerName},
		 {gateway_interface,?GATEWAY_INTERFACE},
		 {server_protocol,?SERVER_PROTOCOL},
		 {server_port,httpd_util:lookup(Info#mod.config_db,port,80)},
		 {request_method,Method},
		 {remote_addr,RemoteAddr},
		 {script_name,Info#mod.request_uri}|
		 parsed_header(Info#mod.parsed_header)],
	    Env1=
		case Type of
		    query_string ->
			[{query_string,Input}|Env];
		    path_info ->
			Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
			{_,PathTranslated,_}=
			    mod_alias:real_name(Info#mod.config_db,[$/|Input],Aliases),
			[{path_info,"/"++httpd_util:decode_hex(Input)},
			 {path_translated,PathTranslated}|Env];
		    entity_body ->
			[{content_length,httpd_util:flatlength(Input)}|Env];
		    no_input ->
			Env
		end,
	    Env2=
		case httpd_util:key1search(Info#mod.data,remote_user) of
		    undefined ->
			Env1;
		    RemoteUser ->
			[{remote_user,RemoteUser}|Env1]
		end,
	    case catch apply(list_to_atom(Mod),list_to_atom(Func),[Env2,Input]) of
		{'EXIT',Reason} ->
		    {proceed,[{status,{500,none,Reason}}|Info#mod.data]};
		Response ->
		    case mod_cgi:status_code(lists:flatten(Response)) of
			{ok,StatusCode} ->
			    {proceed,[{response,{StatusCode,Response}}|Info#mod.data]};
			{error,Reason} ->
			    {proceed,
			     [{status,{400,none,
				       ?NICE("Error in "++Mod++":"++Func++"/2: "++
					     lists:flatten(io_lib:format("~p",[Reason])))}}|
			      Info#mod.data]}
		    end
	    end;
	false ->
	    {proceed,[{status,
		       {403,Info#mod.request_uri,
			?NICE("Client not authorized to evaluate: "++CGIBody)}}|
		      Info#mod.data]}
    end.

parsed_header([]) ->
    [];
parsed_header([{Name,[Value|R1]}|R2]) when list(Value) ->
    NewName=lists:map(fun(X) -> if X == $- -> $_; true -> X end end,Name),
    [{list_to_atom("http_"++httpd_util:to_lower(NewName)),
      multi_value([Value|R1])}|parsed_header(R2)];
parsed_header([{Name,Value}|Rest]) when list(Value)->
    {ok,NewName,_}=regexp:gsub(Name,"-","_"),
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

eval(Info,"POST",CGIBody,Modules) ->
    {proceed,[{status,{501,{"POST",Info#mod.request_uri,Info#mod.http_version},
		       ?NICE("Eval mechanism doesn't support method POST")}}|
	      Info#mod.data]};
eval(Info,"GET",CGIBody,Modules) ->
    case auth(CGIBody,Modules) of
	true ->
	    case lib:eval_str(string:concat(CGIBody,". ")) of
		{error,Reason} ->
		    {proceed,[{status,{500,none,Reason}}|Info#mod.data]};
		{ok,Response} ->
		    case mod_cgi:status_code(lists:flatten(Response)) of
			{ok,StatusCode} ->
			    {proceed,[{response,{StatusCode,Response}}|Info#mod.data]};
			{error,Reason} ->
			    {proceed,[{status,{400,none,Reason}}|Info#mod.data]}
		    end
	    end;
	false ->
	    {proceed,[{status,
		       {403,Info#mod.request_uri,
			?NICE("Client not authorized to evaluate: "++CGIBody)}}|
		      Info#mod.data]}
    end.

auth(CGIBody,["all"]) ->
    true;
auth(CGIBody,Modules) ->
    case regexp:match(CGIBody,"^[^\:(%3A)]*") of
	{match,Start,Length} ->
	    lists:member(string:substr(CGIBody,Start,Length),Modules);
	nomatch ->
	    false
    end.

%%
%% Configuration
%%

%% load

load([$E,$r,$l,$S,$c,$r,$i,$p,$t,$A,$l,$i,$a,$s,$ |ErlScriptAlias],[]) ->
    case regexp:split(ErlScriptAlias," ") of
	{ok, [ErlName|Modules]} ->
	    {ok, [], {erl_script_alias, {ErlName,Modules}}};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(ErlScriptAlias)++
			 " is an invalid ErlScriptAlias")}
    end;
load([$E,$v,$a,$l,$S,$c,$r,$i,$p,$t,$A,$l,$i,$a,$s,$ |EvalScriptAlias],[]) ->
    case regexp:split(EvalScriptAlias, " ") of
	{ok, [EvalName|Modules]} ->
	    {ok, [], {eval_script_alias, {EvalName,Modules}}};
	{ok, _} ->
	    {error, ?NICE(httpd_conf:clean(EvalScriptAlias)++
			  " is an invalid EvalScriptAlias")}
    end.
