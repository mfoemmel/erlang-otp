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
-module(mod_include).
-export([do/1,parse/2,config/6,include/6,echo/6,fsize/6,flastmod/6,exec/6]).

-include("httpd.hrl").

%% do

do(Info) ->
    case Info#mod.method of
	"GET" ->
	    case httpd_util:key1search(Info#mod.data,status) of
		%% A status code has been generated!
		{StatusCode,PhraseArgs,Reason} ->
		    {proceed,Info#mod.data};
		%% No status code has been generated!
		undefined ->
		    case httpd_util:key1search(Info#mod.data, response) of
			%% No response has been generated!
			undefined ->
			    do_include(Info);
			%% A response has been generated or sent!
			Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.

do_include(Info) ->
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
			  Info#mod.request_uri),
    Suffix = httpd_util:suffix(Path),
    case httpd_util:lookup_mime_default(Info#mod.config_db,Suffix) of
	"text/x-server-parsed-html" ->
	    Date = httpd_util:rfc1123_date(),
	    Header = httpd_util:header(200,"text/html",Date),
	    httpd_socket:deliver(Info#mod.socket_type,
				 Info#mod.socket,
				 [Header,"\r\n\r\n"]),
	    case send_in_chunks(Info,Path) of
		{ok,ErrorLog} ->
		    {proceed,[{response,{already_sent,200,0}},
			      {mime_type,"text/html"}|
			      lists:append(ErrorLog,Info#mod.data)]};
		{error,Reason} ->
		    {proceed,[{status,{404,Info#mod.request_uri,
				       ?NICE("Can't access "++Path)}}|
			      Info#mod.data]}
	    end;
	_ ->
	    {proceed,Info#mod.data}
    end.


%%
%% config directive
%%

config(Info, Context, ErrorLog, TagList, ValueList, R) ->
    case verify_tags("config",[errmsg,timefmt,sizefmt],
		     TagList,ValueList) of
	ok ->
	    {ok,update_context(TagList,ValueList,Context),ErrorLog,"",R};
	{error,Reason} ->
	    {ok,Context,[{internal_info,Reason}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

update_context([],[],Context) ->
    Context;
update_context([Tag|R1],[Value|R2],Context) ->
    update_context(R1,R2,[{Tag,Value}|Context]).

verify_tags(Command,ValidTags,TagList,ValueList) when length(TagList)==length(ValueList) ->
    verify_tags(Command,ValidTags,TagList);
verify_tags(Command,ValidTags,TagList,ValueList) ->
    {error,?NICE(Command++" directive has spurious tags")}.

verify_tags(Command, ValidTags, []) ->
    ok;
verify_tags(Command, ValidTags, [Tag|Rest]) ->
    case lists:member(Tag, ValidTags) of
	true ->
	    verify_tags(Command, ValidTags, Rest);
	false ->
	    {error,?NICE(Command++" directive has a spurious tag ("++
			 atom_to_list(Tag)++")")}
    end.

%%
%% include directive
%%

include(Info,Context,ErrorLog,[virtual],[VirtualPath],R) ->
    Aliases = httpd_util:multi_lookup(Info#mod.config_db,alias),
    {_, Path, _AfterPath} =
	mod_alias:real_name(Info#mod.config_db, VirtualPath, Aliases),
    include(Info,Context,ErrorLog,R,Path);
include(Info, Context, ErrorLog, [file], [FileName], R) ->
    Path = file(Info#mod.config_db, Info#mod.request_uri, FileName),
    include(Info, Context, ErrorLog, R, Path);
include(Info, Context, ErrorLog, TagList, ValueList, R) ->
    {ok, Context,
     [{internal_info,?NICE("include directive has a spurious tag")}|
      ErrorLog], httpd_util:key1search(Context, errmsg, ""), R}.

include(Info, Context, ErrorLog, R, Path) ->
    case file:read_file(Path) of
	{ok, Body} ->
	    {ok, NewContext, NewErrorLog, Result} =
		parse(Info, binary_to_list(Body), Context, ErrorLog, []),
	    {ok, Context, NewErrorLog, Result, R};
	{error, Reason} ->
	    {ok, Context, 
	     [{internal_info, ?NICE("Can't open "++Path)}|ErrorLog],
	     httpd_util:key1search(Context, errmsg, ""), R}
    end.

file(ConfigDB, RequestURI, FileName) ->
    Aliases = httpd_util:multi_lookup(ConfigDB, alias),
    {_, Path, _AfterPath}
	= mod_alias:real_name(ConfigDB, RequestURI, Aliases),
    Pwd = filename:dirname(Path),
    filename:join(Pwd, FileName).

%%
%% echo directive
%%

echo(Info,Context,ErrorLog,[var],["DOCUMENT_NAME"],R) ->
    {ok,Context,ErrorLog,document_name(Info#mod.data,Info#mod.config_db,
				       Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["DOCUMENT_URI"],R) ->
    {ok,Context,ErrorLog,document_uri(Info#mod.config_db,
				      Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["QUERY_STRING_UNESCAPED"],R) ->
    {ok,Context,ErrorLog,query_string_unescaped(Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,[var],["DATE_LOCAL"],R) ->
    {ok,Context,ErrorLog,date_local(),R};
echo(Info,Context,ErrorLog,[var],["DATE_GMT"],R) ->
    {ok,Context,ErrorLog,date_gmt(),R};
echo(Info,Context,ErrorLog,[var],["LAST_MODIFIED"],R) ->
    {ok,Context,ErrorLog,last_modified(Info#mod.data,Info#mod.config_db,
				       Info#mod.request_uri),R};
echo(Info,Context,ErrorLog,TagList,ValueList,R) ->
    {ok,Context,
     [{internal_info,?NICE("echo directive has a spurious tag")}|
      ErrorLog],"(none)",R}.

document_name(Data,ConfigDB,RequestURI) ->
    Path = mod_alias:path(Data,ConfigDB,RequestURI),
    case regexp:match(Path,"[^/]*\$") of
	{match,Start,Length} ->
	    string:substr(Path,Start,Length);
	nomatch ->
	    "(none)"
    end.

document_uri(ConfigDB, RequestURI) ->
    Aliases = httpd_util:multi_lookup(ConfigDB, alias),
    {Path, AfterPath} = 
	case mod_alias:real_name(ConfigDB, RequestURI, Aliases) of
	    {_, Name, {[], []}} ->
		{Name, ""};
	    {_, Name, {PathInfo, []}} ->
		{Name, "/"++PathInfo};
	    {_, Name, {PathInfo, QueryString}} ->
		{Name, "/"++PathInfo++"?"++QueryString};
	    {_, Name, _} ->
		{Name, ""};
	    Gurka ->
		io:format("Gurka: ~p~n", [Gurka])
	end,
    VirtualPath = string:substr(RequestURI, 1, 
				length(RequestURI)-length(AfterPath)),
    {match, Start, Length} = regexp:match(Path,"[^/]*\$"),
    FileName = string:substr(Path,Start,Length),
    case regexp:match(VirtualPath, FileName++"\$") of
	{match, _, _} ->
	    httpd_util:decode_hex(VirtualPath)++AfterPath;
	nomatch ->
	    string:strip(httpd_util:decode_hex(VirtualPath),right,$/)++
		"/"++FileName++AfterPath
    end.

query_string_unescaped(RequestURI) ->
  case regexp:match(RequestURI,"[\?].*\$") of
    {match,Start,Length} ->
      %% Escape all shell-special variables with \
      escape(string:substr(RequestURI,Start+1,Length-1));      
    nomatch ->
      "(none)"
  end.

escape([]) -> [];
escape([$;|R]) -> [$\\,$;|escape(R)];
escape([$&|R]) -> [$\\,$&|escape(R)];
escape([$(|R]) -> [$\\,$(|escape(R)];
escape([$)|R]) -> [$\\,$)|escape(R)];
escape([$||R]) -> [$\\,$||escape(R)];
escape([$^|R]) -> [$\\,$^|escape(R)];
escape([$<|R]) -> [$\\,$<|escape(R)];
escape([$>|R]) -> [$\\,$>|escape(R)];
escape([$\n|R]) -> [$\\,$\n|escape(R)];
escape([$ |R]) -> [$\\,$ |escape(R)];
escape([$\t|R]) -> [$\\,$\t|escape(R)];
escape([C|R]) -> [C|escape(R)].

date_local() ->
  {{Year,Month,Day},{Hour,Minute,Second}}=calendar:local_time(),
  %% Time format hard-wired to: "%a %b %e %T %Y" according to strftime(3)
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

date_gmt() ->
  {{Year,Month,Day},{Hour,Minute,Second}}=calendar:universal_time(),
  %% Time format hard-wired to: "%a %b %e %T %Z %Y" according to strftime(3)
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w GMT ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

last_modified(Data,ConfigDB,RequestURI) ->
  {ok,FileInfo}=file:read_file_info(mod_alias:path(Data,ConfigDB,RequestURI)),
  {{Year,Month,Day},{Hour,Minute,Second}}=FileInfo#file_info.mtime,
  io_lib:format("~s ~s ~2w ~2.2.0w:~2.2.0w:~2.2.0w ~w",
		[httpd_util:day(calendar:day_of_the_week(Year,Month,Day)),
		 httpd_util:month(Month),Day,Hour,Minute,Second,Year]).

%%
%% fsize directive
%%

fsize(Info,Context,ErrorLog,[virtual],[VirtualPath],R) ->
  Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
  {_,Path,AfterPath}=
    mod_alias:real_name(Info#mod.config_db,VirtualPath,Aliases),
  fsize(Info, Context, ErrorLog, R, Path);
fsize(Info,Context,ErrorLog,[file],[FileName],R) ->
  Path=file(Info#mod.config_db,Info#mod.request_uri,FileName),
  fsize(Info,Context,ErrorLog,R,Path);
fsize(Info,Context,ErrorLog,TagList,ValueList,R) ->
  {ok,Context,[{internal_info,?NICE("fsize directive has a spurious tag")}|
	       ErrorLog],httpd_util:key1search(Context,errmsg,""),R}.

fsize(Info,Context,ErrorLog,R,Path) ->
    case file:read_file_info(Path) of
	{ok,FileInfo} ->
	    case httpd_util:key1search(Context,sizefmt) of
		"bytes" ->
		    {ok,Context,ErrorLog,integer_to_list(FileInfo#file_info.size),R};
		"abbrev" ->
		    Size=integer_to_list(trunc(FileInfo#file_info.size/1024+1))++"k",
		    {ok,Context,ErrorLog,Size,R};
		Value->
		    {ok,Context,
		     [{internal_info,
		       ?NICE("fsize directive has a spurious tag value ("++
			     Value++")")}|
		      ErrorLog],
		     httpd_util:key1search(Context, errmsg, ""), R}
	    end;
	{error,Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++Path)}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

%%
%% flastmod directive
%%

flastmod(Info, Context, ErrorLog, [virtual], [VirtualPath],R) ->
    Aliases=httpd_util:multi_lookup(Info#mod.config_db,alias),
    {_,Path,AfterPath}=
	mod_alias:real_name(Info#mod.config_db,VirtualPath,Aliases),
    flastmod(Info,Context,ErrorLog,R,Path);
flastmod(Info, Context, ErrorLog, [file], [FileName], R) ->
    Path = file(Info#mod.config_db, Info#mod.request_uri, FileName),
    flastmod(Info, Context, ErrorLog, R, Path);
flastmod(Info,Context,ErrorLog,TagList,ValueList,R) ->
    {ok,Context,[{internal_info,?NICE("flastmod directive has a spurious tag")}|
		 ErrorLog],httpd_util:key1search(Context,errmsg,""),R}.

flastmod(Info,Context,ErrorLog,R,File) ->
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    {{Yr,Mon,Day},{Hour,Minute,Second}}=FileInfo#file_info.mtime,
	    Result=
		io_lib:format("~s ~s ~2w ~w:~w:~w ~w",
			      [httpd_util:day(
				 calendar:day_of_the_week(Yr,Mon, Day)),
			       httpd_util:month(Mon),Day,Hour,Minute,Second, Yr]),
	    {ok,Context,ErrorLog,Result,R};
	{error,Reason} ->
	    {ok,Context,[{internal_info,?NICE("Can't open "++File)}|ErrorLog],
	     httpd_util:key1search(Context,errmsg,""),R}
    end.

%%
%% exec directive
%%

exec(Info,Context,ErrorLog,[cmd],[Command],R) ->
    cmd(Info,Context,ErrorLog,R,Command);
exec(Info,Context,ErrorLog,[cgi],[RequestURI],R) ->
    cgi(Info,Context,ErrorLog,R,RequestURI);
exec(Info,Context,ErrorLog,TagList,ValueList,R) ->
    {ok, Context,
     [{internal_info,?NICE("exec directive has a spurious tag")}|
      ErrorLog], httpd_util:key1search(Context,errmsg,""),R}.

%% cmd

cmd(Info, Context, ErrorLog, R, Command) ->
    Env = env(Info),
    Dir = filename:dirname(Command),
    Port = open_port({spawn, Command}, [{cd, Dir},
					{env, Env},
					stream]),
    {NewErrorLog, Result} = proxy(Port, ErrorLog),
    {ok, Context, NewErrorLog, Result, R}.

env(Info) ->
    [{"DOCUMENT_NAME",document_name(Info#mod.data,Info#mod.config_db,
				    Info#mod.request_uri)},
     {"DOCUMENT_URI", document_uri(Info#mod.config_db, Info#mod.request_uri)},
     {"QUERY_STRING_UNESCAPED", query_string_unescaped(Info#mod.request_uri)},
     {"DATE_LOCAL", date_local()},
     {"DATE_GMT", date_gmt()},
     {"LAST_MODIFIED", last_modified(Info#mod.data, Info#mod.config_db,
				     Info#mod.request_uri)}
    ].

%% cgi

cgi(Info, Context, ErrorLog, R, RequestURI) ->
    ScriptAliases = httpd_util:multi_lookup(Info#mod.config_db, script_alias),
    case mod_alias:real_script_name(Info#mod.config_db, RequestURI,
				    ScriptAliases) of
	{Script, AfterScript} ->
	    Aliases = httpd_util:multi_lookup(Info#mod.config_db, alias),
	    {_, Path, AfterPath} = mod_alias:real_name(Info#mod.config_db,
						       Info#mod.request_uri,
						       Aliases),
	    Env = env(Info)++mod_cgi:env(Info, Path, AfterPath),
	    Dir = filename:dirname(Path),
	    Port = open_port({spawn,Script},[{env, Env},
					     {cd, Dir},
					     stream]),
	    %% Send entity body to port.
	    if
		Info#mod.entity_body == [] ->
		    no_entity_body;
		true ->
		    Port ! {self(), {command, Info#mod.entity_body}}
	    end,
	    {NewErrorLog, Result} = proxy(Port, ErrorLog),
	    {ok, Context, NewErrorLog, remove_header(Result), R};
	not_a_script ->
	    {ok, Context,
	     [{internal_info, ?NICE(RequestURI++" is not a script")}|
	      ErrorLog], httpd_util:key1search(Context, errmsg, ""),R}
    end.

remove_header([]) ->
    [];
remove_header([$\n,$\n|Rest]) ->
    Rest;
remove_header([C|Rest]) ->
    remove_header(Rest).

%%
%% Port communication
%%

proxy(Port,ErrorLog) ->
    process_flag(trap_exit, true),
    proxy(Port, ErrorLog, []).

proxy(Port, ErrorLog, Result) ->
    receive
	{Port, {data, Response}} ->
	    proxy(Port, ErrorLog, lists:append(Result,Response));
	{'EXIT', Port, normal} when port(Port) ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result};
	{'EXIT', Port, Reason} when port(Port) ->
	    process_flag(trap_exit, false),
	    {[{internal_info,?NICE("Scrambled output from CGI-script")}|ErrorLog],
	     Result};
	{'EXIT', Pid, Reason} when pid(Pid) ->
	    process_flag(trap_exit, false),
	    {'EXIT', Pid, Reason};
	%% This should not happen!
	WhatEver ->
	    process_flag(trap_exit, false),
	    {ErrorLog, Result}
    end.

%%
%% Addition to "Fuzzy" HTML parser. This is actually a ugly hack to
%% avoid putting to much data on the heap. To be rewritten...
%%

-define(CHUNK_SIZE, 4096).

send_in_chunks(Info, Path) ->
    case file:open(Path, [read, raw]) of
	{ok, Stream} ->
	    send_in_chunks(Info, Stream, ?DEFAULT_CONTEXT,[]);
	{error, Reason} ->
	    {error, Reason}
    end.

send_in_chunks(Info, Stream, Context, ErrorLog) ->
    case file:read(Stream, ?CHUNK_SIZE) of
	{ok, Data} ->
	    {ok, NewContext, NewErrorLog, ParsedBody}=
		parse(Info, Data, Context, ErrorLog, []),
	    httpd_socket:deliver(Info#mod.socket_type,
				 Info#mod.socket, ParsedBody),
	    send_in_chunks(Info,Stream,NewContext,NewErrorLog);
	eof ->
	    {ok, ErrorLog};
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% "Fuzzy" HTML parser
%%

parse(Info,Body) ->
  parse(Info, Body, ?DEFAULT_CONTEXT, [], []).

parse(Info, [], Context, ErrorLog, Result) ->
    {ok, Context, lists:reverse(ErrorLog), lists:reverse(Result)};
parse(Info,[$<,$!,$-,$-,$#|R1],Context,ErrorLog,Result) ->
  case catch parse0(R1,Context) of
    {parse_error,Reason} ->
      parse(Info,R1,Context,[{internal_info,?NICE(Reason)}|ErrorLog],
	    [$#,$-,$-,$!,$<|Result]);
    {ok,Context,Command,TagList,ValueList,R2} ->
      {ok,NewContext,NewErrorLog,MoreResult,R3}=
	handle(Info,Context,ErrorLog,Command,TagList,ValueList,R2),
      parse(Info,R3,NewContext,NewErrorLog,lists:reverse(MoreResult)++Result)
  end;
parse(Info,[$<,$!,$-,$-|R1],Context,ErrorLog,Result) ->
  {Comment,R2}=parse5(R1,[],0),
  parse(Info,R2,Context,ErrorLog,Comment++Result);
parse(Info,[C|R],Context,ErrorLog,Result) ->
  parse(Info,R,Context,ErrorLog,[C|Result]).

handle(Info,Context,ErrorLog,Command,TagList,ValueList,R) ->
  case catch apply(?MODULE,Command,[Info,Context,ErrorLog,TagList,ValueList,
				    R]) of
    {'EXIT',{undef,_}} ->
      throw({parse_error,"Unknown command "++atom_to_list(Command)++
	     " in parsed doc"});
    Result ->
      Result
  end.

parse0([],Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$-,$-,$>|R],Context) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse0([$ |R],Context) ->
  parse0(R,Context);
parse0(String,Context) ->
  parse1(String,Context,"").

parse1([],Context,Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$-,$-,$>|R],Context,Command) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse1([$ |R],Context,Command) ->
  parse2(R,Context,list_to_atom(lists:reverse(Command)),[],[],"");
parse1([C|R],Context,Command) ->
  parse1(R,Context,[C|Command]).

parse2([],Context,Command,TagList,ValueList,Tag) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse2([$-,$-,$>|R],Context,Command,TagList,ValueList,Tag) ->
  {ok,Context,Command,TagList,ValueList,R};
parse2([$ |R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,Tag);
parse2([$=|R],Context,Command,TagList,ValueList,Tag) ->
  parse3(R,Context,Command,[list_to_atom(lists:reverse(Tag))|TagList],
	 ValueList);
parse2([C|R],Context,Command,TagList,ValueList,Tag) ->
  parse2(R,Context,Command,TagList,ValueList,[C|Tag]).

parse3([],Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$-,$-,$>|R],Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse3([$ |R],Context,Command,TagList,ValueList) ->
  parse3(R,Context,Command,TagList,ValueList);
parse3([$"|R],Context,Command,TagList,ValueList) ->
  parse4(R,Context,Command,TagList,ValueList,"");
parse3(String,Context,Command,TagList,ValueList) ->
  throw({parse_error,"Premature EOF in parsed file"}).

parse4([],Context,Command,TagList,ValueList,Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$-,$-,$>|R],Context,Command,TagList,ValueList,Value) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse4([$"|R],Context,Command,TagList,ValueList,Value) ->
  parse2(R,Context,Command,TagList,[lists:reverse(Value)|ValueList],"");
parse4([C|R],Context,Command,TagList,ValueList,Value) ->
  parse4(R,Context,Command,TagList,ValueList,[C|Value]).

parse5([],Comment,Depth) ->
  throw({parse_error,"Premature EOF in parsed file"});
parse5([$<,$!,$-,$-|R],Comment,Depth) ->
  parse5(R,[$-,$-,$!,$<|Comment],Depth+1);
parse5([$-,$-,$>|R],Comment,0) ->
  {">--"++Comment++"--!<",R};
parse5([$-,$-,$>|R],Comment,Depth) ->
  parse5(R,[$>,$-,$-|Comment],Depth-1);
parse5([C|R],Comment,Depth) ->
  parse5(R,[C|Comment],Depth).
