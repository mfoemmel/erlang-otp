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
-module(mod_get).
-export([do/1]).

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
		    case httpd_util:key1search(Info#mod.data,response) of
			%% No response has been generated!
			undefined ->
			    do_get(Info);
			%% A response has been generated or sent!
			Response ->
			    {proceed,Info#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,Info#mod.data}
    end.


do_get(Info) ->
    Path = mod_alias:path(Info#mod.data, Info#mod.config_db, Info#mod.request_uri),

    %% Find the modification date of the file.
    {FileInfo, LastModified} =
	case file:read_file_info(Path) of
	    {ok, FileInfo0} ->
		{FileInfo0, httpd_util:rfc1123_date(FileInfo0#file_info.mtime)};
	    _ ->
		{#file_info{},""}
	end,

    SendIt = 
	case strip_date(httpd_util:key1search(Info#mod.parsed_header,
						"If-Modified-Since")) of
	    undefined ->
		true;
	    LastModified ->
		false;
	    Other ->
		true
	end,
    case SendIt of
	true ->
	    %% Send the file!
	    case file:open(Path, [raw,binary]) of
		{ok, FileDescriptor} ->
		    Suffix = httpd_util:suffix(Path),
		    MimeType = httpd_util:lookup_mime_default(Info#mod.config_db,
							      Suffix,"text/plain"),
		    Date = httpd_util:rfc1123_date(),
		    Size = integer_to_list(FileInfo#file_info.size),
		    Header = [httpd_util:header(200, MimeType, Info),
			      "Last-Modified: ", LastModified, "\r\n",
			      "Content-length: ",Size,"\r\n", 
			      "\r\n"],
		    send(Info#mod.socket_type, Info#mod.socket,
			 Header, FileDescriptor),
		    file:close(FileDescriptor),
		    {proceed,[{response,{already_sent,200,
					 FileInfo#file_info.size}},
			      {mime_type,MimeType}|Info#mod.data]};
		{error, Reason} ->
		    {proceed,[{status,{404,Info#mod.request_uri,
				       ?NICE("Can't open "++Path)}}|
			      Info#mod.data]}
	    end;
	false ->
	    {proceed, [{status, {304, Info#mod.request_uri, not_modified}}]}
    end.


%% IE4 & NS4 sends an extra '; length=xxxx' string at the end of the If-Modified-Since
%% header, we detect this and ignore it (the RFCs does not mention this).
strip_date(undefined) ->
    undefined;
strip_date([]) ->
    [];
strip_date([$;,$ |Rest]) ->
    [];
strip_date([C|Rest]) ->
    [C|strip_date(Rest)].

%% send

send(SocketType,Socket,Header,FileDescriptor) ->
  case httpd_socket:deliver(SocketType,Socket,Header) of
    socket_closed ->
      socket_close;
    _ ->
      send_body(SocketType,Socket,FileDescriptor)
  end.

send_body(SocketType,Socket,FileDescriptor) ->
  case file:read(FileDescriptor,?FILE_CHUNK_SIZE) of
    {ok,Binary} ->
      case httpd_socket:deliver(SocketType,Socket,Binary) of
	socket_closed ->
	  socket_close;
	_ ->
	  send_body(SocketType,Socket,FileDescriptor)
      end;
    eof ->
      eof
  end.
