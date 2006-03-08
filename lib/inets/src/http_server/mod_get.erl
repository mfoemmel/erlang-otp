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

do(ModData) ->
    ?DEBUG("do -> entry",[]),
    case ModData#mod.method of
	"GET" ->
	    case httpd_util:key1search(ModData#mod.data,status) of
		%% A status code has been generated!
		{_StatusCode, _PhraseArgs, _Reason} ->
		    {proceed,ModData#mod.data};
		%% No status code has been generated!
		undefined ->
		    case httpd_util:key1search(ModData#mod.data,response) of
			%% No response has been generated!
			undefined ->
			    do_get(ModData);
			%% A response has been generated or sent!
			_Response ->
			    {proceed,ModData#mod.data}
		    end
	    end;
	%% Not a GET method!
	_ ->
	    {proceed,ModData#mod.data}
    end.


do_get(ModData) ->
    ?DEBUG("do_get -> Request URI: ~p",[ModData#mod.request_uri]),
    Path = mod_alias:path(ModData#mod.data, ModData#mod.config_db, 
			  ModData#mod.request_uri),
    {FileInfo, LastModified} = get_modification_date(Path),

    send_response(ModData#mod.socket,ModData#mod.socket_type, Path, ModData, 
		  FileInfo, LastModified).


%% The common case when no range is specified
send_response(_Socket, _SocketType, Path, ModData, FileInfo, LastModified)->
    %% Send the file!
    %% Find the modification date of the file
    case file:open(Path,[raw,binary]) of
	{ok, FileDescriptor} ->
	    ?DEBUG("do_get -> FileDescriptor: ~p",[FileDescriptor]),
	    Suffix = httpd_util:suffix(Path),
	    MimeType = httpd_util:lookup_mime_default(ModData#mod.config_db,
						      Suffix,"text/plain"),
	    %% FileInfo = file:read_file_info(Path),
	    Size = integer_to_list(FileInfo#file_info.size),
	    Headers = case ModData#mod.http_version of
			 "HTTP/1.1" ->
			      [{content_type, MimeType},
			       {etag, httpd_util:create_etag(FileInfo)},
			       {content_length, Size}|LastModified];
			  %% OTP-4935
			 _ ->
			     %% i.e http/1.0 and http/0.9
			      [{content_type, MimeType},
			       {content_length, Size}|LastModified]
			  end,
	    send(ModData, 200, Headers, FileDescriptor),
	    file:close(FileDescriptor),
	    {proceed,[{response,{already_sent,200,
				 FileInfo#file_info.size}},
		      {mime_type,MimeType}|ModData#mod.data]};
	{error, Reason} ->
	    {proceed,
	     [{status,open_error(Reason,ModData,Path)}|ModData#mod.data]}
    end.

%% send

send(#mod{socket = Socket, socket_type = SocketType} = ModData,
     StatusCode, Headers, FileDescriptor) ->
    ?DEBUG("send -> send header",[]),
    httpd_response:send_header(ModData, StatusCode, Headers),
    send_body(SocketType,Socket,FileDescriptor).


send_body(SocketType,Socket,FileDescriptor) ->
    case file:read(FileDescriptor,?FILE_CHUNK_SIZE) of
	{ok,Binary} ->
	    ?DEBUG("send_body -> send another chunk: ~p",[size(Binary)]),
	    case httpd_socket:deliver(SocketType,Socket,Binary) of
		socket_closed ->
		    ?LOG("send_body -> socket closed while sending",[]),
		    socket_close;
		_ ->
		    send_body(SocketType,Socket,FileDescriptor)
	    end;
	eof ->
	    ?DEBUG("send_body -> done with this file",[]),
	    eof
    end.
    

%% open_error - Handle file open failure
%%
open_error(eacces,ModData,Path) ->
    open_error(403,ModData,Path,"");
open_error(enoent,ModData,Path) ->
    open_error(404,ModData,Path,"");
open_error(enotdir,ModData,Path) ->
    open_error(404,ModData,Path,
	       ": A component of the file name is not a directory");
open_error(emfile,_ModData,Path) ->
    open_error(500,none,Path,": To many open files");
open_error({enfile,_},_ModData,Path) ->
    open_error(500,none,Path,": File table overflow");
open_error(_Reason,_ModData,Path) ->
    open_error(500,none,Path,"").
	    
open_error(StatusCode,none,Path,Reason) ->
    {StatusCode,none,?NICE("Can't open "++Path++Reason)};
open_error(StatusCode,ModData,Path,Reason) ->
    {StatusCode,ModData#mod.request_uri,?NICE("Can't open "++Path++Reason)}.

get_modification_date(Path)->
    {ok, FileInfo0} = file:read_file_info(Path), 
    LastModified = case catch httpd_util:rfc1123_date(FileInfo0#file_info.mtime) of
		       Date when is_list(Date) -> [{last_modified, Date}];
		       _ -> []
		   end,
    {FileInfo0, LastModified}.
