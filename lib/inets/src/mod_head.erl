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
-module(mod_head).
-export([do/1]).

-include("httpd.hrl").

%% do

do(Info) ->
  case Info#mod.method of
    "HEAD" ->
      case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	  {proceed,Info#mod.data};
	%% No status code has been generated!
	undefined ->
	  case httpd_util:key1search(Info#mod.data,response) of
	    %% No response has been generated!
	    undefined ->
	      Path=mod_alias:path(Info#mod.data,Info#mod.config_db,
				  Info#mod.request_uri),
	      Suffix=httpd_util:suffix(Path),
	      %% Does the file exists?
	      case file:read_file_info(Path) of
		{ok,FileInfo} ->
		  MimeType=httpd_util:lookup_mime_default(Info#mod.config_db,
							  Suffix,"text/plain"),
		  Response=["Content-Type: ",MimeType,"\r\n",
			    "Content-Length: ",
			    integer_to_list(FileInfo#file_info.size),
			    "\r\n\r\n"],
		  {proceed,[{response,{200,Response}}|Info#mod.data]};
		{error,Reason} ->
		  {proceed,[{status,{404,Info#mod.request_uri,
				     ?NICE("Can't open "++Path)}}|
			    Info#mod.data]}
	      end;
	    %% A response has been sent! Nothing to do about it!
	    {already_sent,StatusCode,Size} ->
	      {proceed,Info#mod.data};
	    %% A response has been generated!
	    {StatusCode,Response} ->
	      MimeType=httpd_util:key1search(Info#mod.data,mime_type,
					     "text/plain"),
	      NewResponse=["Content-type: ",MimeType,"\r\n",
			   "Content-length: ",
			   integer_to_list(httpd_util:flatlength(Response)),
			   "\r\n\r\n"],
	      {proceed,lists:keyreplace(response,1,Info#mod.data,
					{response,{StatusCode,NewResponse}})}
	  end
      end;
    %% Not a HEAD method!
    _ ->
      {proceed,Info#mod.data}
  end.
