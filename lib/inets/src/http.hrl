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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%% 
%%

-define(HTTP_REQUEST_TIMEOUT, infinity).
-define(TCP_PIPELINE_LENGTH,3).
-define(MAX_TCP_SESSIONS,400).
-define(MAX_BODY_SIZE, nolimit).
-define(MAX_HEADER_SIZE, 10240).
-define(MAX_REDIRECTS, 4).

-define(CR, $\r).
-define(LF, $\n).
-define(CRLF, [$\r,$\n]).
-define(SP, $\s).
-define(TAB, $\t).

-record(tcp_session,{
	  id,           % {{Host, Port}, HandlerPid}
	  client_close, % true | false
	  scheme,       % http (HTTP/TCP) | https (HTTP/SSL/TCP)
	  socket,       % Open socket, used by connection
	  pipeline_length = 1 % Current length of pipeline 
	 }).


%%% All data associated to a specific HTTP request
-record(request,{
	  id,            % ref() - Request Id
	  from,          % pid() - Caller
	  redircount = 0,% Number of redirects made for this request
	  scheme,        % http | https 
	  address,       % ({Host,Port}) Destination Host and Port
	  path,          % string() - Path of parsed URL
	  pquery,        % string() - Rest of parsed URL
	  method,        % atom() - HTTP request Method
	  headers,       % list() - Key/Value list with Headers
	  content,       % {ContentType, Body} - Current HTTP request
	  settings       % #client_settings{} - User defined settings
	 }).

-record(response,{
	  scheme,      % http | https 
	  socket,      % Open socket, used by connection
	  status,
	  version,     % int() - HTTP minor version number, e.g. 0 or 1
	  headers=#http_response_h{},
	  body = <<>>
	 }).

%%% Response headers
-record(http_response_h,{
%%% --- Standard "General" headers
 	  cache_control,
 	  connection,
 	  date,
 	  pragma,
 	  trailer,
 	  transfer_encoding,
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Request" headers
 	  accept_ranges,
 	  age,
 	  etag,
 	  location,
 	  proxy_authenticate,
 	  retry_after,
 	  server,
 	  vary,
 	  www_authenticate,
%%% --- Standard "Entity" headers
 	  allow,
 	  content_encoding,
 	  content_language,
 	  content_length = "0",
 	  content_location,
	  content_md5,
 	  content_range,
 	  content_type,
 	  expires,
 	  last_modified,
	  other=[]        % list() - Key/Value list with other headers
	 }).


%%% Request headers
-record(http_request_h,{
%%% --- Standard "General" headers
 	  cache_control,
 	  connection="keep-alive",
 	  date,
 	  pragma,
 	  trailer,
 	  transfer_encoding,
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Request" headers
 	  accept,
 	  accept_charset,
 	  accept_encoding,
 	  accept_language,
 	  authorization,
 	  expect, 
 	  from,
 	  host,
 	  if_match,
 	  if_modified_since,
 	  if_none_match,
 	  if_range,
 	  if_unmodified_since,
 	  max_forwards,
	  proxy_authenticate, 
 	  range,
 	  referer,
 	  te, 
 	  user_agent,
%%% --- Standard "Entity" headers
	  allow,
 	  content_encoding,
 	  content_language,
 	  content_length = "0",
 	  content_location,
 	  content_md5,
 	  content_range,
 	  content_type,
	  expires,
 	  last_modified,
	  other=[]        % list() - Key/Value list with other headers
	 }).


-record(http_cookie,{
	  key,  % {Domain,Path} tuple specifying validity of cookie
	  name,
	  value,
	  comment,
	  comment_url,
	  port,
	  discard,
	  maxage,
	  secure,
	  version
	 }).


%%% HTTP Client settings
-record(http_options,{
	  %% Milliseconds before a request times out
	  timeout = ?HTTP_REQUEST_TIMEOUT,                 
	  %% bool() - True if automatic redirection on 30X responses.
	  autoredirect = true, 
	  ssl = [], % Ssl socket options
	  cookie = false, % bool() - cookies are not yet supported, keep ??
	  relaxed = false % bool() true if not strictly standard compliant
	 }).



