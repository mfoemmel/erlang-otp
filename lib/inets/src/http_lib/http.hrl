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
-define(HTTP_PIPELINE_TIMEOUT, infinity).
-define(HTTP_PIPELINE_LENGTH, 2).
-define(HTTP_MAX_TCP_SESSIONS, 2).
-define(HTTP_MAX_BODY_SIZE, nolimit).
-define(HTTP_MAX_HEADER_SIZE, 10240).
-define(HTTP_MAX_REDIRECTS, 4).

-define(CR, $\r).
-define(LF, $\n).
-define(CRLF, [$\r,$\n]).
-define(SP, $\s).
-define(TAB, $\t).

-define(COOKIE_MAX_AGE, httpc_manager_life_time).

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
	  headers,       % #http_request_h{}
	  content,       % {ContentType, Body} - Current HTTP request
	  settings,      % #client_settings{} - User defined settings
	  abs_uri        % string() ex: "http://www.erlang.org"
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
 	  'cache-control',
 	  connection,
 	  date,
 	  pragma,
 	  trailer,
 	  'transfer-encoding',
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Response" headers
 	  'accept-ranges',
 	  age,
 	  etag,
 	  location,
 	  'proxy-authenticate',
 	  'retry-after',
 	  server,
 	  vary,
 	  'www-authenticate',
%%% --- Standard "Entity" headers
 	  allow,
 	  'content-encoding',
 	  'content-language',
 	  'content-length' = "0",
 	  'content-location',
	  'content-md5',
 	  'content-range',
 	  'content-type',
 	  expires,
 	  'last-modified',
	  other=[]        % list() - Key/Value list with other headers
	 }).


%%% Request headers
-record(http_request_h,{
%%% --- Standard "General" headers
 	  'cache-control',
 	  connection = "keep-alive",
 	  date,
 	  pragma,
 	  trailer,
 	  'transfer-encoding',
 	  upgrade,
 	  via,
 	  warning,
%%% --- Standard "Request" headers
 	  accept,
 	  'accept-charset',
 	  'accept-encoding',
 	  'accept-language',
 	  authorization,
 	  expect, 
 	  from,
 	  host,
 	  'if-match',
 	  'if-modified-since',
 	  'if-none-match',
 	  'if-range',
 	  'if-unmodified-since',
 	  'max-forwards',
	  'proxy-authenticate', 
 	  range,
 	  referer,
 	  te, 
 	  'user-agent',
%%% --- Standard "Entity" headers
	  allow,
 	  'content-encoding',
 	  'content-language',
 	  'content-length' = "0",
	  'content-location',
 	  'content-md5',
 	  'content-range',
 	  'content-type',
	  expires,
 	  'last-modified',
	  other=[]        % list() - Key/Value list with other headers
	 }).

-record(http_cookie,{
	  domain,
	  domain_default = false,
	  name,
	  value,
	  comment,
	  max_age = session,
	  path, 
	  path_default = false,
	  secure = false,
	  version = "0" 
	 }).

%%% HTTP Client per request settings
-record(http_options,{
	  %% Milliseconds before a request times out
	  timeout = ?HTTP_REQUEST_TIMEOUT,  
	  %% bool() - True if automatic redirection on 30X responses.
	  autoredirect = true, 
	  ssl = [], % Ssl socket options
	  relaxed = false % bool() true if not strictly standard compliant
	 }).

%%% HTTP Client per profile setting. Currently there is only one profile.
-record(options, {
	  proxy =  {undefined, []}, % {{ProxyHost, ProxyPort}, [NoProxy]},
	  pipeline_timeout = ?HTTP_PIPELINE_LENGTH,
	  max_pipeline_length = ?HTTP_PIPELINE_LENGTH,
	  max_sessions =  ?HTTP_MAX_TCP_SESSIONS,
	  cookies = disabled % enabled | disabled | verify
	 }).






