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
-module(httpd_util).
-export([key1search/2, key1search/3, lookup/2, lookup/3, multi_lookup/2,
	 lookup_mime/2, lookup_mime/3, lookup_mime_default/2,
	 lookup_mime_default/3, reason_phrase/1, message/3, rfc1123_date/0,
	 rfc1123_date/1, day/1, month/1, decode_hex/1, decode_base64/1, encode_base64/1,
	 flatlength/1, split_path/1, split_script_path/1, suffix/1, to_upper/1,
	 to_lower/1, split/3, header/1, header/2, header/3, uniq/1]).

-include("httpd.hrl").

%% key1search

key1search(TupleList,Key) ->
  key1search(TupleList,Key,undefined).

key1search(TupleList,Key,Undefined) ->
  case lists:keysearch(Key,1,TupleList) of
    {value,{Key,Value}} ->
      Value;
    false ->
      Undefined
  end.

%% lookup

lookup(Table,Key) ->
  lookup(Table,Key,undefined).

lookup(Table,Key,Undefined) ->
    case ets:lookup(Table,Key) of
	[] ->
	    Undefined;
	[{Key,Value}|_] ->
	    Value
    end.

%% multi_lookup

multi_lookup(Table,Key) ->
    remove_key(ets:lookup(Table,Key)).

remove_key([]) ->
    [];
remove_key([{Key,Value}|Rest]) ->
    [Value|remove_key(Rest)].

%% lookup_mime

lookup_mime(ConfigDB,Suffix) ->
    lookup_mime(ConfigDB,Suffix,undefined).

lookup_mime(ConfigDB,Suffix,Undefined) ->
    [{mime_types,MimeTypesDB}|_]=ets:lookup(ConfigDB,mime_types),
    case ets:lookup(MimeTypesDB,Suffix) of
	[] ->
	    Undefined;
	[{Suffix,MimeType}|_] ->
	    MimeType
    end.

%% lookup_mime_default

lookup_mime_default(ConfigDB,Suffix) ->
    lookup_mime_default(ConfigDB,Suffix,undefined).

lookup_mime_default(ConfigDB,Suffix,Undefined) ->
    [{mime_types,MimeTypesDB}|_]=ets:lookup(ConfigDB,mime_types),
    case ets:lookup(MimeTypesDB,Suffix) of
	[] ->
	    case ets:lookup(ConfigDB,default_type) of
		[] ->
		    Undefined;
		[{default_type,DefaultType}|_] ->
		    DefaultType
	    end;
	[{Suffix,MimeType}|_] ->
	    MimeType
    end.

%% reason_phrase

reason_phrase(200) -> "OK";
reason_phrase(201) -> "Created";
reason_phrase(202) -> "Accepted";
reason_phrase(204) -> "No Content";
reason_phrase(301) -> "Moved Permanently";
reason_phrase(302) -> "Moved Temporarily";
reason_phrase(304) -> "Not Modified";
reason_phrase(400) -> "Bad Request";
reason_phrase(401) -> "Unauthorized";
reason_phrase(403) -> "Forbidden";
reason_phrase(404) -> "Not Found";
reason_phrase(500) -> "Internal Server Error";
reason_phrase(501) -> "Not Implemented";
reason_phrase(502) -> "Bad Gateway";
reason_phrase(504) -> "Service Unavailable";
reason_phrase(_) -> "Internal Server Error".

%% message

message(301,URL,_) ->
  "The document has moved <A HREF=\""++URL++"\">here</A>.";
message(304,URL,_) ->
    "The document has not been changed.";
message(400,none,_) ->
  "Your browser sent a query that this server could not understand.";
message(401,none,_) ->
  "This server could not verify that you
are authorized to access the document you
requested.  Either you supplied the wrong
credentials (e.g., bad password), or your
browser doesn't understand how to supply
the credentials required.";
message(403,RequestURI,_) ->
  "You don't have permission to access "++RequestURI++" on this server.";
message(404,RequestURI,_) ->
  "The requested URL "++RequestURI++" was not found on this server.";
message(500,none,ConfigDB) ->
  ServerAdmin=lookup(ConfigDB,server_admin,"unknown@unknown"),
  "The server encountered an internal error or
misconfiguration and was unable to complete
your request.
<P>Please contact the server administrator "++ServerAdmin++",
and inform them of the time the error occurred
and anything you might have done that may have
caused the error.";
message(501,{Method,RequestURI,HTTPVersion},ConfigDB) ->
  Method++" to "++RequestURI++" ("++HTTPVersion++") not supported.";
message(504,String,ConfigDB) ->
  "This service in unavailable due to: "++String.

%% rfc1123_date

rfc1123_date() ->
  {{YYYY,MM,DD},{Hour,Min,Sec}}=calendar:universal_time(),
  DayNumber=calendar:day_of_the_week({YYYY,MM,DD}),
  lists:flatten(io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                        [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

rfc1123_date({{YYYY,MM,DD},{Hour,Min,Sec}}) ->
  DayNumber=calendar:day_of_the_week({YYYY,MM,DD}),
  lists:flatten(io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
                        [day(DayNumber),DD,month(MM),YYYY,Hour,Min,Sec])).

%% uniq

uniq([]) ->
    [];
uniq([First,First|Rest]) ->
    uniq([First|Rest]);
uniq([First|Rest]) ->
    [First|uniq(Rest)].


%% day

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat"; 
day(7) -> "Sun".

%% month

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% decode_hex

decode_hex([$%,Hex1,Hex2|Rest]) ->
  [hex2dec(Hex1)*16+hex2dec(Hex2)|decode_hex(Rest)];
decode_hex([First|Rest]) ->
  [First|decode_hex(Rest)];
decode_hex([]) ->
  [].

hex2dec(X) when X>=$0,X=<$9 -> X-$0;
hex2dec(X) when X>=$A,X=<$F -> X-$A+10;
hex2dec(X) when X>=$a,X=<$f -> X-$a+10.

%% decode_base64 (DEBUG STRING: QWxhZGRpbjpvcGVuIHNlc2FtZQ==)

decode_base64([]) ->
  [];
decode_base64([Sextet1,Sextet2,$=,$=|Rest]) ->
  Bits2x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12),
  Octet1=Bits2x6 bsr 16,
  [Octet1|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,$=|Rest]) ->
  Bits3x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6),
  Octet1=Bits3x6 bsr 16,
  Octet2=(Bits3x6 bsr 8) band 16#ff,
  [Octet1,Octet2|decode_base64(Rest)];
decode_base64([Sextet1,Sextet2,Sextet3,Sextet4|Rest]) ->
  Bits4x6=
    (d(Sextet1) bsl 18) bor
    (d(Sextet2) bsl 12) bor
    (d(Sextet3) bsl 6) bor
    d(Sextet4),
  Octet1=Bits4x6 bsr 16,
  Octet2=(Bits4x6 bsr 8) band 16#ff,
  Octet3=Bits4x6 band 16#ff,
  [Octet1,Octet2,Octet3|decode_base64(Rest)];
decode_base64(CatchAll) ->
  "BAD!".

d(X) when X >= $A, X =<$Z ->
    X-65;
d(X) when X >= $a, X =<$z ->
    X-71;
d(X) when X >= $0, X =<$9 ->
    X+4;
d($+) -> 62;
d($/) -> 63;
d(_) -> 63.


encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X>25, X<52 ->     X+71;
e(X) when X>51, X<62 ->     X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X) ->                     exit({bad_encode_base64_token, X}).


%% flatlength

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T],L) when list(H) ->
    flatlength(H,flatlength(T,L));
flatlength([H|T],L) when binary(H) ->
    flatlength(T,L+size(H));
flatlength([H|T],L) ->
    flatlength(T,L+1);
flatlength([],L) ->
  L.

%% split_path

split_path(Path) ->
    case regexp:match(Path,"[\?].*\$") of
	%% A QUERY_STRING exists!
	{match,Start,Length} ->
	    {httpd_util:decode_hex(string:substr(Path,1,Start-1)),
	     string:substr(Path,Start,Length)};
	%% A possible PATH_INFO exists!
	nomatch ->
	    split_path(Path,[])
    end.

split_path([],SoFar) ->
    {httpd_util:decode_hex(lists:reverse(SoFar)),[]};
split_path([$/|Rest],SoFar) ->
    Path=httpd_util:decode_hex(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type == regular ->
	    {Path,[$/|Rest]};
	{ok,FileInfo} ->
	    split_path(Rest,[$/|SoFar]);
	{error,Reason} ->
	    split_path(Rest,[$/|SoFar])
    end;
split_path([C|Rest],SoFar) ->
    split_path(Rest,[C|SoFar]).

%% split_script_path

split_script_path(Path) ->
    case split_script_path(Path, []) of
	{Script, AfterPath} ->
	    {PathInfo, QueryString} = pathinfo_querystring(AfterPath),
	    {Script, {PathInfo, QueryString}};
	not_a_script ->
	    not_a_script
    end.

pathinfo_querystring(Str) ->
    pathinfo_querystring(Str, []).
pathinfo_querystring([], SoFar) ->
    {lists:reverse(SoFar), []};
pathinfo_querystring([$?|Rest], SoFar) ->
    {lists:reverse(SoFar), Rest};
pathinfo_querystring([C|Rest], SoFar) ->
    pathinfo_querystring(Rest, [C|SoFar]).

split_script_path([$?|QueryString], SoFar) ->
    Path = httpd_util:decode_hex(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type == regular ->
	    {Path, [$?|QueryString]};
	{ok,FileInfo} ->
	    not_a_script;
	{error,Reason} ->
	    not_a_script
    end;
split_script_path([], SoFar) ->
    Path = httpd_util:decode_hex(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok,FileInfo} when FileInfo#file_info.type == regular ->
	    {Path, []};
	{ok,FileInfo} ->
	    not_a_script;
	{error,Reason} ->
	    not_a_script
    end;
split_script_path([$/|Rest], SoFar) ->
    Path = httpd_util:decode_hex(lists:reverse(SoFar)),
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type == regular ->
	    {Path, [$/|Rest]};
	{ok, _FileInfo} ->
	    split_script_path(Rest, [$/|SoFar]);
	{error, _Reason} ->
	    split_script_path(Rest, [$/|SoFar])
    end;
split_script_path([C|Rest], SoFar) ->
    split_script_path(Rest,[C|SoFar]).

%% suffix

suffix(Path) ->
    case filename:extension(Path) of
	[] ->
	    [];
	Extension ->
	    tl(Extension)
    end.

%% to_upper

to_upper([C|Cs]) when C >= $a, C =< $z ->
    [C-($a-$A)|to_upper(Cs)];
to_upper([C|Cs]) ->
    [C|to_upper(Cs)];
to_upper([]) ->
    [].

%% to_lower

to_lower([C|Cs]) when C >= $A, C =< $Z ->
    [C+($a-$A)|to_lower(Cs)];
to_lower([C|Cs]) ->
    [C|to_lower(Cs)];
to_lower([]) ->
    [].

%% split

split(String,RegExp,Limit) ->
    case regexp:parse(RegExp) of
	{error,Reason} ->
	    {error,Reason};
	{ok,_} ->
	    {ok,do_split(String,RegExp,Limit)}
    end.

do_split(String,RegExp,1) ->
    [String];

do_split(String,RegExp,Limit) ->
    case regexp:first_match(String,RegExp) of 
	{match,Start,Length} ->
	    [string:substr(String,1,Start-1)|
	     do_split(lists:nthtail(Start+Length-1,String),RegExp,Limit-1)];
	nomatch ->
	    [String]
    end.

%% header



header(StatusCode) when integer(StatusCode) ->
    Date = rfc1123_date(),
    io_lib:format("HTTP/1.0 ~w ~s \r\nDate: ~s\r\nServer: ~s\r\n"
		  "Connection: close\r\n",
		  [StatusCode, httpd_util:reason_phrase(StatusCode),
		   Date, ?SERVER_SOFTWARE]).

header(StatusCode, Info) when integer(StatusCode), record(Info, mod) ->
    Date = rfc1123_date(),
    Connection = 
	case Info#mod.connection of
	    keep_alive ->
		"Keep-Alive";
	    _ ->
		"close"
	end,
    io_lib:format("HTTP/1.0 ~w ~s \r\nDate: ~s\r\nServer: ~s\r\n"
		  "Connection: ~s\r\n",
		  [StatusCode, httpd_util:reason_phrase(StatusCode),
		   Date, ?SERVER_SOFTWARE, Connection]);

header(StatusCode, MimeType) when integer(StatusCode) ->
    header(StatusCode, MimeType, rfc1123_date()).

header(StatusCode, MimeType, Info) when integer(StatusCode), record(Info, mod) ->
    Date = rfc1123_date(),
    Connection = 
	case Info#mod.connection of
	    keep_alive ->
		"Keep-Alive";
	    _ ->
		"close"
	end,
    io_lib:format("HTTP/1.0 ~w ~s \r\nDate: ~s\r\nServer: ~s\r\n"
		  "Content-Type: ~s\r\nConnection: ~s\r\n",
		  [StatusCode, httpd_util:reason_phrase(StatusCode),
		   Date, ?SERVER_SOFTWARE, MimeType, Connection]);

header(StatusCode, MimeType, Date) ->
    io_lib:format("HTTP/1.0 ~w ~s \r\nDate: ~s\r\nServer: ~s\r\n"
		  "Content-Type: ~s\r\nConnection: close\r\n",
		  [StatusCode, httpd_util:reason_phrase(StatusCode),
		   Date, ?SERVER_SOFTWARE, MimeType]).

