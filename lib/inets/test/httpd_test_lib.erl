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
-module(httpd_test_lib).

-include("inets_test_lib.hrl").


%% Poll functions
-export([ppoll/6, epoll/6, poll/6, poll/7, poll/9, cpoll/6, cpoll/7, cpoll/9]).
-export([validate_ppoll/8]).

-export([end_of_header/1]).


%%----------------------------------------------------------------------
%% Persistent poll functions
%%
ppoll(Mode, Host, Port, Node, Request, Options) ->
    ?DEBUG("ppoll(~p) -> "
	   "~n   Host:    ~s"
	   "~n   Port:    ~p"
	   "~n   Request: ~p"
	   "~n   Options: ~p",
	   [Mode,Host,Port,Request,Options]),
    {ok, Socket} = inets_test_lib:connect(Mode, Host, Port),
    ?DEBUG("ppoll -> Socket = ~p",[Socket]),
    inets_test_lib:send(Mode, Socket, Request),
    ?DEBUG("ppoll -> Sent, now validate",[]),
    Res = validate_ppoll(Request,Socket,[],[], Options, Node, Port, head),
    ?DEBUG("ppoll -> Validation result: ~p",[Res]),
    inets_test_lib:close(Mode,Socket),
    Res.


validate_ppoll(Request, Socket,Head,Body, Options, Node, Port, head) ->
        ?DEBUG("validate_ppoll -> "
	       "~n   Socket:     ~p"
	       "~n   HeadSoFar:  ~p",
	   [Socket,Head]),
    receive 
	%%
	%% TCP
	%%
	{tcp, Socket, Data} ->
	    case end_of_header(Data) of
		{true,Head1,Body1,Size}->
		    validate_ppoll(Request, Socket, 
				   Head ++ Head1, 
				   Body ++ Body1, 
				   Options, Node, Port, Size);
		false ->
		    validate_ppoll(Request, Socket, 
				   Head ++ lists:reverse(Data),
				   Body, 
				   Options, Node, Port, head)
	    end;
	{tcp_closed, Socket} ->
	    {error, closed};
	{tcp_error, Socket, Reason} ->
	    {error, Reason};

	%%
	%% SSL
	%%
	{ssl, Socket, Data} ->
	    case end_of_header(Data) of
		{true, Head1, Body1, Size} ->
		    validate_ppoll(Request, Socket,
				   Head ++ Head1,
				   Body ++ Body1, 
				   Options, Node, Port, Size);
		false ->
		    validate_ppoll(Request, Socket,
				   Head ++ lists:reverse(Data),
				   Body, 
				   Options, Node, Port, head)
	    end;
	{ssl_closed, Socket} ->
	    {error, closed};
	{ssl_error, Socket, Reason} ->
	    {error, Reason}

    end;
validate_ppoll(Request, Socket, Head, Body, Options, Node, Port, Size) 
  when Size > 0, Size < length(Body) ->
    ?DEBUG("validate_ppoll->"
	   "~n   Socket:       ~p"
	   "~n   Head:         ~p"
	   "~n   BodySoFar:    ~p"
	   "~n   ExpectedSize: ~p"
	   "~n   RetrievedSize:~p",
	   [Socket, Head, Body, Size, length(Body)]),
    receive 
	%%
	%% TCP
	%%
	{tcp, Socket, Data} ->
	    validate_ppoll(Request, Socket, 
			   Head, Body ++ Data, 
			   Options, Node, Port, Size);
	{tcp_closed, Socket} ->
	    {error, closed};
	{tcp_error, Socket, Reason} ->
	    {error, Reason};
       
	%%
	%% SSL
	%%
	{ssl, Socket, Data} ->
	    validate_ppoll(Request, Socket,
			   Head, Body ++ Data, 
			   Options, Node, Port,Size);
	{ssl_closed, Socket} ->
	    {error, closed};
	{ssl_error, Socket, Reason} ->
	    {error, Reason}
	
    end;

validate_ppoll(Request, Socket, Head, Body, Options, Node, Port, Size) ->
    ?DEBUG("validate_ppoll -> "
	   "~n   Socket:    ~p"
	   "~n   WholeHead: ~p"
	   "~n   WholeBody: ~p",
	   [Socket,Head,Body]),
    validate_epoll_options(Head, Options, Node, Port).



%% ----------------------------------------------------------------------
%% Extended poll functions
%%

epoll(Mode, Host, Port, Node, Request, Options) ->
    ?DEBUG("epoll -> connect to server: ~p:~p", [Host, Port]),
    case ?CONNECT(Mode, Host, Port) of
	{ok, Socket} ->
	    ?DEBUG("epoll -> send request", []),
	    case ?SEND(Mode, Socket, Request) of
		ok ->
		    validate_epoll(Request, Socket, [], Options, Node, Port);
		Error ->
		    ?FAIL({send_failed, Error})
	    end;
	{error, Reason} ->
	    ?FAIL({connection_faild, Reason})
    end.
	    

validate_epoll(Request, Socket, SoFar, Options, N, P) ->
    ?DEBUG("validate_epoll -> await server response", []),
    receive
	%%
	%% TCP
	%%
	{tcp, Socket, Data} ->
	    ?DEBUG("validate_epoll -> (tcp) got data", []),
	    case terminated_header(SoFar++Data) of
		{true, Header, Stuff} ->
		    trash_the_rest(Socket,Header,Request,length(Stuff)),
		    validate_epoll_options(Header,Options,N,P);
		false ->
		    validate_epoll(Request,Socket,SoFar++Data,Options,N,P)
	    end;
	{tcp_closed, Socket} ->
	    ?DEBUG("validate_epoll -> (tcp) socket closed",[]),
	    validate_epoll_options(SoFar,Options,N,P);
	{tcp_error, Socket, Reason} ->
	    ?DEBUG("validate_epoll -> (tcp) socket error: ~p",[Reason]),
	    validate_epoll_options(SoFar,Options,N,P);
	
	%%
	%% SSL
	%%
	{ssl, Socket, Data} ->
	    ?DEBUG("validate_epoll -> (ssl) received message",[]),
	    case terminated_header(SoFar++Data) of
		{true, Header, Stuff} ->
		    trash_the_rest(Socket,Header,Request,length(Stuff)),
		    validate_epoll_options(Header, Options, N, P);
		false ->
		    validate_epoll(Request,Socket,SoFar++Data,Options,N,P)
	    end;
	{ssl_closed, Socket} ->
	    ?DEBUG("validate_epoll -> (ssl) socket closed",[]),
	    validate_epoll_options(SoFar,Options,N,P);
	{ssl_error, Socket, Reason} ->
	    ?DEBUG("validate_epoll -> (ssl) socket error: ~p",[Reason]),
	    validate_epoll_options(SoFar,Options,N,P)

    after 30000 ->
	    ?FLUSH(),
	    ?FAIL(connection_timed_out)
    end.

validate_epoll_options(Data, Options, N, P) ->
    ?DEBUG("validate_epoll_options -> entry with"
	   "~n   length(Data): ~p"
	   "n    Data: ~p", [length(Data), Data]),
    {Status, Header} = parse_reply(Data),
    {value, {statuscode, StatusCode}} = lists:keysearch(statuscode,1,Status),
    case lists:keysearch(statuscode, 1, Options) of
	{value, {statuscode, StatusCode}} ->
	    ok;
	false ->
	    ok;
	{value, {statuscode, OtherStatus}} ->
	    ?FAIL({bad_response, lists:flatten(Data)});
	O ->
	    ?FAIL({bad_response, lists:flatten(Data)})
    end,
    validate_epoll_options1(Header,Options,N,P).

validate_epoll_options1(Header, [], _N, _P) ->
    ok;
validate_epoll_options1(Header, [{statuscode, Code}|Rest],N,P) ->
    validate_epoll_options1(Header, Rest, N, P);
validate_epoll_options1(Header, [{header, HeaderField}|Rest], N, P) ->
    case lists:keysearch(HeaderField, 1, Header) of
	{value, {HeaderField, Value}} ->
	    ok;
	false ->
	    ?FAIL({missing_header_field, HeaderField, Header});
	_ ->
	    ?FAIL({missing_header_field, HeaderField, Header})
    end,
    validate_epoll_options1(Header, Rest, N, P);
validate_epoll_options1(Header, [{header, HeaderField, Value}|Rest],N,P) ->
    case lists:keysearch(HeaderField, 1, Header) of
	{value, {HeaderField, Value}} ->
	    ok;
	false ->
	    ?FAIL({wrong_header_field_value, HeaderField, Header});
	_ ->
	    ?FAIL({wrong_header_field_value, HeaderField, Header})
    end,
    validate_epoll_options1(Header, Rest, N, P);
validate_epoll_options1(Header, [Unknown|Rest], N, P) ->
    validate_epoll_options1(Header, Rest, N, P).


%% ----------------------------------------------------------------------
%% Chunked poll function
%% (This is the same as the poll function, with the difference that
%%  it sends the erequest using the XSEND macro instead of SEND).
%%
%%

cpoll(Mode, Host, Port, Node, Request, StatusCode) ->
    cpoll(Mode, Host, Port, Node, Request, StatusCode, 30000, 500, 6).

cpoll(Mode, Host, Port, Node, Request, StatusCode, Timeout) ->
    cpoll(Mode, Host, Port, Node, Request, StatusCode, Timeout, 500, 6).

cpoll(Mode, Host, Port, Node, Request, StatusCode, Timeout, ETo, ECnt) ->
    ?DEBUG("poll -> connect to server: ~s:~p", [Host, Port]),
    case ?CONNECT(Mode, Host, Port) of
	{ok, Socket} ->
	    ?LOG("poll -> send request "
		 "~n   ~p"
		 "~n   to server ~s:~p", [Request, Host, Port]),
	    case ?CSEND(Mode, Socket, Request, 10, 100) of
		ok ->
		    validate(Mode, Request, StatusCode, Socket, Timeout);
		Error ->
		    ?FAIL({send_failed, Error})
	    end;
	Error ->
	    poll_connect_error(Error, ETo, ECnt),
	    cpoll(Mode, Host, Port, Node, Request, StatusCode, Timeout,
		  ETo*2, ECnt-1)
    end.
	    

%% ----------------------------------------------------------------------
%% Poll function
%%

poll(Mode, Host, Port, Node, Request, StatusCode) ->
    poll(Mode, Host, Port, Node, Request, StatusCode, 30000, 500, 6).

poll(Mode, Host, Port, Node, Request, StatusCode, Timeout) ->
    poll(Mode, Host, Port, Node, Request, StatusCode, Timeout, 500, 6).

poll(Mode, Host, Port, Node, Request, StatusCode, Timeout, ETo, ECnt) ->
    ?DEBUG("poll -> connect to server: ~s:~p", [Host, Port]),
    case ?CONNECT(Mode, Host, Port) of
	{ok, Socket} ->
	    ?LOG("poll -> send request "
		 "~n   ~p"
		 "~n   to server ~s:~p", [Request, Host, Port]),
	    case ?SEND(Mode, Socket, Request) of
		ok ->
		    validate(Mode, Request, StatusCode, Socket, Timeout);
		Error ->
		    ?FAIL({send_failed, Error})
	    end;
	Error ->
	    poll_connect_error(Error, ETo, ECnt),
	    poll(Mode, Host, Port, Node, Request, StatusCode, Timeout, 
		 ETo*2, ECnt-1)
    end.
	    

poll_connect_error({error,Reason}, _, 0) ->
    ?LOG("final connect error: ~p", [Reason]),
    ?FAIL({connect_failed, Reason});
poll_connect_error({error,enfile}, To, _) ->
    ?LOG("connect error: ~p", [enfile]),
    ?SLEEP(To);
poll_connect_error({error,emfile}, To, _) ->
    ?LOG("connect error: ~p", [emfile]),
    ?SLEEP(To);
poll_connect_error({error,econnreset}, To, _) ->
    ?LOG("connect error: ~p", [econnreset]),
    ?SLEEP(To);
poll_connect_error({error,econnrefused}, To, _) ->
    ?LOG("connect error: ~p", [econnrefused]),
    ?SLEEP(To);
poll_connect_error({error,esslconnect}, To, _) ->
    ?LOG("connect error: ~p", [esslconnect]),
    ?SLEEP(To);
poll_connect_error(Error, _, _) ->
    ?FAIL({connect_failed, Error}).


validate(Mode, Request, StatusCode, Socket, Timeout) ->
    validate(Mode, Request, StatusCode, Socket, Timeout, []).
validate(Mode, Request, StatusCode, Socket, Timeout, SoFar) ->
    ?DEBUG("validate -> await server response (~p)", [StatusCode]),
    Response1 =
	receive
	    {tcp, Socket, Response} ->
		?DEBUG("validate -> (tcp) got data", []),
		Response;
	    {tcp_closed, Socket} ->
		?LOG("validate -> (tcp) connection closed: ~p",[Socket]),
		{error, closed};
	    {tcp_error, Socket, Error} ->
		?LOG("validate -> (tcp) connection error: "
		     "~n   Socket: ~p"
		     "~n   Error:  ~p",[Socket, Error]),
		{error, Error};

	    %%
	    %% SSL receives
	    %%
	    {ssl, Socket, Response} ->
		?DEBUG("validate -> (ssl) got data (tcp)", []),
		Response;
	    {ssl_closed, Socket} ->
		?LOG("validate -> connection closed: ~p",[Socket]),
		{error, closed};
	    {ssl_error, Socket, Error} ->
		?LOG("poll -> connection error: "
		     "~n   Socket: ~p"
		     "~n   Error:  ~p",[Socket, Error]),
		{error, Error}

	after Timeout ->
		{error, timeout}
	end,

    ?DEBUG("validate -> ~n   Response1: ~p", [Response1]),

    case Response1 of
	{error, timeout} ->
	    ?CLOSE(Mode, Socket),
	    ?FAIL(connection_timed_out);
	{error, closed} ->
	    ?FAIL(connection_closed);
	{error, Reason} ->
	    ?CLOSE(Mode, Socket),
	    ?FAIL({connection_error, Reason});
	Res when list(Res) ->
	    ?DEBUG("validate -> Res: ~p", [Res]),
	    case terminated_header(SoFar++Res) of
		{true, Header, Stuff} ->
		    ?DEBUG("validate -> Header: ~p", [Header]),
		    trash_the_rest(Socket,Header,Request,length(Stuff)),
		    validate1(Mode, Request, StatusCode, Socket, Response1);
		false ->
		    validate(Mode, Request, StatusCode, Socket, Timeout,
			     SoFar++Res)
	    end
    end.

validate1(Mode, Request, StatusCode, Socket, Response) ->
    ?CLOSE(Mode,Socket),
    case regexp:split(Response," ") of
	{ok,["HTTP/1.0",StatusCode|_]} ->
	    ok;
	{ok,["HTTP/1.0",AlternativeStatusCode|_]} -> 
	    case lists:member(AlternativeStatusCode, StatusCode) of
		true ->
		    ok;
		false ->
		    ?FAIL({wrong_status_code, 
			   AlternativeStatusCode, StatusCode, 
			   Request, Response})
	    end;
	{ok,["HTTP/1.1",StatusCode|_]} ->
	    ok;
	{ok,["HTTP/1.1",AlternativeStatusCode|_]} -> 
	    case lists:member(AlternativeStatusCode,StatusCode) of
		true ->
		    ok;
		false ->
		    ?FAIL({wrong_status_code, 
			   AlternativeStatusCode, StatusCode, 
			   Request, Response})
	    end;
	Reason ->
	    ?FAIL({bad_response, Request, lists:flatten(Response)})
    end.


trash_the_rest(Socket, ResponseHeader, [$H,$E,$A,$D|Request], ReceivedLen) ->
    case string:str(Request,"HTTP/1.1") of
	0 ->
	    %% Remove the socket closed meessage from the message que
	    trash_the_rest(Socket);
	_->  
	    ok
    end;
	
trash_the_rest(Socket, ResponseHeader, Request, ReceivedLen)
  when list(ResponseHeader) ->
    case string:str(httpd_util:to_lower(ResponseHeader),"content-length:") of
	0 ->
	    trash_the_rest(Socket);
	_ByteN ->
	    case string:str(Request,"HTTP/1.1") of
		0 ->
		    %% Remove the socket closed meessage from the message que
		    trash_the_rest(Socket);
		_Number ->
		    Size = get_body_size(ResponseHeader++"\r\n"),
		    trash_the_rest(Socket,Size-ReceivedLen)
	    end
    end.

trash_the_rest(Socket) ->			   
    trash_the_rest(Socket, nosize).

trash_the_rest(Socket, nosize) ->
    receive
	%%
	%% TCP
	%%
	{tcp, Socket, Trash} ->
	    ?DEBUG("trash_the_rest -> (tcp) ~p bytes",[?SZ(Trash)]),
	    trash_the_rest(Socket,nosize);
	{tcp_closed, Socket} ->
	    ?DEBUG("trash_the_rest -> (tcp) closed",[]),
	    ok;
	{tcp_error, Socket, Reason} ->
	    ?DEBUG("trash_the_rest -> (tcp) error: ~p",[Reason]),
	    ok;

	%%
	%% SSL
	%%
	{ssl, Socket, Trash} ->
	    ?DEBUG("trash_the_rest -> (ssl) ~p bytes",[?SZ(Trash)]),
	    trash_the_rest(Socket);
	{ssl_closed, Socket} ->
	    ?DEBUG("trash_the_rest -> (ssl) closed",[]),
	    ok;
	{ssl_error, Socket, Reason} ->
	    ?DEBUG("trash_the_rest -> (ssl) error: ~p",[Reason]),
	    ok

    after 30000 ->
	    ?FLUSH(),
	    ?FAIL(connection_timed_out)
    end;

trash_the_rest(Socket, Size) when Size > 0 ->
    receive
	%%
	%% TCP
	%%
	{tcp, Socket, Trash} ->
	    ?DEBUG("trash_the_rest -> ~p bytes",[?SZ(Trash)]),
	    trash_the_rest(Socket, Size-length(Trash));
	{tcp_closed, Socket} ->
	    ?DEBUG("trash_the_rest -> (tcp) closed",[]),
	    ok;
	{tcp_error, Socket, Reason} ->
	    ?DEBUG("trash_the_rest -> (tcp) error: ~p",[Reason]),
	    ok;

	%%
	%% SSL
	%%
	{ssl, Socket, Trash} ->
	    ?DEBUG("trash_the_rest -> (ssl) ~p bytes",[?SZ(Trash)]),
	    trash_the_rest(Socket, Size-length(Trash));
	{ssl_closed, Socket} ->
	    ?DEBUG("trash_the_rest -> (ssl) closed",[]),
	    ok;
	{ssl_error, Socket, Reason} ->
	    ?DEBUG("trash_the_rest -> (ssl) error: ~p",[Reason]),
	    ok

    after 30000 ->
	    ?FLUSH(),
	    ?FAIL(connection_timed_out)
    end;
trash_the_rest(Socket, Size) when Size =< 0 ->
    ok.


%% ----------------------------------------------------------------------
%% Misc message parse functions
%%

terminated_header(Data) ->
    terminated_header(Data, []).
terminated_header([$\r,$\n,$\r,$\n|Rest], SoFar) ->
    {true, lists:reverse(SoFar), Rest};
terminated_header([$\n,$\n|Rest], SoFar) ->
    {true, lists:reverse(SoFar), Rest};
terminated_header([], SoFar) ->
    false;
terminated_header([C|Rest], SoFar) ->
    terminated_header(Rest, [C|SoFar]).

parse_reply(HeaderChunk) ->
    [StatusLine|Header]=split_lines(HeaderChunk),
    Status = parse_status_line(StatusLine),
    {Status, tagup_header(Header)}.
    

parse_status_line([$H,$T,$T,$P,$/|Rest0]) ->
    [Vmaj0|Rest1]=Rest0,
    [_|Rest2]=Rest1,
    [Vmin0|Rest3]=Rest2,
    Vmaj=Vmaj0-48,
    Vmin=Vmin0-48,
    {Code, _Rest4}=get_returncode(Rest3),
    [
     {protocol, http},
     {protomajor, Vmaj},
     {protominor, Vmin},
     {statuscode, Code}
    ];
parse_status_line(BadStatusLine) ->
    [
     {protocol, http},
     {protomajor, 1},
     {protominor, 0},
     {statuscode, 501}    % Not implemented.
    ].


get_returncode([$ |Rest]) ->
    get_returncode(Rest, []).

get_returncode([$ |Rest], Acc) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	{'EXIT', _} ->
	    {500, Rest};         % Internal Server Error.
	Code ->
	    {Code, Rest}
    end;
get_returncode([Chr|Rest], Acc) ->
    get_returncode(Rest, [Chr|Acc]);
get_returncode([], Acc) ->
    get_returncode(" ", Acc).
    

split_lines(Request) ->
    split_lines(Request, [], []).
split_lines([], CAcc, Acc) ->
    lists:reverse([lists:reverse(CAcc)|Acc]);
split_lines([$\r, $\n|Rest], CAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(CAcc)|Acc]);
split_lines([Chr|Rest], CAcc, Acc) ->
    split_lines(Rest, [Chr|CAcc], Acc).


tagup_header(Req) ->
    tagup_header(Req, []).
tagup_header([], Acc) ->
    Acc;
tagup_header([[]|Rest], Acc) ->
    tagup_header(Rest, Acc);
tagup_header([Line|Rest], Acc) ->
    Tag=tag(Line),
    tagup_header(Rest, [Tag|Acc]).

tag(Line) ->
    tag(Line, []).
tag([], Tag) ->
    {list_to_atom(lists:reverse(Tag)), ""};
tag([$:|Rest], Tag) ->
    {list_to_atom(lists:reverse(Tag)), get_value(Rest)};
tag([Chr|Rest], Tag) ->
    tag(Rest, [Chr|Tag]).

get_value([32|Line]) ->
    get_value(Line, []);
get_value(Line) ->
    get_value(Line, []).
get_value([], Acc) ->
    lists:reverse(Acc);
get_value([Chr|Rest], Acc) ->
    get_value(Rest, [Chr|Acc]).


get_body_size(Head) ->
    case regexp:match(Head,"Content-Length:.*\r\n") of
	{match, Start, Length} ->
	    %% 15 is length of Content-Length, 
	    %% 17 Is length of Content-Length and \r\
	    S = list_to_integer(
		  string:strip(string:substr(Head, Start + 15, Length-17))),
	    ?DEBUG("get_body_size -> Size: ~p",[S]),
	    S;
	_->
	    ?DEBUG("get_body_size -> Size: ~p",[0]),
	    0
    end.


end_of_header(HeaderPart) ->
    case httpd_util:split(HeaderPart,"\r\n\r\n|\n\n",2) of
	{ok, [Head, Body]} ->	
	    {true, Head, Body, get_body_size(Head)};
	_Pos ->
	   false
    end.

