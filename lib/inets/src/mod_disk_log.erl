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
-module(mod_disk_log).
-export([do/1,error_log/5,load/2,store/2,remove/1]).

-export([report_error/2]).

-include("httpd.hrl").

%% do

do(Info) ->
  AuthUser=auth_user(Info#mod.data),
  Date=custom_date(),
  log_internal_info(Info,Date,Info#mod.data),
  case httpd_util:key1search(Info#mod.data,status) of
    %% A status code has been generated!
    {StatusCode,PhraseArgs,Reason} ->
      transfer_log(Info,"-",AuthUser,Date,StatusCode,0),
      if
	StatusCode >= 400 ->
	  error_log(Info,Date,Reason);
	true ->
	  not_an_error
      end,
      {proceed,Info#mod.data};
    %% No status code has been generated!
    undefined ->
      case httpd_util:key1search(Info#mod.data,response) of
	{already_sent,StatusCode,Size} ->
	  transfer_log(Info,"-",AuthUser,Date,StatusCode,Size),
	  {proceed,Info#mod.data};
	{StatusCode,Response} ->
	  transfer_log(Info,"-",AuthUser,Date,200,
		       httpd_util:flatlength(Response)),
	  {proceed,Info#mod.data};
	undefined ->
	  transfer_log(Info,"-",AuthUser,Date,200,0),
	  {proceed,Info#mod.data}
      end
  end.

custom_date() ->
  LocalTime=calendar:local_time(),
  UniversalTime=calendar:universal_time(),
  Minutes=round(diff_in_minutes(LocalTime,UniversalTime)),
  {{YYYY,MM,DD},{Hour,Min,Sec}}=LocalTime,
  Date=io_lib:format("~.2.0w/~.3s/~.4w:~.2.0w:~.2.0w:~.2.0w ~c~.2.0w~.2.0w",
		     [DD,httpd_util:month(MM),YYYY,Hour,Min,Sec,sign(Minutes),
		      abs(Minutes) div 60,abs(Minutes) rem 60]),  
  lists:flatten(Date).

diff_in_minutes(L,U) ->
  (calendar:datetime_to_gregorian_seconds(L) -
   calendar:datetime_to_gregorian_seconds(U))/60.

sign(Minutes) when Minutes > 0 ->
  $+;
sign(Minutes) ->
  $-.

auth_user(Data) ->
  case httpd_util:key1search(Data,remote_user) of
    undefined ->
      "-";
    RemoteUser ->
      RemoteUser
  end.

%% log_internal_info

log_internal_info(Info,Date,[]) ->
  ok;
log_internal_info(Info,Date,[{internal_info,Reason}|Rest]) ->
  error_log(Info,Date,Reason),
  log_internal_info(Info,Date,Rest);
log_internal_info(Info,Date,[_|Rest]) ->
  log_internal_info(Info,Date,Rest).

%% transfer_log

transfer_log(Info,RFC931,AuthUser,Date,StatusCode,Bytes) ->
  case httpd_util:lookup(Info#mod.config_db,transfer_disk_log) of
    undefined ->
      no_transfer_log;
    TransferDiskLog ->
      {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
      Entry=io_lib:format("~s ~s ~s [~s] \"~s\" ~w ~w~n",
			  [RemoteHost,RFC931,AuthUser,Date,
			   Info#mod.request_line,StatusCode,Bytes]),
      disk_log:blog(TransferDiskLog,Entry)
  end.

%% error_log

error_log(Info,Date,Reason) ->
  case httpd_util:lookup(Info#mod.config_db,error_disk_log) of
    undefined ->
      no_error_log;
    ErrorDiskLog ->
      {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
      Entry=io_lib:format("[~s] access to ~s failed for ~s, reason: ~p~n",
			  [Date,Info#mod.request_uri,RemoteHost,Reason]),
      disk_log:blog(ErrorDiskLog,Entry)
  end.

error_log(SocketType,Socket,ConfigDB,{PortNumber,RemoteHost},Reason) ->
  case httpd_util:lookup(ConfigDB,error_disk_log) of
    undefined ->
      no_error_log;
    ErrorDiskLog ->
      Date=custom_date(),
      Entry=io_lib:format("[~s] server crash for ~s, reason: ~p~n",
                          [Date,RemoteHost,Reason]),
      disk_log:blog(ErrorDiskLog,Entry),
      ok
  end.

report_error(ConfigDB,Error) ->
    case httpd_util:lookup(ConfigDB,error_disk_log) of
	undefined ->
	    no_error_log;
	ErrorDiskLog ->
	    Date  = custom_date(),
	    Entry = io_lib:format("[~s] reporting error: ~s",[Date,Error]),
	    disk_log:blog(ErrorDiskLog,Entry),
	    ok
    end.

%%
%% Configuration
%%

%% load

load([$T,$r,$a,$n,$s,$f,$e,$r,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ |
      TransferDiskLogSize],[]) ->
  case regexp:split(TransferDiskLogSize," ") of
    {ok,[MaxBytes,MaxFiles]} ->
      case httpd_conf:make_integer(MaxBytes) of
	{ok,MaxBytesInteger} ->
	  case httpd_conf:make_integer(MaxFiles) of
	    {ok,MaxFilesInteger} ->
	      {ok,[],{transfer_disk_log_size,
		      {MaxBytesInteger,MaxFilesInteger}}};
	    {error,_} ->
	      {error,?NICE(httpd_conf:clean(TransferDiskLogSize)++
			   " is an invalid TransferDiskLogSize")}
	  end;
	{error,_} ->
	  {error,?NICE(httpd_conf:clean(TransferDiskLogSize)++
		       " is an invalid TransferDiskLogSize")}
      end
  end;
load([$T,$r,$a,$n,$s,$f,$e,$r,$D,$i,$s,$k,$L,$o,$g,$ |TransferDiskLog],[]) ->
  {ok,[],{transfer_disk_log,httpd_conf:clean(TransferDiskLog)}};

load([$E,$r,$r,$o,$r,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ | ErrorDiskLogSize],[]) ->
    case regexp:split(ErrorDiskLogSize," ") of
	{ok,[MaxBytes,MaxFiles]} ->
	    case httpd_conf:make_integer(MaxBytes) of
		{ok,MaxBytesInteger} ->
		    case httpd_conf:make_integer(MaxFiles) of
			{ok,MaxFilesInteger} ->
			    {ok,[],{error_disk_log_size,
				    {MaxBytesInteger,MaxFilesInteger}}};
			{error,_} ->
			    {error,?NICE(httpd_conf:clean(ErrorDiskLogSize)++
					 " is an invalid ErrorDiskLogSize")}
		    end;
		{error,_} ->
		    {error,?NICE(httpd_conf:clean(ErrorDiskLogSize)++
				 " is an invalid ErrorDiskLogSize")}
	    end
    end;
load([$E,$r,$r,$o,$r,$D,$i,$s,$k,$L,$o,$g,$ |ErrorDiskLog],[]) ->
    {ok, [], {error_disk_log, httpd_conf:clean(ErrorDiskLog)}};

load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$i,$s,$k,$L,$o,$g,$S,$i,$z,$e,$ |SecurityDiskLogSize],[]) ->
    case regexp:split(SecurityDiskLogSize, " ") of
	{ok, [MaxBytes, MaxFiles]} ->
	    case httpd_conf:make_integer(MaxBytes) of
		{ok, MaxBytesInteger} ->
		    case httpd_conf:make_integer(MaxFiles) of
			{ok, MaxFilesInteger} ->
			    {ok, [], {security_disk_log_size,
				      {MaxBytesInteger, MaxFilesInteger}}};
			{error,_} ->
			    {error, ?NICE(httpd_conf:clean(SecurityDiskLogSize)++
					  " is an invalid SecurityDiskLogSize")}
		    end;
		{error, _} ->
		    {error, ?NICE(httpd_conf:clean(SecurityDiskLogSize)++
				  " is an invalid SecurityDiskLogSize")}
	    end
    end;
load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$i,$s,$k,$L,$o,$g,$ |SecurityDiskLog],[]) ->
    {ok, [], {security_disk_log, httpd_conf:clean(SecurityDiskLog)}}.


%% store

store({transfer_disk_log,TransferDiskLog},ConfigList) ->
  case create_disk_log(TransferDiskLog,ConfigList) of
    {ok,TransferDB} ->
      {ok,{transfer_disk_log,TransferDB}};
    {error,Reason} ->
      {error,Reason}
  end;
store({error_disk_log,ErrorDiskLog},ConfigList) ->
  case create_disk_log(ErrorDiskLog,ConfigList) of
    {ok,ErrorDB} ->
      {ok,{error_disk_log,ErrorDB}};
    {error,Reason} ->
      {error,Reason}
  end.

create_disk_log(LogFile,ConfigList) ->
  Filename=httpd_conf:clean(LogFile),
  {MaxBytes,MaxFiles}=
    httpd_util:key1search(ConfigList,transfer_disk_log_size,{500*1024,8}),
  case filename:pathtype(Filename) of
    absolute ->
      case disk_log:open([{name,Filename},
			  {file,Filename},
			  {type,wrap},
			  {format,external},
			  {size,{MaxBytes,MaxFiles}}]) of
	{ok,LogDB} ->
	  {ok,LogDB};
	_ ->
	  {error,?NICE("Can't create "++Filename)}
      end;
    volumerelative ->
      case disk_log:open([{name,Filename},
			  {file,Filename},
			  {type,wrap},
			  {format,external},
			  {size,{MaxBytes,MaxFiles}}]) of
	{ok,LogDB} ->
	  {ok,LogDB};
	_ ->
	  {error,?NICE("Can't create "++Filename)}
      end;
    relative ->
      case httpd_util:key1search(ConfigList,server_root) of
	undefined ->
	  {error,
	   ?NICE(Filename++
		 " is an invalid ErrorLog beacuse ServerRoot is not defined")};
	ServerRoot ->
	  AbsoluteFilename=filename:join(ServerRoot,Filename),
	  case disk_log:open([{name,AbsoluteFilename},
			      {file,AbsoluteFilename},
			      {type,wrap},
			      {format,external},
			      {size,{MaxBytes,MaxFiles}}]) of
	    {ok,LogDB} ->
	      {ok,LogDB};
	    _ ->
	      {error,?NICE("Can't create "++AbsoluteFilename)}
	  end
      end
  end.

%% remove

remove(ConfigDB) ->
  lists:foreach(fun([DiskLog]) -> disk_log:close(DiskLog) end,
		ets:match(ConfigDB,{transfer_disk_log,'$1'})),
  lists:foreach(fun([DiskLog]) -> disk_log:close(DiskLog) end,
		ets:match(ConfigDB,{error_disk_log,'$1'})),
  ok.
