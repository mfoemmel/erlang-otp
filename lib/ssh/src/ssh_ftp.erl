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

-module(ssh_ftp).

-behaviour(gen_server).

-include("ssh.hrl").

-export([
	 close/2,
	 fstat/2,
	 fsetstat/3,
	 lstat/2,
	 mkdir/3,
	 open/2,
	 open/4,
	 opendir/2,
	 read/4,
	 readdir/2,
	 readlink/2,
	 realpath/2,
	 remove/2,
	 rename/3,
	 rmdir/2,
	 setstat/3,
	 start/1,
	 start/2,
	 stat/2,
	 stop/1,
	 symlink/3,
	 user/3,
	 write/4,
	 empty_attrs/0
	]).

-export([
	 init/1,
	 handle_call/3,
	 handle_info/2,
	 handle_cast/2,
	 terminate/2,
	 code_change/3
	 ]).

-record(sftp,{connection = 	'_',
	      channel =		'_',
	      next_id = 	0,
	      cache = 		<<>>,
	      requests = 	[],
	      user =		'_'}).

-define(SFTP_VSN,	3).
-define(DEFAULT_TIMEOUT,  60000).
-define(MAX_MESSAGE_SIZE, 32768).
%%% OpenSSH's sftp-server will crash if a message with size greater than
%%% 256k is received. The draft does not imply any limit..

close(Pid, Handle) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_CLOSE,
			       Handle}, ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

empty_attrs() ->
    #sftp_file_attrs{}.

fsetstat(Pid, FileHandle, Attrs) ->
    case pack_attrs(Attrs) of
	{ok, PackedAttrs} ->
	    case gen_server:call(Pid, {send, ?SSH_FXP_FSETSTAT,
				       [FileHandle, PackedAttrs]},
				 ?DEFAULT_TIMEOUT) of
		{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
		    status_reply(StatusMsg);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

fstat(Pid, FileHandle) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_FSTAT, FileHandle},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_ATTRS,
	      Attrs}} ->
	    case parse_attrs(Attrs) of
		{ok, ParsedAttrs, _} ->
		    {ok, ParsedAttrs};
		Other ->
		    Other
	    end;
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

lstat(Pid, Name) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_LSTAT,
			       ?SSH_STRING(Name)},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_ATTRS,
	      Attrs}} ->
	    case parse_attrs(Attrs) of
		{ok, ParsedAttrs, _} ->
		    {ok, ParsedAttrs};
		Other ->
		    Other
	    end;
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

mkdir(Pid, Name, Attrs) ->
    case pack_attrs(Attrs) of
	{ok, PackedAttrs} ->
	    case gen_server:call(Pid, {send, ?SSH_FXP_MKDIR,
				       [?SSH_STRING(Name),
					PackedAttrs]},
				 ?DEFAULT_TIMEOUT) of
		{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
		    status_reply(StatusMsg);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

open(Pid, File) ->
    open(Pid, File, [read], empty_attrs()).

open(Pid, File, Pflags, Attrs) ->
    case pack_pflags(Pflags) of
	{ok, PackedPflags} ->
	    case pack_attrs(Attrs) of
		{ok, PackedAttrs} ->
		    case gen_server:call(
			   Pid, {send, ?SSH_FXP_OPEN,
				 [?SSH_STRING(File),
				  PackedPflags,
				  PackedAttrs]},
			   ?DEFAULT_TIMEOUT) of
			{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
			    status_reply(StatusMsg);
			{ok, {?SSH_FXP_HANDLE, Handle}} ->
			    {ok, Handle};
			Other ->
			    Other
		    end;
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

opendir(Pid, Dir) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_OPENDIR,
			       ?SSH_STRING(Dir)},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	{ok, {?SSH_FXP_HANDLE, Handle}} ->
	    {ok, Handle};
	Other ->
	    Other
    end.

read(Pid, Handle, Offset, Length) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_READ,
			       [Handle, 
				<<Offset:64/integer, Length:32/integer>>]}) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	{ok, {?SSH_FXP_DATA,
	      <<Size:32/integer,Data:Size/binary>>}} ->
	    {ok, Data};
	Other ->
	    Other
    end.

readdir(Pid, Handle) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_READDIR,
			       Handle}, ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	{ok, {?SSH_FXP_NAME,
	      <<Count:32/integer, Data/binary>>}} ->
	    parse_name_list(Data, Count, []);
	Other ->
	    Other
    end.

readlink(Pid, Path) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_READLINK,
			       ?SSH_STRING(Path)}, ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_NAME,
	      <<Count:32/integer, Data/binary>>}} when Count == 1 ->
	    case parse_name_list(Data, Count, []) of
		{ok, [Result]} ->
		    {ok, element(1,Result)};
		Other ->
		    Other
	    end;
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

realpath(Pid, Path) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_REALPATH,
			       ?SSH_STRING(Path)}, ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_NAME,
	      <<Count:32/integer, Data/binary>>}} when Count == 1 ->
	    case parse_name_list(Data, Count, []) of
		{ok, [Result]} ->
		    {ok, element(1,Result)};
		Other ->
		    Other
	    end;
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

remove(Pid, File) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_REMOVE, ?SSH_STRING(File)},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

rename(Pid, OldName, NewName) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_RENAME,
			       [?SSH_STRING(OldName), ?SSH_STRING(NewName)]},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

rmdir(Pid, Dir) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_RMDIR, ?SSH_STRING(Dir)},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

setstat(Pid, Path, Attrs) ->
    case pack_attrs(Attrs) of
	{ok, PackedAttrs} ->
	    case gen_server:call(Pid, {send, ?SSH_FXP_SETSTAT,
				       [?SSH_STRING(Path), PackedAttrs]},
				 ?DEFAULT_TIMEOUT) of
		{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
		    status_reply(StatusMsg);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

start(Host) ->
    start(Host,[]).

start(Host, Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, ok, []),
    case gen_server:call(Pid, {post_init, Host, Opts},
			 ?DEFAULT_TIMEOUT) of
	ok ->
	    {ok, Pid};
	Other ->
	    Other
    end.

stat(Pid, Name) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_STAT,
			       ?SSH_STRING(Name)},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_ATTRS,
	      Attrs}} ->
	    case parse_attrs(Attrs) of
		{ok, ParsedAttrs, _} ->
		    {ok, ParsedAttrs};
		Other ->
		    Other
	    end;
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

stop(Pid) ->
    gen_server:call(Pid, stop, ?DEFAULT_TIMEOUT).

symlink(Pid, Path, TargetPath) ->
    case gen_server:call(Pid, {send, ?SSH_FXP_SYMLINK,
			       [?SSH_STRING(Path),
				?SSH_STRING(TargetPath)]},
			 ?DEFAULT_TIMEOUT) of
	{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
	    status_reply(StatusMsg);
	Other ->
	    Other
    end.

user(Pid, Username, Password) ->
    case gen_server:call(Pid, {user, Username, Password}, ?DEFAULT_TIMEOUT) of
	ok ->
	    case gen_server:call(Pid, {send_init, ?SFTP_VSN}, ?DEFAULT_TIMEOUT) of
		{ok, ?SFTP_VSN} ->
		    ok;
		{ok, OtherVsn} ->
		    {error, {incompatible_vsn, OtherVsn}};
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.

write(Pid, Handle, Offset, Data) ->
    Size = ssh_transport:flatlength(Data),
    case Size of
	Size when Size =< ?MAX_MESSAGE_SIZE ->
	    case gen_server:call(Pid, {send, ?SSH_FXP_WRITE,
				       [Handle,
					<<Offset:64/integer, 
					Size:32/integer>>,
					Data]},
				 infinity) of
		{ok, {?SSH_FXP_STATUS, StatusMsg}} ->
		    status_reply(StatusMsg);
		Other ->
		    Other
	    end;
	Size ->
	    {error, too_much_data}
    end.

init(_) ->
    process_flag(trap_exit,true),
    {ok, #sftp{}}.

handle_call({post_init, Host, Opts}, From, State) ->
    case ssh_connection:open(Host, Opts) of
	{ok, Pid} ->
	    {reply, ok, #sftp{connection = Pid}};
	{error, Reason} ->
	    {stop, normal, {error, Reason}, State};
	Other -> %%% FIXME
	    {stop, Other, State}
    end;
handle_call({send, Type, Data}, From, State) ->
    case ssh_connection:send_msg(State#sftp.connection,
				 State#sftp.channel,
				 pack([Type,
				       <<(State#sftp.next_id):32/integer>>,
				       Data])) of
	ok ->
	    {noreply, State#sftp{requests = [{State#sftp.next_id,
					      From}|State#sftp.requests],
				 next_id = State#sftp.next_id + 1}};
	Other ->
	    {stop, Other, Other, State}
    end;
handle_call({send_init, Vsn}, From, State) ->
    case ssh_connection:send_msg(State#sftp.connection,
				 State#sftp.channel,
				 pack([?SSH_FXP_INIT,
				       <<Vsn:32/integer>>])) of
	ok ->
	    {noreply, State#sftp{requests = [{init, From}]}};
	Other ->
	    {stop, Other, Other, State}
    end;
handle_call({user, User, Password}, From, State) ->
    case ssh_connection:user(State#sftp.connection, User, Password) of
	ok ->
	    case ssh_connection:start_subsystem(State#sftp.connection,
						"sftp") of
		{ok, Channel} ->
		    ssh_connection:set_active_once(State#sftp.connection,
						   Channel),
		    {reply, ok, State#sftp{channel = Channel,
					   user = User}};
		Other ->
		    {stop, Other, Other, State}
	    end;
	Other ->
	    {reply, Other, State}
    end;
handle_call(stop, From, State) ->
    case ssh_connection:close_session(State#sftp.connection,
				      State#sftp.channel) of
	ok ->
	    case ssh_connection:close(State#sftp.connection) of
		ok ->
		    {stop, normal, ok, State};
		Other ->
		    {stop, Other, Other, State}
	    end;
	Other ->
	    {stop, Other, Other, State}
    end.

handle_cast(What, State) ->
    {noreply, State}.

handle_info({ssh_connection, Connection, Channel, Packet}, State) ->
    case parse(<<(State#sftp.cache)/binary,Packet/binary>>) of
	incomplete ->
	    ssh_connection:set_active_once(Connection,
					   Channel),
	    {noreply, State#sftp{cache = <<(State#sftp.cache)/binary,
				 Packet/binary>>}};
	<<?SSH_FXP_VERSION, Vsn:32/integer,ExtensionData/binary>> ->
	    case State#sftp.requests of
		[{init, From}] ->
		    gen_server:reply(From, {ok, Vsn}),
		    ssh_connection:set_active_once(Connection,
						   Channel),
		    {noreply, State#sftp{requests = [],
					 cache = <<>>}};
		Other ->
		    {stop, {protocol_error, Other}, State}
	    end;
	<<Type, Id:32/integer, Data/binary>> ->
	    case lists:keysearch(Id, 1, State#sftp.requests) of
		{value, {Id, From}} ->
		    ssh_connection:set_active_once(Connection,
						   Channel),
		    gen_server:reply(From, {ok, {Type, Data}}),
		    {noreply, 
		     State#sftp{
		       requests = lists:keydelete(Id, 1, 
						  State#sftp.requests),
		       cache = <<>>}};
		Other ->
		    {stop, {protocol_error, Other}, State}
	    end
    end;
handle_info({ssh_connection_error, Connection, Channel, Error}, State) ->
    [gen_server:reply(X,{error, Error}) || {_, X} <- State#sftp.requests],
    {stop, normal, State};
handle_info({'EXIT', Connection, Reason},State = #sftp{connection = C})
  when Connection == C ->
    [gen_server:reply(X,{error, Reason}) || {_, X} <- State#sftp.requests],
    {stop, normal, State#sftp{connection = '_'}};
handle_info({'EXIT', Pid, Reason},State = #sftp{connection = C})
  when C /= '_' ->
    ssh_connection:close(State#sftp.connection),
    {stop, normal, State#sftp{connection = '_'}};
handle_info(What, State) ->
    {stop, {unknown_msg, What}, State}.

code_change(OldVsn,Extra,State) ->
    {noreply, State}.

terminate(Why, State) ->
    ok.

%pack(Packet) when list(Packet) ->
%    pack(list_to_binary(Packet));
%pack(Packet) ->
%    <<(size(Packet)):32/integer,
%     Packet/binary>>.
pack(Packet) ->
    Size = ssh_transport:flatlength(Packet),
    {Size + 4, [<<Size:32/integer>>,Packet]}.

parse(Data) ->
    case ssh_transport:flatlength(Data) of
	Length when length < 4 ->
	    incomplete;
	Length ->
	    [Hd,Tl] = ssh_transport:split_data([4], Data),
	    <<PacketSize:32/integer>> = my_list_to_binary(Hd),
	    case PacketSize + 4 of
		Length ->
		    my_list_to_binary(Tl);
		PacketLength when PacketLength > Length ->
		    incomplete
	    end
    end.

parse_name_list(_,0,Acc) ->
    {ok, lists:reverse(Acc)};
parse_name_list(Binary,Count,Acc) ->
    case parse_name(Binary) of
	{ok, Name, NextBinary} ->
	    parse_name_list(NextBinary, Count - 1, [Name|Acc]);
	Other ->
	    Other
    end.

parse_name(<<Len1:32/integer, Str1:Len1/binary,
	   Len2:32/integer, Str2:Len2/binary,
	   AttrsAndRest/binary>>) ->
    case parse_attrs(AttrsAndRest) of
	{ok, Attrs, Rest} ->
	    {ok, {binary_to_list(Str1), binary_to_list(Str2), Attrs}, Rest};
	Other ->
	    Other
    end.

parse_attrs(<<Flags:32/integer,
	    AttrsAndRest/binary>>) ->
    parse_attrs(Flags,AttrsAndRest,#sftp_file_attrs{}).

parse_attrs(0, Rest, Attrs) ->
    {ok, Attrs, Rest};
parse_attrs(Flags,<<Size:64/integer, RestAttrs/binary>>, Attrs) 
  when Flags band ?SSH_FILEXFER_ATTR_SIZE /= 0 ->
    parse_attrs(Flags bxor ?SSH_FILEXFER_ATTR_SIZE,
		RestAttrs, Attrs#sftp_file_attrs{size = Size});
parse_attrs(Flags,<<Uid:32/integer, Gid:32/integer, RestAttrs/binary>>, Attrs) 
  when Flags band ?SSH_FILEXFER_ATTR_UIDGID /= 0 ->
    parse_attrs(Flags bxor ?SSH_FILEXFER_ATTR_UIDGID,
		RestAttrs, Attrs#sftp_file_attrs{uid = Uid, gid = Gid});
parse_attrs(Flags, <<Permissions:32/integer, RestAttrs/binary>>, Attrs)
  when Flags band ?SSH_FILEXFER_ATTR_PERMISSIONS /= 0 ->
    parse_attrs(Flags bxor ?SSH_FILEXFER_ATTR_PERMISSIONS,
		RestAttrs, Attrs#sftp_file_attrs{permissions = Permissions});
parse_attrs(Flags,
	    <<ATime:32/integer, MTime:32/integer, RestAttrs/binary>>, Attrs)
  when Flags band ?SSH_FILEXFER_ATTR_ACMODTIME /= 0 ->
    parse_attrs(Flags bxor ?SSH_FILEXFER_ATTR_ACMODTIME, 
		RestAttrs, Attrs#sftp_file_attrs{atime = int2nowtime(ATime), 
						 mtime = int2nowtime(MTime)});
parse_attrs(Flags,
	    <<ExtendedCount:32/integer,ExtendedAndMore>>,Attrs)
  when Flags band ?SSH_FILEXFER_ATTR_EXTENDED /= 0 ->
    parse_extended_attrs(ExtendedCount, ExtendedAndMore, [], Attrs);
parse_attrs(Flags, Data, Acc) ->
    {error, {unknown_file_attrs, Flags}}.

parse_extended_attrs(0, Rest, Acc, Attrs) ->
    {ok, Attrs#sftp_file_attrs{extended = Acc}};
parse_extended_attrs(Count, <<Len1:32/integer, Str1:Len1/binary,
		     Len2:32/integer, Str2:Len2/binary,
		     Rest/binary>>, Acc, Attrs) ->
    parse_extended_attrs(Count - 1,Rest, 
			 [{binary_to_list(Str1),
			   binary_to_list(Str2)}|Acc], Attrs).

pack_attrs(Attrs) ->
    pack_attrs(Attrs, 0, [], size).

pack_attrs(Attrs, Flags, [], size) ->
    case Attrs#sftp_file_attrs.size of
	'_' ->
	    pack_attrs(Attrs, Flags, [], uid);
	Size -> 
	    pack_attrs(Attrs,
		       Flags bor ?SSH_FILEXFER_ATTR_SIZE,
		       [<<Size:64/integer>>], uid)
    end;
pack_attrs(Attrs, Flags, PackedAttrs, uid) ->
    case {Attrs#sftp_file_attrs.uid, Attrs#sftp_file_attrs.gid} of
	{Uid, Gid} when Uid == '_'; Gid == '_' ->
	    pack_attrs(Attrs, Flags, PackedAttrs, permissions);
	{Uid, Gid} ->
	    pack_attrs(Attrs, Flags bor ?SSH_FILEXFER_ATTR_UIDGID,
		       [<<Gid:32/integer,
			Uid:32/integer>>|PackedAttrs],
		       permissions)
    end;
pack_attrs(Attrs, Flags, PackedAttrs, permissions) ->
    case Attrs#sftp_file_attrs.permissions of
	'_' ->
	    pack_attrs(Attrs, Flags, PackedAttrs, atime);
	Permissions ->
	    pack_attrs(Attrs, Flags bor ?SSH_FILEXFER_ATTR_PERMISSIONS,
		       [<<Permissions:32/integer>>|PackedAttrs],
		       atime)
    end;
pack_attrs(Attrs, Flags, PackedAttrs, atime) ->
    case {Attrs#sftp_file_attrs.atime, Attrs#sftp_file_attrs.mtime} of
	{ATime, MTime} when ATime == '_'; MTime == '_' ->
	    pack_attrs(Attrs, Flags, PackedAttrs, extended);
	{ATime, MTime} ->
	    ATime1 = nowtime2int(ATime),
	    MTime1 = nowtime2int(MTime),
	    pack_attrs(Attrs, Flags bor ?SSH_FILEXFER_ATTR_ACMODTIME,
		       [<<MTime1:32/integer>>,<<ATime1:32/integer>>|
			PackedAttrs],extended)
    end;
pack_attrs(Attrs, Flags, PackedAttrs, extended) ->
    case Attrs#sftp_file_attrs.extended of
	[] ->
	    {ok, <<Flags:32/integer, 
	     (list_to_binary(lists:reverse(PackedAttrs)))/binary>>};
	ExtendedAttrs ->
	    case pack_extended_attrs(ExtendedAttrs,[]) of
		{ok, PackedExtendedAttrs} ->
		    NewFlags = Flags bor ?SSH_FILEXFER_ATTR_EXTENDED,
		    {ok, <<NewFlags:32/integer,
		     (list_to_binary(lists:reverse(PackedAttrs)))/binary,
		     (length(ExtendedAttrs)):32/integer,
		     ExtendedAttrs/binary>>};
		Other ->
		    Other
	    end
    end.

pack_extended_attrs([],Acc) ->
    {ok, list_to_binary(Acc)};
pack_extended_attrs([{AttrName, AttrValue}|Rest],Acc)
  when list(AttrName), list(AttrValue) ->
    pack_extended_attrs(Rest,
			[?SSH_STRING(AttrName),?SSH_STRING(AttrValue)|Acc]).

pack_pflags(Pflags) ->
    pack_pflags(lists:sort(Pflags), 0).

pack_pflags([], PackedPflags) ->
    {ok, <<PackedPflags:32/integer>>};
pack_pflags([read], 0) ->
    {ok, <<(?SSH_FXF_READ):32/integer>>};
pack_pflags([write|Pflags],PackedPflags) ->
    pack_pflags(Pflags,PackedPflags bor ?SSH_FXF_WRITE);
pack_pflags([append|Pflags],PackedPflags) ->
    case lists:member(write, Pflags) of
	true ->
	    pack_pflags(Pflags,PackedPflags bor ?SSH_FXF_APPEND);
	false ->
	    {error, illegal_flags}
    end;
pack_pflags([creat|Pflags],PackedPflags) ->
    case lists:member(write, Pflags) of
	true ->
	    pack_pflags(Pflags,PackedPflags bor ?SSH_FXF_CREAT);
	false ->
	    {error, illegal_flags}
    end;
pack_pflags([trunc|Pflags],PackedPflags) ->
    case PackedPflags band ?SSH_FXF_CREAT of
	0 ->
	    {error, illegal_flags};
	_ ->
	    case PackedPflags band ?SSH_FXF_APPEND of
		0 ->
		    pack_pflags(Pflags, PackedPflags bor ?SSH_FXF_TRUNC);
		_ ->
		    {error, illegal_flags}
	    end
    end;
pack_pflags([excl|Pflags],PackedPflags) ->
    case PackedPflags band ?SSH_FXF_CREAT of
	0 ->
	    {error, illegal_flags};
	_ ->
	    case PackedPflags band ?SSH_FXF_APPEND of
		0 ->
		    pack_pflags(Pflags, PackedPflags bor ?SSH_FXF_EXCL);
		_ ->
		    {error, illegal_flags}
	    end
    end;
pack_pflags(_,_) ->
    {error, illegal_flags}.

translate_status_code(?SSH_FX_OK) ->
    ok;
translate_status_code(?SSH_FX_EOF) ->
    eof;
translate_status_code(?SSH_FX_NO_SUCH_FILE) ->
    no_such_file;
translate_status_code(?SSH_FX_PERMISSION_DENIED) ->
    permission_denied;
translate_status_code(?SSH_FX_FAILURE) ->
    failure;
translate_status_code(?SSH_FX_BAD_MESSAGE) ->
    bad_message;
translate_status_code(?SSH_FX_NO_CONNECTION) ->
    no_connection;
translate_status_code(?SSH_FX_CONNECTION_LOST) ->
    connection_lost;
translate_status_code(?SSH_FX_OP_UNSUPPORTED) ->
    op_unsupported.

status_reply(<<(?SSH_FX_OK):32/integer,_/binary>>) ->
    ok;
status_reply(<<Code:32/integer,
	     Len1:32/integer, Str1:Len1/binary,
	     _/binary>>) ->
    {error, {translate_status_code(Code), binary_to_list(Str1)}}.

int2nowtime(Integer) ->
    MegaSecs = Integer div 1000000,
    Secs = Integer rem 1000000,
    {MegaSecs, Secs, 0}.

nowtime2int({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000) + Secs + round(MicroSecs / 1000000).

my_list_to_binary(Data) when binary(Data) ->
    Data;
my_list_to_binary(Data) ->
    list_to_binary(Data).
