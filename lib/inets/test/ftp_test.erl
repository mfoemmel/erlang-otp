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
-module(ftp_test).

-include("inets_test_lib.hrl").

-export([all/1, 
	 init_per_testcase/2, fin_per_testcase/2, 
	 open/1, bad_open/1, 
	 user/1, bad_user/1, 
	 cd/1, bad_cd/1, 
	 ls/1, bad_ls/1,
	 nlist/1, bad_nlist/1,
	 rename/1,
	 delete/1,
	 mkdir/1, 
	 send/1,
	 send_bin/1,
	 send_chunk/1, 
	 append/1,
	 append_bin/1,
	 append_chunk/1, 
	 recv/1,
	 recv_bin/1,
	 recv_chunk/1,

	 %% Tickets
	 otp_3892/1]).

-define(FTP_HOST, "tuor").			% Default remote server host
-define(FTP_USER, "anonymous").
-define(FTP_PASS, "ftp_SUITE@localhost").
-define(FTP_PORT, 21).

-define(BAD_HOST, "badhostname").
-define(BAD_USER, "baduser").
-define(BAD_DIR,  "baddirectory").


-ifdef(ftp_debug_client).
-define(ftp_open(H),ftp:open(H,[debug,verbose])).
-else.
-define(ftp_open(H),ftp:open(H,[verbose])).
-endif.


all(suite) -> [open, bad_open, 
	       user, bad_user, 
	       cd, bad_cd, 
	       ls, bad_ls,
	       nlist, bad_nlist,
	       rename, 
	       delete,
	       mkdir,
	       send, 
	       send_bin,
	       send_chunk,
	       append, 
	       append_bin,
	       append_chunk,
	       recv,
	       recv_bin,
	       recv_chunk,

	       %% Tickets
	       otp_3892]. 


init_per_testcase(Case, Config0) when list(Config0) ->
    Config1 = ?UPDATE(priv_dir, ?inets_priv_dir, Config0),
    Config1.

fin_per_testcase(Case, Config) when list(Config) ->
    Config.


%%
%%
%%
open(doc) ->
    ["Open an ftp connection to a host and close the connection."];
open(suite) ->
    [];
open(Config) when list(Config) ->
    ?LOG("open -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?LOG("open -> open connection to '~p'",[Host]),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?LOG("open -> connected (~p), now close",[Pid]),
    ?line ok = ftp:close(Pid).

%%
%% Assumes that there is no host with name `hostname'.
%%
bad_open(doc) ->
    ["Open an ftp connection to an unknown host."];
bad_open(suite) ->
    [];
bad_open(Config) when list(Config) ->
    ?line {error, ehost} = ?ftp_open(?BAD_HOST),
    ok.

%%
%%
%%
user(doc) ->
    ["Open an ftp connection to a host, and logon as anonymous ftp."];
user(suite) ->
    [];
user(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS).

%%
%%
%%
bad_user(doc) ->
    ["Open an ftp connection and logon as un unknown user."];
bad_user(suite) ->
    [];
bad_user(Config) when list(Config) ->
    ?line Host = ftp_host(Config),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line {error, euser} = ftp:user(Pid, ?BAD_USER, ?FTP_PASS),
    ok.

%%
%%
%%
cd(doc) ->
    ["Open an ftp connection, log on as anonymous ftp, and cd to the"
     "directory \"/pub\"."];
cd(suite) ->
    [];
cd(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "/pub"),
    ?line ok = ftp:close(Pid).

%%
%% 
%%
bad_cd(doc) ->
    ["Open a ftp connection to a host, logon as anonymous ftp, and"
     "cd to a non-existent directory."];
bad_cd(suite) ->
    [];
bad_cd(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line {error, epath} = ftp:cd(Pid, ?BAD_DIR),
    ?line ok = ftp:close(Pid).

ls(doc) ->
    ["Open an ftp connection; ls the current directory, and the "
     "\"incoming\" directory. We assume that ls never fails, since "
     "it's output is meant to be read by humans. "];
ls(suite) ->
    [];
ls(Config) when list(Config) ->
    ?LOG("ls -> entry", []),
    ?line Host = ftp_host(Config), 
    ?DEBUG("ls -> do open", []),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?DEBUG("ls -> do user (login)", []),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?DEBUG("ls -> do first ls", []),
    ?line {ok, Listing1} = ftp:ls(Pid),
    ?DEBUG("ls -> Listing1: ~p~n   now do second ls (with dir given)", 
	[Listing1]),
    ?line {ok, Listing2} = ftp:ls(Pid, "incoming"),
    ?DEBUG("ls -> Listing2: ~p~n   now do close", [Listing2]),
    ?line ok = ftp:close(Pid),
    ?LOG("ls -> done", []),
    ok.

bad_ls(doc) ->
    ["Open an ftp connection; ls a non-existing directory. See comment "
     "for previous test case."];
bad_ls(suite) ->
    [];
bad_ls(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line {ok, Listing} = ftp:ls(Pid, ?BAD_DIR),
    ?line ok = ftp:close(Pid).

nlist(doc) ->
    ["Open an ftp connection; nlist the current directory, and the "
     "\"incoming\" directory. Nlist does not behave consistenly over "
     "operating systems. On some it is an error to have an empty "
     "directory."];
nlist(suite) ->
    [];
nlist(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line {ok, Listing1} = ftp:nlist(Pid),
    ?line Res = ftp:nlist(Pid, "incoming"),
    ?line ok = ftp:close(Pid),
    ?line case Res of
	      {ok, Listing2} ->
		  ok;
	      {error, epath} ->
		  {comment, "Strange server"}
	  end.

bad_nlist(doc) ->
    ["Open an ftp connection; nlist a non-existing directory."];
bad_nlist(suite) ->
    [];
bad_nlist(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line {error, epath} = ftp:nlist(Pid, ?BAD_DIR),
    ?line ok = ftp:close(Pid).

%%
%% 
%% 
rename(doc) ->
    ["Transfer a file to the server, and rename it; then remove it."];
rename(suite) ->
    [];
rename(Config) when list(Config) ->
    ?LOG("rename -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?line LFile  = "ftp_test.txt",
    ?line NewLFile  = "ftp_test.new",
    ?line PrivDir = ?CONFIG(priv_dir, Config), 
    ?LOG("rename -> PrivDir: ~p",[PrivDir]),
    ?line AbsLFile = filename:absname(LFile, PrivDir),
    ?line Contents = "ftp_SUITE test ...",
    ?line ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:lcd(Pid, PrivDir),
    ?line ftp:delete(Pid,LFile),		% reset
    ?line ftp:delete(Pid,NewLFile),		% reset
    ?line ok = ftp:send(Pid, LFile),
    ?line {error, epath} = ftp:rename(Pid, NewLFile, LFile),
    ?line ok = ftp:rename(Pid, LFile, NewLFile),
    ?line ftp:delete(Pid,LFile),		% cleanup
    ?line ftp:delete(Pid,NewLFile),		% cleanup
    ?line ok = ftp:close(Pid).

%%
%% 
%% 
delete(doc) ->
    ["Transfer a file to the server, and then delete it"];
delete(suite) ->
    [];
delete(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line LFile  = "ftp_test.txt",
    ?line PrivDir = ?CONFIG(priv_dir, Config), 
    ?line AbsLFile = filename:absname(LFile, PrivDir),
    ?line Contents = "ftp_SUITE test ...",
    ?line ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:lcd(Pid, PrivDir),
    ?line ftp:delete(Pid,LFile),		% reset
    ?line ok = ftp:send(Pid, LFile),
    ?line ok = ftp:delete(Pid,LFile),
    ?line ok = ftp:close(Pid).


%%
%% 
%% 
mkdir(doc) ->
    ["Make a remote directory, cd to it, go to parent directory, and "
     "remove the directory."];
mkdir(suite) ->
    [];
mkdir(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    {A, B, C} = erlang:now(),
    ?line NewDir = "nisse_" ++ integer_to_list(A) ++ "_" ++
	integer_to_list(B) ++ "_" ++ integer_to_list(C),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line {ok, CurrDir} = ftp:pwd(Pid),
    ?line ok = ftp:mkdir(Pid, NewDir),
    ?line ok = ftp:cd(Pid, NewDir),
    ?line ok = ftp:cd(Pid, CurrDir),
    ?line ok = ftp:rmdir(Pid, NewDir).

%%
%% 
%% 
send(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; send the file; get a directory listing and check that "
     "the file is on the list;, delete the remote file; get another listing "
     "and check that the file is not on the list; close the session; "
     "delete the local file."];
send(suite) ->
    [];
send(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line LFile  = "ftp_test.txt",
    ?line RFile = LFile ++ ".remote",
    ?line PrivDir = ?CONFIG(priv_dir, Config), 
    ?line AbsLFile = filename:absname(LFile, PrivDir),
    ?line Contents = "ftp_SUITE test ...",
    ?line ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:lcd(Pid, PrivDir),
    ?line ok = ftp:send(Pid, LFile, RFile),
    ?line {ok, RFilesString} = ftp:nlist(Pid),
    ?line RFiles = split(RFilesString),
    ?line true = lists:member(RFile, RFiles),
    ?line ok = ftp:delete(Pid, RFile),
    ?line case ftp:nlist(Pid) of
	      {error, epath} ->
		  ok;				% No files
	      {ok, RFilesString1} ->
		  ?line RFiles1 = split(RFilesString1),
		  ?line false = lists:member(RFile, RFiles1)
	  end,
    ?line ok = ftp:close(Pid),
    ?line ok = file:delete(AbsLFile).

%%
%%
%%
append(doc) ->
    ["Create a local file in priv_dir; open an ftp connection to a host; "
     "logon as anonymous ftp; cd to the directory \"incoming\"; lcd to "
     "priv_dir; append the file to a file at the remote side that not exits"
     "this will create the file at the remote side. Then it append the file "
     "again. When this is done it recive the remote file and control that"
     "the content is doubled in it.After that it will remove the files"];
append(suite) ->
    [];
append(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line LFile  = "lapp_test.txt",
    ?line RFile = "rapp_test.txt",
    ?line PrivDir = ?CONFIG(priv_dir, Config), 
    ?line AbsLFile = filename:absname(LFile, PrivDir),
    ?line Contents = "ftp_SUITE test:appending\n",
    ?line ok = file:write_file(AbsLFile, list_to_binary(Contents)),
    %%The local file that we shall operate on i created 
    %%Start the transfer the first time
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:lcd(Pid, PrivDir),
    ?line ok = ftp:append(Pid, LFile, RFile),
    ?line ok = ftp:append(Pid, LFile, RFile),
    %%Control the contents of the file
    ?line {ok, Bin2}  = ftp:recv_bin(Pid, RFile),
    ?line true = control_content(binary_to_list(Bin2),Contents),
    ?line ok = ftp:delete(Pid,RFile),
    ?line ok = ftp:close(Pid),
    ?line ok = file:delete(AbsLFile).

control_content(RContent,LContent)->
    string:equal(RContent,LContent++LContent).
%%
%% 
%% 
send_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send a binary; remove file; close the connection."];
send_bin(suite) ->
    [];
send_bin(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line File = "ftp_test.txt",
    ?line Contents = "ftp_SUITE test ...",
    ?line Bin = list_to_binary(Contents),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:send_bin(Pid, Bin, File),
    ?line {ok, RFilesString} = ftp:nlist(Pid),
    ?line RFiles = split(RFilesString),
    ?line true = lists:member(File, RFiles),
    ?line ok = ftp:delete(Pid, File),
    ?line ok = ftp:close(Pid).

%%
%% 
%% 
append_bin(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append  a binary twice; get the file and compare the content"
     "remove file; close the connection."];
append_bin(suite) ->
    [];
append_bin(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line File = "ftp_test.txt",
    ?line Contents = "ftp_SUITE test ...",
    ?line Bin = list_to_binary(Contents),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:append_bin(Pid, Bin, File),
    ?line ok = ftp:append_bin(Pid, Bin, File),
    %%Control the contents of the file
    ?line {ok, Bin2}  = ftp:recv_bin(Pid, File),
    ?line true = control_content(binary_to_list(Bin2),binary_to_list(Bin)),
    ?line ok = ftp:delete(Pid,File),
    ?line ok = ftp:close(Pid).
%%
%% 
%% 
send_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "send chunks; remove file; close the connection."];
send_chunk(suite) ->
    [];
send_chunk(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line File = "ftp_test.txt",
    ?line Contents = "ftp_SUITE test ...",
    ?line Bin = list_to_binary(Contents),
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:send_chunk_start(Pid, File),
    ?line ok = ftp:send_chunk(Pid, Bin),
    ?line ok = ftp:send_chunk(Pid, Bin),
    ?line ok = ftp:send_chunk_end(Pid),
    ?line {ok, RFilesString} = ftp:nlist(Pid),
    ?line RFiles = split(RFilesString),
    ?line true = lists:member(File, RFiles),
    ?line ok = ftp:delete(Pid, File),
    ?line ok = ftp:close(Pid).

%%
%%
%%
append_chunk(doc) ->
    ["Open a connection to a host; cd to the directory \"incoming\"; "
     "append chunks;control content remove file; close the connection."];
append_chunk(suite) ->
    [];
append_chunk(Config) when list(Config) ->
    ?line Host = ftp_host(Config), 
    ?line File = "ftp_test.txt",
    ?line Contents = ["ER","LE","RL"],
    ?line {ok, Pid} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid, "incoming"),
    ?line ok = ftp:append_chunk_start(Pid, File),
    ?line ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(1,Contents))),
    ?line ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(2,Contents))),
    ?line ok = ftp:append_chunk(Pid,list_to_binary(lists:nth(3,Contents))),
    ?line ok = ftp:append_chunk_end(Pid),
    %%Control the contents of the file
    ?line {ok, Bin2}  = ftp:recv_bin(Pid, File),
    ?line true = control_content(binary_to_list(Bin2),"ERL"),
    ?line ok = ftp:delete(Pid, File),
    ?line ok = ftp:close(Pid).

%%
%%
%%
recv(doc) ->
    ["Create a local file and transfer it to the remote host into the "
     "the \"incoming\" directory, close the connection, and remove "
     "the local file. Then open a new connection; cd to \"incoming\", "
     "lcd to the private directory; receive the file; delete the "
     "remote file; close connection; check that received file is in "
     "the correct directory; cleanup." ];
recv(suite) ->
    [];
recv(Config) when list(Config) ->
    ?LOG("recv -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?line File  = "ftp_test.txt",
    ?line PrivDir = ?CONFIG(priv_dir, Config), 
    ?line AbsFile = filename:absname(File, PrivDir),
    ?line Contents = "ftp_SUITE:recv test ...",
    ?DEBUG("recv -> create file to send: ~p",[AbsFile]),
    ?line ok = file:write_file(AbsFile, list_to_binary(Contents)),
    ?DEBUG("recv -> connect to host",[]),
    ?line {ok, Pid1} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid1, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid1, "incoming"),
    ?line ftp:delete(Pid1, File),		% reset
    ?line ftp:lcd(Pid1, PrivDir),
    ?DEBUG("recv -> send file",[]),
    ?line ok = ftp:send(Pid1, File),
    ?DEBUG("recv -> close",[]),
    ?line ok = ftp:close(Pid1),
    ?DEBUG("recv -> delete local file",[]),
    ?line ok = file:delete(AbsFile),		% cleanup
    %% new connection
    ?line sleep(100),
    ?DEBUG("recv -> connect to host",[]),
    ?line {ok, Pid2} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid2, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:lcd(Pid2, PrivDir),
    ?line ok = ftp:cd(Pid2, "incoming"),
    ?DEBUG("recv -> receive file",[]),
    ?line ok = ftp:recv(Pid2, File),
    ?DEBUG("recv -> delete remote file",[]),
    ?line ok = ftp:delete(Pid2, File),		% cleanup
    ?DEBUG("recv -> close",[]),
    ?line ok = ftp:close(Pid2),
    ?line {ok, Files} = file:list_dir(PrivDir),
    ?line true = lists:member(File, Files),
    ?line ok = file:delete(AbsFile). % cleanup
  
%%
%%
%%
recv_bin(doc) ->
    ["Send a binary to the remote host; Connect again, and retreive "
     "the file; then remove the file."];
recv_bin(suite) ->
    [];
recv_bin(Config) when list(Config) ->
    ?LOG("recv_bin -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?line File = "ftp_test.txt",
    ?line Contents1 = "ftp_SUITE test ...",
    ?DEBUG("recv_bin -> data to send: '~p'",[Contents1]),
    ?line Bin1 = list_to_binary(Contents1),
    ?DEBUG("recv_bin -> connect to host, send and close",[]),
    ?line {ok, Pid1} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid1, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid1, "incoming"),
    ?line ok = ftp:send_bin(Pid1, Bin1, File),
    ?line ok = ftp:close(Pid1),
    ?line sleep(100),
    ?DEBUG("recv_bin -> connect to host, receive and close",[]),
    ?line {ok, Pid2} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid2, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid2, "incoming"),
    ?line {ok, Bin2}  = ftp:recv_bin(Pid2, File),
    ?DEBUG("recv_bin -> unpack received binary",[]),
    ?line Contents2 = binary_to_list(Bin2),
    ?DEBUG("recv_bin -> data recived: '~p'",[Contents2]),
    ?line Contents1 = Contents2,
    ?DEBUG("recv_bin -> Data equal, now cleanup",[]),
    ?line ok = ftp:delete(Pid2, File),		% cleanup
    ?line ok = ftp:close(Pid2).
    

%%
%%
%%
recv_chunk(doc) ->
    ["Send a binary to the remote host; Connect again, and retreive "
     "the file; then remove the file."];
recv_chunk(suite) ->
    [];
recv_chunk(Config) when list(Config) ->
    ?LOG("recv_chunk -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?DEBUG("recv_chunk -> create test binary",[]),
    ?line File = "ftp_test.txt",
    ?line Contents1 = lists:flatten(lists:duplicate(10,
	"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
	"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
	"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
	"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
	"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"
	"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
	"GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG"
	"HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
	"IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")),
    ?line Bin1 = list_to_binary(Contents1),
    ?DEBUG("recv_chunk -> connect to host, send and close",[]),
    ?line {ok, Pid1} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid1, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid1, "incoming"),
    ?line ok = ftp:send_bin(Pid1, Bin1, File),
    ?line ok = ftp:close(Pid1),
    ?line sleep(100),
    ?DEBUG("recv_chunk -> connect to host, receive and close",[]),
    ?line {ok, Pid2} = ?ftp_open(Host),
    ?line ok = ftp:user(Pid2, ?FTP_USER, ?FTP_PASS),
    ?line ok = ftp:cd(Pid2, "incoming"),
    ?line ok  = ftp:recv_chunk_start(Pid2, File),
    ?line {ok, Bin2} = recv_chunk(Pid2, []),
    ?DEBUG("recv_chunk -> unpack received binary",[]),
    ?line Contents2 = binary_to_list(Bin2),
    ?DEBUG("recv_chunk -> data recived",[]),
    ?line Contents1 = Contents2,
    ?DEBUG("recv_chunk -> Data equal, now cleanup",[]),
    ?line ok = ftp:delete(Pid2, File),		% cleanup
    ?line ok = ftp:close(Pid2).
    
recv_chunk(Pid, Acc) ->
    case ftp:recv_chunk(Pid) of
	ok ->
	    {ok, list_to_binary(lists:reverse(Acc))};
	{ok, Bin} ->
	    ?DEBUG("recv_chunk -> received binary chunk of ~p bytes",
		[size(Bin)]),
	    recv_chunk(Pid, [Bin|Acc]);
	Error ->
	    Error
    end.
    

otp_3892(doc) ->
    ["Open an ftp connection to a host with given port number "
     "and close the connection."];
otp_3892(suite) ->
    [];
otp_3892(Config) when list(Config) ->
    ?LOG("open -> entry",[]),
    ?line Host = ftp_host(Config), 
    ?LOG("open -> open connection to '~p'",[Host]),
    ?line {ok, Pid} = ftp:open(Host,?FTP_PORT),
    ?LOG("open -> connected (~p), now close",[Pid]),
    ?line ok = ftp:close(Pid).


%%
%% Internal functions
%%

%%
%% split(Cs) 
%%
%% Split a (flat) list of characters into list of items. The
%% separator is "\r\n". 
split(Cs) ->
    split(Cs, [], []).

split([$\r, $\n| Cs], I, Is) ->
    split(Cs, [], [lists:reverse(I)| Is]); 
split([C| Cs], I, Is) ->
    split(Cs, [C| I], Is);
split([], I, Is) ->
    lists:reverse([lists:reverse(I)| Is]).

ftp_host(Config) ->
    ?DEBUG("ftp_host -> entry",[]),
    case ?CONFIG(ftp_remote_host, Config) of
	undefined ->
	    ?FTP_HOST;
	Host ->
	    ?DEBUG("ftp_host -> Host = ~p",[Host]),	    
	    Host
    end.
	    
sleep(T) ->
    ?DEBUG("sleep ~p ms",[T]),
    receive after T -> ok  end.


