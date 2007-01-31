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

%%% Description: Default Callback module for ssh_sftpd

-module(ssh_sftpd_file).

%% API
-export([close/1, delete/1, del_dir/1, get_cwd/0, is_dir/1, list_dir/1, 
	 make_dir/1, make_symlink/2, open/2, position/2, read/2,
	 read_file_info/1, read_link/1, read_link_info/1, rename/2,
	 write/2, write_file_info/2]).

close(IoDevice) ->
    file:close(IoDevice).

delete(Path) ->
    file:delete(Path).

del_dir(Path) ->
    file:del_dir(Path).

get_cwd() ->
    file:get_cwd().

is_dir(AbsPath) ->
    filelib:is_dir(AbsPath).

list_dir(AbsPath) ->
    file:list_dir(AbsPath).
     
make_dir(Dir) ->
    file:make_dir(Dir).
     
make_symlink(Path2, Path) ->
    file:make_symlink(Path2, Path).

open(Path, Flags) ->
    file:open(Path, Flags).
     
position(IoDevice, Offs) ->
    file:position(IoDevice, Offs).

read(IoDevice, Len) ->
    file:read(IoDevice, Len).
          
read_link(Path) ->
    file:read_link(Path).

read_link_info(Path) ->
    file:read_link_info(Path).
     
read_file_info(Path) ->
    file:read_file_info(Path).

rename(Path, Path2) ->
    file:rename(Path, Path2).

write(IoDevice, Data) ->
    file:write(IoDevice, Data).
     
write_file_info(Path,Info) ->
    file:write_file_info(Path, Info).
