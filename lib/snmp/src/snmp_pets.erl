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
-module(snmp_pets).

%%%-----------------------------------------------------------------
%%% Persistent Erlang Term Storage
%%% Implemented using the module ets.
%%% All modifications to the db go through the pets interface, but
%%% lookups go direct to ets. Each modifictaion is written to a
%%% logfile. The entire db is dumped to a dbfile, either when the
%%% db is opened, or through a direct call to dump_db/1.
%%%
%%% Note that as a FileDescriptor is used for logging, the process
%%% that starts a pets db, must also keep the LogFileDescriptor,
%%% and not distribute it to any other process.
%%%-----------------------------------------------------------------

%% External exports
-export([new/3, open/1, open/2, stop/1]).
-export([dump_db/1, insert/2, delete/2, match_delete/2]).

-define(DEFAULT_AUTO_REPAIR,true).

%%-----------------------------------------------------------------
%% Func: new/3
%% Args: FileName is a string containing the path and the filename
%%         without extension.
%%       Name and
%%       Type are sent to ets:new/2
%% Purpose: Creates a new, empty persistent db.
%% Returns: {ok, {FileName, LogFileDescriptor, EtsTabIdentifier}}
%%          The EtsTabIdentifier is used for lookups in the db.
%% Fails: If the LogFile isn't writeable.
%%-----------------------------------------------------------------
new(FileName, Name, Type) ->
    Tab = ets:new(Name, Type),
    LogFile = lists:append(FileName, ".log"),
    {ok, LogFd} = file:open(LogFile, write),
    dump_db({FileName, LogFd, Tab}),
    {ok, {FileName, LogFd, Tab}}.


%%-----------------------------------------------------------------
%% Func: open/1, open/2
%% Args: FileName   -> is a string containing the path and the filename
%%                     without extension.
%%       AutoRepair -> false | true | true_verbose
%% Purpose: Opens a persistent db from db file and logfile.
%% PRE: The logfile and dbfile must exist.
%% POST: The db file is updated, and the logfile is empty.
%% Returns: {ok, {FileName, LogFileDescriptor, EtsTabIdentifier}} |
%%          {error, Reason}
%% Fails: If logfile is corrupt/doen't exist etc.
%%-----------------------------------------------------------------
open(FileName) ->
    open(FileName,?DEFAULT_AUTO_REPAIR).

open(FileName,AutoRepair) ->
    LogFile = lists:append(FileName, ".log"),
    DbFile = lists:append(FileName, ".ets"),
    case file:read_file_info(DbFile) of
	{error, Reason} ->
	    exit(Reason);
	{ok, _} ->
	    {ok, Tab} = ets:file2tab(DbFile),
	    case catch read_log_file(LogFile,Tab,AutoRepair) of
		{ok, LogFd} ->
		    case catch dump_db({FileName,LogFd,Tab}) of
			ok ->
			    {ok, {FileName, LogFd, Tab}};
			terminated ->
			    {error, terminated};
			{'EXIT', Reason} ->
			    catch ets:delete(Tab),
			    catch file:close(LogFd),
			    {error, Reason}
		    end;
		{'EXIT', Reason} ->
		    catch ets:delete(Tab),
		    {error, Reason}
	    end
    end.

%%-----------------------------------------------------------------
%% Func: stop/1
%% Args: 
%% Purpose: delete the tab, and close the log file, without
%%          updating any files.
%% Returns: ok
%%-----------------------------------------------------------------
stop({_FileName, LogFd, Tab}) ->
    ets:delete(Tab),
    file:close(LogFd).

%%-----------------------------------------------------------------
%% Func: dump_db/1
%% Args: FileName is a string containing the path and the filename
%%         without extension.
%%       {LogFd, Tab} is as returned from open/1 or new/3
%% Purpose: Write the entire db to disk.
%% POST: The dbfile is updated, and the logfile is empty.
%% Returns: ok
%% Fails: If something goes wrong (check the code).
%%-----------------------------------------------------------------
dump_db({FileName, LogFd, Tab}) ->
    DbFile = lists:append(FileName, ".ets"),
    TmpFile = lists:append(FileName, ".tmp"),
    validate_error(ets:tab2file(Tab, TmpFile), ok),
    validate_error(file:rename(TmpFile, DbFile), ok),
    validate_error(file:position(LogFd, bof), {ok, 0}),
    validate_error(file:truncate(LogFd), ok).
	    
validate_error(ReturnedVal, Ok) -> ok;
validate_error({error, terminated}, _Ok) -> throw(terminated);
validate_error(Error, _Ok) -> throw(Error).

%%-----------------------------------------------------------------
%% Funcs: insert/2, delete/2, match_delete/2
%% Purpose: Modifies the ets-table, and makes a note on the
%%          logfile.
%%-----------------------------------------------------------------
insert({_FileName, LogFd, Tab}, Object) ->
    ok = write(LogFd, {insert, Object}),
    ets:insert(Tab, Object).

delete({_FileName, LogFd, Tab}, Key) ->
    ok = write(LogFd, {delete, Key}),
    ets:delete(Tab, Key).

match_delete({_FileName, LogFd, Tab}, Pattern) ->
    ok = write(LogFd, {match_delete, Pattern}),
    ets:match_delete(Tab, Pattern).


read_log_file(LogFile,Tab,AutoRepair) ->
    {ok,LogFd} = file:open(LogFile,read_write),
    exec_log(LogFile,LogFd,Tab,AutoRepair).

exec_log(LogFile,LogFd,Tab,AutoRepair) ->
    case (catch read(LogFd)) of
	{ok, Command} ->
	    exec_command(Command,Tab),
	    exec_log(LogFile,LogFd,Tab,AutoRepair);
	eof ->
	    {ok, LogFd};
	Error ->
	    auto_repair(AutoRepair,Error,LogFile),
	    exec_log(LogFile,LogFd,Tab,AutoRepair)
    end.

auto_repair(true_verbose,{error,Reason},FileName) ->
    snmp_error:db_err("Error reading from logfile ~p for reason ~p, repairing", 
		      [FileName,Reason]),
    ok;
auto_repair(true_verbose,{'EXIT',Reason},FileName) ->
    snmp_error:db_err("Exception reading from logfile ~p for reason ~p", 
		      [FileName,Reason]),
    ok;
auto_repair(true,_Error,_FileName) ->
    ok;
auto_repair(false,{'EXIT',Reason},_FileName) ->
    exit(Reason);
auto_repair(false,{error,Reason},_FileName) ->
    exit(Reason).
    
    
exec_command({insert, Object}, Tab) ->
    ets:insert(Tab, Object);
exec_command({delete, Key}, Tab) ->
    ets:delete(Tab, Key);
exec_command({match_delete, Pattern}, Tab) ->
    ets:match_delete(Tab, Pattern).

%% Returns: {ok, Term} | eof | {error, Reason}
read(Fd) ->
    case get_size_of_next_binary(Fd) of
	eof -> eof;
	Size ->
	    read(Fd,Size)
    end.

read(Fd,Size) ->
    case io:get_chars(Fd,'',Size) of
	eof ->
	    {error, reading};
	List when length(List) == Size ->
	    Bin = list_to_binary(List),
	    {ok, binary_to_term(Bin)};
	L -> %% Got less then requested
	    {error, {unexpected_eof,Size,length(L)}}
    end.
 
get_size_of_next_binary(Fd) ->
    case io:get_chars(Fd,'',2) of
        eof ->
            eof;
        [Hi,Lo] ->
	    get_int16(Hi,Lo)
    end.

get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


write(Fd, Term) ->
    Bin = term_to_binary(Term),
    [Hi,Lo] = put_int16(size(Bin)),
%    L = [Hi,Lo|binary_to_list(Bin)],   4.2
    L = [Hi,Lo,Bin],  % 4.3 only !!!!
    io:put_chars(Fd,L),
    ok.

put_int16(I) ->
    [((I band 16#ff00) bsr 8),I band 16#ff].

