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

%% Read wrap files with internal format

-module(wrap_log_reader).

%-define(debug, true).

-ifdef(debug).
-define(FORMAT(P, A), io:format(P, A)).
-else.
-define(FORMAT(P, A), ok).
-endif.

%%-compile(export_all).
-export([open/1, open/2, chunk/1, chunk/2, close/1]).
-export([last_mod_time/1]).

-include("disk_log.hrl").
-include_lib("kernel/include/file.hrl").

%%============================================================================
%%============================================================================
%%============================================================================
%% The API
%%============================================================================
%%============================================================================
%%============================================================================



%%============================================================================
%% Open a wrap file for reading. 
%% It is possible to read the whole wrap file (i.e. all the index files)
%% or just one specified index file
%%============================================================================
open(File) when atom(File) ->
    open(atom_to_list(File));
open(File) when list(File) ->
    case read_index_file(File) of
	%% Special case, if current file number is one greater than number of 
	%% files then the max file numer is not yet reached, we are on the 
	%% first 'round' of filling the wrap files.
	{ok, {CurFileNo, CurFileSz, TotSz, NoOfFiles}} 
	  when CurFileNo == NoOfFiles + 1 ->
	    ?FORMAT("****1 ~p~n",[{CurFileNo, CurFileSz, TotSz, NoOfFiles}]),
	    FileNo = 1,
	    case open_int(add_ext(File, FileNo)) of
		{ok, Fd, ModTime} ->
		    {ok, {{Fd, start}, {File, {FileNo, ModTime}, FileNo}}};
		E_open ->
		    E_open
	    end;
	{ok, {CurFileNo, CurFileSz, TotSz, NoOfFiles}} ->
	    ?FORMAT("****2 ~p~n",[{CurFileNo, CurFileSz, TotSz, NoOfFiles}]),
	    FileNo = case (CurFileNo + 1) rem NoOfFiles of
			 0 -> NoOfFiles;

			 No -> No
		     end,
	    ?FORMAT("****2 FileNo ~p~n",[FileNo]),
	    case open_int(add_ext(File, FileNo)) of
		{ok, Fd, ModTime} ->
		    {ok, {{Fd, start}, {File, {FileNo, ModTime}, FileNo}}};
		E_open ->
		    E_open
	    end;
	{error, E} ->
	    {error, E}
    end.

open(File, FileNo) when atom(File), integer(FileNo) ->
    open(atom_to_list(File), FileNo);
open(File, FileNo) when list(File), integer(FileNo) ->
    ?FORMAT("index file  ~p~n",[read_index_file(File)]),
    case read_index_file(File) of
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} 
	  when NoOfFiles >= FileNo ->
	    case open_int(add_ext(File, FileNo)) of
		{ok, Fd, ModTime} ->
		    {ok, {{Fd, start}, {File, {FileNo, ModTime}, one}}};
		E_open ->
		    E_open
	    end;
	%% Special case, if current file number is one greater than number of 
	%% files then the max file numer is not yet reached, we are on 
	%% the first 'round' of filling the wrap files.
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} 
	  when CurFileNo == FileNo, CurFileNo == NoOfFiles +1 ->
	    case open_int(add_ext(File, FileNo)) of
		{ok, Fd, ModTime} ->
		    {ok, {{Fd, start}, {File, {FileNo, ModTime}, one}}};
		E_open ->
		    E_open
	    end;
	{ok, {CurFileNo, _CurFileSz, _TotSz, NoOfFiles}} ->
	    {error, {file_not_found, add_ext(File, FileNo)}};
	{error, E} ->
	    {error, E}
    end.


%%============================================================================
%% Close the file
%%============================================================================
close({{Fd, _}, _Info}) ->
    file:close(Fd);
close(Fd) ->
    file:close(Fd).


%%============================================================================
%% Read a chuck of the opened file.
%% It is possible to specify number of terms to be read in each chunk,
%% default size is 8k.
%%============================================================================
chunk({{Fd, ContIn}, Info}) ->
    chunk({{Fd, ContIn}, Info}, ?MAX_CHUNK_SIZE, 0). 

chunk({{Fd, ContIn}, Info}, N) ->
    chunk({{Fd, ContIn}, Info}, N, 0). 

%chunk({{Fd, Continue}, {File, {CurFileNo,  ModTime}, FirstFileNo}}, N, _) ->
%%    io:format(" ModTime ~p~n",[ModTime]),
%%    case last_mod_time(add_ext(File, CurFileNo)) of
%%	{ok, ModTime} ->
%%	    io:format(" ModTime ~p~n",[ok]),
%%	    chunk2({{Fd, Continue}, 
%%                  {File, {CurFileNo,  ModTime}, FirstFileNo}}, N).
%%	{ok, Changed} ->
%%	    io:format(" ModTime Changed ~p~n",[Changed]),
%%	    close(Fd),
%%	    {error, {add_ext(File, CurFileNo), is_wrapped}}; 
%%	{error, E} ->
%%	    {error, {add_ext(File, CurFileNo), E}}
%%    end.

chunk({{Fd, Continue}, Info}, N, Bad) ->
    {File, {CurFileNo,  _ModTime}, FirstFileNo} = Info,
    case read_a_chunk(Fd, N, Continue, add_ext(File, CurFileNo)) of
	eof ->
	    case FirstFileNo of
		%% Read only one specified wrap file
		one ->
		    {Fd, eof};
		%% Read all files. Must take special care if we are on 
		%% the first round to fill the wrap files (FirstFileNo == 1)
		%% because there may be one file more than the NoOfFiles 
		%% indicates.
		FirstFileNo ->
		    ?FORMAT("index file  ~p~n",[read_index_file(File)]),
		    ?FORMAT(" FirstFileNo ~p~n",[FirstFileNo]),
		    ?FORMAT(" CurFileNo ~p~n",[CurFileNo]),
		    {ok, {_, _, _, NoOfFiles}} = read_index_file(File),
		    ?FORMAT(" NoOfFiles ~p~n",[NoOfFiles]),
		    NewFileNo = case (CurFileNo + 1) rem NoOfFiles of
				    %% The special case described above
				    _ when CurFileNo > NoOfFiles -> 1;
				    0 when NoOfFiles > 1 -> NoOfFiles;
				    %% Check if special case
				    No when CurFileNo == NoOfFiles -> 
					FileName = add_ext(File, CurFileNo+1),
					case file:read_file_info(FileName) of
					    {ok, _} -> CurFileNo + 1;
					    _ -> No
					end;
				    No -> No
				end,
		    ?FORMAT(" NewFileNo  ~p~n",[NewFileNo]),
		    case {FirstFileNo, NewFileNo} of
			{_, 0} -> {Fd, eof};
			{_, FirstFileNo} -> {Fd, eof};
			_ -> read_next_file(Fd, N, Info, NewFileNo, Bad)
		    end
	    end;
	{ContOut, [], BadBytes} ->
	    ?FORMAT(" Read a Chunk:  ~p~n",[chunk_was_empty]),
	    case chunk({{Fd, ContOut}, Info}, N, Bad + BadBytes) of
		{Fd, eof} ->
		    ?FORMAT(" ChunkResult  ~p~n",[eof]),
		    {error, {File, file_has_wrapped}};
		ChunkResult ->
		    ?FORMAT(" ChunkResult  ~p~n",[ChunkResult]),
		    ChunkResult
	    end;
	{ContOut, Chunk, BadBytes} when Bad + BadBytes =:= 0 ->
	    {{{Fd, ContOut}, Info}, Chunk};
	{ContOut, Chunk, BadBytes} ->
	    {{{Fd, ContOut}, Info}, Chunk, Bad + BadBytes};
	Error ->
	    Error
    end.



%%============================================================================
%%============================================================================
%%============================================================================
%% Internal functions
%%============================================================================
%%============================================================================
%%============================================================================

%%============================================================================
%% Check if a file is a wrap file.
%% This function is copied from disk_log_1, just to make it possible to release
%% this file as a patch without releasing also disk_log_1.
%%============================================================================
is_head(Bin) when size(Bin) == ?HEADSZ ->
    L = binary_to_list(Bin),
    case catch {lists:sublist(L, 1,4), lists:sublist(L, 5, 4)} of
	{?LOGMAGIC, ?CLOSED} ->
	    yes;
	{?LOGMAGIC, ?OPENED} ->
	    yes_not_closed;
	_ ->
	    no
    end;
is_head(_Bin) ->
    no.

%%============================================================================
%% Get the last modification time of a file
%%============================================================================
last_mod_time(File) ->
    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    {ok, FileInfo#file_info.mtime};
	E ->
	    {error, E}
    end.


%%============================================================================
%% Do the actual opening of the file
%%============================================================================
open_int(FName) ->
    case file:open(FName, [raw, binary, read]) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, Head} when size(Head) == ?HEADSZ ->
		    case is_head(Head) of
			yes ->
			    file:position(Fd, eof),
			    case last_mod_time(FName) of
				{ok, ModTime} ->
				    {ok, Fd, ModTime};
				{error, E} ->
				    {error, {FName, E}}
			    end;
			yes_not_closed  ->
			    file:position(Fd, eof),
			    case last_mod_time(FName) of
				{ok, ModTime} ->
				    {ok, Fd, ModTime};
				{error, E} ->
				    {error, {FName, E}}
			    end;
			no ->
			    close(Fd),
			    {error, 
			     {FName, "not an internal formatted logfile"}}
		    end;
		_Other ->
		    close(Fd),
		    {error, {FName, "not an internal formatted logfile"}}
	    end;
	_Other ->
	    {error, {FName, "not an internal formatted logfile"}}
    end.


%%=============================================================================
%% Read next chunk
%%============================================================================
read_a_chunk(Fd, N, start, FileName) ->
    read_a_chunk(Fd, FileName, 0, list_to_binary([]), N);
read_a_chunk(Fd, N, More, FileName) ->
    Pos = More#continuation.pos,
    B = More#continuation.b,
    read_a_chunk(Fd, FileName, Pos, B, N).

read_a_chunk(Fd, FileName, Pos, B, N) ->
    R = disk_log_1:chunk_read_only(Fd, FileName, Pos, B, N),
    %% The binaries returned from chunk_read_only/5 are turned into terms.
    %% 'foo' will do here since Log is not used when in read-only mode.
    Log = foo,
    case disk_log:ichunk_end(R, Log) of
	{C, S} when record(C, continuation) ->
	    {C, S, 0};
	Else ->
	    Else
    end.

%%============================================================================
%% Read the index file for the File
%% Out: {ok, {CurFileNo, CurFileSz, TotSz, NoOfFiles}} | {error, no_wrap_file}
%%============================================================================
read_index_file(File) ->
    case file:read_file(add_ext(File, "idx")) of
	{ok, Bin} when size(Bin) >= 1 ->
	    {ok, disk_log_1:read_index_file(File)};
	_ ->
	    {error, {File, index_file_not_found}}
    end.

    


%%============================================================================
%% When reading all the index files this function closes the previous and opens
%% the next index file.
%%============================================================================
read_next_file(OldFd, N, Info, NewFileNo, Bad) ->
    {File, {CurFileNo, ModTime}, FirstFileNo} = Info,
    close(OldFd),
    case last_mod_time(add_ext(File, NewFileNo)) of
	{ok, DateNew} ->
	    ModSeconds = calendar:datetime_to_gregorian_seconds(ModTime),
	    NewSeconds = calendar:datetime_to_gregorian_seconds(DateNew),
	    TimeDiff = NewSeconds - ModSeconds,
	    ?FORMAT("time ~p~n",[calendar:universal_time()]),
	    ?FORMAT("DateNew ~p~n",[DateNew]),
	    ?FORMAT("ModTime ~p~n",[ModTime]),
	    ?FORMAT("calendar:time_difference (seconds)~p~n", [TimeDiff]),
	    if 
		TimeDiff < 0 ->
		    {error, {add_ext(File, CurFileNo), is_wrapped}}; 
		true -> 
		    case open_int(add_ext(File, NewFileNo)) of
			{ok, Fd, NewModTime} ->
			    ?FORMAT("index file 2 ~p~n",[read_next_file]),
			    chunk({{Fd, start}, {File, {NewFileNo, NewModTime},
						 FirstFileNo}}, N, Bad);
			E_open ->
			    E_open
		    end
	    end;
	{error, EN} ->
	    {error, {add_ext(File, NewFileNo), EN}}
    end.

add_ext(File, Ext) ->
    lists:concat([File, ".", Ext]).

