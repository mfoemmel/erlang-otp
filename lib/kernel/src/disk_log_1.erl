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
-module(disk_log_1).

%% Efficient file based log - implementation part

-export([int_open/4, ext_open/4, log/3, close/2, truncate/3, chunk/5, sync/1]).
-export([mf_int_open/6, mf_int_log/3, mf_int_close/2, mf_int_inc/2, 
	 mf_ext_inc/2, file_size/1, mf_int_chunk/4, mf_int_chunk_step/3, 
	 mf_int_sync/1]).
-export([mf_ext_open/6, mf_ext_log/3, mf_ext_close/2, mf_ext_sync/1]).

-export([print_index_file/1]).
-export([read_index_file/1]).
-export([read_size_file/1]).
-export([chunk_read_only/5]).
-export([mf_int_chunk_read_only/4]).
-export([change_size_wrap/2]).
-export([get_wrap_size/1]).

-include("disk_log.hrl").
-include_lib("kernel/include/file.hrl").

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, 
	(Int bsr 8) band 255, Int band 255.

%% at the head of a LOG file we have
%% [?LOGMAGIC, ?OPENED | ?CLOSED]
%% Otherwise it's not a LOG file

%% Following that, (the head), comes the logged items/terms
%% each logged item looks like [4_byte_size, MAGICHEAD, term_as_binary ...]

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%% -> integer() | throw(FileError)
log(Fd, FileName, X) when binary(X) ->
    Sz = size(X),
    fwrite(Fd, FileName, [?i32(Sz), ?MAGICHEAD, X]);
log(Fd, FileName, X) -> log(Fd, FileName, X, [], 0).

log(Fd, FileName, [X|T], List, Res) ->
    Sz = size(X),
    log(Fd, FileName, T, [List, ?i32(Sz), ?MAGICHEAD, X], Res+1);
log(Fd, FileName, [], List, Res) -> 
    fwrite(Fd, FileName, List),
    Res.

sync(Fd) -> file:sync(Fd).
  
%% -> ok | throw(FileError)
truncate(Fd, FileName, Head) ->
    position(Fd, FileName, ?HEADSZ),
    truncate(Fd, FileName),
    case Head of
	{ok, B} -> log(Fd, FileName, B), ok;
	none -> ok
    end.

%% -> {Cont, Binaries} | {error, Reason} | eof
chunk(Fd, FileName, Pos, {B, true}, N) ->
    case handle_chunk(B, Fd, FileName, Pos, N, []) of
	{_Cont, []} ->
	    chunk(Fd, FileName, Pos, B, N);
	Else ->
	    Else
    end;
chunk(Fd, FileName, Pos, B, N) ->
    case catch read_chunk(Fd, FileName, Pos, ?MAX_CHUNK_SIZE) of
	eof when 0 == size(B) ->
	    eof;
	eof ->
	    {error, {corrupt_log_file, FileName}};
	{ok, Bin}  ->
	    NewPos = Pos + size(Bin),
	    handle_chunk(list_to_binary([B,Bin]), Fd, FileName, NewPos, N, []);
	Other -> 
	    Other
    end.

%% Format of a log item is: [Size, Magic, binary_term]

handle_chunk(B, _Fd, _FileName, Pos, 0, Ack) ->
    {#continuation{pos = Pos, b = {B, true}}, lists:reverse(Ack)};
handle_chunk(B, Fd, FileName, Pos, N, Ack) when size(B) > 7 ->
    {Head, Tail} =  split_binary(B, ?HEADERSZ),
    {Sz, Mg} = split_binary(Head, 4),
    Size = i32(Sz),
    Magic = i32(Mg),
    TailSize = size(Tail),
    if
	Magic /= ?MAGICINT ->
	    {error, {corrupt_log_file, FileName}};
	TailSize >= Size  ->
	    {BinTerm, Tail2} = split_binary(Tail, Size),
	    %% The client calls binary_to_term/1.
	    handle_chunk(Tail2, Fd, FileName, Pos, N-1, [BinTerm | Ack]);
	true ->
	    BytesToRead = Size - TailSize,
            case read_chunks(BytesToRead, Fd, FileName, Pos, [B]) of
		{ok, NewPos, Bin} when size(Bin) == Size + ?HEADERSZ ->
		    handle_chunk(Bin, Fd, FileName, NewPos, N, Ack);
		{ok, NewPos, _Bin} -> % when size(Bin) < Size + ?HEADERSZ
		    {error, {corrupt_log_file, FileName}};
                Other ->
                    Other
            end
    end;
handle_chunk(B, _Fd, _FileName, Pos, _N, Ack) ->
    {#continuation{pos = Pos, b = B}, lists:reverse(Ack)}.

%% -> {Cont, Binaries, Bad} (Bad >= 0) | {error, Reason} | eof
chunk_read_only(Fd, FileName, Pos, {B, true}, N) ->
    case handle_chunk_ro(B, Fd, FileName, Pos, N, [], 0) of
	{_Cont, [], 0} ->
	    chunk_read_only(Fd, FileName, Pos, B, N);
	Else ->
	    Else
    end;
chunk_read_only(Fd, FileName, Pos, B, N) ->
    case catch read_chunk(Fd, FileName, Pos, ?MAX_CHUNK_SIZE) of
	eof when 0 == size(B) ->
	    eof;
	eof ->
	    NewCont = #continuation{pos = Pos, b = list_to_binary([])},
	    {NewCont, [], size(B)};
	{ok, Bin}  ->
	    NewPos = Pos + size(Bin),
	    NewBin = list_to_binary([B, Bin]),
	    handle_chunk_ro(NewBin, Fd, FileName, NewPos, N, [], 0);
	Other -> 
	    Other
    end.

%% Format of a log item is: [Size, Magic, binary_term]

handle_chunk_ro(B, _Fd, _FileName, Pos, 0, Ack, Bad) ->
    Cont = #continuation{pos = Pos, b = {B, true}},
    {Cont, lists:reverse(Ack), Bad};
handle_chunk_ro(B, Fd, FileName, Pos, N, Ack, Bad) when size(B) > 7 ->
    {Head, Tail} =  split_binary(B, ?HEADERSZ),
    {Sz, Mg} = split_binary(Head, 4),
    Size = i32(Sz),
    Magic = i32(Mg),
    TailSize = size(Tail),
    if
	Magic /= ?MAGICINT ->
	    {_, B2} = split_binary(B,1),
	    handle_chunk_ro(B2, Fd, FileName, Pos, N-1, Ack, Bad+1);
	TailSize >= Size  ->
	    {BinTerm, Tail2} = split_binary(Tail, Size),
	    NewAck = [BinTerm | Ack],
	    handle_chunk_ro(Tail2, Fd, FileName, Pos, N-1, NewAck, Bad);
	true ->
	    BytesToRead = Size - TailSize,
            case read_chunks(BytesToRead, Fd, FileName, Pos, [B]) of
		{ok, NewPos, Bin} when size(Bin) == Size + ?HEADERSZ ->
		    handle_chunk_ro(Bin, Fd, FileName, NewPos, N, Ack, Bad);
		{ok, NewPos, _Bin} -> % when size(Bin) < Size + ?HEADERSZ
		    {_, B2} = split_binary(B,1),
		    handle_chunk_ro(B2, Fd, FileName, Pos, N-1, Ack, Bad+1);
                Other ->
                    Other
            end
    end;
handle_chunk_ro(B, _Fd, _FileName, Pos, _N, Ack, Bad) ->
    {#continuation{pos = Pos, b = B}, lists:reverse(Ack), Bad}.

%% To be removed in R8.
read_chunks(0, _Fd, _FileName, Pos, Bs) ->
    {ok, Pos, list_to_binary(lists:reverse(Bs))};
read_chunks(BytesToRead, Fd, FileName, Pos, Bs) when BytesToRead > 0 ->
    ToRead = min(BytesToRead, ?MAX_CHUNK_SIZE),
    case catch read_chunk(Fd, FileName, Pos, ToRead) of
	eof ->
	    read_chunks(0, Fd, FileName, Pos, Bs);
	{ok, B} ->
	    Read = size(B),
	    read_chunks(BytesToRead - Read, Fd, FileName, Pos + Read, [B|Bs]);
	Other -> 
	    Other
    end.

read_chunk(Fd, FileName, Pos, MaxBytes) ->
    R = pread(Fd, FileName, Pos + ?HEADSZ, MaxBytes),
    position(Fd, FileName, eof),
    R.

min(A, B) when A < B -> A;
min(_A, B) -> B.

%% -> ok | throw(Error)
close(Fd, FileName) ->
    mark(Fd, FileName, closed),
    file:close(Fd).

%% Open an internal file. Head is ignored if Mode is read_only.
%% int_open(FileName, Repair, Mode) -> {ok, {Alloc, Fd, HeadSize}} 
%%  | {repaired, Fd, Terms, BadBytes} 
%%  | throw(Error)
%% Alloc = new | existed
%% HeadSize = {NumberOfItemsWritten, NumberOfBytesWritten}
%% (HeadSize is equal {0, 0} if Alloc == existed, or no header written.)
int_open(FName, truncate, read_write, Head) ->
    new_int_file(FName, Head);
int_open(FName, Repair, read_write, Head) ->
    case open_read(FName) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, FileHead} ->
		    case is_head(FileHead) of
			yes ->
			    file:close(Fd),
			    case open_update(FName) of
				{ok, Fd2} ->
				    mark(Fd2, FName, opened),
				    position_close(Fd2, FName, eof),
				    {ok, {existed, Fd2, {0, 0}}};
				Error ->
				    file_error(FName, Error)
			    end;
			yes_not_closed when Repair == true ->
			    repair(Fd, FName);
			yes_not_closed when Repair == false ->
			    file:close(Fd),
			    throw({error, {need_repair, FName}});
			no ->
			    file:close(Fd),
			    throw({error, {not_a_log_file, FName}})
		    end;
		eof ->
		    file:close(Fd),
		    throw({error, {not_a_log_file, FName}});
		Error ->
		    file_error_close(Fd, FName, Error)
	    end;
	_Other ->
	    new_int_file(FName, Head)
    end;
int_open(FName, _Repair, read_only, _Head) ->
    case open_read(FName) of
	{ok, Fd} ->  %% File exists
	    case file:read(Fd, ?HEADSZ) of
		{ok, Head} ->
		    case is_head(Head) of
			yes ->
			    position_close(Fd, FName, eof),
			    {ok, {existed, Fd, {0, 0}}};
			yes_not_closed  ->
			    position_close(Fd, FName, eof),
			    {ok, {existed, Fd, {0, 0}}};
			no ->
			    file:close(Fd),
			    throw({error, {not_a_log_file, FName}})
		    end;
		eof ->
		    file:close(Fd),
		    throw({error, {not_a_log_file, FName}});
		Error ->
		    file_error_close(Fd, FName, Error)
	    end;
	Error ->
	    file_error(FName, Error)
    end.

new_int_file(FName, Head) ->
    case open_update(FName) of
	{ok, Fd} ->
	    truncate_close(Fd, FName),
	    case file:write(Fd, [?LOGMAGIC, ?OPENED]) of
		ok -> 
		    {Nh, HeadSz} = int_log_head(Head, FName, Fd),
		    {ok, {new, Fd, {Nh, ?HEADERSZ + HeadSz}}};
		Error ->
		    file_error_close(Fd, FName, Error)
	    end;
	Error ->
	    file_error(FName, Error)
    end.

%% -> {NoItemsWritten, NoBytesWritten} | throw(Error)
int_log_head(Head, FileName, Fd) ->
    case lh(Head, internal) of
	{ok, BinHead} -> 
	    case catch log(Fd, FileName, BinHead) of
		N when integer(N) ->
		    {N, size(BinHead) + ?HEADERSZ};
		Error ->
		    file:close(Fd),
		    throw(Error)
	    end;
	none ->
	    {0, 0};
	Error -> 
	    file:close(Fd),
	    throw(Error)
    end.
    
%% Open an external file.
%% -> {ok, {Alloc, Fd, HeadSize}} | throw(Error)
ext_open(FName, truncate, read_write, Head) ->
    new_ext_file(FName, Head);
ext_open(FName, _Repair, read_write, Head) ->
    case file:read_file_info(FName) of
	{ok, _FileInfo} ->
	    case open_update(FName) of
		{ok, Fd} ->
		    position_close(Fd, FName, eof),
		    {ok, {existed, Fd, {0, 0}}};
		Error ->
		    file_error(FName, Error)
	    end;
	_Other ->
	    new_ext_file(FName, Head)
    end;
ext_open(FName, _Repair, read_only, _Head) ->
    case open_read(FName) of
	{ok, Fd} ->
	    position_close(Fd, FName, eof),
	    {ok, {existed, Fd, {0, 0}}};
	Error ->
	    file_error(FName, Error)
    end.

new_ext_file(FName, Head) ->
    case open_truncate(FName) of
	{ok, Fd} ->
	    HeadSize = ext_log_head(Head, FName, Fd),
	    {ok, {new, Fd, HeadSize}};
	Error ->
	    file_error(FName, Error)
    end.

%% -> {NoItemsWritten, NoBytesWritten} | throw(Error)
ext_log_head(Head, FileName, Fd) ->
    case lh(Head, external) of
	{ok, BinHead} -> 
	    {fwrite_close(Fd, FileName, BinHead), size(BinHead)};
	none ->
	    {0, 0};
	Error -> 
	    file:close(Fd),
	    throw(Error)
    end.
    
%% -> ok | throw()
mark(Fd, FileName, opened) ->
    position_close(Fd, FileName, 4),
    fwrite_close(Fd, FileName, ?OPENED),
    ok;
mark(Fd, FileName, closed) ->
    position_close(Fd, FileName, 4),
    fwrite_close(Fd, FileName, ?CLOSED),
    ok.

file_size(Fname) ->
    {ok, Fi} = file:read_file_info(Fname),
    Fi#file_info.size.

%% -> {ok, Bin} | Error
lh({ok, Bin}, _Format) ->
    {ok, Bin};
lh({M, F, A}, Format) when list(A) ->
    case catch apply(M, F, A) of
	{ok, Head} when Format == internal ->
	    {ok, term_to_binary(Head)};
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, Bytes} ->
	    case catch list_to_binary(Bytes) of
		{'EXIT', _} ->
		    {error, {invalid_header, {{M,F,A}, {ok, Bytes}}}};
		Bin ->
		    {ok, Bin}
	    end;
	{'EXIT', Error} ->
	    {error, {invalid_header, {{M,F,A}, Error}}};
	Error ->
	    {error, {invalid_header, {{M,F,A}, Error}}}
    end;
lh({M, F, A}, _Format) ->
    {error, {invalid_header, {M, F, A}}};
lh(none, _Format) ->
    none;
lh(H, _F) ->
    {error, {invalid_header, H}}.

%% -> {repaired, Fd, {recovered, Terms}, {badbytes, BadBytes}} | throw(Error)
repair(In, FName) ->
    error_logger:info_msg("disk_log: repairing ~p ...\n", [FName]),
    NewName = [FName, ".TMP"],
    case open_truncate(NewName) of
	{ok, Out} ->
	    case file:write(Out, [?LOGMAGIC, ?OPENED]) of
		ok ->
		    {ok, _Pos} = file:position(In, ?HEADSZ),
		    Start = list_to_binary([]),
		    scan_f(Start, 8, NewName, FName, Out, In, 0, 0,
			   list_to_binary(?MAGICHEAD));
		Error ->
		    repair_err(In, Out, NewName, NewName, Error)
	    end;
	Error ->
	    file:close(In),
	    file_error(NewName, Error)
    end.

scan_f(B, Sz, OutName, FName, Out, In, Terms, Bad, Magic) when size(B) >= Sz ->
    case get_8(B) of
	{TermSize, Magic, RestB} when TermSize =< size(RestB) ->
	    {BinTerm, Tail} = split_binary(RestB, TermSize),
	    case catch binary_to_term(BinTerm) of
		{'EXIT', _} ->
		    {_, B2} = split_binary(B, 1),
		    scan_f(B2, 8, OutName, FName, Out, In, Terms, Bad+1,Magic);
		_Term ->
		    case catch log(Out, OutName, BinTerm) of
			N when integer(N) ->
			    scan_f(Tail, 8, OutName, FName, Out, In, 
				   Terms + 1, Bad,Magic);
			Error ->
			    repair_err(In, Out, OutName, OutName, Error)
		    end
	    end;
	{TermSize, Magic, RestB} ->
	    %% size(RestB) < TermSize =>
	    %% size(B) = 8 + size(RestB) < 8 + TermSize =>
	    %% we'll get into 2:nd clause and read more bytes
	    scan_f(B, 8 + TermSize, OutName, FName, Out, In, Terms, Bad,Magic);
	{TermSize, _BadMagic, RestB} ->
	    {_, B2} = split_binary(B, 1),
	    scan_f(B2, 8, OutName, FName, Out, In, Terms, Bad+1, Magic);
	Other ->
	    Other
    end;
scan_f(B, Sz, OutName, FName, Out, In, Terms, Bad, Magic) ->
    case file:read(In, ?MAX_CHUNK_SIZE) of
	eof ->
	    done_scan(Out, In, OutName, FName, Terms, Bad);
	{ok, B2} ->
	    scan_f(list_to_binary([B, B2]), Sz, OutName, FName, Out, In,
		   Terms, Bad, Magic);
	Error ->
	    repair_err(In, Out, OutName, FName, Error)
    end.
	 
done_scan(Out, In, OutName, FName, RecoveredTerms, BadChars) ->
    file:close(Out),
    file:close(In),
    case file:rename(OutName, FName) of
	ok ->
	    case open_update(FName) of
		{ok, New} ->
		    position_close(New, FName, eof),
		    {repaired, New, RecoveredTerms, BadChars};
		Error ->
		    file_error(FName, Error)
	    end;
	Error ->
	    file:delete(OutName),
	    file_error(FName, Error)
    end.
   
get_8(Bin) ->
    {BTermSize, T1} = split_binary(Bin, 4),
    {BMagic, T2} = split_binary(T1, 4),
    {i32(BTermSize), BMagic, T2}.

repair_err(In, Out, OutName, ErrFileName, Error) ->
    file:close(In),
    file:close(Out),
    % OutName is often the culprit, try to remove it anyway...
    file:delete(OutName), 
    file_error(ErrFileName, Error).

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

i32(Int) when binary(Int) ->
    i32(binary_to_list(Int));

i32(Int)  when integer(Int) -> [(Int bsr 24) band 255,
				(Int bsr 16) band 255,
				(Int bsr 8) band 255,
				Int band 255];
i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

%%-----------------------------------------------------------------
%% Func: mf_int_open/6, mf_ext_open/6
%% Args: FName = string()
%%       MaxB = integer() 
%%       MaxF = integer()
%%       Repair = truncate | true | false
%%       Mode = read_write | read_only
%%       Head = none | {ok, Bin} | {M, F, A}
%% Purpose: An ADT for wrapping logs.  mf_int_ writes binaries (mf_ext_
%%          writes bytes)
%%          to files called FName.1, FName.2, ..., FName.MaxF.  
%%          Writes MaxB bytes on each file.  
%%          Creates a file called Name.idx in the Dir.  This
%%          file contains the last written FileName as one byte, and
%%          follwing that, the sizes of each file (size 0 number of items).
%%          On startup, this file is read, and the next available
%%          filename is used as first log file.
%%          Reports can be browsed with Report Browser Tool (rb), or
%%          read with disk_log.
%%-----------------------------------------------------------------
%% -> {ok, handle(), Cnt} 
%%  | {repaired, handle(), Rec, Bad, Cnt) 
%%  | throw(FileError)
mf_int_open(FName, MaxB, MaxF, Repair, Mode, Head) when MaxF > 0, 
							MaxF < ?MAX_FILES -> 
    {First, Sz, TotSz, NFiles} = read_index_file(Repair, FName, MaxF),
    write_size_file(Mode, FName, MaxB, MaxF),
    NewMaxF = if 
		  NFiles > MaxF ->
		      {MaxF, NFiles};
		  true ->
		      MaxF
	      end,
    case int_file_open(FName, First, 0, 0, Head, Repair, Mode) of
	{ok, Fd, FileName, Lost, {NoItems, NoBytes}} ->
	    {ok, CurB} = position_close(Fd, FileName, cur),
	    % firstPos = NoBytes is not always correct when the file existed,
	    % but it will have to do since we don't know where the header ends.
	    CurCnt = Sz + NoItems - Lost,
	    {ok, #handle{filename = FName, maxB = MaxB,
			 maxF = NewMaxF, curF = First, cur_fd = Fd,
			 cur_name = FileName, cur_cnt = CurCnt,
			 acc_cnt = -Sz, curB = CurB, 
			 firstPos = NoBytes, noFull = 0, accFull = 0}, 
	     TotSz + CurCnt};
	{repaired, Fd, FileName, Rec, Bad, FSz} ->
	    {repaired, 
	     #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		     maxF = NewMaxF, curF = First, cur_fd = Fd,
		     cur_cnt = Rec, acc_cnt = -Rec, curB = FSz, 
		     firstPos = 0, noFull = 0, accFull = 0}, 
	     Rec, Bad, TotSz + Rec}
    end.

%% -> {ok, handle(), Lost} 
%%   | {error, Error, handle()}
%%   | throw(FatalError)
%% Fatal errors should always terminate the log.
mf_int_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, acc_cnt = AccCnt, 
	    cur_name = FileName, curF = CurF, maxF = MaxF, 
	    cur_fd = CurFd, noFull = NoFull} = Handle,
    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost} ->
	    close(CurFd, FileName), % throw here is fatal
	    {ok, Handle#handle{cur_fd = NewFd, curF = NewF, 
			       cur_name = NewFileName, 
			       cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
			       maxF = NewMaxF, firstPos = FirstPos,
			       curB = FirstPos, noFull = NoFull + 1}, Lost};
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost} | throw(FatalError)
%% On non-fatal error, the returned handle is not always valid - something may
%% have been written before things went wrong. 
%% Fatal errors should always terminate the log.
mf_int_log(Handle, Bin, Head) when binary(Bin) ->
    mf_int_log(Handle, [Bin], Head, 0, 0, 0);
mf_int_log(Handle, Bins, Head) ->
    mf_int_log(Handle, Bins, Head, 0, 0, 0).

mf_int_log(Handle, [], _Head, No, _Lost, 0) ->
    {ok, Handle, No};
mf_int_log(Handle, [], _Head, No, Lost, _Wraps) ->
    {ok, Handle, No, Lost};
mf_int_log(Handle, Bins, Head, No0, Lost, Wraps) ->
    #handle{filename = FName, curB = CurB, maxB = MaxB, cur_name = FileName, 
	    curF = CurF, maxF = MaxF, cur_fd = CurFd, firstPos = FirstPos0,
	    cur_cnt = CurCnt, acc_cnt = AccCnt, noFull = NoFull} = Handle,
    {FirstBins, LastBins, NoBytes} = 
	split_bins(CurB, MaxB, FirstPos0, Bins, ?HEADERSZ),
    case FirstBins of
	[] ->
	    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost1} ->
		    close(CurFd, FileName), % throw here is fatal
		    Handle1 = Handle#handle{cur_fd = NewFd, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName,
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    mf_int_log(Handle1, Bins, Head, No0 + Nh, 
			       Lost + Lost1, Wraps + 1);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end;
	_ ->
	    case catch log(CurFd, FileName, FirstBins) of
		N when integer(N) ->
		    Handle1 = Handle#handle{curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_int_log(Handle1, LastBins, Head, No0 + N, Lost, Wraps);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end
    end.

wrap_int_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFd, NewFileName, Lost, {Nh, FirstPos}} = 
	int_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost}.

mf_int_chunk(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk(Handle, {FileNo, Pos}, Bin, N) 
              when FileNo == Handle#handle.curF ->
    conv(chunk(Handle#handle.cur_fd, Handle#handle.cur_name, 
	       Pos, Bin, N), FileNo);
mf_int_chunk(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	   error_logger:info_msg("disk_log: chunk error. File ~p missing.\n\n",
				 [FName]),
	    mf_int_chunk(Handle, {inc(FileNo, Handle#handle.maxF), 0},[], N);
	{ok, {_Alloc, Fd, _HeadSize}} ->
	    case chunk(Fd, FName, Pos, Bin, N) of
		eof ->
		    file:close(Fd),
		    mf_int_chunk(Handle, {inc(FileNo, Handle#handle.maxF), 0},
				 [], N);
		Other ->
		    file:close(Fd),
		    conv(Other, FileNo)
	    end
    end.

mf_int_chunk_read_only(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk_read_only(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk_read_only(Handle, {FileNo, Pos}, Bin, N) 
                when FileNo == Handle#handle.curF ->
    conv(chunk_read_only(Handle#handle.cur_fd, Handle#handle.cur_name,
			 Pos, Bin, N), FileNo);
mf_int_chunk_read_only(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	   error_logger:info_msg("disk_log: chunk error. File ~p missing.\n\n",
				 [FName]),
	   mf_int_chunk_read_only(Handle, {inc(FileNo, Handle#handle.maxF), 0},
				  [], N);
	{ok, {_Alloc, Fd, _HeadSize}} ->
	    case chunk_read_only(Fd, FName, Pos, Bin, N) of
		eof ->
		    file:close(Fd),
		    mf_int_chunk_read_only(Handle, 
					   {inc(FileNo, Handle#handle.maxF),0},
					   [], N);
		Other ->
		    file:close(Fd),
		    conv(Other, FileNo)
	    end
    end.

%% -> {ok, Cont} | Error
mf_int_chunk_step(Handle, 0, Step) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk_step(Handle, {FirstF, 0}, Step);
mf_int_chunk_step(Handle, {FileNo, _Pos}, Step) ->
    NFileNo = inc(FileNo, Handle#handle.maxF, Step),
    FileName = add_ext(Handle#handle.filename, NFileNo),
    case file:read_file_info(FileName) of
	{ok, _FileInfo} ->	
	    {ok, #continuation{pos = {NFileNo, 0}, b = list_to_binary([])}};
	_Error ->
	    {error, end_of_log}
    end.

mf_int_sync(#handle{cur_fd = Fd}) ->
    file:sync(Fd).

%% -> ok | throw(FileError)
mf_int_close(#handle{filename = FName, curF = CurF, cur_name = FileName, 
		  cur_fd = CurFd, cur_cnt = CurCnt}, Mode) ->
    close(CurFd, FileName),
    write_index_file(Mode, FName, CurF, CurF, CurCnt),
    ok.

%% -> {ok, handle(), Cnt} | throw(FileError)
mf_ext_open(FName, MaxB, MaxF, Repair, Mode, Head) when MaxF > 0, 
							MaxF < ?MAX_FILES -> 
    {First, Sz, TotSz, NFiles} = read_index_file(Repair, FName, MaxF),
    write_size_file(Mode, FName, MaxB, MaxF),
    NewMaxF = if 
		  NFiles > MaxF ->
		      {MaxF, NFiles};
		  true ->
		      MaxF
	      end,
    {ok, Fd, FileName, Lost, {NoItems, NoBytes}} = 
	ext_file_open(FName, First, 0, 0, Head, Repair, Mode),
    {ok, CurB} = position_close(Fd, FileName, eof),
    CurCnt = Sz + NoItems - Lost,
    {ok, #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		 maxF = NewMaxF, cur_cnt = CurCnt, acc_cnt = -Sz,
		 curF = First, cur_fd = Fd, firstPos = NoBytes,
		 curB = CurB, noFull = 0, accFull = 0},
     TotSz + CurCnt}.

%% -> {ok, handle(), Lost} 
%%   | {error, Error, handle()}
%%   | throw(FatalError)
%% Fatal errors should always terminate the log.
mf_ext_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, acc_cnt = AccCnt, 
	   curF = CurF, maxF = MaxF, cur_fd = CurFd, noFull = NoFull} = Handle,
    file:close(CurFd),
    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost} ->
	    {ok, Handle#handle{cur_fd = NewFd, curF = NewF, 
			       cur_name = NewFileName,
			       cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
			       maxF = NewMaxF, firstPos = FirstPos, 
			       curB = FirstPos, noFull = NoFull + 1}, Lost};
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost}
%% On non-fatal error, the returned handle is not always valid - something may
%% have been written before things went wrong. 
mf_ext_log(Handle, Bin, Head) when binary(Bin) ->
    mf_ext_log(Handle, [Bin], Head, 0, 0, 0);
mf_ext_log(Handle, Bins, Head) ->
    mf_ext_log(Handle, Bins, Head, 0, 0, 0).

mf_ext_log(Handle, [], _Head, No, _Lost, 0) ->
    {ok, Handle, No};
mf_ext_log(Handle, [], _Head, No, Lost, _Wraps) ->
    {ok, Handle, No, Lost};
mf_ext_log(Handle, Bins, Head, No0, Lost, Wraps) ->
    #handle{filename = FName, curB = CurB, maxB = MaxB, cur_name = FileName,
	    cur_cnt = CurCnt, acc_cnt = AccCnt, curF = CurF, maxF = MaxF, 
	    firstPos = FirstPos0, cur_fd = CurFd, noFull = NoFull} = Handle,
    {FirstBins, LastBins, NoBytes} = 
	split_bins(CurB, MaxB, FirstPos0, Bins, 0),
    case FirstBins of
	[] ->
	    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost1} ->
		    file:close(CurFd),
		    Handle1 = Handle#handle{cur_fd = NewFd, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName, 
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    mf_ext_log(Handle1, Bins, Head, No0 + Nh, 
			       Lost + Lost1, Wraps + 1);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end;
	_ ->
	    case catch fwrite(CurFd, FileName, FirstBins) of
		One when integer(One) ->
		    N = length(FirstBins),
		    Handle1 = Handle#handle{curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_ext_log(Handle1, LastBins, Head, No0 + N, Lost, Wraps);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end
    end.

wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFd, NewFileName, Lost, {Nh, FirstPos}} = 
	ext_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFd, NewFileName, Nh, FirstPos, Lost}.

mf_ext_sync(#handle{cur_fd = Fd}) ->
    file:sync(Fd).

%% -> ok | throw(FileError)
mf_ext_close(#handle{filename = FName, curF = CurF,
		     cur_fd = CurFd, cur_cnt = CurCnt}, Mode) ->
    file:close(CurFd),
    write_index_file(Mode, FName, CurF, CurF, CurCnt),
    ok.


%% -> {ok, handle()} | throw(FileError)
change_size_wrap(Handle, {NewMaxB, NewMaxF}) ->
    FName = Handle#handle.filename,
    {_MaxB, MaxF} = get_wrap_size(Handle),
    write_size_file(read_write, FName, NewMaxB, NewMaxF),
    if
	NewMaxF > MaxF ->
	    remove_files(FName, MaxF + 1, NewMaxF),
	    {ok, Handle#handle{maxB = NewMaxB, maxF = NewMaxF}};
	NewMaxF < MaxF ->
	    {ok, Handle#handle{maxB = NewMaxB, maxF = {NewMaxF, MaxF}}};
	true ->
	    {ok, Handle#handle{maxB = NewMaxB, maxF = NewMaxF}}
    end.

%%-----------------------------------------------------------------
%% Misc functions
%%-----------------------------------------------------------------
%% -> {ok, Fd, FileName, Lost, HeadSize} | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

%% -> {ok, Fd, FileName, Lost, HeadSize} 
%%  | {repaired, Fd, FileName, Rec, Bad, FSz} 
%%  | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    N = add_ext(FName, NewFile),
    case int_open(N, Repair, Mode, Head) of
	{ok, {_Alloc, Fd, HeadSize}} ->
	    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {ok, Fd, N, Lost, HeadSize};
	{repaired, Fd, Recovered, BadBytes} ->
	    write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {repaired, Fd, N, Recovered, BadBytes, file_size(N)}
    end.

%% -> {ok, Fd, FileName, Lost, HeadSize} | throw()
ext_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    FileName = add_ext(FName, NewFile),
    case ext_open(FileName, Repair, Mode, Head) of
	{ok, {_Alloc, Fd, HeadSize}} ->
	    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {ok, Fd, FileName, Lost, HeadSize};
	Error ->
	    file_error(FileName, Error)
    end.

-define(index_file_name(F), lists:concat([F, ".idx"])).

read_index_file(truncate, FName, MaxF) ->
    remove_files(FName, 2, MaxF),
    file:delete(?index_file_name(FName)),
    {1, 0, 0, 0};
read_index_file(_, FName, _MaxF) ->
    read_index_file(FName).

%% -> {CurFileNo, CurFileSz, TotSz, NoFiles} | throw(FileError)
%%  where TotSz does not include CurFileSz.
read_index_file(FName) ->
    FileName = ?index_file_name(FName),
    case file:read_file(FileName) of
	{ok, Bin} when size(Bin) >= 1 ->
	    case split_binary(Bin, 1) of
		{BIdx, Tail} when (size(Tail) rem 4) == 0 ->
		    case hd(binary_to_list(BIdx)) of
			%% New format where CurF can be more that 1 byte,
			%% indicated by 0 in the first byte.
			0 ->
			    case split_binary(Tail, 4) of
				{BIdx2, Tail2} when (size(Tail2) rem 4) == 0 ->
				    case i32(BIdx2) of
					CurF2 when 0 < CurF2, 
						   CurF2 < ?MAX_FILES ->
					    parse_index(CurF2, 1, Tail2,0,0,0);
					_ ->
					    throw({error, 
						   {invalid_index_file, 
						    FileName}})
				    end;
				_ -> 
				    throw({error, 
					   {invalid_index_file, FileName}})
			    end;
			%% Old format where CurF is 1 byte, > 0.
			CurF when 0 < CurF, CurF < ?MAX_FILES ->
			    parse_index(CurF, 1, Tail, 0, 0, 0);
			_ ->
			    throw({error, {invalid_index_file, FileName}})
		    end;
		_ ->
		    throw({error, {invalid_index_file, FileName}})
	    end;
	_Error -> {1, 0, 0, 0}
    end.

parse_index(CurF, CurF, Bin, _CurSz, TotSz, NFiles) when size(Bin) >= 4 ->
    {Sz, Tail} = split_binary(Bin, 4),
    parse_index(CurF, CurF+1, Tail, i32(Sz), TotSz, NFiles+1);
parse_index(CurF, N, Bin, CurSz, TotSz, NFiles) when size(Bin) >= 4->
    {Sz, Tail} = split_binary(Bin, 4),
    parse_index(CurF, N+1, Tail, CurSz, TotSz + i32(Sz), NFiles+1);
parse_index(CurF, _, _, CurSz, TotSz, NFiles) ->
    {CurF, CurSz, TotSz, NFiles}.
    
%%-----------------------------------------------------------------
%% The old file format for index file (CurFileNo > 0):
%%
%% CurFileNo SizeFile1 SizeFile2  ... SizeFileN
%%   1 byte   4 bytes    4 bytes       4 bytes
%%
%% The new file format for index file (NewFormat = 0):
%%
%% NewFormat CurFileNo SizeFile1 SizeFile2  ... SizeFileN
%%   1 byte   4 bytes    4 bytes       4 bytes
%%-----------------------------------------------------------------

print_index_file(File) ->
    io:format("-- Index begin --~n"),
    case file:read_file(File) of
	{ok, Bin} when size(Bin) >= 1 ->
	    case split_binary(Bin, 1) of
		{BIdx, Tail} when (size(Tail) rem 4) == 0 ->
		    case hd(binary_to_list(BIdx)) of
			%% New format where CurF can be more that 1 byte,
			%% indicated by 0 in the first byte.
			0 -> 
			    {BIdx2, Tail2} = split_binary(Tail, 4),
			    case i32(BIdx2) of
				CurF when 0 < CurF, CurF < ?MAX_FILES ->
				    io:format("cur file: ~w~n", [CurF]),
				    loop_index(1, Tail2);
				_ ->
				    ok
			    end;
			%% Old format where CurF is 1 byte, > 0.
			CurF when 0 < CurF, CurF < ?MAX_FILES ->
			    io:format("cur file: ~w~n", [CurF]),
			    loop_index(1, Tail);
			_ ->
			    ok
		    end;
		_ -> ok
	    end;
	_ -> ok
    end,
    io:format("-- end --~n").

loop_index(N, Bin) when size(Bin) >= 4 ->
    {Sz, Tail} = split_binary(Bin, 4),
    io:format(" ~p  items: ~w~n", [N, i32(Sz)]),
    loop_index(N+1, Tail);
loop_index(_, _) ->
    done.

%% Returns: Number of lost items (if an old file was truncated)
%% -> integer() | throw(FileError)
write_index_file(read_only, _FName, _NewFile, _OldFile, _OldCnt) ->
    0;
write_index_file(read_write, FName, NewFile, OldFile, OldCnt) ->
    FileName = ?index_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    case file:read_file(FileName) of
		{ok, Bin} when size(Bin) >= 1 ->
		    case split_binary(Bin, 1) of
			{BIdx, Tail} when (size(Tail) rem 4) == 0 ->
			    case hd(binary_to_list(BIdx)) of
				%% New format where CurF can be more than
				%% 1 byte, indicated by 0 in the first byte.
				0 ->
				    position_close(Fd, FileName, 1),
				    fwrite_close(Fd, FileName, i32(NewFile));
				%% Old format where CurF is 1 byte, > 0.
				CurF when 0 < CurF, CurF < ?MAX_FILES ->
				    position_close(Fd, FileName, bof),
				    fwrite_close(Fd, FileName, [0]),
				    fwrite_close(Fd, FileName, i32(NewFile)),
				    fwrite_close(Fd, FileName, Tail)
			    end
		    end;
		{ok, Bin} when size(Bin) == 0 ->
		    fwrite_close(Fd, FileName, [0]),
		    fwrite_close(Fd, FileName, i32(NewFile));
		Error ->
		    file_error_close(Fd, FileName, Error)
	    end,
	    
	    
	    OffSet = 5, %% one byte not used (the old current file number) and 
	                %% four bytes current file number
	    if
		OldFile > 0 ->
		    position_close(Fd, FileName, OffSet + (NewFile - 1)*4),
		    R = file:read(Fd, 4),
		    position_close(Fd, FileName, OffSet + (OldFile - 1)*4),
		    fwrite_close(Fd, FileName, i32(OldCnt)),
		    file:close(Fd),
		    case R of
			{ok, Lost} when size(Lost) == 4 -> i32(Lost);
			eof    -> 0;
			Error2 -> file_error(FileName, Error2)
		    end;
		true -> 	
		    position_close(Fd, FileName, OffSet + (NewFile - 1)*4),
		    fwrite_close(Fd, FileName, i32(OldCnt)),
		    file:close(Fd),
		    0
	    end;
	E -> 
	    file_error(FileName, E)
    end.

%% -> ok | throw(FileError)
index_file_trunc(FName, N) ->
    FileName = ?index_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    case file:read_file(FileName) of
		{ok, Bin} when size(Bin) >= 1 ->
		    case split_binary(Bin, 1) of
			{BIdx, Tail} when (size(Tail) rem 4) == 0 ->
			    Off = case hd(binary_to_list(BIdx)) of
				      0 -> % new format
					  5;
				      _Old ->
					  1
				  end,
			    Pos = Off + N*4,
			    case Pos > size(Bin) of
				true ->
				    ok;
				false ->
				    position_close(Fd, FileName, {bof, Pos}),
				    truncate_close(Fd, FileName)
			    end;
			_ ->
			    ok
		    end;
		{ok, Bin} when size(Bin) == 0 ->
		    ok;
		Error ->
		    file_error_close(Fd, FileName, Error)
	    end,
	    file:close(Fd);
	Error ->
	    file_error(FileName, Error)
    end.

-define(size_file_name(F), lists:concat([F, ".siz"])).

%% -> ok | throw(FileError)
write_size_file(read_only, _FName, _NewSize, _NewMaxFiles) ->
    ok;
write_size_file(read_write, FName, NewSize, NewMaxFiles) ->
    FileName = ?size_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    fwrite_close(Fd, FileName, i32(NewSize)),
	    fwrite_close(Fd, FileName, i32(NewMaxFiles)),
	    file:close(Fd),
	    ok;
	E -> 
	    file_error(FileName, E)
    end.
	    
%% -> {NoBytes, NoFiles}
read_size_file(FName) ->
    case file:read_file(?size_file_name(FName)) of
	{ok, Bin} when size(Bin) == 8 ->
	    {Size, MaxFiles} = split_binary(Bin, 4),
	    {i32(Size), i32(MaxFiles)};
	_ ->
	    {0, 0}
    end.


conv({More, Terms}, FileNo) when record(More, continuation) ->
    Cont = More#continuation{pos = {FileNo, More#continuation.pos}},
    {Cont, Terms};
conv({More, Terms, Bad}, FileNo) when record(More, continuation) ->
    Cont = More#continuation{pos = {FileNo, More#continuation.pos}},
    {Cont, Terms, Bad};
conv(Other, _) ->
    Other.

find_first_file(#handle{filename = FName, curF = CurF, maxF = MaxF}) ->
    fff(FName, inc(CurF, MaxF), CurF, MaxF).

fff(_FName, CurF, CurF, _MaxF) -> CurF;
fff(FName, MaybeFirstF, CurF, MaxF) ->
    N = add_ext(FName, MaybeFirstF),
    case file:read_file_info(N) of
	{ok, _} -> MaybeFirstF;
	_ -> fff(FName, inc(MaybeFirstF, MaxF), CurF, MaxF)
    end.
	    
split_bins(CurB, MaxB, FirstPos, Bins, HeaderSz) ->
    split_bins(CurB, MaxB, FirstPos, [], Bins, 0, HeaderSz).

split_bins(CurB, MaxB, FirstPos, First, [X | Last], Bs, HeaderSz) ->
    NextB = CurB + size(X) + HeaderSz,
    NBs = Bs + size(X) + HeaderSz,
    if
	NextB > MaxB, CurB == FirstPos, First == [] ->
            % To avoid infinite loop - we allow the file to be
   	    % larger than MaxB if it's just one item on the file.
	    {[X], Last, NBs}; 
	NextB =< MaxB ->
	    split_bins(NextB, MaxB, FirstPos, [X | First], Last, NBs,HeaderSz);
	true ->
	    {lists:reverse(First), [X | Last], Bs}
    end;
split_bins(_, _, _, First, [], Bs, _) ->
    {lists:reverse(First), [], Bs}.
	    
%% -> {NewCurrentFileNo, MaxFilesToBe} | throw(FileError)
inc_wrap(FName, CurF, MaxF) ->
    case MaxF of
	%% Number of max files has changed
	{NewMaxF, OldMaxF} ->
	    if 
		CurF >= NewMaxF ->
		    %% We are at or above the new number of files
		    remove_files(FName, CurF + 1, OldMaxF),
		    if 
			CurF > NewMaxF ->
			    %% The change was done while the current file was 
			    %% greater than the new number of files.
			    %% The index file is not trunctated here, since
			    %% writing the index file while opening the file
			    %% with index 1 will write the value for the file
			    %% with extension CurF as well. Next time the 
			    %% limit is reached, the index file will be
			    %% truncated.
			    {1, {NewMaxF, CurF}};
			true ->
			    %% The change was done while the current file was 
			    %% less than the new number of files.
			    %% Remove the files from the index file too
			    index_file_trunc(FName, NewMaxF), 
			    {1, NewMaxF}
		    end;
		true ->
		    %% We haven't reached the new limit yet
		    NewFt = inc(CurF, NewMaxF),
		    {NewFt, MaxF}
	    end;
	MaxF ->
	    %% Normal case.
	    NewFt = inc(CurF, MaxF),
	    {NewFt, MaxF}
    end.

inc(N, {_NewMax, OldMax}) -> inc(N, OldMax, 1);
inc(N, Max) -> inc(N, Max, 1).

inc(N, Max, Step) ->
    Nx = (N + Step) rem Max,
    if
	Nx > 0 -> Nx;
	true -> Nx + Max
    end.


%% -> ok | throw(FileError)
%% Tries to remove each file with name FName.I, N<=I<=Max.
remove_files(FName, N, Max) ->
    remove_files(FName, N, Max, ok).

remove_files(_FName, N, Max, ok) when N > Max ->
    ok;
remove_files(_FName, N, Max, {FileName, Error}) when N > Max ->
    file_error(FileName, Error);
remove_files(FName, N, Max, Reply) ->
    FileName = add_ext(FName, N),
    NewReply = case file:delete(FileName) of
		   ok -> Reply;
		   {error, enoent} -> Reply;
		   Error -> {FileName, Error}
	       end,
    remove_files(FName, N + 1, Max, NewReply).

%% -> {MaxBytes, MaxFiles}
get_wrap_size(#handle{maxB = MaxB, maxF = MaxF}) ->
    case MaxF of
	{NewMaxF,_} -> {MaxB, NewMaxF};
	MaxF        -> {MaxB, MaxF}
    end.

add_ext(Name, Ext) ->
    lists:concat([Name, ".", Ext]).

open_read(FileName) ->
    file:open(FileName, [raw, binary, read]).

open_update(FileName) ->
    file:open(FileName, [raw, binary, read, write]).

open_truncate(FileName) ->
    file:open(FileName, [raw, binary, write]).

%%% Functions that access files, and throw on error. 

fwrite(Fd, FileName, B) ->
    case file:write(Fd, B) of
	ok    -> 1;
	Error -> file_error(FileName, Error)
    end.

fwrite_close(Fd, FileName, B) ->
    case file:write(Fd, B) of
	ok    -> 1;
	Error -> file_error_close(Fd, FileName, Error)
    end.

position(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error(FileName, {error, Error});
	OK -> OK
    end.
	    
pread(Fd, FileName, Position, MaxBytes) ->
    case file:pread(Fd, Position, MaxBytes) of
	{error, Error} -> file_error(FileName, {error, Error});
	OK -> OK
    end.

position_close(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error_close(Fd, FileName, {error, Error});
	OK -> OK
    end.
	    
truncate(Fd, FileName) ->
    case file:truncate(Fd) of
	ok    -> ok;
	Error -> file_error(FileName, Error)
    end.

truncate_close(Fd, FileName) ->
    case file:truncate(Fd) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

file_error(FileName, {error, Error}) ->
    throw({error, {file_error, FileName, Error}}).

file_error_close(Fd, FileName, {error, Error}) ->
    file:close(Fd),
    throw({error, {file_error, FileName, Error}}).
