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

-export([int_open/4, ext_open/4, logl/1, close/3, truncate/3, chunk/5, 
         sync/2, write_cache/2]).
-export([mf_int_open/6, mf_int_log/3, mf_int_close/2, mf_int_inc/2, 
	 mf_ext_inc/2, mf_int_chunk/4, mf_int_chunk_step/3, 
	 mf_sync/1, mf_write_cache/1]).
-export([mf_ext_open/6, mf_ext_log/3, mf_ext_close/2]).

-export([print_index_file/1]).
-export([read_index_file/1]).
-export([read_size_file/1]).
-export([chunk_read_only/5]).
-export([mf_int_chunk_read_only/4]).
-export([change_size_wrap/2]).
-export([get_wrap_size/1]).
-export([is_head/1]).
-export([position/3, truncate_at/3, fwrite/4, fclose/2]).

-include("disk_log.hrl").
-include_lib("kernel/include/file.hrl").

%% At the head of a LOG file we have
%% [?LOGMAGIC, ?OPENED | ?CLOSED]
%% Otherwise it's not a LOG file.

%% Following that, (the head), comes the logged items/terms
%% each logged item looks like [4_byte_size, MAGICHEAD, term_as_binary ...]

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% -> {NoBytes, NewFdC} | throw(FileError)
log(FdC, FileName, X) ->
    {Bs, Size} = logl(X, [], 0),
    {ok, NewFdC} = fwrite(FdC, FileName, Bs, Size),
    {Size, NewFdC}.

%% -> {iolist(), Size} | throw(FileError)
logl(X) ->
    logl(X, [], 0).

logl([X | T], Bs, Size) ->
    Sz = size(X),
    NBs = [Bs, <<Sz:?SIZESZ/unit:8>>, ?MAGICHEAD | X],
    logl(T, NBs, Size + ?HEADERSZ + Sz);
logl([], Bs, Size) ->
    {Bs, Size}.

%% -> {ok, NewFdC} | throw(FileError)
write_cache(#cache{fd = Fd, c = C}, FName) ->
    erase(write_cache_timer_is_running),
    {ok, write_cache(Fd, FName, C)}.
  
%% -> {ok, NewFdC} | throw(FileError)
sync(FdC, FName) -> fsync(FdC, FName).
  
%% -> {ok, NewFdC} | throw(FileError)
truncate(FdC, FileName, Head) ->
    {ok, FdC1} = truncate_at(FdC, FileName, ?HEADSZ),
    case Head of
	{ok, B} -> 
            {_, NewFdC} = log(FdC1, FileName, [B]), 
            {ok, NewFdC};
	none -> 
            {ok, FdC1}
    end.

%% -> {NewFdC, Reply}, Reply = {Cont, Binaries} | {error, Reason} | eof
chunk(FdC, FileName, Pos, {B, true}, N) ->
    case handle_chunk(B, FdC, FileName, Pos, N, []) of
	{NewFdC, {_Cont, []}} ->
	    chunk(NewFdC, FileName, Pos, B, N);
	Else ->
	    Else
    end;
chunk(FdC, FileName, Pos, B, N) ->
    case catch read_chunk(FdC, FileName, Pos, ?MAX_CHUNK_SIZE) of
	{NewFdC, {ok, Bin}} ->
	    NewPos = Pos + size(Bin),
            NewBin = list_to_binary([B,Bin]),
	    handle_chunk(NewBin, NewFdC, FileName, NewPos, N, []);
	{NewFdC, eof} when 0 == size(B) ->
	    {NewFdC, eof};
	{NewFdC, eof} ->
	    {NewFdC, {error, {corrupt_log_file, FileName}}};
	Other -> 
	    Other
    end.

%% Format of a log item is: [Size, Magic, binary_term]

handle_chunk(B, FdC, _FileName, Pos, 0, Ack) ->
    {FdC, {#continuation{pos = Pos, b = {B, true}}, lists:reverse(Ack)}};
handle_chunk(<<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8, Tail/binary>>, 
	     FdC, FileName, Pos, N, Ack) ->
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    %% The client calls binary_to_term/1.
	    handle_chunk(Tail2, FdC, FileName, Pos, N-1, [BinTerm | Ack]);
	_ ->
	    %% We read the whole thing into one binary.
	    Pos1 = Pos - size(Tail) - ?HEADERSZ,
	    BytesToRead = Size + ?HEADERSZ,
            case catch read_chunk(FdC, FileName, Pos1, BytesToRead) of
		{NewFdC, {ok, Bin}} when size(Bin) == BytesToRead ->
		    NewPos = Pos1 + BytesToRead,
		    handle_chunk(Bin, NewFdC, FileName, NewPos, N, Ack);
		{NewFdC, {ok, _Bin}} -> % when size(_Bin) < BytesToRead
		    {NewFdC, {error, {corrupt_log_file, FileName}}};
		{NewFdC, eof} ->
		    %% "Cannot happen"
		    {NewFdC, {error, {corrupt_log_file, FileName}}};
		Other -> 
		    Other
	    end
    end;
handle_chunk(<<_:?HEADERSZ/unit:8,_/binary>>, FdC, FileName, _Pos, _N, _Ack) ->
    {FdC, {error, {corrupt_log_file, FileName}}};
handle_chunk(B, FdC, _FileName, Pos, _N, Ack) ->
    {FdC, {#continuation{pos = Pos, b = B}, lists:reverse(Ack)}}.

read_chunk(FdC, FileName, Pos, MaxBytes) ->
    {FdC1, R} = pread(FdC, FileName, Pos + ?HEADSZ, MaxBytes),
    {NewFdC, _} = position(FdC1, FileName, eof),
    {NewFdC, R}.

%% Used by wrap_log_reader.
%% -> {NewFdC, Reply}, 
%%    Reply = {Cont, Binaries, Bad} (Bad >= 0) | {error, Reason} | eof
chunk_read_only(FdC, FileName, Pos, B, N) when record(FdC, cache) ->
    do_chunk_read_only(FdC, FileName, Pos, B, N);
chunk_read_only(Fd, FileName, Pos, B, N) ->
    %% wrap_log_reader calling...
    FdC = #cache{fd = Fd}, 
    {_NFdC, Reply} = do_chunk_read_only(FdC, FileName, Pos, B, N),
    Reply.

do_chunk_read_only(FdC, FileName, Pos, {B, true}, N) ->
    case handle_chunk_ro(B, FdC, FileName, Pos, N, [], 0) of
	{NewFdC, {_Cont, [], 0}} ->
	    do_chunk_read_only(NewFdC, FileName, Pos, B, N);
	Else ->
	    Else
    end;
do_chunk_read_only(FdC, FileName, Pos, B, N) ->
    case catch read_chunk_ro(FdC, FileName, Pos, ?MAX_CHUNK_SIZE) of
	{NewFdC, {ok, Bin}}  ->
	    NewPos = Pos + size(Bin),
	    NewBin = list_to_binary([B, Bin]),
	    handle_chunk_ro(NewBin, NewFdC, FileName, NewPos, N, [], 0);
	{NewFdC, eof} when 0 == size(B) ->
	    {NewFdC, eof};
	{NewFdC, eof} ->
	    NewCont = #continuation{pos = Pos, b = <<>>},
	    {NewFdC, {NewCont, [], size(B)}};
	Other -> 
	    Other
    end.

%% Format of a log item is: [Size, Magic, binary_term]

handle_chunk_ro(B, FdC, _FileName, Pos, 0, Ack, Bad) ->
    Cont = #continuation{pos = Pos, b = {B, true}},
    {FdC, {Cont, lists:reverse(Ack), Bad}};
handle_chunk_ro(B= <<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8,
		     Tail/binary>>, FdC, FileName, Pos, N, Ack, Bad) ->
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    NewAck = [BinTerm | Ack],
	    handle_chunk_ro(Tail2, FdC, FileName, Pos, N-1, NewAck, Bad);
	_ ->
	    %% We read the whole thing into one binary.
	    TailSize = size(Tail),
	    Pos1 = Pos - TailSize - ?HEADERSZ,
	    BytesToRead = Size + ?HEADERSZ,
            case catch read_chunk_ro(FdC, FileName, Pos1, BytesToRead) of
		{NewFdC, {ok, Bin}} when size(Bin) == BytesToRead ->
		    NewPos = Pos1 + BytesToRead,
		    handle_chunk_ro(Bin, NewFdC, FileName, NewPos, N, Ack,Bad);
		{NewFdC, {ok, _Bin}} -> % when size(_Bin) < BytesToRead
		    <<_:8, B2/binary>> = B,
		    handle_chunk_ro(B2, NewFdC, FileName, Pos, N-1, Ack,Bad+1);
		{NewFdC, eof} ->
		    %% "Cannot happen"
		    NewCont = #continuation{pos = Pos1, b = <<>>},
		    {NewFdC, 
                     {NewCont, lists:reverse(Ack), Bad + TailSize+?HEADERSZ}};
		Other -> 
		    Other
	    end
    end;
handle_chunk_ro(B= <<_:?HEADERSZ/unit:8, _/binary>>, 
		FdC, FileName, Pos, N, Ack, Bad) ->
    <<_:1/unit:8, B2/binary>> = B,
    handle_chunk_ro(B2, FdC, FileName, Pos, N-1, Ack, Bad+1);
handle_chunk_ro(B, FdC, _FileName, Pos, _N, Ack, Bad) ->
    {FdC, {#continuation{pos = Pos, b = B}, lists:reverse(Ack), Bad}}.

read_chunk_ro(FdC, FileName, Pos, MaxBytes) ->
    pread(FdC, FileName, Pos + ?HEADSZ, MaxBytes).

%% -> ok | throw(Error)
close(#cache{fd = Fd, c = []}, _FileName, read_only) ->
    file:close(Fd);
close(#cache{fd = Fd, c = C}, FileName, read_write) ->
    write_cache(Fd, FileName, C),
    mark(Fd, FileName, ?CLOSED),
    file:close(Fd).

%% Open an internal file. Head is ignored if Mode is read_only.
%% int_open(FileName, Repair, Mode, Head) -> 
%%    {ok, {Alloc, FdC, HeadSize, FileSize}} 
%%  | {repaired, FdC, Terms, BadBytes, FileSize} 
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
				    mark(Fd2, FName, ?OPENED),
                                    FdC1 = #cache{fd = Fd2},
				    {FdC, P} = position_close(FdC1, FName,eof),
				    {ok, {existed, FdC, {0, 0}, P}};
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
			    {ok, P} = position_close2(Fd, FName, eof),
                            FdC = #cache{fd = Fd},
			    {ok, {existed, FdC, {0, 0}, P}};
			yes_not_closed  ->
			    {ok, P} = position_close2(Fd, FName, eof),
                            FdC = #cache{fd = Fd},
			    {ok, {existed, FdC, {0, 0}, P}};
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
            ok = truncate_at_close2(Fd, FName, bof),
            fwrite_close2(Fd, FName, [?LOGMAGIC, ?OPENED]),
            {FdC1, Nh, HeadSz} = int_log_head(Fd, Head),
	    {FdC, FileSize} = position_close(FdC1, FName, cur),
            {ok, {new, FdC, {Nh, ?HEADERSZ + HeadSz}, FileSize}};
	Error ->
	    file_error(FName, Error)
    end.

%% -> {FdC, NoItemsWritten, NoBytesWritten} | throw(Error)
int_log_head(Fd, Head) ->
    case lh(Head, internal) of
	{ok, BinHead} -> 
            {Bs, Size} = logl([BinHead]),
            {ok, FdC} = fwrite_header(Fd, Bs, Size),
            {FdC, 1, Size};
	none ->
	    {#cache{fd = Fd}, 0, 0};
	Error -> 
	    file:close(Fd),
	    throw(Error)
    end.
    
%% Open an external file.
%% -> {ok, {Alloc, FdC, HeadSize}, FileSize} | throw(Error)
ext_open(FName, truncate, read_write, Head) ->
    new_ext_file(FName, Head);
ext_open(FName, _Repair, read_write, Head) ->
    case file:read_file_info(FName) of
	{ok, _FileInfo} ->
	    case open_update(FName) of
		{ok, Fd} ->
		    {ok, P} = position_close2(Fd, FName, eof),
                    FdC = #cache{fd = Fd},
		    {ok, {existed, FdC, {0, 0}, P}};
		Error ->
		    file_error(FName, Error)
	    end;
	_Other ->
	    new_ext_file(FName, Head)
    end;
ext_open(FName, _Repair, read_only, _Head) ->
    case open_read(FName) of
	{ok, Fd} ->
	    {ok, P} = position_close2(Fd, FName, eof),
            FdC = #cache{fd = Fd},
	    {ok, {existed, FdC, {0, 0}, P}};
	Error ->
	    file_error(FName, Error)
    end.

new_ext_file(FName, Head) ->
    case open_truncate(FName) of
	{ok, Fd} ->
	    {FdC1, HeadSize} = ext_log_head(Fd, Head),
	    {FdC, FileSize} = position_close(FdC1, FName, cur),
	    {ok, {new, FdC, HeadSize, FileSize}};
	Error ->
	    file_error(FName, Error)
    end.

%% -> {FdC, {NoItemsWritten, NoBytesWritten}} | throw(Error)
ext_log_head(Fd, Head) ->
    case lh(Head, external) of
	{ok, BinHead} -> 
            Size = size(BinHead),
            {ok, FdC} = fwrite_header(Fd, BinHead, Size),
            {FdC, {1, Size}};
	none ->
	    {#cache{fd = Fd}, {0, 0}};
	Error -> 
            file:close(Fd),
	    throw(Error)
    end.
    
%% -> _Any | throw()
mark(Fd, FileName, What) ->
    position_close2(Fd, FileName, 4),
    fwrite_close2(Fd, FileName, What).

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

repair(In, File) ->
    error_logger:info_msg("disk_log: repairing ~p ...\n", [File]),
    Tmp = add_ext(File, "TMP"),
    {ok, {_Alloc, Out, {0, _}, _FileSize}} = new_int_file(Tmp, none),
    scan_f_read(<<>>, In, Out, File, Tmp, ?MAX_CHUNK_SIZE, 0, 0).

scan_f_read(B, In, Out, File, Tmp, MaxBytes, No, Bad) ->
    case file:read(In, MaxBytes) of
	eof when 0 == size(B) ->
	    done_scan(In, Out, Tmp, File, No, Bad);
	eof ->
	    <<_:8, B2/binary>> = B,	    
	    scan_f(B2, In, Out, File, Tmp, No, Bad+1);
	{ok, Bin}  ->
	    scan_f(list_to_binary([B, Bin]), In, Out, File, Tmp, No, Bad);
	Error -> 
	    repair_err(In, Out, Tmp, File, Error)
    end.

scan_f(B = <<Size:?SIZESZ/unit:8, ?MAGICINT:?MAGICSZ/unit:8, Tail/binary>>, 
       In, Out, File, Tmp, No, Bad) ->
    case Tail of
	<<BinTerm:Size/binary, Tail2/binary>> ->
	    case catch binary_to_term(BinTerm) of
		{'EXIT', _} ->
		    <<_:8, B2/binary>> = B,
		    scan_f(B2, In, Out, File, Tmp, No, Bad+1);
		_Term ->
		    case catch log(Out, Tmp, [BinTerm]) of
			{_Size, NewOut} ->
			    scan_f(Tail2, In, NewOut, File, Tmp, No+1, Bad);
			Error ->
			    repair_err(In, Out, Tmp, File, Error)
		    end
	    end;
	_ ->
	    scan_f_read(B, In, Out, File, Tmp, Size-size(Tail), No, Bad)
    end;
scan_f(B = <<_:?HEADERSZ/unit:8, _/binary>>, In, Out, File, Tmp, No, Bad) ->
    <<_:8, B2/binary>> = B,
    scan_f(B2, In, Out, File, Tmp, No, Bad + 1);
scan_f(B, In, Out, File, Tmp, No, Bad) ->
    scan_f_read(B, In, Out, File, Tmp, ?MAX_CHUNK_SIZE, No, Bad).

done_scan(In, Out, OutName, FName, RecoveredTerms, BadChars) ->
    file:close(In),
    case catch fclose(Out, OutName) of
	ok ->
	    case file:rename(OutName, FName) of
		ok ->
		    case open_update(FName) of
			{ok, New} ->
			    {ok, P} = position_close2(New, FName, eof),
                            FdC = #cache{fd = New},
			    {repaired, FdC, RecoveredTerms, BadChars, P};
			Error ->
			    file_error(FName, Error)
		    end;
		Error ->
		    file:delete(OutName),
		    file_error(FName, Error)
	    end;
	Error ->
	    file:delete(OutName),
	    throw(Error)
    end.
   
repair_err(In, Out, OutName, ErrFileName, Error) ->
    file:close(In),
    catch fclose(Out, OutName),
    % OutName is often the culprit, try to remove it anyway...
    file:delete(OutName), 
    file_error(ErrFileName, Error).

%% Used by wrap_log_reader.
%% -> yes | yes_not_closed | no
is_head(<<M:4/binary, S:4/binary>>) when ?LOGMAGIC == M, ?CLOSED == S ->
    yes;
is_head(<<M:4/binary, S:4/binary>>) when ?LOGMAGIC == M, ?OPENED == S ->
    yes_not_closed;
is_head(_Bin) ->
    no.

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
	{ok, FdC, FileName, Lost, {NoItems, NoBytes}, FSz} ->
	    % firstPos = NoBytes is not always correct when the file
	    % existed, but it will have to do since we don't know
	    % where the header ends.
	    CurCnt = Sz + NoItems - Lost,
	    {ok, #handle{filename = FName, maxB = MaxB,
			 maxF = NewMaxF, curF = First, cur_fdc = FdC,
			 cur_name = FileName, cur_cnt = CurCnt,
			 acc_cnt = -Sz, curB = FSz, 
			 firstPos = NoBytes, noFull = 0, accFull = 0}, 
	     TotSz + CurCnt};
	{repaired, FdC, FileName, Rec, Bad, FSz} ->
	    {repaired, 
	     #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		     maxF = NewMaxF, curF = First, cur_fdc = FdC,
		     cur_cnt = Rec, acc_cnt = -Rec, curB = FSz, 
		     firstPos = 0, noFull = 0, accFull = 0}, 
	     Rec, Bad, TotSz + Rec}
    end.

%% -> {ok, handle(), Lost} | {error, Error, handle()}
mf_int_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, acc_cnt = AccCnt, 
	    cur_name = FileName, curF = CurF, maxF = MaxF, 
	    cur_fdc = CurFdC, noFull = NoFull} = Handle,
    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
	    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF, 
				    cur_name = NewFileName, 
				    cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
				    maxF = NewMaxF, firstPos = FirstPos,
				    curB = FirstPos, noFull = NoFull + 1},
	    case catch close(CurFdC, FileName, read_write) of
		ok ->
		    {ok, Handle1, Lost};
		Error -> % Error in the last file, new file opened.
		    {error, Error, Handle1}
	    end;
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost}
%% The returned handle is not always valid - something may
%% have been written before things went wrong. 
mf_int_log(Handle, Bins, Head) ->
    mf_int_log(Handle, Bins, Head, 0, 0, 0).

mf_int_log(Handle, [], _Head, No, _Lost, 0) ->
    {ok, Handle, No};
mf_int_log(Handle, [], _Head, No, Lost, _Wraps) ->
    {ok, Handle, No, Lost};
mf_int_log(Handle, Bins, Head, No0, Lost, Wraps) ->
    #handle{curB = CurB, maxB = MaxB, cur_name = FileName, cur_fdc = CurFdC, 
            firstPos = FirstPos0, cur_cnt = CurCnt} = Handle,
    {FirstBins, LastBins, NoBytes, N} = 
	int_split_bins(CurB, MaxB, FirstPos0, Bins),
    case FirstBins of
	[] ->
            #handle{filename = FName, curF = CurF, maxF = MaxF, 
                    acc_cnt = AccCnt, noFull = NoFull} = Handle,
	    case catch wrap_int_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost1} ->
		    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName,
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    case catch close(CurFdC, FileName, read_write) of
			ok ->
			    mf_int_log(Handle1, Bins, Head, No0 + Nh, 
				       Lost + Lost1, Wraps + 1);
			Error ->
			    {error, Error, Handle1, No0 + Nh, Lost + Lost1}
		    end;
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end;
	_ ->
	    case catch fwrite(CurFdC, FileName, FirstBins, NoBytes) of
                {ok, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC, 
                                            curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_int_log(Handle1, LastBins, Head, No0 + N, Lost, Wraps);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end
    end.

wrap_int_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFdC, NewFileName, Lost, {Nh, FirstPos}, _FileSize} = 
	int_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost}.

%% -> {NewHandle, Reply}, Reply = {Cont, Binaries} | {error, Reason} | eof
mf_int_chunk(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk(#handle{curF = FileNo, cur_fdc = FdC, cur_name = FileName} 
             = Handle, {FileNo, Pos}, Bin, N) ->
    {NewFdC, Reply} = chunk(FdC, FileName, Pos, Bin, N),
    {Handle#handle{cur_fdc = NewFdC}, conv(Reply, FileNo)};
mf_int_chunk(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    NFileNo = inc(FileNo, Handle#handle.maxF),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	   error_logger:info_msg("disk_log: chunk error. File ~p missing.\n\n",
				 [FName]),
	    mf_int_chunk(Handle, {NFileNo, 0}, [], N);
	{ok, {_Alloc, FdC, _HeadSize, _FileSize}} ->
	    case chunk(FdC, FName, Pos, Bin, N) of
		{NewFdC, eof} ->
		    file:close(NewFdC#cache.fd),
		    mf_int_chunk(Handle, {NFileNo, 0}, [], N);
		{NewFdC, Other} ->
		    file:close(NewFdC#cache.fd),
		    {Handle, conv(Other, FileNo)}
	    end
    end.

%% -> {NewHandle, Reply}, 
%%    Reply = {Cont, Binaries, Bad} (Bad >= 0) | {error, Reason} | eof
mf_int_chunk_read_only(Handle, 0, Bin, N) ->
    FirstF = find_first_file(Handle),
    mf_int_chunk_read_only(Handle, {FirstF, 0}, Bin, N);
mf_int_chunk_read_only(#handle{curF = FileNo, cur_fdc = FdC, cur_name=FileName}
                       = Handle, {FileNo, Pos}, Bin, N) ->
    {NewFdC, Reply} = do_chunk_read_only(FdC, FileName, Pos, Bin, N),
    {Handle#handle{cur_fdc = NewFdC}, conv(Reply, FileNo)};
mf_int_chunk_read_only(Handle, {FileNo, Pos}, Bin, N) ->
    FName = add_ext(Handle#handle.filename, FileNo),
    NFileNo = inc(FileNo, Handle#handle.maxF),
    case catch int_open(FName, true, read_only, any) of
	{error, _Reason} ->
	   error_logger:info_msg("disk_log: chunk error. File ~p missing.\n\n",
				 [FName]),
	   mf_int_chunk_read_only(Handle, {NFileNo, 0}, [], N);
	{ok, {_Alloc, FdC, _HeadSize, _FileSize}} ->
	    case do_chunk_read_only(FdC, FName, Pos, Bin, N) of
		{NewFdC, eof} ->
		    file:close(NewFdC#cache.fd),
		    mf_int_chunk_read_only(Handle, {NFileNo,0}, [], N);
		{NewFdC, Other} ->
		    file:close(NewFdC#cache.fd),
		    {Handle, conv(Other, FileNo)}
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
	    {ok, #continuation{pos = {NFileNo, 0}, b = <<>>}};
	_Error ->
	    {error, end_of_log}
    end.

%% -> {ok, NewFdC} | throw(FileError)
mf_write_cache(#handle{filename = FName, cur_fdc = FdC} = Handle) ->
    erase(write_cache_timer_is_running),
    #cache{fd = Fd, c = C} = FdC,
    NewFdC = write_cache(Fd, FName, C),
    {ok, Handle#handle{cur_fdc = NewFdC}}.

mf_sync(#handle{filename = FName, cur_fdc = FdC} = Handle) ->
    {ok, NewFdC} = fsync(FdC, FName),
    {ok, Handle#handle{cur_fdc = NewFdC}}.

%% -> ok | throw(FileError)
mf_int_close(#handle{filename = FName, curF = CurF, cur_name = FileName, 
		  cur_fdc = CurFdC, cur_cnt = CurCnt}, Mode) ->
    close(CurFdC, FileName, Mode),
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
    {ok, FdC, FileName, Lost, {NoItems, NoBytes}, CurB} = 
	ext_file_open(FName, First, 0, 0, Head, Repair, Mode),
    CurCnt = Sz + NoItems - Lost,
    {ok, #handle{filename = FName, maxB = MaxB, cur_name = FileName, 
		 maxF = NewMaxF, cur_cnt = CurCnt, acc_cnt = -Sz,
		 curF = First, cur_fdc = FdC, firstPos = NoBytes,
		 curB = CurB, noFull = 0, accFull = 0},
     TotSz + CurCnt}.

%% -> {ok, handle(), Lost} 
%%   | {error, Error, handle()}
%%   | throw(FatalError)
%% Fatal errors should always terminate the log.
mf_ext_inc(Handle, Head) -> 
    #handle{filename = FName, cur_cnt = CurCnt, cur_name = FileName, 
	    acc_cnt = AccCnt, curF = CurF, maxF = MaxF, cur_fdc = CurFdC, 
	    noFull = NoFull} = Handle,
    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
	{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost} ->
	    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF, 
				    cur_name = NewFileName,
				    cur_cnt = Nh, acc_cnt = AccCnt + CurCnt, 
				    maxF = NewMaxF, firstPos = FirstPos, 
				    curB = FirstPos, noFull = NoFull + 1},
	    case catch fclose(CurFdC, FileName) of
		ok ->
		    {ok, Handle1, Lost};
		Error -> % Error in the last file, new file opened.
		    {error, Error, Handle1}
	    end;
	Error ->
	    {error, Error, Handle}
    end.

%% -> {ok, handle(), Logged, Lost} | {ok, handle(), Logged} 
%%    | {error, Error, handle(), Logged, Lost}

%% The returned handle is not always valid -
%% something may have been written before things went wrong.
mf_ext_log(Handle, Bins, Head) ->
    mf_ext_log(Handle, Bins, Head, 0, 0, 0).

mf_ext_log(Handle, [], _Head, No, _Lost, 0) ->
    {ok, Handle, No};
mf_ext_log(Handle, [], _Head, No, Lost, _Wraps) ->
    {ok, Handle, No, Lost};
mf_ext_log(Handle, Bins, Head, No0, Lost, Wraps) ->
    #handle{curB = CurB, maxB = MaxB, cur_name = FileName, cur_fdc = CurFdC, 
            firstPos = FirstPos0, cur_cnt = CurCnt} = Handle,
    {FirstBins, LastBins, NoBytes, N} = 
	ext_split_bins(CurB, MaxB, FirstPos0, Bins),
    case FirstBins of
	[] ->
            #handle{filename = FName, curF = CurF, maxF = MaxF, 
                    acc_cnt = AccCnt, noFull = NoFull} = Handle,
	    case catch wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) of
		{NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost1} ->
		    Handle1 = Handle#handle{cur_fdc = NewFdC, curF = NewF,
					    cur_cnt = Nh, 
					    cur_name = NewFileName,
					    acc_cnt = AccCnt + CurCnt, 
					    maxF = NewMaxF, 
					    curB = FirstPos, 
					    firstPos = FirstPos,
					    noFull = NoFull + 1},
		    case catch fclose(CurFdC, FileName) of
			ok ->
			    mf_ext_log(Handle1, Bins, Head, No0 + Nh, 
				       Lost + Lost1, Wraps + 1);
			Error ->
			    {error, Error, Handle1, No0 + Nh, Lost + Lost1}
		    end;
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end;
	_ ->
	    case catch fwrite(CurFdC, FileName, FirstBins, NoBytes) of
                {ok, NewCurFdC} ->
		    Handle1 = Handle#handle{cur_fdc = NewCurFdC, 
                                            curB = CurB + NoBytes, 
					    cur_cnt = CurCnt + N},
		    mf_ext_log(Handle1, LastBins, Head, No0 + N, Lost, Wraps);
		Error ->
		    {error, Error, Handle, No0, Lost}
	    end
    end.

wrap_ext_log(FName, CurF, MaxF, CurCnt, Head) ->
    {NewF, NewMaxF} = inc_wrap(FName, CurF, MaxF),
    {ok, NewFdC, NewFileName, Lost, {Nh, FirstPos}, _FileSize} = 
	ext_file_open(FName, NewF, CurF, CurCnt, Head),
    {NewF, NewMaxF, NewFdC, NewFileName, Nh, FirstPos, Lost}.

%% -> ok | throw(FileError)
mf_ext_close(#handle{filename = FName, curF = CurF,
		     cur_fdc = CurFdC, cur_cnt = CurCnt}, Mode) ->
    Res = (catch fclose(CurFdC, FName)),
    write_index_file(Mode, FName, CurF, CurF, CurCnt),
    Res.

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
%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} 
%%  | {repaired, FdC, FileName, Rec, Bad, FileSize} 
%%  | throw(Error)
int_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    N = add_ext(FName, NewFile),
    case int_open(N, Repair, Mode, Head) of
	{ok, {_Alloc, FdC, HeadSize, FileSize}} ->
	    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {ok, FdC, N, Lost, HeadSize, FileSize};
	{repaired, FdC, Recovered, BadBytes, FileSize} ->
	    write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {repaired, FdC, N, Recovered, BadBytes, FileSize}
    end.

%% -> {ok, FdC, FileName, Lost, HeadSize, FileSize} | throw(Error)
ext_file_open(FName, NewFile, OldFile, OldCnt, Head) ->
    Repair = truncate, Mode = read_write,
    ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode).

ext_file_open(FName, NewFile, OldFile, OldCnt, Head, Repair, Mode) ->
    FileName = add_ext(FName, NewFile),
    case ext_open(FileName, Repair, Mode, Head) of
	{ok, {_Alloc, FdC, HeadSize, FileSize}} ->
	    Lost = write_index_file(Mode, FName, NewFile, OldFile, OldCnt),
	    {ok, FdC, FileName, Lost, HeadSize, FileSize};
	Error ->
	    file_error(FileName, Error)
    end.

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

-define(index_file_name(F), add_ext(F, "idx")).

read_index_file(truncate, FName, MaxF) ->
    remove_files(FName, 2, MaxF),
    file:delete(?index_file_name(FName)),
    {1, 0, 0, 0};
read_index_file(_, FName, _MaxF) ->
    read_index_file(FName).

%% Used by wrap_log_reader.
%% -> {CurFileNo, CurFileSz, TotSz, NoFiles} | throw(FileError)
%%  where TotSz does not include CurFileSz.

read_index_file(FName) ->
    FileName = ?index_file_name(FName),
    case open_read(FileName) of
	{ok, Fd} ->
	    R = case file:read(Fd, ?MAX_CHUNK_SIZE) of
		    {ok, <<0, CurF:32, Tail/binary>>} 
		             when 0 < CurF, CurF < ?MAX_FILES -> 
			parse_index(CurF, 1, Tail, Fd, 0, 0, 0);
		    {ok, <<CurF, Tail/binary>>} when 0 < CurF -> 
			parse_index(CurF, 1, Tail, Fd, 0, 0, 0);
		    _ErrorOrEofeof ->
			{1, 0, 0, 0}
		end,
	    file:close(Fd),
	    R;
	_Error ->
	    {1, 0, 0, 0}
    end.

parse_index(CurF, CurF, <<CurSz:32, Tail/binary>>, Fd, _, TotSz, NFiles) ->
    parse_index(CurF, CurF+1, Tail, Fd, CurSz, TotSz, NFiles+1);
parse_index(CurF, N, <<Sz:32, Tail/binary>>, Fd, CurSz, TotSz, NFiles) ->
    parse_index(CurF, N+1, Tail, Fd, CurSz, TotSz + Sz, NFiles+1);
parse_index(CurF, N, B, Fd, CurSz, TotSz, NFiles) ->
    case file:read(Fd, ?MAX_CHUNK_SIZE) of
	eof when B == <<>> ->
	    {CurF, CurSz, TotSz, NFiles};	    
	{ok, Bin} ->
            NewB = list_to_binary([B, Bin]),
	    parse_index(CurF, N, NewB, Fd, CurSz, TotSz, NFiles);
	_ErrorOrEof ->
	    {1, 0, 0, 0}
    end.

%% Returns: Number of lost items (if an old file was truncated)
%% -> integer() | throw(FileError)
write_index_file(read_only, _FName, _NewFile, _OldFile, _OldCnt) ->
    0;
write_index_file(read_write, FName, NewFile, OldFile, OldCnt) ->
    FileName = ?index_file_name(FName),
    case open_update(FileName) of
	{ok, Fd} ->
	    case file:read(Fd, 1) of
		eof ->
		    fwrite_close2(Fd, FileName, <<0, NewFile:32>>);
		{ok, <<0>>} ->
		    pwrite_close2(Fd, FileName, 1, <<NewFile:32>>);
		{ok, <<_>>} -> % old format, do conversion
		    case file:read_file(FileName) of
			{ok, <<_CurF, Tail/binary>>} ->
			    position_close2(Fd, FileName, bof),
			    fwrite_close2(Fd, FileName, <<0, NewFile:32>>),
			    fwrite_close2(Fd, FileName, Tail);
			Error ->
			    file_error_close(Fd, FileName, Error)
		    end;
		Error ->
		    file_error_close(Fd, FileName, Error)
	    end,

	    OffSet = 5, 
	    NewPos = OffSet + (NewFile - 1)*4,
	    if
		OldFile > 0 ->
		    R = file:pread(Fd, NewPos, 4),
		    OldPos = OffSet + (OldFile - 1)*4,
		    pwrite_close2(Fd, FileName, OldPos, <<OldCnt:32>>),
		    file:close(Fd),
		    case R of
			{ok, <<Lost:32>>} -> Lost;
			{ok, _} -> 0; % Should be reported?
			eof    -> 0;
			Error2 -> file_error(FileName, Error2)
		    end;
		true -> 	
		    pwrite_close2(Fd, FileName, NewPos, <<OldCnt:32>>),
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
	    case file:read(Fd, 1) of
		eof ->
		    file:close(Fd),
		    ok;
		{ok, <<0>>} ->
		    truncate_index_file(Fd, FileName, 5, N);
		{ok, <<_>>} ->
		    truncate_index_file(Fd, FileName, 1, N);
		Error ->
		    file_error_close(Fd, FileName, Error)
	    end;
	Error ->
	    file_error(FileName, Error)
    end.

truncate_index_file(Fd, FileName, Offset, N) ->
    Pos = Offset + N*4,
    case Pos > file_size(FileName) of
	true ->
	    file:close(Fd);
	false ->
	    truncate_at_close2(Fd, FileName, {bof, Pos}),
	    file:close(Fd)
    end,
    ok.
	    
print_index_file(File) ->
    io:format("-- Index begin --~n"),
    case file:read_file(File) of
	{ok, <<0, CurF:32, Tail/binary>>} when 0 < CurF, CurF < ?MAX_FILES ->
	    io:format("cur file: ~w~n", [CurF]),
	    loop_index(1, Tail);
	{ok, <<CurF, Tail/binary>>} when 0 < CurF ->
	    io:format("cur file: ~w~n", [CurF]),
	    loop_index(1, Tail);
	_Else ->
	    ok
    end,
    io:format("-- end --~n").    

loop_index(N, <<Sz:32, Tail/binary>>) ->
    io:format(" ~p  items: ~w~n", [N, Sz]),
    loop_index(N+1, Tail);
loop_index(_, _) ->
    ok.

-define(size_file_name(F), add_ext(F, "siz")).

%% -> ok | throw(FileError)
write_size_file(read_only, _FName, _NewSize, _NewMaxFiles) ->
    ok;
write_size_file(read_write, FName, NewSize, NewMaxFiles) ->
    FileName = ?size_file_name(FName),
    case file:write_file(FileName, <<NewSize:32, NewMaxFiles:32>>) of
	ok -> 
	    ok;
	E -> 
	    file_error(FileName, E)
    end.
	    
%% -> {NoBytes, NoFiles}
read_size_file(FName) ->
    case file:read_file(?size_file_name(FName)) of
	{ok, <<Size:32, MaxFiles:32>>} ->
	    {Size, MaxFiles};
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
	    
%% -> {iolist(), LastBins, NoBytes, NoTerms}
ext_split_bins(CurB, MaxB, FirstPos, Bins) ->
    MaxBs = MaxB - CurB, IsFirst = CurB == FirstPos,
    ext_split_bins(MaxBs, IsFirst, [], Bins, 0, 0).

ext_split_bins(MaxBs, IsFirst, First, [X | Last], Bs, N) ->
    NBs = Bs + size(X),
    if
        NBs =< MaxBs ->
	    ext_split_bins(MaxBs, IsFirst, [First | X], Last, NBs, N+1);
	IsFirst == true, First == [] ->
            % To avoid infinite loop - we allow the file to be
   	    % too big if it's just one item on the file.
	    {[X], Last, NBs, N+1}; 
	true ->
	    {First, [X | Last], Bs, N}
    end;
ext_split_bins(_, _, First, [], Bs, N) ->
    {First, [], Bs, N}.

%% -> {iolist(), LastBins, NoBytes, NoTerms}
int_split_bins(CurB, MaxB, FirstPos, Bins) ->
    MaxBs = MaxB - CurB, IsFirst = CurB == FirstPos,
    int_split_bins(MaxBs, IsFirst, [], Bins, 0, 0).

int_split_bins(MaxBs, IsFirst, First, [X | Last], Bs, N) ->
    Sz = size(X),
    NBs = Bs + Sz + ?HEADERSZ,
    XB = [<<Sz:?SIZESZ/unit:8>>, ?MAGICHEAD | X],
    if
        NBs =< MaxBs ->
	    int_split_bins(MaxBs, IsFirst, [First | XB], Last, NBs, N+1);
	IsFirst == true, First == [] ->
            % To avoid infinite loop - we allow the file to be
   	    % too big if it's just one item on the file.
	    {[XB], Last, NBs, N+1}; 
	true ->
	    {First, [X | Last], Bs, N}
    end;
int_split_bins(_, _, First, [], Bs, N) ->
    {First, [], Bs, N}.

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


file_size(Fname) ->
    {ok, Fi} = file:read_file_info(Fname),
    Fi#file_info.size.

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

-define(MAX, ?MAX_CHUNK_SIZE). % bytes
-define(TIMEOUT, 2000). % ms

fwrite(#cache{c = []} = FdC, _FN, B, Size) ->
    case get(write_cache_timer_is_running) of
        true -> 
            ok;
        _ -> 
            put(write_cache_timer_is_running, true),
            erlang:send_after(?TIMEOUT, self(), {self(), write_cache})
    end,
    {ok, FdC#cache{sz = Size, c = B}};
fwrite(#cache{sz = Sz, c = C} = FdC, _FN, B, Size) when Sz < ?MAX ->
    {ok, FdC#cache{sz = Sz+Size, c = [C | B]}};
fwrite(#cache{fd = Fd, c = C}, FileName, B, _Size) ->
    {ok, write_cache(Fd, FileName, [C | B])}.

fwrite_header(Fd, B, Size) ->
    {ok, #cache{fd = Fd, sz = Size, c = B}}.

pread(#cache{fd = Fd, c = C}, FileName, Position, MaxBytes) ->
    NewFdC = write_cache(Fd, FileName, C),
    case file:pread(Fd, Position, MaxBytes) of
	{error, Error} -> file_error(FileName, {error, Error});
	Reply -> {NewFdC, Reply}
    end.

position(#cache{fd = Fd, c = []} = FdC, FileName, Pos) ->
    {ok, Loc} = position2(Fd, FileName, Pos),
    {FdC, Loc};
position(#cache{fd = Fd, c = C}, FileName, Pos) ->
    NewFdC = write_cache(Fd, FileName, C),
    {ok, Loc} = position2(Fd, FileName, Pos),
    {NewFdC, Loc}.
	    
position_close(#cache{fd = Fd, c = C}, FileName, Pos) ->
    NewFdC = write_cache_close(Fd, FileName, C),
    {ok, Loc} = position_close2(Fd, FileName, Pos),
    {NewFdC, Loc}.

fsync(#cache{fd = Fd, c = C}, FileName) ->
    NewFdC = write_cache(Fd, FileName, C),    
    case file:sync(Fd) of
	ok -> {ok, NewFdC};
	Error -> file_error(FileName, Error)
    end.

truncate_at(FdC, FileName, Pos) ->
    {NewFdC, _} = position(FdC, FileName, Pos),
    case file:truncate(NewFdC#cache.fd) of
	ok    -> {ok, NewFdC};
	Error -> file_error(FileName, Error)
    end.

fwrite_close2(Fd, FileName, B) ->
    case file:write(Fd, B) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

pwrite_close2(Fd, FileName, Position, B) ->
    case file:pwrite(Fd, Position, B) of
	ok -> ok;
	Error -> file_error(FileName, {error, Error})
    end.

position2(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error(FileName, {error, Error});
	OK -> OK
    end.

position_close2(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error_close(Fd, FileName, {error, Error});
	OK -> OK
    end.
	    
truncate_at_close2(Fd, FileName, Pos) ->
    position_close2(Fd, FileName, Pos),
    case file:truncate(Fd) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

fclose(#cache{fd = Fd, c = C}, FileName) ->
    %% The cache is empty if the file was opened in read_only mode.
    write_cache(Fd, FileName, C),
    file:close(Fd).

write_cache(Fd, _FileName, []) ->
    #cache{fd = Fd};
write_cache(Fd, FileName, C) ->
    case file:write(Fd, C) of
        ok    -> #cache{fd = Fd};
        Error -> file_error(FileName, Error)
    end.

write_cache_close(Fd, _FileName, []) ->
    #cache{fd = Fd};
write_cache_close(Fd, FileName, C) ->
    case file:write(Fd, C) of
        ok    -> #cache{fd = Fd};
        Error -> file_error_close(Fd, FileName, Error)
    end.

file_error(FileName, {error, Error}) ->
    throw({error, {file_error, FileName, Error}}).

file_error_close(Fd, FileName, {error, Error}) ->
    file:close(Fd),
    throw({error, {file_error, FileName, Error}}).
