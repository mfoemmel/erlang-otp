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
-module(beam_lib).

%%% Reads selected chunks from BEAM files.

-export([info/1,
	 chunks/2,
	 version/1,
	 format_error/1]).

-import(lists,
	[append/1, keysearch/3, keysort/2, nthtail/2, prefix/2, 
	 reverse/1, sort/1, splitwith/2]).

-include_lib("kernel/include/file.hrl").

-record(beam_handle, 
	{fd,                % file descriptor
	 file,              % file name
	 chunk_descs,       % [{ChunkName, StartPositionInFile, SizeInBytes}]
	                    % ChunkName = string()
	                    % StartPositionInFile = integer() > 0
	                    % SizeInBytes = integer > 0
	 atom_table,        % atoms read from the chunk "Atom"
	 module}).          % from the file's module declaration (module/1)

%%
%%  Exported functions
%%

info(FileName) ->
    catch read_info(FileName).

chunks(File, Chunks) ->
    catch read_chunk_data(File, Chunks).

version(File) ->
    case catch read_chunk_data(File, [attributes]) of
	{ok, {Module, [{attributes, Attrs}]}} ->
	    {value, {vsn, Version}} = keysearch(vsn, 1, Attrs),
	    {ok, {Module, Version}};
	Error ->
	    Error
    end.

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({not_a_file_name, Term}) ->
    io_lib:format("File name expected as argument, but ~p was given~n", 
		  [Term]);
format_error({not_a_list, Term}) ->
    io_lib:format("List of chunks expected as argument, but ~p was given~n", 
		  [Term]);
format_error({unknown_chunk, File, ChunkName}) ->
    io_lib:format("~s: Cannot find chunk ~p~n", [File, ChunkName]);
format_error({module_mismatch, File, Module}) ->
    io_lib:format("~s: Module name in object code is ~p~n", [File, Module]);
format_error({invalid_chunk, File, ChunkId}) ->
    io_lib:format("~s: Invalid contents of chunk ~p~n", [File, ChunkId]);
format_error({not_a_beam_file, File}) ->
    io_lib:format("~s: Not a BEAM file~n", [File]);
format_error({file_error, File, Reason}) ->
    io_lib:format("~s: ~p~n", [File, file:format_error(Reason)]);
format_error({missing_chunk, File, ChunkId}) ->
    io_lib:format("~s: Not a BEAM file: no IFF \"~s\" chunk~n", 
		  [File, ChunkId]);
format_error({form_not_beam, File}) ->
    io_lib:format("~s: Not a BEAM file: IFF form type is not \"BEAM\"~n", 
		  [File]);
format_error({form_too_big, File, Size, Len}) ->
    io_lib:format("~s: Form size ~p is greater than size ~p of module~n",
		  [File, Size, Len]);
format_error({invalid_beam_file, File, Pos}) ->
    io_lib:format("~s: Invalid format of BEAM file near byte number ~p~n", 
		  [File, Pos]);
format_error({chunk_too_big, File, ChunkId, Size, Len}) ->
    io_lib:format("~s: Size of chunk \"~s\" is ~p bytes, "
		  "but only ~p bytes could be read~n",
		  [File, ChunkId, Size, Len]);
format_error(E) ->
    io_lib:format("~p~n", [E]).

%%
%%  Local functions
%%

%% -> [InfoList] | throw(Error)
read_info(FileName) ->
    {ok, BH} = open_beam(FileName),
    #beam_handle{file = File, chunk_descs = CS, module = Module} = BH,
    Reply = [{file, File}, {module, Module}, {chunks, CS}],
    close_beam(BH),
    Reply.

%% -> {ok, {Module, Symbols}} | throw(Error)
read_chunk_data(File, ChunkNames) ->
    {ok, BH} = open_beam(File),
    Reply = (catch read_chunk_data(ChunkNames, BH, [])),
    close_beam(BH),
    Reply.
    
read_chunk_data([], BH, Symbols) ->
    Module = BH#beam_handle.module,
    {ok, {Module, reverse(Symbols)}};
read_chunk_data([N|Ns], BH, Symbols) ->
    #beam_handle{fd = Fd, file = File, chunk_descs = CS, atom_table = AT} = BH,
    {ok, Syms} = read_chunk(N, Fd, File, CS, AT),
    read_chunk_data(Ns, BH, [Syms | Symbols]);
read_chunk_data(NotAList, _BH, _Symbols) ->
    error({not_a_list, NotAList}).

%% -> {ok, BeamHandle} | throw(Error)
open_beam(File0) when atom(File0); list(File0) ->
    File = filename:rootname(File0, ".beam") ++ ".beam",
    case file:open(File, [read, raw, binary]) of
	{ok, Fd} ->
	    case catch open_beam(Fd, File) of
		{ok, BeamHandle} ->
		    {ok, BeamHandle};
		Error ->
		    ok = file:close(Fd),
		    throw(Error)
	    end;
	Error ->
	    file_error(File, Error)
    end;
open_beam(NotAFile) -> 
    error({not_a_file_name, NotAFile}).

open_beam(Fd, File) ->
    {ok, ChunkDescs} = find_chunks(Fd, File),
    {ok, Chunk} = get_chunk("Atom", Fd, File, ChunkDescs),
    AT = ets:new(beam_symbols, []),
    case catch extract_atoms(Chunk, AT) of
	{ok, Module} ->
	    BH = #beam_handle{fd = Fd, 
			      file = File, 
			      chunk_descs = ChunkDescs, 
			      atom_table = AT, 
			      module = Module},
	    Basename = filename:basename(File),
	    ModuleL = atom_to_list(Module),
	    case prefix(ModuleL, Basename) of
		true ->
		    case nthtail(length(ModuleL), Basename) of
			T when T == ".beam"; T == [] ->
			    {ok, BH};
			_ -> 
			    error({module_mismatch, File, Module})
		    end;
		false ->
		    error({module_mismatch, File, Module})
	    end;
	{'EXIT', _} ->
	    true = ets:delete(AT),
	    error({invalid_chunk, File, "Atom"})
    end.

%% -> ok | | throw(Error)
find_chunks(Fd, File) ->
    case read(Fd, File, 12) of
	eof -> 
	    error({not_a_beam_file, File});
	{ok, Head} when size(Head) < 12 ->
	    error({not_a_beam_file, File});
	{ok, Head} -> % when size(Head) == 12
	    FSize = (file_info(File))#file_info.size,
            % Verify the head of the IFF file	    
	    {Id, B1} = get_atom(Head, 4),
	    {Size, B2} = get_int(B1),
	    {BeamId, _} = get_atom(B2, 4),
	    if
		Id /= 'FOR1' ->
		    error({missing_chunk, File, "FOR1"});
		BeamId /= 'BEAM' ->
		    error({form_not_beam, File});
		Size > FSize - 8 ->
		    error({form_too_big, File, Size, FSize});
		true -> 
		    %% Find all IFF chunks
		    Start = 12,
		    find_chunks(Fd, File, FSize, Start, [])
	    end
    end.

%% -> {ok, [ChunkDescriptor]} | throw(Error)
find_chunks(Fd, File, FSize, Pos, Cs) ->
    case pread(Fd, File, Pos, 8) of
	eof -> 
	    {ok, reverse(Cs)};
	{ok, ChunkHead} when size(ChunkHead) < 8 ->
	    error({invalid_beam_file, File, Pos});
	{ok, ChunkHead} ->
	    {Id, B1} = get_string(ChunkHead, 4),
	    {Size, _} = get_int(B1),
	    Pos1 = Pos + 8,
	    Pos2 = (4 * trunc((Size+3) / 4)) + Pos1,
	    if 
		Pos2 > FSize ->
		    error({invalid_beam_file, File, Pos});
		true ->
		    find_chunks(Fd, File, FSize, Pos2, [{Id, Pos1, Size} | Cs])
	    end
    end.

extract_atoms(Chunk, AT) ->
    {_Num, B} = get_int(Chunk),
    extract_atoms(B, 1, AT),
    Module = atm(AT, 1),
    {ok, Module}.

extract_atoms(B, _I, _AT) when size(B) == 0 ->
    true;
extract_atoms(B, I, AT) ->
    {[Len], B1} = get_string(B, 1),
    {Atom, B2} = get_atom(B1, Len),
    true = ets:insert(AT, {I, Atom}),
    extract_atoms(B2, I+1, AT).

%% -> {ok, term()} | throw(Error)
read_chunk(Id, Fd, File, ChunkDescs, AtomTable) ->
    {ok, Chunk} = get_chunk(Id, Fd, File, ChunkDescs),
    chunk_to_data(Id, Chunk, File, AtomTable).

chunk_to_data(Id, Chunk, File, _AtomTable) when Id == attributes ->
    case catch binary_to_term(Chunk) of
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
	Term ->
	    {ok, {Id, attributes(Term)}}
    end;
chunk_to_data(Id, Chunk, File, _AtomTable) when Id == abstract_code ->
    case catch binary_to_term(Chunk) of
	{'EXIT', _} when size(Chunk) == 0 ->
	    {ok, {Id, no_abstract_code}};
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
	Term ->
	    {ok, {Id, Term}}
    end;
chunk_to_data(ChunkName, Chunk, File, AtomTable) when atom(ChunkName) ->
    case catch symbols(Chunk, AtomTable, ChunkName) of
	S when list(S) ->
	    {ok, {ChunkName, S}};
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(ChunkName, File)})
    end;
chunk_to_data(ChunkId, Chunk, _File, _AtomTable) -> % when list(ChunkId)
    {ok, {ChunkId, Chunk}}. % Chunk is a binary

%% -> {ok, binary()} | throw(Error)
get_chunk(ChunkName, Fd, File, ChunkDescs) when atom(ChunkName) ->
    ChunkId = chunk_name_to_id(ChunkName, File),
    get_chunk(ChunkId, Fd, File, ChunkDescs);
get_chunk(ChunkId, Fd, File, ChunkDescs) -> % when list(ChunkId)
    case keysearch(ChunkId, 1, ChunkDescs) of
	{value, ChunkDesc} ->
	    get_chunk(ChunkDesc, Fd, File);
	false ->
	    error({missing_chunk, File, ChunkId})
    end.

chunk_name_to_id(imports, _)       -> "ImpT";
chunk_name_to_id(exports, _)       -> "ExpT";
chunk_name_to_id(locals, _)        -> "LocT";
chunk_name_to_id(attributes, _)    -> "Attr";
chunk_name_to_id(abstract_code, _) -> "Abst";
chunk_name_to_id(Other, File) -> 
    error({unknown_chunk, File, Other}).

%% -> {ok, binary()} | throw(Error)
get_chunk({Id, Pos, Size}, Fd, File) ->
    case pread(Fd, File, Pos, Size) of
	eof when Size == 0 ->
	    {ok, list_to_binary([])};
	eof when Size > 0 ->
	    error({chunk_too_big, File, Id, Size, 0});
	{ok, Chunk} when Size > size(Chunk) ->
	    error({chunk_too_big, File, Id, Size, size(Chunk)});
	{ok, Chunk} when Size == size(Chunk) ->
	    {ok, Chunk}
    end.
    
close_beam(BeamHandle) ->
    ok = file:close(BeamHandle#beam_handle.fd),
    true = ets:delete(BeamHandle#beam_handle.atom_table),
    ok.

%% Extract attributes

attributes(Attrs) ->
    attributes(keysort(1, Attrs), []).

attributes([], R) ->
    reverse(R);
attributes(L, R) ->
    K = element(1, hd(L)),
    {L1, L2} = splitwith(fun(T) -> element(1, T) == K end, L),
    V = append([A || {N, A} <- L1]),
    attributes(L2, [{K, V} | R]).

%% Extract symbols

symbols(B, AT, Name) ->
    {_Num, B1} = get_int(B),
    sort(symbols(B1, AT, Name, [])).

symbols(B, _AT, _Name, S) when size(B) == 0 ->
    reverse(S);
symbols(B, AT, Name, S) ->
    {I1, B1} = get_int(B),
    {I2, B2} = get_int(B1),
    {I3, B3} = get_int(B2),
    Symbol = symbol(Name, AT, I1, I2, I3),
    symbols(B3, AT, Name, [Symbol|S]).

symbol(imports, AT, I1, I2, I3) ->
    {atm(AT, I1), atm(AT, I2), I3};
symbol(_, AT, I1, I2, _I3) ->
    {atm(AT, I1), I2}.

atm(AT, N) ->
    [{_N, S}] = ets:lookup(AT, N),
    S.

%% Utils.

i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_int(B) ->
    {I, B1} = split_binary(B, 4),
    {i32(binary_to_list(I)), B1}.

get_atom(B, N) ->
    {S, B1} = split_binary(B, N),
    {list_to_atom(binary_to_list(S)), B1}.

get_string(B, N) ->
    {S, B1} = split_binary(B, N),
    {binary_to_list(S), B1}.

file_info(FileName) ->
    case file:read_file_info(FileName) of
	{ok, Info} -> Info;
	{error, Error} -> file_error(FileName, {error, Error})
    end.

pread(Fd, FileName, Pos, Size) ->
    case file:pread(Fd, Pos, Size) of
	{error, Error} -> file_error(FileName, {error, Error});
	eof -> eof;
	Read -> Read
    end.

read(Fd, FileName, Size) ->
    case file:read(Fd, Size) of
	{error, Error} -> file_error(FileName, {error, Error});
	eof -> eof;
	Read -> Read
    end.

file_error(FileName, {error, Error}) ->
    error({file_error, FileName, Error}).

error(Reason) ->
    throw({error, ?MODULE, Reason}).
