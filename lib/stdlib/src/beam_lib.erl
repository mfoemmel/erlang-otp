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
%%     $Id $
%%
-module(beam_lib).

-export([info/1,
	 cmp/2,
	 cmp_dirs/2,
	 chunks/2,
	 all_chunks/1,
	 diff_dirs/2,
	 strip/1,
	 strip_files/1,
	 strip_release/1,
	 build_module/1,
	 version/1,
	 format_error/1]).

-import(lists,
	[append/1, delete/2, foreach/2, keydelete/3, keymember/3, keysearch/3,
	 keysort/2, map/2, member/2, nthtail/2, prefix/2, reverse/1, 
	 sort/1, splitwith/2]).

-include_lib("kernel/include/file.hrl").

%%
%%  Exported functions
%%

info(File) ->
    catch read_info(beam_filename(File)).

chunks(File, Chunks) ->
    catch read_chunk_data(File, Chunks).

all_chunks(File) ->
    catch read_all_chunks(File).

cmp(File1, File2) ->
    catch cmp_files(File1, File2).

cmp_dirs(Dir1, Dir2) ->
    catch compare_dirs(Dir1, Dir2).

diff_dirs(Dir1, Dir2) ->
    catch diff_directories(Dir1, Dir2).

strip(FileName) ->
    catch strip_file(FileName).
    
strip_files(Files) when list(Files) ->
    catch strip_fils(Files).
    
strip_release(Root) ->
    catch strip_rel(Root).

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
format_error({unknown_chunk, File, ChunkName}) ->
    io_lib:format("~p: Cannot find chunk ~p~n", [File, ChunkName]);
format_error({invalid_chunk, File, ChunkId}) ->
    io_lib:format("~p: Invalid contents of chunk ~p~n", [File, ChunkId]);
format_error({not_a_beam_file, File}) ->
    io_lib:format("~p: Not a BEAM file~n", [File]);
format_error({file_error, File, Reason}) ->
    io_lib:format("~p: ~p~n", [File, file:format_error(Reason)]);
format_error({missing_chunk, File, ChunkId}) ->
    io_lib:format("~p: Not a BEAM file: no IFF \"~s\" chunk~n", 
		  [File, ChunkId]);
format_error({invalid_beam_file, File, Pos}) ->
    io_lib:format("~p: Invalid format of BEAM file near byte number ~p~n", 
		  [File, Pos]);
format_error({chunk_too_big, File, ChunkId, Size, Len}) ->
    io_lib:format("~p: Size of chunk \"~s\" is ~p bytes, "
		  "but only ~p bytes could be read~n",
		  [File, ChunkId, Size, Len]);
format_error({chunks_different, Id}) ->
    io_lib:format("Chunk \"~s\" differs in the two files~n", [Id]);
format_error({modules_different, Module1, Module2}) ->
    io_lib:format("Module names ~p and ~p differ in the two files~n", 
		  [Module1, Module2]);
format_error({not_a_directory, Name}) ->
    io_lib:format("~p: Not a directory~n", [Name]);
format_error(E) ->
    io_lib:format("~p~n", [E]).

%%
%%  Local functions
%%

read_info(File) ->
    {ok, Module, Data} = scan_beam(File, info),
    [if
	 binary(File) -> {binary, File};
	 true -> {file, File}
     end, {module, Module}, {chunks, Data}].

diff_directories(Dir1, Dir2) ->
    {OnlyDir1, OnlyDir2, Diff} = compare_dirs(Dir1, Dir2),
    diff_only(Dir1, OnlyDir1),
    diff_only(Dir2, OnlyDir2),
    foreach(fun(D) -> io:format("** different: ~p~n", [D]) end, Diff),
    ok.
    
diff_only(_Dir, []) -> 
    ok;
diff_only(Dir, Only) ->
    io:format("Only in ~p: ~p~n", [Dir, Only]).

%% -> {OnlyInDir1, OnlyInDir2, Different} | throw(Error)
compare_dirs(Dir1, Dir2) ->
    R1 = sofs:relation(beam_files(Dir1)),
    R2 = sofs:relation(beam_files(Dir2)),
    F1 = sofs:domain(R1),
    F2 = sofs:domain(R2),
    {O1, Both, O2} = sofs:symmetric_partition(F1, F2),
    OnlyL1 = sofs:image(R1, O1),
    OnlyL2 = sofs:image(R2, O2),
    B1 = sofs:to_external(sofs:restriction(R1, Both)),
    B2 = sofs:to_external(sofs:restriction(R2, Both)),
    Diff = compare_files(B1, B2, []),
    {sofs:to_external(OnlyL1), sofs:to_external(OnlyL2), Diff}.

compare_files([], [], Acc) ->
    lists:reverse(Acc);
compare_files([{_,F1} | R1], [{_,F2} | R2], Acc) ->
    NAcc = case catch cmp_files(F1, F2) of
	       {error, _Mod, _Reason} ->
		   [{F1, F2} | Acc];
	       ok ->
		   Acc
	   end,
    compare_files(R1, R2, NAcc).

beam_files(Dir) ->
    ok = assert_directory(Dir),
    L = filelib:wildcard(filename:join(Dir, "*.beam")),
    lists:map(fun(Path) -> {filename:basename(Path), Path} end, L).

%% -> ok | throw(Error)
cmp_files(File1, File2) ->
    Chunks = ["Code", "ExpT", "ImpT", "StrT", "Atom"],
    {ok, {M1, L1}} = read_chunk_data(File1, Chunks),
    {ok, {M2, L2}} = read_chunk_data(File2, Chunks),
    if
	M1 == M2 ->
	    cmp_lists(L1, L2);
	true ->
	    error({modules_different, M1, M2})
    end.

%% It is assumed that all chunks are present in both files.
cmp_lists([], []) ->
    ok;
cmp_lists([{Id, C1} | R1], [{Id, C2} | R2]) ->
    if
	C1 == C2 ->
	    cmp_lists(R1, R2);
	true ->
	    error({chunks_different, Id})
    end.
    
strip_rel(Root) ->
    ok = assert_directory(Root),
    strip_fils(filelib:wildcard(filename:join(Root, "lib/*/ebin/*.beam"))).

%% -> {ok, [{Mod, BinaryOrFileName}]} | throw(Error)
strip_fils(Files) ->
    Fun = fun(F) -> {ok, Reply} = strip_file(F), Reply end,
    L = map(Fun, Files),
    {ok, L}.

%% -> {ok, {Mod, FileName}} | {ok, {Mod, binary()}} | throw(Error)
strip_file(File) ->
    OldEssential = ["Atom", "Code", "StrT", "ImpT", "ExpT"],
    Attributes = ["Attr", "CInf"],
    Ids0 = OldEssential ++ ["FunT"|Attributes],
    Chunks = case catch read_chunk_data(File, Ids0) of
		 {ok, {Mod, Chunks0}} ->
		     Chunks0;
		 _ ->
		     Ids = OldEssential ++ Attributes,
		     {ok, {Mod, Chunk0}} = read_chunk_data(File, Ids),
		     Chunk0
	     end,
    {ok, Stripped0} = build_module(Chunks),
    Stripped = compress(Stripped0),
    case File of
	_ when is_binary(File) ->
	    {ok, {Mod, Stripped}};
	_ ->
	    FileName = beam_filename(File),
	    case file:open(FileName, [raw, binary, write]) of
		{ok, Fd} ->
		    case file:write(Fd, Stripped) of
			ok ->
			    file:close(Fd),
			    {ok, {Mod, FileName}};
			Error ->
			    file:close(Fd),
			    file_error(FileName, Error)
		    end;
		Error ->
		    file_error(FileName, Error)
	    end
    end.

build_module(Chunks0) ->
    Chunks = list_to_binary(build_chunks(Chunks0)),
    Size = size(Chunks),
    0 = Size rem 4, % Assertion: correct padding?
    {ok, <<"FOR1", (Size+4):32, "BEAM", Chunks/binary>>}.

build_chunks([{Id, Data} | Chunks]) ->
    BId = list_to_binary(Id),
    Size = size(Data),
    Chunk = [<<BId/binary, Size:32>>, Data | pad(Size)],
    [Chunk | build_chunks(Chunks)];
build_chunks([]) -> 
    [].

pad(Size) ->
    case Size rem 4 of
	0 -> [];
	Rem -> lists:duplicate(4 - Rem, 0)
    end.

read_all_chunks(File0) when atom(File0);
			    list(File0); 
			    binary(File0) ->
    File = beam_filename(File0),
    {ok, Module, ChunkIds0} = scan_beam(File, info),
    ChunkIds = [Name || {Name,_,_} <- ChunkIds0],
    {ok, Module, Chunks} = scan_beam(File, ChunkIds),
    {ok, Module, lists:reverse(Chunks)}.

%% -> {ok, {Module, Symbols}} | throw(Error)
read_chunk_data(File0, ChunkNames0) when atom(File0); 
					 list(File0); 
					 binary(File0) ->
    File = beam_filename(File0),
    {ChunkIds, Names} = check_chunks(ChunkNames0, File, [], []),
    {ok, Module, Chunks} = scan_beam(File, ChunkIds),
    AT = ets:new(beam_symbols, []),
    T = {empty, AT},
    R = (catch chunks_to_data(Names, Chunks, File, Chunks, Module, T, [])),
    ets:delete(AT),
    R.
    
%% -> {ok, list()} | throw(Error)
check_chunks([ChunkName | Ids], File, IL, L) when atom(ChunkName) ->
    ChunkId = chunk_name_to_id(ChunkName, File),
    check_chunks(Ids, File, [ChunkId | IL], [{ChunkId, ChunkName} | L]);
check_chunks([ChunkId | Ids], File, IL, L) -> % when list(ChunkId)
    check_chunks(Ids, File, [ChunkId | IL], [{ChunkId, ChunkId} | L]);
check_chunks([], _File, IL, L) ->
    {lists:usort(IL), reverse(L)}.

%% -> {ok, Module, Data} | throw(Error)
scan_beam(File, What) ->
    FD = open_file(File),
    case catch scan_beam2(FD, What) of
	Error when error == element(1, Error) ->
	    throw(Error);
	R ->
	    R
    end.
scan_beam2(FD, What) ->
    case pread(FD, 0, 12) of
	{NFD, {ok, <<"FOR1", _Size:32, "BEAM">>}} ->
	    Start = 12,
	    scan_beam(NFD, Start, What, 17, []);
	_Error -> 
	    error({not_a_beam_file, filename(FD)})
    end.

scan_beam(_FD, _Pos, [], Mod, Data) when Mod =/= 17 ->
    {ok, Mod, Data};    
scan_beam(FD, Pos, What, Mod, Data) ->
    case pread(FD, Pos, 8) of
	{_NFD, eof} when Mod == 17 ->
	    error({missing_chunk, filename(FD), "Atom"});	    
	{_NFD, eof} when What == info ->
	    {ok, Mod, reverse(Data)};
	{_NFD, eof} ->
	    error({missing_chunk, filename(FD), hd(What)});
	{NFD, {ok, <<IdL:4/binary, Sz:32>>}} ->
	    Id = binary_to_list(IdL),
	    Pos1 = Pos + 8,
	    Pos2 = (4 * trunc((Sz+3) / 4)) + Pos1,
	    get_data(What, Id, NFD, Sz, Pos1, Pos2, Mod, Data);
	{_NFD, {ok, _ChunkHead}} ->
	    error({invalid_beam_file, filename(FD), Pos})
    end.

get_data(Cs, Id, FD, Size, Pos, Pos2, _Mod, Data) when Id == "Atom" ->
    NewCs = del_chunk(Id, Cs),
    {NFD, Chunk} = get_chunk(Id, Pos, Size, FD),
    <<_Num:32, Chunk2/binary>> = Chunk,
    {Module, _} = extract_atom(Chunk2),
    C = case Cs of
	    info -> 
		{Id, Pos, Size};
	    _ -> 
		{Id, Chunk}
	end,
    scan_beam(NFD, Pos2, NewCs, Module, [C | Data]);
get_data(info, Id, FD, Size, Pos, Pos2, Mod, Data) ->
    scan_beam(FD, Pos2, info, Mod, [{Id, Pos, Size} | Data]);
get_data(Chunks, Id, FD, Size, Pos, Pos2, Mod, Data) ->
    {NFD, NewData} = case member(Id, Chunks) of
			 true ->
			     {FD1, Chunk} = get_chunk(Id, Pos, Size, FD),
			     {FD1, [{Id, Chunk} | Data]};
			 false ->
			     {FD, Data}
	      end,
    NewChunks = del_chunk(Id, Chunks),
    scan_beam(NFD, Pos2, NewChunks, Mod, NewData).
     
del_chunk(_Id, info) ->
    info;
del_chunk(Id, Chunks) ->
    delete(Id, Chunks).

%% -> {NFD, binary()} | throw(Error)
get_chunk(Id, Pos, Size, FD) ->
    case pread(FD, Pos, Size) of
	{NFD, eof} when Size == 0 -> % cannot happen
	    {NFD, <<>>};
	{_NFD, eof} when Size > 0 ->
	    error({chunk_too_big, filename(FD), Id, Size, 0});
	{_NFD, {ok, Chunk}} when Size > size(Chunk) ->
	    error({chunk_too_big, filename(FD), Id, Size, size(Chunk)});
	{NFD, {ok, Chunk}} -> % when Size == size(Chunk)
	    {NFD, Chunk}
    end.

chunks_to_data([{Id, Name} | CNs], Chunks, File, Cs, Module, Atoms, L) ->
    {value, {_Id, Chunk}} =  keysearch(Id, 1, Chunks),
    {NewAtoms, Ret} = chunk_to_data(Name, Chunk, File, Cs, Atoms),
    chunks_to_data(CNs, Chunks, File, Cs, Module, NewAtoms, [Ret | L]);
chunks_to_data([], _Chunks, _File, _Cs, Module, _Atoms, L) ->
    {ok, {Module, reverse(L)}}.

chunk_to_data(Id, Chunk, File, _Cs, AtomTable) when Id == attributes ->
    case catch binary_to_term(Chunk) of
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
	Term ->
	    {AtomTable, {Id, attributes(Term)}}
    end;
chunk_to_data(Id, Chunk, File, _Cs, AtomTable) when Id == abstract_code ->
    case catch binary_to_term(Chunk) of
	{'EXIT', _} when <<>> == Chunk ->
	    {AtomTable, {Id, no_abstract_code}};
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(Id, File)});
	Term ->
	    {AtomTable, {Id, Term}}
    end;
chunk_to_data(Id, _Chunk, _File, Cs, AtomTable0) when Id == atoms ->
    AtomTable = ensure_atoms(AtomTable0, Cs),
    Atoms = ets:tab2list(AtomTable),
    {AtomTable, {Id, lists:sort(Atoms)}};
chunk_to_data(ChunkName, Chunk, File, Cs, AtomTable) when atom(ChunkName) ->
    case catch symbols(Chunk, AtomTable, Cs, ChunkName) of
	{ok, NewAtomTable, S} ->
	    {NewAtomTable, {ChunkName, S}};
	{'EXIT', _} ->
	    error({invalid_chunk, File, chunk_name_to_id(ChunkName, File)})
    end;
chunk_to_data(ChunkId, Chunk, _File, _Cs, AtomTable) -> % when list(ChunkId)
    {AtomTable, {ChunkId, Chunk}}. % Chunk is a binary

chunk_name_to_id(atoms, _)           -> "Atom";
chunk_name_to_id(indexed_imports, _) -> "ImpT";
chunk_name_to_id(imports, _)         -> "ImpT";
chunk_name_to_id(exports, _)         -> "ExpT";
chunk_name_to_id(labeled_exports, _) -> "ExpT";
chunk_name_to_id(locals, _)          -> "LocT";
chunk_name_to_id(labeled_locals, _)  -> "LocT";
chunk_name_to_id(attributes, _)      -> "Attr";
chunk_name_to_id(abstract_code, _)   -> "Abst";
chunk_name_to_id(Other, File) -> 
    error({unknown_chunk, File, Other}).

%% Extract attributes

attributes(Attrs) ->
    attributes(keysort(1, Attrs), []).

attributes([], R) ->
    reverse(R);
attributes(L, R) ->
    K = element(1, hd(L)),
    {L1, L2} = splitwith(fun(T) -> element(1, T) == K end, L),
    V = append([A || {_, A} <- L1]),
    attributes(L2, [{K, V} | R]).

%% Extract symbols

symbols(<<_Num:32, B/binary>>, AT0, Cs, Name) ->
    AT = ensure_atoms(AT0, Cs),
    symbols1(B, AT, Name, [], 1).

symbols1(<<I1:32, I2:32, I3:32, B/binary>>, AT, Name, S, Cnt) ->
    Symbol = symbol(Name, AT, I1, I2, I3, Cnt),
    symbols1(B, AT, Name, [Symbol|S], Cnt+1);
symbols1(<<>>, AT, _Name, S, _Cnt) ->
    {ok, AT, sort(S)}.

symbol(indexed_imports, AT, I1, I2, I3, Cnt) ->
    {Cnt, atm(AT, I1), atm(AT, I2), I3};
symbol(imports, AT, I1, I2, I3, _Cnt) ->
    {atm(AT, I1), atm(AT, I2), I3};
symbol(Name, AT, I1, I2, I3, _Cnt) when Name == labeled_exports; 
					Name == labeled_locals ->
    {atm(AT, I1), I2, I3};
symbol(_, AT, I1, I2, _I3, _Cnt) ->
    {atm(AT, I1), I2}.

atm(AT, N) ->
    [{_N, S}] = ets:lookup(AT, N),
    S.

%% AT is updated.
ensure_atoms({empty, AT}, Cs) ->
    {value, {_Id, AtomChunk}} =  keysearch("Atom", 1, Cs),
    extract_atoms(AtomChunk, AT),
    AT;
ensure_atoms(AT, _Cs) ->
    AT.

extract_atoms(<<_Num:32, B/binary>>, AT) ->
    extract_atoms(B, 1, AT).

extract_atoms(<<>>, _I, _AT) ->
    true;
extract_atoms(B, I, AT) ->
    {Atom, B1} = extract_atom(B),
    true = ets:insert(AT, {I, Atom}),
    extract_atoms(B1, I+1, AT).

extract_atom(<<Len, B/binary>>) ->
    <<SB:Len/binary, Tail/binary>> = B,
    {list_to_atom(binary_to_list(SB)), Tail}.

%%% Utils.

-record(bb, {pos = 0, bin, source}).

open_file(<<"FOR1",_/binary>>=Binary) ->
    #bb{bin = Binary, source = Binary};
open_file(Binary0) when is_binary(Binary0) ->
    Binary = uncompress(Binary0),
    #bb{bin = Binary, source = Binary};
open_file(FileName) ->
    case file:open(FileName, [read, raw, binary]) of
	{ok, Fd} ->
	    read_all(Fd, FileName, []);
	Error ->
	    file_error(FileName, Error)
    end.

read_all(Fd, FileName, Bins) ->
    case file:read(Fd, 1 bsl 18) of
	{ok, Bin} ->
	    read_all(Fd, FileName, [Bin | Bins]);
	eof ->
	    file:close(Fd),
	    #bb{bin = uncompress(reverse(Bins)), source = FileName};
	Error ->
	    file:close(Fd),
	    file_error(FileName, Error)
    end.

pread(FD, AtPos, Size) ->
    #bb{pos = Pos, bin = Binary} = FD,
    Skip = AtPos-Pos,
    case Binary of
	<<_:Skip/binary, B:Size/binary, Bin/binary>> ->
	    NFD = FD#bb{pos = AtPos+Size, bin = Bin},
	    {NFD, {ok, B}};
	<<_:Skip/binary, Bin/binary>> when size(Bin) > 0 ->
	    NFD = FD#bb{pos = AtPos+size(Bin), bin = <<>>},
	    {NFD, {ok, Bin}};
        _ ->
            {FD, eof}
    end.

filename(BB) when binary(BB#bb.source) ->
    BB#bb.source;
filename(BB) -> 
    list_to_atom(BB#bb.source).    

beam_filename(Bin) when binary(Bin) ->
    Bin;
beam_filename(File) ->
    filename:rootname(File, ".beam") ++ ".beam".

uncompress(Binary0) ->
    {ok, Fd} = ram_file:open(Binary0, [write, binary]),
    {ok, _} = ram_file:uncompress(Fd),
    {ok, Binary} = ram_file:get_file(Fd),
    ok = ram_file:close(Fd),
    Binary.

compress(Binary0) ->
    {ok, Fd} = ram_file:open(Binary0, [write, binary]),
    {ok, _} = ram_file:compress(Fd),
    {ok, Binary} = ram_file:get_file(Fd),
    ok = ram_file:close(Fd),
    Binary.

%% -> ok | throw(Error)
assert_directory(FileName) ->
    case filelib:is_dir(FileName) of
	true ->
	    ok;
	false ->
	    error({not_a_directory, FileName})
    end.

file_error(FileName, {error, Error}) ->
    error({file_error, FileName, Error}).

error(Reason) ->
    throw({error, ?MODULE, Reason}).
