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
%% Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $

-module(coff).

-export([pack/4, analyse_bin/1]).

%% coff:pack(BStub, Benv, BRing0, BCode) -> Bin
%%    BStub = binary containing a windows executable 
%%    Benv   = A binary representing an environment
%%               Key1=Val1\r\nKey2=Val2\r\n...KeyN=ValN\r\n
%%    BRing0 = beam code for ring0.erl
%%    BCode  = a binary whose value will be interpreted by ring0:start
%%    Bin    = Binary representing a patched file

%% analyse_bin(Bin) can be used to print out loads
%%                  of useful yuck - for this to work
%%                  the program must be compiled so that
%%                  debugging otput is produced.

-use([lists]).

-import(lists, [duplicate/2, seq/2, foreach/2, reverse/1, 
		reverse/2, map/2, mapfoldl/3, nth/2]).

-define(DWORD, 32/unsigned-little-integer).
-define(LONG,  32/unsigned-little-integer).
-define(WORD,  16/unsigned-little-integer).
-define(BYTE,  8/unsigned-little-integer).

%% swap the following lines to turn debugging on or off

io_format(Format, Data) -> true;
io_format(Format, Data) -> io:format(Format, Data).



%%----------------------------------------------------------------------

pack(BStub, BEnv, BRing0, BCode) ->
    %% round up all the size of the Bins so they are exact
    %% multiples of 4 bytes
    BEnv1   = bin_4(BEnv),
    BRing01 = bin_4(BRing0),
    BCode1  = bin_4(BCode),
    {PatchesIn, ResP, Res1, BinIn} = analyse_bin(BStub),
    {node, {0,0,0,0,I,J}, R} = Res1,
    Res2 = {node, 
	    {0,0,0,0,I+3,J},
	    [mk_node("ERLANG_CODE", BCode1),
	     mk_node("ERLANG_ENV", BEnv1),
	     mk_node("ERLANG_RING0", BRing01)|R]},
    Bin = pack_resource(ResP, Res2),
    Size = size(Bin),
    <<Before:ResP/binary, _/binary>> = BinIn,
    BinOut = <<Before/binary, Bin/binary>>,
    %% io_format("BinOut=~p~n",[BinOut]),
    BinFinal1 = bin_4096(BinOut),
    io_format("PatchesIn=~p~n",[PatchesIn]),
    Size1 = next_4096(Size), 
    io_format("Size before rounding=~p~n",[Size]),
    io_format("Size after rounding=~p~n",[Size1]),
    [{resStart,A1,_},
     {resSize, A2,_},
     {realSize, A3, _},
     {virtualAddress,A4,_}, %% Entry in the image data directory
     {sizeOfResource,A5,_}, %% Size of resource
     {pointerToResource,A6,_}] = PatchesIn,
    PatchesOut = 
	[{resStart,A1, ResP}, %% ResP,
	 {resSize,A2, Size}, %% Size
	 {realSize, A3, Size}, 
	 {virtualAddress,A4,ResP},
	 {sizeOfResource,A5,Size1}, % Size1},
	 {pointerToResource,A6,ResP}],
    io_format("PatchesOut=~p~n",[PatchesOut]),
    BinFinal2 = apply_patches(PatchesOut, BinFinal1),
    BinFinal2.

apply_patches([{Name, Addr, Val}|T], B) ->
    io_format("Patching ~p at address:~p to ~p~n", [Name, Addr, Val]),
    %% Len1 = Addr - 4,
    <<Before:Addr/binary, _:?DWORD, After/binary>> = B,
    B1 = <<Before/binary, Val:?DWORD, After/binary>>,
    %% Check the patch
    io_format("Old val at~p = ~p~n", [Addr, getDWORD(B, Addr)]),
    io_format("New val at~p = ~p~n", [Addr, getDWORD(B1, Addr)]),
    Val = New = getDWORD(B1, Addr),
    apply_patches(T, B1);
apply_patches([], B) ->
    B.

mk_node(Name, Data) ->
    {child,{name,Name},
     {node,{0,0,0,0,0,1},
      [{child,{id,1},
	{node,{0,0,0,0,0,1},
	 [{child,
	   {id,0},
	   {leaf,
	    {data,size(Data),Data}}}]}}]}}.

analyse_bin(Bin) ->
    X = get_resource_pointers(Bin),
    io_format("X=~p~n", [X]),
    {ResP, ResS, Patch} = X,
    Res = unpack_resources(Bin, ResP),
    {Patch, ResP, Res, Bin}.

get_resource_pointers(Bin) ->
    expectString("MZ", 0, 2, Bin),
    CoffStart = getDWORD(Bin, 60),
    %% print_a_little_bit("Coff=", Bin, CoffStart),
    io_format("Coff (PE) starts at ~p~n", [CoffStart]),
    expectString("PE\0\0", CoffStart, 4, Bin),
    CoffOptionalHeaderStart = CoffStart + 24,
    io_format("IMAGE_OPTIONAL_HEADER starts at ~p~n", 
	      [CoffOptionalHeaderStart]),
    %% The next line checks the Magic number at the start of the
    %% Coff image optional header
    %% print_a_little_bit("OptionalHeader", Bin, CoffOptionalHeaderStart),
    expectString([11,1], CoffOptionalHeaderStart, 2, Bin),
    LenStandardFields = 28,
    LenNTAdditionalFields = 68,
    
    ImageDataDirectory = CoffOptionalHeaderStart + LenStandardFields +
	LenNTAdditionalFields,
    io_format("IMAGE_DATA_DIRECTORY starts at ~p~n",[ImageDataDirectory]),
    print_a_little_bit("ImageDataDirectory", Bin, ImageDataDirectory),
    NumberOfRvaAndSizes = getDWORD(Bin, ImageDataDirectory - 4),
    io_format("NumberOfRvaAndSizes=~p~n", [NumberOfRvaAndSizes]),
    %% Each header is two DWORDS (8 bytes)
    %% The resources entry is the 3'th block
    %% so we have to skip 2 blocks (* 8 bytes)
    A1 = ImageDataDirectory + 16,
    A2 = A1 + 4,
    ResStart = getDWORD(Bin, A1),
    ResSize  = getDWORD(Bin, A2),
    %% io_format("ResStart = ~p RESSize=~p~n", [ResStart, ResSize]),
    Patch0 = [{resStart, A1, ResStart},
	      {resSize, A2, ResSize}],
    ImageSectionHeaders = ImageDataDirectory + 8 * NumberOfRvaAndSizes,
    %% print_a_little_bit("ImageSectionHeaders", Bin, ImageSectionHeaders),
    %% Now we have to look in the fourth block
    %% Each Byte = 40 Bytes
    %% ResourceSectionHeader is the pointer to IMAGE_OPTIONAL_HEADER
    ResourceSectionHeader = ImageSectionHeaders + 3 * 40,
    %% print_a_little_bit("ResourceSectionHeader", Bin, ResourceSectionHeader),
    %% Check we are in the right place :-)
    expectString(".rsrc\0\0\0", ResourceSectionHeader, 8, Bin),
    %% VPTR = Ptr to NT additional fields
    VPtr = ResourceSectionHeader + 12,
    VirtualAddress    = getDWORD(Bin, VPtr),
    SizeOfResource    = getDWORD(Bin, VPtr + 4),
    PointerToResource = getDWORD(Bin, VPtr + 8),
    PSizeOfInitializedData = ResourceSectionHeader + 8,
    SizeOfInitializedData = getDWORD(Bin, PSizeOfInitializedData),
    %% io_format("Vaddr=~p Size=~p Prt=~p~n", 
    %%           [VirtualAddress,  SizeOfResource, PointerToResource]),
    Patch1 = Patch0 ++ [{realSize, VPtr-4, getDWORD(Bin, VPtr-4)},
			{virtualAddress,    VPtr, VirtualAddress},
			{sizeOfResource,    VPtr+4, SizeOfResource},
			{pointerToResource, VPtr+8, PointerToResource}],
    %% Check that the resource data is at the end of the file
    Max = SizeOfResource + PointerToResource,
    case size(Bin) of
	Max ->
	    io_format(".rscs is at EOF - good :-)\n", []);
	_ ->
	    io_format(".rscs is malplaced - (this might be bad) ~p~n",
		      [size(Bin)])
    end,
    {PointerToResource, SizeOfResource, Patch1}.

%% unpack_resources(Bin, ResP) -> resourceTree()
%%   Unpack the resource data 
%%      ResP = Pointer to the start of the resource data
%%      Bin  = The content of the entire .exe file

%% +deftype resourceTree() = node()
%% +deftype node() = 
%%    {node, Data, [{Name,node()}]} | {leaf, Data}

unpack_resources(Bin, ResP) ->
    <<_:ResP/binary, Res/binary>> = Bin,
    build_tree(0, Res, Bin).

build_tree(OffSet, Resource, Bin) ->
    {_, Dir} = split_binary(Resource, OffSet),
    <<Characteristics    : ?DWORD,
     TimeDateStamp       : ?DWORD,
     MajorVersion        : ?WORD,
     MinorVersion        : ?WORD,
     NumberOfNamedEntries: ?WORD,
     NumberOfIdEntries   : ?WORD, Rest/binary>> = Dir,
    Facts = {Characteristics, TimeDateStamp, MajorVersion, MinorVersion,
	     NumberOfNamedEntries, NumberOfIdEntries},
    N = NumberOfIdEntries + NumberOfNamedEntries,
    Entries = collect_entries(N, Rest,
			      Dir,
			      []),
    {node, Facts, map(fun({Id,{set,Addr}}) ->
			      {child, Id, 
			       build_tree(Addr, Resource, Bin)};
			 ({Id, {unset, Addr}}) ->
			      {child, Id, 
			       make_leaf(Addr, Resource, Bin)}
		      end, Entries)}.

collect_entries(0, _, _, L) ->
    reverse(L);
collect_entries(N, Bin, Resource, L) ->
    <<Name:?DWORD,OffSetToData:?DWORD, Rest/binary>> = Bin,
    collect_entries(N-1, Rest, Resource,
		[{unpack_name(Name, Resource), peep(OffSetToData)}|L]).

extract_data(Size, Loc, Bin) ->
    {_, Tmp} = split_binary(Bin, Loc),
    {X, _} = split_binary(Tmp, Size),
    X.

make_leaf(OffSet, Resource, Bin) ->
    {_, Block} = split_binary(Resource, OffSet),
    <<Loc:?DWORD, Size:?DWORD, _/binary>> = Block,
    Data = extract_data(Size, Loc, Bin),
    {leaf, {data, Size, Loc, Data}}.

unpack_name(Name, Resource) ->
    case peep(Name) of
	{set, Addr} ->
	    {_, TheNameBin} = split_binary(Resource, Addr),
	    <<Len:?WORD,Tmp/binary>> = TheNameBin,
	    {Uni,_} = split_binary(Tmp, Len*2),
	    Str = uni2str(binary_to_list(Uni)),
	    {name, Str};
	{unset, Addr} ->
	    {id, Addr}
    end.

getDWORD(Bin, N) ->
    <<_:N/binary, X:?DWORD, _/binary>> = Bin,
    X.

expectString(Str, Loc, Len, Bin) ->
    <<_:Loc/binary, X:Len/binary, _/binary>> = Bin,
    case binary_to_list(X) of
	Str ->
	    true;
	F ->
	    io_format("String: ~p not found at location: ~p found:~w~n", 
		      [Str, Loc, F]),
	    exit(fatal)
    end.

print_a_little_bit(Str, Bin, Loc) ->
    L1 = Loc + 50,
    <<_:Loc/binary, X:50/binary, _/binary>> = Bin,
    io_format("~s~p ...~n", [Str, X]).

peep(X) ->
    Set = case (X band 16#80000000) of
	      0 -> unset;
	      _ -> set
	  end,
    Val = (X band 16#7fffffff),
    {Set,Val}.

%%----------------------------------------------------------------------
%% pack_resource

pack_resource(Start, Tree) ->
    %% --
    io_format("Tree=~P~n",[Tree,40]),
    %% Mr Gates wants me to breadth first flatten the tree
    {L1, Free} = bff(Tree, 1, top),
    io_format("L1=~P~n",[L1,60]),
    %% Now it's flattened so we can rip out the names
    {L2, Names, Free1} = remove_names(L1, Free, [], []),
    io_format("L2=~P~nNames=~p~nFree=~p~n", [L2, 70, Names,Free1]),
    %% And the data
    {L3, DataDict, Free2} = 
	remove_data(L2, Free, [], []),
    io_format("L3=~P~n DataDict=~P~n"
	      "Free=~p~n", [L3, 70, DataDict, 40,Free2]),
    assemble(Start, L3, Names, DataDict).
    

%%----------------------------------------------------------------------

remove_names([{node1,Data,Children}|T], Free, L, O) ->
    {Free1, Children1, O1} = remove_names1(Children, Free, [], O),
    remove_names(T, Free1, [{node2, Data, Children1}|L], O1);
remove_names([H|T], Free, L, O) ->
    remove_names(T, Free, [H|L], O);
remove_names([], Free, L, O) ->
    {reverse(L), reverse(O), Free}.

remove_names1([{child, {name,Name}, Data}|T], Free, L, O) ->
    remove_names1(T, Free+1, [{child, {namePtr,Free}, Data}|L],
		 [{Free, Name}|O]);
remove_names1([H|T], Free, L, O) ->
    remove_names1(T, Free, [H|L], O);
remove_names1([], Free, L, O) ->
    {Free, reverse(L), O}.

%%----------------------------------------------------------------------

remove_data([{node2,Data,Children}|T], Free, L, DataDict) ->
    {Free1, Children1, DataDict1} = 
	remove_data1(Children, Free, [], DataDict),
    remove_data(T, Free1, [{node3,Data,Children1}|L], DataDict1);
remove_data([H|T], Free, L, DD) ->
    remove_data(T, Free, [H|L], DD);
remove_data([], Free, L, DD) ->
    {reverse(L), reverse(DD), Free}.

remove_data1([{child, Name, {leaf, Data}}|T], Free, L, O) ->
    remove_data1(T, Free+1,
		 [{child, Name, {dataPtr,Free}}|L],
		 [{dataItem,Free,Data}|O]);
remove_data1([H|T], Free, L, O) ->
    remove_data1(T, Free, [H|L], O);
remove_data1([], Free, L, O) ->
    {Free, reverse(L), O}.

%%----------------------------------------------------------------------
%% bff = breadth first flatten

%% bff(Tree, Free, Name) ->
%%   Breadth first flatten tree of node adding labels
%%   Name is the name of the top node

bff(Tree, Free, Name) ->
    Q = [{Free, Tree}],
    bff_more(Q, Free+1, []).

bff({node,Facts,L}, Label, Free, Q, Out) ->
    {L1, Free1, Q1} = bff_children(L, Free, [], []),
    bff_more(Q ++ Q1, Free1, reverse([{label, Label}, 
				      {node1,Facts,L1}], Out)).

bff_children([Obj={child,Name,{leaf, _}}|T], Free, L, Q) ->
    bff_children(T, Free, [Obj|L], Q);
bff_children([{child,Name, Node={node,_,_}}|T], Free, L, Q) ->
    bff_children(T, Free+1,
		 [{child,Name,{nodePtr, Free}}|L],
		 [{Free,Node}|Q]);
bff_children([], Free, L, Q) ->
    {reverse(L), Free, reverse(Q)}.


bff_more([{Label, Tree}|Q], Free, Out) ->
    bff(Tree, Label, Free, Q, Out);
bff_more([], Free, Out) ->
    {reverse(Out), Free}.

%%----------------------------------------------------------------------
%% assemble(Start,
%%    Start Is the first (Absolute addess to assemble into)

assemble(Start, Dict, Names, DataDict) ->		    
    DictSize = dict_size(Dict, 0),
    io_format("data Dictionary size=~p~n",[DictSize]),
    DataDescSize = 16 * length(DataDict),
    io_format("data Dictionary size=~p~n",[DataDescSize]),
    {BinNames, Symtab1} = asm_names(Names),
    io_format("BinNames=~p~n Symbtab1=~p~n",[BinNames, Symtab1]),
    NamesSize = size(BinNames),
    %% bug DDStartPointer = Start + DictSize,
    DDStartPointer = DictSize,
    %% Names Start Pointer is relative :-)
    NamesStartPointer = DictSize + DataDescSize,
    {BinData, Symtab2}  = asm_binData(DataDict),
    io_format("BinNames = ~p~n Symbtab1=~p~n",[BinNames, Symtab1]),
    io_format("BinData  = ~P~n Symbtab2=~p~n",[BinData, 50, Symtab2]),
    %% AbsDataStart = Start (physical address) of the data
    AbsDataStart =  Start + DictSize + DataDescSize + NamesSize,
    {BinDD, Symtab3} = asm_dataDict(DataDict,AbsDataStart,Symtab2),
    io_format("BinDD=~p~nSymtab3=~p~n",[BinDD, Symtab3]),
    %% Now we can make the data dictionary
    %% finally
    BinDict = asm_dict(Dict, 
		       NamesStartPointer, Symtab1,
		       DDStartPointer, Symtab3),
    io_format("size(BinDict)=~p size(BinDD)=~p size(BinNames)= ~p~n"
	      "size(BinData)=~p~n",
	      [size(BinDict), size(BinDD),size(BinNames),size(BinData)]),
    Ball = concat_binary([BinDict, BinDD, BinNames, BinData]),
    io_format("size(Ball)=~p~n", [size(Ball)]),
    Ball.

asm_dict(Dict, NStart, Nsymbs, DDStart, DDsymbs) ->
    Pass1 = asm_dict_pass1(Dict, NStart, Nsymbs, DDStart, DDsymbs, []),
    io_format("Pass1=~P~n", [Pass1, 50]),
    Fixups = make_fixup_table(Pass1, 0, []),
    io_format("Fixups=~p~n",[Fixups]),
    Bins = apply_fixups(Pass1, Fixups, []),
    io_format("Bins=~p~n",[Bins]),
    concat_binary(Bins).

make_fixup_table([{label,L}|T], Pos, Tab) ->
    make_fixup_table(T, Pos, [{L,Pos}|Tab]);
make_fixup_table([H|T], Pos, Tab) when binary(H) ->
    make_fixup_table(T, Pos + size(H), Tab);
make_fixup_table([{nodePtr,I}|T], Pos, Tab) ->
    make_fixup_table(T, Pos + 4, Tab);
make_fixup_table([], _, Tab) ->
    reverse(Tab).

apply_fixups([{label,_}|T], Fixups, L) ->
    apply_fixups(T, Fixups, L);
apply_fixups([Bin|T], Fixups, L) when binary(Bin) ->
    apply_fixups(T, Fixups, [Bin|L]);
apply_fixups([{nodePtr,I}|T], Fixups, L) ->
    OffSet = lookup1(I, Fixups),
    N1 = OffSet bor 16#80000000,
    B = <<N1:?DWORD>>,
    apply_fixups(T, Fixups, [B|L]);
apply_fixups([], _, Tab) ->
    reverse(Tab).

asm_dict_pass1([{label, X}|T], NStart, Nsymbs, DDStart, DDsymbs, L) ->
    asm_dict_pass1(T, NStart, Nsymbs, DDStart, DDsymbs, [{label,X}|L]);
asm_dict_pass1([{node3,Facts,Children}|T], NStart,  Nsymbs, 
	                                   DDStart, DDsymbs, L) ->
    B1 = asm_facts(Facts),
    L1 = asm_children(Children,  NStart, Nsymbs, DDStart, DDsymbs, 
		      [B1|L]),
    asm_dict_pass1(T, NStart, Nsymbs, DDStart, DDsymbs, L1);
asm_dict_pass1([], NStart, Nsymbs, DDStart, DDsymbs, L) ->
    reverse(L).

asm_children([{child,X,Y}|T], NStart, Nsymbs, DDStart, DDsymbs, L) ->
    B1 = asm_name(X, NStart, Nsymbs),
    B2 = asm_ptr(Y,  DDStart, DDsymbs),
    asm_children(T, NStart, Nsymbs, DDStart, DDsymbs, [B2,B1|L]); 
asm_children([], _, _, _, _, L) ->
    L.

asm_name({id, N}, _, _) ->
    <<N:?DWORD>>;
asm_name({namePtr, Index}, Start, Syms) ->
    {OffSet, _} = lookup(Index, Syms),
    N1 = (OffSet + Start) bor 16#80000000,
    <<N1:?DWORD>>.

asm_ptr({nodePtr, N}, _, _) -> {nodePtr, N};
asm_ptr({dataPtr, Index}, Start, Syms) ->
    OffSet = lookup1(Index, Syms),
    %% bug N1 = (OffSet + Start) bor 16#80000000,
    N1 = (OffSet + Start),
    <<N1:?DWORD>>.

asm_facts({A,B,C,D,E,F}) ->
    <<A:?DWORD, B:?DWORD, C:?WORD, D:?WORD, E:?WORD, F:?WORD>>.

%%----------------------------------------------------------------------

dict_size([{label,_}|T], N) ->
    dict_size(T, N);
dict_size([{node3,_,Children}|T], N) ->
    dict_size(T, N + 16 + length(Children) * 8);
dict_size([], N) ->
    N.

%% Names = [{Index,Name}]
asm_names(Names) ->
    io_format("Names=~p~n",[Names]),
    L1 = map(fun({Index, Str}) -> 
		     Bin = str2uni(Str),
		     Size = size(Bin),
		     {Index, Size, Bin} 
	     end, Names),
    mk_symtab(L1).

mk_symtab(L1) ->
    Bins = map(fun(I) -> element(3, I) end, L1),
    Bin = concat_binary(Bins),
    {Symbtab,_} = mapfoldl(fun({Index,Size,_},Start) ->
				   {{Index,Start,Size}, Start+Size}
			   end, 0, L1),
    {Bin, Symbtab}.

asm_binData(Data) ->
    L1 = map(fun({dataItem,Index,{data,_,_,Bin}}) ->
		     {Index, size(Bin), Bin};
		({dataItem,Index,{data,_,Bin}}) ->
		     {Index, size(Bin), Bin}
		end, Data),
    mk_symtab(L1).

%%----------------------------------------------------------------------
%%    {Bin, Symbtab} = asm_dataDict(DataDict, Start, Symtab2),

asm_dataDict(D, Start, Symbs) ->
    asm_dataDict(D,  Symbs, Start, 0, [], []).

asm_dataDict([{dataItem,Index,_}|T], Symbs,Start, Pos, Bins, S) ->
    {OffSet, Size} = lookup(Index, Symbs),
    %% Construct the binary
    Loc = Start + OffSet,
    io_format("Index=~p Packing size=~p Loc=~p in datadict~n",
	      [Index,Size, Loc]),
    B = <<Loc:?DWORD, Size:?DWORD, 0:?DWORD, 0:?DWORD>>,
    asm_dataDict(T, Symbs, Start, Pos + 16, [B|Bins], [{Index,Pos}|S]);
asm_dataDict([], _, _, _, Bins, Symbtab) ->
    {concat_binary(reverse(Bins)), reverse(Symbtab)}.

%%----------------------------------------------------------------------

lookup(Index, [{Index,Start,Size}|_]) -> {Start, Size};
lookup(Index, [_|T])                  -> lookup(Index, T).

lookup1(Index, [{Index,Start}|_])     -> Start;
lookup1(Index, [_|T])                 -> lookup1(Index, T).

%%----------------------------------------------------------------------
%% Increase the size of a binary to make it an exact
%% multiple of 4 or 4096 bytes

bin_4(Bin)    -> bin_padup(Bin, 4).

bin_4096(Bin) -> bin_padup(Bin, 4096).


bin_padup(Bin, Size) ->
    L1 = size(Bin),
    case (L1 rem Size) of
	0 ->
	    Bin;
	N ->
	    B1 = list_to_binary(duplicate(Size-N, 0)),
	    <<Bin/binary,B1/binary>>
    end.

%%----------------------------------------------------------------------
%% Size1 = next_4096(Size).
%%  Size1 is Size rounded up to the next 4096 boundary

next_4096(N) ->
    N1 = N div 4096,
    case N rem 4096 of
	0 -> N1 * 4096;
	_ -> (N1 + 1) * 4096
    end.

%%----------------------------------------------------------------------
%% unicode conversion

uni2str([H,_|T]) -> [H|uni2str(T)];
uni2str(_)       -> [].

str2uni(S) ->
    Str1 = [length(S),0] ++ str2uni1(S),
    Str2 = case (length(Str1) rem 4) of
	       0 -> Str1;
	       N -> Str1 ++ duplicate(4-N, 0)
	   end,
    list_to_binary(Str2).
    
str2uni1([H|T]) -> [H,0] ++ str2uni1(T);
str2uni1([])    -> [].

