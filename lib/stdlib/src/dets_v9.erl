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
-module(dets_v9).

%% Dets files, implementation part. This module handles version 9.
%% To be called from dets.erl only.

-export([constants/0, mark_dirty/1, read_file_header/2,
         check_file_header/2, do_perform_save/1, initiate_file/10,
         init_freelist/2, fsck_objs/5, bulk_objects/2, fsck_output/4,
         direct_lookup/2, write_cache/1, may_grow/3, find_object/2,
         slot_objs/2, scan_objs/7, db_hash/2, no_slots/1]).

-export([file_info/1, v_segments/1]).

-export([cache_segps/3]).

-compile({inline, [{sz2pos,1},{adjsz,1},{to_objects,2},{scan_skip,8}]}).
-compile({inline, [{skip_bytes,6},{make_object,4}]}).
-compile({inline, [{get_segp,1},{get_arrpart,1}]}).
-compile({inline, [{actual_seg_size,0}]}).

-include("dets.hrl").

%%  The layout of the file is :
%%
%%   bytes   decsription
%%  ---------------------- File header
%%    4      FreelistsPointer
%%    4      Cookie
%%    4      ClosedProperly (pos=8)
%%    4      Type (pos=12)
%%    4      Version (pos=16)
%%    4      M
%%    4      Next
%%    4      KeyPos
%%    4      NoObjects
%%    4      NoKeys
%%    4      MinNoSlots
%%    4      MaxNoSlots
%%    4      MaxObjSize
%%    4      N
%%    256    Reserved for future versions. Initially zeros.
%%  ------------------ end of file header
%%    4*256  SegmentArray Pointers.
%%  ------------------ This is BASE.
%%    4*512  SegmentArray Part 1
%%    ...    More SegmentArray Parts
%%    8*256  First segment
%%    ???    Objects (free and alive)
%%    4*512  Further SegmentArray Part.
%%    ???    Objects (free and alive)
%%    8*256  Further segment.
%%    ???    Objects (free and alive)
%%    ... more objects, segment array parts, and segments ...
%%  -----------------------------
%%    ???    Free lists
%%  -----------------------------
%%    4      File size, in bytes. 

%%  Before we can find an object we must find the slot where the
%%  object resides. Each slot is a (possibly empty) list (or chain) of
%%  objects that hash to the same slot. If the value stored in the
%%  slot is zero, the slot chain is empty. If the slot value is
%%  non-zero, the value points to a position in the file where the
%%  collection of objects resides. Each collection has the following
%%  layout:
%%
%%   bytes  decsription
%%  --------------------
%%    4     Size of the area allocated for the collection (8+Sz)
%%    4     Status  (FREE or ACTIVE). These two are the Object Header.
%%    Sz    A binary containing the objects per key, sorted on key
%% 
%%  When repairing or converting a file, the status field is used.
%%
%%  The binary containing the objects per key of a table of type 'set'
%%  has the following layout:
%%
%%   bytes  decsription
%%  --------------------
%%    4     Size of the object of the first key (4+OSz1)
%%    OSz1  The object of the first key
%%    ...
%%    4     Size of the object of the ith key (4+OSzi)
%%    OSzi  The object of the ith key
%% 
%%  The binary containing the objects per key of a table of type 'bag'
%%  or 'duplicate_bag' has the following layout:
%%
%%   bytes    decsription
%%  ----------------------
%%    4       Size of the objects of the first key (4 + OSz1_1+...+OSz1_j+...)
%%    4       Size of the first object of the first key (4+OSz1_1)
%%    OSz1_1  The first object of the first key
%%    ...
%%    4       Size of the jth object of the first key (4+OSz1_j)
%%    OSz1_j  The jth object of the first key
%%    ...
%%    4       Size of the objects of the ith key (4 + OSzi_1+...+OSzi_k+...)
%%    4       Size of the first object of the ith key (4+OSzi_1)
%%    OSzi_1  The first object of the ith key
%%    ...
%%    4       Size of the kth object of the ith key (4+OSzi_k)
%%    OSzi_k  The kth object of the ith key
%%    ...
%%
%%  The objects of a key are placed in time order, that is, the older
%%  objects come first. If a new object is inserted, it is inserted
%%  last.
%%
%%
%%
%%|---------------|
%%|      head     |
%%|       	  |
%%|               |
%%|_______________|
%%|               |--|
%%|___part ptr 1__|  |
%%|               |  | segarr part 1
%%|___part ptr 2__|  V______________|
%%|               |  |   p1         |
%%|               |  |______________|--|
%%|     ....      |  |   p2         |  |
%%     (256)         |______________|  |
%%                   |              |  |
%%                   |     ....     |  | segment 1
%%                   |    (512)     |  V __slot 0 ____|
%%                                     |   size       |
%%                                     |   pointer    |--|
%%                                     |___slot 1 ____|  |
%%                                     |              |  |
%%                                     |   ....       |  |  objects in slot 0
%%                                         (256)         V  segment 1
%%                                                       |___________|
%%                                                       |  size     |
%%                                                       |___________|
%%                                                       |  status   |
%%                                                       |___________|
%%                                                       |           |
%%                                                       |   object  |
%%                                                       |   collec. |
%%                                                       |___________|

%%%
%%% File header
%%%

-define(RESERVED, 256).        % Reserved for future use.

-define(HEADSZ, 56).           % The size of the file header, in bytes.
-define(HEADEND, (?HEADSZ+?RESERVED)). % End of header and reserved area.
-define(SEGSZ, 512).           % Size of a segment, in words. SZOBJP*SEGSZP.
-define(SEGSZP, 256).          % Size of a segment, in number of pointers.
-define(SEGPARTSZ, 512).       % Size of segment array part, in words.
-define(SEGARRSZ, 256).        % Maximal number of segment array parts..
-define(SEGARRADDR(PartN), (?HEADEND + (4 * (PartN)))).
-define(SEGPARTADDR(P,SegN), ((P) + (4 * ((SegN) rem ?SEGPARTSZ)))).
-define(BASE, ?SEGARRADDR(?SEGARRSZ)).
-define(MAXSLOTS, (?SEGARRSZ * ?SEGPARTSZ * ?SEGSZP)).

%% BIG is used for hashing. BIG must be greater than the maximum
%% number of slots, currently 32 M (MAXSLOTS).
-define(BIG, 16#3ffffff). % 64 M

%% Hard coded positions into the file header:
-define(FREELIST_POS, 0).
-define(CLOSED_PROPERLY_POS, 8).
-define(D_POS, 20).
-define(NO_KEYS_POS, (?D_POS + 16)).

%%% Dets file versions up to 8 are handled in dets_v8. This module
%%% handles version 9, introduced in R8.

-define(FILE_FORMAT_VERSION, 9).

-define(NOT_PROPERLY_CLOSED,0).
-define(CLOSED_PROPERLY,1).

%% Size of object pointer, in words. SEGSZ = SZOBJP * SEGSZP.
-define(SZOBJP, 2).

-define(OHDSZ, 8).          % The size of the object header, in bytes.
-define(STATUS_POS, 4).     % Position of the status field.

-define(OHDSZ_v8, 12).      % The size of the version 8 object header.

%% The size of each object is a multiple of 16.
%% BUMP is used when repairing files.
-define(BUMP, 16).

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%% {Bump}
constants() ->
    {?BUMP, ?BASE}.

%% -> ok | throw({NewHead,Error})
mark_dirty(Head) ->
    Dirty = [{?CLOSED_PROPERLY_POS, <<?NOT_PROPERLY_CLOSED:32>>}],
    dets_utils:pwrite(Head, Dirty),
    file:sync(Head#head.fptr),
    dets_utils:position(Head, Head#head.freelists_p),
    dets_utils:truncate(Head).

%% -> {ok, head()} | throw(Error)
initiate_file(Fd, Tab, Fname, Type, Kp, MinSlots, MaxSlots0, 
	      Ram, CacheSz, Auto) ->
    %% Remove cached pointers to segment array parts and segments:
    lists:foreach(fun({I1,I2}) when integer(I1), integer(I2) -> ok;
		     ({K,V}) -> put(K, V)
		  end, erase()),
    MaxSlots = lists:min([MaxSlots0, ?MAXSLOTS]),

    Factor = est_no_segments(MinSlots),
    dets_utils:position(Fd, Fname, bof),
    dets_utils:truncate(Fd, Fname),
    Freelist = 0,
    Cookie = ?MAGIC,
    Version = ?FILE_FORMAT_VERSION,
    ClosedProperly = ?NOT_PROPERLY_CLOSED, % immediately overwritten
    N = 0,
    M = Next = ?SEGSZP * Factor,
    NoObjects = 0,
    NoKeys = 0,
    MaxObjSize = 0,
    dets_utils:pwrite(Fd, Fname, 0, 
                      <<Freelist:32,
                      Cookie:32,
                      ClosedProperly:32,
                      (dets_utils:type_to_code(Type)):32,
                      Version:32,
                      M:32,
                      Next:32,
                      Kp:32,
                      NoObjects:32,
                      NoKeys:32,
		      MinSlots:32,
		      MaxSlots:32,
		      MaxObjSize:32,
                      N:32,
		      0:?RESERVED/unit:8>>), %% Reserved area.

    Ftab = dets_utils:init_alloc(?BASE),

    %% We need to initialize the segment pointer array,
    dets_utils:position(Fd, Fname, ?HEADEND),
    zero(Fd, Fname, ?SEGARRSZ),

    H0 = #head{freelists=Ftab, fptr = Fd, base = ?BASE},
    %% Initialize array parts. 
    %% All parts before segments, for the sake of repair.
    Zero = seg_zero(),
    {H1, Ws1} = init_parts(H0, 0, no_parts(MinSlots), Zero, []),
    dets_utils:pwrite(Fd, Fname, Ws1),
    %% Initialize segments.
    {H2, Ws2} = init_segments(H1, 0, Factor, Zero, []),
    dets_utils:pwrite(Fd, Fname, Ws2),

    Head = #head{
      m  = M,
      next = Next,
      fptr = Fd,
      no_objects = NoObjects,
      no_keys = NoKeys,
      maxobjsize = MaxObjSize,
      n = N,
      type = Type,
      freelists = H2#head.freelists,
      auto_save = Auto,
      hash_bif = phash,
      keypos = Kp,
      min_no_slots = Factor * ?SEGSZP,
      max_no_slots = no_segs(MaxSlots) * ?SEGSZP,
      
      ram_file = Ram, 
      filename = Fname, 
      name = Tab,
      cache = dets_utils:new_cache(CacheSz),
      version = Version,
      bump = ?BUMP,
      base = ?BASE,
      mod = ?MODULE
     },
    case catch do_perform_save(Head) of
	{NewHead, ok} ->
	    {ok, NewHead};
	{_, Error} ->
	    throw(Error)
    end.

est_no_segments(MinSlots) ->
    no_segs(MinSlots).

init_parts(Head, PartNo, NoParts, Zero, Ws) when PartNo < NoParts ->
    PartPos = ?SEGARRADDR(PartNo),
    {NewHead, W, _Part} = alloc_part(Head, Zero, PartPos),
    init_parts(NewHead, PartNo+1, NoParts, Zero, [W | Ws]);
init_parts(Head, _PartNo, _NoParts, _Zero, Ws) ->
    {Head, lists:append(lists:reverse(Ws))}.

init_segments(Head, SegNo, Factor, SegZero, Ws) when SegNo < Factor ->
    {NewHead, W} = allocate_segment(Head, SegZero, SegNo),
    init_segments(NewHead, SegNo+1, Factor, SegZero, [W | Ws]);
init_segments(Head, _SegNo, _Factor, _SegZero, Ws) ->
    {Head, lists:append(lists:reverse(Ws))}.

allocate_segment(Head, SegZero, SegNo) ->
    PartPos = ?SEGARRADDR(SegNo div ?SEGPARTSZ),
    case get_arrpart(PartPos) of
	undefined ->
	    %% may throw error:
	    {Head1, [InitArrPart, ArrPartPointer], Part} = 
		alloc_part(Head, SegZero, PartPos),
            {NewHead, [InitSegment, SegPointer]} = 
                alloc_seg(Head1, SegZero, SegNo, Part),
	    {NewHead, [InitSegment, InitArrPart, SegPointer, ArrPartPointer]};
	Part ->
            alloc_seg(Head, SegZero, SegNo, Part)
    end.

alloc_part(Head, PartZero, PartPos) ->
    %% may throw error:
    {NewHead, Part} = dets_utils:alloc(Head, adjsz(4 * ?SEGPARTSZ)),
    arrpart_cache(PartPos, Part),
    InitArrPart = {Part, PartZero}, % same size as segment
    ArrPartPointer = {PartPos, <<Part:32>>},
    {NewHead, [InitArrPart, ArrPartPointer], Part}.

alloc_seg(Head, SegZero, SegNo, Part) ->
    %% may throw error:
    {NewHead, Segment} = dets_utils:alloc(Head, adjsz(4 * ?SEGSZ)), 
    InitSegment = {Segment, SegZero},
    Pos = ?SEGPARTADDR(Part, SegNo),
    segp_cache(Pos, Segment),
    SegPointer = {Pos, <<Segment:32>>},
    {NewHead, [InitSegment, SegPointer]}.

%% Read free lists (using a Buddy System) from file. 
init_freelist(Head, true) ->
    Pos = Head#head.freelists_p,
    free_lists_from_file(Head, Pos).

%% -> {ok, Fd, fileheader()} | throw(Error)
read_file_header(Fd, FileName) ->
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, 0, ?HEADSZ),
    [Freelist, Cookie, CP, Type2, Version, M, Next, Kp, 
     NoObjects, NoKeys, MinNoSlots, MaxNoSlots, MaxObjSize, N] 
	= bin2ints(Bin),
    {ok, EOF} = dets_utils:position_close(Fd, FileName, eof),
    {ok, <<FileSize:32>>} = dets_utils:pread_close(Fd, FileName, EOF-4, 4),
    FH = #fileheader{freelist = Freelist,
		     cookie = Cookie,
		     closed_properly = CP,
		     type = dets_utils:code_to_type(Type2),
		     version = Version,
		     m = M,
		     next = Next,
		     keypos = Kp,
		     no_objects = NoObjects,
		     no_keys = NoKeys,
		     min_no_slots = MinNoSlots,
		     max_no_slots = MaxNoSlots,
		     maxobjsize = MaxObjSize,
		     trailer = FileSize,
		     eof = EOF,
		     n = N,
		     mod = ?MODULE},
    {ok, Fd, FH}.

%% -> {ok, head(), ExtraInfo} | {error, Reason} (Reason lacking file name)
%% ExtraInfo = true
check_file_header(FH, Fd) ->
    Test = 
	if
	    FH#fileheader.cookie /= ?MAGIC ->
		{error, not_a_dets_file};
	    FH#fileheader.type == badtype ->
		{error, invalid_type_code};
	    FH#fileheader.version /= ?FILE_FORMAT_VERSION -> 
                {error, bad_version};
	    FH#fileheader.trailer /= FH#fileheader.eof ->
		{error, not_closed};
	    FH#fileheader.closed_properly == ?CLOSED_PROPERLY ->
		{ok, true};
	    FH#fileheader.closed_properly == ?NOT_PROPERLY_CLOSED ->
		{error, not_closed};
	    true ->
		{error, not_a_dets_file}
	end,
    case Test of
	{ok, ExtraInfo} ->
	    H = #head{
	      m = FH#fileheader.m,
	      next = FH#fileheader.next,
	      fptr = Fd,
	      no_objects = FH#fileheader.no_objects,
	      no_keys = FH#fileheader.no_keys,
	      maxobjsize = FH#fileheader.maxobjsize,
	      n = FH#fileheader.n,
	      type = FH#fileheader.type,
	      update_mode = saved,
	      auto_save = infinity,             % not saved on file
	      fixed = false,			% not saved on file
	      freelists_p = FH#fileheader.freelist,
	      hash_bif = phash,
	      keypos = FH#fileheader.keypos},
	    {ok, H, ExtraInfo};
	Error ->
	    Error
    end.

cache_segps(Fd, FileName, M) ->
    NoParts = no_parts(M),
    ArrStart = ?SEGARRADDR(0),
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, ArrStart, 4 * NoParts),
    F = fun(ArrPartPos, Pos) ->
		arrpart_cache(Pos, ArrPartPos),
		{ok, ArrPartBin} = dets_utils:pread_close(Fd, FileName, 
							  ArrPartPos, 
							  ?SEGPARTSZ*4),
		%% Too many are cached, but it does not matter.
		Fun = fun(S, P) -> segp_cache(P, S), P+4 end,
		lists:foldl(Fun, ArrPartPos, bin2ints(ArrPartBin)),
		Pos+4
	end,
    lists:foldl(F, ?HEADEND, bin2ints(Bin)).

no_parts(NoSlots) ->
    ((NoSlots - 1)  div (?SEGSZP * ?SEGPARTSZ)) + 1.

no_segs(NoSlots) ->
    ((NoSlots - 1)  div ?SEGSZP) + 1.

bin2ints(<<Int:32, B/binary>>) ->
    [Int | bin2ints(B)];
bin2ints(B) when size(B) == 0 ->
    [].

%%%
%%% Repair and conversion of a dets file.
%%%

bulk_objects(Head, Kp) ->
    fun(T, Seq) -> 
            Key = element(Kp, T),
            BT = term_to_binary(T),
            {make_object(Head, Key, Seq, BT), Seq+1}
    end.

-define(FSCK_SEGMENT, 1).
-define(FSCK_SEGMENT2, 10000).

-ifdef(vector).
-define(VNEW(N, E), vector:new(N, E)).
-define(VSET(I, V, E), vector:set(I, V, E)).
-define(VGET(I, V), vector:get(I, V)).
-else.
-define(VNEW(N, E), erlang:make_tuple(N, E)).
-define(VSET(I, V, E), setelement(I, V, E)).
-define(VGET(I, V), element(I, V)).
-define(VEXT(S, V, T), 
	list_to_tuple(tuple_to_list(V) ++ lists:duplicate(S-size(V), T))).
-endif.

fsck_output(OldV, Head, SlotNums, Cntrs) when OldV =< 9 ->
    fun(close) ->
	    {ok, 0, Head};
       ([]) ->
	    fsck_output(OldV, Head, SlotNums, Cntrs);
       (L) ->
	    %% Information about number of objects per size is not
	    %% relevant for version 9. It is number of collections
	    %% that matters.
            true = ets:delete_all_objects(Cntrs),
	    [E | Es] = bin2term(L, OldV),	    
	    ZT = dets_utils:make_zero_table(512),
	    %% The cache is tuple indexed by the (log) size. An element
	    %% is [[NoObjs | NoKeys] | [BinaryObject]].
	    Cache = ?VNEW(2, [[0 | 0] | []]),
	    {NE, Acc, NCache} = output_slots(E, Es, [E], ZT, Head, Cache),
	    fsck_output2(NE, Acc, OldV, Head, ZT, NCache, Cntrs, SlotNums, 1,1)
    end.

fsck_output2(E, Acc, OldV, Head, ZT, Cache, FileT, SlotNums, 0, MaxNoChunks) ->
    {NCache, NHead} = write_all_sizes(Cache, FileT, Head),
    fsck_output2(E, Acc, OldV, NHead, ZT, NCache, FileT, SlotNums,
		 MaxNoChunks, MaxNoChunks);
fsck_output2(E, Acc, OldV, Head, ZT, Cache, FileT, SlotNums, ChunkI,
	     MaxNoChunks) ->
    fun(close) ->
	    Cache1 = output_slot(Acc, ZT, Head, Cache),
	    {_NCache, Head1} = write_all_sizes(Cache1, FileT, Head),
	    SegSz = actual_seg_size(),
	    {_, SegEnd} = dets_utils:alloc(Head1, adjsz(SegSz)),
	    {NewHead, NL} = allocate_all_objects(Head1, FileT),
	    %% It is not known until all objects have been collected
	    %% how many object collections there are per size. Now
	    %% that is known and the absolute positions of the object
	    %% collections can be calculated.
            segment_file(FileT, NewHead, NL, ZT, SegEnd),
	    {MinSlots, EstNoSlots, MaxSlots} = SlotNums,
	    if 
		EstNoSlots == bulk_init ->
		    {ok, 0, NewHead};
		true ->
		    NoKeys = NewHead#head.no_keys,
		    EstNoSegs = no_segs(EstNoSlots),
		    MinNoSegs = no_segs(MinSlots),
		    MaxNoSegs = no_segs(MaxSlots),
		    NoSegs = no_segs(NoKeys),
		    Diff = abs(NoSegs - EstNoSegs),
		    if 
			Diff > 5, NoSegs =< MaxNoSegs, NoSegs >= MinNoSegs  ->
			    {try_again, NoKeys};
			true ->
			    {ok, 0, NewHead}
		    end
	    end;
       (L) ->
	    Es = bin2term(L, OldV),
	    {NE, NAcc, NCache} = output_slots(E, Es, Acc, ZT, Head, Cache),
	    fsck_output2(NE, NAcc, OldV, Head, ZT, NCache, FileT, SlotNums,
			 ChunkI-1, MaxNoChunks)
    end.

%% By allocating bigger objects before smaller ones, holes in the
%% buddy system memory map are avoided. Unfortunately, the segments
%% are always allocated first, so if there are objects bigger than a
%% segment, there is a hole to handle. (Haven't considered placing the
%% segments among other objects of the same size.)
allocate_all_objects(Head, FileT) ->
    DTL = lists:reverse(lists:sort(ets:tab2list(FileT))),
    MaxSz = element(1, hd(DTL)),
    SegSize = actual_seg_size(),
    {Head1, HSz, HN, HA} = alloc_hole(MaxSz, Head, SegSize),
    {Head2, NL} = allocate_all(Head1, DTL, []),
    Head3 = free_hole(Head2, HSz, HN, HA),
    {Head3, NL}.

alloc_hole(LSize, Head, SegSz) when ?POW(LSize-1) > SegSz ->
    Size = ?POW(LSize-1),
    {_, SegAddr} = dets_utils:alloc(Head, adjsz(SegSz)),
    {_, Addr} = dets_utils:alloc(Head, adjsz(Size)),
    N = (Addr - SegAddr) div SegSz,
    Head1 = dets_utils:alloc_many(Head, SegSz, N, SegAddr),
    {Head1, SegSz, N, SegAddr};
alloc_hole(_MaxSz, Head, _SegSz) ->
    {Head, 0, 0, 0}.

free_hole(Head, _Size, 0, _Addr) ->
    Head;
free_hole(Head, Size, N, Addr) ->
    Head1 = dets_utils:free(Head, Addr, adjsz(Size)),
    free_hole(Head1, Size, N-1, Addr+Size+1).

%% One (temporary) file for each buddy size, write all objects of that
%% size to the file.
allocate_all(Head, [{?FSCK_SEGMENT,_,FileName,Fd,_}], L) ->
    %% And one file for the segments...
    %% Note that space for the array parts and the segments has
    %% already been allocated.
    NoParts = no_parts(Head#head.m),
    %% All parts first, ensured by init_segments/5.
    Addr = ?BASE + NoParts * 4 * ?SEGPARTSZ,
    {Head, [{?FSCK_SEGMENT,Addr,FileName,Fd,0} | L]};
allocate_all(Head, [{LSize,_,FileName,Fd,NoCollections} | DTL], L) ->
    Size = ?POW(LSize-1),
    {_Head, Addr} = dets_utils:alloc(Head, adjsz(Size)),
    NewHead = dets_utils:alloc_many(Head, Size, NoCollections, Addr),
    E = {LSize,Addr,FileName,Fd,NoCollections},
    allocate_all(NewHead, DTL, [E | L]).

bin2term(Bin, 9) ->
    bin2term1(Bin, []);
bin2term(Bin, 8) ->
    bin2term_v8(Bin, []).

bin2term1([<<Slot:32, Seq:32, BinTerm/binary>> | BTs], L) ->
    bin2term1(BTs, [{Slot, Seq, binary_to_term(BinTerm)} | L]);
bin2term1([], L) ->
    lists:reverse(L).

bin2term_v8([<<Slot:32, BinTerm/binary>> | BTs], L) ->
    bin2term_v8(BTs, [{Slot, foo, binary_to_term(BinTerm)} | L]);
bin2term_v8([], L) ->
    lists:reverse(L).

write_all_sizes(Cache, FileT, Head) ->
    write_sizes(size(Cache), Cache, FileT, Head).

write_sizes(0, Cache, _FileT, Head) ->
    {Cache, Head};
write_sizes(Sz, Cache, FileT, Head) ->
    case ?VGET(Sz, Cache) of
	[_ | []] ->
	    write_sizes(Sz-1, Cache, FileT, Head);
	[[NoObjs | NoKeys] | L] ->
	    {FileName, Fd} = case ets:lookup(FileT, Sz) of
				 [] ->
				     temp_file(Head, FileT, Sz);
				 [{_,_,FN,F,_}] ->
				     {FN, F}
			     end,
	    ets:update_counter(FileT, Sz, {5,length(L)}),
	    NewHead = Head#head{no_objects = Head#head.no_objects+NoObjs,
				no_keys = Head#head.no_keys+NoKeys},
	    case file:write(Fd, lists:reverse(L)) of
		ok ->
		    NCache = ?VSET(Sz, Cache, [[0 | 0] | []]),
		    write_sizes(Sz-1, NCache, FileT, NewHead);
		Error ->
		    dets_utils:file_error(FileName, Error)
	    end
    end.

output_slots(E, [E1 | Es], Acc, ZT, Head, Cache) 
                       when element(1, E) == element(1, E1) ->
    output_slots(E1, Es, [E1 | Acc], ZT, Head, Cache);
output_slots(_E, [E | L], Acc, ZT, Head, Cache) ->
    NCache = output_slot(Acc, ZT, Head, Cache),
    output_slots(E, L, [E], ZT, Head, NCache);
output_slots(E, [], Acc, _ZT, __Head, Cache) ->
    {E, Acc, Cache}.

output_slot(Es, ZT, Head, Cache) ->
    Kp = Head#head.keypos,
    Fun = fun({_Slot, Seq, T}) -> 
		  Key = element(Kp, T),
		  {Key, {Seq, {insert,T}}}
	  end,
    Slot = element(1, hd(Es)),
    WLs0 = lists:map(Fun, Es),
    WLs = sofs:to_external(sofs:relation_to_family(sofs:relation(WLs0, 2))),
    {[], Bins, Size, No, KNo, _} = 
	eval_slot(WLs, [], Head#head.type, [], [], 0, 0, 0, false),

    %% First the object collection.
    BSize = Size + ?OHDSZ,
    LSize = sz2pos(BSize),
    Size2 = ?POW(LSize-1),
    Z = dets_utils:get_zeros(Size2-BSize, ZT),
    BinObject = [<<BSize:32, ?ACTIVE:32>>, Bins | Z],
    Cache1 = 
	if
	    LSize > size(Cache) ->
                C1 = ?VEXT(LSize, Cache, [[0 | 0] | []]),
		?VSET(LSize, C1, [[No | KNo] | [BinObject]]);
	    true ->
		[[NoObjs | NoKeys] | L] = ?VGET(LSize, Cache),
		E = [[NoObjs+No | NoKeys + KNo] | [BinObject | L]],
		?VSET(LSize, Cache, E)
	end,

    %% Then the pointer to the object collection.
    %% Cannot yet determine the absolute pointers; segment_file/4 does that.
    {Pos, _} = slot_position(Slot),
    PBin = <<Pos:32,BSize:32,LSize:8>>,
    [Foo | PL] = ?VGET(?FSCK_SEGMENT, Cache1),
    ?VSET(?FSCK_SEGMENT, Cache1, [Foo | [PBin | PL]]).

segment_file(FileT, Head, FileData, ZT, SegEnd) ->
    I = 2,
    true = ets:delete_all_objects(FileT),    
    lists:foreach(fun(X) -> true = ets:insert(FileT, X) end, FileData),
    {OutFile, Out} = temp_file(Head, FileT, I),
    [{?FSCK_SEGMENT,SegAddr,InFile,In,0} | FileData1] = FileData,
    {ok, 0} = dets_utils:position(In, InFile, bof),
    ok = seg_file(SegAddr, In, InFile, Out, OutFile, FileT, ZT, SegEnd),
    %% Restore the positions.
    true = ets:delete_all_objects(FileT),
    %% To get the segments copied first by dets:fsck_copy/4, use a big
    %% number here, FSCK_SEGMENT2.
    lists:foreach(fun(X) -> true = ets:insert(FileT, X) end, 
		  [{?FSCK_SEGMENT2,SegAddr,OutFile,Out,0} | FileData1]),
    file:close(In),
    file:delete(InFile),
    ok.
    
seg_file(Addr, In, InFile, Out, OutFile, FileT, ZT, SegEnd) ->
    case dets_utils:read_n(In, 4500) of
	eof ->
	    FinalZ = SegEnd - Addr,
      	    PZ = dets_utils:get_zeros(FinalZ, ZT),
	    dets_utils:fwrite(Out, OutFile, PZ);
	Bin ->
	    {NewAddr, L} = seg_file(Bin, Addr, FileT, ZT, []),
	    ok = dets_utils:fwrite(Out, OutFile, lists:reverse(L)),
	    seg_file(NewAddr, In, InFile, Out, OutFile, FileT, ZT, SegEnd)
    end.

seg_file(<<SlotPos:32,BSize:32,LSize:8,T/binary>>, Addr, FileT, ZT, L) ->
    NoZeros = SlotPos - Addr,
    PZ = dets_utils:get_zeros(NoZeros, ZT),
    PSize = NoZeros+?SZOBJP*4,
    Inc = ?POW(LSize-1),
    CollP = ets:update_counter(FileT, LSize, Inc) - Inc,
    PointerBin = [PZ | <<BSize:32, CollP:32>>],
    seg_file(T, Addr + PSize, FileT, ZT, [PointerBin | L]);
seg_file(Bin, Addr, _FileT, _ZT, L) when size(Bin) == 0 ->
    {Addr, L}.

temp_file(Head, FileT, N) ->
    TmpName = lists:concat([Head#head.filename, '.', N]),
    {ok, Fd} = dets_utils:open(TmpName, [raw, binary, write, read]),
    %% The file table is consulted when cleaning up.
    true = ets:insert(FileT, {N,0,TmpName,Fd,0}),
    {TmpName, Fd}.

fsck_objs(Bin = <<Sz:32, Status:32, Tail/binary>>, Kp, Head, L, Seq) ->
    if 
	Status == ?ACTIVE ->
	    Sz1 = Sz-?OHDSZ,
	    case Tail of
		<<BinTerm:Sz1/binary, Tail2/binary>> ->
		    case catch bin2keybins(BinTerm, Head) of
			{'EXIT', _Reason} ->
			    %% The whole collection of objects is skipped.
			    skip_bytes(Bin, ?BUMP, Kp, Head, L, Seq);
			BOs ->
			    {NL, NSeq} = make_objects(BOs, Seq, Kp, Head, L),
			    Skip = ?POW(sz2pos(Sz)-1) - Sz,
			    skip_bytes(Tail2, Skip, Kp, Head, NL, NSeq)
		    end;
		_ ->
                    {more, Bin, Sz, L, Seq}
	    end;
	true -> 
	    skip_bytes(Bin, ?BUMP, Kp, Head, L, Seq)
    end;
fsck_objs(Bin, _Kp, _Head, L, Seq) ->
    {more, Bin, 0, L, Seq}.
    
make_objects([{K,BT}|Os], Seq, Kp, Head, L) when Head#head.version == 8 ->
    LogSz = dets_v8:sz2pos(size(BT)+?OHDSZ_v8),
    Slot = dets_v8:db_hash(K, Head),
    Obj = [LogSz | <<Slot:32, LogSz:8, BT/binary>>],
    make_objects(Os, Seq, Kp, Head, [Obj | L]);
make_objects([{K,BT} | Os], Seq, Kp, Head, L) ->
    Obj = make_object(Head, K, Seq, BT),
    make_objects(Os, Seq+1, Kp, Head, [Obj | L]);
make_objects([], Seq, _Kp, _Head, L) ->
    {L, Seq}.

%% Inlined.
make_object(Head, Key, Seq, BT) ->
    LogSz = sz2pos(size(BT)+?OHDSZ),
    Slot = db_hash(Key, Head),
    [LogSz | <<Slot:32, Seq:32, BT/binary>>].

%% Inlined.
skip_bytes(Bin, Skip, Kp, Head, L, Seq) ->
    case Bin of
	<<_:Skip/binary, Tail/binary>> ->
	    fsck_objs(Tail, Kp, Head, L, Seq);
	_ ->
            {new, Skip - size(Bin), L, Seq}
    end.

%%%
%%% End of repair and conversion of a dets file.
%%%

%% -> {NewHead, ok} | throw({Head, Error})
do_perform_save(H) ->
    ?DEBUGF("head.m = ~p~n", [H#head.m]),
    ?DEBUGF("head.no_objects = ~p~n", [H#head.no_objects]),
    ?DEBUGF("head.no_keys = ~p~n", [H#head.no_keys]),

    {ok, Pos} = dets_utils:position(H, eof),
    free_lists_to_file(H),
    W1 = {?FREELIST_POS, <<Pos:32>>},
    
    W2 = {?D_POS, <<(H#head.m):32, 
	            (H#head.next):32, 
	            (H#head.keypos):32,
	            (H#head.no_objects):32,
                    (H#head.no_keys):32,
	            (H#head.min_no_slots):32,
	            (H#head.max_no_slots):32,
	            (H#head.maxobjsize):32,
		    (H#head.n):32>>},
    W3 = {?CLOSED_PROPERLY_POS, <<?CLOSED_PROPERLY:32>>},
    W4 = {?FILE_FORMAT_VERSION_POS, <<?FILE_FORMAT_VERSION:32>>},
    {H2, ok} = dets_utils:pwrite(H, [W1,W2,W3,W4]),
    {ok, Pos2} = dets_utils:position(H2, eof),
    ?DEBUGF("Writing file size ~p, eof at ~p~n", [Pos2+4, Pos2]),
    {H3, ok} = dets_utils:pwrite(H2, [{Pos2, <<(Pos2 + 4):32>>}]),
    ok = ensure_written(H3),
    {H3#head{update_mode = saved, freelists_p = Pos}, ok}.

ensure_written(H) when H#head.ram_file == true ->
    {ok, EOF} = dets_utils:position(H, eof),
    {ok, Bin} = dets_utils:pread(H, 0, EOF, 0),
    case file:write_file(H#head.filename, Bin) of
	ok ->
	    ok;
	{error, Reason} ->
	    FError = {error, {file_error, H#head.filename, Reason}},
	    throw({H, FError})
    end;
ensure_written(H) when H#head.ram_file == false ->
    file:sync(H#head.fptr).
        
%% Going through some trouble avoiding creating one single binary for
%% the free lists. If the free lists are huge, binary_to_term and
%% term_to_binary could otherwise stop the emulator for quite some time.

-define(MAXFREEOBJ, 4096).
-define(ENDFREE, 12345).

free_lists_to_file(H) ->
    FL = dets_utils:get_freelists(H),
    free_list_to_file(FL, H, 1, size(FL)),
    ok = dets_utils:write(H, <<12:32, ?FREE:32, ?ENDFREE:32>>).

free_list_to_file(_Ftab, _H, Pos, Sz) when Pos > Sz ->
    ok;
free_list_to_file(Ftab, H, Pos, Sz) ->
    Max = (?MAXFREEOBJ - 4 - ?OHDSZ) div 4,
    F = fun(N, L) when N == 0 -> {N, L};
	   (N, L) ->
		{L1, N1, NN, NL} =
		    if 
			N > Max ->
			    {lists:sublist(L, Max), Max,
			     N-Max, lists:nthtail(Max, L)};
			true ->
			    {L, N, 0, []}
		    end,
                Size = N1*4 + 4 + ?OHDSZ,
		Header = <<Size:32, ?FREE:32, Pos:32>>,
		ok = dets_utils:write(H, [Header | L1]),
		{NN, NL}
	end,
    dets_utils:tree_to_bin(element(Pos, Ftab), F, Max),
    free_list_to_file(Ftab, H, Pos+1, Sz).

free_lists_from_file(H, Pos) ->
    dets_utils:position(H#head.fptr, H#head.filename, Pos),
    FL = dets_utils:empty_free_lists(),
    case catch bin_to_tree(H, start, FL, -1, []) of
	{'EXIT', _} ->
	    throw({error, {bad_freelists, H#head.filename}});
	Reply ->
	    Reply
    end.

bin_to_tree(H, LastPos, Ftab, A0, L) ->
    Fd = H#head.fptr,
    case dets_utils:read_n(Fd, ?MAXFREEOBJ) of
	<<Size:32,?FREE:32,Pos:32,T/binary>> when Pos =/= ?ENDFREE ->
	    {NFtab, L1, A1} = 
		if
		    Pos =/= LastPos, LastPos =/= start ->
			Tree = dets_utils:list_to_tree(L),
			{setelement(LastPos, Ftab, Tree), [], -1};
		    true ->
			{Ftab, L, A0}
		    end,
	    {NL, Len, A2} = bin_to_tree1(T, Size-?OHDSZ-4, A1, L1),
	    if 
		Len > 0 ->
		    dets_utils:position(Fd, H#head.filename, {cur,-Len});
		true ->
		    ok
	    end,
	    bin_to_tree(H, Pos, NFtab, A2, NL);
        <<Size:32,?FREE:32,?ENDFREE:32,_/binary>> when L == [] ->
            Ftab;
        <<Size:32,?FREE:32,?ENDFREE:32,_/binary>> ->
            setelement(LastPos, Ftab, dets_utils:list_to_tree(L))
    end.

bin_to_tree1(<<A1:32,A2:32,A3:32,A4:32,T/binary>>, Size, A, L) 
         when Size >= 16, A < A1, A1 < A2, A2 < A3, A3 < A4 ->
    bin_to_tree1(T, Size-16, A4, [A4, A3, A2, A1 | L]);
bin_to_tree1(<<A1:32,T/binary>>, Size, A, L) when Size >= 4, A < A1 ->
    bin_to_tree1(T, Size - 4, A1, [A1 | L]);
bin_to_tree1(B, 0, A, L) ->
    {L, size(B), A}.

%% -> [term()] | throw({Head, Error})
slot_objs(H, Slot) when Slot >= H#head.next ->
    '$end_of_table';
slot_objs(H, Slot) ->
    {ok, _Pointer, Objects} = slot_objects(H, Slot),
    Objects.

db_hash(Key, Head) ->
    H = h(Key, Head#head.hash_bif),
    Hash = H rem Head#head.m,
    if
	Hash < Head#head.n ->
	    H rem (2 * Head#head.m);
	true ->
	    Hash
    end.

no_slots(Head) ->
    {Head#head.min_no_slots, Head#head.next, Head#head.max_no_slots}.

h(I, HF) -> erlang:HF(I, ?BIG) - 1.  %% stupid BIF has 1 counts.

%% Allow quite a lot when reading object collections.
-define(MAXCOLL, (10 * ?CHUNK_SIZE)).

%% Re-hashing a segment, starting with SlotStart.
%%
%% On the average, half of the keys of the slot are put in a new slot.
%% If the old slot is i, then the new slot is i+m. The new slots
%% reside in a newly allocated segment.
%%
%% -> {NewHead, ok} | throw({Head, Error})
re_hash(Head, SlotStart) ->
    {FromSlotPos, _} = slot_position(SlotStart),
    {ToSlotPos, _} = slot_position(SlotStart + Head#head.m),
    RSpec = [{FromSlotPos, 4 * ?SEGSZ}],
    {ok, [FromBin]} = dets_utils:pread(RSpec, Head),
    split_bins(FromBin, Head, FromSlotPos, ToSlotPos, [], [], 0).

split_bins(FB, Head, _Pos1, _Pos2, _ToRead, _L, 0) when size(FB) == 0 ->
    {Head, ok};
split_bins(FB, Head, Pos1, Pos2, ToRead, L, _SoFar) when size(FB) == 0 ->
    re_hash_write(Head, ToRead, L, Pos1, Pos2);
split_bins(FB, Head, Pos1, Pos2, ToRead, L, SoFar) ->
    <<Sz1:32, P1:32, FT/binary>> = FB,
    <<B1:?OHDSZ/binary, _/binary>> = FB,
    NSoFar = SoFar + Sz1,
    NPos1 = Pos1 + ?SZOBJP*4,
    NPos2 = Pos2 + ?SZOBJP*4,
    if
	NSoFar > ?MAXCOLL, ToRead =/= [] ->
	    {NewHead, ok} = re_hash_write(Head, ToRead, L, Pos1, Pos2),
	    split_bins(FB, NewHead, Pos1, Pos2, [], [], 0);
	Sz1 == 0 ->
	    E = {skip,B1},
	    split_bins(FT, Head, NPos1, NPos2, ToRead, [E | L], NSoFar);
        true ->
	    E = {Sz1,P1,B1,Pos1,Pos2},
	    NewToRead = [{P1,Sz1} | ToRead],
	    split_bins(FT, Head, NPos1, NPos2, NewToRead, [E | L], NSoFar)
    end.

re_hash_write(Head, ToRead, L, Pos1, Pos2) ->
    {ok, Bins} = dets_utils:pread(ToRead, Head),
    Z = <<0:32, 0:32>>,
    {Head1, BinFS, BinTS, WsB} = re_hash_slots(Bins, L, Head, Z, [],[],[]),
    WPos1 = Pos1 - ?SZOBJP*4*length(L),
    WPos2 = Pos2 - ?SZOBJP*4*length(L),
    ToWrite = [{WPos1,BinFS}, {WPos2, BinTS} | WsB],
    dets_utils:pwrite(Head1, ToWrite).

re_hash_slots(Bins, [{skip,B1} | L], Head, Z, BinFS, BinTS, WsB) ->
    re_hash_slots(Bins, L, Head, Z, [B1 | BinFS], [Z | BinTS], WsB);
re_hash_slots([FB | Bins], [E | L], Head, Z, BinFS, BinTS, WsB) ->
    {Sz1,P1,B1,Pos1,Pos2} = E,
    KeyObjs = case catch per_key(Head, FB) of
		  {'EXIT', _} ->
		      throw(dets_utils:corrupt_reason(Head, bad_object));
		  Else ->
		      Else
	      end,
    case re_hash_split(KeyObjs, Head, [], 0, [], 0) of
	{_, _, [], 0} ->
	    re_hash_slots(Bins, L, Head, Z, [B1 | BinFS], [Z | BinTS], WsB);
	{[], 0, _ML, _MSz} -> %% Optimization.
	    re_hash_slots(Bins, L, Head, Z, [Z | BinFS], [B1 | BinTS], WsB);
	{KL, KSz, ML, MSz} ->
	    {Head1, FS1, Ws1} = updated(Head, P1, Sz1, KSz, Pos1, KL, true),
	    {NewHead, TS2, Ws2} = updated(Head1, 0, 0, MSz, Pos2, ML, true),
	    NewBinFS = case FS1 of
			   [{Pos1,Bin1}] -> [Bin1 | BinFS];
			   [] -> BinFS
		       end,
	    NewBinTS = case TS2 of
			   [{Pos2,Bin2}] -> [Bin2 | BinTS];
			   [] -> BinTS
		       end,
	    NewWsB = Ws2 ++ Ws1 ++ WsB,
	    re_hash_slots(Bins, L, NewHead, Z, NewBinFS, NewBinTS, NewWsB)
    end;
re_hash_slots([], [], Head, _Z, BinFS, BinTS, WsB) ->
    {Head, BinFS, BinTS, lists:reverse(WsB)}.

re_hash_split([E | KeyObjs], Head, KL, KSz, ML, MSz) ->
    {Key,Sz,Bin,_Item,_Objs} = E,
    New = h(Key, Head#head.hash_bif) rem (2 * Head#head.m),
    if
	New >= Head#head.m ->
	    re_hash_split(KeyObjs, Head, KL, KSz, [Bin | ML], MSz + Sz);
	true ->
	    re_hash_split(KeyObjs, Head, [Bin | KL], KSz + Sz, ML, MSz)
    end;
re_hash_split([], _Head, KL, KSz, ML, MSz) ->
    {lists:reverse(KL), KSz, lists:reverse(ML), MSz}.

zero(Fd, Fname, I) ->
    dets_utils:fwrite(Fd, Fname, make_zeros(4*I)).

make_zeros(0) -> [];
make_zeros(N) when N rem 2 == 0 ->
    P = make_zeros(N div 2),
    [P|P];
make_zeros(N) ->
    P = make_zeros(N div 2),
    [0,P|P].

%% -> {Head, [LookedUpObject]} | throw({Head, Error})
%% Lookup of keys bypassing the cache.
direct_lookup(Head, Ks) ->
    PK = lists:map(fun(K) -> {K,[{1,lookup}]} end, Ks),
    eval_work_list(Head, PK).

%% -> {NewHead, [LookedUpObject]} | throw({NewHead, Error})
write_cache(Head) ->
    C = Head#head.cache,
    {NewC, MaxInserts, PerKey} = dets_utils:reset_cache(C),
    %% MaxNoInsertedKeys is an upper limit on the number of new keys.
    MaxNoInsertedKeys = lists:min([MaxInserts, length(PerKey)]),
    Head1 = Head#head{cache = NewC},
    Return = {_NewHead, Reply}  = 
        case may_grow(Head1, MaxNoInsertedKeys, once) of
            {Head2, ok} ->
                eval_work_list(Head2, PerKey);
            HeadError ->
                throw(HeadError)
        end,
    case Reply of
        _ when list(Reply) ->
            Return;
        _ ->
            throw(Return)
    end.

%% -> {NewHead, ok} | {NewHead, Error}
may_grow(Head, _N, _How) when Head#head.fixed =/= false ->
    {Head, ok};
may_grow(Head, _N, _How) when Head#head.next >= Head#head.max_no_slots ->
    {Head, ok};
may_grow(Head, N, How) ->
    Extra = lists:min([2*?SEGSZP, Head#head.no_keys + N - Head#head.next]),
    case catch may_grow1(Head, Extra, How) of
	{error, Reason} -> % alloc may throw error
	    {Head, {error, Reason}};
	Reply ->
	    Reply
    end.

may_grow1(Head, Extra, many_times) when Extra > ?SEGSZP ->
    Reply = grow(Head, 1, undefined),
    case Reply of
	{_, ok} -> self() ! {self(), may_grow};
	_ -> ok
    end,
    Reply;
may_grow1(Head, Extra, _How) ->    
    grow(Head, Extra, undefined).

%% -> {Head, ok} | throw({Head, Error})
grow(Head, Extra, _SegZero) when Extra =< 0 ->
    {Head, ok};
grow(Head, Extra, undefined) ->
    grow(Head, Extra, seg_zero());
grow(Head, Extra, SegZero) ->
    #head{n = N, next = Next, m = M} = Head,
    SegNum = Next div ?SEGSZP,    
    {Head0, Ws1} = allocate_segment(Head, SegZero, SegNum),
    %% re_hash/2 will overwrite the segment, but initialize it anyway...
    {Head1, ok} = dets_utils:pwrite(Head0, Ws1),
    %% If re_hash fails, segp_cache has been called, but it does not matter.
    {Head2, ok} = re_hash(Head1, N),
    NewHead =
	if 
	    N + ?SEGSZP == M ->
		Head2#head{n = 0, next = Next + ?SEGSZP, m = 2 * M};
	    true ->
		Head2#head{n = N + ?SEGSZP, next = Next + ?SEGSZP}
	end,
    grow(NewHead, Extra - ?SEGSZP, SegZero).

seg_zero() ->
    make_zeros(4 * ?SEGSZ).

find_object(Head, Object) ->
    Key = element(Head#head.keypos, Object),
    Slot = db_hash(Key, Head),
    find_object(Head, Object, Slot).    

find_object(H, _Obj, Slot) when Slot >= H#head.next ->
    false;
find_object(H, Obj, Slot) ->
    case catch slot_objects(H, Slot) of
	{ok, Pointer, Objects} ->
	    case lists:member(Obj, Objects) of
		true -> {ok, Pointer};
		false -> false
	    end;
	_ -> false
    end.

%% -> {ok, BucketP, Objects} | throw({Head, Error})
slot_objects(Head, Slot) ->
    {SlotPos, _Size} = slot_position(Slot),    
    case dets_utils:ipread(Head#head.fptr, SlotPos, infinity) of 
	{ok, {BucketSz, Pointer, <<BucketSz:32, _St:32, KeysObjs/binary>>}} ->
	    case catch bin2objs(KeysObjs, Head#head.type) of
		{'EXIT', _} ->
		    throw(dets_utils:corrupt_reason(Head, bad_object));
		Objs ->
		    {ok, Pointer, Objs}
	    end;
        [] ->
	    {ok, 0, []};
	_Bad -> % eof or bad badly formed binary
	    throw(dets_utils:corrupt_reason(Head, bad_object))
    end.

%%%
%%% Cache routines depending on the dets file format.
%%%

%% -> {Head, [LookedUpObject]} | throw({Head, Error})
eval_work_list(Head, [{Key,[{_Seq,lookup}]}] = WL) ->
    {SlotPos, _} = slot_position(db_hash(Key, Head)),
    case dets_utils:ipread(Head#head.fptr, SlotPos, infinity) of 
	{ok, {_BucketSz, Pointer, Bin}} ->
	    case catch eval_buckets([Bin], [{SlotPos,Pointer,WL}], 
				    Head, [], [], 0, 0) of
		{ok, NewHead, LU, [], 0, 0} ->
		    {NewHead, lists:reverse(LU)};
		_Error ->
		    throw(dets_utils:corrupt_reason(Head, bad_object))
	    end;
	[] ->
	    {Head, []};
	_Bad -> % eof or bad badly formed binary
	    throw(dets_utils:corrupt_reason(Head, bad_object))
    end;
eval_work_list(Head, PerKey) ->
    SWLs = tag_with_slot(PerKey, Head, []),
    P1 = sofs:to_external(sofs:relation_to_family(sofs:relation(SWLs, 2))),
    {PerSlot, SlotPositions} = remove_slot_tag(P1, [], []),
    {ok, Bins} = dets_utils:pread(SlotPositions, Head),
    read_buckets(PerSlot, SlotPositions, Bins, Head, [], [], [], [], 0, 0, 0).

tag_with_slot([{K,_} = WL | WLs], Head, L) ->
    tag_with_slot(WLs, Head, [{db_hash(K, Head), WL} | L]);
tag_with_slot([], _Head, L) ->
    L.

remove_slot_tag([{S,SWLs} | SSWLs], Ls, SPs) ->
    remove_slot_tag(SSWLs, [SWLs | Ls], [slot_position(S) | SPs]);
remove_slot_tag([], Ls, SPs) ->
    {Ls, SPs}.

read_buckets([WLs | SPs], [{P1,_8} | Ss], [<<_Zero:32,P2:32>> | Bs], Head,
	      PWLs, ToRead, LU, Ws, NoObjs, NoKeys, SoFar) when P2 == 0 ->
    {NewHead, NLU, NWs, No, KNo} = 
	eval_bucket_keys(WLs, P1, 0, 0, [], Head, Ws, LU),
    NewNoObjs = No + NoObjs,
    NewNoKeys = KNo + NoKeys,
    read_buckets(SPs, Ss, Bs, NewHead, PWLs, ToRead, NLU, NWs, 
		 NewNoObjs, NewNoKeys, SoFar);
read_buckets([WorkLists| SPs], [{P1,_8} | Ss], [<<Size:32,P2:32>> | Bs], Head,
	     PWLs, ToRead, LU, Ws, NoObjs, NoKeys, SoFar) 
                                 when SoFar + Size < ?MAXCOLL; ToRead == [] ->
    NewToRead = [{P2, Size} | ToRead],
    NewPWLs = [{P1,P2,WorkLists} | PWLs],
    NewSoFar = SoFar + Size,
    read_buckets(SPs, Ss, Bs, Head, NewPWLs, NewToRead, LU, Ws, 
		 NoObjs, NoKeys, NewSoFar);
read_buckets(SPs, Ss, Bs, Head, PWLs, ToRead, LU, Ws, NoObjs, NoKeys, SoFar) 
                                                             when SoFar > 0 ->
    {ok, Bins} = dets_utils:pread(ToRead, Head),
    case catch eval_buckets(Bins, PWLs, Head, LU, Ws, 0, 0) of
	{ok, NewHead, NLU, [], 0, 0} -> 
            read_buckets(SPs, Ss, Bs, NewHead, [], [], NLU, [], 
			 NoObjs, NoKeys, 0);
	{ok, Head1, NLU, NWs, No, KNo} -> 
	    NewNoObjs = NoObjs + No,
	    NewNoKeys = NoKeys + KNo,
            {NewHead, ok} = dets_utils:pwrite(Head1, lists:reverse(NWs)),
            read_buckets(SPs, Ss, Bs, NewHead, [], [], NLU, [], 
			 NewNoObjs, NewNoKeys, 0);
	_Error  -> 
	    throw(dets_utils:corrupt_reason(Head, bad_object))
    end;
read_buckets([], [], [], Head, [], [], LU, Ws, NoObjs, NoKeys, 0) ->
    {Head1, NWs} = update_no_keys(Head, Ws, NoObjs, NoKeys),
    {NewHead, ok} = dets_utils:pwrite(Head1, lists:reverse(NWs)),
    {NewHead, lists:reverse(LU)}.

eval_buckets([Bin | Bins], [SP | SPs], Head, LU, Ws, NoObjs, NoKeys) ->
    {P1, Pos, WLs} = SP,
    KeyObjs = per_key(Head, Bin),
    {NewHead, NLU, NWs, No, KNo} = 
	eval_bucket_keys(WLs, P1, Pos, size(Bin), KeyObjs, Head, Ws, LU),
    eval_buckets(Bins, SPs, NewHead, NLU, NWs, NoObjs + No, NoKeys + KNo);
eval_buckets([], [], Head, LU, Ws, NoObjs, NoKeys) ->
    {ok, Head, LU, Ws, NoObjs, NoKeys}.

eval_bucket_keys(WLs, SlotPos, Pos, OldSize, KeyObjs, Head, Ws, LU) ->
    {NLU, Bins, BSize, No, KNo, Ch} = 
         eval_slot(WLs, KeyObjs, Head#head.type, LU, [], 0, 0, 0, false),
    {NewHead, W1, W2} = updated(Head, Pos, OldSize, BSize, SlotPos, Bins, Ch),
    {NewHead, NLU, W1++W2++Ws, No, KNo}.

updated(Head, Pos, OldSize, BSize, SlotPos, Bins, Ch) ->
    BinsSize = BSize + ?OHDSZ,
    if 
	Pos == 0, BSize == 0 ->
	    {Head, [], []};
	Pos == 0, BSize > 0 ->
	    {NewHead, NewPos} = dets_utils:alloc(Head, adjsz(BinsSize)),
	    W1 = {NewPos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
	    W2 = {SlotPos, <<BinsSize:32, NewPos:32>>},
	    {NewHead, [W2], [W1]};
	Pos =/= 0, BSize == 0 ->
	    NewHead = dets_utils:free(Head, Pos, adjsz(OldSize)),
	    W1 = {Pos+?STATUS_POS, <<?FREE:32>>},
	    W2 = {SlotPos, <<0:32, 0:32>>},
	    {NewHead, [W2], [W1]};
	Pos =/= 0, BSize > 0, Ch == false ->
	    {Head, [], []};
	Pos =/= 0, BSize > 0 ->
	    %% Doubtful. The scan function has to be very careful
	    %% since partly scanned objects may be overwritten.
	    Overwrite = if
			    OldSize == BinsSize -> same;
			    true -> sz2pos(OldSize) == sz2pos(BinsSize)
			end,
	    if 
		Overwrite == same ->
		    W1 = {Pos+?OHDSZ, Bins},
		    {Head, [], [W1]};
		Overwrite == true ->
		    W1 = {Pos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
		    %% Pos is already there, but return {SlotPos, <8 bytes>}.
		    W2 = {SlotPos, <<BinsSize:32, Pos:32>>},
		    {Head, [W2], [W1]};
		true ->
		    Head1 = dets_utils:free(Head, Pos, adjsz(OldSize)),
		    {NewHead, NewPos} = 
			dets_utils:alloc(Head1, adjsz(BinsSize)),
		    W0 = {NewPos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
		    W2 = {SlotPos, <<BinsSize:32, NewPos:32>>},
		    W1 = if 
			      Pos =/= NewPos ->
                                  %% W0 first.
				  [W0, {Pos+?STATUS_POS, <<?FREE:32>>}];
			      true -> 
				  [W0]
			  end,
		    {NewHead, [W2], W1}
	    end
    end.

eval_slot([{Key,Commands} | WLs] = WLs0, KOs, Type, LU, Ws, No, KNo,BSz, Ch) ->
    case KOs of
        [{K,KS,KB,O,Os} | KOs1] when K == Key ->
            Ts = lists:sort([O | binobjs2terms(Os)]),
            {NLU, NWs, Sz, No1, KNo1, NCh} = 
		eval_key(Commands, Ts, Type, KB, KS, LU, Ws, Ch),
            eval_slot(WLs, KOs1, Type, NLU, NWs, No1 + No, 
		      KNo1 + KNo, Sz + BSz, NCh);
        [{K,Size,KeyBin,_,_} | KOs1] when K < Key ->
            eval_slot(WLs0, KOs1, Type, LU, [Ws | KeyBin], No, 
		      KNo, Size + BSz, Ch);
        _ ->
            {NLU, NWs, Sz, No1, KNo1, NCh} = 
		eval_key(Commands, [], Type, [], 0, LU, Ws, Ch),
            eval_slot(WLs, KOs, Type, NLU, NWs, No1 + No, 
		      KNo1 + KNo, Sz + BSz, NCh)
    end;
eval_slot([], [{_Key,Size,KeyBin,_,_} | KOs], Type, LU, Ws, No, KNo,BSz, Ch) ->
    eval_slot([], KOs, Type, LU, [Ws | KeyBin], No, KNo, Size + BSz, Ch);
eval_slot([], [], _Type, LU, Ws, No, KNo, BSz, Ch) ->
    {LU, Ws, BSz, No, KNo, Ch}.

eval_key(Comms, Old, Type, KeyBin, KeySz, LU, Ws, Ch) ->
    LookupOnly = case Comms of
		     [{_Seq,lookup}] -> true;
		     _ -> false
		 end,
    case eval_key1(Comms, [], Old, Type, LU, Ws, skip, 0, Old, LookupOnly) of
	{ok, NLU} when Old == [] ->
	    {NLU, Ws, 0, 0, 0, Ch};
	{ok, NLU} ->
	    {NLU, [Ws | KeyBin], KeySz, 0, 0, Ch};
	{NLU, NWs, NSz, No} when Old == [], NSz > 0 ->
	    {NLU, NWs, NSz, No, 1, true};
	{NLU, NWs, NSz, No} when Old =/= [], NSz == 0 ->
	    {NLU, NWs, NSz, No, -1, true};
	{NLU, NWs, NSz, No} ->
	    {NLU, NWs, NSz, No, 0, true}
    end.

%% First find 'delete_key' and 'lookup' commands, and handle the 'set' type...
eval_key1([{_Seq,lookup} | L], Cs, Old, Type, LU, Ws, _, No, Orig, LF) ->
    eval_key1(L, Cs, Old, Type, LU, Ws, lookup, No, Orig, LF);
eval_key1([{_Seq,delete_key} | L], _Cs, Old, Type, LU, Ws, Look, No,Orig,LF) ->
    NewNo = No - length(Old),
    eval_key1(L, [], [], Type, LU, Ws, Look, NewNo, Orig, LF);
eval_key1([{_Seq,{insert,Term}} | L], Cs, [{Term,_,_}] = Old, Type, 
	  LU, Ws, Look, No, Orig, LF) when Type == set ->
    eval_key1(L, Cs, Old, Type, LU, Ws, Look, No, Orig, LF);
eval_key1([{Seq,{insert,Term}} | L], Cs, Old, Type, LU, Ws, Look, No, Orig,LF) 
                           when Type == set ->
    NNo = No + 1 - length(Old),
    eval_key1(L, Cs, [{Term,Seq,insert}], Type, LU, Ws, Look, NNo, Orig, LF);
eval_key1([{_Seq,{delete_object,Term}} | L], Cs, [{Term,_,_}], Type, 
	  LU, Ws, Look, No, Orig, LF) when Type == set ->
    eval_key1(L, Cs, [], Type, LU, Ws, Look, No-1, Orig, LF);
eval_key1([{_Seq,{delete_object,_T}}| L], Cs, New, Type, LU, 
	      Ws, Look, No, Orig, LF) when Type == set, New == [] ->
    eval_key1(L, Cs, New, Type, LU, Ws, Look, No, Orig, LF);
eval_key1([{Seq,{Comm,Term}} | L], Cs, Old, Type, LU, Ws, Look, No, Orig, LF) 
                                           when Type =/= set ->
    eval_key1(L, [{Term,Seq,Comm} | Cs], Old, Type, LU, Ws, Look, No, Orig,LF);
eval_key1([], Cs, Old, Type, LU, Ws, Lookup, No, Orig, LF) ->
    Commands = lists:sort(Cs),
    %% ... then do the work.
    {ok, L, NewNo} = 
	case Type of
	    set -> {ok, Old, No};
	    bag -> eval_bag(Commands, Old, [], No);
	    duplicate_bag -> eval_dupbag(Commands, Old, [], No)
	end,
    {New0, Sz} = key_final(L, [], 0),
    New = lists:sort(New0),
    NLU = do_lookup(Lookup, New, LU),
    NoChange = if
		   LF == true -> true; % only lookup
		   length(New) =/= length(Orig) -> false;
		   true -> 
		       Tmp = lists:map(fun({T,S,B}) -> {S,B,T} end, Orig),
		       same_terms(lists:sort(Tmp), New)
	       end,
    if 
        NoChange == true ->
	    %% The key's objects have not changed.
	    {ok, NLU};
	New0 == [] -> 
	    {NLU, Ws, 0, NewNo};
	true -> 
            Ws1 = get_bins(New),
	    if 
		Type == set ->
		    {NLU, [Ws | Ws1], Sz, NewNo};
		true -> 
		    NSz = Sz + 4,
		    {NLU, [Ws, <<NSz:32>> | Ws1], NSz, NewNo}
	    end
    end.

key_final([{Term,Seq,B} | L], SB, Sz) when binary(B) ->
    key_final(L, [{Seq,B,Term} | SB], Sz + size(B));
key_final([{Term,Seq,insert} | L], SB, Sz) ->
    B = term_to_binary(Term),
    BSize = size(B) + 4,
    key_final(L, [{Seq,[<<BSize:32>> | B], Term} | SB], Sz + BSize);
key_final([], SB, Sz) ->
    {SB, Sz}.

do_lookup(Lookup, [{_S, _BT, T} | L], LU) when Lookup == lookup ->
    do_lookup(Lookup, L, [T | LU]);
do_lookup(_Looup, _, LU) ->
    LU.

same_terms([E1 | L1], [E2 | L2]) when element(3, E1) == element(3, E2) ->
    same_terms(L1, L2);
same_terms(L1, L2) ->
    (L1 == []) and (L2 == []).

get_bins([{_S,BT,_T} | L]) ->
    [BT | get_bins(L)];
get_bins([]) ->
    [].

eval_bag([{Term1,_Seq1,delete_object} | L], Old, New, No) 
                       when Old == []; Term1 < element(1, hd(Old)) ->
    eval_bag(L, Old, New, No);
eval_bag([{Term,_Seq1,insert} = N | L], Old, New, No) 
                       when Old == []; Term < element(1, hd(Old)) ->
    bag_object(L, Old, New, No, [N], Term);
eval_bag(L, [{Term2,_S,_B} = O | Old], New, No) 
                       when L == []; element(1, hd(L)) > Term2 ->
    eval_bag(L, Old, [O | New], No);
eval_bag([{Term,_,insert} = N | L], [{Term,_,_} | Old], New, No) ->
    bag_object(L, Old, New, No-1, [N], Term);
eval_bag([{Term,_,delete_object} | L], [{Term,_,_} | Old], New, No) ->
    bag_object(L, Old, New, No-1, [], Term);
eval_bag([], [], New, No) ->
    {ok, New, No}.

bag_object([{Term,_,insert} = N | L], Old, New, No, _N, Term) ->
    bag_object(L, Old, New, No, [N], Term);
bag_object([{Term,_,delete_object} | L], Old, New, No, _N, Term) ->
    bag_object(L, Old, New, No, [], Term);
bag_object(L, Old, New, No, [], _Term) ->
    eval_bag(L, Old, New, No);
bag_object(L, Old, New, No, [N], _Term) ->
    eval_bag(L, Old, [N | New], No+1).

eval_dupbag([{Term1,_Seq1,delete_object} | L], Old, New, No) 
                       when Old == []; Term1 < element(1, hd(Old)) ->
    eval_dupbag(L, Old, New, No);
eval_dupbag([{Term,_Seq1,insert} = N | L], Old, New, No) 
                       when Old == []; Term < element(1, hd(Old)) ->
    dup_object(L, Old, New, No+1, Term, [N]);
eval_dupbag(L, [{Term2,_S,_B} = O | Old], New, No) 
                       when L == []; element(1, hd(L)) > Term2 ->
    eval_dupbag(L, Old, [O | New], No);
eval_dupbag([{Term,_,_} | _] = L, [{Term,_,_} = Obj | Old], New, No) ->
    old_dup_object(L, Old, New, No, Term, [Obj]);
eval_dupbag([], [], New, No) ->
    {ok, New, No}.
    
old_dup_object(L, [{Term,_,_} = Obj | Old], New, No, Term, N) ->
    old_dup_object(L, Old, New, No, Term, [Obj | N]);
old_dup_object(L, Old, New, No, Term, N) ->
    dup_object(L, Old, New, No, Term, N).

dup_object([{Term,_,insert} = Obj | L], Old, New, No, Term, Q) ->
    dup_object(L, Old, New, No+1, Term, [Obj | Q]);
dup_object([{Term,_Seq,delete_object} | L], Old, New, No, Term, Q) ->
    %% All objects are deleted.
    NewNo = No - length(Q),
    dup_object(L, Old, New, NewNo, Term, []);
dup_object(L, Old, New, No, _Term, Q) ->
    eval_dupbag(L, Old, Q ++ New, No).

%% Update no_keys on the file too, if the number of segments that
%% dets:fsck/6 uses for estimate has changed.
update_no_keys(Head, Ws, 0, 0) -> {Head, Ws};
update_no_keys(Head, Ws, DeltaObjects, DeltaKeys) ->
    NoKeys = Head#head.no_keys,
    NewNoKeys = NoKeys + DeltaKeys,
    NWs = 
	if 
	    NewNoKeys > Head#head.max_no_slots ->
		Ws;
	    NoKeys div ?SEGSZP == NewNoKeys div ?SEGSZP ->
		Ws;
	    true ->
		[{?NO_KEYS_POS, <<NewNoKeys:32>>} | Ws]
	end,
    NewNoObject = Head#head.no_objects + DeltaObjects,
    {Head#head{no_objects = NewNoObject, no_keys = NewNoKeys}, NWs}.

slot_position(S) ->
    SegNo = S div ?SEGSZP,
    PartPos = ?SEGARRADDR(SegNo div ?SEGPARTSZ),
    Part = get_arrpart(PartPos),
    Pos = ?SEGPARTADDR(Part, SegNo),
    Segment = get_segp(Pos),
    SegObjSize = 4 * ?SZOBJP,
    FinalPos = Segment + (SegObjSize * (S rem ?SEGSZP)),
    {FinalPos, SegObjSize}.

%% Inlined.
actual_seg_size() ->
    ?SEGSZ*4.

segp_cache(Pos, Segment) ->
    put(Pos, Segment).

%% Inlined.
get_segp(Pos) ->
    get(Pos).

arrpart_cache(Pos, ArrPart) ->
    put(Pos, ArrPart).

%% Inlined.
get_arrpart(Pos) ->
    get(Pos).

sz2pos(N) when N > 0 ->
    dets_utils:log(N-1, 16, 2). % adjsz/1 inlined...

%% Inlined. Compensates for the bug in dets_utils:sz2pos/1.
adjsz(N) ->
    N-1.

scan_objs(Bin, From, To, L, Ts, -1, _C) ->
    {stop, Bin, From, To, L, Ts};
scan_objs(B = <<Size:32, St:32, KO/binary>>, From, To, L, Ts, R, C) ->
    if 
	St == ?ACTIVE;
	St == ?FREE -> % deleted after scanning started
	    Size1 = Size-?OHDSZ,
	    case KO of
		<<KeysObjs:Size1/binary, T/binary>> ->
                    case catch to_objects(KeysObjs, C) of
                        {'EXIT', _} ->
                            bad_object;
                        Objs -> 
                            NTs = Objs ++ Ts,
			    Skip = ?POW(sz2pos(Size)-1) - Size,
                            F2 = From + Size,
                            NoObjs = length(Objs),
			    NR = if 
				     R < 0, R + NoObjs > -1 ->
					 -1;
                                     R < 0 ->
                                         R + NoObjs;
				     true ->
					 R + Size + Skip
				 end,
			    scan_skip(T, F2, To, Skip, L, NTs, NR, C)
		    end;
		_ ->
                    {more, B, From, To, L, Ts, R, Size}
	    end;
	true -> % a segment
	    scan_skip(B, From, To, actual_seg_size(), L, Ts, R, C)
    end;
scan_objs(B, From, To, L, Ts, R, _C) ->
    {more, B, From, To, L, Ts, R, 0}.

scan_skip(Bin, From, To, Skip, L, Ts, R, C) when From + Skip == To ->
    scan_next_allocated(Bin, From, L, Ts, R, C);
scan_skip(Bin, From, To, Skip, L, Ts, R, C) ->
    SkipPos = From + Skip,
    case Bin of
	<<_:Skip/binary, Tail/binary>> ->
	    scan_objs(Tail, SkipPos, To, L, Ts, R, C);
	_ ->
            {read, SkipPos, To, L, Ts, R}
    end.

scan_next_allocated(_Bin, _From, L, Ts, _R, _C) when size(L) == 0 ->
    {finished, Ts, L};
scan_next_allocated(Bin, From0, <<From:32, To:32, L/binary>>, Ts, R, C) ->
    Skip = From - From0,
    scan_skip(Bin, From0, To, Skip, L, Ts, R, C).

to_objects(Bin, {binary, _, Type}) ->
    bin2bins(Bin, Type);
to_objects(Bin, {_Repr, _, Type}) -> 
    bin2objs(Bin, Type).

%%%%%%%%%%%%%%%%%  DEBUG functions %%%%%%%%%%%%%%%%

file_info(FH) ->
    #fileheader{closed_properly = CP, keypos = Kp,
                m = M, next = Next, n = N, version = Version,
                type = Type, no_objects = NoObjects, no_keys = NoKeys} 
        = FH,
    if
        CP == 0 ->
            {error, not_closed};
        FH#fileheader.cookie /= ?MAGIC ->
            {error, not_a_dets_file};
        FH#fileheader.version /= ?FILE_FORMAT_VERSION ->
            {error, bad_version};
        true ->
            {ok, [{closed_properly,CP},{keypos,Kp},{m, M},{n,N},
		  {next,Next},{no_objects,NoObjects},{no_keys,NoKeys},
                  {type,Type},{version,Version}]}
    end.

v_segments(H) ->
    v_parts(H, 0, 0).

v_parts(_H, ?SEGARRSZ, _SegNo) ->
    done;
v_parts(H, PartNo, SegNo) ->
    Fd = H#head.fptr,
    PartPos = dets_utils:read_4(Fd, ?SEGARRADDR(PartNo)),
    if
	PartPos == 0 ->
	    done;
	true ->
	    PartBin = dets_utils:pread_n(Fd, PartPos, ?SEGPARTSZ*4),
	    v_segments(H, PartBin, PartNo+1, SegNo)
    end.

v_segments(H, B, PartNo, SegNo) when size(B) == 0 ->
    v_parts(H, PartNo, SegNo);
v_segments(_H, <<0:32,_/binary>>, _PartNo, _SegNo) ->
    done;
v_segments(H, <<Seg:32,T/binary>>, PartNo, SegNo) ->
    io:format("<~w>SEGMENT ~w~n", [Seg, SegNo]),
    v_segment(H, SegNo, Seg, 0),
    v_segments(H, T, PartNo, SegNo+1).

v_segment(_H, _, _SegPos, ?SEGSZP) ->
    done;
v_segment(H, SegNo, SegPos, SegSlot) ->
    Slot = SegSlot + (SegNo * ?SEGSZP),
    BucketP = SegPos + (4 * ?SZOBJP * SegSlot),
    case catch read_bucket(H#head.fptr, BucketP, H#head.type) of
	{'EXIT', Reason} -> 
	    dets_utils:vformat("** dets: Corrupt or truncated dets file~n", 
			       []), 
	    io:format("~nERROR ~p~n", [Reason]);
	[] ->  %% don't print empty buckets
	    true;
	{Size, CollP, Objects} ->
	    io:format("   <~w>~w: <~w:~p>~w~n", 
		      [BucketP, Slot, CollP, Size, Objects])
    end,
    v_segment(H, SegNo, SegPos, SegSlot+1).

%% -> [] | {Pointer, [object()]} | throw(EXIT)
read_bucket(Fd, Position, Type) ->
    case dets_utils:ipread(Fd, Position, infinity) of
	{ok, {Size, Pointer, <<Size:32, _Status:32, KeysObjs/binary>>}} ->
	    {Size, Pointer, bin2objs(KeysObjs, Type)};
	[] ->
	    []
    end.
     
-define(SEQSTART, -(1 bsl 26)).

%% -> [{Key,SizeOfWholeKey,WholeKeyBin,FirstObject,OtherObjects}] |throw(EXIT)
%% FirstObject = {Term, Seq, Binary}
%% Seq < 0 (and ascending).
per_key(Head, <<BinSize:32, ?ACTIVE:32, Bin/binary>> = B) ->
    true = size(B) == BinSize,
    if 
	Head#head.type == set ->
	    per_set_key(Bin, Head#head.keypos, []);
	true ->
	    per_bag_key(Bin, Head#head.keypos, [])
    end.

per_set_key(<<Size:32, T/binary>> = B, KeyPos, L) ->
    <<KeyBin:Size/binary, R/binary>> = B,
    Size1 = Size - 4,   
    <<Obj:Size1/binary, _/binary>> = T,
    Term = binary_to_term(Obj),
    Key = element(KeyPos, Term),
    Item = {Term, ?SEQSTART, KeyBin},
    per_set_key(R, KeyPos, [{Key,Size,KeyBin,Item,[]} | L]);
per_set_key(B, _KeyPos, L) when size(B) == 0 ->
    lists:reverse(L).

per_bag_key(<<Size:32, ObjSz:32, T/binary>> = B, KeyPos, L) ->
    <<KeyBin:Size/binary, R/binary>> = B,
    ObjSz1 = ObjSz - 4, 
    Size1 = Size - ObjSz - 4,
    <<Obj:ObjSz1/binary, KeyObjs:Size1/binary, _/binary>> = T,
    <<_Size:32, Bin:ObjSz/binary, _/binary>> = B,
    Term = binary_to_term(Obj),
    Key = element(KeyPos, Term),
    Item = {Term, ?SEQSTART, Bin},
    per_bag_key(R, KeyPos, [{Key,Size,KeyBin,Item,KeyObjs} | L]);
per_bag_key(B, _KeyPos, L) when size(B) == 0 ->
    lists:reverse(L).


binobjs2terms(<<ObjSz:32, _/binary>> = B) ->
    binobjs2terms(B, ObjSz, size(B)-ObjSz, ?SEQSTART+1, []);
binobjs2terms(B) when B == [] ->
    B;
binobjs2terms(B) when size(B) == 0 ->
    [].

binobjs2terms(Bin, ObjSz, Size, N, L) when Size == 0 ->
    ObjSz1 = ObjSz-4,
    <<_ObjSz:32, Obj:ObjSz1/binary>> = Bin,
    lists:reverse(L, [{binary_to_term(Obj), N, Bin}]);
binobjs2terms(Bin, ObjSz, Size, N, L) ->
    <<B:ObjSz/binary, T/binary>> = Bin,
    ObjSz1 = ObjSz-4,
    <<_ObjSz:32, Obj:ObjSz1/binary>> = B,
    <<NObjSz:32, _/binary>> = T,
    Item = {binary_to_term(Obj), N, B},
    binobjs2terms(T, NObjSz, Size-NObjSz, N+1, [Item | L]).


bin2objs(KeysObjs, set) ->
    <<ObjSz:32, T/binary>> = KeysObjs,
    bin2objs(T, ObjSz-4, size(KeysObjs)-ObjSz, []);
bin2objs(KeysObjs, _Type) ->
    bin2objs2(KeysObjs, []).

bin2objs2(<<Size:32, ObjSz:32, T/binary>>, L) ->
    bin2objs(T, ObjSz-4, Size-ObjSz-4, L);
bin2objs2(B, L) when size(B) == 0 ->
    lists:reverse(L).

bin2objs(Bin, ObjSz, Size, L) when Size == 0 ->
    <<Obj:ObjSz/binary, T/binary>> = Bin,
    bin2objs2(T, [binary_to_term(Obj) | L]);
bin2objs(Bin, ObjSz, Size, L) ->
    <<Obj:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    bin2objs(T, NObjSz-4, Size-NObjSz, [binary_to_term(Obj) | L]).


bin2bins(KeysObjs, set) ->
    <<ObjSz:32, T/binary>> = KeysObjs,
    bin2bins(T, ObjSz-4, size(KeysObjs)-ObjSz, []);
bin2bins(KeysObjs, _bag) ->
    bin2bins2(KeysObjs, []).

bin2bins2(<<Size:32, ObjSz:32, T/binary>>, L) ->
    bin2bins(T, ObjSz-4, Size-ObjSz-4, L);
bin2bins2(B, L) when size(B) == 0 ->
    lists:reverse(L).

bin2bins(Bin, ObjSz, Size, L) when Size == 0 ->
    <<Obj:ObjSz/binary, T/binary>> = Bin,
    bin2bins2(T, [Obj | L]);
bin2bins(Bin, ObjSz, Size, L) ->
    <<Obj:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    bin2bins(T, NObjSz-4, Size-NObjSz, [Obj | L]).


bin2keybins(KeysObjs, Head) when Head#head.type == set ->
    <<ObjSz:32, T/binary>> = KeysObjs,
    bin2keybins(T, Head#head.keypos, ObjSz-4, size(KeysObjs)-ObjSz, []);
bin2keybins(KeysObjs, Head) ->
    bin2keybins2(KeysObjs, Head#head.keypos, []).

bin2keybins2(<<Size:32, ObjSz:32, T/binary>>, Kp, L) ->
    bin2keybins(T, Kp, ObjSz-4, Size-ObjSz-4, L);
bin2keybins2(B, _Kp, L) when size(B) == 0 ->
    lists:reverse(L).

bin2keybins(Bin, Kp, ObjSz, Size, L) when Size == 0 ->
    <<Obj:ObjSz/binary, T/binary>> = Bin,
    Term = binary_to_term(Obj),
    bin2keybins2(T, Kp, [{element(Kp, Term),Obj} | L]);
bin2keybins(Bin, Kp, ObjSz, Size, L) ->
    <<Obj:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    Term = binary_to_term(Obj),
    bin2keybins(T, Kp, NObjSz-4, Size-NObjSz, [{element(Kp,Term),Obj} | L]).
