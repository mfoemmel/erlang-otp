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
-module(dets).

%% Disc based linear hashing lookup dict

-export([all/0,
	 select/2,
	 close/1,
	 delete/2,
	 delete_object/2,
	 do_match/2,
	 file_info/1,
	 first/1,
	 fixtable/2,
	 fsck/1,
	 get_head_field/2,
	 info/1,
	 info/2,
	 init/1,
	 insert/2,
	 inspect_chain/2,
	 istart_link/0,
	 lookup/2,
	 match/2,
	 match_delete/2,
	 match_object/2,
	 next/2,
	 open_file/1,
	 open_file/2,
	 safe_fixtable/2,
	 slot/2,
	 start/0,
	 start_link/0,
	 stop/0,
	 sync/1,
	 traverse/2,
	 update_counter/3,
	 verbose/0,
	 verbose/1
	]).

-export([system_continue/3, system_terminate/4, system_code_change/4]).

-export([is_var/1]).

-include_lib("kernel/include/file.hrl").

%% internal
-export([loop0/0, 
	 do_open_file/2,
	 do_open_file/11,
	 view/1]).

%% state for the dets server
-record(state,{store,parent}).

-define(HEADSZ, 40). %% number of bytes before the segm pointer array
-define(FREE, 16#3abcdef).  %% status fields
-define(FREE_AS_LIST, [3,171,205,239]).
-define(ACTIVE, 16#12345678). %% == i32(?ACTIVE_AS_LIST)
-define(A1, 18). 
-define(A2, 52).
-define(A3, 86).
-define(A4, 120).
-define(ACTIVE_AS_LIST, [?A1,?A2,?A3,?A4]).
-define(MAGIC, 16#0abcdef).   %% dets magic, won't ever change
-define(FILE_FORMAT_VERSION, 8). %% 6 in the R1A release, this is 8(c)
-define(CAN_BUMP_BY_REPAIR, [6, 7]).
-define(CAN_CONVERT_FREELIST, [8]).
%% Two different formats of freelists between 8(a) and 8(b)
%% 8(b) is recognized by the CLOSED_PROPERLY2 indicator.
%% The 8(c) format uses a different hashing algorithm, erlang:phash.
%% this is noted by a CLOSED_PROPERLY_NEW_HASH(_NEED_COMPACTING) written
%% instead of the CLOSED_PROPERLY2(_NEED_COMPACTING) in the CLOSED_PROPERLY 
%% field. 8(b) files are only converted to 8(c) if repair is done, so we need
%% compatability with 8(b) for a LONG time.
%% dets:info(Tab,hash) will give 'hash' for 8(b) files and 'phash' for 8(c).
-define(SET, 1).
-define(BAG, 2).
-define(DUPLICATE_BAG, 3).
-define(BIG, 16#ffffff).
-define(TRAILER, [88, 99, 111, 177]).  %% just a random seq	

-define(int32(Int), ([(Int bsr 24) band 255,
		      (Int bsr 16) band 255,
		      (Int bsr  8) band 255,
		      Int band 255])).


%% defines for the buddy allocator
-define(MAXBUD, 32).  %% 4 Gig is maxfile size
-define(BASE, (?HEADSZ +  (4 * (?SEGSZ + ?SEGARRSZ)))).
-define(ZERO, [0,0,0,0]).
-define(POW(X), (1 bsl X)).


%% Record for the head structure in a file
%% these records are held in RAM by the server

-record(head,  {
	  m,               %% size
	  next,            %% next position for growth (segm mgmt only)
	  fptr,            %% the file descriptor
	  no_items,        %% number of objects in table,
	  n,               %% split indicator
	  type,            %% set | bag | duplicate_bag
	  keypos,          %% default is 1 as for ets
	  ets,             %% local ets table used to hold the freelist
	  cache,           %% A #cache{} record
	  auto_save,       %% Integer | infinity 
	  update_mode,     %% saved | dirty
	  fixed = false,   %% fixed table ?
	  hash_bif         %% hash bif used for this file {phash|hash}
	 }).

-record(info, {    
	  filename,             %% anme of the file being used
	  access = read_write,  %% acces rights = (read | read_write)
	  ram_file = false,     %% true | false
	  name}).               %% the actual Name of the table

%% Assuming that each of these record fields occupy one word
%% This fileheader represents the head of a dets file on the
%% actual file.

-record(fileheader, {
	  freelist,
	  cookie,
	  closed_properly,
	  type,
	  version,
	  m,
	  next,
	  keypos,
	  no_items,
	  trailer,
	  eof,
	  n}).


%% Hardcoded positions right into the head field
-define(FREELIST_POS, 0).
-define(CLOSED_PROPERLY_POS, 8).
-define(CLOSED_PROPERLY,1).
-define(CLOSED_PROPERLY2,2).
-define(CLOSED_PROPERLY2_NEED_COMPACTING,3).
-define(CLOSED_PROPERLY_NEW_HASH,4). %% Format 8(c) rather than 8(b)
-define(CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING,5). %% Format 8(c)
-define(FILE_FORMAT_VERSION_POS, 16).
-define(D_POS, 20).
-define(CHAIN_LEN, 1).  %% medium chain len
-define(SEGSZ, 256).  %% size of each segment in words  
-define(SEGARRSZ, 8192).  %% max # segments


%%efine(TRACE(X, Y), io:format(X, Y)).
-define(TRACE(X, Y), true).

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%%  This is the implementation of the mnesia file storage
%%  Each (non ram-copies) table is maintained in a corresponding
%%  .DAT file. The dat file is organized as a segmented linear
%%  hashlist. The head of the file with the split indicator,
%%  size etc is held in ram by the server at all times.

%%  The actual layout of the file is :
%%   bytes   decsription
%%  ----------------------
%%    4      FreelistPointer
%%    4      Cookie
%%    4      ClosedProperly (pos=8)
%%    4      Type (pos=12)
%%    4      Version (pos=16)
%%    4      M
%%    4      Next
%%    4      KeyPos
%%    4      No_items
%%    4      N
%%  ------------------ end of header
%%    8192  SegmentArray 
%%  ------------------
%%    256  First Segment
%%  -----------------------------
%%    ???   Objects (Free and alive)
%%    256  Second Segment
%%    ???   Objects (Free and alive)
%%    .................

%%  The first word/slot in the segment array, then always has the
%%  value Headsize + 256
%%  Before we can find an object we must find the slot where the
%%  object resides. Each slot is a (possibly empty) list of
%%  objects that hash to the same slot. If the slot is 0, the slot chain
%%  is empty. If the slot is /= 0, the value points to a position
%%  in the file where a chain starts. Each object in a chain has the
%%  following layout:
%%  --------------------
%%    4     Next pointer
%%    4     Size
%%    4     Status  (FREE | ACTIVE)
%%    ??    Binary representing the object

%% We use the STATUS field for 2 different reason.
%%  1. Fast load, we can chunk read a file and just pick out
%%     the real objects from the ones on the freelist
%%  2. Emergency repair/recovery

%%  The freelist is a list with the same layout as the chain
%%  lists except that the status field is FREE on all objects there

%%  The method of hashing is the so called linear hashing algorithm
%%  with segments. 

%%|---------------|
%%|      head     |
%%|       	  |
%%|               |
%%|_______________|
%%|		  |------|
%%|___seg ptr1____|      |
%%|		  |      |
%%|__ seg ptr 2___|      |
%%|               |      |    segment 1
%%|	....	  |      V _____________
%%			 |		|
%%			 |		|
%%			 |___slot 0 ____|
%%                       |              |
%%                       |___slot 1 ____|-----|
%%			 |		|     |
%%			 |   .....	|     |  1:st obj in slot 1
%%					      V  segment 1
%%						|-----------|
%%						|  next     |
%%						|___________|
%%						|  size     |
%%						|___________|
%%						|  status   |
%%						|___________|
%%						|	    |
%%						|           |
%%						|   obj     |
%%						|           |

%% **************** Linear Hashing: *****************
%%  grows linearly
%%  
%%         - n indicates next bucket to split (initially zero); 
%%         - m is the initial size of the hash table 
%%  
%%         - to insert: 
%%                - hash = key mod m 
%%                - if hash < n then hash = key mod 2m 
%%                - if a collision occurs on the initial hash, resolve it 
%%                  by chaining, then split bucket n 
%%                      - add a new bucket to the end of the table 
%%                      - redistribute the contents of bucket n 
%%                        using hash = key mod 2m 
%%                      - increment n 
%%                      - if n = m then m = 2m, n = 0 
%%         - to search: 
%%                hash = key mod m 
%%                if hash < n then hash = key mod 2m 
%%                do linear scan of the bucket 
%%  
%%  
%%  


%% Given, a slot, return the {Pos, Chain} in the file
%% where the objects hashed to this slot resides.
%% Pos is the position in the file where the chain pointer is
%% written and Chain is the position in the file where the chain begins.

chain(Head, Slot) ->
    Pos = ?HEADSZ + (4 * (Slot div ?SEGSZ)),
    F = Head#head.fptr,
    Segment = seg_cache(F, Pos),
    FinalPos = Segment + (4 * (Slot rem ?SEGSZ)),
    {FinalPos, pread_4(F, FinalPos)}.

seg_cache(F, Pos) ->
    case get(Pos) of
	undefined ->
	    Segment = pread_4(F, Pos),
	    put(Pos, Segment),
	    Segment;
	Segment ->
	    Segment
    end.


%% Read the {Next, Size} field from the file
%% assuming the file points to a real object
read_8(File) ->
    {ok,  Bin} = file:read(File, 8),
    [N1,N2,N3,N4, S1,S2,S3,S4] = binary_to_list(Bin),
    {i32(N1,N2,N3,N4), i32(S1,S2,S3,S4)}.

pread_4(File, Pos) ->
    {ok, Bin} = file:pread(File, Pos, 4), 
    [X1,X2,X3,X4] = binary_to_list(Bin),
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

read_4(File) ->
    {ok, Bin} = file:read(File, 4), 
    [X1,X2,X3,X4] = binary_to_list(Bin),
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

pread_12(F, Pos) ->  %% check for possible eof here
    case file:pread(F, Pos, 12) of
	{ok, Bin} when size(Bin) == 12 ->
	    list_to_tuple(bin2ints(Bin));
	_ ->
	    eof
    end.

get_head_field(File, Field) ->
    {ok, _} = file:position(File, Field),
    read_4(File).

%% Open an already existing file, no arguments
fopen(Fname) ->
    case file:open(Fname, [binary, raw, read, write]) of
	{ok, F} ->
	    case read_head(F, Fname, read_write) of
		{error, not_closed} ->
		    file:close(F),
		    io:format(user,"dets: file ~p not properly closed, "
			      "reparing ...~n",[Fname]),

		    case fsck(Fname) of
			{error, Reason} ->
			    {error, Reason};
			ok ->
			    fopen(Fname)
		    end;
		{ok, Head, Info, _} ->
		    file:close(F),
		    fopen(make_ref(), Fname, Head#head.type, 
			  Head#head.keypos, false, default, false, 0, infinity, read_write);
		Other ->
		    file:close(F),
		    err(open_file, Other)
	    end;
	Other ->
	    Other
    end.

access(read) ->
    [raw, binary, read];
access(read_write) ->
    [raw, binary, read, write].


%% Open and possibly create and initialize a file

fopen(Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc) ->
    case catch fopen2(Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc) of
	{'EXIT', Reason} -> err(open_file, Reason);
	Other -> Other
    end.

fopen2(Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc) ->
    case exists(Fname) of
	yes when Ram == false ->
	    case file:open(Fname, access(Acc)) of
		{ok, Fd} ->
		    fopen_existing_file(Fd, Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc);
		{error, Reason} ->
		    err(open_file, Reason)
	    end;
	yes when Ram == true ->
	    {ok, B} = file:read_file(Fname),
	    case ram_file:open(B, access(Acc)) of
		{ok, Fd} -> 
		    fopen_existing_file(Fd, Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc);
		{error, Reason} ->  %% Can this fail ??
		    err(open_file, Reason)
	    end;
	no when Ram == true ->
	    case ram_file:open([], access(Acc)) of
		{ok, Fd} ->
		    fopen_init_file(Fd, Tab, Fname, Type, Kp, Est, Ram, CacheSz, Auto, Acc);
		{error, Reason} ->
		    err(open_file, Reason)
	    end;
	no when Ram == false ->
	    case file:open(Fname,access(Acc)) of
		{ok, Fd} ->
		    fopen_init_file(Fd, Tab, Fname, Type, Kp, Est, Ram, CacheSz, Auto, Acc);
		{error, Reason} ->
		    err(open_file, Reason)
	    end
    end.


fopen_existing_file(F, Tab, Fname, Type, Kp, force, Est, Ram, CacheSz, Auto, Acc) ->
    io:format(user,"dets: file ~p, repair forced.~n",[Tab]),
    file:close(F),
    case fsck(Tab, Type, Kp, Fname, 8) of
	{error, R} -> 
	    {error, R};
	ok ->
	    fopen(Tab, Fname, Type, Kp, false, Est, Ram, CacheSz, Auto, Acc)
    end;
    
fopen_existing_file(F, Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc) ->
    case read_head(F, Fname, Acc) of
	{error, Reason} when Acc == read ->
	    file:close(F),
	    err(open_file, Reason);
	{error, not_closed} when Rep == true   ->
	    %% Gotta repair the file
	    file:close(F),
	    io:format(user,"dets: file ~p not properly "
		      "closed, "
		      "reparing ...~n",[Tab]),
	    case fsck(Tab, Type, Kp, Fname, 8) of
		{error, R} -> 
		    {error, R};
		ok ->
		    fopen(Tab, Fname, Type, Kp, false, Est, Ram, CacheSz, Auto, Acc)
	    end;
	{error, version_bump} when Rep == true ->
	    io:format(user,"dets: file ~p old version, "
		      "upgrading ...~n",[Tab]),
	    file:close(F),
	    case fsck(Tab, Type, Kp, Fname, 1) of
		{error, R} -> 
		    {error, R};
		ok ->
		    fopen(Tab, Fname, Type, Kp, false, Est, Ram, CacheSz, Auto, Acc)
	    end;
	{error, not_closed} when Rep == false ->
	    file:close(F),
	    err(open_file, need_repair);
	{ok, Head, Info, need_compacting} when Rep == true ->
	    %% The file needs to be compacted due to a very big and
	    %% fragmented free_list
	    file:close(F),
	    io:format(user,"dets: file ~p is now compacted ...~n",[Tab]),
	    case fsck(Tab, Type, Kp, Fname, 8) of
		{error, R} -> 
		    {error, R};
		ok ->
		    fopen(Tab, Fname, Type, Kp, false, Est, Ram, CacheSz, Auto, Acc)
	    end;
	{ok, Head, Info, ExtraInfo} ->
	    if
		Head#head.type == Type,
		Head#head.keypos == Kp ->
		    Info2 = Info#info{name = Tab, ram_file = Ram},
		    Ftab =
			case Acc of
			    read_write -> init_freelist(Head, ExtraInfo);
			    read -> false
			end,
		    Cache = cache_init(CacheSz),
		    {ok, Head#head{ets = Ftab, auto_save = Auto, cache = Cache}, Info2};
		true ->
		    file:close(F),
		    err(open_file, wrong_type_or_keypos)
	    end;
	Other -> 
	    err(open_file, Other)
    end.


init_more_segments(Head, F, SegNo, Factor) when SegNo < Factor ->
    {NewHead,Segm} = alloc(Head, 4 * ?SEGSZ),
    {ok, _} = file:position(F, Segm),
    zero(F, ?SEGSZ),
    ok = file:pwrite(F,  ?HEADSZ + (4 * SegNo),  ?int32(Segm)),
    init_more_segments(NewHead, F, SegNo+1, Factor);
init_more_segments(Head, F, SegNo, Factor) ->
    Head.


fopen_init_file(F, Tab, Fname, Type, Kp, Est, Ram, CacheSz, Auto, read_write) ->
    Factor = if Est == default -> 1;
		true           -> 1 + (Est div ?SEGSZ)
	     end,
    file:truncate(F),
    Freelist = 0,
    Cookie = ?MAGIC,
    Version = ?FILE_FORMAT_VERSION,
    ClosedProperly = 1,
    N = 0,
    M = ?SEGSZ * Factor,
    NoItems = 0,
    Next = ?SEGSZ * Factor,
    ok = file:pwrite(F, 0, [?int32(Freelist),
			    ?int32(Cookie),
			    ?int32(ClosedProperly),
			    ?int32(tt(Type)),
			    ?int32(Version),
			    ?int32(M),
			    ?int32(Next),
			    ?int32(Kp),
			    ?int32(NoItems),
			    ?int32(N)]),

    %% That was the header, 

    Ftab = init_alloc(), %% init allocator

    %%now we need to init
    %% the segment pointer array,
    {ok, _} = file:position(F, ?HEADSZ),
    zero(F, ?SEGARRSZ),
    %% We also need to initialize the first segement
    zero(F, ?SEGSZ),
    %% and we must set the first slot of the
    %% segment pointer array to point to the first
    %% segment

    ok = file:pwrite(F, ?HEADSZ,?int32(?HEADSZ + (4 * ?SEGARRSZ))), 

    NewHead = init_more_segments(#head{ets=Ftab}, F, 1, Factor),
    Cache = cache_init(CacheSz),

    %% Return a new nice head structure
    Head = #head{
      m  = M,
      next = Next,
      fptr = F,
      no_items = NoItems,
      n = N,
      type = Type,
      ets = NewHead#head.ets,
      cache = Cache,
      auto_save = Auto,
      update_mode = saved,
      hash_bif = phash,
      keypos = Kp},
    %% and a new nice Info structure
    Info = #info{
      ram_file = Ram,
      filename = Fname,
      name = Tab},
    perform_save(Head#head{update_mode = dirty}, Info),
    {ok, Head, Info};

fopen_init_file(F, Tab, Fname, Type, Kp, Est, Ram, CacheSz, Auto, read) ->
    err(open_file, acccess_mode).


%% Given a file  pointer, read and validate the head of the 
%% file, set opened field, return {ok, Head}

read_head(F, Fn, Access) ->
    case catch read_head_fields(F) of
	{ok, FH} ->
	    Test = if
		       FH#fileheader.cookie /= ?MAGIC ->
			   file:close(F),
			   {error, not_a_dets_file};
		       FH#fileheader.version /= ?FILE_FORMAT_VERSION -> 
			   case 
			       lists:member(FH#fileheader.version, 
					    ?CAN_BUMP_BY_REPAIR) of
			       true ->
				   file:close(F),
				   {error, version_bump};
			       false ->
				   file:close(F),
				   {error, bad_version}
			   end;
		       FH#fileheader.trailer /= FH#fileheader.eof ->
			   file:close(F),
			   {error, not_closed};
		       FH#fileheader.closed_properly == 1  ->
			   case lists:member(FH#fileheader.version,
					     ?CAN_CONVERT_FREELIST) of
			       true ->
				   {ok,{convert_freelist,
					FH#fileheader.version},hash};
			       false ->
				   {error, not_closed} % should not happen
			   end;
		       FH#fileheader.closed_properly == 
		       ?CLOSED_PROPERLY2  ->
			   {ok,true,hash};
		       FH#fileheader.closed_properly == 
		       ?CLOSED_PROPERLY2_NEED_COMPACTING  ->
			   {ok, need_compacting,hash};
		       FH#fileheader.closed_properly == 
		       ?CLOSED_PROPERLY_NEW_HASH  ->
			   {ok,true,phash};
		       FH#fileheader.closed_properly == 
		       ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING  ->
			   {ok, need_compacting,phash};
		       FH#fileheader.closed_properly == 0 ->
			   file:close(F),
			   {error, not_closed};
		       FH#fileheader.closed_properly > 
		       ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING  ->
			   file:close(F),
			   {error, not_closed};
		       true ->
			   {ok,true,phash}
		   end,
	    case Test of
		{ok,ExtraInfo,HashAlg} ->
		    H = #head{
		      m = FH#fileheader.m,
		      next = FH#fileheader.next,
		      fptr = F,
		      no_items= FH#fileheader.no_items,
		      n = FH#fileheader.n,
		      type = FH#fileheader.type,
		      update_mode = saved,
		      hash_bif = HashAlg,
		      keypos = FH#fileheader.keypos},
		    I = #info{filename = Fn, access = Access},
		    {ok, H, I, ExtraInfo};
		{error,Reason} ->
		    {error,Reason}
	    end;
	Other ->
	    file:close(F),
	    Other
    end.

%% Read the fileheader
read_head_fields(Fd) ->
    case file:pread(Fd, 0, ?HEADSZ) of
	{ok, Bin} when size(Bin) == ?HEADSZ ->
	    [Freelist,
	     Cookie,
	     ClosedProperly,
	     Type2, 
	     Version,
	     M,
	     Next,
	     KeyPos,
	     NoItems, 
	     N] = bin2ints(Bin),
	    {ok, EOF} = file:position(Fd, eof),
	    case file:pread(Fd, EOF-4, 4) of
		{ok, BB} when size(BB) == 4 ->
		    {ok, #fileheader {freelist = Freelist,
				      cookie = Cookie,
				      closed_properly = ClosedProperly,
				      type = catch tt(Type2),
				      version = Version,
				      m = M,
				      next = Next,
				      keypos = KeyPos,
				      no_items = NoItems,
				      trailer = i32(binary_to_list(BB)),
				      eof = EOF,
				      n = N}};
		Other ->
		    Other
	    end;
	{ok, Bin} ->
	    {error, tooshort};
	Other ->
	    Other
    end.


tt(?SET) -> set;
tt(?BAG) -> bag;
tt(?DUPLICATE_BAG) -> duplicate_bag;
tt(set) -> ?SET;
tt(bag) -> ?BAG;
tt(duplicate_bag) -> ?DUPLICATE_BAG.


%% Given a filename, fsck it
fsck(Fname) ->
    case file:open(Fname, [binary, raw, read]) of
	{ok ,Fd} ->
	    case read_head_fields(Fd) of
		{ok, FH} ->
		    ?DEBUGF("FileHeader: ~p~n",[FH]),
		    file:close(Fd),
		    Tab = make_ref(),
		    fsck(Tab, FH#fileheader.type, FH#fileheader.keypos,
			 Fname, 8);
		Other ->
		    file:close(Fd),
		    Other
	    end;
	Other ->
	    Other
    end.


fsck(Tab, Type, KeyPos, Fname, Bump) ->
    case catch fsck2(Tab, Type, KeyPos, Fname, Bump) of
	{'EXIT', Reason} -> err(fsck, Reason);
	Other -> Other
    end.

fsck2(Tab, Type, KeyPos, Fname, Bump) ->
    Tmp = Fname ++ ".TMP",
    file:delete(Tmp),
    case fopen(Tab, Tmp, Type,  KeyPos, false, default, false, 0, infinity, read_write) of
	{error, Reason} -> 
	    {error, Reason};
	{ok, Head, I } ->  %% New empty file with wrong filename
	    {ok, Info} = file:read_file_info(Fname),
	    Fz = Info#file_info.size,
	    {ok, F} = file:open(Fname, [raw, binary, read]),
	    Cp = ?HEADSZ + ((?SEGSZ + ?SEGARRSZ) *  4 ), %% current pos
	    H2 = 
		case (Fz >= Cp) of
		    true ->
			do_fsck(Head, Tab, Cp, Fz, F, 0,Bump);
		    false ->
			%% Bad luck, the segment list was truncated /hakan
			Head
		end,
	    file:close(F), %% the corrupted input file
	    case fclose(H2#head{update_mode = dirty}, I) of
		ok ->
		    ok = file:rename(Tmp, Fname);
		Other ->
		    file:delete(Tmp),
		    Other
	    end
    end.

%% Bump is 8 since all objects are allocated aligned to this
%% Bump is 1 if we use fsck to upgrade version of dets file
do_fsck(Head, Tab, Cp, Fz, F, I, Bump) ->
    case pread_12(F, Cp) of
	{Next, Sz, Magic} ->
	    case in_range(F, Cp, Fz, Sz, Magic) of
		{true, active} ->
		    {ok, Bin} = file:pread(F, Cp+12, Sz),
		    case catch erlang:binary_to_term(Bin) of
			{'EXIT', _} ->
			    do_fsck(Head, Tab , Cp + Bump, Fz, F, I+1,Bump);
			Term when Bump == 8 ->
			    ?TRACE("RECOVER ~p~n", [Term]),
			    H2 = finsert(Head, Term),
			    Cp2 = Cp + ?POW(sz2pos(Sz+12)),
			    do_fsck(H2, Tab, Cp2, Fz, F, I+1, Bump);
			Term when Bump == 1 ->
			    H2 = finsert(Head, Term),
			    Cp2 = Cp + Sz + 12,
			    do_fsck(H2, Tab, Cp2, Fz, F, I+1, Bump)
		    end;
		false ->
		    do_fsck(Head, Tab, Cp + Bump, Fz, F, I, Bump)
	    end;
	eof ->
	    Head
    end.

in_range(F, Cp, Fz, Sz, ?ACTIVE) when Cp + 12 + Sz  =< Fz ->
    {true, active};
in_range(F, Cp, Fz, Sz, Magic) -> 
    false.

mark_not_closed(F) ->
    ok = file:pwrite(F, ?CLOSED_PROPERLY_POS, [0,0,0,0]),
    file:sync(F).

mark_dirty(F) ->
    mark_not_closed(F),
    Pos = pread_4(F, ?FREELIST_POS),
    {ok, _} = file:position(F, Pos),
    file:truncate(F).

perform_save(H, I) when H#head.update_mode == dirty ->
    F = H#head.fptr,
    ok = file:pwrite(F, ?D_POS, [?int32(H#head.m),
				 ?int32(H#head.next),
				 ?int32(H#head.keypos),
				 ?int32(H#head.no_items),
				 ?int32(H#head.n)]),
    FL = H#head.ets,
    B = term_to_binary(FL),
    Size = size(B),
%%    io:format("size of freelist = ~p~n",[Size]),
%%    io:format("head.m = ~p~n",[Head#head.m]),
%%    io:format("head.no_items = ~p~n",[Head#head.no_items]),

    {ok,Pos} = file:position(F, eof),
    ok = file:pwrite(F, ?FREELIST_POS, ?int32(Pos)),
    ok = file:pwrite(F, Pos, [?ZERO, ?int32(Size), ?FREE_AS_LIST, B]),
    
    {ClosedProperly, ClosedProperlyNeedCompacitng} = 
	case H#head.hash_bif of
	    hash ->
		{?CLOSED_PROPERLY2, ?CLOSED_PROPERLY2_NEED_COMPACTING};
	    phash ->
		{?CLOSED_PROPERLY_NEW_HASH, ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING}
	end,
    if 
	Size > 1000, Size > H#head.no_items ->
	    ok = file:pwrite(F, ?CLOSED_PROPERLY_POS, 
			     ?int32(ClosedProperlyNeedCompacitng));
	true ->
	    ok = file:pwrite(F, ?CLOSED_PROPERLY_POS, 
			     ?int32(ClosedProperly))
    end,
    ok = file:pwrite(F,?FILE_FORMAT_VERSION_POS,?int32(?FILE_FORMAT_VERSION)),
    {ok,Pos2} = file:position(F, eof),
    ok = file:pwrite(F, Pos2, i32(Pos2 + 4)),  %% sizeof file as trailer
    ensure_written(F, I);
perform_save(H, I) when H#head.update_mode == saved ->
    ?DEBUGF("Already saved?~n",[]),
    %% Already synced
    ok.

ensure_written(F, I) when I#info.ram_file == true ->
    {ok, EOF} = file:position(F, eof),
    case file:pread(F, 0, EOF) of
	{ok, Bin} ->
	    case file:write_file(I#info.filename, Bin) of
		ok ->
		    ok;
		Other ->
		    err(sync, Other)
	    end;
	Other ->
	    err(sync, Other)
    end;
ensure_written(F, I) when I#info.ram_file == false ->
    file:sync(F).
        
fclose(Head, Info) ->
    Res = (catch perform_save(Head, Info)),
    case Info#info.ram_file of
	true -> ignore;
	false -> file:close(Head#head.fptr)
    end,
    Res.

exists(Fn) ->
    case file:open(Fn, [read, raw]) of
	{ok, F} ->
	    file:close(F), 
	    yes;
	_ ->
	    no
    end.

fselect(H,P) ->
    R = make_ref(),
    case (catch {R, do_fselect(H,P)}) of
	{R, Result} -> %%% The result should contain the new (updated?) head
	    Result;
	[] -> %%% A way to interrupt, the head cannot be changed here
	    {H,[]};
	Thrown -> %%% Everything else is an error
	    {H, {error, Thrown}}
    end.

find_all_keys(A,B) ->
    case (catch do_find_all_keys(A,B)) of
	L when list(L) ->
	    L;
	Else ->
	    throw(Else)
    end.

do_find_all_keys([], _) ->
    [];
do_find_all_keys([{H,_,_}|T], KeyPos) when tuple(H) ->
    case size(H) of
	Enough when Enough >= KeyPos ->
	    case find_one_key(element(KeyPos,H)) of
		{ok, Key} ->
		    Keys = do_find_all_keys(T,KeyPos),
		    case lists:member(Key,Keys) of
			true ->
			    Keys;
			_ ->
			    [Key | Keys]
		    end;
		_ ->
		    throw([])
	    end;
	_ ->
	    do_find_all_keys(T,KeyPos)
    end;

do_find_all_keys(_,_) ->
    throw(bad_match_spec).
    

find_one_key(Key) ->
    case contains_variable(Key) of
	true ->
	    no;
	false ->
	    {ok, Key}
    end.

contains_variable('_') ->
    true;
contains_variable(A) when atom(A) ->
    case atom_to_list(A) of
	[$$ | T] ->
	    case (catch list_to_integer(T)) of
		{'EXIT', _} ->
		    false;
		_ ->
		    true
	    end;
	_ ->
	    false
    end;

contains_variable(T) when tuple(T) ->
    contains_variable(tuple_to_list(T));

contains_variable([]) ->
    false;
contains_variable([H|T]) ->
    case contains_variable(H) of
	true ->
	    true;
	false ->
	    contains_variable(T)
    end;

contains_variable(_) ->
    false.
    

do_fselect(Head, Pat) ->
    KeyPos = Head#head.keypos,
    MP = case ets:match_spec_compile(Pat) of
	     X when binary(X) ->
		 X;
	     _ ->
		 throw(bad_match_spec)
	 end,
    case find_all_keys(Pat,KeyPos) of
	[] -> %%% Complete match
	    MFun = fun(Term) ->
			   case ets:match_spec_run([Term],MP) of
			       [] ->
				   false;
			       [Else] ->
				   {true, Else}
			   end
		   end,
	    {Head, match_scan(Head, 0, [], MFun)};
	List ->
	    Fun = fun(H, Key) ->	    
			  {NewHead, Objs} = cache_fread(H, Key),
			  MatchRes = case Objs of 
					 [] -> [];
					 _ -> ets:match_spec_run(Objs,MP)
				     end,
			  {NewHead, MatchRes}
		  end,
	    select_for_keys(List,Fun,Head, [])
    end.
   
select_for_keys([], Fun, Head, Acc) ->
    {Head, Acc};
select_for_keys([K|T], Fun, Head, Acc) ->
    {NewHead, Res} = Fun(Head, K), 
    select_for_keys(T,Fun,NewHead, Res ++ Acc).

%% Depending on the value of Ret, this function can return either
%% objects or bindings.
%% Ret == (object || bindings)
fmatch_object(Head, Pat, Ret)  -> 
    Kp = Head#head.keypos,
    Key = if
	      Pat == '_' -> '_';
	      size(Pat) >= Kp  ->  element(Kp, Pat);
	      true -> '$end_of_table'  %% the most unlikeley key of'em all
	  end,
    case has_var(Key) of
	false ->
	    {NewHead, Objs} = cache_fread(Head, Key),
	    MatchingObjs = 
		lists:zf(fun(O) ->  case do_match(O, Pat) of
					false -> 
					    false;
					{true, Bs} when Ret == object ->
					    {true, O};
					{true, Bs} when Ret == bindings ->
					    {true, fix_b(Bs)}
				    end
			 end,  Objs),
	    {NewHead, MatchingObjs};
	{true,_} -> %% Gotta scan the whole file
	    MFun = fun(Term) ->
			   case do_match(Term,Pat) of
			       {true, Bs} when Ret == bindings -> 
				   {true, fix_b(Bs)};
			       {true, Bs} when Ret == object ->
				   {true, Term};
			       Else ->
				   Else
			   end
		   end,
	    {Head, match_scan(Head, 0, [], MFun)}
    end.

match_scan(_, ?SEGARRSZ, Ack, MFun) -> 
    Ack;
match_scan(Head, SegNo, Ack, MFun) ->
    Seg = pread_4(Head#head.fptr, ?HEADSZ + (4 * SegNo) ),
    if
	Seg == 0 ->
	    Ack;
	true ->
	    Ack2 = scan_seg(Head, SegNo, Seg, 0, Ack, MFun),
	    match_scan(Head, SegNo+1, Ack2, MFun)
    end.

scan_seg(Head,_, _, ?SEGSZ, Ack, MFun) -> Ack;
scan_seg(Head, SegNo, SegPos, SegSlot, Ack, MFun) ->
    %%    Slot = SegSlot + (SegNo * ?SEGSZ),
    Chain = pread_4(Head#head.fptr, SegPos + (4 * SegSlot) ),
    Ack2 = scan_chain(Head, Chain, Ack, MFun),
    scan_seg(Head, SegNo, SegPos, SegSlot+1, Ack2, MFun).

scan_chain(Head, 0, Ack, MFun) -> Ack;
scan_chain(Head, Pos, Ack, MFun) ->
    {ok, Next, Sz, Term} = prterm(Head#head.fptr, Pos),
    case MFun(Term) of
	{true , Bs} ->
	    scan_chain(Head, Next, [Bs | Ack], MFun);
	false ->
	    scan_chain(Head, Next, Ack, MFun)
    end.


	

fmatch_delete(Head, Pat)  ->
    Kp = Head#head.keypos,
    Key = if
	      Pat == '_' -> '_';
	      tuple(Pat), size(Pat) >= Kp -> element(Kp, Pat);
	      true -> '$end_of_table'
	  end,
    {NewHead,Dels} = case has_var(Key) of
			 false ->
			     Slot = db_hash(Key, Head),
			     mdel_slot(Head, Pat, 0, Slot);
			 {true,_} ->
			     mdel_slots(Head, Pat, 0, 0)
		     end,
    X = NewHead#head.no_items - Dels,
    NewHead#head{no_items = X}.


mdel_slots(Head, Pat, Deletions, Slot) ->
    case mdel_slot(Head, Pat, 0, Slot) of
	{NewHead, '$end_of_table'} ->  {NewHead, Deletions};
	{NewHead, D2} ->  mdel_slots(NewHead, Pat, Deletions + D2, Slot+1)
    end.

mdel_slot(Head, Pat, Dels, Slot) when Slot >= Head#head.next ->
    {Head,'$end_of_table'};
mdel_slot(Head, Pat, Dels, Slot) ->
    {Pos, Chain} = chain(Head, Slot),
    mdel_scan(Head, Pat, Dels, Pos, Chain).

mdel_scan(Head, Pat, Dels, Prev, 0) -> 
    {Head,Dels};
mdel_scan(Head, Pat, Dels, Prev, Pos) -> 
    F = Head#head.fptr,
    {ok, Next, Sz, Term} = prterm(F, Pos),
    case do_match(Term, Pat) of
	false ->
	    mdel_scan(Head, Pat, Dels, Pos, Next);
	{true, _} ->
	    NewHead = cache_free(F, Head, Pos, Sz+12, 
				 element(Head#head.keypos, Term)),
	    ok = file:pwrite(F, Prev, ?int32(Next)),
	    mdel_scan(NewHead, Pat, Dels+1, Prev, Next)
    end.


fdelete(Head, Key) ->
    Slot = db_hash(Key, Head),
    Kp = Head#head.keypos,
    F = Head#head.fptr,
    {Pos, Chain} = chain(Head, Slot),
    case search_key(F, Pos, Chain, Key, Kp) of
	{no, _} ->
	    Head;
	{ok, Prev, Pos2, Next, Size, Term}  ->
	    Ets = Head#head.ets,
	    {NewHead,Items} = loop_key_delete(Head, F, Prev, Pos2, Next,Size, Key, 1, Kp),
	    X = NewHead#head.no_items - Items,
	    NewHead#head{no_items = X}
    end.

loop_key_delete(Head, F, Prev, Pos, Next, Size, Key, Deletions, Kp) ->
    ?DEBUGF("loop_key_delete(~p,~p,~p,~p.~p,~p,~p,~p,~p)~n",
	      [Head, F, Prev, Pos, Next, Size, Key, Deletions, Kp]),
    NewHead = cache_free(F, Head, Pos, Size+12, Key),
    ok = file:pwrite(F, Prev, ?int32(Next)),
    if
	Next == 0 ->
	    {NewHead,Deletions};
	true ->
	    case prterm(F, Next) of
		{ok, Next2, Size2, Term} when element(Kp, Term) == Key ->
		    loop_key_delete(NewHead, F, Prev, Next, Next2, Size2, Key, 
				    Deletions+1, Kp);
		_ ->
		    {NewHead,Deletions}
	    end
    end.

fdelete_object(Head, Obj) when tuple(Obj), size(Obj) >= Head#head.keypos  ->
    Kp = Head#head.keypos,
    Key = element(Kp, Obj),
    Slot = db_hash(Key, Head),
    F = Head#head.fptr,
    {Pos, Chain} = chain(Head, Slot),
    case search_object(F, Pos, Chain, Key, Obj) of
	{no, _} ->
	    {Head, ok};
	{ok, Prev, Pos2, Next, Size} when Head#head.type /= duplicate_bag  ->
	    NewHead = cache_free(F, Head, Pos2, Size+12, Key),
	    ok = file:pwrite(F, Prev, ?int32(Next)),
	    X = NewHead#head.no_items - 1,
	    {NewHead#head{no_items = X}, ok};
	{ok, Prev, Pos2, Next, Size} ->
	    NewHead = cache_free(F, Head, Pos2, Size+12, Key),
	    ok = file:pwrite(F, Prev, ?int32(Next)),
	    X = NewHead#head.no_items - 1,
	    fdelete_object(NewHead#head{no_items = X}, Obj)
    end;


fdelete_object(Head, Obj) ->
    {Head, err(delete_object, notuple)}.


ftraverse(Head, Fun) ->
    N = Head#head.no_items,
    ftraverse(Head, Fun, 0, 0, N, []).
ftraverse(Head, Fun, Slot, Sofar, N, Ack)  when Sofar < N ->
    case fslot(Head, Slot) of
	'$end_of_table' -> 
	    [];
	[] ->  %% probably a very common case
	    ftraverse(Head, Fun, Slot+1, Sofar, N, Ack);
	Objs ->
	    case do_ftraverse(Fun, Objs, Ack) of
		{done, Result} ->
		    Result;
		{continue, Ack2} ->
		    Len = Sofar + length(Objs),
		    ftraverse(Head, Fun, Slot+1, Len, N, Ack2)
	    end
    end;
ftraverse(Head, Fun, Slot, Sofar, N, Ack) -> Ack.

do_ftraverse(Fun, [], Ack) ->
    {continue, Ack};
do_ftraverse(Fun, [O|Objs], Ack) ->
    case catch Fun(O) of
	continue  ->
	    do_ftraverse(Fun, Objs, Ack);
	{continue, Val} ->
	    do_ftraverse(Fun, Objs, [Val | Ack]);
	{done, Value} ->
	    {done, [Value|Ack]};
	Other ->
	    {done, err(traverse, Other)}
    end.

finfo(H, I) -> 
    [{type, H#head.type}, 
     {keypos, H#head.keypos}, 
     {size, H#head.no_items},
     {file_size, file_size(H#head.fptr)},
     {filename, I#info.filename}].

file_size(F) -> 
    {ok, Pos} = file:position(F, eof),
    Pos.

finfo(H, I, type) -> H#head.type;
finfo(H, I, keypos) -> H#head.keypos;
finfo(H, I, size) -> H#head.no_items;
finfo(H,I, file_size) -> file_size(H#head.fptr);
finfo(H, I, filename) -> I#info.filename;
finfo(H, I, memory) -> file_size(H#head.fptr);
finfo(H, I, pid) -> self();
finfo(H, I, fixed) -> H#head.fixed;
finfo(H, I, hash) -> H#head.hash_bif;
finfo(_, _,_) -> err(info, badarg).

fslot(H, Slot) when Slot >= H#head.next ->
    '$end_of_table';
fslot(H, Slot) ->
    {Pos, Chain} = chain(H, Slot),
    collect_chain(H#head.fptr, Chain).

collect_chain(F, 0) -> [];
collect_chain(F, Pos) ->
    {ok, Next, Sz, Term} = prterm(F, Pos),
    [Term | collect_chain(F, Next)].


fread(Head, Key) ->
    Slot = db_hash(Key, Head),
    {Pos, Chain} = chain(Head, Slot),
    F = Head#head.fptr,
    Kp = Head#head.keypos,
    case search_key(F, Pos, Chain, Key, Kp) of
	{no, _} ->
	    [];
	{ok, Prev, Pos2, Next, Size, Term} when Head#head.type == set ->
	    [Term];
	{ok, Prev, Pos2, Next, Size, Term}  ->  %% bag or duplicate_bag
	    acc_fread(F, Pos2, Next, Key, [Term], Kp)
    end.

acc_fread(F, Prev, Pos, Key, Ack, Kp) ->
    case search_key(F, Prev, Pos, Key, Kp) of
	{ok, _, Pos2, Next2, _, Term2} ->
	    acc_fread(F, Pos2, Next2, Key, [Term2|Ack], Kp);
	_ ->
	    Ack
    end.

search_key(_,_,0,_,_) ->
    {no, 0};
search_key(F, Prev, Pos, Key, Kp) ->
    case prterm(F, Pos) of
	{ok, Next, Size, Term} when element(Kp, Term) == Key ->
	    {ok, Prev, Pos, Next, Size, Term};
	{ok, 0, Size, _} ->  %% Last obj in chain
	    {no, Pos};
	{ok, Next, _, _} ->
	    search_key(F, Pos, Next, Key, Kp);
	no ->
	    {no, Pos}
    end.

search_object(_,_,0,_,_) ->
    {no, 0};
search_object(F, Prev, Pos, Key, Term) ->
    case prterm(F, Pos) of
	{ok, Next, Size, Term} ->
	    {ok, Prev, Pos, Next, Size};
	{ok, 0, Size, Term2} ->  %% Last obj in chain
	    {no, Pos};
	{ok, Next, _, Term2}  ->
	    search_object(F, Pos, Next, Key, Term);
	_ ->
	    {no, Pos}
    end.

ffirst(H) ->
    ffirst(H, 0).
ffirst(H, Slot) ->
    case fslot(H, Slot) of
	'$end_of_table' -> '$end_of_table';
	[] -> ffirst(H, Slot+1);
	[X|_] -> element(H#head.keypos, X)
    end.

fnext(Head, Key) ->
    Slot = db_hash(Key, Head),
    fnext(Head, Key, Slot).
fnext(H, Key, Slot) ->
    case fslot(H, Slot) of
	'$end_of_table' -> '$end_of_table';
	L -> fnext_search(H, Key, Slot, L)
    end.
fnext_search(H, K, Slot, L) ->
    Kp = H#head.keypos,
    case beyond_key(K, Kp, L) of
	[] ->
	    fnext_slot(H, K, Slot+1);
	L2 -> element(H#head.keypos, hd(L2))
    end.

%% We gotta continue to search for the next key in the next slot
fnext_slot(H, K, Slot) ->
    case fslot(H, Slot) of
	'$end_of_table' -> '$end_of_table';
	[] -> fnext_slot(H, K, Slot+1);
	L -> element(H#head.keypos, hd(L))
    end.

beyond_key(K, Kp, []) -> [];
beyond_key(K, Kp, [H|T]) when element(Kp, H) /= K ->
    beyond_key(K, Kp, T);
beyond_key(K, Kp, [H|T]) when element(Kp, H) == K ->
    beyond_key2(K, Kp, T).

beyond_key2(K, Kp, []) -> [];
beyond_key2(K, Kp, [H|T]) when element(Kp, H) == K ->
    beyond_key2(K, Kp, T);
beyond_key2(K, Kp, L) ->
    L.

finsert(Head, Object) ->
    Kp = Head#head.keypos,
    Key = element(Kp, Object),
    Slot = db_hash(Key, Head),
    {Pos, Chain} = chain(Head, Slot),
    F = Head#head.fptr,
    Bin = term_to_binary(Object),
    Size = size(Bin),
    {H1,I} = if 
		 Head#head.type == set ->
		     ?DEBUGF("search_key(~p, ~p, ~p, ~p, ~p)~n",
			       [F, Pos, Chain, Key, Kp]),
		     case search_key(F, Pos, Chain, Key, Kp) of
			 {no, _} ->  %% insert new object at head of list
			     {NewHead,DataPos} = alloc(Head, 12 + Size),
			     ?DEBUGF("{NewHead,DataPos} = ~p~n",[{NewHead,DataPos}]),
			     ok = file:pwrite(F, DataPos, [?int32(Chain), 
							   ?int32(Size), 
							   ?ACTIVE_AS_LIST, Bin]),
			     ok = file:pwrite(F, Pos, ?int32(DataPos)), 
			     NewHead2 = cache_replace(NewHead, Key, [Object]),
			     {NewHead2,1};

			 {ok, Prev, Pos2, Next, Size2, Term} when Size > Size2    ->
			     {NewHead,DataPos} = alloc(Head, 12 + Size),
			     ok = file:pwrite(F, DataPos, [?int32(Next), 
							   ?int32(Size), 
							   ?ACTIVE_AS_LIST, Bin]),
			     ok = file:pwrite(F, Prev, ?int32(DataPos)),
			     NewHead2 = free(F, NewHead, Pos2, Size2+12),
			     ?DEBUGF("~p:Prev == ~p~n",[?LINE, Prev]),
			     NewHead3 = cache_replace(NewHead2, Key, [Object]),
			     {NewHead3,0};
			 {ok, Prev, Pos2, Next, Size2, Term} ->
			     ok = file:pwrite(F, Pos2+12, Bin),
			     ?DEBUGF("~p:Prev == ~p~n",[?LINE, Prev]),
			     NewHead = cache_replace(Head, Key, [Object]),
			     {NewHead,0}
		     end;
		 Head#head.type == bag ->
		     case search_key(F, Pos, Chain, Key, Kp) of
			 %% The KEY does not exist, we kan insert 
			 %% the new object at head of chain. 
			 {no, _} ->
			     {NewHead,DataPos} = 
				 alloc(Head, 12 + Size),
			     ok = file:pwrite(F, DataPos, 
					      [?int32(Chain), 
					       ?int32(Size), 
					       ?ACTIVE_AS_LIST, 
					       Bin]),
			     ok = file:pwrite(F, Pos, 
					      ?int32(DataPos)),
			     NewHead2 = maybe_add_to_cache(NewHead, 
							   Key, 
							   Object),
			     {NewHead2,1};
			 %% The key exists, but does the object?
			 {ok, Prev, Pos2, Next, Size2, Term} ->
			     case search_object(F, Prev, Pos2, 
						Key, Object) of
				 %% The object is unique
				 %% insert it ahead of the first
				 %% WITH SAME KEY!
				 {no, _} -> 
				     {NewHead,DataPos} = 
					 alloc(Head, 12 + Size),
				     ok = file:pwrite(
					    F, DataPos, 
					    [?int32(Pos2),
					     ?int32(Size),
					     ?ACTIVE_AS_LIST,
					     Bin]),
				     ok = file:pwrite(
					    F, 
					    Prev, 
					    ?int32(DataPos)),
				     NewHead2 = maybe_add_to_cache(
						  NewHead, 
						  Key, 
						  Object),
				     {NewHead2,1};
				 %% The object already exists,
				 %% No action is taken.
				 _ -> 
				     {Head,0}
			     end
		     end;
		 Head#head.type == duplicate_bag ->
		     {NewHead,DataPos} = alloc(Head, 12 + Size),
		     case search_key(F, Pos, Chain, Key, Kp) of
			 {no, _} ->  %% insert new object at head of list
			     ok = file:pwrite(F, DataPos, [?int32(Chain), 
							   ?int32(Size), 
							   ?ACTIVE_AS_LIST, Bin]),
			     ok = file:pwrite(F, Pos, ?int32(DataPos)), 
			     NewHead2 = maybe_add_to_cache(NewHead, Key, 
							   Object),
			     {NewHead2,1};
			 {ok, Prev, Pos2, Next, Size2, Term} -> %% link in object
			     ok = file:pwrite(F, DataPos, [?int32(Pos2),
							   ?int32(Size),
							   ?ACTIVE_AS_LIST, Bin]),
			     ok = file:pwrite(F, Prev, ?int32(DataPos)),
			     NewHead2 = maybe_add_to_cache(NewHead, Key, 
							   Object),
			     {NewHead2,1}
		     end

	     end,
    H2 = H1#head{no_items = H1#head.no_items + I},
    if
	(H2#head.no_items > H2#head.next) ->
	    if (H2#head.fixed == false)->
		    grow(H2);
	       true ->
		    H2
	    end;
	true ->
	    H2
    end.




h(I,HF) -> erlang:HF(I, ?BIG) - 1.  %% stupid BIF has 1 counts.

db_hash(Key, Head) ->
    H = h(Key,Head#head.hash_bif),
    Hash = H rem Head#head.m,
    if
	Hash < Head#head.n ->
	    H rem (2 * Head#head.m);
	true ->
	    Hash
    end.


ensure_alloced(Head) ->
    Next = Head#head.next,
    if 
	(Next >= (?SEGSZ * ?SEGARRSZ)) -> %% can't grow no more
	    {Head,no};
	(Next rem ?SEGSZ) == 0 ->  %% alloc new segment
	    ?TRACE("Alloc new segment \n ", []),
	    Nseg = Next div ?SEGSZ,
	    F = Head#head.fptr,
	    {NewHead,Segm} = alloc(Head, 4 * ?SEGSZ),
	    {ok, _} = file:position(F, Segm),
	    zero(F, ?SEGSZ),
	    ok = file:pwrite(F,  ?HEADSZ + (4 * Nseg),  ?int32(Segm)),
	    {NewHead,Next + 1};
	true ->
	    {Head,Next + 1}
    end.


grow(Head) ->
    F = Head#head.fptr,
    %% First ensure that space in the file is allocated
    case ensure_alloced(Head) of
	{NewHead,no} -> %% no more growth .... fill buckets instead
	    NewHead;
	{NewHead,Next} ->
	    N = NewHead#head.n,
	    {Pos, Chain} = chain(NewHead, N),
	    Kp = NewHead#head.keypos,
	    re_hash_chain(NewHead, Pos, Chain, Kp),
	    N2 = N + 1,
	    if
		N2 == NewHead#head.m ->
		    NewHead#head{n = 0, next = Next, m = 2 * Head#head.m};
		true ->
		    NewHead#head{next = Next, n = N2}
	    end
    end.

re_hash_chain(H2, Prev, 0,_) ->
    done;
re_hash_chain(H2, Prev, Chain, Kp) -> 
    F = H2#head.fptr,
    case prterm(F, Chain) of
	{ok, Next, Size, Term} ->
	    Key = element(Kp, Term),
	    New = h(Key,H2#head.hash_bif) rem (2 * H2#head.m),
	    if
		New == (H2#head.n) ->
		    %% object remains in this chain
		    ?TRACE("Letting ~w remain in ~w~n", [Term, New]),
		    re_hash_chain(H2, Chain, Next, Kp);

		true -> %% need to relink this object
		    ?TRACE("Move ~w from ~w to ~w ~n", [Term,H2#head.n,
							New]),
		    ok = file:pwrite(F, Prev, ?int32(Next)),  %% unlinked
		    {Pos2, Ch2} = chain(H2, New),
		    Old = pread_4(F, Pos2),

		    %%set new chain to point to this obj
		    ok = file:pwrite(F, Pos2, ?int32(Chain)), 

		    %% now set this obj to point to what new chain pointed to
		    ok = file:pwrite(F, Chain, ?int32(Old)),

		    re_hash_chain(H2, Prev, Next, Kp)
	    end;
	no ->
	    done
    end.


zero(F, I) ->
    ok = file:write(F, make_zeros(4*I)).

make_zeros(0) -> [];
make_zeros(N) when N rem 2 == 0 ->
    P = make_zeros(N div 2),
    [P|P];
make_zeros(N) ->
    P = make_zeros(N div 2),
    [0,P|P].

%% Read term from file at position Pos
prterm(F, Pos) ->
    case catch prterm2(F, Pos) of
	{'EXIT', Reason} -> %% truncated DAT file 
	    vformat("** dets: Corrupted or Truncated dets file ~p\n", [Reason]), 
	    {dets_error,Reason};
	Other -> 
	    Other
    end.

prterm2(F, Pos) ->
    ReadAhead = 512,
    Res = file:pread(F, Pos, 8+4+ReadAhead),
    ?DEBUGF("file:pread(~p, ~p, 8) -> ~p~n", [F, Pos, Res]),
    {ok, B} = Res,
    Next = i32(binary_to_list(B, 1, 4)),
    Sz = i32(binary_to_list(B, 5, 8)),
    ?DEBUGF("{Next, Sz} = ~p~n",[{Next, Sz}]),
    {_, Bin0} = split_binary(B, 12),
    Bin = case size(Bin0) of
	      Actual when Actual >= Sz ->
		  Bin0;
	      Actual ->
		  {ok, Bin1} = file:pread(F, Pos + 12 + Actual, Sz-Actual),
		  list_to_binary([Bin0|Bin1])
	  end,
    Term = erlang:binary_to_term(Bin),
    {ok, Next, Sz, Term}.

%% Can't be used at the bucket level!!!!
%% Only when we go down a chain
rterm(F) ->
    case catch rterm2(F) of
	{'EXIT', Reason} -> %% truncated DAT file 
	    vformat("** dets: Corrupted or Truncated dets file ~p\n", []), 
	    {dets_error,Reason};
	Other -> 
	    Other
    end.

rterm2(F) ->
    {ok, B} = file:read(F, 8),
    {B1, B2} = split_binary(B, 4),
    {Next, Sz} = {i32(B1), i32(B2)},
    file:position(F, {cur, 4}), %% skip over the status field
    {ok, Bin} = file:read(F, Sz),
    Term = erlang:binary_to_term(Bin),
    {ok, Next, Sz, Term}.

i32(Int) when binary(Int) ->
    i32(binary_to_list(Int));

i32(Int)  when integer(Int) -> [(Int bsr 24) band 255,
				(Int bsr 16) band 255,
				(Int bsr  8) band 255,
				Int band 255];
i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

bin2ints(B) when size(B) == 0 ->
    [];
bin2ints(B) ->
    {B1, B2} = split_binary(B, 4),
    [i32(B1) | bin2ints(B2)].

%% Implement the ets match algorithm in erlang itself
%% return false | {true, Bindings}
%% Max 10 variables :-(

do_match(Obj, Pat) ->
    case do_match(Obj, Pat, {no,no,no,no,no,no,no,no,no,no}) of
	false -> false;
	{bs, Bs} -> {true, Bs}
    end.

fix_b(Bs) ->
    fix_b(Bs, 1, 1 + size(Bs)).

fix_b(_, I, I) -> [];
fix_b(Bs, Pos, Last) ->
    case element(Pos, Bs) of
	no -> fix_b(Bs, Pos+1, Last);
	Val -> [Val | fix_b(Bs, Pos+1, Last)]
    end.


add_binding(Pos, Bs, Obj) ->
    case catch setelement(Pos, Bs, Obj) of
	{'EXIT', _} ->
	    Bs2 = grow_tuple(Bs, size(Bs)),
	    add_binding(Pos, Bs2, Obj);
	Other -> 
	    Other
    end.

grow_tuple(Tup, Sz) ->
    L = tuple_to_list(Tup),
    L2 = lists:duplicate(Sz + 10, no),
    list_to_tuple(L ++ L2).

binding(Pos, Tup) ->
    case catch element(Pos, Tup) of
	{'EXIT', _} ->
	    Bs2 = grow_tuple(Tup, size(Tup)),
	    {no, Bs2};
	Other -> Other
    end.

do_match(X, X, Bs) -> 
    {bs, Bs};
do_match([H1|T1], [H2|T2], Bs) -> 
    case do_match(H1, H2, Bs) of
	{bs, Bs2} -> do_match(T1, T2, Bs2);
	false -> false
    end;
do_match(Tup1, Tup2, Bs) when tuple(Tup1),tuple(Tup2), 
                              size(Tup1) == size(Tup2) ->
    e_match(Tup1, Tup2, size(Tup1), Bs);


do_match(Obj, '_', Bs) -> {bs, Bs};
do_match(Obj, Pat, Bs) -> 
    case is_var(Pat) of
	{true, Pos} when integer(Pos) ->
	    case binding(Pos + 1, Bs) of
		no ->
		    {bs, add_binding(Pos + 1, Bs, Obj)};
		{no, Bs2} ->
		    {bs, add_binding(Pos + 1, Bs2, Obj)};
		Obj ->
		    {bs, Bs};
		_ ->
		    false
	    end;
	{true, wild} ->
	    {bs, Bs};
	false ->
	    false
    end.

e_match(_, _, 0, Bs) -> {bs, Bs};
e_match(T1, T2, Pos, Bs) ->
    case do_match(element(Pos, T1), element(Pos, T2), Bs) of
	{bs, Bs2} -> e_match(T1, T2, Pos-1, Bs2);
	false -> false
    end.

is_var(X) when atom(X) ->
    case atom_to_list(X) of
	[$$, Dig] when $0 =< Dig, Dig =< $9 -> {true, Dig - $0};
	[$_] -> {true, wild};
	[$$ , Dig | Tail] -> accumulate_digs(lists:reverse([Dig |Tail]), 0,1);
	_ -> false
    end;
is_var(_) -> false.


has_var(X) when atom(X) -> 
    is_var(X);
has_var(X) when tuple(X) ->
    e_has_var(X, size(X));
has_var([H|T]) ->
    case has_var(H) of
	false -> has_var(T);
	Other -> Other
    end;
has_var(_) -> false.

e_has_var(X, 0) -> false;
e_has_var(X, Pos) ->
    case has_var(element(Pos, X))of
	false -> e_has_var(X, Pos-1);
	Other -> Other
    end.

accumulate_digs([], Ack, _) -> {true, Ack};
accumulate_digs([Dig|T], Ack, Pow) when $0 =< Dig, Dig =< $9 ->
    accumulate_digs(T, Ack + (Dig - $0) * Pow, Pow * 10);
accumulate_digs(_,_,_) -> false.


%%%%%%%%%%  server code %%%%%%%%%%%

start() ->
    case whereis(?MODULE) of
	undefined ->
	    register(?MODULE, spawn(?MODULE, loop0, []));
	Pid ->
	    Pid
    end,
    started.

start_link() ->
    case whereis(?MODULE) of
	undefined ->
	    register(?MODULE, spawn_link(?MODULE, loop0, []));
	Pid -> Pid
    end,
    started.

istart_link() ->  
    {ok, register(?MODULE, proc_lib:spawn_link(?MODULE, init, [self()]))}.

stop() ->
    case whereis(?MODULE) of
	undefined ->
	    stopped;
	Pid ->
	    req(?MODULE, stop)
    end.

-define(T, dets_registry).

verbose_flag() ->
    case init:get_argument(dets) of
	{ok, Args} ->
	    lists:member(["verbose"], Args);
	_ ->
	    false
    end.

init() ->
    set_verbose(verbose_flag()),
    process_flag(trap_exit, true),
    ets:new(?T, [set, named_table]),
    ets:new(?MODULE, [duplicate_bag]).

init(Parent) ->
    Store = init(),
    server_loop(#state{store=Store, parent=Parent}).

loop0() ->
    Store = init(),
    server_loop(#state{store=Store}).


server_loop(S) ->
    Store = S#state.store,
    receive
	{From, {open, Tab, Fname, Type, Keypos, Rep, Est, RamBool, CacheSz, Auto, Acc}} ->
	    case ets:lookup(?T, Tab) of
		[] -> 
		    Pid = spawn(?MODULE, do_open_file, 
				[Tab, Fname, Type, 
				 Keypos, Rep, get(verbose),
				 Est, RamBool, CacheSz, Auto, Acc]),
		    receive
			{Pid, {ok, Result}} ->
			    do_link(Store, From),
			    ets:insert(Store, {From, Tab}),
			    ets:insert(?T, {Tab, 1, Pid}),
			    From ! {?MODULE, {ok, Result}};
			{Pid, {error, Reason}} ->
			    From ! {?MODULE, {error, Reason}}
		    end;
		[{Tab, Counter, Pid}] ->
		    Pid ! {self(), {add_user, Tab, Fname, Type, Keypos, Rep, RamBool, CacheSz, Auto, Acc}},
		    receive
			{Pid, {ok, Result}} ->
			    do_link(Store, From),
			    ets:insert(Store, {From, Tab}),
			    ets:update_counter(?T, Tab, 1),
			    From ! {?MODULE, {ok, Result}};
			{Pid, {error, Reason}} ->
			    From ! {?MODULE, {error, Reason}}
		    end

	    end;
	{From, {open, File}} ->
	    Pid = spawn(?MODULE, do_open_file, [File, get(verbose)]),
	    receive
		{Pid, {ok, Tab}} ->
		    do_link(Store, From),
		    ets:insert(Store, {From, Tab}),
		    ets:insert(?T, {Tab, 1, Pid}),
		    From ! {?MODULE, {ok, Tab}};
		{Pid, {error, Reason}} ->
		    From ! {?MODULE, {error, Reason}}
	    end;
	{From, {close, Tab}} ->
	    Res = handle_close(S, From, Tab),
	    From ! {?MODULE, Res};

	{'EXIT', From, _} ->
	    %% First we need to figure out which tables that
	    %% From are using
	    All = ets:lookup(Store, From),
	    handle_all(S, All);

	{From, stop} ->
	    All = ets:tab2list(Store),
	    lists:foreach(fun({{links, _}, _}) -> 
				  ignore;
			     ({Pid, Tab}) -> 
				  handle_close(S, Pid, Tab)
			  end, All),
	    From ! {?MODULE, stopped},
	    [] = ets:tab2list(Store),  %% assertion
	    exit(normal);
	{From, {set_verbose , What}} ->
	    set_verbose(true);
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, S#state.parent, ?MODULE, [], S);
	Other ->
	    ok % maybe we ought to log this, which is not expected to occur
    end,
    server_loop(S).


handle_close(S, From, Tab) ->
    Store = S#state.store,
    case ets:match_object(Store, {From, Tab}) of
	[] -> 
	    %%io:format("DETS: Table ~w close attempt by non-owner~w~n",
	    %%      [Tab, From]),
	    {error, not_owner};
	[_ | Keep] ->
	    case ets:lookup(?T, Tab) of
		[] -> 
		    {error, not_open};
		[{Tab, 1, Pid}] ->
		    do_unlink(Store, From),
		    ets:delete(?T, Tab),
		    ets:match_delete(Store, {From, Tab}),
		    Pid ! {self(), close},
		    receive {Pid, {closed, Res}} -> Res end;
		[{Tab, Counter, Pid}] ->
		    do_unlink(Store, From),
		    ets:match_delete(Store, {From, Tab}),
		    [ets:insert(Store, K) || K <- Keep],
		    ets:update_counter(?T, Tab, -1),
		    fixtable_server:table_closed(?MODULE, Tab, From),
		    ok
	    end
    end.

handle_all(S, []) ->
    done;
handle_all(S, [{From, Tab} | Tail]) ->
    handle_close(S, From, Tab),
    handle_all(S, Tail).


%% Links with counters
do_link(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[] ->
	    ets:insert(Store, {Key, 1}),
	    link(Pid);
	[{_, C}] ->
	    ets:delete(Store, Key),
	    ets:insert(Store, {Key, C+1})
    end.

do_unlink(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[{_, C}] when C > 1 ->
	    ets:delete(Store, Key),
	    ets:insert(Store, {Key, C-1}),
	    true;
	_ ->
	    ets:delete(Store, Key),
	    unlink(Pid)

    end.

do_open_file(Fname, Verbose) ->
    process_flag(trap_exit, true),
    case fopen(Fname) of
	{error, tooshort} ->
	    file:delete(Fname),
	    do_open_file(Fname, Verbose);
	{error, Reason} ->
	    ?MODULE ! {self(), err(open_file, Reason)},
	    exit(normal);
	{ok, Head, I} ->
	    ?MODULE ! {self(), {ok, I#info.name}},
	    maybe_put(verbose, Verbose),
	    open_file_loop(Head, I, 0)
    end.


do_open_file(Tab, Fname, Type, Kp, Rep, Verbose, Est, Ram, CacheSz, Auto, Acc)  ->
    process_flag(trap_exit, true),
    case fopen(Tab, Fname, Type, Kp, Rep, Est, Ram, CacheSz, Auto, Acc) of
	{error, tooshort} ->
	    file:delete(Fname),
	    do_open_file(Tab, Fname, Type, Kp, Rep, Verbose, Est, Ram, CacheSz, Auto, Acc);
	{error, Reason} ->
	    ?MODULE ! {self(), err(open_file, Reason)},
	    exit(normal);
	{ok, Head, I} ->
	    ?MODULE ! {self(), {ok, Tab}},
	    maybe_put(verbose, Verbose),
	    open_file_loop(Head, I, 0)
    end.

error_action(_, _, _, _, normal, N) ->  %% on close
    exit(normal);
error_action(H, I, From, Op, Reason, N)  ->
    vformat("dets (info=~p) file loop  "
	    "failed to perform ~p~n"
	    "Reason was: ~p~n",
	    [I, Op, Reason]),
    Operation =
	case Op of
	    {X, Y} -> X;
	    _ -> Op
	end,
    catch From ! {self(), err(Operation, Reason)},
    open_file_loop(H, I, N).  %% tail recursive call to continue anyway


maybe_put(_, undefined) ->
    ignore;
maybe_put(K, V) ->
    put(K, V).

%% This loop never dies due to crashes and such,
%% If for example a writeop fails, the call will
%% return error and the dets loop continue to operate

open_file_loop(Head, I, N) ->
    receive
	{From, Op} ->
	    case catch apply_op(Op, From, Head, I, N) of
		ok -> 
		    open_file_loop(Head, I, N);
		{'EXIT', Reason} ->
		    error_action(Head, I, From, Op, Reason, N);
		{N2, H2} ->
		    open_file_loop(H2, I, N2);
		H2 ->
		    open_file_loop(H2, I, N)
	    end
    end.

apply_op(Op, From, Head, I, N) ->
    case Op of
	{lookup, Key} ->
	    {H2, Res} = cache_fread(Head, Key),
	    From ! {self(), Res},
	    H2;
	{insert, Obj} when tuple(Obj), Head#head.update_mode == dirty ->
	    H2 = finsert(Head, Obj),
	    From ! {self(), ok},
	    {N + 1, H2};
	{add_user, Tab, Fname, Type, Keypos, _, Ram, _CacheSz, _Auto, Access} ->
	    Res = if
		      Tab == I#info.name,
		      Head#head.keypos == Keypos,
		      Head#head.type == Type,
		      I#info.ram_file == Ram,
		      I#info.access == Access,
		      Fname == I#info.filename ->
			  {ok, Tab};
		      true ->
			  err(open_file, incompatible)
		  end,
	    From ! {self(), Res},
	    ok;
	{match_object, Pat} ->
	    {H2, Res} = fmatch_object(Head, Pat, object),
	    From ! {self(), Res},
	    H2;
	{select, Pat} ->
	    {H2, Res} = fselect(Head, Pat),
	    From ! {self(), Res},
	    H2;
	{delete, Key} when Head#head.update_mode == dirty ->
	    H2 = fdelete(Head, Key),
	    From ! {self(), ok},
	    {N + 1, H2};
	{delete_object, Key} when Head#head.update_mode == dirty ->
	    {H2, Res} = fdelete_object(Head, Key),
	    From ! {self(), Res},
	    {N + 1, H2};
	close  ->
	    From ! {self(), {closed, fclose(Head, I)}},
	    exit(normal);
	first ->
	    From ! {self(), ffirst(Head)},
	    ok;
	{next, Key} ->
	    From ! {self(), fnext(Head, Key)},
	    ok;
	{match, Pat} ->
	    {H2, Res} = fmatch_object(Head, Pat, bindings),
	    From ! {self(), Res},
	    H2;
	{match_delete, Pat} when Head#head.update_mode == dirty ->
	    H2 = fmatch_delete(Head, Pat),
	    From ! {self(), ok},
	    {N + 1, H2};
	{slot, Slot} ->
	    From ! {self(), fslot(Head, Slot)},
	    ok;
	{traverse, F} ->
	    From ! {self(), ftraverse(Head, F)},
	    ok;
	{update_counter, Key, C} when Head#head.type == set, Head#head.update_mode == dirty ->
	    {R,H2} = case cache_fread(Head, Key) of
			 {NewHead, [O]} ->
			     Kp = NewHead#head.keypos,
			     case catch try_update_tuple(O, Kp, C) of
				 {'EXIT', _} ->
				     {err(update_counter, badarg), NewHead};
				 {New, Term2} ->
				     {New, finsert(NewHead,  Term2)}
			     end;
			 {NewHead, _} ->
			     {err(update_counter, badarg), NewHead}
		     end,
	    From ! {self(), R},
	    {N + 1, H2};
	info ->
	    From ! {self(), finfo(Head, I)},
	    ok;
	{info, Tag} ->
	    From ! {self(), finfo(Head, I, Tag)},
	    ok;
	sync ->
	    From ! {self(), perform_save(Head, I)},
	    {0, Head#head{update_mode = saved}};
	{set_verbose, What} ->
	    set_verbose(What), ok;
	{fixtable, true} ->
	    From ! {self(), ok},
	    Head#head{fixed = true};
	{fixtable, false} ->
	    From ! {self(), ok},
	    Head#head{fixed = false};
	
	auto_save ->
	    case Head#head.update_mode of
		saved ->
		    Head;
		dirty when N == 0 ->
		    %% The updates seems to have declined
		    vformat("** dets: Auto save of ~p\n", [I#info.name]), 
		    perform_save(Head, I),
		    {0, Head#head{update_mode = saved}};
		dirty -> 
		    %% Reset counter and try later
		    start_auto_save_timer(Head#head.auto_save),
		    {0, Head}
	    end;

	WriteOp when I#info.access == read_write,
		     Head#head.update_mode == saved ->
	    mark_dirty(Head#head.fptr),
	    start_auto_save_timer(Head#head.auto_save),
	    H2 = Head#head{update_mode = dirty},
            apply_op(WriteOp, From, H2, I, 0);
	    
	WriteOp when tuple(WriteOp), I#info.access == read ->
	    Tag = element(1, WriteOp),
	    From ! {self(), err(Tag, access_mode)},
	    ok
    end.

set_verbose(true) ->
    put(verbose, yes);
set_verbose(_) ->
    erase(verbose).

try_update_tuple(O, Kp, C) ->
    New = element(Kp+1, O) + C,
    {New, setelement(Kp+1, O, New)}.

start_auto_save_timer(infinity) ->
    ok;
start_auto_save_timer(Millis) ->
    erlang:send_after(Millis, self(), {self(), auto_save}).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, _, State) ->
    server_loop(State).

system_terminate(Reason, Parent, _, State) ->
    stop(),
    exit(Reason).

%%-----------------------------------------------------------------
%% Temporay code for upgrade.
%%-----------------------------------------------------------------
system_code_change(State, _Module, OldVsn, Extra) ->
    {ok, State}.



%%%%%  client functions %%%%

%% Assuming that a file already exists, open it with the
%% parameters as already specified in the file itself.
%% Return a ref leading to the file.
open_file(File) ->
    ensure_started(),
    req(?MODULE, {open, File}).

open_file(Tab, Args) ->     
    ensure_started(),
    case catch defaults(Tab, Args) of
	[{file, File}, 
	 {type, Type}, 
	 {keypos, KeyPos}, 
	 {repair, Rep}, 
	 {estimated_no_objects, Est}, 
	 {ram_file, Ram},
	 {cache_size, CacheSz},
	 {auto_save, Auto},
	 {access, Acc}] ->
	    req(?MODULE, {open, Tab, File, Type, KeyPos, Rep, Est, Ram, CacheSz, Auto, Acc});
	_ ->
	    err(open_file, badarg)
    end.

close(Tab) ->  
    req(?MODULE, {close, Tab}).

-define(proc(X), ets:lookup_element(?T, X, 3)).

lookup(Tab, Key) ->        req(?proc(Tab), {lookup, Key}).
insert(Tab, Obj) ->        req(?proc(Tab), {insert, Obj}).
sync(Tab) ->               req(?proc(Tab), sync).
match_object(Tab, Pat) ->  req(?proc(Tab), {match_object, Pat}).
select(Tab, Pat) ->        req(?proc(Tab), {select, Pat}).
match(Tab, Pat)       ->   req(?proc(Tab), {match, Pat}).
match_delete(Tab, Pat)  -> req(?proc(Tab), {match_delete, Pat}).
delete(Tab, Key) ->        req(?proc(Tab), {delete, Key}).
traverse(Tab, F) ->        req(?proc(Tab), {traverse, F}).
delete_object(Tab, O) ->   req(?proc(Tab), {delete_object, O}).
first(Tab) ->              req(?proc(Tab), first).
next(Tab, Key) ->          req(?proc(Tab), {next, Key}).
update_counter(Tab,Key,C)->req(?proc(Tab), {update_counter, Key, C}).
slot(Tab, Slot) ->         req(?proc(Tab), {slot, Slot}).
info(Tab) ->               req(?proc(Tab), info).
info(Tab, owner) ->        case (catch ?proc(Tab)) of
			       Pid when pid(Pid) ->
				   Pid;
			       _ ->
				   undefined
			   end;
info(Tab, safe_fixed) ->   fixtable_server:info(?MODULE, Tab);
info(Tab, fixed) ->        case (catch ?proc(Tab)) of
			       {'EXIT',Reason} ->
				   undefined;
			       Pid ->
				   req(Pid, {info, fixed})
			   end;
info(Tab, Tag) ->          req(?proc(Tab), {info, Tag}).
fixtable(Tab, Bool) ->     req(?proc(Tab), {fixtable, Bool}).
safe_fixtable(Tab, Bool) ->
    fixtable_server:safe_fixtable(?MODULE, Tab, Bool).
all() ->                   ensure_started(), 
			   lists:map(fun(X) -> element(1, X) end, 
				     ets:tab2list(?T)).


verbose() ->           
    verbose(true).
verbose(What) ->
    ensure_started(), 
    ?MODULE ! {self(), {set_verbose, What}},
    lists:map(fun(X) -> 
		      Pid = element(2, X),
		      Pid ! {self(), {set_verbose, What}}
	      end,
	      ets:tab2list(?T)).

req(Proc, R) -> 
    Proc ! {self(), R},
    receive 
	{Proc, Reply} -> 
	    Reply;
	{'EXIT', Proc, Reason} ->
	    exit(Reason)
    end.

ensure_started() ->
    case whereis(?MODULE) of
	undefined -> 
	    DetsServer = {dets, {dets, istart_link, []},
			  permanent, 2000, worker, [dets]},
	    supervisor:start_child(kernel_safe_sup,DetsServer);
	_ -> ok
    end.

%% Process the args list as provided to open_file/2
defaults(Tab, Args) ->
    Defaults = [{file, to_list(Tab)},
		{type, set},
		{keypos, 1},
		{repair, true}, 
		{estimated_no_objects, default},
		{ram_file, false},
		{cache_size, 0},
		{auto_save, timer:minutes(3)},
		{access, read_write}],
    Fun = fun repl/2,
    lists:foldl(Fun, Defaults, Args).

to_list(T) when atom(T) -> atom_to_list(T);
to_list(T) -> T.

positive_int_or_default(default) -> default;
positive_int_or_default(I) when I > 1 -> I.

repl({file, File}, Defs) ->
    lists:keyreplace(file, 1, Defs, {file, to_list(File)});
repl({type, T}, Defs) ->
    mem(T, [set, bag, duplicate_bag]),
    lists:keyreplace(type, 1, Defs, {type, T});
repl({keypos, P}, Defs) when integer(P) , P > 0 ->
    lists:keyreplace(keypos, 1, Defs, {keypos, P});
repl({repair, T}, Defs) ->
    mem(T, [true, false, force]),
    lists:keyreplace(repair, 1, Defs, {repair, T});
repl({ram_file, Bool}, Defs) ->
    mem(Bool, [true, false]),
    lists:keyreplace(ram_file, 1, Defs, {ram_file, Bool});
repl({cache_size, Int}, Defs) when integer(Int), Int >= 0 ->
    lists:keyreplace(cache_size, 1, Defs, {cache_size, Int});
repl({cache_size, infinity}, Defs) ->
    lists:keyreplace(cache_size, 1, Defs, {cache_size, infinity});
repl({auto_save, Int}, Defs) when integer(Int), Int >= 0 ->
    lists:keyreplace(auto_save, 1, Defs, {auto_save, Int});
repl({auto_save, infinity}, Defs) ->
    lists:keyreplace(auto_save, 1, Defs, {auto_save, infinity});
repl({estimated_no_objects, I}, Defs)  ->
    positive_int_or_default(I),
    lists:keyreplace(estimated_no_objects, 1, Defs,{estimated_no_objects,I});
repl({access, A}, Defs) ->
    mem(A, [read, read_write]),
    lists:keyreplace(access, 1, Defs, {access, A});
repl({_, _}, _) ->
    exit(badarg).

mem(X, L) ->
    case lists:member(X, L) of
	true -> true;
	false -> exit(badarg)
    end.

file_info(F) ->
    case file:rawopen(F, {binary, read}) of
	{ok, Fd} ->
	    case read_head_fields(Fd) of
		{ok, FH} ->
		    if
			FH#fileheader.closed_properly /= 1 ->
			    file:close(Fd),
			    {error, not_closed};
			FH#fileheader.cookie /= ?MAGIC ->
			    file:close(Fd),
			    {error, not_a_dets_file};
			FH#fileheader.version /= ?FILE_FORMAT_VERSION ->
			    file:close(Fd),
			    {error, bad_version};
			true ->
			    file:close(Fd),
			    ok
		    end;
		Other ->
		    file:close(Fd),
		    Other
	    end;
	Other ->
	    Other
    end.


%%%%%%%%%%%%%%%%%  DEBUG functions %%%%%%%%%%%%%%%%

%% debug fun to inspect position Pos in an open file
inspect_chain(H, Pos) ->
    F = H#head.fptr,
    file:position(F, Pos),
    case read_4(F) of
	0 -> 
	    0;
	I ->
	    case read_8(F) of
		{Sz, ?FREE} -> 
		    {ok, Bin} = file:read(F, Sz),
		    {free, {next, I}, {obj, catch term_to_binary(Bin)}};
		{Sz, ?ACTIVE} ->
		    {ok, Bin} = file:read(F, Sz),
		    {active, {next, I}, {obj, catch term_to_binary(Bin)}};
		_ ->
		    not_an_object
	    end
    end.

%% Dump the contents of a DAT file to the tty
%% internal debug function which ignores the closed properly thingie
%% and just tries anyway

view(Fn) ->
    case file:open(Fn, [raw, binary, read]) of
	{ok, F} ->
	    {ok, H0, I, ExtraInfo} = read_head(F, Fn, read),
	    Ftab = init_freelist(H0, ExtraInfo),
	    H = H0#head{ets=Ftab},
	    v_free_list(H),
	    v_segments(H, 0),
	    file:close(F);
	X -> 
	    X
    end.

v_free_list(H) ->
    io:format("FREE LIST ...... \n",[]),
    io:format("~p~n",[H#head.ets]),
    io:format("END OF FREE LIST \n",[]).

v_segments(H, ?SEGARRSZ) ->
    done;
v_segments(H, SegNo) ->
    io:format("SEGMENT ~w ", [SegNo]),
    file:position(H#head.fptr, ?HEADSZ + (4 * SegNo)),
    Seg = read_4(H#head.fptr),
    io:format("At position ~w~n", [Seg]),
    if
	Seg == 0 ->
	    done;
	true ->
	    v_segment(H, SegNo, Seg, 0),
	    v_segments(H, SegNo+1)
    end.

v_segment(H, _, SegPos, ?SEGSZ) ->
    done;
v_segment(H, SegNo, SegPos, SegSlot) ->
    Slot = SegSlot + (SegNo * ?SEGSZ),
    file:position(H#head.fptr, SegPos + (4 * SegSlot)),
    Chain = read_4(H#head.fptr),
    if 
	Chain == 0 ->  %% don't print empty chains
	    true;
	true ->
	    io:format("   <~p>~p: [",[SegPos + (4 * SegSlot), Slot]),
	    print_chain(H, Chain)
    end,
    v_segment(H, SegNo, SegPos, SegSlot+1).

print_chain(H, 0) ->
    io:format("] \n", []);
print_chain(H, Pos) ->
    file:position(H#head.fptr, Pos),
    case catch rterm(H#head.fptr) of
	{ok, 0, Sz, Term} ->
	    io:format("<~p>~p] \n",[Pos, Term]);
	{ok, Next, Sz, Term} ->
	    io:format("<~p>~p, ", [Pos, Term]),
	    print_chain(H, Next);
	Other ->
	    io:format("ERROR ~p~n", [Other])
    end.

err(Op, {error, Reason}) -> 
    err(Op, Reason);
err(open_file, tooshort) ->  %% Bizzare special case
    {error, tooshort};
err(Op, Reason) ->
    case get(verbose) of
	yes -> 
	    error_logger:format("dets: ~w failed with ~w~n", [Op, Reason]),
	    {error, {Op, Reason}};
	undefined  ->
	    {error, {Op, Reason}}
    end.

vformat(F, As) ->
    case get(verbose) of
	yes -> error_logger:format(F, As);
	_ -> ok
    end.


%%%%%%%%%%%%%%% allocation routines %%%%%%%%%%%%%%
%%% Algorithm : We use a buddy system on each file. This is nicely described
%%%             In i.e. the last chapter of the first-grade text book 
%%%             Data structures and algorithms by Aho, Hopcroft and
%%%             Ullman. I think buddy systems were invented by Knuth, a long
%%%             time ago.

init_freelist(Head, {convert_freelist,Version}) ->
    %% This function converts the saved freelist of the form
    %% [{Slot1,Addr1},{Addr1,Addr2},...,{AddrN,0},{Slot2,Addr},...]
    %% i.e each slot is a linked list which ends with a 0.
    %% This is stored in a bplus_tree per Slot
    %% Each Slot is a position in a tuple

    F = Head#head.fptr,
    Ftab = init_alloc(), % create tuple with empty bplus_trees
    Pos = pread_4(F, ?FREELIST_POS),
    {0, Size, Status} = pread_12(F, Pos),
    {ok,  B} = file:pread(F, Pos+12, Size),

    FreeList = lists:reverse(erlang:binary_to_term(B)),
    init_slots_from_old_file(FreeList,Ftab);
init_freelist(Head, _) ->
    F = Head#head.fptr,
    Pos = pread_4(F, ?FREELIST_POS),
    {0, Size, Status} = pread_12(F, Pos),
    {ok,  B} = file:pread(F, Pos+12, Size),
    erlang:binary_to_term(B). % if we stored the bplus_tree as is

init_slots_from_old_file([{Slot,Addr}|T],Ftab) ->
    init_slot(Slot,[{Slot,Addr}|T],Ftab);
init_slots_from_old_file([],Ftab) ->
    Ftab.

init_slot(Slot,[],Ftab) ->
    Ftab; % should never happen
init_slot(Slot,[{Addr,0}|T],Ftab) ->
    init_slots_from_old_file(T,Ftab);
init_slot(Slot,[{Slot1,Addr}|T],Ftab) ->
    Stree = element(Slot,Ftab),
    %%    io:format("init_slot ~p:~p~n",[Slot,Addr]),
    init_slot(Slot,T,setelement(Slot, Ftab,
				bplus_insert(Stree,Addr))).

init_alloc0() ->
    Empty = bplus_empty_tree(),
    %% initiate a tuple with ?MAXBUD "Empty" elements
    erlang:make_tuple(?MAXBUD, Empty).

init_alloc() ->
    Ftab = init_alloc0(),
    Empty = bplus_empty_tree(),
    setelement(?MAXBUD, Ftab, bplus_insert(Empty,?BASE)). 

alloc(Head, Sz) ->
    Pos = sz2pos(Sz),
    Ftab = Head#head.ets,
    case bplus_lookup_first(element(Pos+1,Ftab)) of
	undefined -> %% Hard case gotta search upward
	    X = find_next_free(Head#head.ets, Pos+1),
	    NewFtab = move_down(Pos, X, Ftab),
	    alloc(Head#head{ets=NewFtab}, Sz);
	{ok,Addr}->  %% At position Addr in the file, we have a block
	    NewSlotTab = bplus_delete(element(Pos+1,Ftab),Addr),
	    NewFtab = setelement(Pos+1,Ftab,NewSlotTab),
	    {Head#head{ets=NewFtab},Addr}
    end.

find_next_free(Ftab, Pos) ->
    case bplus_lookup_first(element(Pos+1,Ftab)) of  %% read free list
	undefined -> find_next_free(Ftab, Pos+1);
	{ok,Key} -> Pos
    end.

move_down(X, X, Ftab) ->
    Ftab;
move_down(Opos, SplitSlot, Ftab) ->
    Size = ?POW(SplitSlot),
    {ok,Addr} = bplus_lookup_first(element(SplitSlot+1,Ftab)),
    NewSlotTab = bplus_delete(element(SplitSlot+1,Ftab),Addr),
    NewSlotTabDown1 = bplus_insert(element(SplitSlot,Ftab),Addr),
    Half = (Addr + (Size bsr 1)),
    NewSlotTabDown2 = bplus_insert(NewSlotTabDown1,Half),
    NewFtab1 = setelement(SplitSlot+1,Ftab,NewSlotTab), 
    NewFtab2 = setelement(SplitSlot,NewFtab1,NewSlotTabDown2), 
    move_down(Opos, SplitSlot-1, NewFtab2).

sz2pos(Sz0) ->
    sz2pos(Sz0, 0).
sz2pos(Li, Pos) when Li > 0 ->
    sz2pos(Li bsr 1, Pos+1);
sz2pos(Li, Pos) ->
    Pos.

free(F, Head, Addr, Sz) ->
    ok = file:pwrite(F, Addr+8, ?FREE_AS_LIST),  %% set status field
    Ftab = Head#head.ets,
    Slot = sz2pos(Sz),
    Head#head{ets=free_in_slot(Ftab,Addr,Slot)}.

free_in_slot(Ftab, Addr, Slot) when Slot > ?MAXBUD ->
    Ftab;
free_in_slot(Ftab, Addr, Slot) ->
    SlotTree = element(Slot+1,Ftab),
    BuddyAddr = my_buddy(Addr,?POW(Slot)),
    MoveUpAddr = if 
		     BuddyAddr < Addr -> BuddyAddr;
		     true -> Addr
		 end,
    case bplus_lookup(SlotTree,BuddyAddr) of
	undefined -> % no buddy found
	    setelement(Slot+1,Ftab,
		       bplus_insert(SlotTree,Addr));
	{ok,BuddyAddr} -> % buddie found
	    SlotTree1 = bplus_delete(SlotTree,Addr),
	    SlotTree2 = bplus_delete(SlotTree1,BuddyAddr),
	    free_in_slot(setelement(Slot+1,Ftab,SlotTree2),MoveUpAddr,Slot+1)
    end.


%% Calculate the buddie to Addr
my_buddy(Addr,Sz) ->
    case ((Addr - ?BASE) div Sz) band 1 of
	0 -> % even , buddy is higher addr
	    Addr+Sz;
	1 -> % odd  , buddy is lower addr
	    Addr-Sz
    end.


%%%-----------------------------------------------------------------
%%% These functions implements a B+ tree.
%%% The code is originally written by lelle@erlang.ericsson.se,
%%%-----------------------------------------------------------------

-define(max_size, 16).
-define(min_size, 8).
%%-----------------------------------------------------------------
%% Finds out the type of the node: 'l' or 'n'.
%%-----------------------------------------------------------------
-define(NODE_TYPE(Tree), element(1, Tree)).
%% Finds out if a node/leaf is full or not.
-define(FULL(Tree), (bplus_get_size(Tree) >= ?max_size)).
%% Finds out if a node/leaf is filled up over its limit.
-define(OVER_FULL(Tree), (bplus_get_size(Tree) > ?max_size)).
%% Finds out if a node/leaf has less items then allowed.
-define(UNDER_FILLED(Tree), (bplus_get_size(Tree) < ?min_size)).
%% Finds out if a node/leaf has as few items as minimum allowed.
-define(LOW_FILLED(Tree), (bplus_get_size(Tree) =< ?min_size)).


%%-----------------------------------------------------------------
%% Func: empty_tree/0
%% Purpose: Creates a new empty tree.
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_empty_tree() -> v.

%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Looks for Key in the Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
bplus_lookup(v, Key) -> undefined;
bplus_lookup(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_lookup_leaf(Key, Tree);
	n ->
	    {_, SubTree} = bplus_select_sub_tree(Tree, Key),
	    bplus_lookup(SubTree, Key)
    end.

%%-----------------------------------------------------------------
%% Searches through a leaf until the Key is ok or
%% when it is determined that it does not exist.
%%-----------------------------------------------------------------
bplus_lookup_leaf(Key, Leaf) -> 
    bplus_lookup_leaf_2(Key, Leaf, bplus_get_size(Leaf)).

bplus_lookup_leaf_2(_, _, 0) -> undefined;
bplus_lookup_leaf_2(Key, Leaf, N) ->
    case bplus_get_leaf_key(Leaf, N) of
	Key -> {ok, Key};
	_ ->
	    bplus_lookup_leaf_2(Key, Leaf, N-1)
    end.

%%-----------------------------------------------------------------
%% Func: lookup_first/1
%% Purpose: Finds the smallest key in the entire Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
bplus_lookup_first(v) -> undefined;
bplus_lookup_first(Tree) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    % Then it is the leftmost key here.
	    {ok, bplus_get_leaf_key(Tree, 1)};         
	n ->
	    % Look in the leftmost subtree.
	    bplus_lookup_first(bplus_get_tree(Tree, 1))
    end.


%%-----------------------------------------------------------------
%% Func: insert/3
%% Purpose: Inserts a new {Key, Value} into the tree.
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_insert(v, Key) -> bplus_mk_leaf([Key]);
bplus_insert(Tree, Key) ->
    NewTree = bplus_insert_in(Tree, Key),
    case ?OVER_FULL(NewTree) of
	false ->
	    NewTree;
	% If the node is over-full the tree will grow.
	true ->
	    {LTree, DKey, RTree} = 
		case ?NODE_TYPE(NewTree) of
		    l ->
			bplus_split_leaf(NewTree);
		    n ->
			bplus_split_node(NewTree)
		end,
	    bplus_mk_node([LTree, DKey, RTree])
    end.

%%-----------------------------------------------------------------
%% Func: delete/2
%% Purpose: Deletes a key from the tree (if present).
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_delete(v, Key) -> v;
bplus_delete(Tree, Key) ->
    NewTree = bplus_delete_in(Tree, Key),
    S = bplus_get_size(NewTree),
    case ?NODE_TYPE(NewTree) of
	l ->
	    if
		S == 0 ->
		    v;
		true ->
		    NewTree
	    end;
	n ->
	    if
		S == 1 ->
		    bplus_get_tree(NewTree, 1);
		true ->
		    NewTree
	    end
    end.


%%% -----------------------
%%% Help function to insert.
%%% -----------------------

bplus_insert_in(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_insert_in_leaf(Tree, Key);
	n ->
	    {Pos, SubTree} = bplus_select_sub_tree(Tree, Key),  
            % Pos = "the position of the subtree".
	    NewSubTree = bplus_insert_in(SubTree, Key),
	    case ?OVER_FULL(NewSubTree) of
		false ->
		    bplus_put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case bplus_reorganize_tree_ins(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    bplus_put_subtree(bplus_put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    bplus_put_subtree(bplus_put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{split, {LeftT, DKey, RightT}} ->
			    bplus_extend_tree(Tree, {LeftT, DKey, RightT}, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Inserts a key in correct position in a leaf.
%%-----------------------------------------------------------------
bplus_insert_in_leaf(Leaf, Key) ->
    bplus_insert_in_leaf_2(Leaf, Key, bplus_get_size(Leaf), []).

bplus_insert_in_leaf_2(Leaf, Key, 0, Accum) ->
    bplus_insert_in_leaf_3(Leaf, 0, [Key|Accum]);
bplus_insert_in_leaf_2(Leaf, Key, N, Accum) ->
    K = bplus_get_leaf_key(Leaf, N),
    if
	Key < K ->
	    % Not here!
	    bplus_insert_in_leaf_2(Leaf, Key, N-1, [K|Accum]);
	K < Key ->
	    % Insert here.
	    bplus_insert_in_leaf_3(Leaf, N-1, [K, Key|Accum]);
	K == Key ->
	    % Replace (?).
	    bplus_insert_in_leaf_3(Leaf, N-1, [ Key|Accum])
    end.

bplus_insert_in_leaf_3(Leaf, 0, LeafList) ->
    bplus_mk_leaf(LeafList);
bplus_insert_in_leaf_3(Leaf, N, LeafList) ->
    bplus_insert_in_leaf_3(Leaf, N-1, [bplus_get_leaf_key(Leaf, N)|LeafList]).


%%% -------------------------
%%% Help functions for delete.
%%% -------------------------

bplus_delete_in(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_delete_in_leaf(Tree, Key);
	n ->
	    {Pos, SubTree} = bplus_select_sub_tree(Tree, Key),  
	    % Pos = "the position of the subtree".
	    NewSubTree = bplus_delete_in(SubTree, Key),
	    % Check if it has become to small now
	    case ?UNDER_FILLED(NewSubTree) of
		false ->
		    bplus_put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case bplus_reorganize_tree_del(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    bplus_put_subtree(bplus_put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    bplus_put_subtree(bplus_put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{join_left, JoinedTree} ->
			    bplus_joinleft_tree(Tree, JoinedTree, Pos);
			{join_right, JoinedTree} ->
			    bplus_joinright_tree(Tree, JoinedTree, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Deletes a key from the leaf returning a new (smaller) leaf.
%%-----------------------------------------------------------------
bplus_delete_in_leaf(Leaf, Key) ->
    bplus_delete_in_leaf_2(Leaf, Key, bplus_get_size(Leaf), []).

bplus_delete_in_leaf_2(Leaf, _, 0, _) -> Leaf;
bplus_delete_in_leaf_2(Leaf, Key, N, Accum) ->
    K = bplus_get_leaf_key(Leaf, N),
    if
	Key == K ->
            % Remove this one!
	    bplus_delete_in_leaf_3(Leaf, N-1, Accum);
	true ->
	    bplus_delete_in_leaf_2(Leaf, Key, N-1, [K|Accum])
    end.

bplus_delete_in_leaf_3(Leaf, 0, LeafList) ->
    bplus_mk_leaf(LeafList);
bplus_delete_in_leaf_3(Leaf, N, LeafList) ->
    bplus_delete_in_leaf_3(Leaf, N-1, [bplus_get_leaf_key(Leaf, N)|LeafList]).



%%-----------------------------------------------------------------
%% Selects and returns which subtree the search should continue in.
%%-----------------------------------------------------------------
bplus_select_sub_tree(Tree, Key) ->
    bplus_select_sub_tree_2(Tree, Key, bplus_get_size(Tree)).

bplus_select_sub_tree_2(Tree, Key, 1) -> {1, bplus_get_tree(Tree, 1)};
bplus_select_sub_tree_2(Tree, Key, N) ->
    K = bplus_get_lkey(Tree, N),
    if
	K > Key ->
	    bplus_select_sub_tree_2(Tree, Key, N-1);
	K =< Key ->
            % Here it is!
	    {N, bplus_get_tree(Tree, N)}
    end.

%%-----------------------------------------------------------------
%% Selects which brother that should take over some of our items.
%% Or if they are both full makes a split.
%%-----------------------------------------------------------------
bplus_reorganize_tree_ins(Tree, NewSubTree, 1) ->
    RTree = bplus_get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case ?FULL(RTree) of
	false ->
	    bplus_reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is full, we must split this one!
	    bplus_reorganize_tree_s(Tree, NewSubTree, 1)
    end;
bplus_reorganize_tree_ins(Tree, NewSubTree, Pos) ->
    Size = bplus_get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = bplus_get_tree(Tree, Pos-1),
 	    case ?FULL(LTree) of
		false ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    % It is full, we must split this one!
		    bplus_reorganize_tree_s(Tree, NewSubTree, Pos)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    RTree = bplus_get_tree(Tree, Pos+1),
	    SL = bplus_get_size(LTree),
	    SR = bplus_get_size(RTree),
	    if
		SL > SR ->
		    bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		SL < SR ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    case ?FULL(LTree) of
			false ->
			    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    bplus_reorganize_tree_s(Tree, NewSubTree, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% This function fills over items from brothers to maintain the minimum
%% number of items per node/leaf.
%%-----------------------------------------------------------------
bplus_reorganize_tree_del(Tree, NewSubTree, 1) ->
    % The case when Pos is at leftmost position.
    RTree = bplus_get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case ?LOW_FILLED(RTree) of
	false ->
	    bplus_reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is to small, we must join them!
	    bplus_reorganize_tree_jr(Tree, NewSubTree, 1, RTree)
    end;
bplus_reorganize_tree_del(Tree, NewSubTree, Pos) ->
    Size = bplus_get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    case ?LOW_FILLED(LTree) of
		false ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
                    % It is to small, we must join this one!
		    bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    RTree = bplus_get_tree(Tree, Pos+1),
	    SL = bplus_get_size(LTree),
	    SR = bplus_get_size(RTree),
	    if
		SL>SR ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		SL < SR ->
		    bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		true ->
		    case ?LOW_FILLED(LTree) of
			false ->
			    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
		    end
	    end
    end.


bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {left, bplus_split_leaf(
		     bplus_mk_leaf(
		       lists:append(bplus_leaf_to_list(LTree),
				    bplus_leaf_to_list(NewSubTree))))};
	n ->
	    {left, bplus_split_node(
		     bplus_mk_node(
		       lists:append([bplus_node_to_list(LTree),
				     [bplus_get_lkey(Tree, Pos)],
				     bplus_node_to_list(NewSubTree)])))}
    end.

bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {right, bplus_split_leaf(bplus_mk_leaf(lists:append([bplus_leaf_to_list(NewSubTree),
						    bplus_leaf_to_list(RTree)])))};
	n ->
	    {right, bplus_split_node(bplus_mk_node(lists:append([bplus_node_to_list(NewSubTree),
						    [bplus_get_rkey(Tree, Pos)],
						    bplus_node_to_list(RTree)])))}
    end.

bplus_reorganize_tree_s(Tree, NewSubTree, Pos) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {split, bplus_split_leaf(NewSubTree)};
	n ->
	    {split, bplus_split_node(NewSubTree)}
    end.

bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {join_left, bplus_mk_leaf(lists:append([bplus_leaf_to_list(LTree),
					     bplus_leaf_to_list(NewSubTree)]))};
	n ->
	    {join_left, bplus_mk_node(lists:append([bplus_node_to_list(LTree),
					     [bplus_get_lkey(Tree, Pos)],
					     bplus_node_to_list(NewSubTree)]))}
    end.

bplus_reorganize_tree_jr(Tree, NewSubTree, Pos, RTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {join_right, bplus_mk_leaf(lists:append([bplus_leaf_to_list(NewSubTree),
					      bplus_leaf_to_list(RTree)]))};
	n ->
	    {join_right, bplus_mk_node(lists:append([bplus_node_to_list(NewSubTree),
					      [bplus_get_rkey(Tree, Pos)],
					      bplus_node_to_list(RTree)]))}
    end.


%%-----------------------------------------------------------------
%% Takes a leaf and divides it into two equal big leaves.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
bplus_split_leaf(Leaf) ->
    S = bplus_get_size(Leaf),
    bplus_split_leaf_2(Leaf, S, S div 2, []).

bplus_split_leaf_2(Leaf, Pos, 1, Accum) -> 
    K = bplus_get_leaf_key(Leaf, Pos),
    bplus_split_leaf_3(Leaf, Pos-1, [], K, [K|Accum]);
bplus_split_leaf_2(Leaf, Pos, N, Accum) ->
    bplus_split_leaf_2(Leaf, Pos-1, N-1, [bplus_get_leaf_key(Leaf, Pos)|Accum]).

bplus_split_leaf_3(_, 0, LeftAcc, DKey, RightAcc) ->
    {bplus_mk_leaf(LeftAcc), DKey, bplus_mk_leaf(RightAcc)};
bplus_split_leaf_3(Leaf, Pos, LeftAcc, DKey, RightAcc) ->
    bplus_split_leaf_3(Leaf, Pos-1, [bplus_get_leaf_key(Leaf, Pos)|LeftAcc],
		 DKey, RightAcc).

%%-----------------------------------------------------------------
%% Takes a node and divides it into two equal big nodes.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
bplus_split_node(Node) ->
    S = bplus_get_size(Node),
    bplus_split_node_2(Node, S, S div 2, []).

bplus_split_node_2(Node, Pos, 1, Accum) ->
    bplus_split_node_3(Node, Pos-1, [], bplus_get_lkey(Node, Pos),
		 [bplus_get_tree(Node, Pos)|Accum]);
bplus_split_node_2(Node, Pos, N, Accum) ->
    bplus_split_node_2(Node, Pos-1, N-1, [bplus_get_lkey(Node, Pos),
				    bplus_get_tree(Node, Pos)|Accum]).

bplus_split_node_3(Node, 1, LeftAcc, DKey, RightAcc) ->
    {bplus_mk_node([bplus_get_tree(Node, 1)|LeftAcc]), DKey, bplus_mk_node(RightAcc)};
bplus_split_node_3(Node, Pos, LeftAcc, DKey, RightAcc) ->
    bplus_split_node_3(Node, Pos-1,
		 [bplus_get_lkey(Node, Pos), bplus_get_tree(Node, Pos)|LeftAcc],
		 DKey, RightAcc).

%%-----------------------------------------------------------------
%% Inserts a joined tree insted of the old one at position Pos and
%% the one nearest left/right brother.
%%-----------------------------------------------------------------
bplus_joinleft_tree(Tree, JoinedTree, Pos) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos, bplus_get_size(Tree), []).
bplus_joinright_tree(Tree, JoinedTree, Pos) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos+1, bplus_get_size(Tree), []).

bplus_join_tree_2(Tree, JoinedTree, Pos, Pos, Accum) ->
    bplus_join_tree_3(Tree, Pos-2, [JoinedTree|Accum]);
bplus_join_tree_2(Tree, JoinedTree, Pos, N, Accum) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos, N-1,
		[bplus_get_lkey(Tree, N), bplus_get_tree(Tree, N)|Accum]).

bplus_join_tree_3(Tree, 0, Accum) -> bplus_mk_node(Accum);
bplus_join_tree_3(Tree, Pos, Accum) ->
    bplus_join_tree_3(Tree, Pos-1, [bplus_get_tree(Tree, Pos), bplus_get_rkey(Tree, Pos)|Accum]).

%%% ---------------------------------
%%% Primitive datastructure functions.
%%% ---------------------------------

%%-----------------------------------------------------------------
%% Constructs a node out of list format.
%%-----------------------------------------------------------------
bplus_mk_node(NodeList) -> list_to_tuple([ n |NodeList]).

%%-----------------------------------------------------------------
%% Converts the node into list format.
%%-----------------------------------------------------------------
bplus_node_to_list(Node) ->
    [_|NodeList] = tuple_to_list(Node),
    NodeList.

%%-----------------------------------------------------------------
%% Constructs a leaf out of list format.
%%-----------------------------------------------------------------
bplus_mk_leaf(KeyList) -> list_to_tuple([l|KeyList]).

%%-----------------------------------------------------------------
%% Converts a leaf into list format.
%%-----------------------------------------------------------------
bplus_leaf_to_list(Leaf) ->
    [_|LeafList] = tuple_to_list(Leaf),
    LeafList.



%%-----------------------------------------------------------------
%% Changes subtree "pointers" in a node.
%%-----------------------------------------------------------------
bplus_put_subtree(Tree, []) -> Tree;
bplus_put_subtree(Tree, [NewSubTree, Pos|Rest]) ->
    bplus_put_subtree(setelement(Pos*2, Tree, NewSubTree), Rest).

%%-----------------------------------------------------------------
%% Replaces the tree at position Pos with two new trees.
%%-----------------------------------------------------------------
bplus_extend_tree(Tree, Inserts, Pos) ->
    bplus_extend_tree_2(Tree, Inserts, Pos, bplus_get_size(Tree), []).

bplus_extend_tree_2(Tree, {T1, DKey, T2}, Pos, Pos, Accum) ->
    bplus_extend_tree_3(Tree, Pos-1, [T1, DKey, T2|Accum]);
bplus_extend_tree_2(Tree, Inserts, Pos, N, Accum) ->
    bplus_extend_tree_2(Tree, Inserts, Pos, N-1,
		  [bplus_get_lkey(Tree, N), bplus_get_tree(Tree, N)|Accum]).

bplus_extend_tree_3(_, 0, Accum) -> bplus_mk_node(Accum);
bplus_extend_tree_3(Tree, N, Accum) ->
    bplus_extend_tree_3(Tree, N-1, [bplus_get_tree(Tree, N), bplus_get_rkey(Tree, N)|Accum]).

%%-----------------------------------------------------------------
%% Changes the dividing key between two trees.
%%-----------------------------------------------------------------
bplus_put_lkey(Tree, DKey, Pos) -> setelement(Pos*2-1, Tree, DKey).
bplus_put_rkey(Tree, DKey, Pos) -> setelement(Pos*2+1, Tree, DKey).


%%-----------------------------------------------------------------
%% Calculates the number of items in a node/leaf.
%%-----------------------------------------------------------------
bplus_get_size(Tree) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    size(Tree)-1;
	n ->
	    size(Tree) div 2
    end.

%%-----------------------------------------------------------------
%% Returns a tree at position Pos from an internal node.
%%-----------------------------------------------------------------
bplus_get_tree(Tree, Pos) -> element(Pos*2, Tree).

%%-----------------------------------------------------------------
%% Returns a key in a leaf at position Pos.
%%-----------------------------------------------------------------
bplus_get_leaf_key(Leaf, Pos) -> element(Pos+1, Leaf).

%%-----------------------------------------------------------------
%% Returns dividing keys, left of or right of a tree.
%%-----------------------------------------------------------------
bplus_get_lkey(Tree, Pos) -> element(Pos*2-1, Tree).
bplus_get_rkey(Tree, Pos) -> element(Pos*2+1, Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cache management
%% 

-record(cache, {
	  store,         %% Each element is a {Key, Timestamp, Recs} tuple
	  max_size,      %% Integer | infinity
	  current_size,  %% Integer 
	  last_timestamp,%% Integer
	  victims}).     %% Each element is a {Timestamp, Key}

%% Initiate the cache  
cache_init(CacheSz) when CacheSz == 0 ->
    #cache{max_size = CacheSz};
cache_init(CacheSz) when CacheSz == infinity ->
    #cache{store = ets:new(dets_cache, [set]),
	   max_size = CacheSz};
cache_init(CacheSz) ->
    #cache{store = ets:new(dets_cache, [set]),
	   max_size = CacheSz,
	   current_size = 0,
	   last_timestamp= 0,
	   victims = ets:new(dets_victims, [ordered_set])}.

%% Try a lookup in the cache and administer the cache
%% in orwhen 
%% some bookeeping 
cache_fread(Head, Key) ->
    C = Head#head.cache,
    case C#cache.max_size == 0 of
	true ->
	    %% Cache is disabled, read directly from disk
	    Recs = fread(Head, Key),
	    {Head, Recs};
	false ->
	    case ets:lookup(C#cache.store, Key) of
		[] ->
		    %% A cache miss, time to read from disk
		    cache_fread_from_disk(Head, Key);
		[{_, OldT, Recs}] ->
		    %% Nice, a cache hit!
		    case C#cache.max_size == infinity of
			true ->
			    %% Unlimited cache, no need for timestamp admin
			    {Head, Recs};
			false ->
			    ets:delete(C#cache.victims, OldT),
			    NewT = C#cache.last_timestamp + 1,
			    DiffT = NewT - OldT,
			    ets:insert(C#cache.victims, {NewT, Key}),
			    ets:update_counter(C#cache.store, Key, DiffT),
			    C2 = C#cache{last_timestamp = NewT},
			    {Head#head{cache = C2}, Recs}
		    end
	    end
    end.

%% Add to cached list if already in cache
maybe_add_to_cache(Head,Key,Object) ->
    C = Head#head.cache,
    case C#cache.max_size of
	0 ->
	    Head;
	Siz ->
	    case ets:lookup(C#cache.store, Key) of
		[] ->
		    Head;
		[{Key, OldT, Odata}]  ->
		    case Siz of
			infinity ->
			    ets:insert(C#cache.store, 
				       {Key, 0, [Object | Odata]}),
			    Head;
			_ ->
			    NewT = C#cache.last_timestamp + 1, 
			    ets:delete(C#cache.victims, OldT),
			    ets:insert(C#cache.victims, {NewT, Key}),
			    ets:insert(C#cache.store, {Key, NewT, 
						       [Object | Odata]}),
			    NewC = C#cache{last_timestamp = NewT},
			    Head#head{cache = NewC}
		    end
	    end
    end.
			    

cache_insert(Head, Key, Recs) ->
    C = Head#head.cache,
    case C#cache.max_size == infinity of
	true ->
	    %% Unlimited cache, no need for timestamp admin
	    ets:insert(C#cache.store, {Key, 0, Recs}),
	    Head;
	false ->
	    C2 = cache_opt_reclaim(C),
	    NewT = C2#cache.last_timestamp + 1,
	    NewSize = C2#cache.current_size + 1,
	    ets:insert(C2#cache.victims, {NewT, Key}),
	    ets:insert(C2#cache.store, {Key, NewT, Recs}),
	    C3 = C2#cache{last_timestamp = NewT, current_size = NewSize},
	    Head#head{cache = C3}
    end.


%% Read from disk and cache the result
cache_fread_from_disk(Head, Key) ->
    Recs = fread(Head, Key),
    NewH = cache_insert(Head, Key, Recs),
    {NewH, Recs}.

%% Possibly reclaim space
cache_opt_reclaim(C) when C#cache.current_size =< C#cache.max_size ->
    C;
cache_opt_reclaim(C) ->
    TabV = C#cache.victims,
    OldestTimestamp = ets:first(TabV),
    Key = ets:lookup_element(TabV, OldestTimestamp, 2),
    ets:delete(TabV, OldestTimestamp),
    ets:delete(C#cache.store, Key),
    NewSize = C#cache.current_size - 1,
    C#cache{current_size = NewSize}.

%% Free some bytes on disk and invalidate entry in cache
cache_free(F, Head, Pos, Size, Key) ->
    Head2 = free(F, Head, Pos, Size),
    cache_delete(Head2, Key).

%% Invalidate entry in cache
cache_delete(Head, Key) ->
    C = Head#head.cache,
    case C#cache.max_size of
	0 ->
	    Head;
	infinity ->
	    ets:delete(C#cache.store, Key),
	    Head;
	_Max ->
	    case (catch ets:lookup_element(C#cache.store, Key, 2)) of
		{'EXIT', _} ->
		    Head;
		Timestamp ->
		    ets:delete(C#cache.store, Key),
		    ets:delete(C#cache.victims, Timestamp),
		    NewSize = C#cache.current_size - 1,
		    C2 = C#cache{current_size = NewSize},
		    Head#head{cache = C2}
	    end
    end.

%% Replace entry in cache
cache_replace(Head, Key, Recs) -> 
    C = Head#head.cache,
    case C#cache.max_size of
	0 ->
	    Head;
	_Max ->
	    case catch ets:lookup_element(C#cache.store, Key, 2) of
		{'EXIT', _} ->
		    cache_insert(Head, Key, Recs);
		0 ->
		    ets:insert(C#cache.store, {Key, 0, Recs});
		OldT ->
		    NewT = C#cache.last_timestamp + 1, 
		    ets:delete(C#cache.victims, OldT),
		    ets:insert(C#cache.victims, {NewT, Key}),
		    ets:insert(C#cache.store, {Key, NewT, Recs}),
		    NewC = C#cache{last_timestamp = NewT},
		    Head#head{cache = NewC}
	    end
    end.


