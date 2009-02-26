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
%% The Initial Developer of the Original Code is Tony Rogvall.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(zlib).

-export([open/0,close/1,deflateInit/1,deflateInit/2,deflateInit/6,
	 deflateSetDictionary/2,deflateReset/1,deflateParams/3,
	 deflate/2,deflate/3,deflateEnd/1,
	 inflateInit/1,inflateInit/2,inflateSetDictionary/2,
	 inflateSync/1,inflateReset/1,inflate/2,inflateEnd/1,
	 setBufSize/2,getBufSize/1,
	 crc32/1,crc32/2,crc32/3,adler32/2,adler32/3,getQSize/1,
	 crc32_combine/4,adler32_combine/4,
	 compress/1,uncompress/1,zip/1,unzip/1,
	 gzip/1,gunzip/1]).

%% flush argument encoding
-define(Z_NO_FLUSH,      0).
-define(Z_SYNC_FLUSH,    2).
-define(Z_FULL_FLUSH,    3).
-define(Z_FINISH,        4).

%% compression level
-define(Z_NO_COMPRESSION,         0).
-define(Z_BEST_SPEED,             1).
-define(Z_BEST_COMPRESSION,       9).
-define(Z_DEFAULT_COMPRESSION,  (-1)).

%% compresssion strategy
-define(Z_FILTERED,            1).
-define(Z_HUFFMAN_ONLY,        2).
-define(Z_DEFAULT_STRATEGY,    0).


%% deflate compression method
-define(Z_DEFLATED,  8).

-define(Z_NULL, 0).

-define(MAX_WBITS, 15).

%% gzip defs (rfc 1952)

-define(ID1, 16#1f).
-define(ID2, 16#8b).

-define(FTEXT,     16#01).
-define(FHCRC,     16#02).
-define(FEXTRA,    16#04).
-define(FNAME,     16#08).
-define(FCOMMENT,  16#10).
-define(RESERVED,  16#E0).

-define(OS_MDDOS,   0).
-define(OS_AMIGA,   1).
-define(OS_OPENVMS, 2).
-define(OS_UNIX,    3).
-define(OS_VMCMS,   4).
-define(OS_ATARI,   5).
-define(OS_OS2,     6).
-define(OS_MAC,     7).
-define(OS_ZSYS,    8).
-define(OS_CPM,     9).
-define(OS_TOP20,  10).
-define(OS_NTFS,   11).
-define(OS_QDOS,   12).
-define(OS_ACORN,  13).
-define(OS_UNKNOWN,255).

-define(DEFLATE_INIT,    1).
-define(DEFLATE_INIT2,   2).
-define(DEFLATE_SETDICT, 3).
-define(DEFLATE_RESET,   4).
-define(DEFLATE_END,     5).
-define(DEFLATE_PARAMS,  6).
-define(DEFLATE,         7).

-define(INFLATE_INIT,    8).
-define(INFLATE_INIT2,   9).
-define(INFLATE_SETDICT, 10).
-define(INFLATE_SYNC,    11).
-define(INFLATE_RESET,   12).
-define(INFLATE_END,     13).
-define(INFLATE,         14).

-define(CRC32_0,         15).
-define(CRC32_1,         16).
-define(CRC32_2,         17).

-define(SET_BUFSZ,       18).
-define(GET_BUFSZ,       19).
-define(GET_QSIZE,       20).

-define(ADLER32_1,       21).
-define(ADLER32_2,       22).

-define(CRC32_COMBINE,   23).
-define(ADLER32_COMBINE, 24).


%%------------------------------------------------------------------------

%% Main data types of the file -- make the first one builtin?
-type(iodata()      :: iolist() | binary()).
-type(zstream()     :: port()).

%% Auxiliary data types of the file
-type(zlevel()      :: 'none' | 'default' | 'best_compression' | 'best_speed' 
                     | 0..9).
-type(zmethod()     :: 'deflated').
-type(zwindowbits() :: -15..-9 | 9..47).
-type(zmemlevel()   :: 1..9).
-type(zstrategy()   :: 'default' | 'filtered' | 'huffman_only').
-type(zflush()      :: 'none' | 'sync' | 'full' | 'finish').

%%------------------------------------------------------------------------

%% open a z_stream
-spec(open/0 :: () -> zstream()).
open() ->
    open_port({spawn, zlib_drv}, [binary]).

%% close and release z_stream
-spec(close/1 :: (zstream()) -> 'ok').
close(Z) ->
    try
	true = port_close(Z),
	receive	      %In case the caller is the owner and traps exits
	    {'EXIT',Z,_} -> ok
	after 0 -> ok
	end
    catch _:_ -> erlang:error(badarg)
    end.

-spec(deflateInit/1 :: (zstream()) -> 'ok').
deflateInit(Z) ->
    call(Z, ?DEFLATE_INIT, <<?Z_DEFAULT_COMPRESSION:32>>).

-spec(deflateInit/2 :: (zstream(), zlevel()) -> 'ok').
deflateInit(Z, Level) ->
    call(Z, ?DEFLATE_INIT, <<(arg_level(Level)):32>>).

-spec(deflateInit/6 :: (zstream(), zlevel(), zmethod(),
		        zwindowbits(), zmemlevel(), zstrategy()) -> 'ok').
deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) ->
    call(Z, ?DEFLATE_INIT2, <<(arg_level(Level)):32, 
			     (arg_method(Method)):32,
			     (arg_bitsz(WindowBits)):32, 
			     (arg_mem(MemLevel)):32,
			     (arg_strategy(Strategy)):32>>).

-spec(deflateSetDictionary/2 :: (zstream(), binary()) -> integer()).
deflateSetDictionary(Z, Dictionary) ->
    call(Z, ?DEFLATE_SETDICT, Dictionary).

-spec(deflateReset/1 :: (zstream()) -> 'ok').
deflateReset(Z) ->
    call(Z, ?DEFLATE_RESET, []).

-spec(deflateParams/3 :: (zstream(), zlevel(), zstrategy()) -> 'ok').
deflateParams(Z, Level, Strategy) ->
    call(Z, ?DEFLATE_PARAMS, <<(arg_level(Level)):32, 
			      (arg_strategy(Strategy)):32>>).

-spec(deflate/2 :: (zstream(), iodata()) -> iolist()).
deflate(Z, Data) ->
    deflate(Z, Data, none).

-spec(deflate/3 :: (zstream(), iodata(), zflush()) -> iolist()).
deflate(Z, Data, Flush) ->
    try port_command(Z, Data) of
	true ->
	    call(Z, ?DEFLATE, <<(arg_flush(Flush)):32>>),
	    collect(Z)
    catch 
	error:_Err ->
	    flush(Z),
	    erlang:error(badarg) 
    end.

-spec(deflateEnd/1 :: (zstream()) -> 'ok').
deflateEnd(Z) ->
    call(Z, ?DEFLATE_END, []).    

-spec(inflateInit/1 :: (zstream()) -> 'ok').
inflateInit(Z) ->
    call(Z, ?INFLATE_INIT, []).    

-spec(inflateInit/2 :: (zstream(), zwindowbits()) -> 'ok').
inflateInit(Z, WindowBits) -> 
    call(Z, ?INFLATE_INIT2, <<(arg_bitsz(WindowBits)):32>>).

-spec(inflateSetDictionary/2 :: (zstream(), binary()) -> 'ok').
inflateSetDictionary(Z, Dictionary) -> 
    call(Z, ?INFLATE_SETDICT, Dictionary).

-spec(inflateSync/1 :: (zstream()) -> 'ok').
inflateSync(Z) -> 
    call(Z, ?INFLATE_SYNC, []).

-spec(inflateReset/1 :: (zstream()) -> 'ok').
inflateReset(Z) -> 
    call(Z, ?INFLATE_RESET, []).

-spec(inflate/2 :: (zstream(), iodata()) -> iolist()).
inflate(Z, Data) ->
    try port_command(Z, Data) of
	true -> 
	    call(Z, ?INFLATE, <<?Z_NO_FLUSH:32>>),
	    collect(Z)
    catch 
	error:_Err ->
	    flush(Z),
	    erlang:error(badarg) 
    end.

-spec(inflateEnd/1 :: (zstream()) -> 'ok').
inflateEnd(Z) ->
    call(Z, ?INFLATE_END, []).

-spec(setBufSize/2 :: (zstream(), non_neg_integer()) -> 'ok').
setBufSize(Z, Size) ->
    call(Z, ?SET_BUFSZ, <<Size:32>>).

-spec(getBufSize/1 :: (zstream()) -> non_neg_integer()).
getBufSize(Z) ->
    call(Z, ?GET_BUFSZ, []).

-spec(crc32/1 :: (zstream()) -> integer()).
crc32(Z) ->
    call(Z, ?CRC32_0, []).

-spec(crc32/2 :: (zstream(), binary()) -> integer()).
crc32(Z, Binary) ->
    call(Z, ?CRC32_1, Binary).

-spec(crc32/3 :: (zstream(), integer(), binary()) -> integer()).
crc32(Z, CRC, Binary) when is_binary(Binary), is_integer(CRC) ->
    call(Z, ?CRC32_2, <<CRC:32, Binary/binary>>);
crc32(_Z, _CRC, _Binary)  ->
    erlang:error(badarg).

-spec(adler32/2 :: (zstream(), binary()) -> integer()).
adler32(Z, Binary) ->
    call(Z, ?ADLER32_1, Binary).

-spec(adler32/3 :: (zstream(), integer(), binary()) -> integer()).
adler32(Z, Adler, Binary) when is_binary(Binary), is_integer(Adler) ->
    call(Z, ?ADLER32_2, <<Adler:32, Binary/binary>>);
adler32(_Z, _Adler, _Binary)  ->
    erlang:error(badarg).

crc32_combine(Z, CRC1, CRC2, Len2) 
  when is_integer(CRC1), is_integer(CRC2), is_integer(Len2) ->
    call(Z, ?CRC32_COMBINE, <<CRC1:32, CRC2:32, Len2:32>>);
crc32_combine(_Z, _CRC1, _CRC2, _Len2) ->
    erlang:error(badarg).

adler32_combine(Z, Adler1, Adler2, Len2) 
  when is_integer(Adler1), is_integer(Adler2), is_integer(Len2) ->
    call(Z, ?ADLER32_COMBINE, <<Adler1:32, Adler2:32, Len2:32>>);
adler32_combine(_Z, _Adler1, _Adler2, _Len2) ->
    erlang:error(badarg).

getQSize(Z) ->
    call(Z, ?GET_QSIZE, []).

%% compress/uncompress zlib with header
-spec(compress/1 :: (binary()) -> binary()).
compress(Binary) ->
    Z = open(),
    deflateInit(Z, default),
    Bs = deflate(Z, Binary,finish),
    deflateEnd(Z),
    close(Z),
    list_to_binary(Bs).

-spec(uncompress/1 :: (binary()) -> binary()).
uncompress(Binary) when byte_size(Binary) >= 8 ->
    Z = open(),
    inflateInit(Z),
    Bs = inflate(Z, Binary),
    inflateEnd(Z),
    close(Z),
    list_to_binary(Bs);
uncompress(Binary) when is_binary(Binary) -> erlang:error(data_error);
uncompress(_) -> erlang:error(badarg).

%% unzip/zip zlib without header (zip members)
-spec(zip/1 :: (binary()) -> binary()).
zip(Binary) ->
    Z = open(),
    deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    Bs = deflate(Z, Binary, finish),
    deflateEnd(Z),
    close(Z),
    list_to_binary(Bs).

-spec(unzip/1 :: (binary()) -> binary()).
unzip(Binary) ->
    Z = open(),
    inflateInit(Z, -?MAX_WBITS),
    Bs = inflate(Z, Binary),
    inflateEnd(Z),
    close(Z),
    list_to_binary(Bs).
    
-spec(gzip/1 :: (iodata()) -> binary()).
gzip(Data) when is_binary(Data); is_list(Data) ->
    Z = open(),
    deflateInit(Z, default, deflated, 16+?MAX_WBITS, 8, default),
    Bs = deflate(Z, Data, finish),
    deflateEnd(Z),
    close(Z),
    iolist_to_binary(Bs);
gzip(_) -> erlang:error(badarg).

-spec(gunzip/1 :: (iodata()) -> binary()).
gunzip(Data) when is_binary(Data); is_list(Data) ->
    Z = open(),
    inflateInit(Z, 16+?MAX_WBITS),
    Bs = inflate(Z, Data),
    inflateEnd(Z),
    close(Z),
    iolist_to_binary(Bs);
gunzip(_) -> erlang:error(badarg).

-spec(collect/1 :: (zstream()) -> iolist()).
collect(Z) -> 
    collect(Z,[]).

-spec(collect/2 :: (zstream(), iolist()) -> iolist()).
collect(Z,Acc) ->
    receive 
	{Z, {data, Bin}} ->
	    collect(Z,[Bin|Acc])
    after 0 ->
	    reverse(Acc)
    end.

-spec(flush/1 :: (zstream()) -> 'ok').
flush(Z) ->
    receive
	{Z, {data,_}} ->
	    flush(Z)
    after 0 ->
	    ok
    end.
    
arg_flush(none)    -> ?Z_NO_FLUSH;
%% ?Z_PARTIAL_FLUSH is deprecated in zlib -- deliberately not included.
arg_flush(sync)    -> ?Z_SYNC_FLUSH;
arg_flush(full)    -> ?Z_FULL_FLUSH;
arg_flush(finish)  -> ?Z_FINISH;
arg_flush(_) -> erlang:error(badarg).

arg_level(none)             -> ?Z_NO_COMPRESSION;
arg_level(best_speed)       -> ?Z_BEST_SPEED;
arg_level(best_compression) -> ?Z_BEST_COMPRESSION;
arg_level(default)          -> ?Z_DEFAULT_COMPRESSION;
arg_level(Level) when is_integer(Level), Level >= 0, Level =< 9 -> Level;
arg_level(_) -> erlang:error(badarg).
     
arg_strategy(filtered) ->     ?Z_FILTERED;
arg_strategy(huffman_only) -> ?Z_HUFFMAN_ONLY;
arg_strategy(default) ->      ?Z_DEFAULT_STRATEGY;
arg_strategy(_) -> erlang:error(badarg).

arg_method(deflated) -> ?Z_DEFLATED;
arg_method(_) -> erlang:error(badarg).

-spec(arg_bitsz/1 :: (zwindowbits()) -> zwindowbits()).
arg_bitsz(Bits) when is_integer(Bits) andalso
		     ((8 < Bits andalso Bits < 48) orelse
		      (-15 =< Bits andalso Bits < -8)) ->
    Bits;
arg_bitsz(_) -> erlang:error(badarg).

-spec(arg_mem/1 :: (zmemlevel()) -> zmemlevel()).
arg_mem(Level) when is_integer(Level), 1 =< Level, Level =< 9 -> Level;
arg_mem(_) -> erlang:error(badarg).

call(Z, Cmd, Arg) ->
    try port_control(Z, Cmd, Arg) of
	[0|Res] -> list_to_atom(Res);
	[1|Res] ->
	    flush(Z),
	    erlang:error(list_to_atom(Res));
	[2,A,B,C,D] ->
	    (A bsl 24)+(B bsl 16)+(C bsl 8)+D;
	[3,A,B,C,D] ->
	    erlang:error({need_dictionary,(A bsl 24)+(B bsl 16)+(C bsl 8)+D})
    catch 
	error:badarg -> %% Rethrow loses port_control from stacktrace.
	    erlang:error(badarg)
    end.

reverse(X) ->
    reverse(X, []).

reverse([H|T], Y) ->
    reverse(T, [H|Y]);
reverse([], X) -> 
    X.
