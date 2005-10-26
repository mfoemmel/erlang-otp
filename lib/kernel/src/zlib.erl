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

-record(gzip, 
	{
	  method = ?Z_DEFLATED, 
	  flags = 0,         %% :8
	  mtime = 0,         %% :32/little
	  xflags = 0,        %% :8
	  ostype = ?OS_UNIX, %% :8 = unix
	  extra,
	  name,
	  comment,
	  crc
	 }).
	  

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

%% open a z_stream
open() ->
    open_port({spawn, zlib_drv}, [binary]).

%% close and release z_stream
close(Z) ->
    try true = port_close(Z),
	ok
    catch _:_ -> erlang:error(badarg)
    end.

deflateInit(Z) ->
    call(Z, ?DEFLATE_INIT, <<?Z_DEFAULT_COMPRESSION:32>>).

deflateInit(Z, Level) ->
    call(Z, ?DEFLATE_INIT, <<(arg_level(Level)):32>>).

deflateInit(Z, Level, Method, WindowBits, MemLevel, Strategy) ->
    call(Z, ?DEFLATE_INIT2, <<(arg_level(Level)):32, 
			     (arg_method(Method)):32,
			     (arg_bitsz(WindowBits)):32, 
			     (arg_mem(MemLevel)):32,
			     (arg_strategy(Strategy)):32>>).

deflateSetDictionary(Z, Dictionary) ->
    call(Z, ?DEFLATE_SETDICT, Dictionary).

deflateReset(Z) ->
    call(Z, ?DEFLATE_RESET, []).

deflateParams(Z, Level, Strategy) ->
    call(Z, ?DEFLATE_PARAMS, <<(arg_level(Level)):32, 
			      (arg_strategy(Strategy)):32>>).

deflate(Z, Data) ->
    deflate(Z, Data, none).

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

deflateEnd(Z) ->
    call(Z, ?DEFLATE_END, []).    

inflateInit(Z) ->
    call(Z, ?INFLATE_INIT, []).    

inflateInit(Z, WindowBits) -> 
    call(Z, ?INFLATE_INIT2, <<(arg_bitsz(WindowBits)):32>>).

inflateSetDictionary(Z, Dictionary) -> 
    call(Z, ?INFLATE_SETDICT, Dictionary).

inflateSync(Z) -> 
    call(Z, ?INFLATE_SYNC, []).

inflateReset(Z) -> 
    call(Z, ?INFLATE_RESET, []).    

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

inflateEnd(Z) ->
    call(Z, ?INFLATE_END, []).

setBufSize(Z, Size) ->
    call(Z, ?SET_BUFSZ, <<Size:32>>).

getBufSize(Z) ->
    call(Z, ?GET_BUFSZ, []).

crc32(Z) ->
    call(Z, ?CRC32_0, []).

crc32(Z, Binary) ->
    call(Z, ?CRC32_1, Binary).

crc32(Z, CRC, Binary) when is_binary(Binary), is_integer(CRC) ->
    call(Z, ?CRC32_2, <<CRC:32, Binary/binary>>);
crc32(_Z, _CRC, _Binary)  ->
    erlang:error(badarg).

adler32(Z, Binary) ->
    call(Z, ?ADLER32_1, Binary).

adler32(Z, Adler, Binary) when is_binary(Binary), is_integer(Adler) ->
    call(Z, ?ADLER32_2, <<Adler:32, Binary/binary>>);
adler32(_Z, _Adler, _Binary)  ->
    erlang:error(badarg).

getQSize(Z) ->
    call(Z, ?GET_QSIZE, []).    

%% compress/uncompress zlib with header
compress(Binary) ->
    Z = open(),
    deflateInit(Z, default),
    Bs = deflate(Z, Binary,finish),
    deflateEnd(Z),
    close(Z),
    list_to_binary(Bs).

uncompress(Binary) when is_binary(Binary), size(Binary) >= 8 ->
    Z = open(),
    inflateInit(Z),
    Bs = inflate(Z, Binary),
    inflateEnd(Z),
    close(Z),
    list_to_binary(Bs);
uncompress(Binary) when is_binary(Binary) -> erlang:error(data_error);
uncompress(_) -> erlang:error(badarg).

%% unzip/zip zlib without header (zip members)
zip(Binary) ->
    Z = open(),
    deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    Bs = deflate(Z, Binary, finish),
    deflateEnd(Z),
    close(Z),
    list_to_binary(Bs).

unzip(Binary) ->
    Z = open(),
    inflateInit(Z, -?MAX_WBITS),
    Bs = inflate(Z, Binary),
    inflateEnd(Z),
    close(Z),
    list_to_binary(Bs).
    
gzip(Data) ->
    Bin0 = if list(Data) -> 
		   list_to_binary(Data);
	      binary(Data) ->
		   Data;
	      true -> 
		   erlang:error(badarg)
	   end,
    Z = open(),
    deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    Bs = deflate(Z, Bin0, finish),
    deflateEnd(Z),
    Crc = crc32(Z, Bin0),
    close(Z),
    %% add header and crc
    Head = write_gzip_header(#gzip {}),
    Tail = <<Crc:32/little, (size(Bin0)):32/little>>,
    list_to_binary([Head,Bs,Tail]).

gunzip(Bin0 = <<?ID1, ?ID2, Method, Flags, MTime:32, 
	       XFlags, OsType, _/binary>>) ->
    Gz0 = #gzip { method = Method,
		  flags = Flags, 
		  mtime = MTime, 
		  xflags = XFlags, 
		  ostype = OsType },
    {_,Bin1} = read_gzip_header(Flags, Bin0, 10, Gz0),
    %% io:format("gunzip header = ~p\n", [Gz1]),
    Z = open(),
    inflateInit(Z, -?MAX_WBITS),
    Bs = inflate(Z, Bin1),
    Crc = crc32(Z),
    Remain = getQSize(Z),
    inflateEnd(Z),
    close(Z),
    Offset = size(Bin1) - Remain,
    Bin2 = list_to_binary(Bs),
    <<_:Offset/binary, Crc32:32/little, Length:32/little, _/binary>> = Bin1,
    if Crc32 =/= Crc ->
	    erlang:error(bad_crc);
       Length =/= size(Bin2) ->
	    erlang:error(bad_length);
       true ->
	    Bin2
    end;
gunzip(_) -> 
    erlang:error(badarg).
    

write_gzip_header(Gz) ->
    {D1,F1} = 
	if Gz#gzip.extra == undefined ->
		{<<>>, 0};
	   true ->
		Extra = list_to_binary([Gz#gzip.extra]),
		{ <<(size(Extra)):16/little, Extra/binary>>, ?FEXTRA}
	end,
    {D2,F2} =
	if Gz#gzip.name == undefined ->
		{<<>>, 0};
	   true ->
		Name = list_to_binary([Gz#gzip.name, 0]),
		{ Name, ?FNAME }
	end,
    {D3,F3} =
	if Gz#gzip.comment == undefined ->
		{<<>>, 0};
	   true ->
		Comment = list_to_binary([Gz#gzip.comment, 0]),
		{ Comment, ?FCOMMENT }
	end,
    {D4,F4} =
	if Gz#gzip.crc == undefined ->
		{<<>>, 0};
	   true ->
		{ <<(Gz#gzip.crc):16/little >>, ?FHCRC }
	end,
    << ?ID1, ?ID2, 
     (Gz#gzip.method):8,
     (F1 bor F2 bor F3 bor F4):8,
     (Gz#gzip.mtime):32/little,
     (Gz#gzip.xflags):8,
     (Gz#gzip.ostype):8,
     D1/binary, D2/binary, D3/binary, D4/binary>>.


%% read the variable part of the gzip header
read_gzip_header(Flags, Binary, Offs0, Gz0) ->
    {Gz1,Offs1} =
	if (Flags band ?FEXTRA) =/= 0 ->
		<<_:Offs0/binary, Len:16/little, _/binary>> = Binary,
		Offs00 = Offs0+2,
		<<_:Offs00/binary, Extra:Len/binary,_/binary>> = Binary,
		{Gz0#gzip { extra = Extra }, Offs0 + 2 + Len};
	   true -> 
		{Gz0,Offs0}
	end,
     {Gz2,Offs2} = 
	if (Flags band ?FNAME) =/= 0 ->
		Name = cname(Binary, Offs1),
		{Gz1#gzip { name = Name}, Offs1 + length(Name)+1 };
	   true ->
		{Gz1, Offs1}
	end,
    {Gz3, Offs3} = 
	if (Flags band ?FCOMMENT) =/= 0 ->
		Comment = cname(Binary, Offs2),
		{Gz2#gzip { comment = Comment}, Offs2 + length(Comment)+1};
	   true ->
		{Gz2, Offs2}
	end,
    {Gz4, Offs4} = 
	if (Flags band ?FHCRC) =/= 0 ->
		<<_:Offs3, Crc:16/little, _/binary>> = Binary,
		{Gz3#gzip { crc = Crc }, Offs3+2};
	   true ->
		{Gz3, Offs3}
	end,
    <<_:Offs4/binary, Body/binary>> = Binary,
    {Gz4, Body}.


cname(Binary, Offs) ->
    case Binary of
	<<_:Offs/binary, C, _/binary>> ->
	    if C == 0 -> [];
	       true -> [C|cname(Binary, Offs+1)]
	    end;
	<<_:Offs/binary>> ->
	    []
    end.    
    
	    
collect(Z) -> 
    collect(Z,[]).

collect(Z,Acc) ->
    receive 
	{Z, {data, Bin}} ->
	    collect(Z,[Bin|Acc])
    after 0 ->
	    lists:reverse(Acc)
    end.

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
arg_level(Level) when Level >= 0, Level =< 9  -> Level;
arg_level(_) -> erlang:error(badarg).
     
arg_strategy(filtered) -> ?Z_FILTERED;
arg_strategy(huffman_only) -> ?Z_HUFFMAN_ONLY;
arg_strategy(default) ->      ?Z_DEFAULT_STRATEGY;
arg_strategy(_) -> erlang:error(badarg).

arg_method(deflated) -> ?Z_DEFLATED;
arg_method(_) -> erlang:error(badarg).

arg_bitsz(Bits) when 8 < abs(Bits), abs(Bits) =< 15 ->  Bits;
arg_bitsz(_) -> erlang:error(badarg).

arg_mem(Level) when is_integer(Level), 1 =< Level , Level =< 9 -> Level;
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
