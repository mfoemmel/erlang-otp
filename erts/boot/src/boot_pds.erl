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
%%

-module(boot_pds).

%% Purpose: Primitive Disk Store (PDS)
%%
%% PDS provides a primitive disk store.
%% The goals of PDS are to make a *minimal*
%% {Key,Value} disk based file store
%% that calls *nothing at all* and only uses the linked in
%% File driver. PDS is designed to be used with primitive code loaders
%% though it can also be used for other purposes.
%% No file locking is assumed - so funny things will happen if two 
%% concurrent processes try to simultaneous read and write
%% the same store.

-compile(export_all).

-export([open/2, close/1, keys/1, fat/1, compact/2, 
	 fetch/2, store/3, delete/2, info/1]).

%% open(File, OpenMode)     -> Pid.
%% close(Pid)               -> ok.
%% fat(Pid)                 -> FAT.
%% keys(Pid)                -> [Keys].
%% fetch(Pid, Key)          -> {ok,Val} | error.
%% store(Pid, Key, Val)     -> ok.
%% delete(Pid, Key)         -> ok.
%% info(Pid)                -> {TotalSize, FreeSpace}.
%% compact(InFile, OutFile) -> true.

%% OpenMode = read | read_write

%% FAT  = {Free, Ftab}
%% Ftab = [{Start,Length,Key,Flag}]
%% Flag = used | free.
%% The FAT is stored at the end of the file
%% Just like PDF
%% Free is the first free address after all the data blocks
%% This is where the start of the FAT table should be
%% 
%% Obj1
%% Obj2
%% Obj3
%% <<Bin>>           <-- MMM Length is an erlang binary
%% xref
%% MMMM             <- start 
%% LLLL             <- length

%% usage:
%%   
%% 1> P=boot_pds:open("foo.fat").
%% <0.41.0>
%% 2> boot_pds:keys(P).
%% []
%% 3> boot_pds:fat(P).
%% {41,[]}
%% 4> boot_pds:store(P,a,b).
%% ok
%% 5> boot_pds:keys(P).
%% [a]
%% 6> boot_pds:fetch(P,a).
%% {ok,b}
%% 7> boot_pds:fetch(P,z).
%% error
%% 8> boot_pds:fat(P).
%% {46,[{41,5,a,used}]}
%% 9> boot_pds:close(P).
%% stopped               

open(File, read) ->
    make_server(fun() -> read_fat(File, read) end,
		fun handler/2);
open(File, read_write) ->
    make_server(fun() -> read_fat(File, read_write) end,
		fun handler/2).


fat(P)             -> rpc(P, fat).
keys(P)            -> rpc(P, keys).
store(P, Key, Val) -> rpc(P, {store, Key, Val}).
fetch(P, Key)      -> rpc(P, {fetch, Key}).
delete(P, Key)     -> rpc(P, {delete, Key}).
close(P)           -> rpc(P, close).
info(P)            -> rpc(P, info).

%% Compact the FAT

compact(In, Out) ->    
    Pin = open(In, read),
    Pout = open(Out, read_write),
    compact_copy(keys(Pin), Pin, Pout),
    close(Pin),
    close(Pout),
    ok.

compact_copy([Key|T], Pin, Pout) ->
    {ok, Val} = fetch(Pin, Key),
    store(Pout, Key, Val),
    compact_copy(T, Pin, Pout);
compact_copy([], _, _) ->
    true.

read_fat(File, Mode) ->
    case exists(File) of
	true ->
	    %% the file exists
	    {ok, P} = open_file(File, mode(Mode)),
	    Fat = recover_fat(P),
	    {P, Fat};
	false ->
	    {ok, P} = open_file(File, mode(Mode)),
	    Fat = fat_new(),
	    store_fat(P, Fat),
	    {P, Fat}
    end.

mode(read_write) -> [binary,raw,read,write];
mode(read)       -> [binary,raw,read].

handler(fat, State={_,Fat}) ->
    {Fat, State};
handler(keys, State={_,{_,Tab}}) ->
    Keys = [Key||{_,_,Key,used}<-Tab, Key =/= ""],
    {Keys, State};
handler({fetch, Key}, State={P,{_,Tab}}) ->
    Content = fat_read(Key, Tab, P),
    {Content, State};
handler({store,Key,Val}, _State={P,Fat}) ->
    Fat1 = fat_write(Key,Val,Fat,P),
    {ok, {P,Fat1}};
handler({delete,Key}, _State={P,Fat}) ->
    Fat1 = freeup(Key, Fat),
    Fat2 = merge_free_blocks(Fat1),
    store_fat(P, Fat2),
    {ok, {P, Fat2}};
handler(close, _) ->
    {stop, stopped};
handler(info, State={_,{Free,Slots}}) ->
    {{Free-1, fat_count_free(Slots, 0)}, State}.

fat_new() ->  {41, []}.

fat_count_free([{_,Len,_,free}|T], Sum) -> fat_count_free(T, Sum + Len);
fat_count_free([_|T], Sum)              -> fat_count_free(T, Sum);
fat_count_free([], Sum)                 -> Sum.

fat_read(Key, [{Start,Len,Key,used}|_], P) ->
    {ok, Bin} = pread(P, Start, Len),
    %% erlang:display({fat_read,Key,length,Len}),
    {ok, binary_to_term(Bin)};
fat_read(Key, [_|T], P) ->
    fat_read(Key, T, P);
fat_read(_Key, [], _) ->
    error.

fat_write(Key, Val, Fat, P) ->
    Fat1 = freeup(Key, Fat),
    Fat2 = merge_free_blocks(Fat1),
    B = term_to_binary(Val),
    Size = size(B),
    %% erlang:display({fat,write,Key,length,Size}),
    Fat3 = case store(Key, Fat2, B, Size, P) of
	       no ->
		   store_at_end(Key, B, Size, Fat2, P);
	       Fat4 ->
		   Fat4
	   end,
    store_fat(P, Fat3),
    Fat3.


freeup(Key, {Free, Fat})          -> {Free, freeup1(Key, Fat)}.

freeup1(Key, [{Loc,Len,Key,_}|T]) -> [{Loc,Len,"",free}|T];
freeup1(Key, [H|T])               -> [H|freeup1(Key, T)];
freeup1(_Key, [])                  -> [].
	
merge_free_blocks({Free,Fat})     -> {Free, merge_free_blocks1(Fat)}.

merge_free_blocks1([{Loc,Len,"",free},{_Loc1,Len1,"",free}|T]) ->
    merge_free_blocks1([{Loc,Len+Len1,"",free}|T]);
merge_free_blocks1([H|T]) ->
    [H|merge_free_blocks1(T)];
merge_free_blocks1([]) ->
    [].

store(Key, {Free,Fat}, B, Size, P) ->
    case store1(Key, Fat, B, Size, P, []) of
	{yes, Fat1} ->
	    {Free, Fat1};
	no ->
	    no
    end.

store1(Key, [{Loc,Len,_,free}|T], B, Size, P, L) when Size =< Len ->
    Fat1 = [{Loc,Size,Key,used},{Loc+Size,Len-Size,"",free}|T],
    pwrite(P, Loc, B),
    {yes, reverse(L, Fat1)};
store1(Key, [{Loc,Len,_,free}|T], B, Len, P, L) ->
    Fat1 = [{Loc,Len,Key,used}|T],
    pwrite(P, Loc, B),
    {yes, reverse(L, Fat1)};
store1(Key, [H|T], B, Len, P, L) ->
    store1(Key, T, B, Len, P, [H|L]);
store1(_Key, [], _B, _Len, _P, _) ->
    no.

store_at_end(Key, Val, Size, {Free, Fat}, P) ->
    pwrite(P, Free, Val),
    Fat1 = Fat ++ [{Free,Size,Key,used}],
    {Free+Size, Fat1}.

store_fat(P, {Free, Fat}) ->
    B = term_to_binary(Fat),
    S = size(B),
    pwrite(P, Free, B),
    pwrite(P, 0, pack_int(20, Free)),
    pwrite(P, 20, pack_int(20, S)).

recover_fat(P) ->    
    {ok, B1} = pread(P, 0, 20),
    L1 = binary_to_list(B1),
    Ptr = list_to_integer(L1),
    {ok, B2} = pread(P, 20, 20),
    L2 = binary_to_list(B2),
    Len = list_to_integer(L2),
    {ok, B3} = pread(P, Ptr, Len),
    Fat = {Ptr, binary_to_term(B3)},
    Fat.
    

pack_int(Len, I) ->
    Str = integer_to_list(I),
    Str1 = pack_int1(Len, length(Str), Str),
    list_to_binary(Str1).

pack_int1(Max, Size, Str) when Size < Max ->
    [$0|pack_int1(Max, Size+1, Str)];
pack_int1(Max, Max, Str) ->
    Str.

%%----------------------------------------------------------------------
%% This section of the program provides a
%% *minimal* interface to the filer
%% It *only* offers primitive
%% random aceess I/O

open_mode([binary,raw,read,write]) -> {3, [binary]};
open_mode([binary,raw,read]) -> {1, [binary]}.
    
-define(FILE_OPEN, 1).
    
open_file(File, ModeList) ->
    case catch call_prim_file(open, [File, ModeList]) of
	not_loaded ->
	    {Mode, PortMode} = open_mode(ModeList),
	    Cmd = [<<?FILE_OPEN, Mode:32>>, File, 0],
	    case mkport(Cmd, PortMode) of
		{'EXIT', _} ->
		    {error, emfile};
		P ->
		    case get_response(P) of
			{ok, _Number}  ->  
			    {ok, P};
			Error ->
			    close_port(P),
			    Error
		    end
	    end;
	{'EXIT', Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.

-define(FILE_CLOSE, 23).

close_file(Handle) ->
    case catch call_prim_file(close, [Handle]) of
	not_loaded ->
	    port_command(Handle, <<?FILE_CLOSE>>),
	    get_response(Handle);
	{'EXIT', Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.

-define(FILE_PREADV, 25).

pread(Port, Offs, Size) ->
    case catch call_prim_file(pread, [Port, Offs, Size]) of
	not_loaded ->
	    port_command(Port, 
			 <<?FILE_PREADV, 0:32, 1:32, Offs:64/signed, Size:64>>),
	    case get_response(Port) of
		{ok, [eof]} ->
		    eof;
		{ok, [Data]} ->
		    {ok, Data};
		Other ->
		    Other
	    end;
	{'EXIT', Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.

-define(FILE_PWRITEV, 24).

pwrite(Port, Offs, Bytes) ->
    case catch call_prim_file(pwrite, [Port, Offs, Bytes]) of
	not_loaded ->
	    Data = list_to_binary(Bytes),
	    Size = size(Data),
	    port_command(Port, 
			 [<<?FILE_PWRITEV, 1:32, Offs:64/signed, Size:64>> 
			  | Data]),
	    case get_response(Port) of
		{ok, _SizeWritten} ->
		    ok;
		Other ->
		    Other
	    end;
	{'EXIT', Reason} ->
	    exit(Reason);
	Result ->
	    Result
    end.


call_prim_file(Name, Args) ->
    case get(no_prim_file) of
	true ->
	    not_loaded;
	_ ->
	    case erlang:function_exported(prim_file, Name, length(Args)) of
		true ->
		    apply(prim_file, Name, Args);
		false ->
		    put(no_prim_file, true),
		    not_loaded
	    end
    end.

get_response(Port) ->
    receive
	{Port, {data, [Response|Rest]}} ->
	    get_response(Response, Rest);
	{'EXIT', Port, _Reason} ->
	    {error, port_died}
    end.

-define(FILE_RESP_OK,          0).
-define(FILE_RESP_ERROR,       1).
-define(FILE_RESP_DATA,        2).
-define(FILE_RESP_NUMBER,      3).
-define(FILE_RESP_NUMERR,      5).
-define(FILE_RESP_LDATA,       6).

get_response(?FILE_RESP_OK, []) ->
    ok;
get_response(?FILE_RESP_OK, Data) ->
    {ok, Data};
get_response(?FILE_RESP_ERROR, List) when list(List) ->
    {error, list_to_atom(List)};
get_response(?FILE_RESP_NUMBER, [X1,X2,X3,X4,X5,X6,X7,X8]) ->
    {ok, i64(X1,X2,X3,X4,X5,X6,X7,X8)};
get_response(?FILE_RESP_NUMERR, [X1,X2,X3,X4,X5,X6,X7,X8|List]) ->
    {ok, {i64(X1,X2,X3,X4,X5,X6,X7,X8), list_to_atom(List)}};
get_response(?FILE_RESP_DATA, [X1,X2,X3,X4,X5,X6,X7,X8|Data]) ->
    {ok, {i64(X1,X2,X3,X4,X5,X6,X7,X8), Data}};
get_response(?FILE_RESP_LDATA, [<<0:32, 1:32, 0:64>> | <<>>]) ->
    {ok, [eof]};
get_response(?FILE_RESP_LDATA, [<<0:32, 1:32, _Size:64>> | Data]) ->
    {ok, [Data]};
get_response(X, Data) ->
    {error, {bad_response_from_port, X, Data}}.


mkport(Cmd, Mode) ->
    case catch erlang:open_port_prim({spawn, efile}, Mode) of
	P ->
	    port_command(P, Cmd),
	    P;
	{'EXIT', Reason} ->
	    {error, {'EXIT', Reason}}
    end.

exists(F) ->
    case open_file(F, [binary,raw,read]) of
	{ok, Port} ->
	    close_file(Port),
	    true;
	{error, enoent} ->
	    false
    end.

close_port(Port) ->
    catch erlang:port_close(Port),
    receive {'EXIT', Port, _Reason} -> ok
    after 0 -> ok
    end.

% i32(Int) when binary(Int) ->
%     i32(binary_to_list(Int));

% i32(Int)  when integer(Int) -> [(Int bsr 24) band 255,
% 				(Int bsr 16) band 255,
% 				(Int bsr  8) band 255,
% 				Int band 255];
i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

i32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

i64(X1,X2,X3,X4,X5,X6,X7,X8) ->
    (i32(X1,X2,X3,X4) bsl 32) bor i32(X5,X6,X7,X8).

%%----------------------------------------------------------------------
%% from lists

reverse([], L)    -> L;
reverse([H|T], L) -> reverse(T, [H|L]).

%%----------------------------------------------------------------------
%% A minimal server

make_server(FunD, FunH) ->
    spawn_link(fun() -> 
		       Data = FunD(),
		       Self = self(),
		       server_loop(Self, Data, FunH) 
               end).

server_loop(Name, Data, Fun) ->
    receive
        {rpc, Pid, Q} ->
            case (catch Fun(Q, Data)) of
                {'EXIT', Why} ->
                    Pid ! {Name, exit, Why},
                    server_loop(Name, Data, Fun);
		{stop, Msg} ->
		    Pid ! {Name, Msg};
                {Reply, Data1} ->
                    Pid ! {Name, Reply},
                    server_loop(Name, Data1, Fun)
            end
    end.

rpc(Pid, Q) -> 
    Pid ! {rpc, self(), Q},
    receive     
        {Pid, Reply} ->
            Reply;
        {Pid, exit, Why} ->
            exit(Why)
    end.
