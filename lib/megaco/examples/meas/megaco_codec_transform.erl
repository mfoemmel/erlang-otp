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
%%----------------------------------------------------------------------
%% Purpose: Megaco message transformation (from codec to codec)
%% 
%% Usage:   To simplify matters, two assumptions:
%%          
%%          1) The message dirs are in the working dir
%%          2) The message dirs are name accordingly:
%%
%%             Pretty text:  pretty
%%             Compact text: compact
%%             Binary ber:   ber
%%             Binary per:   per
%%             Erlang:       erlang
%%          
%%          This api exports several tranformation functions:
%%          
%%          tt/0  -> Transform messages using pretty text as base.
%%                   This is the equivalent of calling t/2 with:
%%                   megaco_codec_transform:t(pretty, 
%%                                            [compact, per, ber, erlang]).
%%          
%%          tb/0  -> Transform messages using ber binary as base.
%%                   This is the equivalent of calling t/2 with:
%%                   megaco_codec_transform:t(ber, 
%%                                            [pretty, compact, per, erlang]).
%%          
%%          t/1   -> Called from the command line (shell) to transform
%%                   all messages in a given codec dir to a given 
%%                   list of codec dirs. The dirs will _not_ be created.
%%          
%%          t/2   -> Transforms all messages in a given codec dir to a 
%%                   given list of codec dirs. The dirs will _not_ be 
%%                   created.
%%          
%%          tmf/3 -> Transform a message in a file encoded with the 
%%                   given codec to another codec. Written to file.
%%          
%%          tm/3  -> Tranforms a message binary encoded with the
%%                   given codec to another codec. Returned.
%%          
%%----------------------------------------------------------------------

-module(megaco_codec_transform).

-export([tt/0, tb/0]).
-export([t/1, t/2]).
-export([tmf/3, tm/3]).

-include_lib("kernel/include/file.hrl").


tt() ->
    t(pretty, [compact, per, ber, erlang]).


tb() ->
    t(ber, [pretty, compact, per, erlang]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% t/1 -> transform messages from codec to codec's
%%
%% Description: This function is used when calling from a shell.
%%              Example:
%%
%%              erl -noshell -sname megaco ../ebin \
%%                  -run megaco_codec_transform t ber "pretty compact per" \
%%                  -s erlang halt
%%  
%%              This example will from convert messages encoded with
%%              codec ber to codecs pretty, compact and per.
%%  
%% Parameters:  FromCodec -> codec_string()
%%              ToCodecs  -> [codec_string()]
%%              codec_string() -> "pretty" | "compact" | 
%%                                "per" | "ber" | "erlang"
%%  
%% Returns:     ok | {error, Reason}
%%  
t([FromCodec, ToCodecs0]) when list(FromCodec), list(ToCodecs0) ->
    case regexp:split(ToCodecs0, " ") of
	{ok, ToCodecs1} ->
	    ToCodecs2 = [list_to_atom(ToCodec) || ToCodec <- ToCodecs1],
	    t(list_to_atom(FromCodec), ToCodecs2);
	Error ->
	    Error
    end.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% t/2 -> transform messages from codec to codec's
%%
%% Description: This function is used to convert messages encoded with
%%              a codec (from) to several other codecs (to).
%%  
%% Parameters:  FromCodec -> codec()
%%              ToCodecs  -> [codec()]
%%              codec()   -> pretty | compact | per | ber | erlang
%%  
%% Returns:     ok | {error, Reason}
%%  
t(FromCodec, ToCodecs) when atom(FromCodec), list(ToCodecs) ->
    case (catch codec_dir(FromCodec)) of
	{ok, FromDir} ->
	    FromFiles = message_files(FromDir),
	    do_t(FromFiles, FromCodec, ToCodecs);
	Error ->
	    Error
    end.


do_t(_Files, _FromCodec, []) ->
    info("done", []),
    ok;
do_t(Files, FromCodec, [ToCodec|ToCodecs]) ->
    io:format("~p -> ~p: ", [FromCodec, ToCodec]),
    case (catch do_t1(Files, FromCodec, ToCodec)) of
	ok ->
	    io:format("~n", []),
	    ok;
	{error, S} when list(S) ->
	    error("~s", [S]);
	{error, Reason} ->
	    io:format("~n", []),
	    error("skipping codec ~p:~n~p", [ToCodec, Reason]);
	{info, S} ->
	    info("~s", [S])
    end,
    do_t(Files, FromCodec, ToCodecs).


do_t1([], _FromCodec, _ToCodec) ->
    ok;
do_t1([File|Files], FromCodec, ToCodec) ->
    io:format(".", []),
    case tmf(File, FromCodec, ToCodec) of
	ok ->
	    ok;
	{error, S} when list(S) ->
	    error("~s", [S]);
	{fatal_error, Reason} ->
	    %% This means we shall abort transforming to this codec
	    throw({error, Reason});
	{info, S} ->
	    info("~s", [S])
    end,
    do_t1(Files, FromCodec, ToCodec).
	    

message_files(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    lists:sort(Files);
	Error ->
	    throw(Error)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tmf/3 - transform file message
%% 
%% Description: Transform a message in a file encoded with one codec 
%%              (from) to another codec (to).
%%              The resulting message is written to a file in the 
%%              ToCodec dir. The file will be named: Text codec will
%%              give extension ".txt" and binary (and erlang) will
%%              give extension ".bin".
%% 
%% Parameters:
%%   FromFile  -> string() -> Filename of the message encoded according 
%%                            to FromCodec
%%   FromCodec -> codec()
%%   ToCodec   -> codec()
%%   codec()   -> pretty | compact | ber | per | erlang
%% 
%% Returns:
%%   ToMsg     -> ok | {error, Reason}
%% 
tmf(FromFile, FromCodec, ToCodec) when list(FromFile) ->
    (catch do_tmf(FromFile, FromCodec, ToCodec)).

do_tmf(FromFile, FromCodec, ToCodec) ->
    {ok, FromDir} = codec_dir(FromCodec),
    {ok, ToDir}   = codec_dir(ToCodec),
    FromBin       = read_message(FromDir, FromFile),
    case tm(FromBin, FromCodec, ToCodec) of
	{ok, ToMsg} ->
	    ToName = filename:rootname(FromFile) ++ extension_of(ToCodec),
	    write_message(ToDir, ToName, ToMsg);
	Error ->
	    Error
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tm/3 - transform message
%% 
%% Description: Transform a message encoded with one codec (from) 
%%              to another codec (to)
%% 
%% Parameters:
%%   FromMsg   -> binary() -> Message encoded according to FromCodec
%%   FromCodec -> codec()
%%   ToCodec   -> codec()
%%   codec()   -> pretty | compact | ber | per | erlang
%% 
%% Returns:
%%   ToMsg     -> binary() -> Message encoded according to ToCodec
%% 
tm(FromMsg, FromCodec, ToCodec) when binary(FromMsg) ->
    (catch do_tm(FromMsg, FromCodec, ToCodec)).


do_tm(FromMsg, FromCodec, ToCodec) ->
    {ok, FMsg} = decode_message(FromCodec, FromMsg),
    encode_message(ToCodec, FMsg).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% read_message

read_message(Dir, FileName) ->
    File = filename:join([Dir, FileName]),
    case file:read_file_info(File) of
	{ok, #file_info{size = Sz, type = regular}} when Sz > 0 ->
	    case file:read_file(File) of
		{ok, Msg} ->
		    Msg;
		{error, Reason} ->
		    S = format("failed reading file ~s: ~p", [File, Reason]),
		    throw({error, S})
	    end;

	{ok, #file_info{type = regular}} ->
	    S = format("skipping empty file ~s", [File]),
	    throw({info, S});
	    
	{ok, #file_info{type = Type}} ->
	    S = format("skipping ~p file ~s", [Type, File]),
	    throw({info, S});

	{ok, Info} ->
	    S = format("skipping file ~s~n~p", [File, Info]),
	    throw({info, S});
						 
	{error, Reason} ->
	    S = format("failed reading file info for ~s: ~p", [File, Reason]),
	    throw({error, S})

    end.


%% write_message

write_message(Dir, File, Bin) ->    
    Filename = filename:join([Dir, File]),
    case file:open(Filename, [raw, binary, write]) of
	{ok, Fd} ->
	    case file:write(Fd, Bin) of
		ok ->
		    file:close(Fd),
		    ok;
		{error, Reason} ->
		    S = format("failed writing message (~p bytes): ~p",
			       [size(Bin), Reason]),
		    file:close(Fd),
		    throw({error, S})
	    end;

	{error, Reason} ->
	    S = format("failed open file ~s: ~p", [Filename, Reason]),
	    throw({error, S})
    end.


extension_of(pretty) ->
    ".txt";
extension_of(compact) ->
    ".txt";
extension_of(ber) ->
    ".bin";
extension_of(per) ->
    ".bin";
extension_of(erlang) ->
    ".bin".


%% decode_message
%% Note: The '{version3,prev3a}' will be ignored 
%%       by other than v3-codec's.
%%       This is a "fix" to make enable the pre-v3
%%       encoder introduced in version 3.0 of 
%%       the Megaco application.

decode_message(pretty, BinMsg) ->
    Mod  = megaco_pretty_text_encoder,
    Conf = [{version3,prev3a}],
    do_decode(Mod, Conf, BinMsg);
decode_message(compact, BinMsg) ->
    Mod  = megaco_compact_text_encoder,
    Conf = [{version3,prev3a}],
    do_decode(Mod, Conf, BinMsg);
decode_message(ber, BinMsg) ->
    Mod  = megaco_ber_bin_encoder,
    Conf = [{version3,prev3a}],
    do_decode(Mod, Conf, BinMsg);
decode_message(per, BinMsg) ->
    Mod  = megaco_per_bin_encoder,
    Conf = [{version3,prev3a}],
    do_decode(Mod, Conf, BinMsg);
decode_message(erlang, BinMsg) ->
    Mod  = megaco_erl_dist_encoder,
    Conf = [{version3,prev3a}],
    do_decode(Mod, Conf, BinMsg);
decode_message(Codec, _) ->
    throw({error, {unsupported_codec, Codec}}).


do_decode(Mod, Conf, Bin) ->
    case (catch Mod:decode_message(Conf, Bin)) of
	{ok, Msg} ->
	    {ok, Msg};
	 {error, Reason} ->
	    S = format("decode error: ~p", [Reason]),
	    %% throw({error, {decode_error, Reason}});
	    throw({error, S});
	{'EXIT', Reason} ->
	    S = format("decode exit: ~p", [Reason]),
	    %% throw({error, {decode_exit, Reason}});
	    throw({error, S});
	Other ->
	    S = format("unknwon decode result: ~p", [Other]),
	    %% throw({error, {unknown_decode_result, Other}})
	    throw({error, S})
    end.


%% encode_message
%% Note: See note above (decode_message)

encode_message(pretty, Msg) ->
    Mod  = megaco_pretty_text_encoder,
    Conf = [{version3,prev3a}],
    do_encode(Mod, Conf, Msg);
encode_message(compact, Msg) ->
    Mod  = megaco_compact_text_encoder,
    Conf = [{version3,prev3a}],
    do_encode(Mod, Conf, Msg);
encode_message(ber, Msg) ->
    Mod  = megaco_ber_bin_encoder,
    Conf = [{version3,prev3a}],
    do_encode(Mod, Conf, Msg);
encode_message(per, Msg) ->
    Mod  = megaco_per_bin_encoder,
    Conf = [{version3,prev3a}],
    do_encode(Mod, Conf, Msg);
encode_message(erlang, Msg) ->
    Mod  = megaco_erl_dist_encoder,
    Conf = [{version3,prev3a}],
    do_encode(Mod, Conf, Msg);
encode_message(Codec, _) ->
    throw({error, {unsupported_codec, Codec}}).


do_encode(Mod, Conf, Msg) ->
    case (catch Mod:encode_message(Conf, Msg)) of
	{ok, Bin} ->
	    {ok, Bin};
	 {error, Reason} ->
	    S = format("encode error: ~p", [Reason]),
	    %% throw({error, {encode_error, Reason}});
	    throw({error, S});
	{'EXIT', Reason} ->
	    S = format("encode exit: ~p", [Reason]),
	    %% throw({error, {encode_exit, Reason}});
	    throw({error, S});
	Other ->
	    S = format("unknwon encode result: ~p", [Other]),
	    %% throw({error, {unknown_encode_result, Other}})
	    throw({error, S})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_codec(Codec) when atom(Codec) ->
    Codecs = [pretty, compact, ber, per, erlang],
    case lists:member(Codec, Codecs) of
	true ->
	    ok;
	false ->
	    throw({fatal_error, {unsupported_codec, Codec}})
    end.

					 
codec_dir(Codec) when atom(Codec) ->
    check_codec(Codec),
    {ok, atom_to_list(Codec)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


info(F, A) ->
    io:format(F ++ "~n", A).


error(F, A) -> 
    io:format("ERROR: " ++ F ++ "~n", A).


format(F, A) ->    
    lists:flatten(io_lib:format(F, A)).
