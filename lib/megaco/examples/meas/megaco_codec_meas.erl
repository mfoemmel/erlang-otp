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
%% Purpose: Measure megaco codec's encoding & decoding time's
%%
%% Measurement process consists of:
%%   For each message in a directory:
%%     Pre:         Read message from the file, close the file
%%     Measurement: 1) measure decode
%%                  2) measure encode (of the previously decoded message)
%%     Post:        Print average
%%   For each directory: 
%%     A summery is written, both to the console and to a file, 
%%     in an excel compatible format.
%%
%% megaco_codec_meas:t().
%% megaco_codec_meas:t([pretty, compact]).
%% megaco_codec_meas:t([per, pretty, compact]).
%%
%%----------------------------------------------------------------------

-module(megaco_codec_meas).


%% API
-export([t/0, t1/0, t/1]).

%% Internal exports
-export([do_measure_codec/6, do_measure_codec_loop/6]).
-export([flex_scanner_handler/1]).


-include_lib("kernel/include/file.hrl").


-define(MEASURE_TIMEOUT, 100000).

-ifndef(MEASURE_COUNT_TIME).
-define(MEASURE_COUNT_TIME, 2*1000*1000). % 2 seconds
-endif.

-ifndef(MEASURE_TIME).
-define(MEASURE_TIME, 10000).
-endif.

-ifndef(MEASURE_CODECS).
-define(MEASURE_CODECS, [pretty, compact, per, ber, erlang]).
-endif.


-record(stat, {file, ecount, etime, dcount, dtime, size}).


%% Runs the measurement on all "official" codecs

t1() ->
    put(everbose,true),
    t().

t() ->
    t(?MEASURE_CODECS).


%% Dirs is a list of directories containing files,
%% each with a single megaco message. 
%%
%% Note that it is a requirement that each dir has
%% the name of the codec with which the messages has
%% been encoded: 
%%
%%    pretty | compact | ber | per | erlang
%%

t(Dirs) when list(Dirs) ->
    io:format("~n", []),
    EDirs = expand_dirs(Dirs, []),
    Results = t1(EDirs, []), 
    store_results(Results).


    
t1([], Results) ->
    lists:reverse(Results);
t1([{Dir, Codec, Conf, _} = EDir|Dirs], Results) ->
    case (catch measure(EDir)) of
	{'EXIT', Reason} ->
	    error("measure of codec ~p exited: ~n~p", [Codec, Reason]),
	    t1(Dirs, Results);
	{error, Reason} ->
	    error("skipping codec ~p: ~n~p", [Codec, Reason]),
	    t1(Dirs, Results);
	{ok, Res} ->
	    t1(Dirs, [{Dir, Conf, Res}| Results])
    end.


measure({Dir, Codec, Conf, Count}) when list(Dir) ->
    io:format("measure using codec ~p ~p~n  ", [Codec, Conf]),
    {Init, Conf1} = measure_init(Conf),
    Res = measure(Dir, Codec, Conf1, read_files(Dir), [], Count),
    measure_fin(Init),
    Res.


expand_dirs([], EDirs) ->
    lists:reverse(lists:flatten(EDirs));
expand_dirs([Dir|Dirs], EDirs) when atom(Dir) ->
    EDir = expand_dir(atom_to_list(Dir)),
    expand_dirs(Dirs, [EDir|EDirs]);
expand_dirs([Dir|Dirs], EDirs) when list(Dir) ->
    EDir = expand_dir(Dir),
    expand_dirs(Dirs, [EDir|EDirs]).

expand_dir(Dir) ->
    case Dir of
	"pretty" ->
	    [{Dir, megaco_pretty_text_encoder, [flex_scanner], 5000},
	     {Dir, megaco_pretty_text_encoder, [], 1000}];
	"compact" ->
	    [{Dir, megaco_compact_text_encoder, [flex_scanner], 5000},
	     {Dir, megaco_compact_text_encoder, [], 1500}];
	"ber" ->
	    [{Dir, megaco_ber_bin_encoder, [], 800},
	     {Dir, megaco_ber_bin_encoder, [native], 500}];
	"per" ->
	    [{Dir, megaco_per_bin_encoder, [], 800},
	     {Dir, megaco_per_bin_encoder, [native], 500}];
	"erlang" ->
	    [{Dir, megaco_erl_dist_encoder, [], 10000}];
	Else ->
	    exit({error, {invalid_codec, Else}})
    end.


read_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:sort(Files);
        Error ->
            exit(Error)
    end.

measure_init([flex_scanner]) ->
    start_flex_scanner();
measure_init(Conf) ->
    {undefined, Conf}.


measure_fin(Pid) when pid(Pid) ->
    stop_flex_scanner(Pid),
    ok;
measure_fin(_) ->
    ok.


measure(_Dir, _Codec, _Conf, [], Res, _MCount) ->

    Eavg = avg([Etime/Ecnt || #stat{ecount = Ecnt, etime = Etime} <- Res]),
    Davg = avg([Dtime/Dcnt || #stat{dcount = Dcnt, dtime = Dtime} <- Res]),
    Savg = avg([Size       || #stat{size = Size} <- Res]),

    io:format("~n  Measurment on ~p messages:"
	      "~n  Average size:   ~w bytes, "
	      "~n          encode: ~w microsec, "
	      "~n          decode: ~w microsec~n~n", 
	      [length(Res), Savg, Eavg, Davg]),

    {ok, lists:reverse(Res)};

measure(Dir, Codec, Conf, [File|Files], Results, MCount) ->
    io:format("~s ", [File]),
    case (catch do_measure(Dir, Codec, Conf, File, MCount)) of
	{ok, Stat} ->
	    measure(Dir, Codec, Conf, Files, [Stat | Results], MCount);

	{error, S} ->
	    case get(everbose) of
		true ->
		    io:format("~n", []),
		    error(S,[]);
		_ ->
		    io:format("~n~s failed~n", [File])
	    end,
	    measure(Dir, Codec, Conf, Files, Results, MCount);

	{info, S} ->
	    case get(verbose) of
		true ->
		    io:format("~n", []),
		    info(S,[]);
		_ ->
		    io:format("~n~s skipped~n", [File])
	    end,
	    measure(Dir, Codec, Conf, Files, Results, MCount)

    end.


do_measure(Dir, Codec, Conf, File, MCount) ->
    BinMsg             = read_message(Dir, File),
    {Msg, Dcnt, Dtime} = measure_decode(Codec, Conf, BinMsg, MCount),
    {_,   Ecnt, Etime} = measure_encode(Codec, Conf, Msg, MCount),

    {ok, #stat{file = File, 
	       ecount = Ecnt, etime = Etime, 
	       dcount = Dcnt, dtime = Dtime, 
	       size = size(BinMsg)}}.


read_message(Dir, FileName) ->
    File = filename:join([Dir, FileName]),
    case file:read_file_info(File) of
        {ok, #file_info{size = Sz, type = regular}} when Sz > 0 ->
            case file:read_file(File) of
                {ok, Msg} ->
		    %% io:format(".", []),
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


measure_decode(Codec, Conf, Bin, MCount) ->
    case measure_codec(Codec, decode_message, Conf, Bin, MCount) of
	{ok, Res} ->
	    Res;
	{error, Reason} ->
	    S = format("decode failed for ~p:~n~p", [Codec, Reason]),
	    throw({error, S})
    end.


measure_encode(Codec, Conf, Bin, MCount) ->
    case measure_codec(Codec, encode_message, Conf, Bin, MCount) of
	{ok, Res} ->
	    Res;
	{error, Reason} ->
	    S = format("encode failed for ~p:~n~p", [Codec, Reason]),
	    throw({error, S})
    end.


measure_codec(Codec, Func, Conf, Bin, MCount) ->
    Pid = spawn_link(?MODULE, do_measure_codec, 
                     [self(), Codec, Func, Conf, Bin, MCount]),
    receive
	{measure_result, Pid, Func, Res} ->
	    {ok, Res};
	{error, Pid, Error} ->
	    {error, Error};
	Else ->
	    {error, {unexpected_result, Else}}
    after ?MEASURE_TIMEOUT ->
	    {error, timeout}
    end.


do_measure_codec(Parent, Codec, Func, Conf, Bin, MCount) ->
    {ok, Count} = measure_warmup(Codec, Func, Conf, Bin, MCount),
    Res = timer:tc(?MODULE, do_measure_codec_loop, 
		   [Codec, Func, Conf, Bin, Count, dummy]),
    case Res of
	{Time, {ok, M}} ->
	    %% io:format("~w ", [Time]),
	    Parent ! {measure_result, self(), Func, {M, Count, Time}};
	{Time, Error} ->
	    Parent ! {error, self(), Error}
    end,
    unlink(Parent). % Make sure Parent don't get our exit signal


%% This function does more mor less what the real measure function
%% above does. But with the diff:
%% 1) Warmup to ensure that all used code are loaded
%% 2) To aproximate the encoding time, to ensure that 
%%    the real encode is done with enough iterations.
measure_warmup(Codec, Func, Conf, M, MCount) ->
    Res = timer:tc(?MODULE, do_measure_codec_loop, 
		   [Codec, Func, Conf, M, MCount, dummy]),
    case Res of
	{Time, {ok, _}} ->
	    %% OK so far, now calculate the count:
	    Count = round(?MEASURE_COUNT_TIME/(Time/MCount)),
	    %% io:format("~w ", [Count]),
	    {ok, Count};
	{_Time, Error} ->
	    {error, {warmup_failed, Error}}
    end.


do_measure_codec_loop(_Codec, _Func, _Conf, _Bin, 0, M) ->
    {ok, M};
do_measure_codec_loop(Codec, Func, Conf, Bin, Count, _) ->
    {ok, M} = apply(Codec, Func, [Conf, Bin]),
    do_measure_codec_loop(Codec, Func, Conf, Bin, Count - 1, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_results(Results) ->
    io:format("storing: ~n", []),    
    store_excel_message_size(Results),
    store_excel_decode_time(Results),
    store_excel_encode_time(Results),
    store_excel_total_time(Results),
    io:format("~n", []),
    ok.


store_excel_message_size(Res) ->
    Filename = "message_size.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Sizes = message_sizes(Res, []),
    store_excel_tab(Fd, Sizes),
    ok.

store_excel_decode_time(Res) ->
    Filename = "decode_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Decodes = dec_times(Res, []),
    store_excel_tab(Fd, Decodes),
    ok.

store_excel_encode_time(Res) ->
    Filename = "encode_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Encodes = enc_times(Res, []),
    store_excel_tab(Fd, Encodes),
    ok.

store_excel_total_time(Res) ->
    Filename = "total_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Totals = tot_times(Res, []),
    store_excel_tab(Fd, Totals),
    ok.


message_sizes([], Sizes) ->
    lists:reverse(Sizes);
message_sizes([{Dir, Conf, Res}|T], Acc) ->
    Sizes = [Size || #stat{size = Size} <- Res],
    Avg   = avg(Sizes),
    message_sizes(T, [{Dir, Conf, Avg, Sizes}|Acc]).

dec_times([], Times) ->
    lists:reverse(Times);
dec_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [Time/Count || #stat{dcount = Count, dtime = Time} <- Res],
    Avg   = avg(Times),
    dec_times(T, [{Dir, Conf, Avg, Times}|Acc]).

enc_times([], Times) ->
    lists:reverse(Times);
enc_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [Time/Count || #stat{ecount = Count, etime = Time} <- Res],
    Avg   = avg(Times),
    enc_times(T, [{Dir, Conf, Avg, Times}|Acc]).

tot_times([], Times) ->
    lists:reverse(Times);
tot_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [(Etime/Ecnt)+(Dtime/Dcnt) || #stat{ecount = Ecnt, 
						etime  = Etime, 
						dcount = Dcnt, 
						dtime  = Dtime} <- Res],
    Avg   = avg(Times),
    tot_times(T, [{Dir, Conf, Avg, Times}|Acc]).


avg(Vals) ->
    round(lists:sum(Vals)/length(Vals)).


store_excel_tab(Fd, []) ->
    ok; % Just in case there was something wrong with the test
store_excel_tab(Fd, Res) ->
    %% For all elements of this list, the Values is of the same length...
    [{_, _, _, Values}|_] = Res,
    store_excel_tab_header(Fd, length(Values), 1),
    store_excel_tab1(Fd, Res).

store_excel_tab1(Fd, []) ->
    io:format(Fd, "~n", []);
store_excel_tab1(Fd, [{Dir, [], Avg, Values}|T]) ->
    io:format(Fd, "~s (~w)", [filename:basename(Dir), Avg]),
    store_excel_tab_row(Fd, Values),
    store_excel_tab1(Fd, T);
store_excel_tab1(Fd, [{Dir, [Conf], Avg, Values}|T]) when atom(Conf) ->
    io:format(Fd, "~s_~w (~w)", [filename:basename(Dir), Conf, Avg]),
    store_excel_tab_row(Fd, Values),
    store_excel_tab1(Fd, T).

store_excel_tab_header(Fd, 0, _) ->
    io:format(Fd, "~n", []);
store_excel_tab_header(Fd, N, M) ->
    io:format(Fd, "\t~w", [M]),
    store_excel_tab_header(Fd, N-1, M+1).

store_excel_tab_row(Fd, []) ->
    io:format(Fd, "~n", []);
store_excel_tab_row(Fd, [Value|Values]) ->
    io:format(Fd, "\t~w", [round(Value)]),
    store_excel_tab_row(Fd, Values).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_flex_scanner() ->
    Pid = proc_lib:spawn(?MODULE, flex_scanner_handler, [self()]),
    receive
        {flex_scanner_started, Pid, Conf} ->
            {Pid, [Conf]};
        {flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
            throw({error, {failed_loading_flex_scanner_driver, Reason}});
        {flex_scanner_error, Reason} ->
            throw({error, {failed_loading_flex_scanner_driver, Reason}})
    after 10000 ->
            exit(Pid, kill),
            throw({error, {failed_starting_flex_scanner, timeout}})
    end.

stop_flex_scanner(Pid) ->
    Pid ! stop_flex_scanner.

flex_scanner_handler(Pid) ->
    case (catch megaco_flex_scanner:start()) of
        {ok, Port} when port(Port) ->
            Pid ! {flex_scanner_started, self(), {flex, Port}},
            flex_scanner_handler(Pid, Port);
        {error, {load_driver, {open_error, Reason}}} ->
            Error = {failed_loading_flex_scanner_driver, Reason},
            Pid ! {flex_scanner_error, Error},
            exit(Error);
        Else ->
            Error = {unknown_result_from_start_flex_scanner, Else},
            Pid ! {flex_scanner_error, Error},
            exit(Error)
    end.

flex_scanner_handler(Pid, Port) ->
    receive
        {ping, Pinger} ->
            Pinger ! {pong, self()},
            flex_scanner_handler(Pid, Port);
        {'EXIT', Port, Reason} ->
            Pid ! {flex_scanner_exit, Reason},
            exit({flex_scanner_exit, Reason});
        stop_flex_scanner ->
            megaco_flex_scanner:stop(Port),
            exit(normal);
        Other ->
            info("flex scanner handler got something:~n~p", [Other]),
            flex_scanner_handler(Pid, Port)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(F, A) ->
    io:format(F ++ "~n", A).


error(F, A) -> 
    io:format("ERROR: " ++ F ++ "~n", A).


format(F, A) ->    
    lists:flatten(io_lib:format(F, A)).
