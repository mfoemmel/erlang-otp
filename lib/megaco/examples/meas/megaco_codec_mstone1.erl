%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: mstone measurement
%%
%%----------------------------------------------------------------------

-module(megaco_codec_mstone1).


%% API
-export([start/0,          start/1,
	 start_flex/0,     start_flex/1, 
	 start_no_drv/0,   start_no_drv/1, 
	 start_only_drv/0, start_only_drv/1]).

%% Internal exports
-export([mstone_runner_init/4]).


-define(LIB, megaco_codec_mstone_lib).

-ifndef(MSTONE_TIME).
-define(MSTONE_TIME, 10).
-endif.
-define(MSTONE_RUN_TIME, timer:minutes(?MSTONE_TIME)).

-ifndef(MSTONE_VERSION3).
-define(MSTONE_VERSION3, v3).
-endif.
-define(VERSION3, ?MSTONE_VERSION3).

-ifndef(MSTONE_CODECS).
-define(MSTONE_CODECS, [pretty, compact, per, ber, erlang]).
-endif.

%% -define(VERBOSE_STATS,true).

-ifndef(MSTONE_RUNNER_MIN_HEAP_SZ).
-define(MSTONE_RUNNER_MIN_HEAP_SZ,  16#ffff).
-endif.
-define(MSTONE_RUNNER_OPTS, 
        [link, {min_heap_size, ?MSTONE_RUNNER_MIN_HEAP_SZ}]).

-record(mstone, {id, count, codec, econf, heap_size, reds}).


start() ->
    start(1).

start(Factor) ->
    do_start(Factor, ignore).


start_flex() ->
    start_flex(1).

start_flex(Factor) ->
    do_start(Factor, flex).


start_only_drv() ->
    start_only_drv(1).

start_only_drv(Factor) ->
    do_start(Factor, only_drv).


start_no_drv() ->
    start_no_drv(1).

start_no_drv(Factor) ->
    do_start(Factor, no_drv).


do_start(Factor, DrvInclude) when is_integer(Factor) andalso (Factor > 0) ->
    t(Factor, ?MSTONE_CODECS, DrvInclude);
do_start([FactorAtom], DrvInclude) when is_atom(FactorAtom) ->
    case (catch list_to_integer(atom_to_list(FactorAtom))) of
	Factor when is_integer(Factor) ->
	    t(Factor, ?MSTONE_CODECS, DrvInclude);
	_ ->
	    io:format("ERROR: Invalid factor value: ~p~n", [FactorAtom]),
	    ok
    end;
do_start(Crap, _) ->    
    io:format("ERROR: Invalid argument: ~p~n", [Crap]),
    ok.

%% Dirs is a list of directories containing files,
%% each with a single megaco message. 
%%
%% Note that it is a requirement that each dir has
%% the name of the codec with which the messages has
%% been encoded: 
%%
%%    pretty | compact | ber | per | erlang
%%

t(Factor, Dirs, DrvInclude) ->
    io:format("~n", []),
    ?LIB:display_os_info(),
    ?LIB:display_system_info(),
    ?LIB:display_app_info(),
    io:format("~n", []),
    {Pid, Conf} = ?LIB:start_flex_scanner(),
    put(flex_scanner_conf, Conf),
    EDirs  = duplicate(Factor, ?LIB:expand_dirs(Dirs, DrvInclude)),
    MStone = t1(EDirs),
    ?LIB:stop_flex_scanner(Pid),
    io:format("~n", []),
    io:format("MStone: ~p~n", [MStone]).

duplicate(N, Elements) ->
    duplicate(N, Elements, []).

duplicate(_N, [], Acc) ->
    lists:flatten(Acc);
duplicate(N, [H|T], Acc) ->
    duplicate(N, T, [lists:duplicate(N, H)|Acc]).

t1(EDirs) ->
    io:format("starting runners [~w] ", [length(EDirs)]),
    t1(EDirs, []).

t1([], Runners) ->
    io:format(" done~nawait runners ready ", []),
    await_runners_ready(Runners),
    io:format(" done~nrelease them~n", []),
    lists:foreach(fun(P) -> P ! {go, self()} end, Runners),
    t2(1, [], Runners);
t1([H|T], Runners) ->
    Runner = init_runner(H),
    io:format(".", []),
    t1(T, [Runner|Runners]).

await_runners_ready([]) ->
    ok;
await_runners_ready(Runners) ->
    receive
        {ready, Runner} ->
            io:format(".", []),
	    %% i("runner ~w ready", [Runner]),
            await_runners_ready(lists:delete(Runner, Runners));
	{'EXIT', Pid, Reason} ->
	    case lists:member(Pid, Runners) of
		true ->
		    io:format("~nERROR: "
			      "received exit signal from from runner ~p:"
			      "~n~p~n", [Pid, Reason]),
		    exit(Reason);
		false ->
		    await_runners_ready(Runners)
	    end
    end.

-ifdef(VERBOSE_STATS).
print_runner_stats(RunnerStats) ->
    Sorted = lists:keysort(2, RunnerStats),
    lists:foreach(fun(#mstone{id        = Id,
			      count     = Num,
                              codec     = Codec,
                              econf     = EConf, 
                              heap_size = HeapSz, 
                              reds      = Reds}) ->
			  i("runner: ~w"
			    "~n   Count:           ~w"
			    "~n   Codec:           ~w"
			    "~n   Encoding config: ~p"
			    "~n   Heap size:       ~p"
			    "~n   Reductions:      ~p", 
			    [Id, Num, Codec, EConf, HeapSz, Reds]) end, 
                  Sorted),
    ok.
-else.
print_runner_stats(_) ->
    ok.
-endif.

t2(_, Acc, []) ->
    i("~w runners", [length(Acc)]),
    print_runner_stats(Acc),

    HeapSzAcc = lists:sort([HS || #mstone{heap_size = HS} <- Acc]),
    i("Runner heap size data:"
      "~n   Min: ~w"
      "~n   Max: ~w"
      "~n   Avg: ~w", 
      [hd(HeapSzAcc), 
       hd(lists:reverse(HeapSzAcc)), 
       (lists:sum(HeapSzAcc) div length(HeapSzAcc))]), 
    
    RedsAcc   = lists:sort([R || #mstone{reds = R} <- Acc]),
    i("Runner reductions data:"
      "~n   Min: ~w"
      "~n   Max: ~w"
      "~n   Avg: ~w", 
      [hd(RedsAcc), 
       hd(lists:reverse(RedsAcc)), 
       (lists:sum(RedsAcc) div length(RedsAcc))]), 

    lists:sum([Num || #mstone{count = Num} <- Acc]);
t2(N, Acc, Runners) ->
    receive
	{'EXIT', Pid, {runner_done, Codec, Conf, Num, Info}} ->
            {value, {_, HeapSz}} = lists:keysearch(heap_size,  1, Info),
            {value, {_, Reds}}   = lists:keysearch(reductions, 1, Info),
%% 	    i("runner ~w done: ~w"
%% 	      "~n   Codec:           ~w"
%% 	      "~n   Encoding config: ~p"
%% 	      "~n   Heap size:       ~p"
%% 	      "~n   Reductions:      ~p", 
%%               [Pid, Num, Codec, Conf, HeapSz, Reds]),
            MStone = #mstone{id        = N,
			     count     = Num,
                             codec     = Codec,
                             econf     = Conf,
                             heap_size = HeapSz,
                             reds      = Reds},
	    t2(N + 1, [MStone|Acc], lists:delete(Pid, Runners))
    end.

init_runner({Dir, Codec, Conf}) ->
    Conf1 = runner_conf(Conf),
    Conf2 = [{version3,?VERSION3}|Conf1],
    Pid   = spawn_opt(?MODULE, mstone_runner_init, 
		      [self(), Codec, Conf2, Dir],
		      ?MSTONE_RUNNER_OPTS),
    Pid.

runner_conf([flex_scanner]) ->
    get(flex_scanner_conf);
runner_conf(Conf) ->
    Conf.



detect_versions(Codec, _Conf, [], []) ->
    exit({no_messages_found_for_codec, Codec});
detect_versions(_Codec, _Conf, [], Acc) ->
    lists:reverse(Acc);
detect_versions(Codec, Conf, [Bin|Bins], Acc) ->
    Data = ?LIB:detect_version(Codec, Conf, Bin),
    detect_versions(Codec, Conf, Bins, [Data|Acc]).
	    

mstone_runner_init(Parent, Codec, Conf, Dir) ->
    Msgs = detect_versions(Codec, Conf, ?LIB:read_messages(Dir), []),
    warmup(Codec, Conf, Msgs, []),
    Parent ! {ready, self()},
    receive
        {go, Parent} ->
            ok
    end,
    erlang:send_after(?MSTONE_RUN_TIME, self(), stop),
    mstone_runner_loop(Parent, Codec, Conf, 0, Msgs).

mstone_runner_loop(Parent, Codec, Conf, N, Msgs1) ->
    receive
        stop ->
            exit({runner_done, Codec, Conf, N, mstone_runner_process_info()})
    after 0 ->
        {Inc, Msgs2} = mstone_all(Codec, Conf, Msgs1, []),
	mstone_runner_loop(Parent, Codec, Conf, N+Inc, Msgs2)
    end.

mstone_runner_process_info() ->
    PI = process_info(self()),
    FL = [heap_size, stack_size, reductions],
    lists:filter(fun({Key, _}) -> lists:member(Key, FL) end, PI).
			  
		      
mstone_all(_Codec, _Conf, [], Acc) ->
    {length(Acc), lists:reverse(Acc)};
mstone_all(Codec, Conf, [{V, Bin}|Bins], Acc) when is_binary(Bin) ->
    {ok, Msg} = apply(Codec, decode_message, [Conf, V, Bin]),
    mstone_all(Codec, Conf, Bins, [{V, Msg}|Acc]);
mstone_all(Codec, Conf, [{V, Msg}|Msgs], Acc) ->
    {ok, Bin} = apply(Codec, encode_message, [Conf, V, Msg]),
    mstone_all(Codec, Conf, Msgs, [{V, Bin}|Acc]).


warmup(_Codec, _Conf, [], Acc) ->
    lists:reverse(Acc);
warmup(Codec, Conf, [{V, M}|Msgs], Acc) ->
    case (catch apply(Codec, decode_message, [Conf, V, M])) of
        {ok, Msg} ->
            case (catch apply(Codec, encode_message, [Conf, V, Msg])) of
                {ok, Bin} ->
                    warmup(Codec, Conf, Msgs, [Bin|Acc]);
                EncodeError ->
                    emsg("failed encoding message: ~n~p", [EncodeError])
            end;
        DecodeError ->
            emsg("failed decoding message: ~n~p", [DecodeError])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emsg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

%% i(F) ->
%%     i(F, []).
i(F, A) ->
    io:format(F ++ "~n", A).

