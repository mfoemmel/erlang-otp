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
%% Purpose: Test encoding/decoding (codec) module of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_codec_test).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([tt/1, tt/2]).
-export([tt_official/1, tt_all/1, tt_texts/1, tt_bins/1]).

-export([t/0, t/1]).

-export([tickets/0]).

-export([all/1, 

	 text/1, 

	 pretty/1, 
	 pretty_msg1a/1, pretty_msg1b/1, 
	 pretty_msg2/1, 
	 pretty_msg3/1, 
	 pretty_msg4/1, 
	 pretty_msg5/1, 
	 pretty_msg6a/1, pretty_msg6b/1, 
	 pretty_msg7/1, 
	 pretty_msg8a/1, pretty_msg8b/1, 
	 pretty_msg9/1, 
	 pretty_msg10/1, 
	 pretty_msg11/1, 
	 pretty_msg12/1, 
	 pretty_msg13/1, 
	 pretty_msg14/1, 
	 pretty_msg15/1, 
	 pretty_msg16/1, 
	 pretty_msg17/1, 
	 pretty_msg18/1, 
	 pretty_msg19/1, 
	 pretty_msg20/1, 
	 pretty_msg21/1, 
	 pretty_msg22a/1, 
	 pretty_msg22b/1, 
	 pretty_msg22c/1, 
	 pretty_msg22d/1, 
	 pretty_msg22e/1, 
	 pretty_msg22f/1, 
	 pretty_msg23/1, 
	 pretty_msg24/1, 
	 pretty_msg25/1, 

	 compact/1, 
	 compact_msg1a/1, compact_msg1b/1, 
	 compact_msg2/1, 
	 compact_msg3/1, 
	 compact_msg4/1, 
	 compact_msg5/1, 
	 compact_msg6a/1, compact_msg6b/1, 
	 compact_msg7/1, 
	 compact_msg8a/1, compact_msg8b/1, 
	 compact_msg9/1, 
	 compact_msg10/1, 
	 compact_msg11/1, 
	 compact_msg12/1, 
	 compact_msg13/1, 
	 compact_msg14/1, 
	 compact_msg15/1, 
	 compact_msg16/1, 
	 compact_msg17/1, 
	 compact_msg18/1, 
	 compact_msg19/1, 
	 compact_msg20/1, 
	 compact_msg21/1, 
	 compact_msg22a/1, 
	 compact_msg22b/1, 
	 compact_msg22c/1, 
	 compact_msg22d/1, 
	 compact_msg22e/1, 
	 compact_msg22f/1, 
	 compact_msg23/1, 
	 compact_msg24/1, 
	 compact_msg25/1, 

	 flex_pretty/1, 
	 flex_pretty_init/1, 
	 flex_pretty_finish/1, 
	 flex_pretty_msg1a/1, flex_pretty_msg1b/1, 
	 flex_pretty_msg2/1, 
	 flex_pretty_msg3/1, 
	 flex_pretty_msg4/1, 
	 flex_pretty_msg5/1, 
	 flex_pretty_msg6a/1, flex_pretty_msg6b/1, 
	 flex_pretty_msg7/1, 
	 flex_pretty_msg8a/1, flex_pretty_msg8b/1, 
	 flex_pretty_msg9/1, 
	 flex_pretty_msg10/1, 
	 flex_pretty_msg11/1, 
	 flex_pretty_msg12/1, 
	 flex_pretty_msg13/1, 
	 flex_pretty_msg14/1, 
	 flex_pretty_msg15/1, 
	 flex_pretty_msg16/1, 
	 flex_pretty_msg17/1, 
	 flex_pretty_msg18/1, 
	 flex_pretty_msg19/1, 
	 flex_pretty_msg20/1, 
	 flex_pretty_msg21/1, 
	 flex_pretty_msg22a/1, 
	 flex_pretty_msg22b/1, 
	 flex_pretty_msg22c/1, 
	 flex_pretty_msg22d/1, 
	 flex_pretty_msg22e/1, 
	 flex_pretty_msg22f/1, 
	 flex_pretty_msg23/1, 
	 flex_pretty_msg24/1, 
	 flex_pretty_msg25/1, 

	 flex_compact/1, 
	 flex_compact_init/1, 
	 flex_compact_finish/1, 
	 flex_compact_msg1a/1, flex_compact_msg1b/1, 
	 flex_compact_msg2/1, 
	 flex_compact_msg3/1, 
	 flex_compact_msg4/1, 
	 flex_compact_msg5/1, 
	 flex_compact_msg6a/1, flex_compact_msg6b/1, 
	 flex_compact_msg7/1, 
	 flex_compact_msg8a/1, flex_compact_msg8b/1, 
	 flex_compact_msg9/1, 
	 flex_compact_msg10/1, 
	 flex_compact_msg11/1, 
	 flex_compact_msg12/1, 
	 flex_compact_msg13/1, 
	 flex_compact_msg14/1, 
	 flex_compact_msg15/1, 
	 flex_compact_msg16/1, 
	 flex_compact_msg17/1, 
	 flex_compact_msg18/1, 
	 flex_compact_msg19/1, 
	 flex_compact_msg20/1, 
	 flex_compact_msg21/1, 
	 flex_compact_msg22a/1, 
	 flex_compact_msg22b/1, 
	 flex_compact_msg22c/1, 
	 flex_compact_msg22d/1, 
	 flex_compact_msg22e/1, 
	 flex_compact_msg22f/1, 
	 flex_compact_msg23/1, 
	 flex_compact_msg24/1, 
	 flex_compact_msg25/1, 

	 flex_compact_dm_timers1/1, 
	 flex_compact_dm_timers2/1, 
	 flex_compact_dm_timers3/1, 
	 flex_compact_dm_timers4/1, 
	 flex_compact_dm_timers5/1, 
	 flex_compact_dm_timers6/1, 

	 binary/1, 
	 
	 bin/1, 
	 bin_msg1a/1, bin_msg1b/1, 
	 bin_msg2/1, 
	 bin_msg3/1, 
	 bin_msg4/1, 
	 bin_msg5/1, 
	 bin_msg6a/1, bin_msg6b/1, 
	 bin_msg7/1, 
	 bin_msg8a/1, bin_msg8b/1, 
	 bin_msg9/1, 
	 bin_msg10/1, 
	 bin_msg11/1, 
	 bin_msg12/1, 
	 bin_msg13/1, 
	 bin_msg14/1, 
	 bin_msg15/1, 
	 bin_msg16/1, 
	 bin_msg17/1, 
	 bin_msg18/1, 
	 bin_msg19/1, 
	 bin_msg20/1, 
	 bin_msg21/1, 
	 bin_msg22a/1, 
	 bin_msg22b/1, 
	 bin_msg22c/1, 
	 bin_msg22d/1, 
	 bin_msg22e/1, 
	 bin_msg22f/1, 
	 bin_msg23/1, 
	 bin_msg24/1, 
	 bin_msg25/1, 

	 ber/1, 
	 ber_msg1a/1, ber_msg1b/1, 
	 ber_msg2/1, 
	 ber_msg3/1, 
	 ber_msg4/1, 
	 ber_msg5/1, 
	 ber_msg6a/1, ber_msg6b/1, 
	 ber_msg7/1, 
	 ber_msg8a/1, ber_msg8b/1, 
	 ber_msg9/1, 
	 ber_msg10/1, 
	 ber_msg11/1, 
	 ber_msg12/1, 
	 ber_msg13/1, 
	 ber_msg14/1, 
	 ber_msg15/1, 
	 ber_msg16/1, 
	 ber_msg17/1, 
	 ber_msg18/1, 
	 ber_msg19/1, 
	 ber_msg20/1, 
	 ber_msg21/1, 
	 ber_msg22a/1, 
	 ber_msg22b/1, 
	 ber_msg22c/1, 
	 ber_msg22d/1, 
	 ber_msg22e/1, 
	 ber_msg22f/1, 
	 ber_msg23/1, 
	 ber_msg24/1, 
	 ber_msg25/1, 

	 ber_bin/1, 
	 ber_bin_msg1a/1, ber_bin_msg1b/1, 
	 ber_bin_msg2/1, 
	 ber_bin_msg3/1, 
	 ber_bin_msg4/1, 
	 ber_bin_msg5/1, 
	 ber_bin_msg6a/1, ber_bin_msg6b/1, 
	 ber_bin_msg7/1, 
	 ber_bin_msg8a/1, ber_bin_msg8b/1, 
	 ber_bin_msg9/1, 
	 ber_bin_msg10/1, 
	 ber_bin_msg11/1, 
	 ber_bin_msg12/1, 
	 ber_bin_msg13/1, 
	 ber_bin_msg14/1, 
	 ber_bin_msg15/1, 
	 ber_bin_msg16/1, 
	 ber_bin_msg17/1, 
	 ber_bin_msg18/1, 
	 ber_bin_msg19/1, 
	 ber_bin_msg20/1, 
	 ber_bin_msg21/1, 
	 ber_bin_msg22a/1, 
	 ber_bin_msg22b/1, 
	 ber_bin_msg22c/1, 
	 ber_bin_msg22d/1, 
	 ber_bin_msg22e/1, 
	 ber_bin_msg22f/1, 
	 ber_bin_msg23/1, 
	 ber_bin_msg24/1, 
	 ber_bin_msg25/1, 

	 per/1,
	 per_msg1a/1, per_msg1b/1, 
	 per_msg2/1, 
	 per_msg3/1, 
	 per_msg4/1, 
	 per_msg5/1, 
	 per_msg6a/1, per_msg6b/1, 
	 per_msg7/1, 
	 per_msg8a/1, per_msg8b/1, 
	 per_msg9/1, 
	 per_msg10/1, 
	 per_msg11/1, 
	 per_msg12/1, 
	 per_msg13/1, 
	 per_msg14/1, 
	 per_msg15/1, 
	 per_msg16/1, 
	 per_msg17/1, 
	 per_msg18/1, 
	 per_msg19/1, 
	 per_msg20/1, 
	 per_msg21/1, 
	 per_msg22a/1, 
	 per_msg22b/1, 
	 per_msg22c/1, 
	 per_msg22d/1, 
	 per_msg22e/1, 
	 per_msg22f/1, 
	 per_msg23/1, 
	 per_msg24/1, 
	 per_msg25/1, 

	 per_bin/1,
	 per_bin_msg1a/1, per_bin_msg1b/1, 
	 per_bin_msg2/1, 
	 per_bin_msg3/1, 
	 per_bin_msg4/1, 
	 per_bin_msg5/1, 
	 per_bin_msg6a/1, per_bin_msg6b/1, 
	 per_bin_msg7/1, 
	 per_bin_msg8a/1, per_bin_msg8b/1, 
	 per_bin_msg9/1, 
	 per_bin_msg10/1, 
	 per_bin_msg11/1, 
	 per_bin_msg12/1, 
	 per_bin_msg13/1, 
	 per_bin_msg14/1, 
	 per_bin_msg15/1, 
	 per_bin_msg16/1, 
	 per_bin_msg17/1, 
	 per_bin_msg18/1, 
	 per_bin_msg19/1, 
	 per_bin_msg20/1, 
	 per_bin_msg21/1, 
	 per_bin_msg22a/1, 
	 per_bin_msg22b/1, 
	 per_bin_msg22c/1, 
	 per_bin_msg22d/1, 
	 per_bin_msg22e/1, 
	 per_bin_msg22f/1, 
	 per_bin_msg23/1, 
	 per_bin_msg24/1, 
	 per_bin_msg25/1, 

% 	 per_bin_opt/1,
% 	 per_bin_opt_msg1a/1, per_bin_opt_msg1b/1, 
% 	 per_bin_opt_msg2/1, 
% 	 per_bin_opt_msg3/1, 
% 	 per_bin_opt_msg4/1, 
% 	 per_bin_opt_msg5/1, 
% 	 per_bin_opt_msg6a/1, per_bin_opt_msg6b/1, 
% 	 per_bin_opt_msg7/1, 
% 	 per_bin_opt_msg8a/1, per_bin_opt_msg8b/1, 
% 	 per_bin_opt_msg9/1, 
% 	 per_bin_opt_msg10/1, 
% 	 per_bin_opt_msg11/1, 
% 	 per_bin_opt_msg12/1, 
% 	 per_bin_opt_msg13/1, 
% 	 per_bin_opt_msg14/1, 
% 	 per_bin_opt_msg15/1, 
% 	 per_bin_opt_msg16/1, 
% 	 per_bin_opt_msg17/1, 
% 	 per_bin_opt_msg18/1, 
% 	 per_bin_opt_msg19/1, 
% 	 per_bin_opt_msg20/1, 
% 	 per_bin_opt_msg21/1, 
% 	 per_bin_opt_msg22a/1, 
% 	 per_bin_opt_msg22b/1, 
% 	 per_bin_opt_msg22c/1, 
% 	 per_bin_opt_msg22d/1, 
% 	 per_bin_opt_msg22e/1, 
% 	 per_bin_opt_msg22f/1, 
% 	 per_bin_opt_msg23/1, 
% 	 per_bin_opt_msg24/1, 
% 	 per_bin_opt_msg25/1, 

	 tickets/1, 

	 compact_tickets/1, 
	 compact_otp4011_msg1/1, 
	 compact_otp4011_msg2/1,
	 compact_otp4011_msg3/1,
	 compact_otp4013_msg1/1, 
	 compact_otp4085_msg1/1, 
	 compact_otp4085_msg2/1, 
	 compact_otp4280_msg1/1, 
	 compact_otp4299_msg1/1, 
	 compact_otp4299_msg2/1, 
	 compact_otp4359_msg1/1, 


	 time_test/1,
	 pretty_time_test/1,
	 flex_pretty_time_test/1,
	 compact_time_test/1,
	 flex_compact_time_test/1,
	 bin_time_test/1,
	 ber_time_test/1,
	 ber_bin_time_test/1,
	 per_time_test/1,
	 per_bin_time_test/1,
% 	 per_bin_opt_time_test/1,
	 erl_dist_time_test/1,
	 erl_dist_compressed_time_test/1,

	 init_per_testcase/2, fin_per_testcase/2]).  

-export([do_time_tester/2, do_time_tester_loop/3]).

-export([flex_scanner_handler/1]).


%% ----

-define(TIME_TEST_COUNT_TIME, 2*1000*1000). % 2 seconds
-define(TIME_TEST_TIMEOUT,    30000).       % 10 seconds


-define(DEFAULT_PORT, 55555).
-define(MG1_MID_NO_PORT, {ip4Address,
                          #'IP4Address'{address = [124, 124, 124, 222]}}).
-define(MG1_MID, {ip4Address, #'IP4Address'{address = [124, 124, 124, 222],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MG2_MID, {ip4Address, #'IP4Address'{address = [125, 125, 125, 111],
                                            portNumber = ?DEFAULT_PORT}}).
-define(MGC_MID, {ip4Address, #'IP4Address'{address = [123, 123, 123, 4],
                                            portNumber = ?DEFAULT_PORT}}).

-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).


expand(RootCase) ->
    expand([RootCase], []).

expand([], Acc) ->
    lists:flatten(lists:reverse(Acc));
expand([Case|Cases], Acc) ->
    case (catch apply(?MODULE,Case,[suite])) of
	[] ->
	    expand(Cases, [Case|Acc]);
	C when list(C) ->
	    expand(Cases, [expand(C, [])|Acc]);
	_ ->
	    expand(Cases, [Case|Acc])
    end.

	    
%% ----

tickets() ->
    Flag  = process_flag(trap_exit, true),    
    Cases = expand(tickets),
    Fun   = fun(Case) ->
		    C = init_per_testcase(Case, [{tc_timeout, 
						  timer:minutes(10)}]),
		    io:format("Eval ~w~n", [Case]),
		    Result = 
			case (catch apply(?MODULE, Case, [C])) of
			    {'EXIT', Reason} ->
 				io:format("~n~p exited:~n   ~p", 
 					  [Case, Reason]),
				{error, {Case, Reason}};
			    Res ->
				Res
			end,
		    fin_per_testcase(Case, C),
		    Result
	    end,
    process_flag(trap_exit, Flag),
    lists:map(Fun, Cases).


%% ----
%% To manually run the time test, in the test directory, type:
%% 
%% erl -sname megaco -pa ../../megaco/examples/simple -pa ../../megaco/ebin -pa ../../et/ebin
%% DelCases = [ber_time_test, bin_time_test, erl_dist_compressed_time_test],
%% megaco_codec_test:tt("/tmp", DelCases).
%%

do_tt(DeleteCases) ->
    Flag  = process_flag(trap_exit, true),    
    Cases = time_test(suite) -- DeleteCases,
    Fun   = fun(Case) ->
		    C = init_per_testcase(Case, [{tc_timeout, 
						  timer:minutes(10)}]),
		    Result = 
			case (catch apply(?MODULE, Case, [C])) of
			    {'EXIT', Reason} ->
 				io:format("~n~p exited:~n   ~p", 
 					  [Case, Reason]),
				{error, {Case, Reason}};
			    Res ->
				Res
			end,
		    fin_per_testcase(Case, C),
		    Result
	    end,
    process_flag(trap_exit, Flag),
    lists:map(Fun, Cases).


tt([Path]) when list(Path) ->
    tt(Path, []);
tt(Path) when list(Path) ->
    tt(Path, []).


tt(Path, DeleteCases) ->
    Res    = do_tt(DeleteCases),
    TopDir = filename:join(Path, "time_test") ++ "/",
    file:make_dir(TopDir),
    io:format("storing: ", []),
    tt_store(TopDir, Res),
    %% Formats: "postscript" | "png" | "pbm" 
    tt_store_gnuplot(TopDir, Res, "postscript"),
    tt_store_excel(TopDir, Res).


tt_official([Path]) when list(Path) ->
    tt_official(Path);
tt_official(Path) when list(Path) ->
    DelCases = [pretty, 
		compact,
		per_time_test, 
		%% per_bin_opt_time_test, 
		ber_time_test, 
		bin_time_test,
		erl_dist_compressed_time_test],
    tt(Path, DelCases).


tt_all([Path]) when list(Path) ->
    tt_all(Path);
tt_all(Path) when list(Path) ->
    tt(Path, []).


tt_texts([Path]) when list(Path) ->
    tt_texts(Path);
tt_texts(Path) when list(Path) ->
    DelCases = [ber_time_test, 
		bin_time_test, 
		ber_bin_time_test,
		per_time_test,
		per_bin_time_test,
		%% per_bin_opt_time_test,
		erl_dist_time_test,
		erl_dist_compressed_time_test],
    tt(Path, DelCases).


tt_bins([Path]) when list(Path) ->
    tt_bins(Path);
tt_bins(Path) when list(Path) ->
    %% BMK
    DelCases = [pretty_time_test,
		flex_pretty_time_test,
		compact_time_test,
		flex_compact_time_test,
		erl_dist_time_test,
		erl_dist_compressed_time_test],



    tt(Path, DelCases).


tt_store(TopDir, []) ->
    io:format("~n", []),
    ok;
tt_store(TopDir, [H|T]) ->
    io:format(".", []),
    tt_check_store(H, (catch tt_store_case(TopDir, H))),
    tt_store(TopDir, T).

tt_check_store(_, ok) ->
    ok;
tt_check_store({error, {Case, Reason}}, _) ->
    io:format("ERROR: test ~p failed: "
	      "~n   Reason: ~p~n", [Case, Reason]);
tt_check_store({ok, {Codec, _Res}}, {error, Reason}) ->
    io:format("ERROR: failed storing: "
	      "~n   Reason: ~p"
	      "~n   Codec:  ~p~n", [Reason, Codec]);
tt_check_store(O, {error, Reason}) ->
    io:format("ERROR: failed storing: "
	      "~n   Reason: ~p"
	      "~n   O:      ~p~n", [Reason, O]);
tt_check_store(CodecRes, StoreRes) ->
    io:format("ERROR: failed storing: "
	      "~n   StoreRes: ~p"
	      "~n   CodecRes: ~p~n", [StoreRes, CodecRes]).


tt_store_case(TopDir, {ok, {Case, Res}}) ->
    tt_store_case(TopDir, Case, Res);
tt_store_case(_, ok) ->
    ok.

tt_store_case(TopDir, Case, Res) ->
    Dir = TopDir ++ atom_to_list(Case),
    file:make_dir(Dir),
    tt_store_case_result(Dir, Res).

tt_store_case_result(Dir, Res) ->
    {value, {desc,   Desc}}  = lists:keysearch(desc,   1, Res),
    {value, {encode, Etime}} = lists:keysearch(encode, 1, Res),
    {value, {decode, Dtime}} = lists:keysearch(decode, 1, Res),
    {value, {size,   Size}}  = lists:keysearch(size,   1, Res),
    {value, {bins,   Bins}}  = lists:keysearch(bins,   1, Res),
    tt_store_case_text(Dir, Desc, Etime, Dtime, Size, Bins),
    tt_store_case_msgs(Dir, Bins).

tt_store_case_text(Dir, Desc, Etime, Dtime, Size, Bins) ->
    {ok, Fid} = file:open(filename:join(Dir,"result.txt"),[write]),
    io:format(Fid, 
	      "~s avg: "
	      "~n   Encode: ~p microsec" 
	      "~n   Decode: ~p microsec" 
	      "~n   Total:  ~p microsec"
	      "~n   Message (encoded) size: ~p bytes~n", 
	      [Desc, Etime, Dtime, Etime + Dtime, Size]),
    io:format(Fid, "~nencoding/decoding times sorted by messages size:~n", []),
    tt_store_case_text1(Fid, lists:keysort(3, Bins)),
    file:close(Fid).

tt_store_case_text1(Fid, []) ->
    ok;
tt_store_case_text1(Fid, [{Name, _Bin, Size, ETime, DTime}|Msgs]) ->
    io:format(Fid, "   ~p: Size: ~p, Encode: ~p, Decode: ~p~n", 
	      [Name, Size, ETime, DTime]),
    tt_store_case_text1(Fid, Msgs).


tt_store_case_msgs(Dir, []) ->
    ok;
tt_store_case_msgs(Dir, [{Name, Bin, _Size, _ETime, _DTime}|Msgs]) ->
    File = tt_file_name(Name),
    case file:open(filename:join(Dir,File),[write]) of 
	{ok, Fid} -> 
	    file:write(Fid, Bin),
	    file:close(Fid),
	    tt_store_case_msgs(Dir, Msgs);
	{error, Reason} ->
	    throw({error, Reason})
    end.


tt_store_gnuplot(TopDir, Res, Format) ->
    io:format("storing gnuplot data: ", []),
    Dir = filename:join(TopDir, "gnuplot"),
    file:make_dir(Dir),
    tt_store_gnuplot_size(Dir, Res, Format),
    tt_store_gnuplot_encode_time(Dir, Res, Format),
    tt_store_gnuplot_decode_time(Dir, Res, Format),
    tt_store_gnuplot_total_time(Dir, Res, Format),
    io:format("~n", []),
    ok.

tt_store_gnuplot_size(TopDir, Res, Format) ->
    io:format(".", []),
    Dir = filename:join(TopDir, "size"),
    file:make_dir(Dir),
    OutFile = tt_store_gnuplot_outfile("encoded_size", Format),
    Header  = tt_store_gnuplot_header("Size comparison", 
				      "Message size in bytes",
				      Format, OutFile),
    Stats = tt_msg_sizes(Res, []),
    Batch = tt_gnuplot_batch(Dir, Header, Stats),
    Cmd   = tt_store_gnuplot_cmd(Dir, Batch, Format, OutFile),
    os:cmd(Cmd),
    ok.

tt_store_gnuplot_encode_time(TopDir, Res, Format) ->
    io:format(".", []),
    Dir = filename:join(TopDir, "encode_time"),
    file:make_dir(Dir),
    OutFile = tt_store_gnuplot_outfile("encode_time",Format),
    Header  = tt_store_gnuplot_header("Encoding time", 
				      "Encoding time in micro sec",
				      Format, OutFile),
    Stats = tt_enc_times(Res, []),
    Batch = tt_gnuplot_batch(Dir, Header, Stats),
    Cmd   = tt_store_gnuplot_cmd(Dir, Batch, Format, OutFile),
    os:cmd(Cmd),
    ok.


tt_store_gnuplot_decode_time(TopDir, Res, Format) ->
    io:format(".", []),
    Dir = filename:join(TopDir, "decode_time"),
    file:make_dir(Dir),
    OutFile = tt_store_gnuplot_outfile("decode_time", Format),
    Header  = tt_store_gnuplot_header("Decoding time", 
				      "Decoding time in micro sec",
				      Format, OutFile),
    Stats = tt_dec_times(Res, []),
    Batch = tt_gnuplot_batch(Dir, Header, Stats),
    Cmd   = tt_store_gnuplot_cmd(Dir, Batch, Format, OutFile),
    os:cmd(Cmd),
    ok.

tt_store_gnuplot_total_time(TopDir, Res, Format) ->
    io:format(".", []),
    Dir = filename:join(TopDir, "code_time"),
    file:make_dir(Dir),
    OutFile = tt_store_gnuplot_outfile("code_time", Format),
    Header  = tt_store_gnuplot_header("Coding time", 
				      "Coding time in micro sec",
				      Format, OutFile),
    Stats = tt_tot_times(Res, []),
    Batch = tt_gnuplot_batch(Dir, Header, Stats),
    Cmd   = tt_store_gnuplot_cmd(Dir, Batch, Format, OutFile),
    os:cmd(Cmd),
    ok.


tt_store_gnuplot_header(Title, Ylabel, Format, OutFile) ->
    ["set title \"", Title, " comparison of Megaco/H.248 encoding formats\"\n",
     "set timestamp top\n",
     "set terminal ", Format, "\n",
     "set xlabel \"Messages\"\n",
     "set ylabel \"", Ylabel, " \"\n",
     "set rmargin 10\n",
     "set key left top Left\n",
     "set output \"", OutFile, "\"\n\n"].


tt_store_gnuplot_outfile(Filename, Format) ->
    [Filename, tt_format2ext(Format)].

tt_format2ext("postscript") ->
    ".ps";
tt_format2ext("latex") -> 
    ".tex";
tt_format2ext("pbm") -> 
    ".pbm";
tt_format2ext("png") -> 
    ".png".


tt_store_gnuplot_cmd(Dir, Batch, "postscript", OutFile) ->
    "cd " ++ Dir ++ "; gnuplot " ++ Batch ++ "; distill " ++ OutFile;
tt_store_gnuplot_cmd(Dir, Batch, "png", _) ->
    "cd " ++ Dir ++ "; gnuplot " ++ Batch;
tt_store_gnuplot_cmd(Dir, Batch, "pbm", _) ->
    "cd " ++ Dir ++ "; gnuplot " ++ Batch.
    

tt_gnuplot_batch(Dir, Hdr, Stats) ->
    {Names, Arrows} = tt_gnuplot_data(Dir, Stats, 1, [], []),
    [H | T] = [lists:concat(["\"", N, "\" with linespoints"]) || N <- Names],
    Plots = [[", ", Plot] || Plot <- T],
    IoList = [Hdr, Arrows, "\nplot ", H, Plots, "\n\nshow output\n"],
    Bin = list_to_binary(IoList),
    BatchFile = "gnuplot.batch",
    file:write_file(filename:join(Dir, BatchFile), Bin),
    BatchFile.


tt_gnuplot_data(_Dir, [], _Pos, Names, Arrows) ->
    {lists:reverse(Names), lists:reverse(Arrows)};
tt_gnuplot_data(Dir, [{Case, Desc, Data}|T], Pos, Names, Arrows) ->
    Filename = lists:flatten(io_lib:format("~p", [Case])),
    {ok, Fd} = file:open(filename:join(Dir, Filename),[write]),
    tt_gnuplot_data1(Fd, 1, Data),
    Vals = [Val || {_, Val} <- Data],
    Min  = trunc(lists:min(Vals)),
    Max  = round(lists:max(Vals)),
    Sum  = round(lists:sum(Vals)),
    Len  = length(Vals),  
    Avg  = Sum div Len,
    %% Name = lists:concat([Case, " (", Min, ",", Avg, ",", Max, ")"]),
    Arrow =
	lists:concat(["set arrow from 1,", Avg,
		      " to ", Len, ", ", Avg,
		      " nohead lt ", Pos, "\n",
		      "set label \" ", Avg, " (avg)\" at ", Len, ",", 
		      Avg + 10, "\n"]),
    tt_gnuplot_data(Dir, T, Pos + 1, [Case | Names], [Arrow | Arrows]).

tt_gnuplot_data1(Fd, N, []) ->
    ok;
tt_gnuplot_data1(Fd, N, [{Name, Val}|T]) ->
    io:format(Fd, "~w ~w~n", [N, Val]),
    tt_gnuplot_data1(Fd, N+1, T).
    

tt_store_excel(TopDir, Res) ->
    io:format("storing excel data: ", []),
    Dir = filename:join(TopDir, "excel"),
    file:make_dir(Dir),
    tt_store_excel_size(Dir, Res),
    tt_store_excel_encode_time(Dir, Res),
    tt_store_excel_decode_time(Dir, Res),
    tt_store_excel_total_time(Dir, Res),
    io:format("~n", []),
    ok.


tt_store_excel_size(Dir, Res) ->
    io:format(".", []),
    {ok, Fd} = file:open(filename:join(Dir,"size.txt"),[write]),
    Sizes = tt_msg_sizes(Res, []),
    tt_store_excel_tab(Fd, Sizes),
    ok.


tt_store_excel_encode_time(Dir, Res) ->
    io:format(".", []),
    {ok, Fd} = file:open(filename:join(Dir,"encode_time.txt"),[write]),
    Encodes = tt_enc_times(Res, []),
    tt_store_excel_tab(Fd, Encodes),
    ok.


tt_store_excel_decode_time(Dir, Res) ->
    io:format(".", []),
    {ok, Fd} = file:open(filename:join(Dir,"decode_time.txt"),[write]),
    Decodes = tt_dec_times(Res, []),
    tt_store_excel_tab(Fd, Decodes),
    ok.


tt_store_excel_total_time(Dir, Res) ->
    io:format(".", []),
    {ok, Fd} = file:open(filename:join(Dir,"total_time.txt"),[write]),
    Totals = tt_tot_times(Res, []),
    tt_store_excel_tab(Fd, Totals),
    ok.


tt_store_excel_tab(Fd, Res) ->
    %% For all elements of this list, the Values is of the same length...
    [{_, _, Values}|_] = Res,
    tt_store_excel_tab_header(Fd, length(Values), 1),
    tt_store_excel_tab1(Fd, Res).

tt_store_excel_tab1(Fd, []) ->
    io:format(Fd, "~n", []);
tt_store_excel_tab1(Fd, [{Codec, _, Values}|T]) ->
    io:format(Fd, "~w", [Codec]),
    tt_store_excel_tab_row(Fd, Values),
    tt_store_excel_tab1(Fd, T).

tt_store_excel_tab_header(Fd, 0, _) ->
    io:format(Fd, "~n", []);
tt_store_excel_tab_header(Fd, N, M) ->
    io:format(Fd, "\t~w", [M]),
    tt_store_excel_tab_header(Fd, N-1, M+1).

tt_store_excel_tab_row(Fd, []) ->
    io:format(Fd, "~n", []);
tt_store_excel_tab_row(Fd, [{_,Value}|Values]) ->
    io:format(Fd, "\t~w", [round(Value)]),
    tt_store_excel_tab_row(Fd, Values).
    

tt_msg_sizes([], Sizes) ->
    Sizes;
tt_msg_sizes([{ok, {Case, Res}}|T], S) ->
    %% io:format("tt_msg_sizes -> entry with Case: ~p~n", [Case]),
    {value, {desc, Desc}} = lists:keysearch(desc, 1, Res),
    {value, {bins, Bins}} = lists:keysearch(bins, 1, Res),
    Sizes = [{Name, Size} || {Name, _Bin, Size, Etime, Dtime} <- Bins],
    tt_msg_sizes(T, [{Case, Desc, lists:keysort(1,Sizes)}|S]);
tt_msg_sizes([_|T], S) ->
    tt_msg_sizes(T, S).
    

tt_enc_times([], Times) ->
    Times;
tt_enc_times([{ok, {Case, Res}}|T], Acc) ->
    %% io:format("tt_enc_times -> entry with Case: ~p~n", [Case]),
    {value, {desc, Desc}} = lists:keysearch(desc, 1, Res),
    {value, {bins, Bins}} = lists:keysearch(bins, 1, Res),
    Times = [{Name, Etime} || {Name, _Bin, _Size, Etime, _Dtime} <- Bins],
    tt_enc_times(T, [{Case, Desc, lists:keysort(1,Times)}|Acc]);
tt_enc_times([_|T], Acc) ->
    tt_enc_times(T, Acc).
    

tt_dec_times([], Times) ->
    Times;
tt_dec_times([{ok, {Case, Res}}|T], Acc) ->
    {value, {desc, Desc}} = lists:keysearch(desc, 1, Res),
    {value, {bins, Bins}} = lists:keysearch(bins, 1, Res),
    Times = [{Name, Dtime} || {Name, _Bin, _Size, _Etime, Dtime} <- Bins],
    tt_dec_times(T, [{Case, Desc, lists:keysort(1,Times)}|Acc]);
tt_dec_times([_|T], Acc) ->
    tt_dec_times(T, Acc).
    

tt_tot_times([], Times) ->
    Times;
tt_tot_times([{ok, {Case, Res}}|T], Acc) ->
    %% io:format("tt_dec_times -> entry with Case: ~p~n", [Case]),
    {value, {desc, Desc}} = lists:keysearch(desc, 1, Res),
    {value, {bins, Bins}} = lists:keysearch(bins, 1, Res),
    Times = [{Name, Etime+Dtime} || {Name, _Bin, _Size, Etime, Dtime} <- Bins],
    tt_tot_times(T, [{Case, Desc, lists:keysort(1,Times)}|Acc]);
tt_tot_times([_|T], Acc) ->
    tt_tot_times(T, Acc).
    

tt_file_name(Name) ->
    io_lib:format("~w.bin", [Name]).




%% ----

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
%% init_per_testcase(flex_compact_dm_timers1 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
%% init_per_testcase(flex_compact_dm_timers2 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
%% init_per_testcase(flex_compact_dm_timers3 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
%% init_per_testcase(flex_compact_dm_timers4 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
%% init_per_testcase(flex_compact_dm_timers5 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
%% init_per_testcase(flex_compact_dm_timers6 = Case, Config) ->
%%     put(severity,trc),
%%     put(dbg,true),
%%     megaco_test_lib:init_per_testcase(Case, Config);
% init_per_testcase(compact_msg9 = Case, Config) ->
%     put(severity,trc),
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
% init_per_testcase(compact_otp4011_msg1 = Case, Config) ->
%     put(severity,trc),
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
% init_per_testcase(compact_otp4085_msg1 = Case, Config) ->
%     put(severity,trc),
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
% init_per_testcase(compact_otp4299_msg1 = Case, Config) ->
%     put(severity,trc),
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
% init_per_testcase(compact_otp4299_msg2 = Case, Config) ->
%     put(severity,trc),
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
init_per_testcase(Case, Config) ->
    CaseString = io_lib:format("~p", [Case]),
    C = 
	case lists:suffix("time_test", atom_to_list(Case)) of
	    true ->
		[{tc_timeout, timer:minutes(10)}|Config];
	    false ->
		Config
	end,
    megaco_test_lib:init_per_testcase(Case, C).


%% fin_per_testcase(flex_compact_dm_timers1 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
%% fin_per_testcase(flex_compact_dm_timers2 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
%% fin_per_testcase(flex_compact_dm_timers3 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
%% fin_per_testcase(flex_compact_dm_timers4 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
%% fin_per_testcase(flex_compact_dm_timers5 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
%% fin_per_testcase(flex_compact_dm_timers6 = Case, Config) ->
%%     erase(dbg),
%%     erase(severity),
%%     megaco_test_lib:fin_per_testcase(Case, Config);
% fin_per_testcase(compact_msg9 = Case, Config) ->
%     erase(dbg),
%     erase(severity),
%     megaco_test_lib:fin_per_testcase(Case, Config);
% fin_per_testcase(compact_otp4085_msg1 = Case, Config) ->
%     erase(dbg),
%     erase(severity),
%     megaco_test_lib:fin_per_testcase(Case, Config);
% fin_per_testcase(compact_otp4299_msg1 = Case, Config) ->
%     erase(dbg),
%     erase(severity),
%     megaco_test_lib:fin_per_testcase(Case, Config);
% fin_per_testcase(compact_otp4299_msg2 = Case, Config) ->
%     erase(dbg),
%     erase(severity),
%     megaco_test_lib:fin_per_testcase(Case, Config);
fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     text,
     binary,
     tickets %% ,
     %% time_test
    ].

text(suite) ->
    [
     pretty,
     flex_pretty,
     compact,
     flex_compact
    ].

binary(suite) ->
    [
     bin,
     ber,
     ber_bin,
     per,
     per_bin %% ,
     %% per_bin_opt
    ].

time_test(suite) ->
    [
     pretty_time_test,
     flex_pretty_time_test,
     compact_time_test,
     flex_compact_time_test,
     bin_time_test,
     ber_time_test,
     ber_bin_time_test,
     per_time_test,
     per_bin_time_test,
     %% per_bin_opt_time_test,
     erl_dist_time_test,
     erl_dist_compressed_time_test
    ].


pretty(suite) ->
    [
     pretty_msg1a, pretty_msg1b, 
     pretty_msg2, 
     pretty_msg3, 
     pretty_msg4, 
     pretty_msg5, 
     pretty_msg6a, pretty_msg6b, 
     pretty_msg7, 
     pretty_msg8a, pretty_msg8b, 
     pretty_msg9, 
     pretty_msg10, 
     pretty_msg11,
     pretty_msg12,
     pretty_msg13,
     pretty_msg14,
     pretty_msg15,
     pretty_msg16,
     pretty_msg17,
     pretty_msg18,
     pretty_msg19,
     pretty_msg20,
     pretty_msg21,
     pretty_msg22a,
     pretty_msg22b,
     pretty_msg22c,
     pretty_msg22d,
     pretty_msg22e,
     pretty_msg22f,
     pretty_msg23,
     pretty_msg24,
     pretty_msg25
    ].


compact(suite) ->
    [
     compact_msg1a, compact_msg1b, 
     compact_msg2, 
     compact_msg3, 
     compact_msg4, 
     compact_msg5, 
     compact_msg6a, compact_msg6b, 
     compact_msg7, 
     compact_msg8a, compact_msg8b, 
     compact_msg9, 
     compact_msg10, 
     compact_msg11,
     compact_msg12,
     compact_msg13,
     compact_msg14,
     compact_msg15,
     compact_msg16,
     compact_msg17,
     compact_msg18,
     compact_msg19,
     compact_msg20,
     compact_msg21,
     compact_msg22a,
     compact_msg22b,
     compact_msg22c,
     compact_msg22d,
     compact_msg22e,
     compact_msg22f,
     compact_msg23,
     compact_msg24,
     compact_msg25
    ].


flex_pretty(suite) ->
    {req, [], 
     {conf, flex_pretty_init, flex_pretty_cases(), flex_pretty_finish}}.

flex_pretty_cases() ->
    [
     flex_pretty_msg1a, flex_pretty_msg1b, 
     flex_pretty_msg2, 
     flex_pretty_msg3, 
     flex_pretty_msg4, 
     flex_pretty_msg5, 
     flex_pretty_msg6a, flex_pretty_msg6b, 
     flex_pretty_msg7, 
     flex_pretty_msg8a, flex_pretty_msg8b, 
     flex_pretty_msg9, 
     flex_pretty_msg10, 
     flex_pretty_msg11,
     flex_pretty_msg12,
     flex_pretty_msg13,
     flex_pretty_msg14,
     flex_pretty_msg15,
     flex_pretty_msg16,
     flex_pretty_msg17,
     flex_pretty_msg18,
     flex_pretty_msg19,
     flex_pretty_msg20,
     flex_pretty_msg21,
     flex_pretty_msg22a,
     flex_pretty_msg22b,
     flex_pretty_msg22c,
     flex_pretty_msg22d,
     flex_pretty_msg22e,
     flex_pretty_msg22f,
     flex_pretty_msg23,
     flex_pretty_msg24,
     flex_pretty_msg25
    ].


flex_compact(suite) ->
    {req, [], 
     {conf, flex_compact_init, flex_compact_cases(), flex_compact_finish}}.

flex_compact_cases() ->
    [
     flex_compact_msg1a, flex_compact_msg1b, 
     flex_compact_msg2, 
     flex_compact_msg3, 
     flex_compact_msg4, 
     flex_compact_msg5, 
     flex_compact_msg6a, flex_compact_msg6b, 
     flex_compact_msg7, 
     flex_compact_msg8a, flex_compact_msg8b, 
     flex_compact_msg9, 
     flex_compact_msg10, 
     flex_compact_msg11,
     flex_compact_msg12,
     flex_compact_msg13,
     flex_compact_msg14,
     flex_compact_msg15,
     flex_compact_msg16,
     flex_compact_msg17,
     flex_compact_msg18,
     flex_compact_msg19,
     flex_compact_msg20,
     flex_compact_msg21,
     flex_compact_msg22a,
     flex_compact_msg22b,
     flex_compact_msg22c,
     flex_compact_msg22d,
     flex_compact_msg22e,
     flex_compact_msg22f,
     flex_compact_msg23,
     flex_compact_msg24,
     flex_compact_msg25,

     flex_compact_dm_timers1,
     flex_compact_dm_timers2,
     flex_compact_dm_timers3,
     flex_compact_dm_timers4,
     flex_compact_dm_timers5,
     flex_compact_dm_timers6
    ].


bin(suite) ->
    [
     bin_msg1a, bin_msg1b, 
     bin_msg2, 
     bin_msg3, 
     bin_msg4, 
     bin_msg5, 
     bin_msg6a, bin_msg6b, 
     bin_msg7, 
     bin_msg8a, bin_msg8b, 
     bin_msg9, 
     bin_msg10, 
     bin_msg11,
     bin_msg12,
     bin_msg13,
     bin_msg14,
     bin_msg15,
     bin_msg16,
     bin_msg17,
     bin_msg18,
     bin_msg19,
     bin_msg20,
     bin_msg21,
     bin_msg22a,
     bin_msg22b,
     bin_msg22c,
     bin_msg22d,
     bin_msg22e,
     bin_msg22f,
     bin_msg23,
     bin_msg24,
     bin_msg25
    ].


ber(suite) ->
    [
     ber_msg1a, ber_msg1b, 
     ber_msg2, 
     ber_msg3, 
     ber_msg4, 
     ber_msg5, 
     ber_msg6a, ber_msg6b, 
     ber_msg7, 
     ber_msg8a, ber_msg8b, 
     ber_msg9, 
     ber_msg10, 
     ber_msg11,
     ber_msg12,
     ber_msg13,
     ber_msg14,
     ber_msg15,
     ber_msg16,
     ber_msg17,
     ber_msg18,
     ber_msg19,
     ber_msg20,
     ber_msg21,
     ber_msg22a,
     ber_msg22b,
     ber_msg22c,
     ber_msg22d,
     ber_msg22e,
     ber_msg22f,
     ber_msg23,
     ber_msg24,
     ber_msg25
    ].


ber_bin(suite) ->
    [
     ber_bin_msg1a, ber_bin_msg1b, 
     ber_bin_msg2, 
     ber_bin_msg3, 
     ber_bin_msg4, 
     ber_bin_msg5, 
     ber_bin_msg6a, ber_bin_msg6b, 
     ber_bin_msg7, 
     ber_bin_msg8a, ber_bin_msg8b, 
     ber_bin_msg9, 
     ber_bin_msg10, 
     ber_bin_msg11,
     ber_bin_msg12,
     ber_bin_msg13,
     ber_bin_msg14,
     ber_bin_msg15,
     ber_bin_msg16,
     ber_bin_msg17,
     ber_bin_msg18,
     ber_bin_msg19,
     ber_bin_msg20,
     ber_bin_msg21,
     ber_bin_msg22a,
     ber_bin_msg22b,
     ber_bin_msg22c,
     ber_bin_msg22d,
     ber_bin_msg22e,
     ber_bin_msg22f,
     ber_bin_msg23,
     ber_bin_msg24,
     ber_bin_msg25
    ].


per(suite) ->
    [
     per_msg1a, per_msg1b, 
     per_msg2, 
     per_msg3, 
     per_msg4, 
     per_msg5, 
     per_msg6a, per_msg6b, 
     per_msg7, 
     per_msg8a, per_msg8b, 
     per_msg9, 
     per_msg10, 
     per_msg11,
     per_msg12,
     per_msg13,
     per_msg14,
     per_msg15,
     per_msg16,
     per_msg17,
     per_msg18,
     per_msg19,
     per_msg20,
     per_msg21,
     per_msg22a,
     per_msg22b,
     per_msg22c,
     per_msg22d,
     per_msg22e,
     per_msg22f,
     per_msg23,
     per_msg24,
     per_msg25
    ].


%% Support for per_bin was added to ASN.1 as of version
%% 1.3.2 (R8). And later merged into 1.3.1.3 (R7). These
%% releases are identical (as far as I know).
%% 
per_bin(suite) ->
    [
     per_bin_msg1a, per_bin_msg1b, 
     per_bin_msg2, 
     per_bin_msg3, 
     per_bin_msg4, 
     per_bin_msg5, 
     per_bin_msg6a, per_bin_msg6b, 
     per_bin_msg7, 
     per_bin_msg8a, per_bin_msg8b, 
     per_bin_msg9, 
     per_bin_msg10, 
     per_bin_msg11,
     per_bin_msg12,
     per_bin_msg13,
     per_bin_msg14,
     per_bin_msg15,
     per_bin_msg16,
     per_bin_msg17,
     per_bin_msg18,
     per_bin_msg19,
     per_bin_msg20,
     per_bin_msg21,
     per_bin_msg22a,
     per_bin_msg22b,
     per_bin_msg22c,
     per_bin_msg22d,
     per_bin_msg22e,
     per_bin_msg22f,
     per_bin_msg23,
     per_bin_msg24,
     per_bin_msg25
    ].


% per_bin_opt(suite) ->
%     [
%      per_bin_opt_msg1a, per_bin_opt_msg1b, 
%      per_bin_opt_msg2, 
%      per_bin_opt_msg3, 
%      per_bin_opt_msg4, 
%      per_bin_opt_msg5, 
%      per_bin_opt_msg6a, per_bin_opt_msg6b, 
%      per_bin_opt_msg7, 
%      per_bin_opt_msg8a, per_bin_opt_msg8b, 
%      per_bin_opt_msg9, 
%      per_bin_opt_msg10, 
%      per_bin_opt_msg11,
%      per_bin_opt_msg12,
%      per_bin_opt_msg13,
%      per_bin_opt_msg14,
%      per_bin_opt_msg15,
%      per_bin_opt_msg16,
%      per_bin_opt_msg17,
%      per_bin_opt_msg18,
%      per_bin_opt_msg19,
%      per_bin_opt_msg20,
%      per_bin_opt_msg21,
%      per_bin_opt_msg22a,
%      per_bin_opt_msg22b,
%      per_bin_opt_msg22c,
%      per_bin_opt_msg22d,
%      per_bin_opt_msg22e,
%      per_bin_opt_msg22f,
%      per_bin_opt_msg23,
%      per_bin_opt_msg24,
%      per_bin_opt_msg25
%     ].


tickets(suite) ->
    [
     compact_tickets
    ].


compact_tickets(suite) ->
    [
     compact_otp4011_msg1,
     compact_otp4011_msg2,
     compact_otp4011_msg3,
     compact_otp4013_msg1,
     compact_otp4085_msg1,
     compact_otp4085_msg2,
     compact_otp4280_msg1,
     compact_otp4299_msg1,
     compact_otp4299_msg2,
     compact_otp4359_msg1
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_msg1a(suite) ->
    [];
pretty_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg1a()),
    ok.

pretty_msg1b(suite) ->
    [];
pretty_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg1b()),
    ok.

pretty_msg2(suite) ->
    [];
pretty_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg2()),
    ok.

pretty_msg3(suite) ->
    [];
pretty_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg3()),
    ok.

pretty_msg4(suite) ->
    [];
pretty_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg4()),
    ok.

pretty_msg5(suite) ->
    [];
pretty_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg5()),
    ok.

pretty_msg6a(suite) ->
    [];
pretty_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg6a()),
    ok.

pretty_msg6b(suite) ->
    [];
pretty_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg6b()),
    ok.

pretty_msg7(suite) ->
    [];
pretty_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg7()),
    ok.

pretty_msg8a(suite) ->
    [];
pretty_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg8a()),
    ok.

pretty_msg8b(suite) ->
    [];
pretty_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg8b()),
    ok.

pretty_msg9(suite) ->
    [];
pretty_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg9()),
    ok.

pretty_msg10(suite) ->
    [];
pretty_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg10()),
    ok.

pretty_msg11(suite) ->
    [];
pretty_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg11()),
    ok.

pretty_msg12(suite) ->
    [];
pretty_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg12()),
    ok.

pretty_msg13(suite) ->
    [];
pretty_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg13()),
    ok.

pretty_msg14(suite) ->
    [];
pretty_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg14()),
    ok.

pretty_msg15(suite) ->
    [];
pretty_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg15()),
    ok.

pretty_msg16(suite) ->
    [];
pretty_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg16()),
    ok.

pretty_msg17(suite) ->
    [];
pretty_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg17()),
    ok.

pretty_msg18(suite) ->
    [];
pretty_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg18()),
    ok.

pretty_msg19(suite) ->
    [];
pretty_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg19()),
    ok.

pretty_msg20(suite) ->
    [];
pretty_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg20()),
    ok.

pretty_msg21(suite) ->
    [];
pretty_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg21()),
    ok.

pretty_msg22a(suite) ->
    [];
pretty_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22a()),
    ok.

pretty_msg22b(suite) ->
    [];
pretty_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22b()),
    ok.

pretty_msg22c(suite) ->
    [];
pretty_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22c()),
    ok.

pretty_msg22d(suite) ->
    [];
pretty_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22d()),
    ok.

pretty_msg22e(suite) ->
    [];
pretty_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22e()),
    ok.

pretty_msg22f(suite) ->
    [];
pretty_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg22f()),
    ok.

pretty_msg23(suite) ->
    [];
pretty_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg23()),
    ok.

pretty_msg24(suite) ->
    [];
pretty_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg24()),
    ok.

pretty_msg25(suite) ->
    [];
pretty_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_pretty_init(Config) when list(Config) ->
    Flag = process_flag(trap_exit, true),    
    Res = (catch start_flex_scanner()),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
  	    ?LOG("flex_pretty_init -> error: "
  		 "~n   Reason: ~p~n", [Reason]),
	    skip(Reason);
	{Pid, Conf} when pid(Pid) ->
	    [{flex_scanner, Pid, Conf}|Config]
    end;
flex_pretty_init(Config) ->
    exit({invalid_config, Config}).
    

flex_pretty_finish(Config) when list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, Pid, _Conf}} ->
	    stop_flex_scanner(Pid),
	    lists:keydelete(flex_scanner, 1, Config);
	false ->
	    Config
    end.
    

flex_pretty_msg1a(suite) ->
    [];
flex_pretty_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config),
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg1a()),
    ok.

flex_pretty_msg1b(suite) ->
    [];
flex_pretty_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg1b()),
    ok.

flex_pretty_msg2(suite) ->
    [];
flex_pretty_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg2()),
    ok.

flex_pretty_msg3(suite) ->
    [];
flex_pretty_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg3()),
    ok.

flex_pretty_msg4(suite) ->
    [];
flex_pretty_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg4()),
    ok.

flex_pretty_msg5(suite) ->
    [];
flex_pretty_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg5()),
    ok.

flex_pretty_msg6a(suite) ->
    [];
flex_pretty_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg6a()),
    ok.

flex_pretty_msg6b(suite) ->
    [];
flex_pretty_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg6b()),
    ok.

flex_pretty_msg7(suite) ->
    [];
flex_pretty_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg7()),
    ok.

flex_pretty_msg8a(suite) ->
    [];
flex_pretty_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg8a()),
    ok.

flex_pretty_msg8b(suite) ->
    [];
flex_pretty_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg8b()),
    ok.

flex_pretty_msg9(suite) ->
    [];
flex_pretty_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg9()),
    ok.

flex_pretty_msg10(suite) ->
    [];
flex_pretty_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg10()),
    ok.

flex_pretty_msg11(suite) ->
    [];
flex_pretty_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg11()),
    ok.

flex_pretty_msg12(suite) ->
    [];
flex_pretty_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg12()),
    ok.

flex_pretty_msg13(suite) ->
    [];
flex_pretty_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg13()),
    ok.

flex_pretty_msg14(suite) ->
    [];
flex_pretty_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg14()),
    ok.

flex_pretty_msg15(suite) ->
    [];
flex_pretty_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg15()),
    ok.

flex_pretty_msg16(suite) ->
    [];
flex_pretty_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg16()),
    ok.

flex_pretty_msg17(suite) ->
    [];
flex_pretty_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg17()),
    ok.

flex_pretty_msg18(suite) ->
    [];
flex_pretty_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg18()),
    ok.

flex_pretty_msg19(suite) ->
    [];
flex_pretty_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg19()),
    ok.

flex_pretty_msg20(suite) ->
    [];
flex_pretty_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg20()),
    ok.

flex_pretty_msg21(suite) ->
    [];
flex_pretty_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg21()),
    ok.

flex_pretty_msg22a(suite) ->
    [];
flex_pretty_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22a()),
    ok.

flex_pretty_msg22b(suite) ->
    [];
flex_pretty_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22b()),
    ok.

flex_pretty_msg22c(suite) ->
    [];
flex_pretty_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22c()),
    ok.

flex_pretty_msg22d(suite) ->
    [];
flex_pretty_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22d()),
    ok.

flex_pretty_msg22e(suite) ->
    [];
flex_pretty_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22e()),
    ok.

flex_pretty_msg22f(suite) ->
    [];
flex_pretty_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_pretty_text_encoder, [Conf], msg22f()),
    ok.

flex_pretty_msg23(suite) ->
    [];
flex_pretty_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg23()),
    ok.

flex_pretty_msg24(suite) ->
    [];
flex_pretty_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg24()),
    ok.

flex_pretty_msg25(suite) ->
    [];
flex_pretty_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_pretty_text_encoder, [Conf], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compact_msg1a(suite) ->
    [];
compact_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg1a()),
    ok.

compact_msg1b(suite) ->
    [];
compact_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg1b()),
    ok.

compact_msg2(suite) ->
    [];
compact_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg2()),
    ok.

compact_msg3(suite) ->
    [];
compact_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg3()),
    ok.

compact_msg4(suite) ->
    [];
compact_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg4()),
    ok.

compact_msg5(suite) ->
    [];
compact_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg5()),
    ok.

compact_msg6a(suite) ->
    [];
compact_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg6a()),
    ok.

compact_msg6b(suite) ->
    [];
compact_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg6b()),
    ok.

compact_msg7(suite) ->
    [];
compact_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg7()),
    ok.

compact_msg8a(suite) ->
    [];
compact_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg8a()),
    ok.

compact_msg8b(suite) ->
    [];
compact_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg8b()),
    ok.

compact_msg9(suite) ->
    [];
compact_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg9()),
    ok.

compact_msg10(suite) ->
    [];
compact_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg10()),
    ok.

compact_msg11(suite) ->
    [];
compact_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg11()),
    ok.

compact_msg12(suite) ->
    [];
compact_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg12()),
    ok.

compact_msg13(suite) ->
    [];
compact_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg13()),
    ok.

compact_msg14(suite) ->
    [];
compact_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg14()),
    ok.

compact_msg15(suite) ->
    [];
compact_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg15()),
    ok.

compact_msg16(suite) ->
    [];
compact_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg16()),
    ok.

compact_msg17(suite) ->
    [];
compact_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg17()),
    ok.

compact_msg18(suite) ->
    [];
compact_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg18()),
    ok.

compact_msg19(suite) ->
    [];
compact_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg19()),
    ok.

compact_msg20(suite) ->
    [];
compact_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg20()),
    ok.

compact_msg21(suite) ->
    [];
compact_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg21()),
    ok.

compact_msg22a(suite) ->
    [];
compact_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22a()),
    ok.

compact_msg22b(suite) ->
    [];
compact_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22b()),
    ok.

compact_msg22c(suite) ->
    [];
compact_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22c()),
    ok.

compact_msg22d(suite) ->
    [];
compact_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22d()),
    ok.

compact_msg22e(suite) ->
    [];
compact_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22e()),
    ok.

compact_msg22f(suite) ->
    [];
compact_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [], msg22f()),
    ok.

compact_msg23(suite) ->
    [];
compact_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg23()),
    ok.

compact_msg24(suite) ->
    [];
compact_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg24()),
    ok.

compact_msg25(suite) ->
    [];
compact_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_compact_init(Config) when list(Config) ->
    Flag = process_flag(trap_exit, true),    
    Res = (catch start_flex_scanner()),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
 	    ?LOG("flex_compact_init -> error: "
 		 "~n   Reason: ~p~n", [Reason]),
	    skip(Reason);
	{Pid, Conf} when pid(Pid) ->
	    [{flex_scanner, Pid, Conf}|Config]
    end.
    

flex_compact_finish(Config) when list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, Pid, _Conf}} ->
	    stop_flex_scanner(Pid),
	    lists:keydelete(flex_scanner, 1, Config);
	false ->
	    Config
    end.
    

flex_compact_msg1a(suite) ->
    [];
flex_compact_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg1a()),
    ok.

flex_compact_msg1b(suite) ->
    [];
flex_compact_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg1b()),
    ok.

flex_compact_msg2(suite) ->
    [];
flex_compact_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg2()),
    ok.

flex_compact_msg3(suite) ->
    [];
flex_compact_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg3()),
    ok.

flex_compact_msg4(suite) ->
    [];
flex_compact_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg4()),
    ok.

flex_compact_msg5(suite) ->
    [];
flex_compact_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg5()),
    ok.

flex_compact_msg6a(suite) ->
    [];
flex_compact_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg6a()),
    ok.

flex_compact_msg6b(suite) ->
    [];
flex_compact_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg6b()),
    ok.

flex_compact_msg7(suite) ->
    [];
flex_compact_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg7()),
    ok.

flex_compact_msg8a(suite) ->
    [];
flex_compact_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg8a()),
    ok.

flex_compact_msg8b(suite) ->
    [];
flex_compact_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg8b()),
    ok.

flex_compact_msg9(suite) ->
    [];
flex_compact_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = do(megaco_compact_text_encoder, [Conf], msg9()),
    ok.

flex_compact_msg10(suite) ->
    [];
flex_compact_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg10()),
    ok.

flex_compact_msg11(suite) ->
    [];
flex_compact_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg11()),
    ok.

flex_compact_msg12(suite) ->
    [];
flex_compact_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg12()),
    ok.

flex_compact_msg13(suite) ->
    [];
flex_compact_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg13()),
    ok.

flex_compact_msg14(suite) ->
    [];
flex_compact_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg14()),
    ok.

flex_compact_msg15(suite) ->
    [];
flex_compact_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg15()),
    ok.

flex_compact_msg16(suite) ->
    [];
flex_compact_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg16()),
    ok.

flex_compact_msg17(suite) ->
    [];
flex_compact_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg17()),
    ok.

flex_compact_msg18(suite) ->
    [];
flex_compact_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg18()),
    ok.

flex_compact_msg19(suite) ->
    [];
flex_compact_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg19()),
    ok.

flex_compact_msg20(suite) ->
    [];
flex_compact_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg20()),
    ok.

flex_compact_msg21(suite) ->
    [];
flex_compact_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg21()),
    ok.

flex_compact_msg22a(suite) ->
    [];
flex_compact_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22a()),
    ok.

flex_compact_msg22b(suite) ->
    [];
flex_compact_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22b()),
    ok.

flex_compact_msg22c(suite) ->
    [];
flex_compact_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22c()),
    ok.

flex_compact_msg22d(suite) ->
    [];
flex_compact_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22d()),
    ok.

flex_compact_msg22e(suite) ->
    [];
flex_compact_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22e()),
    ok.

flex_compact_msg22f(suite) ->
    [];
flex_compact_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg22f()),
    ok.

flex_compact_msg23(suite) ->
    [];
flex_compact_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg23()),
    ok.

flex_compact_msg24(suite) ->
    [];
flex_compact_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg24()),
    ok.

flex_compact_msg25(suite) ->
    [];
flex_compact_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = flex_scanner_conf(Config), 
    {equal, 'MegacoMessage'} = 
	do(megaco_compact_text_encoder, [Conf], msg25()),
    ok.


flex_compact_dm_timers1(suite) ->
    [];
flex_compact_dm_timers1(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("1", "2", "3"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers1 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(1,2,3, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers2(suite) ->
    [];
flex_compact_dm_timers2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("02", "03", "04"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers2 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(2,3,4, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers3(suite) ->
    [];
flex_compact_dm_timers3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("1", "02", "31"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers3 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(1,2,31, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers4(suite) ->
    [];
flex_compact_dm_timers4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("10", "21", "99"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers4 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(10,21,99, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers5(suite) ->
    [];
flex_compact_dm_timers5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("99", "23", "11"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers5 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(99,23,11, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


flex_compact_dm_timers6(suite) ->
    [];
flex_compact_dm_timers6(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    M = build_dm_timers_message("77", "09", "1"),
    B = list_to_binary(M),
    Conf = flex_scanner_conf(Config), 
    case d(megaco_compact_text_encoder, [Conf], B) of
	{ok, M1} when record(M1,'MegacoMessage') ->
	    t("flex_compact_dm_timers6 -> "
	      "~n   M:  ~s"
	      "~n   M1: ~p", [M, M1]),
	    verify_dm_timers(77,9,1, M1);
	Else ->
	    exit({decode_failed, M, Else})
    end.


build_dm_timers_message(T, S, L) ->
    M = io_lib:format("!/1 [123.123.123.4]:55555\nT=10001{C=-{MF=11111111/00000000/00000000{E=2223{al/on,dd/ce{DM=dialplan00}},SG{cg/rt},DM=dialplan00{T:~s,S:~s,L:~s,(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)}}}}", [T, S, L]),
    lists:flatten(M).


verify_dm_timers(T,S,L, #'MegacoMessage'{mess = Mess}) ->
    #'Message'{messageBody = Body} = Mess,
    case get_dm_timers(Body) of
	{T, S, L} ->
	    ok;
	{T1, S1, L1} ->
	    exit({invalid_timer_values, {{T, S, L}, {T1, S1, L1}}});
	{error, Reason} ->
	    exit({invalid_timer, {T, S, L, Reason}})
    end.

get_dm_timers({transactions, T}) when list(T) ->
    get_dm_timers1(T);
get_dm_timers(Other) ->
    {error, {invalid_transactions, Other}}.

get_dm_timers1([{transactionRequest,T}|Ts]) when record(T,'TransactionRequest') ->
    case get_dm_timers2(T) of
	{ok, Timers} ->
	    Timers;
	_ ->
	    get_dm_timers1(Ts)
    end;
get_dm_timers1([_|Ts]) ->
    get_dm_timers1(Ts);
get_dm_timers1([]) ->
    {error, {no_timers, 'TransactionRequest'}}.


get_dm_timers2(#'TransactionRequest'{actions = Actions}) when list(Actions) ->
    get_dm_timers3(Actions).


get_dm_timers3([#'ActionRequest'{commandRequests = Cmds}|Ars]) when list(Cmds) ->
    case get_dm_timers4(Cmds) of
	{ok, Timers} ->
	    {ok, Timers};
	_ ->
	    get_dm_timers3(Ars)
    end;
get_dm_timers3([_|Ars]) ->
    get_dm_timers3(Ars);
get_dm_timers3([]) ->
    {error, {no_timers, 'ActionRequest'}}.

get_dm_timers4([#'CommandRequest'{command = Cmd}|Cmds]) ->
    case get_dm_timers5(Cmd) of
	{ok, Timers} ->
	    {ok, Timers};
	_ ->
	    get_dm_timers4(Cmds)
    end;
get_dm_timers4([_|Cmds]) ->
    get_dm_timers4(Cmds);
get_dm_timers4([]) ->
    {error, {no_timers, 'CommandRequest'}}.


get_dm_timers5({modReq, #'AmmRequest'{descriptors = Descriptors}}) ->
    get_dm_timers6(Descriptors);
get_dm_timers5(R) ->
    {error, {no_modReq, R}}.


get_dm_timers6([{digitMapDescriptor, #'DigitMapDescriptor'{digitMapValue = Val}}|_]) ->
    case Val of
	#'DigitMapValue'{startTimer   = T,
			 shortTimer   = S,
			 longTimer    = L} ->
	    {ok, {T, S, L}};
	_ ->
	    {error, no_value_in_dm}
    end;
get_dm_timers6([_|Descs]) ->
    get_dm_timers6(Descs);
get_dm_timers6([]) ->
    {error, {no_timers, descriptors}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bin_msg1a(suite) ->
    [];
bin_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg1a()),
    ok.

bin_msg1b(suite) ->
    [];
bin_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg1b()),
    ok.

bin_msg2(suite) ->
    [];
bin_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg2()),
    ok.

bin_msg3(suite) ->
    [];
bin_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg3()),
    ok.

bin_msg4(suite) ->
    [];
bin_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg4()),
    ok.

bin_msg5(suite) ->
    [];
bin_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg5()),
    ok.

bin_msg6a(suite) ->
    [];
bin_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg6a()),
    ok.

bin_msg6b(suite) ->
    [];
bin_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg6b()),
    ok.

bin_msg7(suite) ->
    [];
bin_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg7()),
    ok.

bin_msg8a(suite) ->
    [];
bin_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg8a()),
    ok.

bin_msg8b(suite) ->
    [];
bin_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg8b()),
    ok.

bin_msg9(suite) ->
    [];
bin_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg9()),
    ok.

bin_msg10(suite) ->
    [];
bin_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg10()),
    ok.

bin_msg11(suite) ->
    [];
bin_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg11()),
    ok.

bin_msg12(suite) ->
    [];
bin_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg12()),
    ok.

bin_msg13(suite) ->
    [];
bin_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg13()),
    ok.

bin_msg14(suite) ->
    [];
bin_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg14()),
    ok.

bin_msg15(suite) ->
    [];
bin_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg15()),
    ok.

bin_msg16(suite) ->
    [];
bin_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg16()),
    ok.

bin_msg17(suite) ->
    [];
bin_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg17()),
    ok.

bin_msg18(suite) ->
    [];
bin_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg18()),
    ok.

bin_msg19(suite) ->
    [];
bin_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg19()),
    ok.

bin_msg20(suite) ->
    [];
bin_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg20()),
    ok.

bin_msg21(suite) ->
    [];
bin_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg21()),
    ok.

bin_msg22a(suite) ->
    [];
bin_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22a()),
    ok.

bin_msg22b(suite) ->
    [];
bin_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22b()),
    ok.

bin_msg22c(suite) ->
    [];
bin_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22c()),
    ok.

bin_msg22d(suite) ->
    [];
bin_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22d()),
    ok.

bin_msg22e(suite) ->
    [];
bin_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22e()),
    ok.

bin_msg22f(suite) ->
    [];
bin_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg22f()),
    ok.

bin_msg23(suite) ->
    [];
bin_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg23()),
    ok.

bin_msg24(suite) ->
    [];
bin_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg24()),
    ok.

bin_msg25(suite) ->
    [];
bin_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_binary_encoder, [], msg25()),
    ok.

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ber_msg1a(suite) ->
    [];
ber_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg1a()),
    ok.

ber_msg1b(suite) ->
    [];
ber_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg1b()),
    ok.

ber_msg2(suite) ->
    [];
ber_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg2()),
    ok.

ber_msg3(suite) ->
    [];
ber_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg3()),
    ok.

ber_msg4(suite) ->
    [];
ber_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg4()),
    ok.

ber_msg5(suite) ->
    [];
ber_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg5()),
    ok.

ber_msg6a(suite) ->
    [];
ber_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg6a()),
    ok.

ber_msg6b(suite) ->
    [];
ber_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg6b()),
    ok.

ber_msg7(suite) ->
    [];
ber_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg7()),
    ok.

ber_msg8a(suite) ->
    [];
ber_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg8a()),
    ok.

ber_msg8b(suite) ->
    [];
ber_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg8b()),
    ok.

ber_msg9(suite) ->
    [];
ber_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg9()),
    ok.

ber_msg10(suite) ->
    [];
ber_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg10()),
    ok.

ber_msg11(suite) ->
    [];
ber_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg11()),
    ok.

ber_msg12(suite) ->
    [];
ber_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg12()),
    ok.

ber_msg13(suite) ->
    [];
ber_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg13()),
    ok.

ber_msg14(suite) ->
    [];
ber_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg14()),
    ok.

ber_msg15(suite) ->
    [];
ber_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg15()),
    ok.

ber_msg16(suite) ->
    [];
ber_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg16()),
    ok.

ber_msg17(suite) ->
    [];
ber_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg17()),
    ok.

ber_msg18(suite) ->
    [];
ber_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg18()),
    ok.

ber_msg19(suite) ->
    [];
ber_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg19()),
    ok.

ber_msg20(suite) ->
    [];
ber_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg20()),
    ok.

ber_msg21(suite) ->
    [];
ber_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg21()),
    ok.

ber_msg22a(suite) ->
    [];
ber_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22a()),
    ok.

ber_msg22b(suite) ->
    [];
ber_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22b()),
    ok.

ber_msg22c(suite) ->
    [];
ber_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22c()),
    ok.

ber_msg22d(suite) ->
    [];
ber_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22d()),
    ok.

ber_msg22e(suite) ->
    [];
ber_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22e()),
    ok.

ber_msg22f(suite) ->
    [];
ber_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg22f()),
    ok.

ber_msg23(suite) ->
    [];
ber_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg23()),
    ok.

ber_msg24(suite) ->
    [];
ber_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg24()),
    ok.

ber_msg25(suite) ->
    [];
ber_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_encoder, [], msg25()),
    ok.

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ber_bin_msg1a(suite) ->
    [];
ber_bin_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg1a()),
    ok.

ber_bin_msg1b(suite) ->
    [];
ber_bin_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg1b()),
    ok.

ber_bin_msg2(suite) ->
    [];
ber_bin_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg2()),
    ok.

ber_bin_msg3(suite) ->
    [];
ber_bin_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg3()),
    ok.

ber_bin_msg4(suite) ->
    [];
ber_bin_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg4()),
    ok.

ber_bin_msg5(suite) ->
    [];
ber_bin_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg5()),
    ok.

ber_bin_msg6a(suite) ->
    [];
ber_bin_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg6a()),
    ok.

ber_bin_msg6b(suite) ->
    [];
ber_bin_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg6b()),
    ok.

ber_bin_msg7(suite) ->
    [];
ber_bin_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg7()),
    ok.

ber_bin_msg8a(suite) ->
    [];
ber_bin_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg8a()),
    ok.

ber_bin_msg8b(suite) ->
    [];
ber_bin_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg8b()),
    ok.

ber_bin_msg9(suite) ->
    [];
ber_bin_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg9()),
    ok.

ber_bin_msg10(suite) ->
    [];
ber_bin_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg10()),
    ok.

ber_bin_msg11(suite) ->
    [];
ber_bin_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg11()),
    ok.

ber_bin_msg12(suite) ->
    [];
ber_bin_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg12()),
    ok.

ber_bin_msg13(suite) ->
    [];
ber_bin_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg13()),
    ok.

ber_bin_msg14(suite) ->
    [];
ber_bin_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg14()),
    ok.

ber_bin_msg15(suite) ->
    [];
ber_bin_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg15()),
    ok.

ber_bin_msg16(suite) ->
    [];
ber_bin_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg16()),
    ok.

ber_bin_msg17(suite) ->
    [];
ber_bin_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg17()),
    ok.

ber_bin_msg18(suite) ->
    [];
ber_bin_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg18()),
    ok.

ber_bin_msg19(suite) ->
    [];
ber_bin_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg19()),
    ok.

ber_bin_msg20(suite) ->
    [];
ber_bin_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg20()),
    ok.

ber_bin_msg21(suite) ->
    [];
ber_bin_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg21()),
    ok.

ber_bin_msg22a(suite) ->
    [];
ber_bin_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22a()),
    ok.

ber_bin_msg22b(suite) ->
    [];
ber_bin_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22b()),
    ok.

ber_bin_msg22c(suite) ->
    [];
ber_bin_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22c()),
    ok.

ber_bin_msg22d(suite) ->
    [];
ber_bin_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22d()),
    ok.

ber_bin_msg22e(suite) ->
    [];
ber_bin_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22e()),
    ok.

ber_bin_msg22f(suite) ->
    [];
ber_bin_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg22f()),
    ok.

ber_bin_msg23(suite) ->
    [];
ber_bin_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg23()),
    ok.

ber_bin_msg24(suite) ->
    [];
ber_bin_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg24()),
    ok.

ber_bin_msg25(suite) ->
    [];
ber_bin_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_ber_bin_encoder, [], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

per_msg1a(suite) ->
    [];
per_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg1a()),
    ok.

per_msg1b(suite) ->
    [];
per_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg1b()),
    ok.

per_msg2(suite) ->
    [];
per_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg2()),
    ok.

per_msg3(suite) ->
    [];
per_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg3()),
    ok.

per_msg4(suite) ->
    [];
per_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg4()),
    ok.

per_msg5(suite) ->
    [];
per_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg5()),
    ok.

per_msg6a(suite) ->
    [];
per_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg6a()),
    ok.

per_msg6b(suite) ->
    [];
per_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg6b()),
    ok.

per_msg7(suite) ->
    [];
per_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg7()),
    ok.

per_msg8a(suite) ->
    [];
per_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg8a()),
    ok.

per_msg8b(suite) ->
    [];
per_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg8b()),
    ok.

per_msg9(suite) ->
    [];
per_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg9()),
    ok.

per_msg10(suite) ->
    [];
per_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg10()),
    ok.

per_msg11(suite) ->
    [];
per_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg11()),
    ok.

per_msg12(suite) ->
    [];
per_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg12()),
    ok.

per_msg13(suite) ->
    [];
per_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg13()),
    ok.

per_msg14(suite) ->
    [];
per_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg14()),
    ok.

per_msg15(suite) ->
    [];
per_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg15()),
    ok.

per_msg16(suite) ->
    [];
per_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg16()),
    ok.

per_msg17(suite) ->
    [];
per_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg17()),
    ok.

per_msg18(suite) ->
    [];
per_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg18()),
    ok.

per_msg19(suite) ->
    [];
per_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg19()),
    ok.

per_msg20(suite) ->
    [];
per_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg20()),
    ok.

per_msg21(suite) ->
    [];
per_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg21()),
    ok.

per_msg22a(suite) ->
    [];
per_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22a()),
    ok.

per_msg22b(suite) ->
    [];
per_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22b()),
    ok.

per_msg22c(suite) ->
    [];
per_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22c()),
    ok.

per_msg22d(suite) ->
    [];
per_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22d()),
    ok.

per_msg22e(suite) ->
    [];
per_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22e()),
    ok.

per_msg22f(suite) ->
    [];
per_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg22f()),
    ok.

per_msg23(suite) ->
    [];
per_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg23()),
    ok.

per_msg24(suite) ->
    [];
per_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg24()),
    ok.

per_msg25(suite) ->
    [];
per_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_encoder, [], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

per_bin_msg1a(suite) ->
    [];
per_bin_msg1a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg1a()),
    ok.

per_bin_msg1b(suite) ->
    [];
per_bin_msg1b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg1b()),
    ok.

per_bin_msg2(suite) ->
    [];
per_bin_msg2(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg2()),
    ok.

per_bin_msg3(suite) ->
    [];
per_bin_msg3(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg3()),
    ok.

per_bin_msg4(suite) ->
    [];
per_bin_msg4(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg4()),
    ok.

per_bin_msg5(suite) ->
    [];
per_bin_msg5(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg5()),
    ok.

per_bin_msg6a(suite) ->
    [];
per_bin_msg6a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg6a()),
    ok.

per_bin_msg6b(suite) ->
    [];
per_bin_msg6b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg6b()),
    ok.

per_bin_msg7(suite) ->
    [];
per_bin_msg7(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg7()),
    ok.

per_bin_msg8a(suite) ->
    [];
per_bin_msg8a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg8a()),
    ok.

per_bin_msg8b(suite) ->
    [];
per_bin_msg8b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg8b()),
    ok.

per_bin_msg9(suite) ->
    [];
per_bin_msg9(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg9()),
    ok.

per_bin_msg10(suite) ->
    [];
per_bin_msg10(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg10()),
    ok.

per_bin_msg11(suite) ->
    [];
per_bin_msg11(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg11()),
    ok.

per_bin_msg12(suite) ->
    [];
per_bin_msg12(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg12()),
    ok.

per_bin_msg13(suite) ->
    [];
per_bin_msg13(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg13()),
    ok.

per_bin_msg14(suite) ->
    [];
per_bin_msg14(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg14()),
    ok.

per_bin_msg15(suite) ->
    [];
per_bin_msg15(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg15()),
    ok.

per_bin_msg16(suite) ->
    [];
per_bin_msg16(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg16()),
    ok.

per_bin_msg17(suite) ->
    [];
per_bin_msg17(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg17()),
    ok.

per_bin_msg18(suite) ->
    [];
per_bin_msg18(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg18()),
    ok.

per_bin_msg19(suite) ->
    [];
per_bin_msg19(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg19()),
    ok.

per_bin_msg20(suite) ->
    [];
per_bin_msg20(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg20()),
    ok.

per_bin_msg21(suite) ->
    [];
per_bin_msg21(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg21()),
    ok.

per_bin_msg22a(suite) ->
    [];
per_bin_msg22a(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22a()),
    ok.

per_bin_msg22b(suite) ->
    [];
per_bin_msg22b(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22b()),
    ok.

per_bin_msg22c(suite) ->
    [];
per_bin_msg22c(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22c()),
    ok.

per_bin_msg22d(suite) ->
    [];
per_bin_msg22d(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22d()),
    ok.

per_bin_msg22e(suite) ->
    [];
per_bin_msg22e(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22e()),
    ok.

per_bin_msg22f(suite) ->
    [];
per_bin_msg22f(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg22f()),
    ok.

per_bin_msg23(suite) ->
    [];
per_bin_msg23(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg23()),
    ok.

per_bin_msg24(suite) ->
    [];
per_bin_msg24(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg24()),
    ok.

per_bin_msg25(suite) ->
    [];
per_bin_msg25(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    {equal, 'MegacoMessage'} = do(megaco_per_bin_encoder, [], msg25()),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% per_bin_opt_msg1a(suite) ->
%     [];
% per_bin_opt_msg1a(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg1a()),
%     ok.

% per_bin_opt_msg1b(suite) ->
%     [];
% per_bin_opt_msg1b(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg1b()),
%     ok.

% per_bin_opt_msg2(suite) ->
%     [];
% per_bin_opt_msg2(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg2()),
%     ok.

% per_bin_opt_msg3(suite) ->
%     [];
% per_bin_opt_msg3(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg3()),
%     ok.

% per_bin_opt_msg4(suite) ->
%     [];
% per_bin_opt_msg4(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg4()),
%     ok.

% per_bin_opt_msg5(suite) ->
%     [];
% per_bin_opt_msg5(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg5()),
%     ok.

% per_bin_opt_msg6a(suite) ->
%     [];
% per_bin_opt_msg6a(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg6a()),
%     ok.

% per_bin_opt_msg6b(suite) ->
%     [];
% per_bin_opt_msg6b(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg6b()),
%     ok.

% per_bin_opt_msg7(suite) ->
%     [];
% per_bin_opt_msg7(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg7()),
%     ok.

% per_bin_opt_msg8a(suite) ->
%     [];
% per_bin_opt_msg8a(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg8a()),
%     ok.

% per_bin_opt_msg8b(suite) ->
%     [];
% per_bin_opt_msg8b(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg8b()),
%     ok.

% per_bin_opt_msg9(suite) ->
%     [];
% per_bin_opt_msg9(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg9()),
%     ok.

% per_bin_opt_msg10(suite) ->
%     [];
% per_bin_opt_msg10(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg10()),
%     ok.

% per_bin_opt_msg11(suite) ->
%     [];
% per_bin_opt_msg11(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg11()),
%     ok.

% per_bin_opt_msg12(suite) ->
%     [];
% per_bin_opt_msg12(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg12()),
%     ok.

% per_bin_opt_msg13(suite) ->
%     [];
% per_bin_opt_msg13(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg13()),
%     ok.

% per_bin_opt_msg14(suite) ->
%     [];
% per_bin_opt_msg14(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg14()),
%     ok.

% per_bin_opt_msg15(suite) ->
%     [];
% per_bin_opt_msg15(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg15()),
%     ok.

% per_bin_opt_msg16(suite) ->
%     [];
% per_bin_opt_msg16(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg16()),
%     ok.

% per_bin_opt_msg17(suite) ->
%     [];
% per_bin_opt_msg17(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg17()),
%     ok.

% per_bin_opt_msg18(suite) ->
%     [];
% per_bin_opt_msg18(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg18()),
%     ok.

% per_bin_opt_msg19(suite) ->
%     [];
% per_bin_opt_msg19(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg19()),
%     ok.

% per_bin_opt_msg20(suite) ->
%     [];
% per_bin_opt_msg20(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg20()),
%     ok.

% per_bin_opt_msg21(suite) ->
%     [];
% per_bin_opt_msg21(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg21()),
%     ok.

% per_bin_opt_msg22a(suite) ->
%     [];
% per_bin_opt_msg22a(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22a()),
%     ok.

% per_bin_opt_msg22b(suite) ->
%     [];
% per_bin_opt_msg22b(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22b()),
%     ok.

% per_bin_opt_msg22c(suite) ->
%     [];
% per_bin_opt_msg22c(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22c()),
%     ok.

% per_bin_opt_msg22d(suite) ->
%     [];
% per_bin_opt_msg22d(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22d()),
%     ok.

% per_bin_opt_msg22e(suite) ->
%     [];
% per_bin_opt_msg22e(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22e()),
%     ok.

% per_bin_opt_msg22f(suite) ->
%     [];
% per_bin_opt_msg22f(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg22f()),
%     ok.

% per_bin_opt_msg23(suite) ->
%     [];
% per_bin_opt_msg23(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg23()),
%     ok.

% per_bin_opt_msg24(suite) ->
%     [];
% per_bin_opt_msg24(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg24()),
%     ok.

% per_bin_opt_msg25(suite) ->
%     [];
% per_bin_opt_msg25(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     {equal, 'MegacoMessage'} = do(megaco_per_bin_opt_encoder, [], msg25()),
%     ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
%% Ticket test cases:


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg1(suite) ->
    [];
compact_otp4011_msg1(Config) when list(Config) ->
    d("compact_otp4011_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/1 ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
    ok = compact_otp4011(M).


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg2(suite) ->
    [];
compact_otp4011_msg2(Config) when list(Config) ->
    d("compact_otp4011_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/1 ML T=233350{C=${A=stedevice/01{M{O{MO=SO,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SR}}}}}",
    ok = compact_otp4011(M).


%% --------------------------------------------------------------
%% Observe that this decode SHALL fail
compact_otp4011_msg3(suite) ->
    [];
compact_otp4011_msg3(Config) when list(Config) ->
    d("compact_otp4011_msg3 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "!/1 ML T=233350{C=${A=stedevice/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=OFF,MO=SO}}}}}",
    ok = compact_otp4011(M).


compact_otp4011(M) ->
    case d(megaco_compact_text_encoder, [], list_to_binary(M)) of
	{ok, _} ->
	    exit({decoded_erroneous_message,M});
	{error, {decode_error, Error}} when list(Error) -> % Expected result
	    d("compact_otp4011 -> expected error result (so far)", []),
	    case lists:keysearch(reason,1,Error) of
		{value, {reason,Reason}} ->
		    d("compact_otp4011 -> expected error: "
		      "~n   Reason: ~p", [Reason]),
		    case Reason of
			{0, megaco_text_parser, 
			 {do_merge_control_streamParms, [A,B]}} 
			when list(A), record(B, 'LocalControlDescriptor') ->
			    case lists:keysearch(mode,1,A) of
				{value, {mode,Mode}} 
				when B#'LocalControlDescriptor'.streamMode /= asn1_NOVALUE ->
				    d("compact_otp4011 -> expected error",[]),
				    ok;
				Other ->
				    exit({unexpected_mode_reason, {A,B,Other}})
			    end;
			Other ->
			    exit({unexpected_reason, Other})
		    end;

		false ->
		    d("compact_otp4011 -> OUPS, wrong kind of error", []),
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    d("compact_otp4011 -> unexpected decode result", []),
	    exit({unexpected_decode_result, Else})
    end.
    
%% --------------------------------------------------------------
%% Observe that this decode SHALL fail, because of the misspelled
%% MEGCAO instead of the correct MEGACO.
compact_otp4013_msg1(suite) ->
    [];
compact_otp4013_msg1(Config) when list(Config) ->
    d("compact_otp4013_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = "MEGCAO/1 MG1 T=12345678{C=-{SC=root{SV{MT=RS,RE=901}}}}",
    case d(megaco_compact_text_encoder, [], list_to_binary(M)) of
	{ok, _} ->
	    exit({decoded_erroneous_message,M});
	{error, {decode_error, Error}} when list(Error) -> % Expected result
	    case lists:keysearch(reason,1,Error) of
		{value, {reason,{{case_clause,"megcao"},_}}} ->
		    ok;
		false ->
		    exit({unexpected_result,Error})
	    end;
	Else ->
	    exit({unexpected_decode_result,Else})
    end.
	    


%% --------------------------------------------------------------
%% 
%% 
compact_otp4085_msg1(suite) ->
    [];
compact_otp4085_msg1(Config) when list(Config) ->
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M = compact_otp4085_erroneous_msg(),
    case d(megaco_compact_text_encoder, [], list_to_binary(M)) of
	{ok, M} ->
	    exit({decoded_erroneous_message,M});
	{error, {decode_error, Error}} when list(Error) -> % Expected result
	    t("compact_otp4085_msg1 -> decode failed", []),
	    case lists:keysearch(reason, 1, Error) of
		{value, {reason,{999999, Module, Crap}}} ->
		    t("compact_otp4085_msg1 -> THE ACTUAL ERROR: "
		      "~n   LINE NUMBER: 999999"
		      "~n   Module: ~p"
		      "~n   Crap:   ~p", [Module, Crap]),
		    %% ok;
		    exit({decode_failed_999999, Module, Crap});
		{value, {reason,{Line, Module, Crap}}} ->
		    t("compact_otp4085_msg1 -> Expected: "
		      "~n   Line:   ~p"
		      "~n   Module: ~p"
		      "~n   Crap:   ~p", [Line, Module, Crap]),
		    ok;
		false ->
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    exit({unexpected_decode_result, Else})
    end.


%% --------------------------------------------------------------
%% This test case is just to show that the message used in
%% compact_otp4085_msg1 is actually ok when you add '}' at the end.
compact_otp4085_msg2(suite) ->
    [];
compact_otp4085_msg2(Config) when list(Config) ->
    d("compact_otp4085_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    M1 = compact_otp4085_erroneous_msg() ++ "}",
    case d(megaco_compact_text_encoder, [], list_to_binary(M1)) of
	{ok, M2} ->
	    l("compact_otp4085_msg1 -> successfull decode"
	      "~n   M2: ~p", [M2]),
	    ok;
	Else ->
	    e("compact_otp4085_msg1 -> decode error"
	      "~n   Else: ~p", [Else]),
	    exit({unexpected_decode_result,Else})
    end.


%% This message lack the ending parentesis (}).
compact_otp4085_erroneous_msg() ->
    M = "!/1 ML T=11223342{C=${A=${M{O{MO=SR,RV=OFF,RG=OFF},L{v=0,"
	"c=ATM NSAP $ ,"
	"a=eecid:$ ,"
	"m=audio - AAL1/ATMF -,"
	"}}},A=stee1181/01{M{O{MO=SR,RV=OFF,RG=OFF,tdmc/ec=off}}}}",
    M.

%% --------------------------------------------------------------
%% 
%% 
compact_otp4280_msg1(suite) ->
    [];
compact_otp4280_msg1(Config) when list(Config) ->
    d("compact_otp4280_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    BinMsg = list_to_binary(compact_otp4280_msg()),
    case d(megaco_compact_text_encoder, [], BinMsg) of
	{ok, Msg} ->
	    ok;
	{error, {decode_error, Error}} when list(Error) -> 
	    t("compact_otp4280_msg1 -> decode failed", []),
	    case lists:keysearch(reason, 1, Error) of
		{value, {reason,{Line, Module, Reason} = R}} ->
		    t("compact_otp4280_msg1 -> "
		      "~n   Line:   ~w"
		      "~n   Module: ~w"
		      "~n   Reason: ~w", [Line, Module, Reason]),
		    exit({decode_failed, R});
		false ->
		    exit({unexpected_result, Error})
	    end;
	Else ->
	    exit({unexpected_decode_result, Else})
    end.

compact_otp4280_msg() ->
    M = "!/1 mgw1 P=71853646{C=-{AV=root{M{TS{root/maxnumberofcontexts=49500,"
	"root/maxterminationspercontext=2,root/normalmgexecutiontime=200,"
	"root/normalmgcexecutiontime=150,"
	"root/provisionalresponsetimervalue=2000,BF=OFF,SI=IV}}}}}",
    M.


%% --------------------------------------------------------------
%% This ticket is about comments in a message
compact_otp4299_msg1(suite) ->
    [];
compact_otp4299_msg1(Config) when list(Config) ->
    d("compact_otp4299_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    BinMsg = list_to_binary(compact_otp4299_msg()),
    case d(megaco_compact_text_encoder, [], BinMsg) of
	{ok, Msg} ->
	    ok;

	{error, Reason} ->
	    exit({decode_error, Reason});

	Else ->
	    exit({unexpected_decode_result, Else})
    end.


%% Same message, but this time decoded using the flex scanner
compact_otp4299_msg2(suite) ->
    [];
compact_otp4299_msg2(Config) when list(Config) ->
    d("compact_otp4299_msg2 -> entry", []),
    ?ACQUIRE_NODES(1, Config),

    {Pid, Conf} = compact_otp4299_msg2_init(),
    
    BinMsg = list_to_binary(compact_otp4299_msg()),
    Res = d(megaco_compact_text_encoder, [Conf], BinMsg),
    compact_otp4299_msg2_finish(Pid),

    case Res of
	{ok, Msg} ->
	    ok;

	{error, Reason} ->
	    exit({decode_error, Reason});

	Else ->
	    exit({unexpected_decode_result, Else})
    end.


compact_otp4299_msg2_init() ->
    Flag = process_flag(trap_exit, true),
    Res = (catch start_flex_scanner()),
    process_flag(trap_exit, Flag),
    case Res of
	{error, Reason} ->
	    skip(Reason);
	{Pid, Conf} when pid(Pid) ->
	    {Pid, Conf}
    end.

compact_otp4299_msg2_finish(Pid) ->
    stop_flex_scanner(Pid).
    
    
compact_otp4299_msg() ->
    M = ";KALLE\n"
	"!/1 mg58_1 P=005197711{; YET ANOTHER COMMENT\n"
	"C=035146207{A=mg58_1_1_4_1_23/19; BEFORE COMMA\n"
	",; AFTER COMMA\n"
	"A=eph58_1/0xA4023371{M{L{\n"
	"v=0\n"
	"c=ATM NSAP 39.0102.0304.0506.0708.090a.0b58.0100.0000.0000.00\n"
	"m=audio - AAL1/ATMF -\n"
	"a=eecid:A4023371\n"
	"}}; HOBBE\n}; KALLE \"HOBBE \n}}"
	";KALLE\n\n",
    M.


%% --------------------------------------------------------------
%% 
%% 
compact_otp4359_msg1(suite) ->
    [];
compact_otp4359_msg1(Config) when list(Config) ->
    d("compact_otp4359_msg1 -> entry", []),
    ?ACQUIRE_NODES(1, Config),
    BinMsg = list_to_binary(compact_otp4359_msg()),
    case d(megaco_compact_text_encoder, [], BinMsg) of
	{ok, #'MegacoMessage'{mess = Mess}} ->
	    {transactions, Trans} = Mess#'Message'.messageBody,
	    case Trans of
		[{transactionRequest,#'TransactionRequest'{transactionId = asn1_NOVALUE}}] ->
		    ok;
		Else ->
		    exit({unexpected_transactions, Trans})
	    end;
	Else ->
	    t("compact_otp4359_msg1 -> "
	      "~n   Else: ~w", [Else]),
	    exit({unexpected_decode_result, Else})
    end.

compact_otp4359_msg() ->
    M = "!/1 ml2 T={C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}",
    M.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_time_test(suite) ->
    [];
pretty_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_pretty_text_encoder, "Pretty"),
    {ok, {pretty, Res}}.


flex_pretty_time_test(suite) ->
    [];
flex_pretty_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    case (catch start_flex_scanner()) of
	{error, Reason} ->
% 	    ?LOG("flex_pretty_time_test -> error: "
% 		 "~n   Reason: ~p~n", [Reason]),
	    skip(Reason);
	{Pid, Conf} when pid(Pid) ->
	    Res = do_time_test(megaco_pretty_text_encoder, 
			       "Flex pretty", [Conf]),
	    stop_flex_scanner(Pid),
	    {ok, {flex_pretty, Res}}
    end.


compact_time_test(suite) ->
    [];
compact_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_compact_text_encoder, "Compact"),
    {ok, {compact, Res}}.


flex_compact_time_test(suite) ->
    [];
flex_compact_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    case (catch start_flex_scanner()) of
	{error, Reason} ->
% 	    ?LOG("flex_compact_time_test -> error: "
% 		 "~n   Reason: ~p~n", [Reason]),
	    skip(Reason);
	{Pid, Conf} when pid(Pid) ->
	    Res = do_time_test(megaco_compact_text_encoder, 
			       "Flex compact", [Conf]),
	    stop_flex_scanner(Pid),
	    {ok, {flex_compact, Res}}
    end.


ber_time_test(suite) ->
    [];
ber_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_ber_encoder, "Ber"),
    {ok, {ber, Res}}.


bin_time_test(suite) ->
    [];
bin_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_binary_encoder, "Binary"),
    {ok, {bin, Res}}.


ber_bin_time_test(suite) ->
    [];
ber_bin_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_ber_bin_encoder, "Ber bin"),
    {ok, {ber_bin, Res}}.


per_time_test(suite) ->
    [];
per_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    case (catch do_time_test(megaco_per_encoder, "Per")) of
	Res when list(Res) ->
	    {ok, {per, Res}};
	{'EXIT', Reason} ->
	    ?LOG("per exit: ~n~p~n", [Reason]),
	    exit(Reason);
	{error, Reason} ->
	    ?LOG("per error: ~n~p~n", [Reason]),
	    exit(Reason);
	Error ->
	    ?LOG("per other: ~n~p~n", [Error]),
	    exit(Error)
    end.
   

per_bin_time_test(suite) ->
    [];
per_bin_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    case (catch do_time_test(megaco_per_bin_encoder, "Per bin")) of
	Res when list(Res) ->
	    {ok, {per_bin, Res}};
	{'EXIT', Reason} ->
	    ?LOG("per_bin exit: ~n~p~n", [Reason]),
	    exit(Reason);
	{error, Reason} ->
	    ?LOG("per_bin error: ~n~p~n", [Reason]),
	    exit(Reason);
	Error ->
	    ?LOG("per_bin other: ~n~p~n", [Error]),
	    exit(Error)
    end.
   

% per_bin_opt_time_test(suite) ->
%     [];
% per_bin_opt_time_test(Config) when list(Config) ->
%     ?ACQUIRE_NODES(1, Config),
%     case (catch do_time_test(megaco_per_bin_opt_encoder, "Per bin")) of
% 	Res when list(Res) ->
% 	    {ok, {per_bin_opt, Res}};
% 	{'EXIT', Reason} ->
% 	    ?LOG("per_bin_opt exit: ~n~p~n", [Reason]),
% 	    exit(Reason);
% 	{error, Reason} ->
% 	    ?LOG("per_bin_opt error: ~n~p~n", [Reason]),
% 	    exit(Reason);
% 	Error ->
% 	    ?LOG("per_bin_opt other: ~n~p~n", [Error]),
% 	    exit(Error)
%     end.
   

erl_dist_time_test(suite) ->
    [];
erl_dist_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Res = do_time_test(megaco_erl_dist_encoder, "Erlang distribution"),
    {ok, {erl_dist, Res}}.
   

erl_dist_compressed_time_test(suite) ->
    [];
erl_dist_compressed_time_test(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Conf = [compressed],
    Res = do_time_test(megaco_erl_dist_encoder, 
		       "Erlang compressed distribution", Conf),
    {ok, {erl_dist_comp, Res}}.
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_time_test(Codec, Desc) ->
    do_time_test(Codec, Desc, []).

do_time_test(Codec, Desc, Conf) when atom(Codec), list(Conf) ->
    Msgs    = do_time_test_msgs(),
    %% Double of the measure time: 
    %% 5 * measure time for a message * number of messages
    %% For most codec's this differ for encode and decode, but...
    Timeout = 5 * (?TIME_TEST_COUNT_TIME) * length(Msgs), 
    Fun1    = fun({_, _,   _, Count}) -> Count end,
    Fun2    = fun({_, Bin, _, Count}) -> Count*size(Bin) end,
    io:format("~s codec~n", [Desc]),
    io:format("   encode: ", []),
    
    Encodes = do_time_test_encode(Timeout, Codec, Desc, Conf, Msgs, []),
    BinMsgs = [{Name, BinMsg} || {Name, BinMsg, _Time, _Count} <- Encodes],
    io:format("   decode: ", []),
    Decodes = do_time_test_decode(Timeout, Codec, Desc, Conf, BinMsgs, []),

    %% Total number of messages that has been encoded/decoded
    ECount  = lists:sum([Count || {_, _, _, Count} <- Encodes]),
    %% io:format("ECount: ~p~n", [ECount]),
    DCount  = lists:sum([Count || {_, _, _, Count} <- Decodes]),
    %% io:format("DCount: ~p~n", [DCount]),

    %% Total time for encoding/decoding the messages
    ETime   = lists:sum([Time || {_, _, Time, _} <- Encodes]),
    %% io:format("ETime: ~p~n", [ETime]),
    DTime   = lists:sum([Time || {_, _, Time, _} <- Decodes]),
    %% io:format("DTime: ~p~n", [DTime]),
    
    %% Average message size
    AvgSz   = lists:sum([size(B) || {_,B,_,_} <- Encodes])/length(Encodes),

    ETime1 = ETime/ECount,
    DTime1 = DTime/DCount, 
    TTime1 = ETime1 + DTime1, 

    io:format("   time avg: "
	      "~n      Encode: ~p microsec" 
	      "~n      Decode: ~p microsec" 
	      "~n      Total:  ~p microsec"
	      "~n      Avg msg (encoded) size: ~p bytes~n", 
	      [ETime1, DTime1, TTime1, AvgSz]),

    Ms = do_time_test1(Encodes, Decodes),

%     io:format("~s time test done~n", [Desc]),

    [{desc, Desc}, 
     {encode, ETime1}, {decode, DTime1}, {size, AvgSz},
     {bins, Ms}].


do_time_test1(Msgs1, Msgs2) ->
    io:format("   messages sorted by size:~n", []),
    Msgs = do_time_test2(Msgs1, Msgs2, []),
    do_time_test3(lists:keysort(3, Msgs)),
    Msgs.

do_time_test2([], [], M) ->
    M;
do_time_test2([{Name, Bin, ETime, ECount}| Msgs1], 
	      [{Name, Msg, DTime, DCount}| Msgs2], M) ->
    do_time_test2(Msgs1, Msgs2, 
		  [{Name, Bin, size(Bin), ETime/ECount, DTime/DCount}| M]).

do_time_test3([]) ->
    io:format("~n", []),
    ok;
do_time_test3([{Name, _Bin, Size, ETime, DTime}| Msgs]) ->
    io:format("      ~p: Size: ~p, Encode: ~p, Decode: ~p~n", 
	      [Name, Size, ETime, DTime]),
    do_time_test3(Msgs).


do_time_test_encode(Timeout, Codec, Desc, Conf, [], BinMsgs) ->
    io:format("~n", []),
    lists:reverse(BinMsgs);
do_time_test_encode(Timeout, Codec, Desc, Conf, [{Name, M}|Msgs], BinMsgs)  ->
    io:format(".", []),
    case do_time_test_encode(Timeout, Codec, Desc, Conf, M) of
	{ok, {BinMsg, Count, Time}} ->
	    do_time_test_encode(Timeout, Codec, Desc, Conf, Msgs, 
				[{Name, BinMsg, Time, Count}|BinMsgs]);
	Else ->
	    throw({error, {time_test_failed, Codec, Name, M, Else}})
    end.
    
do_time_test_encode(Timeout, Codec, Desc, Conf, Msg) ->
    Pid = spawn_link(?MODULE, do_time_tester, 
		     [self(), {Codec, encode_message, [Conf, Msg]}]),
    receive
        {measure_result, Pid, Res} ->
            {ok, Res};
        {error, Pid, Error} ->
            {error, Error};
	{'EXIT', Pid, Reason} ->
	    ?LOG("time tester ~s exited: ~n~p~n", [Desc, Reason]),
	    exit({time_tester_crash, Reason})
    after Timeout ->
	    exit(no_result_from_time_tester)
    end.


do_time_test_decode(Timeout, Codec, Desc, Conf, [], Msgs) ->
    io:format("~n", []),
    lists:reverse(Msgs);
do_time_test_decode(Timeout, Codec, Desc, Conf, [{Name, B}|BinMsgs], Msgs)  ->
    io:format(".", []),
    case do_time_test_decode(Timeout, Codec, Desc, Conf, B) of
	{ok, {Msg, Count, Time}} ->
	    do_time_test_decode(Timeout, Codec, Desc, Conf, BinMsgs, 
				[{Name, Msg, Time, Count}|Msgs]);
	Else ->
	    throw({error, {time_test_failed, Codec, Name, B, Else}})
    end.
    
do_time_test_decode(Timeout, Codec, Desc, Conf, Msg) ->
    Pid = spawn_link(?MODULE, do_time_tester, 
		     [self(), {Codec, decode_message, [Conf, Msg]}]),
    receive
        {measure_result, Pid, Res} ->
            {ok, Res};
        {error, Pid, Error} ->
            {error, Error};
	{'EXIT', Pid, Reason} ->
	    ?LOG("time tester ~s exited: ~n~p~n", [Desc, Reason]),
	    exit({time_tester_crash, Reason})
    after Timeout ->
	    exit(no_result_from_time_tester)
    end.


%% do_time_test_codec(Desc, Codec, Func, Conf, M) ->
%%     Pid = spawn_link(?MODULE, do_time_tester, 
%% 			[self(), {Codec, Func, [Conf, M]}]),
%%     receive
%% 	   {measure_result, Pid, Res} ->
%% 	       {ok, Res};
%% 	   {error, Pid, Error} ->
%% 	       {error, Error};
%% 	   {'EXIT', Pid, Reason} ->
%% 	       ?LOG("time tester ~s exited: ~n~p~n", [Desc, Reason]),
%% 	       exit({time_tester_crash, Reason})
%%     after ?TIME_TEST_TIMEOUT -> 
%% 	       exit(no_result_from_time_tester)
%%     end.


do_time_tester(Parent, MFA) ->
    {ok, Count} = time_test_warmup(MFA, 1000),
    %% io:format("~nCount: ~p~n", [Count]),
    Res = timer:tc(?MODULE, do_time_tester_loop, [MFA, Count, dummy]),
    case Res of
	{Time, {ok, M}} ->
	    Parent ! {measure_result, self(), {M, Count, Time}};
	{Time, Error} ->
	    Parent ! {error, self(), Error}
    end,
    unlink(Parent),
    exit(normal).

%% This function does more mor less what the real measure function
%% above does. But with the diff:
%% 1) Warmup to ensure that all used code are loaded
%% 2) To aproximate the encoding time, to ensure that 
%%    the real encode is done with enough iterations
%%    (not to many, not to few).
time_test_warmup(MFA, MCount) ->
    Res = timer:tc(?MODULE, do_time_tester_loop, [MFA, MCount, dummy]),
    case Res of
        {Time, {ok, _}} ->
            %% OK so far, now calculate the count:
            Count = round(?TIME_TEST_COUNT_TIME/(Time/MCount)),
            %% io:format("~w ", [Count]),
            {ok, Count};
        {_Time, Error} ->
            {error, {warmup_failed, Error}}
    end.


do_time_tester_loop(_MFA, 0, Msg) ->
    {ok, Msg};
do_time_tester_loop({M, F, A} = MFA, Count, _) ->
    {ok, Msg} = apply(M, F, A),
    do_time_tester_loop(MFA, Count - 1, Msg).


% do_time_tester_loop(ED, Msg, Codec, Conf, Count, EdMsg) ->
%     receive
% 	stop ->
% 	    {Count, EdMsg}
%     after 0 ->
% 	    M2 = do_time_test1(ED, Codec, Conf, Msg),
% 	    do_time_tester_loop(ED, Msg, Codec, Conf, Count + 1, M2)
%     end.
    

% do_time_test1(encode = ED, Codec, Conf, Msg) ->
%     case e(Codec, Conf, Msg) of
% 	{ok, BinMsg} ->
% 	    BinMsg;
% 	{error, {encode_error, {asn1, {undef, L}}} = Reason} when list(L) ->
% 	    %% Very crude way to find out if the problems is that per_bin
% 	    %% is not available in the "current" asn1 version
% 	    case lists:keysearch(asn1rt_per_bin, 1, L) of
% 		{value, E} ->
% 		    ?SKIP(no_per_bin);
% 		false ->
% 		    exit(Reason)
% 	    end;
% 	{error, Reason} ->
% 	    exit(Reason);
% 	Error ->
% 	    exit({encode_failed, Error})
%     end;
% do_time_test1(decode = ED, Codec, Conf, BinMsg) ->
%     case d(Codec, Conf, BinMsg) of
% 	{ok, Msg} ->
% 	    Msg;
% 	{error, Reason} ->
% 	    exit(Reason);
% 	Error ->
% 	    exit({decode_failed, Error})
%     end.	    


do_time_test_msgs() ->
    [{msg01a, msg1a()}, 
     {msg01b, msg1b()},
     {msg02,  msg2()},  
     {msg03,  msg3()},  
     {msg04,  msg4()},  
     {msg05,  msg5()}, 
     {msg06a, msg6a()}, 
     {msg06b, msg6b()}, 
     {msg07,  msg7()}, 
     {msg08a, msg8a()}, 
     {msg08b, msg8b()}, 
     {msg09,  msg9()}, 
     {msg10,  msg10()}, 
     {msg11,  msg11()}, 
     {msg12,  msg12()}, 
     {msg13,  msg13()}, 
     {msg14,  msg14()}, 
     {msg15,  msg15()}, 
     {msg16,  msg16()}, 
     {msg17,  msg17()}, 
     {msg18,  msg18()}, 
     {msg19,  msg19()}, 
     {msg20,  msg20()}, 
     {msg21,  msg21()}, 
     {msg22a, msg22a()}, 
     {msg23,  msg23()}, 
     {msg24,  msg24()}, 
     {msg25,  msg25()}].
    
%      {msg22b, msg22b()}, 
%      {msg22c, msg22c()}, 
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do(Codec, Conf, M1) ->
    %% d("do -> entry", []),
    case (catch encode_decode(Codec, Conf, M1)) of
	{ok, M2} ->
	    %% d("do -> check messages", []),
	    case (catch chk_MegacoMessage(M1, M2)) of
		{equal, M} -> 
		    {equal, M};
		{error, Reason} ->
		    exit({error, {Reason, M1, M2}});
		{wrong_type, What} ->
		    exit({wrong_type, What, M1, M2});
		{not_equal, What} ->
		    exit({not_equal, What, M1, M2})
	    end;
	{error, {encode_error, {asn1, {undef, L}}} = Reason} when list(L) ->
	    %% Very crude way to find out if the problems is that per_bin
	    %% is not available in the "current" asn1 version
	    case lists:keysearch(asn1rt_per_bin, 1, L) of
		{value, E} ->
		    ?SKIP(no_per_bin);
		false ->
		    exit(Reason)
	    end;
	{error, Reason} ->
	    exit(Reason); 
	{'EXIT', Reason} ->
	    exit(Reason)
    end.


encode_decode(Codec, Conf, M1) ->
%     d("encode_decode -> entry with"
%       "~n   Codec: ~p"
%       "~n   Conf:  ~p"
%       "~n   M1:    ~p", [Codec, Conf, M1]),
    case e(Codec,Conf,M1) of
	{ok,B} ->
	    d(Codec,Conf,B);
	Error ->
	    Error
    end.
    

e(Codec,Config,Msg) ->
%     d("e -> entry with"
%       "~n   Codec:  ~p"
%       "~n   Config: ~p", [Codec, Config]),
    case (catch apply(Codec,encode_message,[Config,Msg])) of
	{ok,EncodedMsg} when binary(EncodedMsg) ->
%  	    t("e -> decode ok, size: ~p"
% 	      "~n   ~p", [size(EncodedMsg),binary_to_list(EncodedMsg)]),
	    {ok,EncodedMsg};
	{'EXIT',Reason} ->
% 	    e("e -> encode exit signal:"
% 	      "~n   Reason: ~p", [Reason]),
	    {error, {encode_exit, Reason, Msg}};
	{error,Reason} ->
% 	    e("e -> encode error:"
% 	      "~n   Reason: ~p", [Reason]),
	    {error, {encode_error, Reason, Msg}};
	Other ->
% 	    l("e -> unknown reply:"
% 	      "~n   Other: ~p", [Other]),
	    Other
    end.

d(Codec, Config, BinMsg) when binary(BinMsg) ->
%     d("d -> entry with"
%       "~n   Codec:  ~p"
%       "~n   Config: ~p", [Codec, Config]),
    case (catch apply(Codec,decode_message,[Config, BinMsg])) of
	{ok,DecodedMsg} ->
% 	    t("d -> ~n   ~p",[DecodedMsg]),
	    {ok,DecodedMsg};
	{'EXIT',Reason} ->
	    e("d -> exit signal:"
	      "~n   Reason: ~p", [Reason]),
	    {error,{decode_exit,Reason}};
	{error,Reason} ->
	    e("d -> decode error:"
	      "~n   Reason: ~p", [Reason]),
	    {error,{decode_error,Reason}};
	Other ->
	    l("d -> unknown reply:"
	      "~n   Other: ~p", [Other]),
	    Other
    end.    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msg_request(Mid, TransId, ContextId, CmdReq) when list(CmdReq) ->
    Actions = [#'ActionRequest'{contextId = ContextId,
                                commandRequests = CmdReq}],
    Req = {transactions,
           [{transactionRequest,
             #'TransactionRequest'{transactionId = TransId,
                                   actions = Actions}}]},
    #'MegacoMessage'{mess = #'Message'{version = 1,
                                       mId = Mid,
                                       messageBody = Req}}.

msg_reply(Mid, TransId, ContextId, CmdReply) when list(CmdReply) ->
    Actions = [#'ActionReply'{contextId = ContextId,
                              commandReply = CmdReply}],
    Req = {transactions,
           [{transactionReply,
             #'TransactionReply'{transactionId = TransId,
                                 transactionResult = 
				 {actionReplies, Actions}}}]},
    #'MegacoMessage'{mess = #'Message'{version = 1,
                                       mId = Mid,
                                       messageBody = Req}}.


%% -------------------------------------------------------------------------


msg1(Mid, Tid) ->
    Gain  = cre_propertyParm("tdmc/gain", "2"),
    Ec    = cre_propertyParm("tdmc/ec", "g165"), 
    LCD   = cre_localControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_propertyParm("v", "0"),
    %% C    = cre_propertyParm("c", "IN IP4 $ "),
    C     = cre_propertyParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    A     = cre_propertyParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_localRemoteDesc([[V, C, M, A]]),
    Parms = cre_streamParms(LCD,LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    ReqEvent   = cre_requestedEvent("al/of"),
    EventsDesc = cre_eventsDesc(2222,[ReqEvent]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).


msg1a() ->
    msg1a(?MGC_MID).
msg1a(Mid) ->
    msg1(Mid, ?A4444).

msg1b() ->
    msg1b(?MGC_MID).
msg1b(Mid) ->
    msg1(Mid, ?A4445).


%% --------------------------


msg2() ->
    msg2(?MGC_MID).
msg2(Mid) ->
    msg2(Mid, ?A4444).
msg2(Mid, Tid) ->
    Gain  = cre_propertyParm("tdmc/gain", "2"),
    Ec    = cre_propertyParm("tdmc/ec", "g165"), 
    LCD   = cre_localControlDesc(sendRecv,[Gain, Ec]),
    V     = cre_propertyParm("v", "0"),
    %% C    = cre_propertyParm("c", "IN IP4 $ "),
    C     = cre_propertyParm("c", [$I,$N,$ ,$I,$P,$4,$ ,$$,$ ]),
    M     = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    A     = cre_propertyParm("a", "fmtp:PCMU VAD=X-NNVAD"),
    LD    = cre_localRemoteDesc([[V, C, M, A]]),
    Parms = cre_streamParms(LCD,LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    EventParm  = cre_eventParm("strict",["exact"]),
    ReqEvent   = cre_requestedEvent("al/of", [EventParm]),
    EventsDesc = cre_eventsDesc(2222,[ReqEvent]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = Tid}],
			    [{mediaDescriptor, MediaDesc},
			     {eventsDescriptor, EventsDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 9999, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg3() ->
    msg3(?MG1_MID).
msg3(Mid) ->
    TimeStamp = cre_timeNotation("19990729", "22000000"),
    Event     = cre_observedEvent("al/of",TimeStamp),
    Desc      = cre_observedEventsDesc(2222,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A4444}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10000, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg4() ->
    msg4(?MG1_MID_NO_PORT).
msg4(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_serviceChangeProf("resgw",1),
    Parm    = cre_serviceChangeParm(restart,Address,
				    ["901 mg col boot"],Profile),
    Req     = cre_serviceChangeReq([?megaco_root_termination_id],Parm),
    CmdReq  = cre_commandReq({serviceChangeReq, Req}),
    msg_request(Mid, 9998, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg5() ->
    msg5(?MGC_MID).
msg5(Mid) ->
    Address = {portNumber, ?DEFAULT_PORT},
    Profile = cre_serviceChangeProf("resgw",1),
    Parm    = cre_serviceChangeResParm(Address,Profile),
    Reply   = cre_serviceChangeReply([?megaco_root_termination_id],
				     {serviceChangeResParms,Parm}),
    msg_reply(Mid, 9998, ?megaco_null_context_id,
	      [{serviceChangeReply, Reply}]).


%% --------------------------

msg6(Mid, Tid) ->
    Reply = cre_ammsReply([#megaco_term_id{id = Tid}]),
    msg_reply(Mid, 9999, ?megaco_null_context_id, [{modReply, Reply}]).

msg6a() ->
    msg6a(?MG1_MID).
msg6a(Mid) ->
    msg6(Mid, ?A4444).

msg6b() ->
    msg6b(?MG2_MID).
msg6b(Mid) ->
    msg6(Mid, ?A5555).


%% --------------------------

msg7() ->
    msg7(?MGC_MID).
msg7(Mid) ->
    Reply = cre_notifyReply([#megaco_term_id{id = ?A4444}]),
    msg_reply(Mid, 10000, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg8(Mid, DigitMapValue) ->
    Name = "dialplan00",
    On     = cre_requestedEvent("al/on"),
    Action = cre_requestedActions(Name),
    Ce     = cre_requestedEvent("dd/ce", Action),
    EventsDesc = cre_eventsDesc(2223,[On, Ce]),
    Signal     = cre_signal("cg/rt"),
    DigMapDesc = cre_digitMapDesc(Name, DigitMapValue),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4444}],
                           [{eventsDescriptor, EventsDesc},
			    {signalsDescriptor, [{signal, Signal}]},
			    {digitMapDescriptor, DigMapDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 10001, ?megaco_null_context_id, [CmdReq]).

msg8a() ->
    msg8a(?MGC_MID).
msg8a(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_digitMapValue(Body),
    msg8(Mid, Value).

msg8b() ->
    msg8b(?MGC_MID).
msg8b(Mid) ->
    Body = "(0s| 00s|[1-7]xlxx|8lxxxxxxx|#xxxxxxx|*xx|9l1xxxxxxxxxx|9l011x.s)",
    Value = cre_digitMapValue(Body, 1, 23, 99),
    msg8(Mid, Value).


%% --------------------------

msg9() ->
    msg9(?MG1_MID).
msg9(Mid) ->
    TimeStamp = cre_timeNotation("19990729","22010001"),
    Parm      = cre_eventParm("ds",["916135551212"]),
    Event     = cre_observedEvent("dd/ce",TimeStamp,[Parm]),
    Desc      = cre_observedEventsDesc(2223,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A4444}], Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 10002, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg10() ->
    msg10(?MGC_MID).
msg10(Mid) ->
    AmmReq = cre_ammReq([#megaco_term_id{id = ?A4444}],[]),
    CmdReq = cre_commandReq({addReq, AmmReq}),
    Jit = cre_propertyParm("nt/jit", "40"),
    LCD = cre_localControlDesc(recvOnly,[Jit]),
    V   = cre_propertyParm("v", "0"),
    C   = cre_propertyParm("c", "IN IP4 $ "),
    M   = cre_propertyParm("m", "audio $ RTP/AVP 4"),
    A   = cre_propertyParm("a", "ptime:30"),
    V2  = cre_propertyParm("v", "0"),
    C2  = cre_propertyParm("c", "IN IP4 $ "),
    M2  = cre_propertyParm("m", "audio $ RTP/AVP 0"),
    LD  = cre_localRemoteDesc([[V, C, M, A], [V2, C2, M2]]),
    Parms      = cre_streamParms(LCD, LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    ChooseTid  = #megaco_term_id{contains_wildcards = true,
				 id = [[?megaco_choose]]},
    AmmReq2    = cre_ammReq([ChooseTid],[{mediaDescriptor, MediaDesc}]),
    CmdReq2    = cre_commandReq({addReq, AmmReq2}),
    msg_request(Mid, 10003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


msg11() ->
    msg11(?MG1_MID).
msg11(Mid) ->
    V  = cre_propertyParm("v", "0"),
    C  = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    M  = cre_propertyParm("m", "audio 2222 RTP/AVP 4"),
    A  = cre_propertyParm("a", "a=ptime:30"),
    A2 = cre_propertyParm("a", "recvonly"),
    LD = cre_localRemoteDesc([[V, C, M, A, A2]]),
    Parms      = cre_streamParmsL(LD),
    StreamDesc = cre_streamDesc(1, Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Reply  = cre_ammsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A4445}],
			   [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 10003, 2000, [{addReply, Reply}, {addReply, Reply2}]).


%% --------------------------

msg12() ->
    msg12(?MGC_MID).
msg12(Mid) ->
    LCD        = cre_localControlDesc(sendRecv),
    Parms      = cre_streamParms(LCD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Signal     = cre_signal("al/ri"),
    Descs      = [{mediaDescriptor, MediaDesc},
		  {signalsDescriptor, [{signal, Signal}]}],
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A5555}], Descs),
    CmdReq     = cre_commandReq({addReq, AmmReq}),
    Jit        = cre_propertyParm("nt/jit", "40"),
    LCD2       = cre_localControlDesc(sendRecv, [Jit]),
    V      = cre_propertyParm("v", "0"),
    C      = cre_propertyParm("c", "IN IP4 $ "),
    M      = cre_propertyParm("m", "audio $ RTP/AVP 4"),
    A      = cre_propertyParm("a", "ptime:30"),
    LD2    = cre_localRemoteDesc([[V, C, M, A]]),
    V2     = cre_propertyParm("v", "0"),
    C2     = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    M2     = cre_propertyParm("m", "audio 2222 RTP/AVP 4"),
    RD2    = cre_localRemoteDesc([[V2, C2, M2]]),
    Parms2 = cre_streamParms(LCD2,LD2,RD2),
    StreamDesc2 = cre_streamDesc(1,Parms2),
    MediaDesc2  = cre_mediaDesc(StreamDesc2),
    ChooseTid   = #megaco_term_id{contains_wildcards = true,
				  id = [[?megaco_choose]]},
    AmmReq2     = cre_ammReq([ChooseTid],[{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_commandReq({addReq, AmmReq2}),
    msg_request(Mid, 50003, ?megaco_choose_context_id, [CmdReq, CmdReq2]).


%% --------------------------

msg13() ->
    msg13(?MG2_MID).
msg13(Mid) ->
    V     = cre_propertyParm("v", "0"),
    C     = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    M     = cre_propertyParm("m", "audio 1111 RTP/AVP 4"),
    LD    = cre_localRemoteDesc([[V, C, M]]),
    Parms = cre_streamParmsL(LD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    Reply      = cre_ammsReply([#megaco_term_id{id = ?A5556}],
			       [{mediaDescriptor, MediaDesc}]),
    msg_reply(Mid, 50003, 5000, [{addReply, Reply}]).


%% --------------------------

msg14() ->
    msg14(?MGC_MID).
msg14(Mid) ->
    Signal = cre_signal("cg/rt"), 
    AmmReq = cre_ammReq([#megaco_term_id{id = ?A4444}],
			[{signalsDescriptor, [{signal, Signal}]}]),
    CmdReq = cre_commandReq({modReq, AmmReq}),
    Gain   = cre_propertyParm("tdmc/gain", "2"),
    Ec     = cre_propertyParm("tdmc/ec", "G165"),
    LCD    = cre_localControlDesc(sendRecv, [Gain, Ec]),
    V      = cre_propertyParm("v", "0"),
    C      = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    M      = cre_propertyParm("m", "audio 1111 RTP/AVP 4"),
    RD2    = cre_localRemoteDesc([[V, C, M]]),
    Parms2 = cre_streamParmsR(RD2),
    StreamDesc2 = cre_streamDesc(1,Parms2),
    MediaDesc2  = cre_mediaDesc(StreamDesc2),
    AmmReq2     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			     [{mediaDescriptor, MediaDesc2}]),
    CmdReq2     = cre_commandReq({modReq, AmmReq2}),
    msg_request(Mid, 10005, 2000, [CmdReq, CmdReq2]).


%% --------------------------

msg15() ->
    msg15(?MG1_MID).
msg15(Mid) ->
    Reply  = cre_ammsReply([#megaco_term_id{id = ?A4444}]),
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 10005, 2000, [{modReply, Reply}, {modReply, Reply2}]).


%% --------------------------

msg16() ->
    msg16(?MG2_MID).
msg16(Mid) ->
    TimeStamp = cre_timeNotation("19990729","22020002"),
    Event     = cre_observedEvent("al/of",TimeStamp),
    Desc      = cre_observedEventsDesc(1234,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50005, 5000, [CmdReq]).


%% --------------------------

msg17() ->
    msg17(?MGC_MID).
msg17(Mid) ->
    Reply = cre_notifyReply([#megaco_term_id{id = ?A5555}]),
    msg_reply(Mid, 50005, ?megaco_null_context_id, [{notifyReply, Reply}]).


%% --------------------------

msg18() ->
    msg18(?MGC_MID).
msg18(Mid) ->
    On         = cre_requestedEvent("al/on"),
    EventsDesc = cre_eventsDesc(1235,[On]),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A5555}],
			    [{eventsDescriptor, EventsDesc},
			     {signalsDescriptor, []}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    msg_request(Mid, 50006, 5000, [CmdReq]).


%% --------------------------

msg19() ->
    msg19(?MG2_MID).
msg19(Mid) ->
    Reply = cre_ammsReply([#megaco_term_id{id = ?A4445}]),
    msg_reply(Mid, 50006, 5000, [{modReply, Reply}]).


%% --------------------------

msg20() ->
    msg20(?MGC_MID).
msg20(Mid) ->
    LCD        = cre_localControlDesc(sendRecv),
    Parms      = cre_streamParms(LCD),
    StreamDesc = cre_streamDesc(1,Parms),
    MediaDesc  = cre_mediaDesc(StreamDesc),
    AmmReq     = cre_ammReq([#megaco_term_id{id = ?A4445}],
			    [{mediaDescriptor, MediaDesc}]),
    CmdReq     = cre_commandReq({modReq, AmmReq}),
    AmmReq2    = cre_ammReq([#megaco_term_id{id = ?A4444}],
                            [{signalsDescriptor, []}]),
    CmdReq2    = cre_commandReq({modReq, AmmReq2}),
    msg_request(Mid, 10006, 2000, [CmdReq, CmdReq2]).


%% --------------------------

msg21() ->
    msg21(?MGC_MID).
msg21(Mid) ->
    Tokens    = [mediaToken, eventsToken, signalsToken,
		 digitMapToken, statsToken, packagesToken],
    AuditDesc = cre_auditDesc(Tokens),
    Req       = cre_auditReq(#megaco_term_id{id = ?A5556},AuditDesc),
    CmdReq    = cre_commandReq({auditValueRequest, Req}),
    msg_request(Mid, 50007, ?megaco_null_context_id, [CmdReq]).


%% --------------------------

msg22a() ->
    msg22(1).

msg22b() ->
    msg22(10).

msg22c() ->
    msg22(25).

msg22d() ->
    msg22(50).

msg22e() ->
    msg22(75).

msg22f() ->
    msg22(100).

msg22(N) ->
    msg22(?MG2_MID, N).
msg22(Mid, N) ->
    Jit = cre_propertyParm("nt/jit", "40"),
    LCD = cre_localControlDesc(sendRecv,[Jit]),
    LDV = cre_propertyParm("v", "0"),
    LDC = cre_propertyParm("c", "IN IP4 125.125.125.111"),
    LDM = cre_propertyParm("m", "audio 1111 RTP/AVP  4"),
    LDA = cre_propertyParm("a", "ptime:30"),
    LD  = cre_localRemoteDesc([[LDV, LDC, LDM, LDA]]),
    RDV = cre_propertyParm("v", "0"),
    RDC = cre_propertyParm("c", "IN IP4 124.124.124.222"),
    RDM = cre_propertyParm("m", "audio 2222 RTP/AVP  4"),
    RDA = cre_propertyParm("a", "ptime:30"),
    RD  = cre_localRemoteDesc([[RDV, RDC, RDM, RDA]]),
    StreamParms   = cre_streamParms(LCD,LD,RD),
    StreamDesc    = cre_streamDesc(1,StreamParms),
    Media         = cre_mediaDesc(StreamDesc),
    PackagesItem  = cre_packagesItem("nt",1),
    PackagesItem2 = cre_packagesItem("rtp",1),
    Stat       = cre_statisticsParm("rtp/ps","1200"),
    Stat2      = cre_statisticsParm("nt/os","62300"),
    Stat3      = cre_statisticsParm("rtp/pr","700"),
    Stat4      = cre_statisticsParm("nt/or","45100"),
    Stat5      = cre_statisticsParm("rtp/pl","0.2"),
    Stat6      = cre_statisticsParm("rtp/jit","20"),
    Stat7      = cre_statisticsParm("rtp/delay","40"),
    Statistics = [Stat, Stat2, Stat3, Stat4, Stat5, Stat6, Stat7],
    Audits     = [{mediaDescriptor, Media},
		  {packagesDescriptor, [PackagesItem, PackagesItem2]},
		  {statisticsDescriptor, Statistics}],
    Reply      = {auditResult, 
		  cre_auditRes(#megaco_term_id{id = ?A5556},Audits)},
    msg_reply(Mid, 50007, ?megaco_null_context_id, 
	      lists:duplicate(N,{auditValueReply, Reply})).
%%     msg_reply(Mid, 50007, ?megaco_null_context_id, 
%% 	      lists.duplicate([{auditValueReply, Reply}]).


%% --------------------------

msg23() ->
    msg23(?MG2_MID).
msg23(Mid) ->
    TimeStamp = cre_timeNotation("19990729","24020002"),
    Event     = cre_observedEvent("al/on",TimeStamp),
    Desc      = cre_observedEventsDesc(1235,[Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = ?A5555}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    msg_request(Mid, 50008, 5000, [CmdReq]).


%% --------------------------

msg24() ->
    msg24(?MGC_MID).
msg24(Mid) ->
    AuditDesc = cre_auditDesc([statsToken]),
    SubReq    = cre_subtractReq([#megaco_term_id{id = ?A5555}], AuditDesc),
    SubReq2   = cre_subtractReq([#megaco_term_id{id = ?A5556}], AuditDesc),
    CmdReq    = cre_commandReq({subtractReq, SubReq}),
    CmdReq2   = cre_commandReq({subtractReq, SubReq2}),
    msg_request(Mid, 50009, 5000, [CmdReq, CmdReq2]).


%% --------------------------

msg25() ->
    msg25(?MG2_MID).
msg25(Mid) ->
    Stat11 = cre_statisticsParm("nt/os","45123"),
    Stat12 = cre_statisticsParm("nt/dur", "40"),
    Stats1 = [Stat11, Stat12],
    Reply1 = cre_ammsReply([#megaco_term_id{id = ?A5555}],
			   [{statisticsDescriptor, Stats1}]),
    Stat21 = cre_statisticsParm("rtp/ps","1245"),
    Stat22 = cre_statisticsParm("nt/os", "62345"),
    Stat23 = cre_statisticsParm("rtp/pr", "780"),
    Stat24 = cre_statisticsParm("nt/or", "45123"),
    Stat25 = cre_statisticsParm("rtp/pl", "10"),
    Stat26 = cre_statisticsParm("rtp/jit", "27"),
    Stat27 = cre_statisticsParm("rtp/delay","48"),
    Stats2 = [Stat21, Stat22],
    Reply2 = cre_ammsReply([#megaco_term_id{id = ?A5556}],
                          [{statisticsDescriptor, Stats2}]),
    msg_reply(Mid, 50009, 5000, 
	      [{subtractReply, Reply1}, {subtractReply, Reply2}]).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%% Compact text messages

%% --------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flex_scanner_conf(Config) when list(Config) ->
    case lists:keysearch(flex_scanner, 1, Config) of
	{value, {flex_scanner, Pid, Conf}} ->
	    case ping_flex_scanner(Pid) of
		ok ->
		    Conf;
		Else ->
		    skip({no_response_from_flex_scanner_handler, Else})
	    end;
	false ->
	    skip("Flex scanner driver not loaded")
    end.


skip({What, Why}) when atom(What), list(Why) ->
    Reason = lists:flatten(io_lib:format("~p: ~s", [What, Why])),
    exit({skipped, Reason});
skip({What, Why}) ->
    Reason = lists:flatten(io_lib:format("~p: ~p", [What, Why])),
    exit({skipped, Reason});
skip(Reason) when list(Reason) ->
    exit({skipped, Reason});
skip(Reason1) ->
    Reason2 = lists:flatten(io_lib:format("~p", [Reason1])),
    exit({skipped, Reason2}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_flex_scanner() ->
    Pid = proc_lib:spawn(?MODULE, flex_scanner_handler, [self()]),
    receive
	{flex_scanner_started, Pid, Conf} ->
	    {Pid, Conf};
	{flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
 	    ?LOG("start_flex_scanner -> failed loading flex scanner driver: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}};
	{flex_scanner_error, Reason} ->
 	    ?LOG("start_flex_scanner -> error: "
 		 "~n   Reason: ~p~n", [Reason]),
	    {error, {failed_loading_flex_scanner_driver, Reason}}
    after 10000 ->
	    exit(Pid, kill),
	    {error, {failed_starting_flex_scanner, timeout}}
    end.


ping_flex_scanner(Pid) ->
    Pid ! {ping, self()},
    receive
	{pong, Pid} ->
	    ok
    after 5000 ->
	    timeout
    end.


stop_flex_scanner(Pid) ->
    Pid ! stop_flex_scanner.


flex_scanner_handler(Pid) ->
%     io:format("flex_scanner_handler -> entry whith"
% 	      "~n   Pid: ~p~n", [Pid]),
    case (catch megaco_flex_scanner:start()) of
	{ok, Port} when port(Port) ->
% 	    io:format("flex_scanner_handler -> ok:"
% 		      "~n   Port: ~p~n", [Port]),
%  	    ?LOG("flex_scanner_handler -> started: ~p~n", [Port]),
	    Pid ! {flex_scanner_started, self(), {flex, Port}},
	    flex_scanner_handler(Pid, Port);
	{error, {load_driver, {open_error, Reason}}} ->
% 	    io:format("flex_scanner_handler -> load_driver error:"
% 		      "~n   Reason: ~p~n", [Reason]),
%   	    ?LOG("flex_scanner_handler -> error: ~p~n", [Reason]),
	    Error = {failed_loading_flex_scanner_driver, Reason},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error);
	Else ->
% 	    io:format("flex_scanner_handler -> unknown response:"
% 		      "~n   % Else: ~p~n", [Else]),
%   	    ?LOG("flex_scanner_handler -> else: ~p~n", [Else]),
	    Error = {unknown_result_from_start_flex_scanner, Else},
	    Pid ! {flex_scanner_error, Error},
	    exit(Error)
    end.

flex_scanner_handler(Pid, Port) ->
%     io:format("flex_scanner_handler -> entry with"
% 	      "~n   Pid:  ~p"
% 	      "~n   Port: ~p~n", [Pid, Port]),
    receive
	{ping, Pinger} ->
% 	    io:format("flex_scanner_handler -> got ping from ~p~n", [Pinger]),
	    Pinger ! {pong, self()},
	    flex_scanner_handler(Pid, Port);
	{'EXIT', Port, Reason} ->
% 	    io:format("flex_scanner_handler -> exit when"
% 		      "~n   Port:   ~p"
% 		      "~n   Reason: ~p~n", [Port, Reason]),
	    Pid ! {flex_scanner_exit, Reason},
	    exit({flex_scanner_exit, Reason});
	stop_flex_scanner ->
% 	    io:format("flex_scanner_handler -> stop~n", []),
	    megaco_flex_scanner:stop(Port),
	    exit(normal);
	Other ->
	    io:format("flex scanner handler got something:~n"
		      "~p", [Other]),
	    flex_scanner_handler(Pid, Port)
    end.
	    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chk_MegacoMessage(M,M) when record(M,'MegacoMessage') ->
    {equal,'MegacoMessage'};
chk_MegacoMessage(#'MegacoMessage'{authHeader = Auth1,
				   mess       = Mess1},
		  #'MegacoMessage'{authHeader = Auth2,
				   mess       = Mess2}) ->
    chk_opt_AuthenticationHeader(Auth1,Auth2),
    chk_Message(Mess1,Mess2),
    {equal,'MegacoMessage'};
chk_MegacoMessage(M1,M2) ->
    throw({wrong_type,{'MegacoMessage',M1,M2}}).
    
chk_opt_AuthenticationHeader(A,A) ->
    {equal,auth};
chk_opt_AuthenticationHeader(A1,A2) ->
    throw({not_equal,{auth,A1,A2}}).

chk_Message(M,M) when record(M,'Message') ->
    {equal,'Message'};
chk_Message(#'Message'{version     = Version1,
		       mId         = MID1,
		       messageBody = Body1},
	    #'Message'{version     = Version2,
		       mId         = MID2,
		       messageBody = Body2}) ->
    chk_version(Version1,Version2),
    chk_MId(MID1,MID2),
    chk_messageBody(Body1,Body2),
    {equal,'Message'};
chk_Message(M1,M2) ->
    throw({wrong_type,{'Message',M1,M2}}).


chk_version(V,V) when integer(V) ->
    {equal,version};
chk_version(V1,V2) when integer(V1), integer(V2) ->
    throw({not_equal,{version,V1,V2}});
chk_version(V1,V2) ->
    throw({wrong_type,{integer,V1,V2}}).


chk_MId(M,M) ->
    {equal,mid};
chk_MId({Tag,M1},{Tag,M2}) ->
    Res = chk_MId(Tag,M1,M2),
    throw({error,{equal,Res}});
chk_MId(M1,M2) ->
    throw({not_equal,{mid,M1,M2}}).

chk_MId(ip4Address,M1,M2) -> chk_IP4Address(M1, M2);
chk_MId(ip6Address,M1,M2) -> chk_IP6Address(M1, M2);
chk_MId(domainName,M1,M2) -> chk_DomainName(M1, M2);
chk_MId(deviceName,M1,M2) -> chk_PathName(M1, M2);
chk_MId(mtpAddress,M1,M2) -> chk_mtpAddress(M1, M2);
chk_MId(Tag,M1,M2) ->
    throw({wrong_type,{invalid_tag,Tag,M1,M2}}).
			       
    
chk_IP4Address(M, M) ->
    {equal,ip4Address};
chk_IP4Address(M1, M2) ->
    throw({not_equal,{ip4Address,M1,M2}}).

chk_IP6Address(M, M) ->
    {equal,ip6Address};
chk_IP6Address(M1, M2) ->
    throw({not_equal,{ip6Address,M1,M2}}).

chk_DomainName(D, D) when record(D,'DomainName') ->
    {equal,'DomainName'};
chk_DomainName(#'DomainName'{name       = Name1,
			     portNumber = Port1},
	       #'DomainName'{name       = Name2,
			     portNumber = Port2}) ->
    chk_DomainName_name(Name1,Name2),
    chk_DomainName_opt_portNumber(Port1,Port2),
    throw({error,{equal,'DomainName'}});
chk_DomainName(D1,D2) ->
    throw({wrong_type,{'DomainName',D1,D2}}).

chk_DomainName_name(N,N) when list(N) ->
    {equal,name};
chk_DomainName_name(N1,N2) when list(N1), list(N2) ->
    throw({not_equal,{'DomainName',name,N1,N2}});
chk_DomainName_name(N1,N2) ->
    throw({wrong_type,{'DomainName',name,N1,N2}}).

chk_DomainName_opt_portNumber(asn1_NOVALUE, asn1_NOVALUE) ->
    {equal, portNumber};
chk_DomainName_opt_portNumber(P,P) when integer(P), P >= 0 ->
    {equal, portNumber};
chk_DomainName_opt_portNumber(P1,P2) when integer(P1), P1 >= 0,
					  integer(P2), P2 >= 0 ->
    throw({not_equal,{'DomainName',portNumber,P1,P2}});
chk_DomainName_opt_portNumber(P1,P2) ->
    throw({wrong_type,{'DomainName',portNumber,P1,P2}}).


chk_PathName(P, P) ->
    {equal,pathname};
chk_PathName(P1, P2) ->
    throw({not_equal,{pathname,P1,P2}}).

chk_mtpAddress(M, M) ->
    {equal, mtpAddress};
chk_mtpAddress(M1, M2) ->
    throw({not_equal,{mtpAddress, M1, M2}}).
    

chk_messageBody({messageError,B},{messageError,B}) when record(B,'ErrorDescriptor') ->
    
    {equal, messageBody};
chk_messageBody({messageError,B1},{messageError,B2}) ->
    chk_ErrorDescriptor(B1,B2),
    throw({error,{equal, messageBody, messageError}});
chk_messageBody({transactions,T},{transactions,T}) when list(T) ->
    {equal, messageBody};
chk_messageBody({transactions,T1},{transactions,T2}) ->
    chk_transactions(T1,T2),
    {equal, messageBody};
chk_messageBody(B1,B2) ->
    throw({wrong_type,{messageBody,B1,B2}}).
     

chk_transactions(T,T) when list(T) ->
    {equal,transactions};
chk_transactions(T1,T2) when list(T1), list(T2), length(T1) == length(T2) ->
    chk_transactions1(T1,T2);
chk_transactions(T1,T2) ->
    throw({wrong_type,{transactions,T1,T2}}).

chk_transactions1([],[]) ->
    throw({error,{equal,transactions}});
chk_transactions1([T|Ts1],[T|Ts2]) ->
    chk_transactions1(Ts1,Ts2);
chk_transactions1([T1|_Ts1],[T2|_Ts2]) ->
    chk_transaction(T1,T2),
    {equal,transaction}.

chk_transaction(T,T) ->
    {equal,transaction};
chk_transaction({transactionRequest,T1},{transactionRequest,T2}) ->
    chk_transactionRequest(T1,T2),
    {equal,transactionRequest};
chk_transaction({transactionPending,T1},{transactionPending,T2}) ->
    chk_transactionPending(T1,T2),
    throw({error,{equal,{transactionPending,T1,T2}}});
chk_transaction({transactionReply,T1},{transactionReply,T2}) ->
    chk_transactionReply(T1,T2),
    throw({error,{equal,{transactionReply,T1,T2}}});
chk_transaction({transactionResponseAck,T1},{transactionResponseAck,T2}) ->
    chk_transactionAck(T1,T2),
    throw({error,{equal,{transactionResponseAck,T1,T2}}});
chk_transaction({Tag1,T1},{Tag2,T2}) ->
    throw({wrong_type,{transaction_tag,Tag1,Tag2}}).


chk_transactionRequest(T,T) when record(T,'TransactionRequest') ->
    {equal,transactionAck};
chk_transactionRequest(T1,T2) when record(T1,'TransactionRequest'),
				   record(T2,'TransactionRequest') ->
    chk_transactionId(T1#'TransactionRequest'.transactionId,
		      T2#'TransactionRequest'.transactionId),
    chk_actionRequests(T1#'TransactionRequest'.actions,
		       T2#'TransactionRequest'.actions),
    {equal,transactionRequest};
chk_transactionRequest(T1,T2) ->
    throw({wrong_type,{transactionRequest,T1,T2}}).
    

chk_transactionPending(T,T) when record(T,'TransactionPending') ->
    {equal,transactionPending};
chk_transactionPending(#'TransactionPending'{transactionId = Id1} = T1,
		       #'TransactionPending'{transactionId = Id2} = T2) ->
    chk_transactionId(Id1,Id2),
    throw({error,{equal,transactionPending}});
chk_transactionPending(T1,T2) ->
    throw({wrong_type,{transactionPending,T1,T2}}).

chk_transactionReply(T,T) when record(T,'TransactionReply') ->
    {equal,transactionReply};
chk_transactionReply(#'TransactionReply'{transactionId     = Id1,
					 immAckRequired    = ImmAck1,
					 transactionResult = TransRes1} = T1,
		     #'TransactionReply'{transactionId     = Id2,
					 immAckRequired    = ImmAck2,
					 transactionResult = TransRes2} = T2) ->
    chk_transactionId(Id1,Id2),
    ImmAck1 = ImmAck2,
    chk_transactionReply_transactionResult(TransRes1,TransRes2),
    throw({error,{equal,transactionReply}});
chk_transactionReply(T1,T2) ->
    throw({wrong_type,{transactionReply,T1,T2}}).

chk_transactionReply_transactionResult(R,R) ->
    {equal,transactionReply_transactionResult};
chk_transactionReply_transactionResult(R1,R2) ->
    throw({not_equal,{transactionReply_transactionResult,R1,R2}}).

chk_transactionAck(T,T) when record(T,'TransactionAck') ->
    {equal,transactionAck};
chk_transactionAck(#'TransactionAck'{firstAck = F1,
				     lastAck  = L1} = T1,
		   #'TransactionAck'{firstAck = F2,
				     lastAck  = L2} = T2) ->
    chk_transactionId(F1,F2),
    chk_opt_transactionId(L1,L2),
    throw({error,{equal,'TransactionAck'}});
chk_transactionAck(T1,T2) ->
    throw({wrong_type,{transactionAck,T1,T2}}).

chk_actionRequests(A,A) when list(A), length(A) == 0 ->
    {equal,actionRequests};
chk_actionRequests(A,A) when list(A) ->
    case hd(A) of
	A when record(A,'ActionRequest') ->
	    {equal,actionRequests};
	Else ->
	    throw({wrong_type,{'ActionRequest',Else}})
    end;
chk_actionRequests(A1,A2) when list(A1), list(A2), 
			       length(A1) == length(A2) ->
    chk_actionRequests1(A1,A2);
chk_actionRequests(A1,A2) ->
    throw({wrong_type,{actionRequests,A1,A2}}).

chk_actionRequests1([],[]) ->
    throw({error,{equal,actionRequests}});
chk_actionRequests1([A|As1],[A|As2]) when record(A,'ActionRequest') ->
    chk_actionRequests1(As1,As2);
chk_actionRequests1([A1|_As1],[A2|_As2]) ->
    chk_actionRequest(A1,A2),
    {equal,actionRequest}.

chk_actionRequest(A,A) when record(A,'ActionRequest') ->
    {equal,actionRequest};
chk_actionRequest(#'ActionRequest'{contextId           = Id1,
				   contextRequest      = Req1,
				   contextAttrAuditReq = AuditReq1,
				   commandRequests     = CmdReqs1} = A1,
		  #'ActionRequest'{contextId           = Id2,
				   contextRequest      = Req2,
				   contextAttrAuditReq = AuditReq2,
				   commandRequests     = CmdReqs2} = A2) ->
    t("chk_actionRequest -> entry with"
      "~n   CmdReqs1: ~p"
      "~n   CmdReqs2: ~p",[CmdReqs1,CmdReqs2]),
    chk_contextId(Id1,Id2),
    chk_opt_contextRequest(Req1,Req2),
    chk_opt_contextAttrAuditReq(AuditReq1,AuditReq2),
    chk_commandRequests(CmdReqs1,CmdReqs2),
    {equal,'ActionRequest'}.
    
chk_contextId(Id,Id) when integer(Id) ->
    {equal,contextId};
chk_contextId(Id1,Id2) when integer(Id1), integer(Id2) ->
    throw({not_equal,{contextId,Id1,Id2}});
chk_contextId(Id1,Id2) ->
    throw({wrong_type,{contextId,Id1,Id2}}).

chk_opt_contextRequest(asn1_NOVALUE, asn1_NOVALUE) ->
    {equal,contextRequest};
chk_opt_contextRequest(R,R) when record(R,'ContextRequest') ->
    {equal,contextRequest};
chk_opt_contextRequest(#'ContextRequest'{priority    = Prio1,
					 emergency   = Em1,
					 topologyReq = TopReq1} = C1,
		       #'ContextRequest'{priority    = Prio2,
					 emergency   = Em2,
					 topologyReq = TopReq2} = C2) ->
    chk_contextRequest_priority(Prio1,Prio2),
    chk_contextRequest_emergency(Em1,Em2),
    chk_topologyRequest(TopReq1,TopReq2),
    throw({error,{equal,'ContextRequest',C1,C2}}).

chk_contextRequest_priority(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,contextRequest_priority};
chk_contextRequest_priority(P,P) when integer(P) ->
    {equal,contextRequest_priority};
chk_contextRequest_priority(P1,P2) when integer(P1), integer(P2) ->
    throw({not_equal,{contextRequest_priority,P1,P2}});    
chk_contextRequest_priority(P1,P2) ->
    throw({wrong_type,{contextRequest_priority,P1,P2}}).

chk_contextRequest_emergency(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,contextRequest_emergency};
chk_contextRequest_emergency(true,true) ->
    {equal,contextRequest_emergency};
chk_contextRequest_emergency(false,false) ->
    {equal,contextRequest_emergency};
chk_contextRequest_emergency(E1,E2) ->
    throw({not_equal,{contextRequest_emergency,E1,E2}}).

chk_topologyRequest(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,topologyRequest};
chk_topologyRequest(T,T) when record(T,'TopologyRequest') ->
    {equal,topologyRequest};
chk_topologyRequest(#'TopologyRequest'{terminationFrom   = F1,
				       terminationTo     = T1,
				       topologyDirection = D1} = T1,
		    #'TopologyRequest'{terminationFrom   = F2,
				       terminationTo     = T2,
				       topologyDirection = D2} = T2) ->
    chk_terminationId(F1,F2),
    chk_terminationId(T1,T2),
    chk_topologyRequest_topologyDirection(D1,D2),
    throw({error,{equal,'TopologyRequest',D1,D2}}).

chk_topologyRequest_topologyDirection(bothway,bothway) ->
    {equal,topologyRequest_topologyDirection};
chk_topologyRequest_topologyDirection(isolate,isolate) ->
    {equal,topologyRequest_topologyDirection};
chk_topologyRequest_topologyDirection(oneway,oneway) ->
    {equal,topologyRequest_topologyDirection};
chk_topologyRequest_topologyDirection(D1,D2) ->
    throw({not_equal,{topologyRequest_topologyDirection, D1, D2}}).

chk_opt_contextAttrAuditReq(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,contextAttrAuditReq};
chk_opt_contextAttrAuditReq(R,R) when record(R,'ContextAttrAuditRequest') ->
    {equal,contextAttrAuditReq};
chk_opt_contextAttrAuditReq(#'ContextAttrAuditRequest'{topology  = T1,
						       emergency = E1,
						       priority  = P1} = R1,
			    #'ContextAttrAuditRequest'{topology  = T2,
						       emergency = E2,
						       priority  = P2} = R2)  ->
    T1 = T2,
    E1 = E2,
    P1 = P2,
    throw({error,{equal,'ContextAttrAuditRequest',R1,R2}}).

chk_commandRequests(C1,C2) when list(C1), list(C2), length(C1) == length(C2) ->
    t("chk_commandRequests -> entry with"
      "~n   C1: ~p"
      "~n   C2: ~p", [C1, C2]),
    chk_commandRequests1(C1,C2);
chk_commandRequests(C1,C2) ->
    t("chk_commandRequests -> entry",[]),
    throw({wrong_type,{commandRequests,C1,C2}}).

chk_commandRequests1([],[]) ->
    {equal,commandRequests};
chk_commandRequests1([C1|Cs1],[C2|Cs2]) ->
    chk_commandRequest(C1,C2),
    chk_commandRequests1(Cs1,Cs2).

chk_commandRequest(C,C) when record(C,'CommandRequest') ->
    {equal,commandRequest};
chk_commandRequest(#'CommandRequest'{command        = Cmd1,
				     optional       = O1,
				     wildcardReturn = W1} = C1,
		   #'CommandRequest'{command        = Cmd2,
				     optional       = O2,
				     wildcardReturn = W2} = C2) ->
    t("chk_commandRequest -> entry with"
      "~n   C1: ~p"
      "~n   C2: ~p", [Cmd1, Cmd2]),
    chk_commandRequest_command(Cmd1,Cmd2),
    O1 = O2,
    W1 = W2,
    {equal,'CommandRequest'};
chk_commandRequest(C1,C2) ->
    throw({wrong_type,{commandRequest,C1,C2}}).

chk_commandRequest_command({addReq,C1},{addReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({moveReq,C1},{moveReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({modReq,C1},{modReq,C2}) ->
    chk_AmmRequest(C1,C2);
chk_commandRequest_command({subtractReq,C1},{subtractReq,C2}) ->
    chk_SubtractRequest(C1,C2);
chk_commandRequest_command({auditCapRequest,C1},{auditCapRequest,C2}) ->
    chk_AuditRequest(C1,C2);
chk_commandRequest_command({auditValueRequest,C1},{auditValueRequest,C2}) ->
    chk_AuditRequest(C1,C2);
chk_commandRequest_command({notifyReq,C1},{notifyReq,C2}) ->
    chk_NotifyRequest(C1,C2);
chk_commandRequest_command({serviceChangeReq,C1},{serviceChangeReq,C2}) ->
    chk_ServiceChangeRequest(C1,C2);
chk_commandRequest_command(C1,C2) ->
    throw({wrong_type,{commandRequest_command,C1,C2}}).
    

chk_AmmRequest(R,R) when record(R,'AmmRequest') ->
    {equal,'AmmRequest'};
chk_AmmRequest(#'AmmRequest'{terminationID = Tids1,
			     descriptors   = D1} = R1,
	       #'AmmRequest'{terminationID = Tids2,
			     descriptors   = D2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_AmmRequest_descriptors(D1,D2),
    {equal,'AmmRequest',R1,R2}; % DigitMap body can have trailing '\n', ...
chk_AmmRequest(R1,R2) ->
    throw({wrong_type,{'AmmRequest',R1,R2}}).

chk_AmmRequest_descriptors([],[]) ->
    {equal,ammRequest_descriptors};
chk_AmmRequest_descriptors(D1,D2) when list(D1), list(D2), 
				       length(D1) == length(D2) ->
    chk_AmmRequest_descriptors1(D1,D2);
chk_AmmRequest_descriptors(D1,D2) ->
    throw({wrong_type,{ammRequest_descriptors,D1,D2}}).

chk_AmmRequest_descriptors1([],[]) ->
    {equal,ammRequest_descriptors};
chk_AmmRequest_descriptors1([D1|Ds1],[D2|Ds2]) ->
    chk_AmmRequest_descriptor(D1,D2),
    chk_AmmRequest_descriptors1(Ds1,Ds2).
    
chk_AmmRequest_descriptor({mediaDescriptor,D1},{mediaDescriptor,D2}) -> 
    chk_MediaDescriptor(D1,D2);
chk_AmmRequest_descriptor({modemDescriptor,D1},{modemDescriptor,D2}) -> 
    chk_ModemDescriptor(D1,D2);
chk_AmmRequest_descriptor({muxDescriptor,D1},{muxDescriptor,D2}) -> 
    chk_MuxDescriptor(D1,D2);
chk_AmmRequest_descriptor({eventsDescriptor,D1},{eventsDescriptor,D2}) -> 
    chk_EventsDescriptor(D1,D2);
chk_AmmRequest_descriptor({eventBufferDescriptor,D1},{eventBufferDescriptor,D2}) -> 
    chk_EventBufferDescriptor(D1,D2);
chk_AmmRequest_descriptor({signalsDescriptor,D1},{signalsDescriptor,D2}) -> 
    chk_SignalsDescriptor(D1,D2);
chk_AmmRequest_descriptor({digitMapDescriptor,D1},{digitMapDescriptor,D2}) -> 
    chk_DigitMapDescriptor(D1,D2);
chk_AmmRequest_descriptor({auditDescriptor,D1},{auditDescriptor,D2}) -> 
    chk_AuditDescriptor(D1,D2);
chk_AmmRequest_descriptor({Tag1,D1},{Tag2,D2}) -> 
    throw({wrong_type,{ammRequest_descriptor_tag,Tag1,Tag2}}).
    
    
chk_SubtractRequest(R,R) when record(R,'SubtractRequest') ->
    {equal,'SubtractRequest'};
chk_SubtractRequest(#'SubtractRequest'{terminationID   = Tids1,
				       auditDescriptor = D1} = R1,
		    #'SubtractRequest'{terminationID   = Tids2,
				       auditDescriptor = D2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_opt_AuditDescriptor(D1,D2),
    throw({error,{equal,{'SubtractRequest',R1,R2}}});
chk_SubtractRequest(R1,R2) ->
    throw({wrong_type,{'SubtractRequest',R1,R2}}).


chk_AuditRequest(R,R) when record(R,'AuditRequest') ->
    {equal,'AuditRequest'};
chk_AuditRequest(#'AuditRequest'{terminationID   = Tid1,
				 auditDescriptor = D1} = R1,
		 #'AuditRequest'{terminationID   = Tid2,
				 auditDescriptor = D2} = R2) ->
    chk_terminationId(Tid1,Tid2),
    chk_AuditDescriptor(D1,D2),
    throw({error,{equal,{'AuditRequest',R1,R2}}});
chk_AuditRequest(R1,R2) ->
    throw({wrong_type,{'AuditRequest',R1,R2}}).


chk_NotifyRequest(R,R) when record(R,'NotifyRequest') ->
    {equal,'NotifyRequest'};
chk_NotifyRequest(#'NotifyRequest'{terminationID            = Tids1,
				   observedEventsDescriptor = ObsDesc1,
				   errorDescriptor          = ErrDesc1} = R1,
		  #'NotifyRequest'{terminationID            = Tids2,
				   observedEventsDescriptor = ObsDesc2,
				   errorDescriptor          = ErrDesc2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_ObservedEventsDescriptor(ObsDesc1,ObsDesc2),
    chk_opt_ErrorDescriptor(ErrDesc1,ErrDesc2),
    throw({error,{equal,{'NotifyRequest',R1,R2}}});
chk_NotifyRequest(R1,R2) ->
    throw({wrong_type,{'NotifyRequest',R1,R2}}).


chk_ServiceChangeRequest(R,R) when record(R,'ServiceChangeRequest') ->
    {equal,'ServiceChangeRequest'};
chk_ServiceChangeRequest(#'ServiceChangeRequest'{terminationID      = Tids1,
						 serviceChangeParms = P1} = R1,
			 #'ServiceChangeRequest'{terminationID      = Tids2,
						 serviceChangeParms = P2} = R2) ->
    chk_terminationIds(Tids1,Tids2),
    chk_ServiceChangeParm(P1,P2),
    throw({error,{equal,{'ServiceChangeRequest',R1,R2}}});
chk_ServiceChangeRequest(R1,R2) ->
    throw({wrong_type,{'ServiceChangeRequest',R1,R2}}).


chk_MediaDescriptor(D,D) when record(D,'MediaDescriptor') ->
    {equal,'MediaDescriptor'};
chk_MediaDescriptor(#'MediaDescriptor'{termStateDescr = Tsd1,
				       streams        = S1} = D1,
		    #'MediaDescriptor'{termStateDescr = Tsd2,
				       streams        = S2} = D2) ->
    Tsd1 = Tsd2,
    S1   = S2,
    throw({error,{equal,{'MediaDescriptor',D1,D2}}});
chk_MediaDescriptor(D1,D2) ->
    throw({wrong_type,{'MediaDescriptor',D1,D2}}).

chk_ModemDescriptor(D,D) when record(D,'ModemDescriptor') ->
    {equal,'ModemDescriptor'};
chk_ModemDescriptor(#'ModemDescriptor'{mtl = T1,
				       mpl = P1} = D1,
		    #'ModemDescriptor'{mtl = T2,
				       mpl = P2} = D2) ->
    T1 = T2,
    P1 = P2,
    throw({error,{equal,{'ModemDescriptor',D1,D2}}});
chk_ModemDescriptor(D1,D2) ->
    throw({wrong_type,{'ModemDescriptor',D1,D2}}).

chk_MuxDescriptor(D,D) when record(D,'MuxDescriptor') ->
    {equal,'MuxDescriptor'};
chk_MuxDescriptor(#'MuxDescriptor'{muxType  = T1,
				   termList = I1} = D1,
		  #'MuxDescriptor'{muxType  = T2,
				   termList = I2} = D2) ->
    T1 = T2,
    I1 = I2,
    throw({error,{equal,{'MuxDescriptor',D1,D2}}});
chk_MuxDescriptor(D1,D2) ->
    throw({wrong_type,{'MuxDescriptor',D1,D2}}).

chk_EventsDescriptor(D,D) when record(D,'EventsDescriptor') ->
    {equal,'EventsDescriptor'};
chk_EventsDescriptor(#'EventsDescriptor'{requestID = I1,
					 eventList = E1} = D1,
		     #'EventsDescriptor'{requestID = I2,
					 eventList = E2} = D2) ->
    I1 = I2,
    E1 = E2,
    throw({error,{equal,{'EventsDescriptor',D1,D2}}});
chk_EventsDescriptor(D1,D2) ->
    throw({wrong_type,{'EventsDescriptor',D1,D2}}).

chk_EventBufferDescriptor(D1,D2) when list(D1), list(D2), 
				      length(D1) == length(D2) ->
    chk_EventBufferDescriptor1(D1,D2);
chk_EventBufferDescriptor(D1,D2) ->
    throw({wrong_type,{eventBufferDescriptor,D1,D2}}).

chk_EventBufferDescriptor1([],[]) ->
    {equal,eventBufferDescriptor};
chk_EventBufferDescriptor1([ES1|D1],[ES2|D2]) ->
    chk_EventSpec(ES1,ES2),
    chk_EventBufferDescriptor1(D1,D2).

chk_EventSpec(ES,ES) when record(ES,'EventSpec') ->
    {equal,'EventSpec'};
chk_EventSpec(#'EventSpec'{eventName    = N1,
			   streamID     = I1,
			   eventParList = P1} = ES1,
	      #'EventSpec'{eventName    = N2,
			   streamID     = I2,
			   eventParList = P2} = ES2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_EventParameters(P1,P2),
    throw({error,{equal,{'EventSpec',ES1,ES2}}});
chk_EventSpec(ES1,ES2) ->
    throw({wrong_type,{'EventSpec',ES1,ES2}}).


chk_opt_ErrorDescriptor(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,'ErrorDescriptor'};
chk_opt_ErrorDescriptor(E1,E2) ->
    chk_ErrorDescriptor(E1,E2).

chk_ErrorDescriptor(E,E) when record(E,'ErrorDescriptor') ->
    {equal,'ErrorDescriptor'};
chk_ErrorDescriptor(#'ErrorDescriptor'{errorCode = Code1,
				       errorText = Text1} = E1,
		    #'ErrorDescriptor'{errorCode = Code2,
				       errorText = Text2} = E2) ->
    chk_ErrorCode(Code1,Code2),
    chk_opt_ErrorText(Text1,Text2),
    throw({error,{equal,{'ErrorDescriptor',E1,E2}}});
chk_ErrorDescriptor(E1,E2) ->
    throw({wrong_type,{'ErrorDescriptor',E1,E2}}).

chk_ErrorCode(C,C) when integer(C) ->
    {equal,errorCode};
chk_ErrorCode(C1,C2) when integer(C1), integer(C2) ->
    throw({not_equal,{errorCode,C1,C2}});
chk_ErrorCode(C1,C2) ->
    throw({wrong_type,{errorCode,C1,C2}}).

chk_opt_ErrorText(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,errorText};
chk_opt_ErrorText(T,T) when list(T) ->
    {equal,errorText};
chk_opt_ErrorText(T1,T2) when list(T1), list(T2) ->
    throw({not_equal,{errorText,T1,T2}});
chk_opt_ErrorText(T1,T2) ->
    throw({wrong_type,{errorText,T1,T2}}).


chk_SignalsDescriptor(D1,D2) when list(D1), list(D2), 
				  length(D1) == length(D2) ->
    chk_SignalsDescriptor1(D1,D2);
chk_SignalsDescriptor(D1,D2) ->
    throw({wrong_type,{signalsDescriptor,D1,D2}}).

chk_SignalsDescriptor1([],[]) ->
    {equal,signalsDescriptor};
chk_SignalsDescriptor1([S1|D1],[S2|D2]) ->
    chk_SignalRequest(S1,S2),
    chk_SignalsDescriptor1(D1,D2).

chk_SignalRequest({signal,S1},{signal,S2}) ->
    chk_Signal(S1,S2);
chk_SignalRequest({seqSigList,S1},{seqSigList,S2}) ->
    chk_SeqSignalList(S1,S2);
chk_SignalRequest(R1,R2) ->
    throw({wrong_type,{signalRequest,R1,R2}}).

chk_SeqSignalList(S,S) when record(S,'SeqSigList') ->
    {equal,'SeqSigList'};
chk_SeqSignalList(#'SeqSigList'{id         = Id1,
				signalList = SigList1} = S1,
		  #'SeqSigList'{id         = Id2,
				signalList = SigList2} = S2) ->
    Id1 = Id2,
    chk_Signals(SigList1,SigList2),
    throw({error,{equal,{'SeqSigList',S1,S2}}});
chk_SeqSignalList(S1,S2) ->
    throw({wrong_type,{'SeqSigList',S1,S2}}).


chk_Signals([],[]) ->
    {equal,signals};
chk_Signals([Sig1|Sigs1],[Sig2|Sigs2]) ->
    chk_Signal(Sig1,Sig2),
    chk_Signals(Sigs1,Sigs2).


chk_Signal(S,S) when record(S,'Signal') ->
    {equal,'Signal'};
chk_Signal(#'Signal'{signalName       = N1,
		     streamID         = I1,
		     sigType          = T1,
		     duration         = D1,
		     notifyCompletion = C1,
		     keepActive       = K1,
		     sigParList       = P1} = S1,
	   #'Signal'{signalName       = N2,
		     streamID         = I2,
		     sigType          = T2,
		     duration         = D2,
		     notifyCompletion = C2,
		     keepActive       = K2,
		     sigParList       = P2} = S2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_opt_SignalType(T1,T2),
    chk_opt_duration(D1,D2),
    chk_opt_NotifyCompletion(C1,C2),
    chk_opt_keepAlive(K1,K2),
    chk_sigParameters(P1,P2),
    throw({error,{equal,{'Signal',S1,S2}}});
chk_Signal(S1,S2) ->
    throw({wrong_type,{'Signal',S1,S2}}).

chk_DigitMapDescriptor(D,D) when record(D,'DigitMapDescriptor') ->
    {equal,'DigitMapDescriptor'};
chk_DigitMapDescriptor(#'DigitMapDescriptor'{digitMapName  = N1,
					     digitMapValue = V1} = D1,
		       #'DigitMapDescriptor'{digitMapName  = N2,
					     digitMapValue = V2} = D2) ->
    chk_opt_digitMapName(N1,N2),
    chk_opt_digitMapValue(V1,V2),
    {equal,'DigitMapDescriptor'};
chk_DigitMapDescriptor(D1,D2) ->
    throw({wrong_type,{'DigitMapDescriptor',D1,D2}}).

chk_opt_digitMapName(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,digitMapName};
chk_opt_digitMapName(N1,N2) ->
    chk_digitMapName(N1,N2).

chk_digitMapName(N,N) ->
    {equal,digitMapName};
chk_digitMapName(N1,N2) ->
    throw({not_equal,{digitMapName,N1,N2}}).

chk_opt_digitMapValue(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,digitMapValue};
chk_opt_digitMapValue(V1,V2) ->
    chk_digitMapValue(V1,V2).

chk_digitMapValue(V,V) when record(V,'DigitMapValue') ->
    {equal,digitMapValue};
chk_digitMapValue(#'DigitMapValue'{digitMapBody = Body1,
				   startTimer   = Start1,
				   shortTimer   = Short1,
				   longTimer    = Long1} = V1,
		  #'DigitMapValue'{digitMapBody = Body2,
				   startTimer   = Start2,
				   shortTimer   = Short2,
				   longTimer    = Long2} = V2) ->
    chk_digitMapValue_digitMapBody(Body1,Body2), % Could contain trailing '\n', ...
    chk_opt_timer(Start1,Start2),
    chk_opt_timer(Short1,Short2),
    chk_opt_timer(Long1,Long2),
    {equal,'DigitMapValue'};
chk_digitMapValue(V1,V2) ->
    throw({wrong_type,{digitMapValue,V1,V2}}).

chk_digitMapValue_digitMapBody(B,B) when list(B) ->
    {equal,digitMapValue_digitMapBody};
chk_digitMapValue_digitMapBody(B1,B2) when list(B1), list(B2), length(B1) > length(B2)  ->
    case string:str(B2, B1) of
	0 ->
	    {equal,digitMapValue_digitMapBody};
	_ ->
	    throw({not_equal,{digitMapValue_digitMapBody,B1,B2}})
    end;
chk_digitMapValue_digitMapBody(B1,B2) when list(B1), list(B2), length(B1) < length(B2)  ->
    case string:str(B1, B2) of
	0 ->
	    {equal,digitMapValue_digitMapBody};
	_ ->
	    throw({not_equal,{digitMapValue_digitMapBody,B1,B2}})
    end;
chk_digitMapValue_digitMapBody(B1,B2) when list(B1), list(B2) ->
    throw({not_equal,{digitMapValue_digitMapBody,B1,B2}});
chk_digitMapValue_digitMapBody(B1,B2) ->
    throw({wrong_type,{digitMapValue_digitMapBody,B1,B2}}).


chk_opt_AuditDescriptor(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,'AuditDescriptor'};
chk_opt_AuditDescriptor(D1,D2) ->
    chk_AuditDescriptor(D1,D2).

chk_AuditDescriptor(D,D) when record(D,'AuditDescriptor') ->
    {equal,'AuditDescriptor'};
chk_AuditDescriptor(#'AuditDescriptor'{auditToken = T1} = D1,
		    #'AuditDescriptor'{auditToken = T2} = D2) ->
    chk_opt_auditToken(T1,T2),
    throw({error,{equal,{'AuditDescriptor',D1,D2}}});
chk_AuditDescriptor(D1,D2) ->
    throw({wrong_type,{'AuditDescriptor',D1,D2}}).

chk_opt_auditToken(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,auditToken};
chk_opt_auditToken(T1,T2) ->
    chk_auditToken(T1,T2).

chk_auditToken(T1,T2) when list(T1), list(T2), length(T1) == length(T2) ->
    chk_auditToken1(T1,T2);
chk_auditToken(T1,T2) ->
    throw({wrong_type,{auditToken,T1,T2}}).

chk_auditToken1([],[]) ->
    {equal,auditToken};
chk_auditToken1([H1|T1],[H2|T2]) ->
    chk_auditToken2(H1,H2),
    chk_auditToken1(T1,T2).

chk_auditToken2(muxToken,muxToken) ->
    {equal,auditToken};
chk_auditToken2(modemToken,modemToken) ->
    {equal,auditToken};
chk_auditToken2(mediaToken,mediaToken) ->
    {equal,auditToken};
chk_auditToken2(eventsToken,eventsToken) ->
    {equal,auditToken};
chk_auditToken2(signalsToken,signalsToken) ->
    {equal,auditToken};
chk_auditToken2(digitMapToken,digitMapToken) ->
    {equal,auditToken};
chk_auditToken2(statsToken,statsToken) ->
    {equal,auditToken};
chk_auditToken2(observedEventsToken,observedEventsToken) ->
    {equal,auditToken};
chk_auditToken2(packagesToken,packagesToken) ->
    {equal,auditToken};
chk_auditToken2(eventBufferToken,eventBufferToken) ->
    {equal,auditToken};
chk_auditToken2(T1,T2) when atom(T1), atom(T2) ->
    throw({not_equal,{auditToken,T1,T2}});
chk_auditToken2(T1,T2) ->
    throw({wrong_type,{auditToken,T1,T2}}).

chk_ObservedEventsDescriptor(D,D) when record(D,'ObservedEventsDescriptor') ->
    {equal,'ObservedEventsDescriptor'};
chk_ObservedEventsDescriptor(#'ObservedEventsDescriptor'{requestId        = Id1,
							 observedEventLst = E1} = D1,
			     #'ObservedEventsDescriptor'{requestId        = Id2,
							 observedEventLst = E2} = D2) ->
    Id1 = Id2,
    chk_ObservedEvents(E1,E2),
    throw({error,{equal,{'ObservedEventsDescriptor',D1,D2}}});
chk_ObservedEventsDescriptor(D1,D2) ->
    throw({wrong_type,{'ObservedEventsDescriptor',D1,D2}}).
    

chk_ObservedEvents(E1,E2) when list(E1), list(E2), length(E1) == length(E2) ->
    chk_ObservedEvents1(E1,E2);
chk_ObservedEvents(E1,E2) ->
    throw({wrong_type,{observedEvents,E1,E2}}).


chk_ObservedEvents1([],[]) ->
    {equal,observedEvents};
chk_ObservedEvents1([Ev1|Evs1],[Ev2|Evs2]) ->
    chk_ObservedEvent(Ev1,Ev2),
    chk_ObservedEvents1(Evs1,Evs2).

chk_ObservedEvent(#'ObservedEvent'{eventName    = N1,
				   streamID     = I1,
				   eventParList = P1,
				   timeNotation = T1} = E1,
		  #'ObservedEvent'{eventName    = N2,
				   streamID     = I2,
				   eventParList = P2,
				   timeNotation = T2} = E2) ->
    N1 = N2,
    chk_opt_StreamId(I1,I2),
    chk_EventParameters(P1,P2),
    chk_opt_TimeNotation(T1,T2),
    throw({error,{equal,{'ObservedEvent',E1,E2}}});
chk_ObservedEvent(E1,E2) ->
    throw({wrong_type,{'ObservedEvent',E1,E2}}).
    

chk_opt_TimeNotation(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,'TimeNotation'};
chk_opt_TimeNotation(T1,T2) ->
    chk_TimeNotation(T1,T2).

chk_TimeNotation(T,T) when record(T,'TimeNotation') ->
    {equal,'TimeNotation'};
chk_TimeNotation(#'TimeNotation'{date = Date1,
				 time = Time1} = T1,
		 #'TimeNotation'{date = Date2,
				 time = Time2} = T2) ->
    Date1 = Date2,
    Time1 = Time2,
    throw({error,{equal,{'TimeNotation',T1,T2}}});
chk_TimeNotation(T1,T2) ->
    throw({wrong_type,{'TimeNotation',T1,T2}}).
    
    
chk_opt_timer(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,timer};
chk_opt_timer(T1,T2) ->
    chk_timer(T1,T2).

chk_timer(T,T) when integer(T) ->
    {equal,timer};
chk_timer(T1,T2) when integer(T1), integer(T2) ->
    throw({not_equal,{timer,T1,T2}});
chk_timer(T1,T2) ->
    throw({wrong_type,{timer,T1,T2}}).
    

chk_opt_SignalType(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,signalType};
chk_opt_SignalType(T1,T2) ->
    chk_SignalType(T1,T2).

chk_SignalType(brief,brief) ->
    {equal,signalType};
chk_SignalType(onOffonOff,onOffonOff) ->
    {equal,signalType};
chk_SignalType(timeOut,timeOut) ->
    {equal,signalType};
chk_SignalType(T1,T2) ->
    throw({wrong_type,{signalType,T1,T2}}).


chk_opt_duration(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,duration};
chk_opt_duration(D1,D2) ->
    chk_duration(D1,D2).

chk_duration(D,D) when integer(D) ->
    {equal,duration};
chk_duration(D1,D2) when integer(D1), integer(D2) ->
    throw({not_equal,{duration,D1,D2}});
chk_duration(D1,D2) ->
    throw({wrong_type,{duration,D1,D2}}).


chk_opt_NotifyCompletion(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,notifyCompletion};
chk_opt_NotifyCompletion(N1,N2) ->
    chk_NotifyCompletion(N1,N2).

chk_NotifyCompletion([],[]) ->
    {equal,notifyCompletion};
chk_NotifyCompletion([Item1|Items1],[Item2|Items2]) ->
    chk_NotifyCompletion1(Item1,Item2),
    chk_NotifyCompletion(Items1,Items2);
chk_NotifyCompletion(C1,C2) ->
    throw({wrong_type,{notifyCompletion,C1,C2}}).

chk_NotifyCompletion1(onTimeOut,onTimeOut) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(onInterruptByEvent,onInterruptByEvent) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(onInterruptByNewSignalDescr,onInterruptByNewSignalDescr) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(otherReason,otherReason) ->
    {equal,notifyCompletion_part};
chk_NotifyCompletion1(C1,C2) ->
    throw({wrong_type,{notifyCompletion_part,C1,C2}}).


chk_opt_keepAlive(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,keepAlive};
chk_opt_keepAlive(K1,K2) ->
    chk_keepAlive(K1,K2).

chk_keepAlive(true,true) ->
    {equal,keepAlive};
chk_keepAlive(false,false) ->
    {equal,keepAlive};
chk_keepAlive(K1,K2) ->
    throw({wrong_type,{keepAlive,K1,K2}}).


chk_ServiceChangeParm(P,P) when  record(P,'ServiceChangeParm') ->
    {equal,'ServiceChangeParm'};
chk_ServiceChangeParm(#'ServiceChangeParm'{serviceChangeMethod  = M1, 
					   serviceChangeAddress = A1, 
					   serviceChangeVersion = V1, 
					   serviceChangeProfile = P1, 
					   serviceChangeReason  = R1, 
					   serviceChangeDelay   = D1, 
					   serviceChangeMgcId   = Mid1, 
					   timeStamp            = T1} = P1,
		      #'ServiceChangeParm'{serviceChangeMethod  = M2, 
					   serviceChangeAddress = A2, 
					   serviceChangeVersion = V2, 
					   serviceChangeProfile = P2, 
					   serviceChangeReason  = R2, 
					   serviceChangeDelay   = D2, 
					   serviceChangeMgcId   = Mid2, 
					   timeStamp            = T2} = P2) ->
    M1 = M2,
    A1 = A2,
    V1 = V2,
    P1 = P2,
    R1 = R2,
    D1 = D2,
    Mid1 = Mid2,
    T2 = T2,
    throw({error,{equal,{'ServiceChangeParm',P1,P2}}});
chk_ServiceChangeParm(P1,P2) ->
    throw({wrong_type,{'ServiceChangeParm',P1,P2}}).

     
chk_sigParameters(S1,S2) when list(S1), list(S2), length(S1) == length(S2) ->
    chk_sigParameters1(S1,S2);
chk_sigParameters(S1,S2) ->
    throw({wrong_type,{sigParameters,S1,S2}}).

chk_sigParameters1([],[]) ->
    {equal,sigParameters};
chk_sigParameters1([H1|T1],[H2|T2]) ->
    chk_sigParameter(H1,H2),
    chk_sigParameters1(T1,T2);
chk_sigParameters1(P1,P2) ->
    throw({wrong_type,{sigParameters,P1,P2}}).
    
chk_sigParameter(#'SigParameter'{sigParameterName = N1,
				 value            = V1,
				 extraInfo        = E1} = P1,
		 #'SigParameter'{sigParameterName = N2,
				 value            = V2,
				 extraInfo        = E2} = P2) ->
    N1 = N2,
    chk_Value(V1,V2),
    chk_opt_extraInfo(E1,E2),
    throw({error,{equal,{extraInfo,E1,E2}}});
chk_sigParameter(P1,P2) ->
    throw({wrong_type,{'SigParameter',P1,P2}}).

    
chk_opt_StreamId(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,streamId};
chk_opt_StreamId(I1,I2) ->
    chk_StreamId(I1,I2).

chk_StreamId(I,I) when integer(I) ->
    {equal,streamId};
chk_StreamId(I1,I2) when integer(I1), integer(I2) ->
    throw({not_equal,{streamId,I1,I2}});
chk_StreamId(I1,I2) ->
    throw({wrong_type,{streamId,I1,I2}}).
    

chk_EventParameters(EP1,EP2) when list(EP1), list(EP2), 
				  length(EP1) == length(EP2) ->
    chk_EventParameters1(EP1,EP2);
chk_EventParameters(EP1,EP2) ->
    throw({wrong_type,{eventParameters,EP1,EP2}}).

chk_EventParameters1([],[]) ->
    {equal,eventParameters};
chk_EventParameters1([EP1|EPS1],[EP2|EPS2]) ->
    chk_EventParameter(EP1,EP2),
    chk_EventParameters1(EPS1,EPS2).
    
chk_EventParameter(EP,EP) when record(EP,'EventParameter') ->
    {equal,'EventParameter'};
chk_EventParameter(#'EventParameter'{eventParameterName = N1,
				     value              = V1,
				     extraInfo          = E1} = EP1,
		   #'EventParameter'{eventParameterName = N2,
				     value              = V2,
				     extraInfo          = E2} = EP2) ->
    N1 = N2,
    chk_Value(V1,V2),
    chk_opt_extraInfo(E1,E2),
    throw({error,{equal,{'EventParameter',EP1,EP2}}});
chk_EventParameter(EP1,EP2) ->
    throw({wrong_type,{'EventParameter',EP1,EP2}}).


chk_Value(V,V) when list(V) ->
    chk_Value(V);
chk_Value(V1,V2) when list(V1), list(V2), length(V1) == length(V2) ->
    chk_Value1(V1,V2);
chk_Value(V1,V2) ->
    throw({wrong_type,{value,V1,V2}}).

chk_Value([]) ->
    ok;
chk_Value([H|T]) when list(H) ->
    chk_Value(T);
chk_Value([H|T]) ->
    throw({wrong_type,{value_part,H}}).

chk_Value1([],[]) ->
    {equal,value};
chk_Value1([H|T1],[H|T2]) when list(H) ->
    chk_Value1(T1,T2);
chk_Value1([H|T1],[H|T2]) ->
    throw({wrong_type,{value_part,H}});
chk_Value1([H1|T1],[H2|T2]) when list(H1), list(H2) ->
    throw({not_equal,{value_part,H1,H2}});
chk_Value1(V1,V2) ->
    throw({wrong_type,{value,V1,V2}}).


chk_opt_extraInfo(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,extraInfo};
chk_opt_extraInfo(E1,E2) ->
    chk_extraInfo(E1,E2).

chk_extraInfo({relation,greaterThan},{relation,greaterThan}) ->
    {equal,extraInfo};
chk_extraInfo({relation,smallerThan},{relation,smallerThan}) ->
    {equal,extraInfo};
chk_extraInfo({relation,unequalTo},{relation,unequalTo}) ->
    {equal,extraInfo};
chk_extraInfo({range,true},{range,true}) ->
    {equal,extraInfo};
chk_extraInfo({range,false},{range,false}) ->
    {equal,extraInfo};
chk_extraInfo({sublist,true},{sublist,true}) ->
    {equal,extraInfo};
chk_extraInfo({sublist,false},{sublist,false}) ->
    {equal,extraInfo};
chk_extraInfo(E1,E2) ->
    throw({wrong_type,{extraInfo,E1,E2}}).


chk_opt_transactionId(asn1_NOVALUE,asn1_NOVALUE) ->
    {equal,transactionId};
chk_opt_transactionId(Id1,Id2) ->
    chk_transactionId(Id1,Id2).

chk_transactionId(Id,Id) when integer(Id) ->
    {equal,transactionId};
chk_transactionId(Id1,Id2) when integer(Id1), integer(Id2) ->
    throw({not_equal,{transactionId,Id1,Id2}});
chk_transactionId(Id1,Id2) ->
    throw({wrong_type,{transactionId,Id1,Id2}}).
    

chk_terminationIds(Tids1,Tids2) when list(Tids1), list(Tids2), 
				     length(Tids1) == length(Tids2) ->
    chk_terminationIds1(Tids1,Tids2);
chk_terminationIds(Tids1,Tids2) ->
    throw({wrong_type,{terminationIds,Tids1,Tids2}}).

chk_terminationIds1([],[]) ->
    {equal,terminationIds};
chk_terminationIds1([Tid1|Tids1],[Tid2|Tids2]) ->
    chk_terminationId(Tid1,Tid2),
    chk_terminationIds1(Tids1,Tids2).

chk_terminationId(Id,Id) when record(Id,'TerminationID') ->
    {equal,terminationId};
chk_terminationId(Id,Id) when record(Id,megaco_term_id) ->
    {equal,terminationId};
chk_terminationId(#'TerminationID'{wildcard = W1,
				   id       = I1} = Tid1,
		  #'TerminationID'{wildcard = W2,
				   id       = I2} = Tid2) ->
    chk_terminationId_wildcard(W1,W2),
    chk_terminationId_id(I1,I2),
    throw({error,{equal,{'TerminationID',Tid1,Tid2}}});
chk_terminationId(#megaco_term_id{contains_wildcards = W1,
				  id                 = I1} = Tid1,
		  #megaco_term_id{contains_wildcards = W2,
				  id                 = I2} = Tid2) ->
    chk_terminationId_wildcard(W1,W2),
    chk_terminationId_id(I1,I2),
    throw({error,{equal,{megaco_term_id,Tid1,Tid2}}});
chk_terminationId(Tid1,Tid2) ->
    throw({wrong_type,{terminationId,Tid1,Tid2}}).

chk_terminationId_wildcard(W,W) ->
    {equal,terminationId_wildcard};
chk_terminationId_wildcard(W1,W2) ->
    throw({not_equal,{terminationId_wildcard,W1,W2}}).

chk_terminationId_id(I,I) ->
    {equal,terminationId_id};
chk_terminationId_id(I1,I2) ->
    throw({not_equal,{terminationId_id,I1,I2}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Parameter related
cre_propertyParm(Name, Val) ->
    #'PropertyParm'{name  = Name, value = [Val]}.


%% Statistics related
cre_statisticsParm(Name, Val) ->
    #'StatisticsParameter'{statName  = Name, statValue = [Val]}.


% Event related 
cre_eventParm(Name, Val) ->
    #'EventParameter'{eventParameterName = Name, value = Val}.

cre_observedEvent(Name, Not) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not}.
cre_observedEvent(Name, Not, Par) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not, eventParList = Par}.

cre_requestedEvent(Name) ->
    #'RequestedEvent'{pkgdName = Name}.
cre_requestedEvent(Name, ParList) when list(ParList) ->
    #'RequestedEvent'{pkgdName = Name, evParList = ParList};
cre_requestedEvent(Name, Action) when tuple(Action) ->
    #'RequestedEvent'{pkgdName = Name, eventAction = Action}.


cre_observedEventsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId = Id, observedEventLst = EvList}.

cre_eventsDesc(Id, EvList) ->
    #'EventsDescriptor'{requestID = Id, eventList = EvList}.


%% Service change related
cre_serviceChangeParm(M,A,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, serviceChangeAddress = A,
			 serviceChangeReason  = R, serviceChangeProfile = P}.

cre_serviceChangeResParm(A,P) ->
    #'ServiceChangeResParm'{serviceChangeAddress = A, 
			    serviceChangeProfile = P}.

cre_serviceChangeReq(Tid, P) ->
    #'ServiceChangeRequest'{terminationID = Tid, serviceChangeParms = P}.

cre_serviceChangeProf(Name, Ver) ->
    #'ServiceChangeProfile'{profileName = Name, version = Ver}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID = Tid, serviceChangeResult = Res}.


%% Stream related
cre_streamParms(Lcd) ->
    #'StreamParms'{localControlDescriptor = Lcd}.
cre_streamParms(Lcd, Ld) ->
    #'StreamParms'{localControlDescriptor = Lcd, localDescriptor = Ld}.
cre_streamParms(Lcd, Ld, Rd) ->
    #'StreamParms'{localControlDescriptor = Lcd, 
		   localDescriptor        = Ld,
		   remoteDescriptor       = Rd}.
cre_streamParmsL(Ld) ->
    #'StreamParms'{localDescriptor = Ld}.
cre_streamParmsR(Rd) ->
    #'StreamParms'{remoteDescriptor = Rd}.

cre_streamDesc(Id, P) ->
    #'StreamDescriptor'{streamID = Id, streamParms = P}.


%% "Local" related
cre_localControlDesc(Mode) ->
    #'LocalControlDescriptor'{streamMode = Mode}.
cre_localControlDesc(Mode, Parms) ->
    #'LocalControlDescriptor'{streamMode = Mode, propertyParms = Parms }.

cre_localRemoteDesc(Grps) ->
    #'LocalRemoteDescriptor'{propGrps = Grps}.


%% DigitMap related
cre_digitMapDesc(Name, Val) ->
    #'DigitMapDescriptor'{digitMapName = Name, digitMapValue = Val}.

cre_digitMapValue(Body) ->
    #'DigitMapValue'{digitMapBody = Body}.

cre_digitMapValue(Body, Start, Short, Long) ->
    #'DigitMapValue'{startTimer   = Start,
		     shortTimer   = Short,
		     longTimer    = Long,
		     digitMapBody = Body}.

%% Media related
cre_mediaDesc(StreamDesc) ->
    #'MediaDescriptor'{streams = {multiStream, [StreamDesc]}}.


%% Notify related
cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID = Tid, observedEventsDescriptor = EvsDesc}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.


%% Subtract related
cre_subtractReq(Tid, Desc) ->
    #'SubtractRequest'{terminationID = Tid, auditDescriptor = Desc}.


%% Audit related
cre_auditDesc(Tokens) ->
    #'AuditDescriptor'{auditToken = Tokens}.

cre_auditReq(Tid, Desc) ->
    #'AuditRequest'{terminationID   = Tid, auditDescriptor = Desc}.

cre_auditRes(Tid, Res) ->
    #'AuditResult'{terminationID = Tid, terminationAuditResult = Res}.


%% AMM/AMMS related
cre_ammReq(Tid, Descs) ->
    #'AmmRequest'{terminationID = Tid, descriptors = Descs}.

cre_ammsReply(Tid) ->
    #'AmmsReply'{terminationID = Tid}.
cre_ammsReply(Tid, Descs) ->
    #'AmmsReply'{terminationID = Tid, terminationAudit = Descs}.


%% Command related
cre_commandReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.


%% Actions related
cre_requestedActions(DmName) ->
    #'RequestedActions'{eventDM = {digitMapName, DmName}}.


%% Signal related
cre_signal(Name) ->
    #'Signal'{signalName = Name}.


%% Others
cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_packagesItem(Name, Ver) ->
    #'PackagesItem'{packageName = "nt", packageVersion = 1}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t(F,A) ->
    p(printable(get(severity),trc),trc,F,A).

d(F,A) ->
    p(printable(get(severity),dbg),dbg,F,A).

l(F,A) ->
    p(printable(get(severity),log),log,F,A).

e(F,A) ->
    p(printable(get(severity),err),err,F,A).


printable(trc,_) ->
    true;
printable(dbg,trc) ->
    false;
printable(dbg,_) ->
    true;
printable(log,log) ->
    true;
printable(log,err) ->
    true;
printable(err,err) ->
    true;
printable(_,_) ->
    false.


p(true,L,F,A) ->
    io:format("~s: " ++ F ++ "~n", [image_of(L)|A]);
p(_,_,_,_) ->
    ok.

image_of(trc) ->
    "T";
image_of(dbg) ->
    "D";
image_of(log) ->
    "L";
image_of(err) ->
    "E";
image_of(L) ->
    io_lib:format("~p",[L]).


