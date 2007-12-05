%%<copyright>
%% <year>2006-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% Purpose: Misc utility functions for the mstone modules
%%----------------------------------------------------------------------

-module(megaco_codec_mstone_lib).


%% API
-export([start_flex_scanner/0, stop_flex_scanner/1,
	 read_messages/1,
	 expand_dirs/2,
	 display_os_info/0, 
	 display_system_info/0, 
	 display_app_info/0,
	 detect_version/3]).

%% Internal exports
-export([flex_scanner_handler/1]).

-include_lib("kernel/include/file.hrl").


%%----------------------------------------------------------------------
%% 
%% D e t e c t   V e r s i o n
%% 
%%----------------------------------------------------------------------

detect_version(Codec, Conf, Bin) ->
    case (catch Codec:version_of(Conf, Bin)) of
	{ok, V} ->
	    case (catch Codec:decode_message(Conf, V, Bin)) of
		{ok, M} ->
		    case (catch Codec:encode_message(Conf, V, M)) of
			{ok, NewBin} ->
			    {V, NewBin};
			Error1 ->
			    error({encode_failed, Error1, Codec, Conf, M})
		    end;
		Error2 ->
		    error({decode_failed, Error2, Codec, Conf, Bin})
	    end;
	Error3 ->
	    error({version_of_failed, Error3, Codec, Conf, Bin})
    end.


%%----------------------------------------------------------------------
%% 
%% D i s p l a y   O s   I n f o 
%% 
%%----------------------------------------------------------------------

display_os_info() ->
    V = case os:version() of
	    {Major, Minor, Release} ->
		lists:flatten(
		  io_lib:format("~w.~w.~w", [Major, Minor, Release]));
	    Str ->
		Str
	end,
    case os:type() of
	{OsFam, OsName} ->
	    io:format("OS:                  ~p-~p: ~s~n", [OsFam, OsName, V]);
	OsFam ->
	    io:format("OS:                  ~p: ~s~n", [OsFam, V])
    end.
	    

%%----------------------------------------------------------------------
%% 
%% D i s p l a y   S y s t e m   I n f o 
%% 
%%----------------------------------------------------------------------

display_system_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    OtpRel  = otp_release(),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    SysHT   = erlang:system_info(heap_type),
    SysGHSz = erlang:system_info(global_heaps_size),
    SysSMP  = erlang:system_info(smp_support),
    SysNumSched  = erlang:system_info(schedulers),
    SysProcLimit = erlang:system_info(process_limit),
    SysThreads   = erlang:system_info(threads),
    SysTPSz      = erlang:system_info(thread_pool_size),
    io:format("System architecture: ~s~n", [SysArch]),
    io:format("OTP release:         ~s~n", [OtpRel]),
    io:format("System version:      ~s~n", [SysVer]),
    io:format("Heap type:           ~w~n", [SysHT]),
    io:format("Global heap size:    ~w~n", [SysGHSz]),
    io:format("Thread support:      ~w~n", [SysThreads]),
    io:format("Thread pool size:    ~w~n", [SysTPSz]),
    io:format("SMP support:         ~w~n", [SysSMP]),
    io:format("Num schedulers:      ~w~n", [SysNumSched]),
    io:format("Process limit:       ~w~n", [SysProcLimit]),
    ok.


otp_release() ->
    case (catch erlang:system_info(otp_release)) of
	R when is_list(R) ->
	    R;
	_ ->
	    "-"
    end.


%%----------------------------------------------------------------------
%% 
%% D i s p l a y   A p p   I n f o 
%% 
%%----------------------------------------------------------------------

display_app_info() ->
    display_megaco_info(),
    display_asn1_info().

display_megaco_info() ->
    MI = megaco:module_info(),
    {value, {attributes, Attr}} = lists:keysearch(attributes, 1, MI),
    {value, {app_vsn,    Ver}}  = lists:keysearch(app_vsn, 1, Attr),
    io:format("Megaco version:      ~s~n", [Ver]).

display_asn1_info() ->
    AI = megaco_ber_bin_drv_media_gateway_control_v1:info(),
    Vsn = 
	case lists:keysearch(vsn, 1, AI) of
	    {value, {vsn, V}} when atom(V) ->
		atom_to_list(V);
	    {value, {vsn, V}} when list(V) ->
		V;
	    _ ->
		"unknown"
	end,
    io:format("ASN.1 version:       ~s~n", [Vsn]).


%%----------------------------------------------------------------------
%% 
%% E x p a n d   D i r s
%% 
%%----------------------------------------------------------------------

expand_dirs(Dirs, DrvInclude) ->
    expand_dirs(Dirs, DrvInclude, []).

expand_dirs([], _, EDirs) ->
    lists:reverse(lists:flatten(EDirs));
expand_dirs([Dir|Dirs], DrvInclude, EDirs) when atom(Dir) ->
    EDir = expand_dir(atom_to_list(Dir), DrvInclude),
    expand_dirs(Dirs, DrvInclude, [EDir|EDirs]);
expand_dirs([Dir|Dirs], DrvInclude, EDirs) when list(Dir) ->
    EDir = expand_dir(Dir, DrvInclude),
    expand_dirs(Dirs, DrvInclude, [EDir|EDirs]).

expand_dir(Dir, only_drv) ->
    case Dir of
	"pretty" ->
	    [{Dir, megaco_pretty_text_encoder, [flex_scanner]},
	     {Dir, megaco_pretty_text_encoder, [flex_scanner]}];
	"compact" ->
	    [{Dir, megaco_compact_text_encoder, [flex_scanner]},
	     {Dir, megaco_compact_text_encoder, [flex_scanner]}];
	"ber" ->
	    [{Dir, megaco_ber_bin_encoder, [driver,native]},
	     {Dir, megaco_ber_bin_encoder, [driver]},
	     {Dir, megaco_ber_bin_encoder, [driver,native]},
	     {Dir, megaco_ber_bin_encoder, [driver]}];
	"per" ->
	    [{Dir, megaco_per_bin_encoder, [driver,native]},
	     {Dir, megaco_per_bin_encoder, [native]},
	     {Dir, megaco_per_bin_encoder, [driver,native]},
	     {Dir, megaco_per_bin_encoder, [native]}];
	"erlang" ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Dir, Encoder, [megaco_compressed,compressed]},
	     {Dir, Encoder, [compressed]},
	     {Dir, Encoder, [megaco_compressed,compressed]},
	     {Dir, Encoder, [compressed]}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end;
expand_dir(Dir, no_drv) ->
    case Dir of
	"pretty" ->
	    [{Dir, megaco_pretty_text_encoder, []},
	     {Dir, megaco_pretty_text_encoder, []}];
	"compact" ->
	    [{Dir, megaco_compact_text_encoder, []},
	     {Dir, megaco_compact_text_encoder, []}];
	"ber" ->
	    [{Dir, megaco_ber_bin_encoder, [native]},
	     {Dir, megaco_ber_bin_encoder, []},
	     {Dir, megaco_ber_bin_encoder, [native]},
	     {Dir, megaco_ber_bin_encoder, []}];
	"per" ->
	    [{Dir, megaco_per_bin_encoder, [native]},
	     {Dir, megaco_per_bin_encoder, []},
	     {Dir, megaco_per_bin_encoder, [native]},
	     {Dir, megaco_per_bin_encoder, []}];
	"erlang" ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Dir, Encoder, [megaco_compressed]},
 	     {Dir, Encoder, []},
	     {Dir, Encoder, [megaco_compressed]},
 	     {Dir, Encoder, []}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end;
expand_dir(Dir, _) ->
    case Dir of
	"pretty" ->
	    [{Dir, megaco_pretty_text_encoder, [flex_scanner]},
	     {Dir, megaco_pretty_text_encoder, []}];
	"compact" ->
	    [{Dir, megaco_compact_text_encoder, [flex_scanner]},
	     {Dir, megaco_compact_text_encoder, []}];
	"ber" ->
	    [{Dir, megaco_ber_bin_encoder, [driver,native]},
	     {Dir, megaco_ber_bin_encoder, [native]},
	     {Dir, megaco_ber_bin_encoder, [driver]},
	     {Dir, megaco_ber_bin_encoder, []}];
	"per" ->
	    [{Dir, megaco_per_bin_encoder, [driver,native]},
	     {Dir, megaco_per_bin_encoder, [native]},
	     {Dir, megaco_per_bin_encoder, [driver]},
	     {Dir, megaco_per_bin_encoder, []}];
	"erlang" ->
	    Encoder = megaco_erl_dist_encoder,
	    [
	     {Dir, Encoder, [megaco_compressed,compressed]},
	     {Dir, Encoder, [compressed]},
	     {Dir, Encoder, [megaco_compressed]},
 	     {Dir, Encoder, []}
	    ];
	Else ->
	    error({invalid_codec, Else})
    end.


%%----------------------------------------------------------------------
%% 
%% R e a d   M e s s a g e s
%% 
%%----------------------------------------------------------------------

read_messages(Dir) ->
    [read_message(Dir, File) || File <- read_files(Dir)].

read_message(Dir, FileName) ->
    File = filename:join([Dir, FileName]),
    case file:read_file_info(File) of
        {ok, #file_info{size = Sz, type = regular}} when Sz > 0 ->
            case file:read_file(File) of
		{ok, Msg} ->
		    Msg;
		Error ->
		    error({failed_reading_file, Error})
	    end;

        {ok, #file_info{type = regular}} ->
            error({file_empty, FileName});

        {ok, #file_info{type = Type}} ->
            error({invalid_type, Type, FileName});

        {ok, Info} ->
            error({unexpected_file_info, Info, FileName});

        Error ->
            error({failed_reading_file_info, Error})

    end.


read_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:sort(Files);
        Error ->
            error({failed_listing_dir, Error})
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%% 
%% S t a r t   F l e x   S c a n n e r   H a n d l e r
%% 
%%----------------------------------------------------------------------

start_flex_scanner() ->
    Pid = proc_lib:spawn(?MODULE, flex_scanner_handler, [self()]),
    receive
        {flex_scanner_started, Pid, Conf} ->
            {Pid, [Conf]};
        {flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
            error({failed_loading_flex_scanner_driver, Reason});
        {flex_scanner_error, Reason} ->
            error({failed_loading_flex_scanner_driver, Reason})
    after 10000 ->
            exit(Pid, kill),
            error({failed_starting_flex_scanner, timeout})
    end.

%%----------------------------------------------------------------------
%% 
%% S t o p   F l e x   S c a n n e r   H a n d l e r
%% 
%%----------------------------------------------------------------------

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
        _Other ->
            flex_scanner_handler(Pid, Port)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(Reason) ->
    throw({error, Reason}).

