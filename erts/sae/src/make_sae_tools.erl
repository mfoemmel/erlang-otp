-module(make_sae_tools).

%% erlc fake_ecc_code.erl
%% make_sae_tools:all().
%%   makes ecc_base
%%  ecc_base hello.erl ok
%%  ecc_base "test.erl"
%% ./test.erl:34: can't find include lib "kernel/include/file.hrl"
%%  ecc_base -install /usr/local/lib/erlang 
%%  makes ecc
%%  ecc test.erl

-doc([{author,joe},
      {title,"Stand alone compiler for erlang"},
      {keywords, [stand,alone,compiler]},
      {date,981012}]).

-compile(export_all).
-export([all/0, main/1, read_mods/2]).

-import(lists, [map/2]).

all() -> 
    elink:make_elink(),
    io:format("made elink  -- the Erlang linker~n"),
    make_ecc(),
    io:format("made ecc    -- the Erlang compiler~n"),
    make_esh(),
    io:format("made esh    -- the Erlang shell~n").

%%----------------------------------------------------------------------
%% This is the entry point for the compiler

%% The first argument of main is "erl_base" which we throw away

main([_, "-install", Dir]) ->
    start_sys(),
    elink:patch("ecc", "ERL_CODE_ROOT", Dir),
    elink:patch("esh", "ERL_CODE_ROOT", Dir),
    io:format("ecc patched~n"),
    io:format("esh patched~n"),
    erlang:halt();
main([File|T]) ->
    start_sys(),
    Root = code_root_dir(),
    %% io:format("Root=~p~n",[Root]),
    code:start(Root),
    map(fun compile/1, T),
    code:stop(),
    erlang:halt().

code_root_dir() ->
    case os:getenv("ERL_CODE_ROOT") of
	false ->
	    "";
	Str ->
	    Str
    end.

start_sys() ->
    %% tricky bit, this sets up a file system and IO user server
    %% up and running
    user:start_out(),
    G = whereis(user),
    group_leader(G, self()),
    %% io:format("IO is running ...\n"),
    file_server:start(),
    %% io:format("File server running ...\n"),
    true.

compile(File) ->
    case filename:extension(File) of
	".erl" ->
	    %% io:format("compiling:~p~n",[File]),
	    V = compile:file(File, [report_errors, report_warnings]),
	    io:format("~p ~p~n",[File, V]),
	    case V of
		{ok, _} ->
		    Beam = filename:rootname(File) ++ ".beam",
		    {ok, Bin1} = file:read_file(Beam),
		    Bin2 = compress_binary(Bin1),
		    file:write_file(Beam, Bin2);
		_ ->
		    true
	    end;
	_ ->
	    io:format("Bad extension:~p~n",[File])
    end.

%%----------------------------------------------------------------------
%% All the code below this line is used to build
%% erl_base itself.

make_ecc() ->
    %% SpecialModules is a list of {Mod,FileName} pairs
    %% this is used when the filename and module name differ
    compile:file("elink.erl"), %% to pull in the compiler
    compile:file("erl_root_dir"),
    LocalMods = [prim_file,
		 erl_root_dir,
		 fake_ecc_code,
		 erl_prim_loader,
		 fake_ecc_init,
		 fake_ecc_error_handler],
    Ms = read_mods(compiler_modules_r8b(), LocalMods),
    code:purge(make_sae_tools),
    elink:make("ecc", Ms, make_sae_tools, main, [""]).

read_mods(StandardMods, SpecialMods) ->
    M1 = map(fun(I) -> {I, get_standard_module(I)} end, StandardMods),
    %% io:format("now local modules~n"),
    M2 = map(fun(M) -> {real_name(M), get_local_module(M)} end, 
	    SpecialMods),
    M1 ++ M2.

%% get_standard_module(mod::atom()) -> {Modname::atom(), bin()}
%%   fetch code using the code path

get_standard_module(M) ->
    io:format("get_standard_module:~p~n",[M]),
    case code:is_loaded(M) of
	{file,preloaded} ->
	    io:format("*** ~p is preloaded~n",  [M]);
	{file, F} ->
	    {ok, Bin} = file:read_file(F),
	    %% io:format("read:~p~n",[F]),
	    compress_binary(Bin);
	false ->
	    case code:ensure_loaded(M) of
		{module, M} ->
		    case code:is_loaded(M) of
			{file, F} ->	
			    {ok, Bin} = file:read_file(F),
			    %% io:format("read:~p~n",[M]),
			    compress_binary(Bin);
			false ->
			    exit({cannot_find_code_for, M})
		    end;
		{error, What} ->
		    exit({cannot_find_code_for, M, reason, What})
	    end
    end.

%% Get a local module 
get_local_module(Atom) ->    
    %% io:format("get_local:~p~n",[Atom]),
    File = filename:join("../ebin", atom_to_list(Atom) ++ ".beam"),
    case file:read_file(File) of
	{ok, Bin} -> 
	    %% io:format("read file:~p size:~p~n",[File, size(Bin)]),
	    Bin;
	_ -> 
	    erlang:display({cannot,locate,local,file,File}),
	    erlang:halt()
    end.

real_name(A) -> 
    list_to_atom(real_name1(atom_to_list(A))).

real_name1([$f,$a,$k,$e,$_|T]) -> skip_under(T);
real_name1(X) -> X.

skip_under([$_|T]) -> T;
skip_under([_|T]) -> skip_under(T);
skip_under([]) -> exit(bad_fake_name).

%% compress_binary(B) -> B;
compress_binary(B) ->
    {B1, _} = split_binary(B, 2),
    case binary_to_list(B1) of
	[8#037,8#213] -> 
	    %% it already was compressed -- ho ho
	    B;
	_ ->
	    {ok, P} = ram_file:open(B, [read,binary]),
	    ram_file:compress(P),
	    {ok, B2} = ram_file:get_file_close(P),
	    B2
    end.

%%----------------------------------------------------------------------
%% compiler_modules() -> [Mod] 
%%   The list of modules required by the compiler
%%
%% The initial list was found by compiling a file (which causes all
%% the compiler modules to be loaded) - and then by 
%% calling code:all_loaded().
%%
%% The initial list had 76 modules, 36 of which are retained.
%%
%% A "%%" in front of the name means I commented out this line and the
%%       compiler *appears* to still work
%% "% need" means I commented out this line and the compiler definately 
%%       didn't work when this file was missing

compiler_modules_r8b() ->
    [elink, make_sae_tools, erl_posix_msg, ram_file, user] ++
	[%% application,
	 %% application_controller,
	 %% application_master,
	 beam_bs,
	 beam_block,
	 beam_jump,
	 beam_type,
	 beam_clean,
	 beam_flatten,
	 beam_listing,
	 beam_asm,
	 beam_dict,
	 beam_opcodes,
	 beam_pp,
	 c,
	 cerl,
	 cerl_clauses,
	 cerl_inline,
	 cerl_trees,
	 code,
	 %% code_aux,
	 %% code_server,
	 %% code_server_int,
	 compile,
	 core_lib,
	 dict,
	 %% edlin,
	 epp,
	 erl_bits,
	 %% erl_distribution,
	 %% erl_eval,
	 erl_internal,
	 erl_lint,
	 erl_open_port, %% keep
	 erl_parse,
	 %% erl_prim_loader, SPECIAL
	 erl_scan,
	 erlang,
	 %% error_handler, SPECIAL
	 error_logger,
	 error_logger_tty_h,
	 esh,
	 ets,
	 file,
	 file_server,
	 file_io_server,
	 filename,
	 gb_sets,
	 gen,
	 %% gen_event,
	 gen_server, %% keep
	 %% global,
	 %% global_group,
	 group,
	 %% heart,
	 %% inet,
	 %% inet_config,
	 %% inet_db,
	 %% inet_hosts,
	 %% inet_parse,
	 %% inet_udp,
	 %% init,
	 io,
	 io_lib,
	 io_lib_format,
	 io_lib_pretty,
	 %% kernel,
	 %% kernel_config,
	 lists,
	 lists_sort,
	 orddict,
	 ordsets,
	 os,
	 otp_internal,
	 %% otp_ring0,  PRELOADED
	 %% prim_inet,  PRELOADED
	 proc_lib,   %% keep
	 property_lists,
	 rec_env,
	 %% rpc,
	 sets,
	 %% shell,
	 %% shell_default,
	 sofs,
	 string,
	 %% supervisor,
	 %% supervisor_bridge,
	 sys,
	 sys_core_fold,
	 sys_core_inline,
	 sys_pre_expand,
	 user_drv,
	 user_sup,
	 v3_codegen,
	 v3_core,
	 v3_core_opt,
	 v3_kernel,
	 v3_life].

%% make_esh(Dir).
make_esh() ->
    %% io:format("Here%~n"),
    Ms = read_mods([], [init,erlang,erl_prim_loader,erl_root_dir,esh]),
    %% io:format("Here:~p~n",[Ms]),
    elink:make("esh", Ms, esh, main, [""]).
