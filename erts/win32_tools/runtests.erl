-module(runtests).

-author('jakob@erix.ericsson.se').

-export([install_at/1, run_at/1, save_at/1, install_run_save/1, delete_tree/1, copy_tests/1, just_to_start_epmd/0]).

run_at(Tstdir) ->
    file:set_cwd(Tstdir),
    ts:run(),
    halt().

save_at(Tstdir) ->
    file:set_cwd(Tstdir),
    ts:save(),
    halt().

install_at(Tstdir) ->
    file:set_cwd(Tstdir),
    ts:install(),
    halt().

install_run_save(Tstdir) ->
    file:set_cwd(Tstdir),
    io:format("Installing at ~s ~n", [Tstdir]),
    ts:install(),
    io:format("Runnig tests at ~s ~n", [Tstdir]),
    ts:run(),
    io:format("Saving from ~s ~n", [Tstdir]),
    ts:save(),
    halt().


delete_tree(Item) -> % no error checking whatsoever
    case filelib:is_dir(Item) of
	true ->
	    Wild= filename:join(Item, "*"),
	    Items= filelib:wildcard(Wild),
	    lists:foreach(fun(F) -> delete_tree(F) end, Items),
%	    io:format("d ~s ~n", [Item]),
	    file:change_mode(Item, 8#777),
	    file:del_dir(Item);
	false ->
%	    io:format("f ~s ~n", [Item]),
	    file:change_mode(Item, 8#777),
	    file:delete(Item)
    end.

copy_tests([Test_tar, Nt_test_path, Nt_tmp_path]) ->
    Testbak= Nt_test_path++".bak",
    io:format("Renaming ~s to ~s ~n", [Nt_test_path, Testbak]),
    file:rename(Nt_test_path, Testbak),
    io:format("Removing ~s ~n", [Testbak]),
    delete_tree(Testbak),
    io:format("Creating ~s ~n", [Nt_test_path]),
    file:make_dir(Nt_test_path),
    Tar_file= lists:last(filename:split(Test_tar)),
    Nt_tar= filename:join(Nt_tmp_path, Tar_file),
    io:format("Untaring ~s to ~s~n", [Nt_tar, Nt_test_path]),
    file:set_cwd(Nt_test_path),
    erl_tar:extract(Nt_tar, [compressed]),
    io:format("Done~n", []),
    halt().

just_to_start_epmd() ->
    erlang:display(ok),
    halt().
