-module(copy_files).
-export([start/1]).

start([SslRoot,SslEay,Target]) ->
    make_dir("bin"),
    make_dir("obj"),
    LibObj = lib_obj(SslRoot, Target),
    io:format("SSL = ~p\n", [SslEay]),
    LibFiles = filelib:wildcard(filename:join(SslEay, "*")),
    case os:type() of
	{unix,_} ->
	    make_symlinks(LibFiles);
	{win32,_} ->
	    copy_tree(SslEay, "bin", ".")
    end,
    io:format("lib/obj = ~p\n", [LibObj]),
    copy_tree(LibObj, "obj", "."),
    halt(0).

make_symlinks([Name|Ns]) ->
    AbsName = filename:absname(Name),
    LinkName = filename:join("bin", filename:basename(Name)),
    case file:read_link(LinkName) of
	{ok, AbsName} ->			% Already exists
	    ok;
	_ ->
	    ok = file:make_symlink(AbsName, LinkName)
    end, 
    make_symlinks(Ns);
make_symlinks([]) -> ok.

lib_obj(SslRoot, Target) ->
    PrivObj = filename:join([SslRoot,"priv","obj"]),
    CCPath = filename:join(PrivObj, Target),
    case filelib:is_dir(CCPath) of
	true -> CCPath;
	false -> PrivObj
    end.

make_dir(Name) ->
    case file:make_dir(Name) of
	ok -> ok;
	{error,eexist} -> ok
    end.

copy_tree(Src, NewName, DestDir) ->
    io:format("copy_tree(~p, ~p, ~p)\n", [Src,NewName,DestDir]),
    TempTarName = "temp_tar_file.tar.gz",
    {ok,Tar} = erl_tar:open(TempTarName, [write,compressed]),
    ok = erl_tar:add(Tar, Src, NewName, []),
    ok = erl_tar:close(Tar),
    ok = erl_tar:extract(TempTarName, [compressed,{cwd,DestDir}]),
    ok = file:delete(TempTarName),
    ok.
