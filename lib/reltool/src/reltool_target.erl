%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(reltool_target).

%% Public
-export([
         gen_config/1,
         gen_app/1,
         gen_rel/2, gen_rel_files/2,
         gen_boot/1,
         gen_script/4,
         gen_target/2,
         install/2
        ]).

-include_lib("kernel/include/file.hrl").
-include("reltool.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hardcoded internals about the kernel application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Mandatory modules are modules that must be loaded before processes
%% can be started. These are a collection of modules from the kernel
%% and stdlib applications. Nowadays, error_handler dynamically loads
%% almost every module. The error_handler self must still be there
%% though.

mandatory_modules() ->
    [error_handler].

%% Kernel processes are specially treated by the init process. If a
%% kernel process terminates the whole system terminates.

kernel_processes(KernelApp) ->
    [
     {kernelProcess, heart, {heart, start, []}},
     {kernelProcess, error_logger , {error_logger, start_link, []}},
     {kernelProcess, application_controller, {application_controller, start, [KernelApp]}}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a config file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_config(#sys{mod_cond = ModCond,
                incl_cond = AppCond,
                debug_info = DebugInfo,
                app_file = AppFile,
                incl_erts_dirs = InclErtsDirs,
                excl_erts_dirs = ExclErtsDirs,
                incl_app_dirs = InclAppDirs,
                excl_app_dirs = ExclAppDirs,
                root_dir = RootDir,
                lib_dirs = LibDirs,
                escripts = Escripts,
                apps = Apps,
                boot_rel = BootRel,
                rels = Rels}) ->
    ErtsItem = 
        case lists:keysearch(erts, #app.name, Apps) of
            {value, Erts} ->
                [{erts, gen_config(Erts)}];
            false ->
                []
        end,
    Default = reltool_utils:default_rels(),
    RelsItem =
        case {[{rel, R#rel.name, R#rel.vsn, gen_config(R)} || R <- Rels], 
              [{rel, R#rel.name, R#rel.vsn, gen_config(R)} || R <- Default]} of
            {RI, RI} -> [];
            {RI, _}  -> RI
        end,
    {sys, 
     [{mod_cond, ModCond}            || ModCond      =/= ?DEFAULT_MOD_COND] ++
     [{incl_cond, AppCond}           || AppCond      =/= ?DEFAULT_INCL_COND] ++
     [{debug_info, DebugInfo}        || DebugInfo    =/= ?DEFAULT_DEBUG_INFO] ++
     [{app_file, AppFile}            || AppFile      =/= ?DEFAULT_APP_FILE] ++
     [{incl_erts_dirs, InclErtsDirs} || InclErtsDirs =/= ?DEFAULT_INCL_ERTS_DIRS] ++
     [{excl_erts_dirs, ExclErtsDirs} || ExclErtsDirs =/= ?DEFAULT_EXCL_ERTS_DIRS] ++
     [{incl_app_dirs, InclAppDirs}   || InclAppDirs  =/= ?DEFAULT_INCL_APP_DIRS] ++
     [{excl_app_dirs, ExclAppDirs}   || ExclAppDirs  =/= ?DEFAULT_EXCL_APP_DIRS] ++
     [{root_dir, RootDir}            || RootDir      =/= code:root_dir()] ++
     [{lib_dirs, LibDirs}            || LibDirs      =/= []] ++
     [{escripts, Escripts}           || Escripts     =/= []] ++
     ErtsItem ++
     [{app, A#app.name, gen_config(A)} || A <- Apps, A#app.name =/= erts] ++
     [{boot_rel, BootRel} || BootRel =/= ?DEFAULT_REL_NAME] ++
     RelsItem};
gen_config(#app{name = _Name,
                mod_cond = ModCond,
                incl_cond = AppCond,
                debug_info = DebugInfo,
                app_file = AppFile,
                incl_app_dirs = InclAppDirs,
                excl_app_dirs = ExclAppDirs,
                use_selected_vsn = UseSelected,
                vsn = Vsn,
                mods = Mods}) ->
    [{mod_cond, ModCond}     || ModCond   =/= undefined] ++
        [{incl_cond, AppCond}    || AppCond   =/= undefined] ++
        [{debug_info, DebugInfo} || DebugInfo =/= undefined] ++
        [{app_file, AppFile}     || AppFile   =/= undefined] ++
        [{incl_app_dirs, InclAppDirs}   || InclAppDirs  =/= undefined] ++
        [{excl_app_dirs, ExclAppDirs}   || ExclAppDirs  =/= undefined] ++
        [{vsn, Vsn}              || Vsn =/= undefined, UseSelected =/= true] ++
        [{mod, M#mod.name, gen_config(M)} || M <- Mods];
gen_config(#mod{name = _Name,
                incl_cond = AppCond,
                debug_info = DebugInfo}) ->
    [{incl_cond, AppCond}    || AppCond =/= undefined] ++
        [{debug_info, DebugInfo} || DebugInfo =/= undefined];
gen_config(#rel{name = _Name,
                vsn = _Vsn,
                rel_apps = RelApps}) ->
    [gen_config(RA) || RA <- RelApps];
gen_config(#rel_app{name = Name,
                    type = Type,
                    incl_apps = InclApps}) ->
    case {Type, InclApps} of
        {undefined, []} -> Name;
        {undefined, _}  -> {Name, InclApps};
        {_, []}         -> {Name, Type};
        {_, _}          -> {Name, Type, InclApps}
    end;
gen_config({Tag, Val}) ->
    [{Tag, Val} || Val =/= undefined];
gen_config([]) ->
    [];
gen_config([H | T]) ->
    lists:flatten([gen_config(H), gen_config(T)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of an app file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_app(#app{name = Name, 
             info = #app_info{description = Desc,
                              id = Id,
                              vsn = Vsn,
                              modules = Mods,
                              maxP = MaxP,
                              maxT = MaxT,
                              registered = Regs,
                              incl_apps = InclApps,
                              applications = ReqApps,
                              env = Env,
                              mod = StartMod,
                              start_phases = StartPhases}}) ->
    StartMod2 =
        case StartMod =:= undefined of
            true  -> [];
            false -> [{mod, StartMod}]
        end,
    {application, Name,
     [{description, Desc},
      {vsn, Vsn},
      {id, Id},
      {modules, Mods},
      {registered, Regs},
      {applications, ReqApps},
      {included_applications, InclApps},
      {env, Env},
      {start_phases, StartPhases},
      {maxT, MaxT},
      {maxP, MaxP} |
      StartMod2]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a rel file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_rel(#rel{name = RelName, vsn = RelVsn, rel_apps = RelApps},
        #sys{apps = Apps}) ->
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    {release, 
      {RelName, RelVsn},
      {erts, Erts#app.vsn},
      [app_to_rel(RA, Apps ) || RA <- RelApps]}.

app_to_rel(#rel_app{name = Name, type = Type, incl_apps = InclApps}, Apps) ->
    {value, #app{vsn = Vsn}} = lists:keysearch(Name, #app.name, Apps),
    case {Type, InclApps} of
        {undefined, []} -> {Name, Vsn};
        {undefined, _}  -> {Name, Vsn, InclApps};
        {_, []}         -> {Name, Vsn, Type};
        {_, _}          -> {Name, Vsn, Type, InclApps}
    end.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a boot file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_boot({script, {_, _}, _} = Script) ->
    {ok, term_to_binary(Script)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a script file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_script(Rel, Sys, PathFlag, Variables) ->
    try
        do_gen_script(Rel, Sys, PathFlag, Variables)
    catch 
        throw:{error, Text} ->
            {error, Text}
    end.

throw_error(Format, Args) ->
    throw({error, lists:flatten(io_lib:format(Format, Args))}).

do_gen_script(#rel{name = RelName, vsn = RelVsn, rel_apps = RelApps},
              #sys{apps = Apps},
              PathFlag,
              Variables) ->
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    Preloaded = [Mod#mod.name || Mod <- Erts#app.mods],
    Mandatory = mandatory_modules(),
    Early = Mandatory ++ Preloaded,
    MergedApps = [merge_app(RA, Apps) || RA <- RelApps],
    SortedApps = sort_apps(MergedApps),
    {value, KernelApp} = lists:keysearch(kernel, #app.name, SortedApps),

    InclApps = lists:append([I || #app{info = #app_info{incl_apps = I}} <- SortedApps]),

    %% Create the script
    DeepList =
        [
         %% Register preloaded modules
         {preLoaded, lists:sort(Preloaded)},
         {progress, preloaded},

         %% Load mandatory modules
         {path, create_mandatory_path(SortedApps, PathFlag, Variables)},
         {primLoad, lists:sort(Mandatory)},
         {kernel_load_completed},
         {progress, kernel_load_completed},
         
         %% Load remaining modules
         [load_app_mods(A, Early, PathFlag, Variables) || A <- SortedApps],
         {progress, modules_loaded},
         
         %% Start kernel processes
         {path, create_path(SortedApps, PathFlag, Variables)},
         kernel_processes(gen_app(KernelApp)),
         {progress, init_kernel_started},
         
         %% Load applications
         [{apply, {application, load, [gen_app(A)]}} ||
             A <- SortedApps,
             A#app.name =/= kernel,
             A#app.type =/= none],
         {progress, applications_loaded},
         
         %% Start applications
         [{apply, {application, start_boot, [Name, Type]}} ||
             #app{name = Name, type = Type} <- SortedApps, 
             Type =/= none, 
             Type =/= load, 
             not lists:member(Name, InclApps)],
         
         %% Apply user specific customizations
                    {apply, {c, erlangrc, []}},
         {progress, started}
        ],
    {ok, {script, {RelName, RelVsn}, lists:flatten(DeepList)}}.

merge_app(#rel_app{name = Name, type = Type, incl_apps = RelIncl}, Apps) ->
    {value, App} = lists:keysearch(Name, #app.name, Apps),
    Type2 =
        case {Type, App#app.type} of
            {undefined, undefined} -> permanent;
            {undefined, AppType} -> AppType;
            {_, _} -> Type
        end,
    Info = App#app.info,
    case RelIncl -- Info#app_info.incl_apps of
        [] ->
            App#app{type = Type2, info = Info#app_info{incl_apps = RelIncl}};
        BadIncl ->
            throw_error("~p: These applications are missing as "
                        "included_applications in the app file: ~p\n",
                        [Name, BadIncl])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_app_mods(#app{mods = Mods} = App, Mand, PathFlag, Variables) ->
    Path = cr_path(App, PathFlag, Variables),
    PartNames =
        lists:sort([{packages:split(M),M} ||
                       #mod{name = M} <- Mods,
                       not lists:member(M, Mand)]),
    SplitMods =
        lists:foldl(
          fun({Parts,M}, [{Last, Acc}|Rest]) ->
                  [_|Tail] = lists:reverse(Parts),
                  case lists:reverse(Tail) of
                      Subs when Subs == Last ->
                          [{Last,[M|Acc]}|Rest];
                      Subs ->
                          [{Subs, [M]}|[{Last,Acc}|Rest]]
                  end
          end, 
          [{[],
            []}],
          PartNames),
    lists:foldl(
      fun({Subs,Ms}, Cmds) ->
              [{path, [filename:join([Path | Subs])]},
               {primLoad, lists:sort(Ms)} | Cmds]
      end,
      [],
      SplitMods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: sort_apps(Apps) -> {ok, Apps'} | throw({error, Error})
%% Types: Apps = {{Name, Vsn}, #application}]
%% Purpose: Sort applications according to dependencies among
%%          applications.  If order doesn't matter, use the same
%%          order as in the original list.
%% Alg. written by Ulf Wiger 970917 (etxuwig@etxb.ericsson.se)
%% Mod. by mbj
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_apps(Apps) -> 
    sort_apps(Apps, [], [], []).

sort_apps([#app{name = Name, info = Info} = App | Apps], Missing, Circular, Visited) ->
    {Uses, Apps1, NotFnd1} = find_all(Name, Info#app_info.applications, Apps, Visited, [], []),
    {Incs, Apps2, NotFnd2} = find_all(Name, lists:reverse(Info#app_info.incl_apps),
                                      Apps1, Visited, [], []),
    
    Missing1 = NotFnd1 ++ NotFnd2 ++ Missing,
    case Uses ++ Incs of
        [] -> 
            %% No more app that must be started before this one is
            %% found; they are all already taken care of (and present
            %% in Visited list)
            [App | sort_apps(Apps, Missing1, Circular, [Name | Visited])];
        L ->
            %% The apps in L must be started before the app.
            %% Check if we have already taken care of some app in L,
            %% in that case we have a circular dependency.
            NewCircular = [N1 || N1 <- L, N2 <- Visited, N1 =:= N2],
            Circular1 = case NewCircular of 
                            [] -> Circular; 
                            _ -> [Name | NewCircular] ++ Circular
                        end,
            %% L must be started before N, try again, with all apps
            %% in L added before N.
            Apps3 = del_apps(NewCircular, L ++ [App | Apps2]),
            sort_apps(Apps3, Missing1, Circular1, [Name | Visited])
    end;
sort_apps([], [], [], _) ->
    [];
sort_apps([], Missing, [], _) ->
    %% this has already been checked before, but as we have the info...
    throw_error("Undefined applications: ~p\n", [make_set(Missing)]);
sort_apps([], [], Circular, _) ->
    throw_error("Circular dependencies: ~p\n", [make_set(Circular)]);
sort_apps([], Missing, Circular, _) ->
    throw_error("Circular dependencies: ~p\n"
                "Undefined applications: ~p\n",
                [make_set(Circular), make_set(Missing)]).

find_all(CheckingApp, [Name | Names], Apps, Visited, Found, NotFound) ->
    case lists:keysearch(Name, #app.name, Apps) of
        {value, #app{info = Info} = App} ->
            %% It is OK to have a dependecy like
            %% X includes Y, Y uses X.
            case lists:member(CheckingApp, Info#app_info.incl_apps) of
                true ->
                    case lists:member(Name, Visited) of
                        true ->
                            find_all(CheckingApp, Names, Apps, Visited, Found, NotFound);
                        false ->
                            find_all(CheckingApp, Names, Apps, Visited, Found, [Name | NotFound])
                    end;
                false ->
                    find_all(CheckingApp, Names, Apps -- [App], Visited, [App|Found], NotFound)
            end;
        false ->
            case lists:member(Name, Visited) of
                true ->
                    find_all(CheckingApp, Names, Apps, Visited, Found, NotFound);
                false ->
                    find_all(CheckingApp, Names, Apps, Visited, Found, [Name|NotFound])
            end
    end;
find_all(_CheckingApp, [], Apps, _Visited, Found, NotFound) ->
    {Found, Apps, NotFound}.
            
del_apps([Name | Names], Apps) ->
    del_apps(Names, lists:keydelete(Name, #app.name, Apps));
del_apps([], Apps) ->
    Apps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the load path used in the generated script.
%% If PathFlag is true a script intended to be used as a complete
%% system (e.g. in an embbeded system), i.e. all applications are
%% located under $ROOT/lib.
%% Otherwise all paths are set according to dir per application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create the complete path.
create_path(Apps, PathFlag, Variables) ->
    make_set([cr_path(App, PathFlag, Variables) || App <- Apps]).

%% Create the path to a specific application.
%% (The otp_build flag is only used for OTP internal system make)
cr_path(#app{label = Label}, true, []) ->
    filename:join(["$ROOT", "lib", Label, "ebin"]);
cr_path(#app{name = Name, vsn = Vsn, label = Label, active_dir = Dir}, true, Variables) ->
    Tail = [Label, "ebin"],
    case variable_dir(Dir, atom_to_list(Name), Vsn, Variables) of
        {ok, VarDir} ->
            filename:join([VarDir] ++ Tail);
        _ ->
            filename:join(["$ROOT", "lib"] ++ Tail)
    end;
cr_path(#app{name = Name}, otp_build, _) ->
    filename:join(["$ROOT", "lib", atom_to_list(Name), "ebin"]);
cr_path(#app{active_dir = Dir}, _, _) ->
    filename:join([Dir, "ebin"]).

variable_dir(Dir, Name, Vsn, [{Var,Path} | Variables]) ->
    case lists:prefix(Path, Dir) of
        true ->
            D0 = strip_prefix(Path, Dir),
            case strip_name_ebin(D0, Name, Vsn) of
                {ok, D} ->
                    {ok, filename:join(["\$" ++ Var] ++ D)};
                _ ->
                    %% We know at least that we are located
                    %% under the variable dir.
                    {ok, filename:join(["\$" ++ Var] ++ D0)}
            end;
        false ->
            variable_dir(Dir, Name, Vsn, Variables)
    end;
variable_dir(_Dir, _, _, []) ->
    false.

strip_prefix(Path, Dir) ->
    L = length(filename:split(Path)),
    lists:nthtail(L, filename:split(Dir)).

strip_name_ebin(Dir, Name, Vsn) ->
    FullName = Name ++ "-" ++ Vsn,
    case lists:reverse(Dir) of
        ["ebin", Name     | D] -> {ok, lists:reverse(D)};
        ["ebin", FullName | D] -> {ok, lists:reverse(D)};
        _                      -> false
    end.

%% Create the path to the kernel and stdlib applications.
create_mandatory_path(Apps, PathFlag, Variables) ->
    Mandatory = [kernel, stdlib],
    make_set(lists:map(fun(#app{name = Name} = App) ->
                               case lists:member(Name, Mandatory) of
                                   true ->
                                       cr_path(App, PathFlag, Variables);
                                   false ->
                                       ""
                               end
                       end,
                       Apps)).

make_set([]) ->
    [];
make_set([""|T]) -> % Ignore empty items.
    make_set(T);
make_set([H|T]) ->
    [H | [ Y || Y<- make_set(T),
                Y =/= H]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate a complete target system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_target(Sys, TargetDir) ->
    try
        do_gen_target(Sys, TargetDir)
    catch 
        throw:{error, Text} ->
            {error, Text}
    end.

do_gen_target(Sys, TargetDir) ->
    TargetDir2 = filename:absname(TargetDir),
    recursive_delete(TargetDir2),
    create_dir(TargetDir2),
    RelVsnDir = create_system_files(Sys, TargetDir2),
    ErtsDir = copy_apps(Sys, TargetDir2),
    create_bin(Sys, ErtsDir, RelVsnDir, TargetDir2).

create_system_files(Sys, TargetDir) ->
    RelDir = filename:join([TargetDir, "releases"]),
    create_dir(RelDir),
    {value, Erts} = lists:keysearch(erts, #app.name, Sys#sys.apps),
    {value, BootRel} = lists:keysearch(Sys#sys.boot_rel, #rel.name, Sys#sys.rels),
    Data = Erts#app.vsn ++ " " ++ BootRel#rel.vsn ++ "\n",
    DataFile = filename:join([RelDir, "start_erl.data"]),
    case file:write_file(DataFile, Data) of
        ok ->
            ok;
        {error, Reason} ->
            throw_error("~s: ~s\n", [DataFile, file:format_error(Reason)])
    end,
    RelVsnDir = filename:join([RelDir, BootRel#rel.vsn]),
    do_gen_rel_files(Sys, RelVsnDir),
    file:copy(filename:join([Sys#sys.root_dir, "COPYRIGHT"]),
              filename:join([TargetDir, "COPYRIGHT"])),
    RelVsnDir.    
                                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate rel, script and boot files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_rel_files(Sys, Dir) ->
    try
        do_gen_rel_files(Sys, Dir)
    catch 
        throw:{error, Text} ->
            {error, Text}
    end.

do_gen_rel_files(#sys{rels = Rels} = Sys, Dir) ->
    lists:foreach(fun(R) -> do_gen_rel_files(R, Sys, Dir) end, Rels).

do_gen_rel_files(R,  Sys, Dir) ->
    create_dir(Dir),
    Rel = gen_rel(R, Sys),
    PathFlag = true,
    Variables = [],
    {ok, Script} = gen_script(R, Sys, PathFlag, Variables),
    {ok, BootBin} = gen_boot(Script),
    Base = filename:join([Dir, R#rel.name]),
    Date = date(),
    Time = time(),
    RelIoList = io_lib:format("%% rel generated at ~w ~w\n~p.\n\n",
                              [Date, Time, Rel]),
    RelFile = Base ++ ".rel",
    RelRes = file:write_file(RelFile, RelIoList),

    ScriptIoList = io_lib:format("%% script generated at ~w ~w\n~p.\n\n",
                                 [Date, Time, Script]),
    ScriptFile = Base ++ ".script",
    ScriptRes = file:write_file(ScriptFile, ScriptIoList),

    BootFile = Base ++ ".boot",
    BootRes = file:write_file(BootFile, BootBin),
    case lists:keysearch(error, 1, [RelRes, ScriptRes, BootRes]) of
        false ->
            ok;
        {value, {_, Reason}} ->
            file:delete(RelFile),
            file:delete(ScriptFile),
            file:delete(BootFile),
            throw_error("~s: ~s\n", [RelFile, file:format_error(Reason)])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_apps(#sys{apps = Apps} = Sys, TargetDir) ->
      Filter = fun(#app{is_included = IsIncl, is_pre_included = IsPre, name = Name}) ->
                       if
                           IsIncl; IsPre ->
                               Name =/= erts andalso Name =/= ?MISSING_APP;
                           true ->
                               false
                       end
               end,
      case lists:filter(Filter, Apps) of
          [] ->
              throw_error("No applications are included", []);
          IncludedApps ->
              LibDir = filename:join([TargetDir, "lib"]),
              create_dir(LibDir),
              lists:foreach(fun(App) -> copy_app(App, Sys, LibDir) end, IncludedApps),
              copy_erts(Sys, LibDir, TargetDir)       
      end.

copy_erts(#sys{apps = Apps, root_dir = SourceDir} = Sys, LibDir, TargetDir) ->
    %% Try to copy preloaded modules
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    PreloadedDir = Erts#app.active_dir,
    case filename:basename(PreloadedDir) of
        "preloaded" ->
            Base = filename:dirname(PreloadedDir);
        Base ->
            ignore
    end,
    case filelib:is_dir(PreloadedDir, erl_prim_loader) of
        true ->
            %% Maybe copy preloaded modules for tools to play with
            do_copy_app(Erts, Sys, filename:join([LibDir, Erts#app.label]));
        false ->
            %% No preloaded modules to copy. Do not care as they are optional.
            ignore
    end,

    %% Copy main erts directory
    ErtsSource = filename:join([SourceDir, Base]),
    ErtsTarget = filename:join([TargetDir, Erts#app.label]),
    create_dir(ErtsTarget),
    {ok, SubDirs} = file:list_dir(ErtsSource),
    Copy = fun(Dir) ->
                   copy_file(filename:join([ErtsSource, Dir]),
                             filename:join([ErtsTarget, Dir]))
           end,
    lists:foreach(Copy, strip_dirs(SubDirs, Sys#sys.incl_erts_dirs, Sys#sys.excl_erts_dirs)),
    
    ErtsSourceBinDir = filename:join([ErtsSource, "bin"]),
    ErtsTargetBinDir = filename:join([ErtsTarget, "bin"]),
    case filelib:is_dir(ErtsSourceBinDir, erl_prim_loader) of
        true ->
            ok;
        false ->
            %% ClearCase special
            Arch = erlang:system_info(system_architecture),
            ClearCaseBinDir = filename:join([SourceDir, "bin", Arch]),
            copy_file(ClearCaseBinDir, ErtsTargetBinDir)
    end,

    %% Remove scripts
    file:delete(filename:join([ErtsTargetBinDir, "erl"])),
    file:delete(filename:join([ErtsTargetBinDir, "start"])),
    
    %% Remove superfluous preloaded
    recursive_delete(filename:join([ErtsTarget, "preloaded"])),
    ErtsTarget.

copy_app(#app{label = Label} = App, Sys, LibDir) ->
    do_copy_app(App, Sys, filename:join([LibDir, Label])).

do_copy_app(#app{active_dir = SourceDir,
                 incl_app_dirs = AppInclDirs,
                 excl_app_dirs = AppExclDirs} = App,
         #sys{incl_app_dirs = SysInclDirs,
              excl_app_dirs = SysExclDirs} = Sys,
            TargetDir) ->
    create_dir(TargetDir),
    {ok, SubDirs} = file:list_dir(SourceDir),
    InclDirs = default_val(AppInclDirs, SysInclDirs),
    ExclDirs = default_val(AppExclDirs, SysExclDirs),
    Copy = fun(Dir) -> copy_app_dir(Dir, SourceDir, TargetDir, App, Sys) end,
    lists:foreach(Copy, strip_dirs(SubDirs, InclDirs, ExclDirs)).
    
copy_app_dir("ebin" = Dir, SourceDir, TargetDir, App, Sys) ->
    EbinSource = filename:join([SourceDir, Dir]),
    EbinTarget = filename:join([TargetDir, Dir]),
    create_dir(EbinTarget),
    copy_app_file(EbinSource, EbinTarget, App, Sys),
    DebugInfo = default_val(App#app.debug_info, Sys#sys.debug_info),
    [copy_mod(M, EbinSource, EbinTarget, DebugInfo) ||
        M <- App#app.mods, M#mod.is_included, M#mod.exists],
    ok;
copy_app_dir(Dir, SourceDir, TargetDir, _App, _Sys) ->
    copy_file(filename:join([SourceDir, Dir]),
              filename:join([TargetDir, Dir])).

copy_app_file(SourceDir, TargetDir, App, Sys) ->
    Base = atom_to_list(App#app.name) ++ ".app",
    AppSource = filename:join([SourceDir, Base]),
    AppTarget = filename:join([TargetDir, Base]),
    Info = App#app.info,
    Mods = App#app.mods,
    AppMods = Info#app_info.modules,
    case default_val(App#app.app_file, Sys#sys.app_file) of
        keep ->
            _ = file:copy(AppSource, AppTarget); % Copy if it exists
        strip ->
            %% Remove non-included modules
            %% Generate new file
            ModNames = [M#mod.name || M <- Mods,
                                      M#mod.is_included,
                                      lists:member(M#mod.name, AppMods)],
            App2 = App#app{info = Info#app_info{modules = ModNames}},
            gen_app_file(App2, AppTarget);
        all ->
            %% Include all included modules
            %% Generate new file
            ModNames = [M#mod.name || M <- Mods, M#mod.is_included],
            App2 = App#app{info = Info#app_info{modules = ModNames}},
            gen_app_file(App2, AppTarget)
    end.

gen_app_file(App, AppFile) ->
    Content = gen_app(App),
    AppIoList = io_lib:format("%% app generated at ~w ~w\n~p.\n\n",
                              [date(), time(), Content]),
    ok = file:write_file(AppFile, AppIoList).

copy_mod(Mod, SourceDir, TargetDir, DebugInfo) ->
    Base = atom_to_list(Mod#mod.name) ++ code:objfile_extension(),
    SourceFile = filename:join([SourceDir, Base]),
    TargetFile = filename:join([TargetDir, Base]),    
    case default_val(Mod#mod.debug_info, DebugInfo) of
        keep ->
            {ok, _} = file:copy(SourceFile, TargetFile),
            ok;
        strip ->
            {ok, BeamBin} = file:read_file(SourceFile),
            {ok, {_, BeamBin2}} = beam_lib:strip(BeamBin),
            ok = file:write_file(TargetFile, BeamBin2)
    end.

strip_dirs(_SubDirs, _InclDirs, all) ->
    [];
strip_dirs(SubDirs, all, ExclDirs) ->
    SubDirs -- ExclDirs;
strip_dirs(SubDirs, InclDirs, ExclDirs) ->
    [Dir || Dir <- SubDirs, 
            lists:member(Dir, InclDirs),
            not lists:member(Dir, ExclDirs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_bin(Sys, ErtsDir, RelVsnDir, TargetDir) ->
    SystemBinDir = filename:join([TargetDir, "bin"]),
    create_dir(SystemBinDir),
    ErtsBinDir = filename:join([ErtsDir, "bin"]),
    BootFile = Sys#sys.boot_rel ++ ".boot",
    {ok, Files} = file:list_dir(RelVsnDir),
    [copy_file(RelVsnDir, F, SystemBinDir, F) || F <- Files],
    copy_file(RelVsnDir, BootFile, SystemBinDir, "start.boot"),
    [copy_file(ErtsBinDir, F, SystemBinDir, F) || F <- execs(Sys)],
    [copy_file(F, filename:join([SystemBinDir, filename:basename(F)])) || F <- escripts(Sys)],
    ok.

execs(Sys) ->
    Execs = 
	case Sys#sys.profile of
	    standalone  -> [];
	    development -> ["dialyzer", "erl", "erlc", "escript", "typer"];
	    embedded    -> ["erl", "escript"]
	end,
    case os:type() of
	{win32, _} -> [Exec ++ ".exe" || Exec <- Execs];
	_          -> Execs
    end.

escripts(Sys) ->
    case Sys#sys.profile of
	standalone  -> Sys#sys.escripts;
	development -> [];
	embedded    -> []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install(RelName, TargetDir) ->
    try
        do_install(RelName, TargetDir)
    catch 
        throw:{error, Text} ->
            {error, Text}
    end.

do_install(RelName, TargetDir) ->
    TargetDir2 = filename:absname(TargetDir),
    RelDir = filename:join([TargetDir2, "releases"]),
    DataFile = filename:join([RelDir, "start_erl.data"]),
    case file:read_file(DataFile) of
        {ok, Bin} ->
            case string:tokens(binary_to_list(Bin), " \n") of
                [ErlVsn, RelVsn | _] ->
                    ErtsBinDir = filename:join([TargetDir2, "erts-" ++ ErlVsn, "bin"]),
                    BinDir = filename:join([TargetDir2, "bin"]),
                    subst_src_scripts(start_scripts(), ErtsBinDir, BinDir, 
                                      [{"FINAL_ROOTDIR", TargetDir2}, {"EMU", "beam"}],
                                      [preserve]),
                    RelFile = filename:join([RelDir, RelVsn, RelName ++ ".rel"]),
                    ok = release_handler:create_RELEASES(TargetDir2, RelFile),
                    ok;
                _ ->
                    throw_error("~s: Illegal syntax.\n", [DataFile])
            end;
        {error,Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s: ~s\n", [DataFile, Text])
    end.

subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) ->
    Fun = fun(Script) -> subst_src_script(Script, SrcDir, DestDir, Vars, Opts) end,
    lists:foreach(Fun, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) -> 
    subst_file(filename:join([SrcDir, Script ++ ".src"]),
               filename:join([DestDir, Script]),
               Vars, 
               Opts).

subst_file(Src, Dest, Vars, Opts) ->
    case file:read_file(Src) of
        {ok, Bin} ->
            Chars = subst(binary_to_list(Bin), Vars),
            case file:write_file(Dest, Chars) of
                ok ->
                    case lists:member(preserve, Opts) of
                        true ->
                            {ok, FileInfo} = file:read_file_info(Src),
                            ok = file:write_file_info(Dest, FileInfo);
                        false ->
                            ok
                    end;
                {error, Reason} ->
                    Text = file:format_error(Reason),
                    throw_error("~s: ~s\n", [Src, Text])
            end;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s: ~s\n", [Src, Text])
    end.

%% subst(Str, Vars)
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Str, using the list
%% of variables in Vars.
%%
subst(Str, Vars) ->
    subst(Str, Vars, []).

subst([$%, C | Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C | Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C | Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$% | VarAcc ++ [$% | Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$% | Result]]).
   

start_scripts() ->
    ["erl", "start", "start_erl"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dir(Dir) ->
    filelib:ensure_dir(Dir),
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s: ~s\n", [Dir, Text])
    end.

recursive_delete(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            Fun = fun(F) -> recursive_delete(filename:join([Dir, F])) end,
            lists:foreach(Fun, Files),
            delete(Dir, directory);
        {error, enoent} ->
            ok;
        {error, enotdir} ->
            %% Plain file
            delete(Dir, regular);
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s: ~s\n", [Dir, Text])
    end.

delete(File, Type) ->
    case do_delete(File, Type) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s: ~s\n", [File, Text])
    end.

do_delete(File, regular) ->
    file:delete(File);
do_delete(Dir, directory) ->
    file:del_dir(Dir).

copy_file(FromDir, FromBase, ToDir, ToBase) ->
    copy_file(filename:join([FromDir, FromBase]),
	      filename:join([ToDir, ToBase])).

copy_file(From, To) ->
    case file:list_dir(From) of
        {ok, Files} ->
            create_dir(To),
            Copy =
                fun(F) ->
                        copy_file(filename:join([From, F]),
                                  filename:join([To, F]))
                end,
            lists:foreach(Copy, Files);
        {error, enotdir} ->
            %% Plain file
            case file:copy(From, To) of
                {ok, _} ->
                    {ok, FromInfo} = file:read_file_info(From),
                    {ok, ToInfo} = file:read_file_info(To),
                    FromMode = FromInfo#file_info.mode,
                    ToMode = ToInfo#file_info.mode,
                    ToMode2 = FromMode bor ToMode,
                    FileInfo = FromInfo#file_info{mode = ToMode2},
                    ok = file:write_file_info(To, FileInfo),
                    ok;
                {error, Reason} ->
                    Text = file:format_error(Reason),
                    throw_error("~s -> ~s: ~s\n", [From, To, Text])
            end;
        {error, Reason} ->
            Text = file:format_error(Reason),
            throw_error("~s -> ~s: ~s\n", [From, To, Text])
    end.

default_val(Val, Default) ->
    case Val of
        undefined -> Default;
        _         -> Val
    end.
