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
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_appup_test).

-compile(export_all).

-include("megaco_test_lib.hrl").


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 appup
	],
    {req, [], {conf, appup_init, Cases, appup_fin}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup_init(suite) -> [];
appup_init(doc) -> [];
appup_init(Config) when list(Config) ->
    AppFile   = file_name(megaco, ".app"),
    AppupFile = file_name(megaco, ".appup"),
    [{app_file, AppFile}, {appup_file, AppupFile}|Config].
    

file_name(App, Ext) ->
    LibDir = code:lib_dir(App),
    filename:join([LibDir, "ebin", atom_to_list(App) ++ Ext]).


appup_fin(suite) -> [];
appup_fin(doc) -> [];
appup_fin(Config) when list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

appup(suite) ->
    [];
appup(doc) ->
    "perform a simple check of the appup file";
appup(Config) when list(Config) ->
    AppupFile = key1search(appup_file, Config),
    AppFile   = key1search(app_file, Config),
    Modules   = modules(AppFile),
    check_appup(AppupFile, Modules).

modules(File) ->
    case file:consult(File) of
        {ok, [{application,megaco,Info}]} ->
            case lists:keysearch(modules,1,Info) of
                {value, {modules, Modules}} ->
                    Modules;
                false ->
                    fail({bad_appinfo, Info})
            end;
        Error ->
            fail({bad_appfile, Error})
    end.

    
check_appup(AppupFile, Modules) ->
    case file:consult(AppupFile) of
	{ok, [{V, UpFrom, DownTo}]} ->
	    check_appup(V, UpFrom, DownTo, Modules);
	Else ->
	    fail({bad_appupfile, Else})
    end.


check_appup(V, UpFrom, DownTo, Modules) ->
    check_version(V),
    check_depends(up,   UpFrom, Modules),
    check_depends(down, DownTo, Modules),
    ok.


check_depends(_, [], _) ->
    ok;
check_depends(UpDown, [Dep|Deps], Modules) ->
    check_depend(UpDown, Dep, Modules),
    check_depends(UpDown, Deps, Modules).


check_depend(UpDown, {V, Instructions}, Modules) ->
    check_version(V),
    case check_instructions(UpDown, 
			    Instructions, Instructions, [], [], Modules) of
	{_Good, []} ->
	    ok;
	{_, Bad} ->
	    fail({bad_instructions, Bad, UpDown})
    end.


check_instructions(_, [], _, Good, Bad, _) ->
    {lists:reverse(Good), lists:reverse(Bad)};
check_instructions(UpDown, [Instr|Instrs], AllInstr, Good, Bad, Modules) ->
    case (catch check_instruction(UpDown, Instr, AllInstr, Modules)) of
        ok ->
            check_instructions(UpDown, Instrs, AllInstr, 
			       [Instr|Good], Bad, Modules);
        {error, Reason} ->
            check_instructions(UpDown, Instrs, AllInstr, Good, 
                               [{Instr, Reason}|Bad], Modules)
    end.

%% A new module is added
check_instruction(up, {add_module, Module}, _, Modules) 
  when atom(Module) ->
    check_module(Module, Modules);

%% An old module is re-added
check_instruction(down, {add_module, Module}, _, Modules) 
  when atom(Module) ->
    case (catch check_module(Module, Modules)) of
	{error, {unknown_module, Module, Modules}} ->
	    ok;
	ok ->
	    error({existing_readded_module, Module})
    end;

%% Removing a module on upgrade: 
%% - the module has been removed from the app-file.
%% - check that no module depends on this (removed) module
check_instruction(up, {remove, {Module, Pre, Post}}, _, Modules) 
  when atom(Module), atom(Pre), atom(Post) ->
    case (catch check_module(Module, Modules)) of
	{error, {unknown_module, Module, Modules}} ->
	    check_purge(Pre),
	    check_purge(Post);
	ok ->
	    error({existing_removed_module, Module})
    end;

%% Removing a module on downgrade: the module exist
%% in the app-file.
check_instruction(down, {remove, {Module, Pre, Post}}, AllInstr, Modules) 
  when atom(Module), atom(Pre), atom(Post) ->
    case (catch check_module(Module, Modules)) of
	ok ->
	    check_purge(Pre),
	    check_purge(Post),
	    check_no_remove_depends(Module, AllInstr);
	{error, {unknown_module, Module, Modules}} ->
	    error({nonexisting_removed_module, Module})
    end;

check_instruction(_, {load_module, Module, Pre, Post, Depend}, 
		  AllInstr, Modules) 
  when atom(Module), atom(Pre), atom(Post), list(Depend) ->
    check_module(Module, Modules),
    check_module_depend(Module, Depend, Modules),
    check_module_depend(Module, Depend, updated_modules(AllInstr, [])),
    check_purge(Pre),
    check_purge(Post);

check_instruction(_, {update, Module, Change, Pre, Post, Depend}, 
		  AllInstr, Modules) 
  when atom(Module), atom(Pre), atom(Post), list(Depend) ->
    check_module(Module, Modules),
    check_module_depend(Module, Depend, Modules),
    check_module_depend(Module, Depend, updated_modules(AllInstr, [])),
    check_change(Change),
    check_purge(Pre),
    check_purge(Post);

check_instruction(_, Instr, _AllInstr, _Modules) ->
    error({error, {unknown_instruction, Instr}}).


%% If Module X depends on Module Y, then module Y must have an update
%% instruction of some sort (otherwise the depend is faulty).
updated_modules([], Modules) ->
    Modules;
updated_modules([Instr|Instrs], Modules) ->
    Module = instruction_module(Instr),
    updated_modules(Instrs, [Module|Modules]).
    
instruction_module({add_module, Module}) ->
    Module;
instruction_module({remove, {Module, _, _}}) ->
    Module;
instruction_module({load_module, Module, _, _, _}) ->
    Module;
instruction_module({update, Module, _, _, _, _}) ->
    Module;
instruction_module(Instr) ->
    error({error, {unknown_instruction, Instr}}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_version(V) when list(V) ->
    ok;
check_version(V) ->
    error({bad_version, V}).


check_module(M, Modules) when atom(M) ->
    case lists:member(M,Modules) of
        true ->
            ok;
        false ->
            error({unknown_module, M, Modules})
    end;
check_module(M, _) ->
    error({bad_module, M}).


check_module_depend(M, [], _) when atom(M) ->
    ok;
check_module_depend(M, Deps, Modules) when atom(M), list(Deps) ->
    case [Dep || Dep <- Deps, lists:member(Dep, Modules) == false] of
        [] ->
            ok;
        Unknown ->
            error({unknown_depend_modules, Unknown})
    end;
check_module_depend(M, D, Modules) ->
    error({bad_depend, D}).


check_no_remove_depends(Module, []) ->
    ok;
check_no_remove_depends(Module, [Instr|Instrs]) ->
    check_no_remove_depend(Module, Instr),
    check_no_remove_depends(Module, Instrs).

check_no_remove_depend(Module, {load_module, Mod, _Pre, _Post, Depend}) ->
    case lists:member(Module, Depend) of
	true ->
	    error({removed_module_in_depend, load_module, Mod, Module});
	false ->
	    ok
    end;
check_no_remove_depend(Module, {update, Mod, _Change, _Pre, _Post, Depend}) ->
    case lists:member(Module, Depend) of
	true ->
	    error({removed_module_in_depend, update, Mod, Module});
	false ->
	    ok
    end;
check_no_remove_depend(_, _) ->
    ok.
    

check_change(soft) ->
    ok;
check_change({advanced, Something}) ->
    ok;
check_change(Change) ->
    error({bad_change, Change}).


check_purge(soft_purge) ->
    ok;
check_purge(brutal_purge) ->
    ok;
check_purge(Purge) ->
    error({bad_purge, Purge}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

error(Reason) ->
    throw({error, Reason}).

fail(Reason) ->
    exit({suite_failed, Reason}).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L});
	{value, {Key, Value}} ->
	    Value
    end.
