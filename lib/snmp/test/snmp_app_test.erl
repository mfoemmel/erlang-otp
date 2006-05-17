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
%% Purpose: Verify the application specifics of the snmp application
%%----------------------------------------------------------------------
-module(snmp_app_test).

-compile(export_all).

-include("snmp_test_lib.hrl").


% t()     -> megaco_test_lib:t(?MODULE).
% t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(_Case, Config) ->
    Config.

fin_per_testcase(_Case, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 fields,
	 modules,
	 exportall,
	 app_depend
	],
    {req, [], [{conf, app_init, Cases, app_fin}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_init(suite) -> [];
app_init(doc) -> [];
app_init(Config) when list(Config) ->
    case is_app(snmp) of
	{ok, AppFile} ->
	    io:format("AppFile: ~n~p~n", [AppFile]),
	    print_versions(1, (catch snmp:versions1())),
	    print_versions(2, (catch snmp:versions2())),
	    [{app_file, AppFile}|Config];
	{error, Reason} ->
	    fail(Reason)
    end.

is_app(App) ->
    LibDir = code:lib_dir(App),
    File = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
	{ok, [{application, App, AppFile}]} ->
	    {ok, AppFile};
	Error ->
	    {error, {invalid_format, Error}}
    end.

print_versions(N, {ok, Versions}) ->
    V = lists:flatten(io_lib:format("VERSIONS ~w: ", [N])),
    snmp:print_versions(V, Versions);
print_versions(_, _) ->
    ok.

app_fin(suite) -> [];
app_fin(doc) -> [];
app_fin(Config) when list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fields(suite) ->
    [];
fields(doc) ->
    [];
fields(Config) when list(Config) ->
    AppFile = key1search(app_file, Config),
    Fields = [vsn, description, modules, registered, applications],
    case check_fields(Fields, AppFile, []) of
	[] ->
	    ok;
	Missing ->
	    fail({missing_fields, Missing})
    end.

check_fields([], _AppFile, Missing) ->
    Missing;
check_fields([Field|Fields], AppFile, Missing) ->
    check_fields(Fields, AppFile, check_field(Field, AppFile, Missing)).

check_field(Name, AppFile, Missing) ->
    io:format("checking field: ~p~n", [Name]),
    case lists:keymember(Name, 1, AppFile) of
	true ->
	    Missing;
	false ->
	    [Name|Missing]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modules(suite) ->
    [];
modules(doc) ->
    [];
modules(Config) when list(Config) ->
    AppFile  = key1search(app_file, Config),
    Mods     = key1search(modules, AppFile),
    EbinList = get_ebin_mods(snmp),
    case missing_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Missing ->
	    fail({missing_modules, Missing})
    end,
    Allowed = [snmpc,
	       snmpc_lib,
	       snmpc_misc,
	       snmpc_mib_gram,
	       snmpc_mib_to_hrl,
	       snmpc_tok],
    case extra_modules(Mods, EbinList, Allowed, []) of
	[] ->
	    ok;
	Extra ->
	    fail({extra_modules, Extra})
    end,
    {ok, Mods}.
	    
get_ebin_mods(App) ->
    LibDir  = code:lib_dir(App),
    EbinDir = filename:join([LibDir,"ebin"]),
    {ok, Files0} = file:list_dir(EbinDir),
    Files1 = [lists:reverse(File) || File <- Files0],
    [list_to_atom(lists:reverse(Name)) || [$m,$a,$e,$b,$.|Name] <- Files1].
    

missing_modules([], _Ebins, Missing) ->
    Missing;
missing_modules([Mod|Mods], Ebins, Missing) ->
    case lists:member(Mod, Ebins) of
	true ->
	    missing_modules(Mods, Ebins, Missing);
	false ->
	    io:format("missing module: ~p~n", [Mod]),
	    missing_modules(Mods, Ebins, [Mod|Missing])
    end.


extra_modules(_Mods, [], Allowed, Extra) ->
    Extra--Allowed;
extra_modules(Mods, [Mod|Ebins], Allowed, Extra) ->
    case lists:member(Mod, Mods) of
	true ->
	    extra_modules(Mods, Ebins, Allowed, Extra);
	false ->
	    io:format("superfluous module: ~p~n", [Mod]),
	    extra_modules(Mods, Ebins, Allowed, [Mod|Extra])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exportall(suite) ->
    [];
exportall(doc) ->
    [];
exportall(Config) when list(Config) ->
    AppFile = key1search(app_file, Config),
    Mods    = key1search(modules, AppFile),
    check_export_all(Mods).


check_export_all([]) ->
    ok;
check_export_all([Mod|Mods]) ->
    case (catch apply(Mod, module_info, [compile])) of
	{'EXIT', {undef, _}} ->
	    check_export_all(Mods);
	O ->
            case lists:keysearch(options, 1, O) of
                false ->
                    check_export_all(Mods);
                {value, {options, List}} ->
                    case lists:member(export_all, List) of
                        true ->
			    fail({export_all, Mod});
			false ->
			    check_export_all(Mods)
                    end
            end
    end.

	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_depend(suite) ->
    [];
app_depend(doc) ->
    [];
app_depend(Config) when list(Config) ->
    AppFile = key1search(app_file, Config),
    Apps    = key1search(applications, AppFile),
    check_apps(Apps).


check_apps([]) ->
    ok;
check_apps([App|Apps]) ->
    case is_app(App) of
	{ok, _} ->
	    check_apps(Apps);
	Error ->
	    throw({error, {missing_app, {App, Error}}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fail(Reason) ->
    exit({suite_failed, Reason}).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L});
	{value, {Key, Value}} ->
	    Value
    end.
