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
-module(code_server).

%% This file holds the server part of the code_server.

-behaviour(gen_server).


%% gen_server callback exports

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).

-include_lib("kernel/include/file.hrl").

-import(lists, [foreach/2]).

-record(state,{root,
	       path,
	       moddb,
	       namedb,
	       cache = no_cache,
	       mode=interactive}).


%% -----------------------------------------------------------
%% Init the code_server process.
%% -----------------------------------------------------------

init([Root, Mode]) ->
    process_flag(trap_exit, true),
    IPath = case Mode of
		interactive ->
		    LibDir = filename:append(Root, "lib"),
		    {ok,Dirs} = file:list_dir(LibDir),
		    {Paths,_Libs} = make_path(LibDir, Dirs),
		    ["."|Paths];
		_ ->
		    []
	    end,

    Db = ets:new(code, [private]),
    foreach(fun (M) ->  ets:insert(Db, {M,preloaded}) end, erlang:pre_loaded()),
    foreach(fun (MF) -> ets:insert(Db, MF) end, init:fetch_loaded()),


    Path = add_loader_path(IPath, Mode),
    State0 = #state{root = Root,
		    path = Path,
		    moddb = Db,
		    namedb = init_namedb(Path),
		    mode = Mode},
    
    State = case init:get_argument(code_path_cache) of
		error -> 
		    State0;
		{ok, _} -> 
		    create_cache(State0)
	    end,
    {ok,State}.


code_change(_OldVsn, State, _Extra) ->
    %% I doubt that changing the code for this module will work,
    %% but at least we avoid a compilation warning.
    {ok,State}.

%%
%% The gen_server call back functions.
%%

handle_call({stick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, true, S),S};

handle_call({unstick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, false, S),S};

handle_call({stick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, true, S),S};

handle_call({unstick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, false, S),S};

handle_call({dir,Dir},{_From,_Tag}, S) ->
    Root = S#state.root,
    Resp = do_dir(Root,Dir,S#state.namedb),
    {reply,Resp,S};

handle_call({load_file,Mod},{_From,_Tag}, S) ->
    case modp(Mod) of
	false ->
	    {reply,{error, badarg},S};
	_ ->
	    {St,Status} = load_file(Mod, S),
	    {reply,Status,St}
    end;

handle_call({add_path,Where,Dir}, {_From,_Tag}, S) ->
    {Resp,Path} = add_path(Where, Dir, S#state.path, S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call({add_paths,Where,Dirs}, {_From,_Tag}, S) ->
    {Resp,Path} = add_paths(Where,Dirs,S#state.path,S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call({set_path,PathList}, {_From,_Tag}, S) ->
    Path = S#state.path,
    {Resp, NewPath,NewDb} = set_path(PathList, Path, S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = NewPath, namedb=NewDb})};

handle_call({del_path,Name},{_From,_Tag}, S) ->
    {Resp,Path} = del_path(Name,S#state.path,S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call({replace_path,Name,Dir},{_From,_Tag}, S) ->
    {Resp,Path} = replace_path(Name,Dir,S#state.path,S#state.namedb),
    {reply,Resp,rehash_cache(S#state{path = Path})};

handle_call(rehash, {_From,_Tag}, S0) ->
    S = create_cache(S0),
    {reply,ok,S};

handle_call(get_path, {_From,_Tag}, S) ->
    {reply,S#state.path,S};

%% Messages to load, delete and purge modules/files.
handle_call({load_abs,File,Mod}, {_From,_Tag}, S) ->
    case modp(File) of
	false ->
	    {reply,{error,badarg},S};
	_ ->
	    Status = load_abs(File,Mod,S#state.moddb),
	    {reply,Status,S}
    end;

handle_call({load_binary,Mod,File,Bin}, {_From,_Tag}, S) ->
    Status = do_load_binary(Mod,File,Bin,S#state.moddb),
    {reply,Status,S};

handle_call({ensure_loaded,Mod0}, {_From,_Tag}, St0) ->
    Fun = fun (M, St) ->
		  case erlang:module_loaded(M) of
		      true ->
			  {St, {module,M}};
		      false when St#state.mode =:= interactive ->
			  load_file(M, St);
		      false -> 
			  {St, {error,embedded}}
		  end
	  end,
    do_mod_call(Fun, Mod0, {error,badarg}, St0);

handle_call({delete,Mod0}, {_From,_Tag}, S) ->
    Fun = fun (M, St) ->
		  case catch erlang:delete_module(M) of
		      true ->
			  ets:delete(St#state.moddb, M),
			  {St, true};
		      _ -> 
			  {St, false}
		  end
	  end,
    do_mod_call(Fun, Mod0, false, S);

handle_call({purge,Mod0}, {_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St, do_purge(M)} end, Mod0, false, St0);

handle_call({soft_purge,Mod0},{_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St,do_soft_purge(M)} end, Mod0, true, St0);

handle_call({is_loaded,Mod0},{_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) -> {St, is_loaded(M, St#state.moddb)} end, Mod0, false, St0);

handle_call(all_loaded, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply,all_loaded(Db),S};

handle_call({get_object_code,Mod0}, {_From,_Tag}, St0) ->
    Fun = fun(M, St) ->
		  Path = St#state.path,
		  case mod_to_bin(Path, atom_to_list(M)) of
		      {_,Bin,FName} -> {St,{M,Bin,FName}};
		      Error -> {St,Error}
		  end
	  end,
    do_mod_call(Fun, Mod0, error, St0);

handle_call({is_sticky, Mod}, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply, is_sticky(Mod,Db), S};

handle_call(stop,{_From,_Tag}, S) ->
    {stop,normal,stopped,S};

handle_call(Other,{_From,_Tag}, S) ->			
    error_logger:error_msg(" ** Codeserver*** ignoring ~w~n ",[Other]),
    {noreply,S}.

handle_cast(_,S) ->
    {noreply,S}.
handle_info(_,S) ->
    {noreply,S}.

terminate(_Reason,_) ->
    ok.

do_mod_call(Action, Module, _Error, St0) when is_atom(Module) ->
    {St, Res} = Action(Module, St0),
    {reply,Res,St};
do_mod_call(Action, Module, Error, St0) ->
    case catch list_to_atom(Module) of
	{'EXIT',_} ->
	    {reply,Error,St0};
	Atom when atom(Atom) ->
	    {St, Res} = Action(Atom, St0),
	    {reply,Res,St}
    end.

%% --------------------------------------------------------------
%% Cache functions 
%% --------------------------------------------------------------

create_cache(St = #state{cache = no_cache}) ->
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St);
create_cache(St) ->
    rehash_cache(St).

rehash_cache(St = #state{cache = no_cache}) ->
    St;
rehash_cache(St = #state{cache = OldCache}) ->
    ets:delete(OldCache), 
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St).

rehash_cache(Cache, St = #state{path = Path}) ->
    Ext = code_aux:objfile_extension(),
    {Cache,NewPath} = locate_mods(lists:reverse(Path), Ext, Cache, []),
    St#state{cache = Cache, path=NewPath}.

locate_mods([Dir0|Path], Ext, Cache, Acc) ->
    Dir = filename:absname(Dir0), %% Cache always expands the path 
    case file:list_dir(Dir) of
	{ok, Files} -> 
	    Cache = filter_mods(Files, Ext, Dir, Cache),
	    locate_mods(Path, Ext, Cache, [Dir|Acc]);
	{error, _} ->
	    locate_mods(Path, Ext, Cache, Acc)
    end;
locate_mods([], _, Cache, Path) ->
    {Cache,Path}.

filter_mods([File|Rest], Ext, Dir, Cache) ->
    case filename:extension(File) of
	Ext ->
	    Mod = list_to_atom(filename:rootname(File, Ext)),
	    true = ets:insert(Cache, {Mod, Dir}),
	    filter_mods(Rest, Ext, Dir, Cache);
	_ ->
	    filter_mods(Rest, Ext, Dir, Cache)
    end;
filter_mods([], _, _, Cache) ->
    Cache.

%% --------------------------------------------------------------
%% Path handling functions.
%% --------------------------------------------------------------

%%
%% Create the initial path. 
%%
make_path(BundleDir,Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir,Bundles,[],[]).

choose_bundles(Bundles) ->
    Bs = lists:sort(lists:map(fun(B) -> cr_b(B) end,Bundles)),
    lists:map(fun({_Name,_NumVsn,FullName}) -> FullName end,
	      choose(lists:reverse(Bs),[])).

cr_b(FullName) ->
    case split(FullName, "-") of
	Toks when length(Toks) > 1 ->
	    VsnStr = lists:last(Toks),
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
		    Name = join(lists:sublist(Toks,length(Toks)-1),"-"),
		    {Name,VsnNum,FullName};
		_ ->
		    {FullName, [0], FullName}
	    end;
	_ ->
	    {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
	true ->
	    {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
	_  ->
	    false
    end.

is_vsn(Str) when list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns);
is_vsn(_) ->
    false.

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> 
		      true; 
		  (_) -> false end, Cs).

split(Cs, S) ->
    string:tokens(Cs, S).
    
join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].


choose([{Name,NumVsn,FullName}|Bs],Ack) ->
    case lists:keymember(Name,1,Ack) of
	true ->
	    choose(Bs,Ack);
	_ ->
	    choose(Bs,[{Name,NumVsn,FullName}|Ack])
    end;
choose([],Ack) ->
    Ack.

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Bin = filename:append(Dir,"ebin"),
    %% First try with /ebin otherwise just add the dir
    case file:read_file_info(Bin) of
	{ok, #file_info{type=directory}} -> 
	    make_path(BundleDir,Tail,[Bin|Res],[Bundle|Bs]);
	_ ->
	    case file:read_file_info(Dir) of
		{ok,#file_info{type=directory}} ->
		    make_path(BundleDir,Tail,
			      [Dir|Res],[Bundle|Bs]);
		_ ->
		    make_path(BundleDir,Tail,Res,Bs)
	    end
    end.



%%
%% Add the erl_prim_loader path.
%% 
%%
add_loader_path(IPath,Mode) ->
    {ok,P0} = erl_prim_loader:get_path(),
    case Mode of
        embedded ->
            strip_path(P0,Mode);  %% i.e. only normalize
        _ ->
            Pa = get_arg(pa),
            Pz = get_arg(pz),
            P = exclude_pa_pz(P0,Pa,Pz),
            Path0 = strip_path(P,Mode),
            Path = add(Path0,IPath,[]),
            add_pa_pz(Path,Pa,Pz)
    end.

%% As the erl_prim_loader path includes the -pa and -pz
%% directories they have to be removed first !!
exclude_pa_pz(P0,Pa,Pz) ->
    P1 = excl(Pa, P0),
    P = excl(Pz, lists:reverse(P1)),
    lists:reverse(P).

excl([], P) -> 
    P;
excl([D|Ds], P) ->
    excl(Ds, lists:delete(D, P)).

%%
%% Keep only 'valid' paths in code server.
%% Only if mode is interactive, in an embedded
%% system we can't rely on file.
%%
strip_path([P0|Ps], embedded) ->
    P = filename:join([P0]), % Normalize
    [P|strip_path(Ps, embedded)];
strip_path([P0|Ps], I) ->
    P = filename:join([P0]), % Normalize
    case check_path([P]) of
	true ->
	    [P|strip_path(Ps, I)];
	_ ->
	    strip_path(Ps, I)
    end;
strip_path(_, _) ->
    [].
    
%%
%% Add only non-existing paths.
%% Also delete other versions of directories,
%% e.g. .../test-3.2/ebin should exclude .../test-*/ebin (and .../test/ebin).
%% Put the Path directories first in resulting path.
%%
add(Path,["."|IPath],Ack) ->
    RPath = add1(Path,IPath,Ack),
    ["."|lists:delete(".",RPath)];
add(Path,IPath,Ack) ->
    add1(Path,IPath,Ack).

add1([P|Path],IPath,Ack) ->
    case lists:member(P,Ack) of
	true ->
	    add1(Path,IPath,Ack); % Already added
	_ ->
	    IPath1 = exclude(P,IPath),
	    add1(Path,IPath1,[P|Ack])
    end;
add1(_,IPath,Ack) ->
    lists:reverse(Ack) ++ IPath.

add_pa_pz(Path0, Patha, Pathz) ->
    {_,Path1} = add_paths(first,Patha,Path0,false),
    {_,Path2} = add_paths(first,Pathz,lists:reverse(Path1),false),
    lists:reverse(Path2).

get_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok, Values} ->
	    lists:append(Values);
	_ ->
	    []
    end.

%%
%% Exclude other versions of Dir or duplicates.
%% Return a new Path.
%%
exclude(Dir,Path) ->
    Name = get_name(Dir),
    lists:filter(fun(D) when D == Dir ->
			 false;
		    (D) ->
			 case get_name(D) of
			     Name ->
				 false; % exclude this dir !
			     _ ->
				 true
			 end
		 end, Path).

%%
%% Get the "Name" of a directory. A directory in the code server path
%% have the following form: .../Name-Vsn or .../Name
%% where Vsn is any sortable term (the newest directory is sorted as
%% the greatest term).
%%
%%
get_name(Dir) ->
    get_name2(get_name1(Dir), []).

get_name1(Dir) ->
    case lists:reverse(filename:split(Dir)) of
	["ebin",DirName|_] -> DirName;
	[DirName|_]        -> DirName;
	_                  -> ""        % No name !
    end.

get_name2([$-|_],Ack) -> lists:reverse(Ack);
get_name2([H|T],Ack)  -> get_name2(T,[H|Ack]);
get_name2(_,Ack)      -> lists:reverse(Ack).

check_path([]) -> 
    true;
check_path([Dir |Tail]) ->
    case catch file:read_file_info(Dir) of
	{ok, #file_info{type=directory}} -> 
	    check_path(Tail);
	_ -> 
	    {error, bad_directory}
    end;
check_path(_) ->
    {error, bad_path}.


%%
%% Add new path(s).
%%
add_path(Where,Dir,Path,NameDb) when atom(Dir) ->
    add_path(Where,atom_to_list(Dir),Path,NameDb);
add_path(Where,Dir0,Path,NameDb) when list(Dir0) ->
    case int_list(Dir0) of
	true ->
	    Dir = filename:join([Dir0]), % Normalize
	    case check_path([Dir]) of
		true ->
		    {true, do_add(Where,Dir,Path,NameDb)};
		Error ->
		    {Error, Path}
	    end;
	_ ->
	    {{error, bad_directory}, Path}
    end;
add_path(_,_,Path,_) ->
    {{error, bad_directory}, Path}.


%%
%% If the new directory is added first or if the directory didn't exist
%% the name-directory table must be updated.
%% If NameDb is false we should NOT update NameDb as it is done later
%% then the table is created :-)
%%
do_add(first,Dir,Path,NameDb) ->
    update(Dir,NameDb),
    [Dir|lists:delete(Dir,Path)];
do_add(last,Dir,Path,NameDb) ->
    case lists:member(Dir,Path) of
	true ->
	    Path;
	_ ->
	    maybe_update(Dir,NameDb),
	    Path ++ [Dir]
    end.

%% Do not update if the same name already exists !
maybe_update(Dir,NameDb) ->
    case lookup_name(get_name(Dir),NameDb) of
        false -> update(Dir,NameDb);
        _     -> false
    end.

update(_Dir, false) ->
    ok;
update(Dir,NameDb) ->
    replace_name(Dir,NameDb).



%%
%% Set a completely new path.
%%
set_path(NewPath0, OldPath, NameDb) ->
    NewPath = normalize(NewPath0),
    case check_path(NewPath) of
	true ->
	    ets:delete(NameDb),
	    NewDb = init_namedb(NewPath),
	    {true, NewPath, NewDb};
	Error ->
	    {Error, OldPath, NameDb}
    end.

%%
%% Normalize the given path.
%% The check_path function catches erroneous path,
%% thus it is ignored here.
%%
normalize([P|Path]) when atom(P) ->
    normalize([atom_to_list(P)|Path]);
normalize([P|Path]) when list(P) ->
    case int_list(P) of
	true -> [filename:join([P])|normalize(Path)];
	_    -> [P|normalize(Path)]
    end;
normalize([P|Path]) ->
    [P|normalize(Path)];
normalize([]) ->
    [];
normalize(Other) ->
    Other.

%% Handle a table of name-directory pairs.
%% The priv_dir/1 and lib_dir/1 functions will have
%% an O(1) lookup.
init_namedb(Path) ->
    Db = ets:new(code_names,[private]),
    init_namedb(lists:reverse(Path), Db),
    Db.
    
init_namedb([P|Path], Db) ->
    insert_name(P, Db),
    init_namedb(Path, Db);
init_namedb([], _) ->
    ok.

-ifdef(NOTUSED).
clear_namedb([P|Path], Db) ->
    delete_name_dir(P, Db),
    clear_namedb(Path, Db);
clear_namedb([], _) ->
    ok.
-endif.

insert_name(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name -> insert_name(Name, Dir, Db)
    end.

insert_name(Name, Dir, Db) ->
    ets:insert(Db, {Name, del_ebin(Dir)}),
    true.



%%
%% Delete a directory from Path.
%% Name can be either the the name in .../Name[-*] or
%% the complete directory name.
%%
del_path(Name0,Path,NameDb) ->
    case catch code_aux:to_list(Name0)of
	{'EXIT',_} ->
	    {{error,bad_name},Path};
	Name ->
	    case del_path1(Name,Path,NameDb) of
		Path -> % Nothing has changed
		    {false,Path};
		NewPath ->
		    {true,NewPath}
	    end
    end.

del_path1(Name,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    delete_name(Name, NameDb),
	    insert_old_shadowed(Name, Path, NameDb),
	    Path;
	_ when Name == P ->
	    case delete_name_dir(Name, NameDb) of
		true -> insert_old_shadowed(get_name(Name), Path, NameDb);
		false -> ok
	    end,
	    Path;
	_ ->
	    [P|del_path1(Name,Path,NameDb)]
    end;
del_path1(_,[],_) ->
    [].

insert_old_shadowed(Name, [P|Path], NameDb) ->
    case get_name(P) of
	Name -> insert_name(Name, P, NameDb);
	_    -> insert_old_shadowed(Name, Path, NameDb)
    end;
insert_old_shadowed(_, [], _) ->
    ok.

%%
%% Replace an old occurrence of an directory with name .../Name[-*].
%% If it does not exist, put the new directory last in Path.
%%
replace_path(Name,Dir,Path,NameDb) ->
    case catch check_pars(Name,Dir) of
	{ok,N,D} ->
	    {true,replace_path1(N,D,Path,NameDb)};
	{'EXIT',_} ->
	    {{error,{badarg,[Name,Dir]}},Path};
	Error ->
	    {Error,Path}
    end.

replace_path1(Name,Dir,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    insert_name(Name, Dir, NameDb),
	    [Dir|Path];
	_ ->
	    [P|replace_path1(Name,Dir,Path,NameDb)]
    end;
replace_path1(Name, Dir, [], NameDb) ->
    insert_name(Name, Dir, NameDb),
    [Dir].

check_pars(Name,Dir) ->
    N = code_aux:to_list(Name),
    D = filename:join([code_aux:to_list(Dir)]), % Normalize
    case get_name(Dir) of
	N ->
	    case check_path([D]) of
		true ->
		    {ok,N,D};
		Error ->
		    Error
	    end;
	_ ->
	    {error,bad_name}
    end.


del_ebin(Dir) ->
    case filename:basename(Dir) of
	"ebin" -> filename:dirname(Dir);
	_      -> Dir
    end.



replace_name(Dir, Db) ->
    case get_name(Dir) of
	Dir ->
	    false;
	Name ->
	    delete_name(Name, Db),
	    insert_name(Name, Dir, Db)
    end.

delete_name(Name, Db) ->
    ets:delete(Db, Name).

delete_name_dir(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name ->
	    Dir0 = del_ebin(Dir),
	    case lookup_name(Name, Db) of
		{ok, Dir0} ->
		    ets:delete(Db, Name), 
		    true;
		_ -> false
	    end
    end.

lookup_name(Name, Db) ->
    case ets:lookup(Db, Name) of
	[{Name, Dir}] -> {ok, Dir};
	_             -> false
    end.


%%
%% Fetch a directory.
%%
do_dir(Root,lib_dir,_) ->
    filename:append(Root, "lib");
do_dir(Root,root_dir,_) ->
    Root;
do_dir(_Root,compiler_dir,NameDb) ->
    case lookup_name("compiler", NameDb) of
	{ok, Dir} -> Dir;
	_         -> ""
    end;
do_dir(_Root,{lib_dir,Name},NameDb) ->
    case catch lookup_name(code_aux:to_list(Name), NameDb) of
	{ok, Dir} -> Dir;
	_         -> {error, bad_name}
    end;
do_dir(_Root,{priv_dir,Name},NameDb) ->
    case catch lookup_name(code_aux:to_list(Name), NameDb) of
	{ok, Dir} -> filename:append(Dir, "priv");
	_         -> {error, bad_name}
    end;
do_dir(_, _, _) ->
    'bad request to code'.

stick_dir(Dir, Stick, St) ->
    case file:list_dir(Dir) of
	{ok,Listing} ->
	    Mods = get_mods(Listing, code_aux:objfile_extension()),
	    Db = St#state.moddb,
	    case Stick of
		true ->
		    foreach(fun (M) -> ets:insert(Db, {{sticky,M},true}) end, Mods);
		false ->
		    foreach(fun (M) -> ets:delete(Db, {sticky,M}) end, Mods)
	    end;
	Error -> Error
    end.

stick_mod(M, Stick, St) ->
  Db = St#state.moddb,
  case Stick of
    true ->
      ets:insert(Db, {{sticky,M},true});
    false ->
      ets:delete(Db, {sticky,M})
  end.

get_mods([File|Tail], Extension) ->
    case filename:extension(File) of
	Extension ->
	    [list_to_atom(filename:basename(File, Extension)) |
	     get_mods(Tail, Extension)];
	_ ->
	    get_mods(Tail, Extension)
    end;
get_mods([], _) -> [].

is_sticky(Mod, Db) ->
    case erlang:module_loaded(Mod) of
	true ->
	    case ets:lookup(Db, {sticky,Mod}) of
		[] -> false;
		_  -> true
	    end;
	_ -> false
    end.

add_paths(Where,[Dir|Tail],Path,NameDb) ->
    {_,NPath} = add_path(Where,Dir,Path,NameDb),
    add_paths(Where,Tail,NPath,NameDb);
add_paths(_,_,Path,_) ->
    {ok,Path}.


do_load_binary(Module,File,Binary,Db) ->
    case {modp(Module),modp(File)} of
	{true, true} when binary(Binary) ->
	    case erlang:module_loaded(code_aux:to_atom(Module)) of
		true ->
		    code_aux:do_purge(Module);
		false ->
		    ok
	    end,
	    try_load_module(File, Module, Binary, Db);
	_ ->
	    {error, badarg}
    end.

modp(Atom) when atom(Atom) -> true;
modp(List) when list(List) -> int_list(List);
modp(_)                    -> false.


load_abs(File, Mod0, Db) ->
    Ext = code_aux:objfile_extension(),
    FileName0 = lists:concat([File, Ext]),
    FileName = filename:absname(FileName0),
    Mod = if Mod0 == [] ->
		  list_to_atom(filename:basename(FileName0, Ext));
	     true ->
		  Mod0
	  end,
    case erl_prim_loader:get_file(FileName) of
	{ok,Bin,_} ->
	    try_load_module(FileName, Mod, Bin, Db);
	error ->
	    {error,nofile}
    end.

try_load_module(Mod, Dir, Db) ->
    File = filename:append(Dir, code_aux:to_path(Mod) ++ 
			   code_aux:objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    %% No cache case tries with erl_prim_loader here
	    %% Should the caching code do it??
%           File2 = code_aux:to_path(Mod) ++ code_aux:objfile_extension(),
% 	    case erl_prim_loader:get_file(File2) of
% 		error -> 
	    error;     % No more alternatives !
% 		{ok,Bin,FName} ->
% 		    	    try_load_module(absname(FName), Mod, Binary, Db)
% 	    end;
	{ok,Binary,FName} ->
	    try_load_module(absname(FName), Mod, Binary, Db)
    end.

try_load_module(File, Mod, Bin, Db) ->
    M = code_aux:to_atom(Mod),

    case is_sticky(M, Db) of
	true ->                         %% Sticky file reject the load
	    error_logger:error_msg("Can't load module that resides in sticky dir\n",[]),
	    {error, sticky_directory};
	false ->
	case catch load_native_code(Mod, Bin) of
	  {module,M} ->
	    ets:insert(Db, {M,File}),
	    {module,Mod};
	  no_native ->
	    case erlang:load_module(M, Bin) of
	      {module,M} ->
		ets:insert(Db, {M,File}),
		{module,Mod};
	      {error,What} ->
		error_logger:error_msg("Loading of ~s failed: ~p\n", [File, What]),
		{error,What}
	    end;
	  Error ->
	   error_logger:error_msg("Native loading of ~s failed: ~p\n", [File, Error])
	end
    end.

load_native_code(Mod, Bin) ->
    %% During bootstrapping of Open Source Erlang, we don't have any hipe
    %% loader modules, but the Erlang emulator might be hipe enabled.
    %% Therefore we must test for that the loader modules are available
    %% before trying to to load native code.
    case erlang:module_loaded(hipe_unified_loader) of
	false -> no_native;
	true -> load_native_code_1(Mod, Bin)
    end.

load_native_code_1(Mod, Bin) ->
    %% At this stage, we know that the hipe loader modules are loaded.
    case erlang:system_info(hipe_architecture) of
	ultrasparc ->
	    hipe_unified_loader:patch_to_emu(Mod),
	    load_native_code(Mod, Bin, "HS8P",
			     fun(M, B) -> 
				     hipe_unified_loader:load_module(M, B, Bin) 
			     end);
	x86 ->
	    hipe_unified_loader:patch_to_emu(Mod),
	    load_native_code(Mod, Bin, "HX86",
			     fun(M, B) -> 
				     hipe_unified_loader:load_module(M, B, Bin) 
			     end);
	_ ->
	    %% Can't happen (in principle).
	    no_native
    end.


load_native_code(Mod, Bin, ChunkTag, Loader) ->
    case code:get_chunk(Bin, ChunkTag) of
	undefined -> no_native;
	NativeCode when binary(NativeCode) ->
	    Loader(Mod, NativeCode)
    end.

int_list([H|T]) when integer(H) -> int_list(T);
int_list([_|_])                 -> false;
int_list([])                    -> true.


load_file(Mod, St=#state{path=Path,moddb=Db,cache=no_cache}) ->
    case mod_to_bin(Path, Mod) of
	error -> {St, {error,nofile}};
	{Mod,Binary,File} -> {St,try_load_module(File, Mod, Binary, Db)}
    end;
load_file(Mod, St0=#state{moddb=Db,cache=Cache}) ->
    case ets:lookup(Cache, Mod) of
	[] -> 
	    St = rehash_cache(St0),
	    case ets:lookup(St#state.cache, Mod) of
		[] -> 
		    {St, {error,nofile}};
		[{Mod,Dir}] ->
		    {St, try_load_module(Mod, Dir, Db)}
	    end;
	[{Mod,Dir}] ->
	    {St0, try_load_module(Mod, Dir, Db)}
    end.

mod_to_bin([Dir|Tail], Mod) ->
    File = filename:append(Dir, code_aux:to_path(Mod) ++ code_aux:objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    mod_to_bin(Tail, Mod);
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end;
mod_to_bin([], Mod) ->
    %% At last, try also erl_prim_loader's own method
    File = code_aux:to_path(Mod) ++ code_aux:objfile_extension(),
    case erl_prim_loader:get_file(File) of
	error -> 
	    error;     % No more alternatives !
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end.

absname(File) ->
    case prim_file:get_cwd() of
	{ok,Cwd} -> filename:absname(File, Cwd);
	_Error -> File
    end.

%% do_purge(Module)
%%  Kill all processes running code from *old* Module, and then purge the
%%  module. Return true if any processes killed, else false.

do_purge(Mod) ->
    do_purge(processes(), Mod, false).

do_purge([P|Ps], Mod, Purged) ->
    case erlang:check_process_code(P, Mod) of
	true ->
	    exit(P, kill),
	    do_purge(Ps, Mod, true);
	false ->
	    do_purge(Ps, Mod, Purged)
    end;
do_purge([], Mod, Purged) ->
    catch erlang:purge_module(Mod),
    Purged.

%% do_soft_purge(Module)
%% Purge old code only if no procs remain that run old code
%% Return true in that case, false if procs remain (in this
%% case old code is not purged)

do_soft_purge(Mod) ->
    catch do_soft_purge(processes(), Mod).

do_soft_purge([P|Ps], Mod) ->
    case erlang:check_process_code(P, Mod) of
	true -> throw(false);
	false -> do_soft_purge(Ps, Mod)
    end;
do_soft_purge([], Mod) ->
    catch erlang:purge_module(Mod),
    true.

is_loaded(M, Db) ->
    case ets:lookup(Db, M) of
	[{M,File}] -> {file,File};
	[] -> false
    end.

%% -------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------

all_loaded(Db) ->
    all_l(Db, ets:slot(Db, 0), 1, []).

all_l(_Db, '$end_of_table', _, Acc) ->
    Acc;
all_l(Db, ModInfo, N, Acc) ->
    NewAcc = strip_mod_info(ModInfo,Acc),
    all_l(Db, ets:slot(Db, N), N + 1, NewAcc).


strip_mod_info([{{sticky,_},_}|T], Acc) -> strip_mod_info(T, Acc);
strip_mod_info([H|T], Acc)              -> strip_mod_info(T, [H|Acc]);
strip_mod_info([], Acc)                 -> Acc.
