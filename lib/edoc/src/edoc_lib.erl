%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: edoc_lib.erl,v 1.26 2004/04/05 17:12:39 richardc Exp $
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Utility functions for EDoc.

-module(edoc_lib).

-export([count/2, split/2, split_at_stop/1, filename/1, transpose/1,
	 segment/2, get_first_sentence/1, strip_space/1, escape_uri/1,
	 join_uri/2, is_name/1, find_sources/2, find_sources/3,
	 find_sources/5, find_file/3, try_subdir/2, unique/1,
	 write_file/3, write_file/4, write_info_file/4,
	 read_info_file/1, get_info_files/1, get_doc_env/1,
	 get_doc_env/4, copy_file/2]).

-import(edoc_report, [report/2, warning/2]).

-include("edoc.hrl").
-include("xmerl.hrl").


count(X, Xs) ->
    count(X, Xs, 0).

count(X, [X | Xs], N) ->
    count(X, Xs, N + 1);
count(X, [_ | Xs], N) ->
    count(X, Xs, N);
count(_X, [], N) ->
    N.

split(Cs, K) ->
    split(Cs, K, [], []).

split([K | Cs], K, As, Ls) ->
    split(Cs, K, [], [lists:reverse(As) | Ls]);
split([C | Cs], K, As, Ls) ->
    split(Cs, K, [C | As], Ls);
split([], _K, As, Ls) ->
    lists:reverse([lists:reverse(As) | Ls]).

split_at_stop(Cs) ->
    split_at_stop(Cs, []).

split_at_stop([$., $\s | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$., $\t | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$., $\n | Cs], As) ->
    {lists:reverse(As), Cs};
split_at_stop([$.], As) ->
    {lists:reverse(As), []};
split_at_stop([C | Cs], As) ->
    split_at_stop(Cs, [C | As]);
split_at_stop([], As) ->
    {lists:reverse(As), []}.

strip_space([$\s | Cs]) -> strip_space(Cs);
strip_space([$\t | Cs]) -> strip_space(Cs);
strip_space([$\n | Cs]) -> strip_space(Cs);
strip_space(Cs) -> Cs.

segment(Es, N) ->
    segment(Es, [], [], 0, N).

segment([E | Es], As, Cs, N, M) when N < M ->
    segment(Es, [E | As], Cs, N + 1, M);
segment([_ | _] = Es, As, Cs, _N, M) ->
    segment(Es, [], [lists:reverse(As) | Cs], 0, M);
segment([], [], Cs, _N, _M) ->
    lists:reverse(Cs);
segment([], As, Cs, _N, _M) ->
    lists:reverse([lists:reverse(As) | Cs]).

transpose([]) -> [];
transpose([[] | Xss]) -> transpose(Xss);
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _T] <- Xss]]
     | transpose([Xs | [T || [_H | T] <- Xss]])].

%% This is a conservative URI escaping, which escapes anything that may
%% not appear in an NMTOKEN ([a-zA-Z0-9]|'.'|'-'|'_'), including ':'.
%% Characters are first encoded in UTF-8.

escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C > 16#7f ->
    %% This assumes that characters are at most 16 bits wide.
    %% TODO: general utf-8 encoding for all of Unicode (0-16#10ffff)
    escape_byte(((C band 16#c0) bsr 6) + 16#c0)
	++ escape_byte(C band 16#3f + 16#80)
	++ escape_uri(Cs);
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

escape_byte(C) ->
    "%" ++ hex_octet(C).

% utf8([C | Cs]) when C > 16#7f ->
%     [((C band 16#c0) bsr 6) + 16#c0, C band 16#3f ++ 16#80 | utf8(Cs)];
% utf8([C | Cs]) ->
%     [C | utf8(Cs)];
% utf8([]) ->
%     [].

hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    hex_octet(N bsr 4) ++ hex_octet(N band 15);
hex_octet(N) ->
    [N - 10 + $a].

%% Please note that URI are not file names. Don't use the stdlib
%% 'filename' module for operations on (parts of) URI.

join_uri(Base, "") ->
    Base;
join_uri("", Path) ->
    Path;
join_uri(Base, Path) ->
    Base ++ "/" ++ Path.

%% Note that the parser will not produce two adjacent text segments;
%% thus, if a text segment ends with a period character, it marks the
%% end of the summary sentence only if it is also the last segment in
%% the list.

get_first_sentence([E = #xmlText{value = Txt}]) ->
    {_, Txt1} = end_of_sentence(Txt),
    [E#xmlText{value = Txt1}];
get_first_sentence([E = #xmlText{value = Txt} | Es]) ->
    case end_of_sentence(Txt) of
	{true, Txt1} ->
	    [E#xmlText{value = Txt1}];
	{false, _} ->
	    [E | get_first_sentence(Es)]
    end;
get_first_sentence([E | Es]) ->
    % Skip non-text segments - don't descend
    [E | get_first_sentence(Es)];
get_first_sentence([]) ->
    [].

end_of_sentence(Cs) ->
    end_of_sentence(Cs, []).

%% We detect '.' and '!' as end-of-sentence markers.

end_of_sentence([C=$., $\s | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$., $\t | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$., $\n | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$.], As) ->
    end_of_sentence(false, C, As);
end_of_sentence([C=$!, $\s | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$!, $\t | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$!, $\n | _], As) ->
    end_of_sentence(true, C, As);
end_of_sentence([C=$!], As) ->
    end_of_sentence(false, C, As);
end_of_sentence([C | Cs], As) ->
    end_of_sentence(Cs, [C | As]);
end_of_sentence([], As) ->
    end_of_sentence(false, $., strip_space(As)).  % add a '.'

end_of_sentence(Flag, C, As) ->
    {Flag, lists:reverse([C | As])}.


%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase

%% Names must begin with a lowercase letter and contain only
%% alphanumerics and underscores.

is_name([C | Cs]) when C >= $a, C =< $z ->
    is_name_1(Cs);
is_name([C | Cs]) when C >= $\337, C =< $\377, C =/= $\367 ->
    is_name_1(Cs);
is_name(_) -> false.

is_name_1([C | Cs]) when C >= $a, C =< $z ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $A, C =< $Z ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $0, C =< $9 ->
    is_name_1(Cs);
is_name_1([C | Cs]) when C >= $\300, C =< $\377, C =/= $\327, C =/= $\367 ->
    is_name_1(Cs);
is_name_1([$_ | Cs]) ->
    is_name_1(Cs);
is_name_1([]) -> true;
is_name_1(_) -> false.

to_atom(A) when atom(A) -> A;
to_atom(S) when list(S) -> list_to_atom(S).
    
unique([X | Xs]) -> [X | unique(Xs, X)];
unique([]) -> [].

unique([X | Xs], X) -> unique(Xs, X);
unique([X | Xs], _) -> [X | unique(Xs, X)];
unique([], _) -> [].

% key_unique([X | Xs]) -> [X | key_unique(Xs, element(1,X))];
% key_unique([]) -> [].

% key_unique([X | Xs], K) when element(1,X) =:= K -> key_unique(Xs, K);
% key_unique([X | Xs], _) -> [X | key_unique(Xs, element(1,X))];
% key_unique([], _) -> [].


%% ---------------------------------------------------------------------
%% Files

filename([C | T]) when integer(C), C > 0 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when atom(N) ->
    atom_to_list(N);
filename(N) ->
    report("bad filename: `~P'.", [N, 25]),
    exit(error).

list_dir(Dir) ->
    case file:list_dir(Dir) of
	{ok, Fs} ->
	    Fs;
	{error, _} ->
	    warning("could not read directory '~s'.", [filename(Dir)]),
	    []
    end.

find_sources(Path, Opts) ->
    find_sources(Path, "", Opts).

find_sources(Path, Pkg, Opts) ->
    Rec = proplists:get_bool(recursive, Opts),
    Ext = proplists:get_value(source_suffix, Opts, ?DEFAULT_SOURCE_SUFFIX),
    find_sources(Path, Pkg, Rec, Ext, Opts).

find_sources(Path, Pkg, Rec, Ext, Opts) ->
    Skip = proplists:get_value(exclude_packages, Opts, []),
    lists:flatten(find_sources_1(Path, to_atom(Pkg), Rec, Ext, Skip)).

find_sources_1([P | Ps], Pkg, Rec, Ext, Skip) ->
    Dir = filename:join(P, filename:join(packages:split(Pkg))),
    Fs1 = find_sources_1(Ps, Pkg, Rec, Ext, Skip),
    case filelib:is_dir(Dir) of
	true ->
	    [find_sources_2(Dir, Pkg, Rec, Ext, Skip) | Fs1];
	false ->
	    Fs1
    end;
find_sources_1([], _Pkg, _Rec, _Ext, _Skip) ->
    [].

find_sources_2(Dir, Pkg, Rec, Ext, Skip) ->
    case lists:member(Pkg, Skip) of
	false ->
	    Es = list_dir(Dir),
	    Es1 = [{Pkg, E, Dir} || E <- Es, is_source_file(E, Ext)],
	    case Rec of
		true ->
		    [find_sources_3(Es, Dir, Pkg, Rec, Ext, Skip) | Es1];
		false ->
		    Es1
	    end;
	true ->
	    []
    end.

find_sources_3(Es, Dir, Pkg, Rec, Ext, Skip) ->
    [find_sources_2(filename:join(Dir, E),
		    to_atom(packages:concat(Pkg, E)), Rec, Ext, Skip)
     || E <- Es, is_package_dir(E, Dir)].

is_source_file(Name, Ext) ->
    (filename:extension(Name) == Ext)
	andalso is_name(filename:rootname(Name, Ext)).

is_package_dir(Name, Dir) ->
    is_name(filename:rootname(filename:basename(Name)))
	andalso filelib:is_dir(filename:join(Dir, Name)).

find_file([P | Ps], Pkg, Name) ->
    Dir = filename:join(P, filename:join(packages:split(Pkg))),
    File = filename:join(Dir, Name),
    case filelib:is_file(File) of
	true ->
	    File;    
	false ->
	    find_file(Ps, Pkg, Name)
    end;    
find_file([], _Pkg, _Name) ->
    "".

try_subdir(Dir, Subdir) ->
    D = filename:join(Dir, Subdir),
    case filelib:is_dir(D) of
	true -> D; 
	false -> Dir
    end.

%% @spec (Text::deep_string(), Dir::filename(), Name::filename()) -> ok
%%
%% @doc Write the given `Text' to the file named by `Name' in directory
%% `Dir'. If the target directory does not exist, it will be created.

write_file(Text, Dir, Name) ->
    write_file(Text, Dir, Name, '').


%% @spec (Text::deep_string(), Dir::filename(), Name::filename(),
%%        Package::atom()|string()) -> ok
%% @doc Like {@link write_file/3}, but adds path components to the target
%% directory corresponding to the specified package.

write_file(Text, Dir, Name, Package) ->
    Dir1 = filename:join([Dir | packages:split(Package)]),
    File = filename:join(Dir1, Name),
    filelib:ensure_dir(File),
    {ok, FD} = file:open(File, [write]),
    io:put_chars(FD, Text),
    file:close(FD),
    ok.

write_info_file(App, Packages, Modules, Dir) ->
    Ts = [{packages, Packages},
	  {modules, Modules}],
    Ts1 = if App == ?NO_APP -> Ts;
	     true -> [{application, App} | Ts]
	  end,
    S = [io_lib:fwrite("~p.\n", [T]) || T <- Ts1],
    write_file(S, Dir, ?INFO_FILE).

read_info_file(Dir) ->
    File = filename:join(Dir, ?INFO_FILE),
    case file:consult(File) of
	{ok, Ts} ->
	    App = proplists:get_value(application, Ts, ?NO_APP),
	    Ps = proplists:append_values(packages, Ts),
	    Ms = proplists:append_values(modules, Ts),
	    {ok, {App, Ps, Ms}};
	{error, E} ->
	    {error, File, E}
    end.

find_doc_dirs([P | Ps]) ->
    P1 = case filename:basename(P) of
	     ?EBIN_DIR ->
		 filename:dirname(P);
	     _ ->
		 P
	 end,
    Dir = try_subdir(P1, ?EDOC_DIR),
    File = filename:join(Dir, ?INFO_FILE),
    case filelib:is_file(File) of
	true ->
	    ["file://" ++ Dir | find_doc_dirs(Ps)]; 
	false ->
	    find_doc_dirs(Ps)
    end;
find_doc_dirs([]) ->
    [].

%% All names with "internal linkage" are mapped to the empty string, so
%% that relative references will be created.

get_doc_links(App, Packages, Modules, Opts) ->
    Path = proplists:get_value(doc_path, Opts, [])
	++ find_doc_dirs(code:get_path()),
    Ds = [{P, get_info_file(P)} || P <- Path],
    Ds1 = [{"", {App, Packages, Modules}} | Ds],
    D = dict:new(),
    make_links(Ds1, D, D, D).

%% TODO: info-file paths should be any URI, not just "file://..."!
%% (Must find or implement an easy way to read a file over HTTP.)

get_info_file("file://" ++ Dir) ->
    case read_info_file(Dir) of
	{ok, Info} ->
	    Info;
	{error, File, E} ->
	    warning("could not read file '~s': ~p.", [File, E]),
	    {?NO_APP, [], []}
    end;
get_info_file(URI) ->
    warning("cannot access URI other than 'file://...' yet; ignoring '~s'.",
	    [URI]),
    {?NO_APP, [], []}.

get_info_files([P | Ps]) ->
    case get_info_file(P) of
	{?NO_APP, [], []} ->
	    get_info_files(Ps);
	Info ->
	    [{P, Info} | get_info_files(Ps)]
    end;
get_info_files([]) ->
    [].

make_links([{Dir, {App, Ps, Ms}} | Ds], A, P, M) ->
    A1 = if App == ?NO_APP -> A;
	    true -> add_new(App, Dir, A)
	 end,
    F = fun (K, D) -> add_new(K, Dir, D) end,
    P1 = lists:foldl(F, P, Ps),
    M1 = lists:foldl(F, M, Ms),
    make_links(Ds, A1, P1, M1);
make_links([], A, P, M) ->
    F = fun (D) ->
		fun (K) ->
			case dict:find(K, D) of
			    {ok, V} -> V;
			    error -> ""
			end
		end
	end,
    {F(A), F(P), F(M)}.

add_new(K, V, D) ->
    case dict:is_key(K, D) of
	true ->
	    D;
	false ->
	    dict:store(K, V, D)
    end.

%% @spec (Options::option_list()) -> edoc_env()
%%
%% @see get_doc_env/4

get_doc_env(Opts) ->
    get_doc_env([], [], [], Opts).

%% @spec (App, Packages, Modules, Options::option_list()) -> edoc_env()
%%     App = [] | atom()
%%     Packages = [atom()]
%%     Modules = [atom()]
%%
%% @type edoc_env(). Environment information needed by EDoc for
%% generating references. The data representation is not documented.
%%
%% @doc Creates an environment data structure used by parts of EDoc for
%% generating references, etc.
%% @see edoc_extract:source/4

get_doc_env(App, Packages, Modules, Opts) ->
    Suffix = proplists:get_value(file_suffix, Opts,
				 ?DEFAULT_FILE_SUFFIX),
    AppDefault = proplists:get_value(app_default, Opts, ?APP_DEFAULT),
    {A, P, M} = get_doc_links(App, Packages, Modules, Opts),
    #env{file_suffix = Suffix,
	 package_summary = ?PACKAGE_SUMMARY ++ Suffix,
	 apps = A,
	 packages = P,
	 modules = M,
	 app_default = AppDefault
	}.


copy_file(From, To) ->
    case file:copy(From, To) of
	{ok, _} -> ok;
	{error, R} ->
	    report("error copying '~s' to '~s': ~w.", [From, To, R])
    end.
