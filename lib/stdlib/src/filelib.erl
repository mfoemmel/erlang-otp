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
-module(filelib).

%% File utilities.

-export([wildcard/1, wildcard/2, is_dir/1, is_file/1, is_regular/1, 
	 compile_wildcard/1]).
-export([fold_files/5, last_modified/1, file_size/1, ensure_dir/1]).

-include_lib("kernel/include/file.hrl").

wildcard({compiled_wildcard,{exists,File}}) ->
    case file:read_file_info(File) of
	{ok,_} -> [File];
	_ -> []
    end;
wildcard({compiled_wildcard,[Base|Rest]}) ->
    wildcard_1([Base], Rest);
wildcard(Pattern) when is_list(Pattern) ->
    try
	wildcard(compile_wildcard(Pattern))
    catch
	error:{badpattern,_}=Error ->
	    %% Get the stack backtrace correct.
	    erlang:error(Error)
    end.

wildcard({compiled_wildcard,{exists,File}}, Cwd) ->
    case file:read_file_info(filename:absname(File, Cwd)) of
	{ok,_} -> [File];
	_ -> []
    end;
wildcard({compiled_wildcard,[current|Rest]}, Cwd) ->
    PrefixLen = length(Cwd)+1,
    [lists:nthtail(PrefixLen, N) || N <- wildcard_1([Cwd], Rest)];
wildcard({compiled_wildcard,[Base|Rest]}, _Cwd) ->
    wildcard_1([Base], Rest);
wildcard(Pattern, Cwd) when is_list(Pattern), is_list(Cwd) ->
    try
	wildcard(compile_wildcard(Pattern), Cwd)
    catch
	error:{badpattern,_}=Error ->
	    %% Get the stack backtrace correct.
	    erlang:error(Error)
    end.

is_dir(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type=directory}} ->
	    true;
	_ ->
	    false
    end.

is_file(File) ->
    case file:read_file_info(File) of
	{ok, #file_info{type=regular}} ->
	    true;
	{ok, #file_info{type=directory}} ->
	    true;
        _ ->
            false
    end.

is_regular(File) ->
    case file:read_file_info(File) of
	{ok, #file_info{type=regular}} ->
	    true;
        _ ->
            false
    end.

%% fold_files(Dir, RegExp, Recursive, Fun, AccIn).

%% folds the function Fun(F, Acc) -> Acc1 over
%%   all files <F> in <Dir> that match the regular expression <RegExp>
%%   If <Recursive> is true all sub-directories to <Dir> are processed

fold_files(Dir, RegExp, Recursive, Fun, Acc) ->
    {ok, Re1} = regexp:parse(RegExp),
    fold_files1(Dir, Re1, Recursive, Fun, Acc).

fold_files1(Dir, RegExp, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} -> fold_files2(Files, Dir, RegExp, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

fold_files2([], _Dir, _RegExp, _Recursive, _Fun, Acc) -> Acc;
fold_files2([File|T], Dir, RegExp, Recursive, Fun, Acc0) ->
    FullName = filename:join(Dir, File),
    case is_regular(FullName) of
	true  ->
	    case regexp:match(File, RegExp) of
		{match, _, _}  -> 
		    Acc = Fun(FullName, Acc0),
		    fold_files2(T, Dir, RegExp, Recursive, Fun, Acc);
		_ ->
		    fold_files2(T, Dir, RegExp, Recursive, Fun, Acc0)
	    end;
	false ->
	    case Recursive and is_dir(FullName) of
		true ->
		    Acc1 = fold_files1(FullName, RegExp, Recursive, Fun, Acc0),
		    fold_files2(T, Dir, RegExp, Recursive, Fun, Acc1);
		false ->
		    fold_files2(T, Dir, RegExp, Recursive, Fun, Acc0)
	    end
    end.

last_modified(File) ->
    case file:read_file_info(File) of
	{ok, Info} ->
	    Info#file_info.mtime;
	_ ->
	    0
    end.

file_size(File) ->
    case file:read_file_info(File) of
	{ok, Info} ->
	    Info#file_info.size;
	_ ->
	    0
    end.

%%----------------------------------------------------------------------
%% +type ensure_dir(X) -> true.
%% +type X = filename() | dirname()
%% ensures that the directory name required to create D exists

ensure_dir("/") ->
    true;
ensure_dir(F) ->
    Dir = filename:dirname(F),
    case is_dir(Dir) of
	true ->
	    true;
	false ->
	    ensure_dir(Dir),
	    file:make_dir(Dir)
    end.


%%%
%%% Pattern matching using a compiled wildcard.
%%%

wildcard_1(Files, Pattern) ->
    wildcard_2(Files, Pattern, []).

wildcard_2([File|Rest], Pattern, Result) ->
    wildcard_2(Rest, Pattern, wildcard_3(File, Pattern, Result));
wildcard_2([], _, Result) ->
    Result.

wildcard_3(Base, [Pattern|Rest], Result) ->
    case list_dir(Base) of
	{ok, Files0} ->
	    Files = lists:sort(Files0),
	    Matches = wildcard_4(Pattern, Files, Base, []),
	    wildcard_2(Matches, Rest, Result);
	_ ->
	    Result
    end;
wildcard_3(Base, [], Result) ->
    [Base|Result].

wildcard_4(Pattern, [File|Rest], Base, Result) ->
    case wildcard_5(Pattern, File) of
	true ->
	    wildcard_4(Pattern, Rest, Base, [join(Base, File)|Result]);
	false ->
	    wildcard_4(Pattern, Rest, Base, Result)
    end;
wildcard_4(_Patt, [], _Base, Result) ->
    Result.

wildcard_5([question|Rest1], [_|Rest2]) ->
    wildcard_5(Rest1, Rest2);
wildcard_5([accept], _) ->
    true;
wildcard_5([star|Rest], File) ->
    do_star(Rest, File);
wildcard_5([{one_of, Ordset}|Rest], [C|File]) ->
    case ordsets:is_element(C, Ordset) of
	true  -> wildcard_5(Rest, File);
	false -> false
    end;
wildcard_5([{alt, Alts}], File) ->
    do_alt(Alts, File);
wildcard_5([C|Rest1], [C|Rest2]) when is_integer(C) ->
    wildcard_5(Rest1, Rest2);
wildcard_5([X|_], [Y|_]) when is_integer(X), is_integer(Y) ->
    false;
wildcard_5([], []) ->
    true;
wildcard_5([], [_|_]) ->
    false;
wildcard_5([_|_], []) ->
    false.

do_star(Pattern, [X|Rest]) ->
    case wildcard_5(Pattern, [X|Rest]) of
	true  -> true;
	false -> do_star(Pattern, Rest)
    end;
do_star(Pattern, []) ->
    wildcard_5(Pattern, []).

do_alt([Alt|Rest], File) ->
    case wildcard_5(Alt, File) of
	true  -> true;
	false -> do_alt(Rest, File)
    end;
do_alt([], _File) ->
    false.

list_dir(current) -> file:list_dir(".");
list_dir(Dir) ->     file:list_dir(Dir).

join(current, File) -> File;
join(Base, File) -> filename:join(Base, File).

	    
%%% Compiling a wildcard.

compile_wildcard(Pattern) ->
    try 
	{compiled_wildcard,compile_wildcard_1(Pattern)}
    catch
	error:{badpattern,_}=Error ->
	    %% Get the stack backtrace correct.
	    erlang:error(Error)
    end.

compile_wildcard_1(Pattern) ->
    [Root|Rest] = filename:split(Pattern),
    case filename:pathtype(Root) of
	relative ->
	    compile_wildcard_2([Root|Rest], current);
	_ ->
	    compile_wildcard_2(Rest, [Root])
    end.

compile_wildcard_2([Part|Rest], Root) ->
    case compile_part(Part) of
	Part ->
	    compile_wildcard_2(Rest, join(Root, Part));
	Pattern ->
	    compile_wildcard_3(Rest, [Pattern,Root])
    end;
compile_wildcard_2([], Root) -> {exists,Root}.

compile_wildcard_3([Part|Rest], Result) ->
    compile_wildcard_3(Rest, [compile_part(Part)|Result]);
compile_wildcard_3([], Result) ->
    lists:reverse(Result).

compile_part(Part) ->
    compile_part(Part, false, []).

compile_part_to_sep(Part) ->
    compile_part(Part, true, []).

compile_part([], true, _) ->
    error(missing_delimiter);
compile_part([$,|Rest], true, Result) ->
    {ok, $,, lists:reverse(Result), Rest};
compile_part([$}|Rest], true, Result) ->
    {ok, $}, lists:reverse(Result), Rest};
compile_part([$?|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [question|Result]);
compile_part([$*], Upto, Result) ->
    compile_part([], Upto, [accept|Result]);
compile_part([$*|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [star|Result]);
compile_part([$[|Rest], Upto, Result) ->
    case compile_charset(Rest, ordsets:new()) of
	{ok, Charset, Rest1} ->
	    compile_part(Rest1, Upto, [Charset|Result]);
	error ->
	    compile_part(Rest, Upto, [$[|Result])
    end;
compile_part([${|Rest], Upto, Result) ->
    case compile_alt(Rest) of
	{ok, Alt} ->
	    lists:reverse(Result, [Alt]);
	error ->
	    compile_part(Rest, Upto, [${|Result])
    end;
compile_part([X|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [X|Result]);
compile_part([], _Upto, Result) ->
    lists:reverse(Result).

compile_charset([$]|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element($], Ordset));
compile_charset([$-|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element($-, Ordset));
compile_charset([], _Ordset) ->
    error;
compile_charset(List, Ordset) ->
    compile_charset1(List, Ordset).

compile_charset1([Lower, $-, Upper|Rest], Ordset) when Lower =< Upper ->
    compile_charset1(Rest, compile_range(Lower, Upper, Ordset));
compile_charset1([$]|Rest], Ordset) ->
    {ok, {one_of, Ordset}, Rest};
compile_charset1([X|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element(X, Ordset));
compile_charset1([], _Ordset) ->
    error.
    
compile_range(Lower, Current, Ordset) when Lower =< Current ->
    compile_range(Lower, Current-1, ordsets:add_element(Current, Ordset));
compile_range(_, _, Ordset) ->
    Ordset.

compile_alt(Pattern) ->
    compile_alt(Pattern, []).

compile_alt(Pattern, Result) ->
    case compile_part_to_sep(Pattern) of
	{ok, $,, AltPattern, Rest} ->
	    compile_alt(Rest, [AltPattern|Result]);
	{ok, $}, AltPattern, Rest} ->
	    NewResult = [AltPattern|Result],
	    RestPattern = compile_part(Rest),
	    {ok, {alt, [Alt++RestPattern || Alt <- NewResult]}};
	Pattern ->
	    error
    end.

error(Reason) ->
    erlang:error({badpattern,Reason}).
