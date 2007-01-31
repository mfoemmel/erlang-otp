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
%%     $Id $
%%
-module(qlc).

%%% Purpose: Main API module QLC. Functions for evaluation.
%%% Other files:
%%% qlc_pt. Implements the parse transform.

%% External exports 

-export([parse_transform/2, transform_from_evaluator/2]).

-export([q/1, q/2]).

-export([eval/1, e/1, eval/2, e/2, fold/3, fold/4]).
-export([cursor/1, cursor/2, 
         next_answers/1, next_answers/2, 
         delete_cursor/1]).
-export([append/1, append/2]).

-export([sort/1, sort/2, keysort/2, keysort/3]).

-export([table/2]).

-export([info/1, info/2]).

-export([string_to_handle/1, string_to_handle/2, string_to_handle/3]).

-export([format_error/1]).

%% Exported to qlc_pt.erl only:
-export([template_state/0]).

%% When cache=list lists bigger than ?MAX_LIST_SIZE bytes are put on
%% file. Also used when merge join finds big equivalence classes.
-define(MAX_LIST_SIZE, 512*1024).

-record(qlc_append, % qlc:append/1,2
        {hl
        }).

-record(qlc_table,   % qlc:table/2
        {trav_fun,   % traverse fun
         trav_MS,    % bool(); true iff traverse fun takes a match spec
         pre_fun,
         post_fun,
         info_fun,
         format_fun,
         lookup_fun,
         parent_fun,
         f1,         % unused
         lu_vals,    % undefined | {Position,Values}; values to be looked up
         ms = no_match_spec
                     % match specification; [T || P <- Tab, Fs]
        }).

-record(qlc_sort,   % qlc:sort/1,2 and qlc:keysort/2,3
        {h,
         keypos,    % sort | {keysort, KeyPos}
         unique,
         compressed,
         order,
         opts,
         tmpdir
        }).

%% Also in qlc_pt.erl.
-record(qlc_lc,     % qlc:q/1,2
        {lc,
         opt        % #qlc_opt
        }).

-record(qlc_list,   % a prepared list
        {l,
         ms = no_match_spec
        }).

-record(qlc_join,        % a prepared join
        {kind,           % merge | {lookup, LookupFun}
         opt,            % #qlc_opt from q/2.
         h1, q1, c1,     % to be traversed by "lookup join"
         h2, q2, c2      % to be looked up by "lookup join"
        }).

%%% A query cursor is a tuple {qlc_cursor, Cursor} where Cursor is a pair
%%% {CursorPid, OwnerPid}.

-record(qlc_cursor, {c}).

-record(qlc_opt,
        {unique = false,      % bool()
         cache = false,       % bool() | list (true~ets, false~no)
         max_lookup = -1,     % int() >= 0 | -1 (represents infinity)
         join = any,          % any | nested_loop | merge | lookup
         tmpdir = "",         % global tmpdir
         lookup = any,        % any | bool()
         max_list = ?MAX_LIST_SIZE  % int() >= 0
        }).

-record(setup, {parent}).

-define(THROWN_ERROR, {?MODULE, throw_error, _}).

%%% A query handle is a tuple {qlc_handle, Handle} where Handle is one
%%% of #qlc_append, #qlc_table, #qlc_sort, and #qlc_lc.

-record(qlc_handle, {h}).

get_handle(#qlc_handle{h = #qlc_lc{opt = {qlc_opt, U, C, M}}=H}) ->
    %% R10B and R11B-0.
    H#qlc_lc{opt = #qlc_opt{unique = U, cache = C, max_lookup = M}};
get_handle(#qlc_handle{h = H}) ->
    H;
get_handle(L) when is_list(L) ->
    L;
get_handle(_) ->
    badarg.

%%%
%%% Exported functions
%%%

append(QHs) ->
    Hs = lists:map(fun(QH) -> 
                           case get_handle(QH) of
                               badarg -> erlang:error(badarg, [QHs]);
                               H -> H
                           end end, QHs),
    #qlc_handle{h = #qlc_append{hl = Hs}}.

append(QH1, QH2) ->
    Hs = lists:map(fun(QH) -> 
                           case get_handle(QH) of
                               badarg -> erlang:error(badarg, [QH1, QH2]);
                               H -> H
                           end end, [QH1, QH2]),
    #qlc_handle{h = #qlc_append{hl = Hs}}.

cursor(QH) ->
    cursor(QH, []).

cursor(QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, 
                            spawn_options, max_list_size]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, TmpDir, SpawnOptions0, MaxList], H} ->
            SpawnOptions = spawn_options(SpawnOptions0),
            case cursor_process(H, GUnique, GCache, TmpDir, 
                                SpawnOptions, MaxList) of
                Pid when is_pid(Pid) ->
                    #qlc_cursor{c = {Pid, self()}};
                Error ->
                    Error
            end
    end.

delete_cursor(#qlc_cursor{c = {_, Owner}}=C) when Owner =/= self() ->
    erlang:error(not_cursor_owner, [C]);
delete_cursor(#qlc_cursor{c = {Pid, _}}) ->
    stop_cursor(Pid);
delete_cursor(T) ->
    erlang:error(badarg, [T]).

e(QH) ->
    eval(QH, []).

e(QH, Options) ->
    eval(QH, Options).

eval(QH) ->
    eval(QH, []).

eval(QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, max_list_size]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, TmpDir, MaxList], Handle} ->
            try 
                Prep = prepare_qlc(Handle, [], GUnique, GCache, 
                                   TmpDir, MaxList),
                case setup_qlc(Prep, #setup{parent = self()}) of
                    {L, Post, _LocalPost} when is_list(L) ->
                        post_funs(Post),
                        L;
                    {Objs, Post, _LocalPost} when is_function(Objs) ->
                        try
                            (ensure_collecting(Prep, Objs))()
                        after
                            post_funs(Post)
                        end
                end
            catch Term ->
                case erlang:get_stacktrace() of
                    [?THROWN_ERROR | _] ->
                        Term;
                    Stacktrace ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

fold(Fun, Acc0, QH) ->
    fold(Fun, Acc0, QH, []).

fold(Fun, Acc0, QH, Options) ->
    case {options(Options, [unique_all, cache_all, tmpdir, max_list_size]), 
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [Fun, Acc0, QH, Options]);
        {[GUnique, GCache, TmpDir, MaxList], Handle} ->
            try
                Prep = prepare_qlc(Handle, not_a_list, GUnique, GCache,
                                   TmpDir, MaxList),
                case setup_qlc(Prep, #setup{parent = self()}) of
                    {Objs, Post, _LocalPost} when is_function(Objs); 
                                                  is_list(Objs) ->
                        try
                            fold_loop(Fun, Objs, Acc0)
                        after
                            post_funs(Post)
                        end
                end
            catch Term ->
                case erlang:get_stacktrace() of
                    [?THROWN_ERROR | _] ->
                        Term;
                    Stacktrace ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

format_error(not_a_query_list_comprehension) ->
    io_lib:format("argument is not a query list comprehension", []);
format_error({used_generator_variable, V}) ->
    io_lib:format("generated variable ~w must not be used in list expression",
                  [V]);
format_error(too_complex_join) ->
    io_lib:format("cannot handle join of three or more generators efficiently",
                  []);
format_error(too_many_joins) ->
    io_lib:format("cannot handle more than one join efficiently", []);
format_error({Line, Mod, Reason}) when is_integer(Line) ->
    io_lib:format("~p: ~s~n", 
                  [Line, lists:flatten(Mod:format_error(Reason))]);
%% file_sorter errors
format_error({bad_object, FileName}) ->
    io_lib:format("the temporary file \"~s\" holding answers is corrupt",
                 [FileName]);
format_error(bad_object) ->
    io_lib:format("the keys could not be extracted from some term", []);
format_error({file_error, FileName, Reason}) ->
    io_lib:format("\"~s\": ~p~n",[FileName, file:format_error(Reason)]);
format_error({premature_eof, FileName}) ->
    io_lib:format("\"~s\": end-of-file was encountered inside some binary term", 
                  [FileName]);
format_error({not_a_directory, FileName}) ->
    io_lib:format("\"~s\": the file supplied with the tmpdir option "
                  "is not a directory", [FileName]);
format_error({error, Module, Reason}) ->
    Module:format_error(Reason);
format_error(E) ->
    io_lib:format("~p~n", [E]).

info(QH) ->
    info(QH, []).

info(QH, Options) ->
    case {options(Options, [unique_all, cache_all, flat, format, n_elements, 
                            tmpdir, max_list_size]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[GUnique, GCache, Flat, Format, NElements, TmpDir, MaxList], H} ->
            try
                Prep = prepare_qlc(H, [], GUnique, GCache, TmpDir, MaxList),
                Info = le_info(Prep),
                AbstractCode = abstract(Info, Flat, NElements),
                case Format of
                    abstract_code ->
                        AbstractCode;
                    string ->
                        lists:flatten(erl_pp:expr(AbstractCode, 0, none));
                    debug -> % Not documented. Intended for testing only.
                        Info
                end
            catch Term ->
                case erlang:get_stacktrace() of
                    [?THROWN_ERROR | _] ->
                        Term;
                    Stacktrace ->
                        erlang:raise(throw, Term, Stacktrace)
                end
            end
    end.

keysort(KeyPos, QH) ->
    keysort(KeyPos, QH, []).

keysort(KeyPos, QH, Options) ->
    case {is_keypos(KeyPos), 
          options(Options, [tmpdir, order, unique, compressed, 
                            size, no_files]),
          get_handle(QH)} of
        {true, [TmpDir, Order, Unique,Compressed | _], H} when H =/= badarg ->
            #qlc_handle{h = #qlc_sort{h = H, keypos = {keysort,KeyPos}, 
                                      unique = Unique, compressed = Compressed,
                                      order = Order, opts = listify(Options), 
                                      tmpdir = TmpDir}};
        _ ->
            erlang:error(badarg, [KeyPos, QH, Options])
    end.

-define(DEFAULT_NUM_OF_ANSWERS, 10).

next_answers(C) ->
    next_answers(C, ?DEFAULT_NUM_OF_ANSWERS).

next_answers(#qlc_cursor{c = {_, Owner}}=C, 
             NumOfAnswers) when Owner =/= self() ->
    erlang:error(not_cursor_owner, [C, NumOfAnswers]);
next_answers(#qlc_cursor{c = {Pid, _}}=C, NumOfAnswers) ->
    N = case NumOfAnswers of
            all_remaining -> -1;
            _ when is_integer(NumOfAnswers), NumOfAnswers > 0 -> NumOfAnswers;
            _ -> erlang:error(badarg, [C, NumOfAnswers])
        end,
    next_loop(Pid, [], N);
next_answers(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

parse_transform(Forms, Options) ->
    qlc_pt:parse_transform(Forms, Options).

%% The funcspecs qlc:q/1 and qlc:q/2 are known by erl_eval.erl and
%% erl_lint.erl.
q(QLC_lc) ->
    q(QLC_lc, []).

q(#qlc_lc{}=QLC_lc, Options) ->
    case options(Options, [unique, cache, max_lookup, join, lookup]) of
        [Unique, Cache, Max, Join, Lookup] ->
            Opt = #qlc_opt{unique = Unique, cache = Cache, 
                           max_lookup = Max, join = Join, lookup = Lookup},
            #qlc_handle{h = QLC_lc#qlc_lc{opt = Opt}};
        _ ->
            erlang:error(badarg, [QLC_lc, Options])
    end;
q(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

sort(QH) ->
    sort(QH, []).

sort(QH, Options) ->
    case {options(Options, [tmpdir, order, unique, compressed, 
                            size, no_files]), get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:error(badarg, [QH, Options]);
        {[TD, Order, Unique, Compressed | _], H} ->
            #qlc_handle{h = #qlc_sort{h = H, keypos = sort, unique = Unique, 
                                      compressed = Compressed, order = Order,
                                      opts = listify(Options), tmpdir = TD}}
    end.

%% Note that the generated code is evaluated by (the slow) erl_eval.
string_to_handle(Str) ->
    string_to_handle(Str, []).

string_to_handle(Str, Options) ->
    string_to_handle(Str, Options, []).

string_to_handle(Str, Options, Bindings) when is_list(Str), 
                                              is_list(Bindings) ->
    case options(Options, [unique, cache, max_lookup, join, lookup]) of
        badarg ->
            erlang:error(badarg, [Str, Options, Bindings]);
        [Unique, Cache, MaxLookup, Join, Lookup] ->
            case erl_scan:string(Str) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_exprs(Tokens) of
                        {ok, [Expr]} ->
                            case qlc_pt:transform_expression(Expr, Bindings) of
                                {ok, {call, _, _QlcQ,  Handle}} ->
                                    {value, QLC_lc, _} = 
                                        erl_eval:exprs(Handle, Bindings),
                                    O = #qlc_opt{unique = Unique, 
                                                 cache = Cache,
                                                 max_lookup = MaxLookup, 
                                                 join = Join,
                                                 lookup = Lookup},
                                    #qlc_handle{h = QLC_lc#qlc_lc{opt = O}};
                                {not_ok, [{error, Error} | _]} ->
                                    error(Error)
                            end;
                        {ok, _ExprList} ->
                            erlang:error(badarg, [Str, Options, Bindings]);
                        {error, ErrorInfo} ->
                            error(ErrorInfo)
                    end;
                {error, ErrorInfo, _EndLine} ->
                    error(ErrorInfo)
            end
    end;
string_to_handle(T1, T2, T3) ->    
    erlang:error(badarg, [T1, T2, T3]).

table(TraverseFun, Options) when is_function(TraverseFun) ->
    case {is_function(TraverseFun, 0), 
          IsFun1 = is_function(TraverseFun, 1)} of
        {false, false} ->
            erlang:error(badarg, [TraverseFun, Options]);
        _ ->
            case options(Options, [pre_fun, post_fun, info_fun, format_fun, 
                                   lookup_fun, parent_fun]) of
                [PreFun, PostFun, InfoFun, FormatFun, LookupFun, ParentFun] ->
                    T = #qlc_table{trav_fun = TraverseFun, pre_fun = PreFun,
                                   post_fun = PostFun, info_fun = InfoFun, 
                                   parent_fun = ParentFun,
                                   trav_MS = IsFun1,
                                   format_fun = FormatFun, 
                                   lookup_fun = LookupFun},
                    #qlc_handle{h = T};
                badarg ->
                    erlang:error(badarg, [TraverseFun, Options])
            end
    end;
table(T1, T2) ->
    erlang:error(badarg, [T1, T2]).

transform_from_evaluator(LC, Bs0) ->
    qlc_pt:transform_from_evaluator(LC, Bs0).

%%%
%%% Local functions
%%%

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options0, [Key | Keys], L) when is_list(Options0) ->
    Options = case lists:member(Key, Options0) of
                  true -> 
                      [atom_option(Key) | lists:delete(Key, Options0)];
                  false ->
                      Options0
              end,
    V = case lists:keysearch(Key, 1, Options) of
            {value, {format_fun, U=undefined}} ->
                {ok, U};
            {value, {info_fun, U=undefined}} ->
                {ok, U};
            {value, {lookup_fun, U=undefined}} ->
                {ok, U};
            {value, {parent_fun, U=undefined}} ->
                {ok, U};
            {value, {post_fun, U=undefined}} ->
                {ok, U};
            {value, {pre_fun, U=undefined}} ->
                {ok, U};
            {value, {info_fun, Fun}} when is_function(Fun), 
                                          is_function(Fun, 1) ->
                {ok, Fun};
            {value, {pre_fun, Fun}} when is_function(Fun),
                                         is_function(Fun, 1) ->
                {ok, Fun};
            {value, {post_fun, Fun}} when is_function(Fun), 
                                          is_function(Fun, 0) ->
                {ok, Fun};
            {value, {lookup_fun, Fun}} when is_function(Fun),
                                            is_function(Fun, 2) ->
                {ok, Fun};
            {value, {max_lookup, Max}} when is_integer(Max), Max >= 0 ->
                {ok, Max};
            {value, {max_lookup, infinity}} ->
                {ok, -1};
            {value, {format_fun, Fun}} when is_function(Fun),
                                            is_function(Fun, 1) ->
                {ok, Fun};
            {value, {parent_fun, Fun}} when is_function(Fun),
                                            is_function(Fun, 0) ->
                {ok, Fun};
            {value, {join, J=any}} ->
                {ok, J};
            {value, {join, J=nested_loop}} ->
                {ok, J};
            {value, {join, J=merge}} ->
                {ok, J};
            {value, {join, J=lookup}} ->
                {ok, J};
            {value, {lookup, LookUp}} when LookUp; 
                                           not LookUp; 
                                           LookUp =:= any ->
                {ok, LookUp};
            {value, {max_list_size, Max}} when is_integer(Max), Max >= 0 ->
                {ok, Max};
            {value, {unique, Unique}} when Unique; not Unique ->
                {ok, Unique};
            {value, {cache, Cache}} when Cache; not Cache; Cache =:= list ->
                {ok, Cache};
            {value, {cache, ets}} ->
                {ok, true};
            {value, {cache, no}} ->
                {ok, false};
            {value, {unique_all, UniqueAll}} when UniqueAll; not UniqueAll ->
                {ok, UniqueAll};
            {value, {cache_all, CacheAll}} when CacheAll; 
                                                not CacheAll;
                                                CacheAll =:= list ->
                {ok, CacheAll};
            {value, {cache_all, ets}} ->
                {ok, true};
            {value, {cache_all, no}} ->
                {ok, false};
            {value, {spawn_options, default}} ->
                {ok, default};
            {value, {spawn_options, SpawnOptions}} ->
                case is_proper_list(SpawnOptions) of
                    true -> 
                        {ok, SpawnOptions};
                    false ->
                        badarg
                end;
            {value, {flat, Flat}} when Flat; not Flat ->
                {ok, Flat};
            {value, {format, Format}} when Format =:= string;
                                           Format =:= abstract_code;
                                           Format =:= debug ->
                {ok, Format};
            {value, {n_elements, NElements}} when NElements =:= infinity; 
                                          integer(NElements), NElements > 0 ->
                {ok, NElements};
            {value, {order, Order}} when is_function(Order), 
                                           is_function(Order, 2);
                                         (Order =:= ascending);
                                         (Order =:= descending) ->
                {ok, Order};
            {value, {compressed, Comp}} when Comp ->
                {ok, [compressed]};
            {value, {compressed, Comp}} when not Comp ->
                {ok, []};
            {value, {tmpdir, T}} ->
                {ok, T};
            {value, {size, Size}} when is_integer(Size), Size > 0 ->
                {ok, Size};
            {value, {no_files, NoFiles}} when integer(NoFiles), NoFiles > 1 ->
                {ok, NoFiles};
            {value, {Key, _}} ->
                badarg;
            false ->
                Default = default_option(Key),
                {ok, Default}
        end,
    case V of
        badarg ->
            badarg;
        {ok, Value} ->
            NewOptions = lists:keydelete(Key, 1, Options),
            options(NewOptions, Keys, [Value | L])
    end;
options([], [], L) ->
    lists:reverse(L);
options(_Options, _, _L) ->
    badarg.

default_option(pre_fun) -> undefined;
default_option(post_fun) -> undefined;
default_option(info_fun) -> undefined;
default_option(format_fun) -> undefined;
default_option(lookup_fun) -> undefined;
default_option(max_lookup) -> -1;
default_option(join) -> any;
default_option(lookup) -> any;
default_option(parent_fun) -> undefined;
default_option(spawn_options) -> default;
default_option(flat) -> true;
default_option(format) -> string;
default_option(n_elements) -> infinity;
default_option(max_list_size) -> ?MAX_LIST_SIZE;
default_option(cache) -> false;
default_option(cache_all) -> false;
default_option(unique) -> false;
default_option(unique_all) -> false;
default_option(order) -> ascending; % default values from file_sorter.erl
default_option(compressed) -> [];
default_option(tmpdir) -> "";
default_option(size) -> 524288;
default_option(no_files) -> 16.

atom_option(cache) -> {cache, true};
atom_option(unique) -> {unique, true};
atom_option(cache_all) -> {cache_all, true};
atom_option(unique_all) -> {unique_all, true};
atom_option(lookup) -> {lookup, true};
atom_option(flat) -> {flat, true};
atom_option(Key) -> Key.

is_proper_list([_ | L]) ->
    is_proper_list(L);
is_proper_list(L) ->
    L =:= [].

spawn_options(default) ->
    [link];
spawn_options(SpawnOptions) ->
    lists:delete(monitor, 
                 case lists:member(link, SpawnOptions) of
                     true -> 
                         SpawnOptions;
                     false ->
                         [link | SpawnOptions]
                 end).

is_keypos(Keypos) when is_integer(Keypos), Keypos > 0 ->
    true;
is_keypos([]) ->
    false;
is_keypos(L) ->
    is_keyposs(L).

is_keyposs([Kp | Kps]) when is_integer(Kp), Kp > 0 ->
    is_keyposs(Kps);
is_keyposs(Kps) ->
    Kps =:= [].

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% Optimizations to be carried out.
-record(optz,
        {unique = false,    % bool()
         cache = false,     % bool() | list
         join_option = any, % constraint set by the 'join' option
         fast_join = no,    % no | #qlc_join. 'no' means nested loop.
         opt                % #qlc_opt
        }).

%% Prepared #qlc_lc.
-record(qlc,
        {lcf,       % fun() -> Val
         codef,
         qdata,     % with evaluated list expressions
         init_value,
         optz       % #optz
        }).

%% Prepared simple #qlc_lc.
-record(simple_qlc,
        {p,         % atom(), pattern variable
         le,
         line,
         init_value,
         optz       % #optz
         }).

-record(prepared,
        {qh,     % #qlc_append | #qlc_table | #qlc | #simple_qlc | 
                 % #qlc_sort | list()
         sorted = no,  % yes | no | ascending | descending
         sort_info = [], % 
         sort_info2 = [], % 'sort_info' updated with pattern info; qh is LE
         join = {[],[]}, % {Lookup, Merge}
         n_objs = undefined,   % for join (not used yet)
         is_unique_objects = false, % bool()
         is_cached = false          % bool() (true means 'ets' or 'list')
        }).

ensure_collecting(Prep, Objs) ->
    case Prep#prepared.qh of
        #qlc{optz = #optz{unique = false, cache = false}} ->
            Objs;
        _ -> 
            fun() -> collect(Objs, []) end
    end.

%%% Cursor process functions.

cursor_process(H, GUnique, GCache, TmpDir, SpawnOptions, MaxList) ->
    Parent = self(),
    Setup = #setup{parent = Parent},
    CF = fun() -> 
                 %% Unless exit/2 is trapped no cleanup can be done.
                 %% The user is assumed not to set the flag to false.
                 process_flag(trap_exit, true),
                 MonRef = erlang:monitor(process, Parent),
                 {Objs, Post, _LocalPost} = 
                     try 
                         Prep = prepare_qlc(H, not_a_list, GUnique, 
                                            GCache, TmpDir, MaxList),
                         setup_qlc(Prep, Setup)
                     catch Class:Reason ->
                           Parent ! {self(), {caught, Class, Reason, 
                                     erlang:get_stacktrace()}},
                           exit(normal)
                     end,
                 Parent ! {self(), ok},
                 wait_for_request(Parent, MonRef, Post), 
                 reply(Parent, MonRef, Post, Objs)
         end,
    Pid = spawn_opt(CF, SpawnOptions),
    parent_fun(Pid, Parent).

%% Expect calls from tables calling the parent_fun and finally an 'ok'.
parent_fun(Pid, Parent) ->
    receive 
        {Pid, ok} -> Pid;
        {TPid, {parent_fun, Fun}} ->
            V = try 
                    {value, Fun()}
                catch Class:Reason ->
                    {parent_fun_caught, Class, Reason, erlang:get_stacktrace()}
            end,
            TPid ! {Parent, V},
            parent_fun(Pid, Parent);
        {Pid, {caught, throw, Error, [?THROWN_ERROR | _]}} ->
            Error;
        {Pid, {caught, Class, Reason, Stacktrace}} ->
            erlang:raise(Class, Reason, Stacktrace);
        _Ignored ->
            parent_fun(Pid, Parent)
    end.

reply(Parent, MonRef, Post, []) ->
    no_more(Parent, MonRef, Post);
reply(Parent, MonRef, Post, [Answer | Cont]) ->
    Parent ! {self(), {answer, Answer}},    
    wait_for_request(Parent, MonRef, Post),
    reply(Parent, MonRef, Post, Cont);
reply(Parent, MonRef, Post, Cont) ->
    Reply = try 
                if 
                    is_function(Cont) ->
                        Cont();
                    true ->
                        throw_error(Cont)
                end
            catch 
                Class:Reason ->
                   post_funs(Post),
                   Message = {caught, Class, Reason, erlang:get_stacktrace()},
                   Parent ! {self(), Message},
                   exit(normal)
            end,
    reply(Parent, MonRef, Post, Reply).

no_more(Parent, MonRef, Post) ->
    Parent ! {self(), no_more},
    wait_for_request(Parent, MonRef, Post),
    no_more(Parent, MonRef, Post).

wait_for_request(Parent, MonRef, Post) ->
    receive 
        {Parent, stop} ->
            post_funs(Post),
            exit(normal);
        {Parent, more} ->
            ok;
        {'EXIT', Parent, _Reason} ->
            post_funs(Post),
            exit(normal);
        {'DOWN', MonRef, process, Parent, _Info} ->
            post_funs(Post),
            exit(normal);
        {'EXIT', Pid, _Reason} when Pid =:= self() ->
            %% Trapped signal. The cursor ignores it...
            wait_for_request(Parent, MonRef, Post);
        Other ->
            error_logger:error_msg(
              "The QLC cursor ~w received an unexpected message:\n~p\n", 
              [self(), Other]),
            wait_for_request(Parent, MonRef, Post)
    end.

%%% End of cursor process functions.

%% Also in qlc_pt.erl.
-define(Q, q).
-define(QLC_Q(L1, L2, L3, L4, LC, Os), 
        {call,L1,{remote,L2,{atom,L3,?MODULE},{atom,L4,?Q}},[LC | Os]}).

abstract(Info, false, NElements) ->
    abstract(Info, NElements);
abstract(Info, true, NElements) ->
    Abstract = abstract(Info, NElements),
    Vars = vars(Abstract),
    {_, Body0, Expr} = flatten_abstr(Abstract, 1, Vars, []),
    case Body0 of
        [] -> 
            Expr;
        [{match,_,Expr,Q}] ->
            Q;
        [{match,_,Expr,Q} | Body] ->
            {block, 0, lists:reverse(Body, [Q])};
        _ ->
            {block, 0, lists:reverse(Body0, [Expr])}
    end.

abstract({qlc, E0, Qs0, Opt}, NElements) ->
    Qs = lists:map(fun({generate, P, LE}) ->
                           {generate, 1, binary_to_term(P), 
                            abstract(LE, NElements)};
                      (F) -> 
                           binary_to_term(F)
                   end, Qs0),
    E = binary_to_term(E0),
    Os = case Opt of
             [] -> [];
             _ -> [erl_parse:abstract(Opt, 1)]
         end,
    ?QLC_Q(1, 1, 1, 1, {lc,1,E,Qs}, Os);
abstract({table, {M, F, As0}}, _NElements) 
                          when is_atom(M), is_atom(F), is_list(As0) ->
    As = lists:map(fun(A) -> erl_parse:abstract(A, 1) end, As0),
    {call, 1, {remote, 1, {atom, 1, M}, {atom, 1, F}}, As};
abstract({table, TableDesc}, _NElements) ->
    case io_lib:deep_char_list(TableDesc) of
        true ->
            {ok, Tokens, _} = erl_scan:string(lists:flatten(TableDesc++".")),
            {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
            Expr;
        false -> % abstract expression
            TableDesc
    end;
abstract({append, Infos}, NElements) ->
    As = lists:foldr(fun(Info, As0) -> {cons,1,abstract(Info, NElements),As0}
                     end, {nil, 1}, Infos),
    {call, 1, {remote, 1, {atom, 1, ?MODULE}, {atom, 1, append}}, [As]};
abstract({sort, Info, SortOptions}, NElements) ->    
    {call, 1, {remote, 1, {atom, 1, ?MODULE}, {atom, 1, sort}},
     [abstract(Info, NElements), erl_parse:abstract(SortOptions, 1)]};
abstract({keysort, Info, Kp, SortOptions}, NElements) ->    
    {call, 1, {remote, 1, {atom, 1, ?MODULE}, {atom, 1, keysort}},
     [erl_parse:abstract(Kp, 1), abstract(Info, NElements), 
      erl_parse:abstract(SortOptions, 1)]};
abstract({list,L,MS}, NElements) ->
    {call, 1, {remote, 1, {atom, 1, ets}, {atom, 1, match_spec_run}},
     [abstract(L, NElements),
      {call, 1, {remote, 1, {atom, 1, ets}, {atom, 1, match_spec_compile}},
       [erl_parse:abstract(MS, 1)]}]};
abstract({list, L}, NElements) when NElements =:= infinity; 
                                    NElements >= length(L) ->
    erl_parse:abstract(L, 1);
abstract({list, L}, NElements) ->
    erl_parse:abstract(lists:sublist(L, NElements) ++ more, 1).

%% Since generator pattern variables cannot be used in list
%% expressions, it is OK to flatten out QLC expressions using
%% temporary variables.
flatten_abstr(?QLC_Q(L1, L2, L3, L4, LC0, Os), VN0, Vars, Body0) ->
    {lc,L,E,Qs0} = LC0,
    F = fun({generate,Ln,P,LE0}, {VN1,Body1}) ->
                {VN2,Body2,LE} = flatten_abstr(LE0, VN1, Vars, Body1),
                {{generate,Ln,P,LE}, {VN2,Body2}};
           (Fil, VN_Body) ->
                {Fil, VN_Body}
        end,
    {Qs, {VN3,Body}} = lists:mapfoldl(F, {VN0,Body0}, Qs0),
    LC = {lc,L,E,Qs},
    {V, VN} = qlc_pt:aux_name1('V', VN3, Vars),
    Var = {var, L1, V},
    QLC = ?QLC_Q(L1, L2, L3, L4, LC, Os),
    {VN + 1, [{match, L1, Var, QLC} | Body], Var};
flatten_abstr(T0, VN0, Vars, Body0) when is_tuple(T0) ->
    {VN, Body, L} = flatten_abstr(tuple_to_list(T0), VN0, Vars, Body0),
    {VN, Body, list_to_tuple(L)};
flatten_abstr([E0 | Es0], VN0, Vars, Body0) ->
    {VN1, Body1, E} = flatten_abstr(E0, VN0, Vars, Body0),
    {VN, Body, Es} = flatten_abstr(Es0, VN1, Vars, Body1),
    {VN, Body, [E | Es]};
flatten_abstr(E, VN, _Vars, Body) ->
    {VN, Body, E}.

vars(Abstract) ->
    sets:from_list(ordsets:to_list(qlc_pt:vars(Abstract))).

collect([], L) ->
    lists:reverse(L);
collect([Answer | Cont], L) ->
    collect(Cont, [Answer | L]);
collect(Cont, L) when is_function(Cont) ->
    collect(Cont(), L);
collect(Term, _L) ->
    throw_error(Term).

fold_loop(Fun, [Obj | Cont], Acc) ->
    fold_loop(Fun, Cont, Fun(Obj, Acc));
fold_loop(_Fun, [], Acc) ->
    Acc;
fold_loop(Fun, Cont, Acc) when is_function(Cont) ->
    fold_loop(Fun, Cont(), Acc);
fold_loop(_Fun, Term, _Acc) ->
    throw_error(Term).

next_loop(Pid, L, N) when N =/= 0 ->
    case monitor_request(Pid, more) of
        no_more ->
            lists:reverse(L);
        {answer, Answer} ->
            next_loop(Pid, [Answer | L], N - 1);
        {caught, throw, Error, [?THROWN_ERROR | _]} ->
            Error;
        {caught, Class, Reason, Stacktrace} ->
            _ = (catch erlang:error(foo)),
            erlang:raise(Class, Reason, Stacktrace ++ erlang:get_stacktrace());
        error ->
            erlang:error({qlc_cursor_pid_no_longer_exists, Pid})
    end;
next_loop(_Pid, L, _N) ->
    lists:reverse(L).

stop_cursor(Pid) ->
    erlang:monitor(process, Pid),
    unlink(Pid),
    receive
        {'EXIT',Pid,_Reason} -> % Simply ignore the error.
            receive 
                {'DOWN',_,process,Pid,_} -> ok
            end
    after 0 -> 
            Pid ! {self(),stop},
            receive
                {'DOWN',_,process,Pid,_} -> ok
            end
    end.

monitor_request(Pid, Req) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Req},
    receive 
        {'DOWN', Ref, process, Pid, _Info} ->
            receive
                {'EXIT', Pid, _Reason} -> ok
            after 1 -> ok end,
            error;
        {'EXIT', Pid, _Reason} ->
            receive 
                {'DOWN', _, process, Pid, _} -> error
            end;
        {Pid, Reply} ->
            erlang:demonitor(Ref),
            receive 
                {'DOWN', Ref, process, Pid, _Reason} -> Reply
            after 0 -> Reply end
    end.

-define(TEMPLATE_STATE, 1).

template_state() ->
    ?TEMPLATE_STATE.

%% Marker for skipped filter or unused generator.
-define(SKIP, (-1)).

%% Qual = {gen, LE} | fil
-define(qual_data(QNum, GoToIndex, State, Qual), 
        {QNum, GoToIndex, State, Qual}).

-record(join, {op, q1, q2, wh1, wh2, cs_fun}). % generated by qlc_pt

%% le_info/1 returns an intermediate information format only used for
%% testing purposes. Changes will happen without notice.
%%
%% QueryDesc = {qlc, TemplateDesc, [QualDesc], [QOpt]} 
%%           | {table, TableDesc}
%%           | {append, [QueryDesc]}
%%           | {sort, QueryDesc, [SortOption]}
%%           | {keysort, KeyPos, QueryDesc, [SortOption]}
%%           | {list, list()}
%%           | {list, QueryDesc, MatchExpression}
%% TableDesc = {Mod, Fun, Args}
%%           | AbstractExpression
%%           | character_list()
%% Mod = module()
%% Fun = atom()
%% Args = [term()]
%% QualDesc = FilterDesc
%%          | {generate, PatternDesc, QueryDesc}
%% QOpt = {cache, bool()} | cache 
%%      | {unique, bool()} | unique
%% FilterDesc = PatternDesc = TemplateDesc = binary()

le_info(#prepared{qh = #simple_qlc{le = LE, p = P, line = L, optz = Optz}}) ->
    QVar = term_to_binary({var, L, P}),
    {qlc, QVar, [{generate, QVar, le_info(LE)}], opt_info(Optz)};
le_info(#prepared{qh = #qlc{codef = CodeF, qdata = Qdata, optz = Optz}}) ->
    Code = CodeF(),
    TemplateState = template_state(),
    E = element(TemplateState, Code),
    QualInfo0 = qual_info(Qdata, Code),
    QualInfo1 = case Optz#optz.fast_join of
                    #qlc_join{} = Join ->
                        join_info(Join, QualInfo0, Qdata, Code);
                    no ->
                        QualInfo0
                end,
    QualInfo = [I || I <- QualInfo1, I =/= skip],
    {qlc, E, QualInfo, opt_info(Optz)};
le_info(#prepared{qh = #qlc_table{format_fun = FormatFun, trav_MS = TravMS, 
                                  ms = MS, lu_vals = LuVals}}) ->
    case LuVals of
        _ when FormatFun =:= undefined ->
            {table, {'$MOD', '$FUN', []}};
        {Pos, Vals} when MS =:= no_match_spec ->
            {table, FormatFun({lookup, Pos, Vals})};
        {Pos, Vals} ->
            {list, {table, FormatFun({lookup, Pos, Vals})}, MS};
        _ when TravMS, is_list(MS) ->
            {table, FormatFun({match_spec, MS})};
        _ when MS =:= no_match_spec ->
            {table, FormatFun(all)}
    end;
le_info(#prepared{qh = #qlc_append{hl = HL}}) ->
    {append, lists:map(fun le_info/1, HL)};
le_info(#prepared{qh = #qlc_sort{h = H, keypos = sort, 
                                 opts = SortOptions0, tmpdir = TmpDir}}) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    {sort, le_info(H), SortOptions};
le_info(#prepared{qh = #qlc_sort{h = H, keypos = {keysort, Kp}, 
                                 opts = SortOptions0, tmpdir = TmpDir}}) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    {keysort, le_info(H), Kp, SortOptions};
le_info(#prepared{qh = #qlc_list{l = L, ms = no_match_spec}}) ->
    {list, L};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}}) when is_list(L) ->
    {list, {list, L}, MS};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}}) ->
    {list, le_info(L), MS}.

qual_info([?qual_data(_QNum, _GoI, ?SKIP, fil) | Qdata], Code) ->
    %% see skip_lookup_filters()
    [skip | qual_info(Qdata, Code)];
qual_info([?qual_data(QNum, _GoI, _SI, fil) | Qdata], Code) ->
    [element(QNum + 1, Code) | qual_info(Qdata, Code)];
qual_info([?qual_data(_QNum, _GoI, _SI, {gen,#join{}}) | Qdata], Code) ->
    [skip | qual_info(Qdata, Code)];
qual_info([?qual_data(QNum, _GoI, _SI, {gen,LE}) | Qdata], Code) ->
    [{generate,element(QNum + 1, Code),le_info(LE)} | qual_info(Qdata, Code)];
qual_info([], _Code) ->
    [].

join_info(Join, QInfo, Qdata, Code) ->
    #qlc_join{kind = Kind, q1 = QNum1a, c1 = C1, q2 = QNum2a, c2 = C2, 
              opt = Opt} = Join,
    {?qual_data(JQNum,_,_,_), Rev, QNum1, QNum2, _WH1, _WH2, CsFun} = 
        find_join_data(Qdata, QNum1a, QNum2a),
    {Cs1, Cs2, _Compat} = CsFun(),
    L = 0,
    G1_0 = {var,L,'G1'}, G2_0 = {var,L,'G2'},
    JP = element(JQNum + 1, Code),
    %% Create code for wh1 and wh2 in #join{}:
    {{I1,G1}, {I2,G2}, QInfoL} = 
        if
            Kind =:= merge ->
                %% Create code for wh1 and wh2 in #join{}:
                {JG1,QInfo1} = join_merge_info(QNum1, QInfo, Code, G1_0, Cs1),
                {JG2,QInfo2} = join_merge_info(QNum2, QInfo, Code, G2_0, Cs2),
                {JG1, JG2, QInfo1 ++ QInfo2};
            Rev ->
                {JG2,QInfo2} = join_merge_info(QNum2, QInfo, Code, G2_0, Cs2),
                {J1, QInfo1} = join_lookup_info(QNum1, QInfo, G1_0),
                {{J1,G1_0}, JG2, QInfo2 ++ [QInfo1]};
            true ->
                {JG1,QInfo1} = join_merge_info(QNum1, QInfo, Code, G1_0, Cs1),
                {J2, QInfo2} = join_lookup_info(QNum2, QInfo, G2_0),
                {JG1, {J2,G2_0}, QInfo1 ++ [QInfo2]}
        end,
    {JOptVal, JOp} = case Kind of
                         merge -> {merge, '=='};
                         {lookup, _} -> {lookup, '=:='}
              end,
    JOpt = [{join, JOptVal}] ++ opt_info(join_unique_cache(Opt)),
    JFil = term_to_binary({op,L,JOp,
                           {call,L,{atom,L,element},[{integer,L,C1},G1]},
                           {call,L,{atom,L,element},[{integer,L,C2},G2]}}),
    P = term_to_binary({cons, L, G1, G2}),
    JInfo = {generate, JP, {qlc, P, QInfoL ++ [JFil], JOpt}},
    {Before, [I1 | After]} = lists:split(QNum1 - 1, QInfo),
    Before ++ [JInfo] ++ lists:delete(I2, After).

%% qlc:q(P0 || P0 = Pattern <- H1, ConstFilters), 
%% where "P0" is a fresh variable and ConstFilters are filters that
%% test constant values of pattern columns.
join_merge_info(QNum, QInfo, Code, G, ExtraConstants) ->
    {generate, _, LEInfo}=I = lists:nth(QNum, QInfo),
    P = binary_to_term(element(QNum + 1, Code)),
    case {P, ExtraConstants} of
        {{var, _, _}, []} ->
            %% No need to introduce a QLC expression.
            {{I,P}, [I]};
        _ -> 
            {EPV, M} = 
                case P of
                    {var, _, _} -> 
                        %% No need to introduce a pattern variable.
                        {P, P};
                    _ -> 
                        {PV, _} = qlc_pt:aux_name1('P', 0, vars(P)),
                        L = 0,
                        V = {var, L, PV},
                        {V, {match, L, V, P}}
                 end,
            DQP = term_to_binary(EPV),
            LEI = {generate, term_to_binary(M), LEInfo},
            TP = term_to_binary(G),
            CFs = [begin
                       Call = {call,0,{atom,0,element},[{integer,0,Col},EPV]},
                       F = list2op([{op,0,'=:=',erl_parse:abstract(Con),Call}
                                      || Con <- Cs], 'or'),
                       term_to_binary(F)
                   end ||
                      {Col,Cs} <- ExtraConstants],
            {{I,G}, [{generate, TP, {qlc, DQP, [LEI | CFs], []}}]}
    end.

list2op([E], _Op) ->
    E;
list2op([E | Es], Op) ->
    {op,0,Op,E,list2op(Es, Op)}.

join_lookup_info(QNum, QInfo, G) ->
    {generate, _, LEInfo}=I = lists:nth(QNum, QInfo),
    TP = term_to_binary(G),
    {I, {generate, TP, LEInfo}}.

opt_info(#optz{unique = Unique, cache = Cache0, join_option = JoinOption}) ->
    %% No 'nested_loop' options are added here, even if there are
    %% nested loops to carry out, unless a 'nested_loop' was given as
    %% option. The reason is that QLC does not know about all
    %% instances of nested loops.
    Cache = if
                Cache0 -> ets;
                true -> Cache0
            end,
    [{T,V} || {T,V} <- [{cache,Cache},{unique,Unique}],
              V =/= default_option(T)] ++
    [{T,V} || {T,V} <- [{join,JoinOption}], V =:= nested_loop].

prepare_qlc(H, InitialValue, GUnique, GCache, TmpDir, MaxList) ->
    GOpt = #qlc_opt{unique = GUnique, cache = GCache, 
                    tmpdir = TmpDir, max_list = MaxList},
    case opt_le(prep_le(H, GOpt), 1) of
        #prepared{qh = #qlc{} = QLC}=Prep ->
            Prep#prepared{qh = QLC#qlc{init_value = InitialValue}};
        #prepared{qh = #simple_qlc{}=SimpleQLC}=Prep ->
            Prep#prepared{qh = SimpleQLC#simple_qlc{init_value = InitialValue}};
        Prep ->
            Prep
    end.

%%% The options given to append, q and table (unique and cache) as well
%%% as the type of expression (list, table, append, qlc...) are
%%% analyzed by prep_le. The results are is_unique_objects and
%%% is_cached. List expressions are evaluated. 

prep_le(#qlc_lc{lc = LC_fun, opt = #qlc_opt{} = Opt0}=H, GOpt) ->
    #qlc_opt{unique = GUnique, cache = GCache, 
             tmpdir = TmpDir, max_list = MaxList} = GOpt,
    Unique = Opt0#qlc_opt.unique or GUnique,
    Cache = if
                not GCache -> Opt0#qlc_opt.cache;
                true -> GCache
            end,
    Opt = Opt0#qlc_opt{unique = Unique, cache = Cache, 
                       tmpdir = TmpDir, max_list = MaxList},
    prep_qlc_lc(LC_fun(), Opt, GOpt, H);
prep_le(#qlc_table{info_fun = IF}=T, GOpt) ->
    {SortInfo, Sorted} = table_sort_info(T),
    IsUnique = grd(IF, is_unique_objects),
    Prep = #prepared{qh = T, sort_info = SortInfo, sorted = Sorted,
                     is_unique_objects = IsUnique},
    Opt = if 
              IsUnique or not GOpt#qlc_opt.unique,
              T#qlc_table.ms =:= no_match_spec -> 
                  GOpt#qlc_opt{cache = false};
              true ->
                  GOpt
          end,
    may_create_simple(Opt, Prep);
prep_le(#qlc_append{hl = HL}, GOpt) ->
    case lists:flatmap(fun(#prepared{qh = #qlc_list{l = []}}) -> [];
                          (#prepared{qh = #qlc_append{hl = HL1}}) -> HL1;
                          (H) -> [H] end, 
                       lists:map(fun(H) -> prep_le(H, GOpt) end, HL)) of
        []=Nil -> 
            short_list(Nil);
        [Prep] -> 
            Prep;
        PrepL -> 
            Cache = lists:all(fun(#prepared{is_cached = IM}) -> IM =/= false
                              end, PrepL),
            %% The handles in hl are replaced by prepared handles:
            Prep = #prepared{qh = #qlc_append{hl = PrepL}, is_cached = Cache},
            may_create_simple(GOpt, Prep)
    end;
prep_le(#qlc_sort{h = H0}=Q0, GOpt) ->
    %% The handle h is replaced by a prepared handle:
    Q = Q0#qlc_sort{h = prep_le(H0, GOpt)},
    prep_sort(Q, GOpt);
prep_le([_, _ | _]=L, GOpt) ->
    Prep = #prepared{qh = #qlc_list{l = L}, is_cached = true},
    Opt = if 
              not GOpt#qlc_opt.unique -> 
                  GOpt#qlc_opt{cache = false}; 
             true -> GOpt 
          end,
    may_create_simple(Opt, Prep);
prep_le(L, _GOpt) when is_list(L) ->
    short_list(L);
prep_le(T, _GOpt) ->
    erlang:error({unsupported_qlc_handle, #qlc_handle{h = T}}).

eval_le(LE_fun, GOpt) ->
    case LE_fun() of
        {error, ?MODULE, _} = Error ->
            throw_error(Error);
        R -> 
            case get_handle(R) of
                badarg ->
                    erlang:error(badarg, [R]);
                H -> 
                    prep_le(H, GOpt)
            end
    end.

prep_qlc_lc({simple_v1, PVar, LE_fun, L}, Opt, GOpt, _H) ->
    check_lookup_option(Opt, false),
    prep_simple_qlc(PVar, L, eval_le(LE_fun, GOpt), Opt);
prep_qlc_lc({single_v1, QFun, CodeF, Qdata0, _, MS, PosFun}, Opt, GOpt, _H) ->
    %% R10B
    check_join_option(Opt),
    %% Filter optional:
    [?qual_data(QNum, GoI_G, SI_G, {gen, LE_fun}) | Filter] = Qdata0, 
    Prep0 = eval_le(LE_fun, GOpt),
    Fs = [0], % (effect: the match specification is always run)
    {Done, _Skip, LookUp, Prep} = prep_gen(Prep0, PosFun, {MS,Fs}, Opt),
    check_lookup_option(Opt, LookUp),
    if
        Done =:= replace ->
            Prep;
        true ->
            Qdata = [?qual_data(QNum, GoI_G, SI_G, {gen, Prep}) | Filter],
            QOpt = undefined,
            prep_qlc(QFun, CodeF, Qdata, QOpt, Opt)
    end;
prep_qlc_lc({qlc_v1, QFun, CodeF, Qdata0, QOpt}, Opt, GOpt, _H) ->
    F = fun(?qual_data(_QNum, _GoI, _SI, fil)=QualData, ModGens) -> 
                {QualData, ModGens};
           (?qual_data(_QNum, _GoI, _SI, {gen, #join{}})=QualData, ModGens) ->
                {QualData, ModGens};
           (?qual_data(QNum, GoI, SI, {gen, LE_fun}), ModGens0) -> 
                Prep1 = eval_le(LE_fun, GOpt),
                {Prep, ModGens} = 
                    prep_generator(QNum, Prep1, QOpt, Opt, ModGens0),
                {?qual_data(QNum, GoI, SI, {gen, Prep}), ModGens}
        end,
    {Qdata, ModGens} = lists:mapfoldl(F, [], Qdata0),
    SomeLookUp = lists:keysearch(true, 2, ModGens) =/= false,
    check_lookup_option(Opt, SomeLookUp),
    case ModGens of
        [{_QNum, _LookUp, all, OnePrep}] ->
            check_join_option(Opt),
            OnePrep;
        _ -> 
            Prep0 = prep_qlc(QFun, CodeF, Qdata, QOpt, Opt),
            SkipFs = lists:flatmap(fun({_QNum,_LookUp,Fs,_Prep}) -> Fs 
                                   end, ModGens),
            Prep1 = skip_lookup_filters(Prep0, SkipFs),
            prep_join(Prep1, QOpt, Opt)
    end;
prep_qlc_lc(_, _Opt, _GOpt, H) ->
    erlang:error({unsupported_qlc_handle, #qlc_handle{h = H}}).

prep_generator(QNum, Prep0, QOpt, Opt, ModGens) ->
    PosFun = constants(QOpt, QNum),
    MSFs = case match_specs(QOpt, QNum) of
               undefined ->
                   {no_match_spec, []};
               {_, _}=MSFs0 ->
                   MSFs0
         end,
    case prep_gen(Prep0, PosFun, MSFs, Opt) of
        {replace, Fs, LookUp, Prep} ->
            {Prep, [{QNum,LookUp,Fs,Prep} | ModGens]};
        {skip, SkipFils, LookUp, Prep} ->
            {Prep, [{QNum,LookUp,SkipFils,Prep} | ModGens]};
        {no, _Fs, _LookUp, Prep} ->
            {Prep, ModGens}
    end.

prep_gen(#prepared{qh = LE0}=Prep0, PosFun, {MS, Fs}, Opt) ->
    {LuV, {STag,SkipFils}} = find_const_positions(LE0, PosFun, Opt),
    LU = LuV =/= false,
    case LE0 of
        #qlc_table{lu_vals = LuV0, ms = MS0} when LuV0 =/= undefined; 
                                                  MS0 =/= no_match_spec ->
            {no, [], false, Prep0};
        #qlc_table{} when MS =/= no_match_spec, LU ->
            MS1 = if 
                      Fs =:= SkipFils; STag =:= Fs ->
                          %% The guard of the match specification 
                          %% is covered by the lookup.
                          case MS of
                              [{'$1',_Guard,['$1']}] -> % no transformation
                                  no_match_spec;
                              [{Head,_Guard,Body}] ->
                                  [{Head,[],Body}] % true guard
                          end;
                      true ->
                          MS
                  end,
            Prep = Prep0#prepared{qh = LE0#qlc_table{lu_vals = LuV,ms = MS1}},
            {replace, Fs, LU, Prep};
        #qlc_table{} when LU ->
            Prep = Prep0#prepared{qh = LE0#qlc_table{lu_vals = LuV}},
            {skip, SkipFils, LU, Prep};
        #qlc_table{trav_MS = true} when MS =/= no_match_spec ->
            Prep = Prep0#prepared{qh = LE0#qlc_table{ms = MS}},
            {replace, Fs, false, may_create_simple(Opt, Prep)};
        #qlc_list{l = []} -> % unique and cached
            {replace, Fs, false, Prep0};
        #qlc_list{ms = no_match_spec} when MS =/= no_match_spec ->
            Prep = Prep0#prepared{qh = LE0#qlc_list{ms = MS}, 
                                  is_cached = false},
            {replace, Fs, false, may_create_simple(Opt, Prep)};
        #qlc_list{} when MS =/= no_match_spec ->
            ListMS = #qlc_list{l = Prep0, ms = MS},
            LE = #prepared{qh = ListMS, is_cached = false},
            {replace, Fs, false, may_create_simple(Opt, LE)};
        _ ->
            {no, [], false, Prep0}
    end.

-define(SIMPLE_QVAR, 'SQV').

may_create_simple(#qlc_opt{unique = Unique, cache = Cache} = Opt, 
                  #prepared{is_cached = IsCached, 
                            is_unique_objects = IsUnique} = Prep) ->
    if 
        Unique and not IsUnique; (Cache =/= false) and not IsCached ->
            prep_simple_qlc(?SIMPLE_QVAR, 1, Prep, Opt);
        true ->
            Prep
    end.

%% Sorted. Var kommer den ifrån? Påverkas den av unique&cache=list?
prep_simple_qlc(PVar, Line, LE, Opt) ->
    check_join_option(Opt),
    #prepared{is_cached = IsCached, 
              sort_info = SortInfo, sorted = Sorted,
              is_unique_objects = IsUnique} = LE,
    #qlc_opt{unique = Unique, cache = Cache} = Opt,
    Cachez = if
                 Unique -> Cache;
                 not IsCached -> Cache;
                 true -> false
             end,
    Optz = #optz{unique = Unique and not IsUnique, 
                 cache = Cachez, opt = Opt},
    QLC = #simple_qlc{p = PVar, le = LE, line = Line, 
                      init_value = not_a_list, optz = Optz},
    %% LE#prepared.join is not copied
    #prepared{qh = QLC, is_unique_objects = IsUnique or Unique, 
              sort_info = SortInfo, sorted = Sorted,
              is_cached = IsCached or (Cachez =/= false)}.

prep_sort(#qlc_sort{h = #prepared{sorted = yes}=Prep}, _Opt) ->
    Prep;
prep_sort(#qlc_sort{h = #prepared{is_unique_objects = IsUniqueObjs}}=Q, Opt) ->
    S1 = sort_unique(IsUniqueObjs, Q),
    S = sort_tmpdir(S1, Opt),
    {SortInfo, Sorted} = sort_sort_info(S),
    #prepared{qh = S, is_cached = true, sort_info = SortInfo,
              sorted = Sorted,
              is_unique_objects = S#qlc_sort.unique or IsUniqueObjs}.

prep_qlc(QFun, CodeF, Qdata0, QOpt, Opt) ->
    #qlc_opt{unique = Unique, cache = Cache, join = Join} = Opt,
    Optz = #optz{unique = Unique, cache = Cache, 
                 join_option = Join, opt = Opt},
    {Qdata, SortInfo} = qlc_sort_info(Qdata0, QOpt),
    QLC = #qlc{lcf = QFun, codef = CodeF, qdata = Qdata, 
               init_value = not_a_list, optz = Optz},
    #prepared{qh = QLC, sort_info = SortInfo,
              is_unique_objects = Unique, 
              is_cached = Cache =/= false}.

%% 'sorted', 'sorted_info', and 'sorted_info2' are used to avoid
%% sorting on a key when there is no need to sort on the key. 'sorted'
%% is set by qlc:sort() only; its purpose is to assure that if columns
%% 1 to i are constant, then column i+1 is key-sorted (always true if
%% the tuples are sorted). Note: the implementation is (too?) simple.
%% For instance, each column is annotated with 'ascending' or
%% 'descending' (not yet). More exact would be, as examples, 'always
%% ascending' and 'ascending if all preceding columns are constant'.
%%
%% The 'size' of the template is not used (size_of_qualifier(QOpt, 0)).

qlc_sort_info(Qdata, undefined) -> % single_v1, R10B
    F = fun(?qual_data(QNum, GoI, SI, 
                       {gen, #prepared{sort_info = SI}=Prep})) ->
                NPrepLE = Prep#prepared{sort_info2 = SI},
                ?qual_data(QNum, GoI, SI, {gen, NPrepLE});
           (Qd) -> Qd
        end,
    {lists:map(F, Qdata), []};
qlc_sort_info(Qdata0, QOpt) ->
    F = fun(?qual_data(_QNum, _GoI, _SI, fil)=Qd, Info) -> 
                {Qd, Info};
           (?qual_data(_QNum, _GoI, _SI, {gen, #join{}})=Qd, Info) ->
                {Qd, Info};
           (?qual_data(QNum, GoI, SI, {gen, PrepLE0}), Info) -> 
                PrepLE = sort_info(PrepLE0, QNum, QOpt),
                Qd = ?qual_data(QNum, GoI, SI, {gen, PrepLE}),
                I = [{{Column,Order}, [{traverse,QNum,C}]} ||
                        {{C,Order},What} <- PrepLE#prepared.sort_info2,
                        What =:= [], % Something else later...
                        Column <- equal_template_columns(QOpt, {QNum,C})],
                {Qd, [I | Info]}
        end,
    {Qdata, SortInfoL} = lists:mapfoldl(F, [], Qdata0),
    SortInfo0 = [{{Pos,Ord}, [template]} || 
                    Pos <- constant_columns(QOpt, 0), 
                    Ord <- orders(yes)]
           ++ lists:append(SortInfoL),
    SortInfo = family_union(SortInfo0),
    {Qdata, SortInfo}.

sort_info(#prepared{sort_info = SI, sorted = S} = Prep, QNum, QOpt) ->
    SI1 = [{{C,Ord},[]} || 
              S =/= no, 
              is_integer(Sz = size_of_qualifier(QOpt, QNum)), 
              Sz > 0, % the size of the pattern
              (NConstCols = size_of_constant_prefix(QOpt, QNum)) < Sz, 
              C <- [NConstCols+1],
              Ord <- orders(S)]
       ++ [{{Pos,Ord},[]} || Pos <- constant_columns(QOpt, QNum),
                             Ord <- orders(yes)]
       ++ [{PosOrd,[]} || {PosOrd,_} <- SI],
    SI2 = lists:usort(SI1),
    Prep#prepared{sort_info2 = SI2}. 

%orders(descending=O) ->
%    [O];
orders(ascending=O) ->
    [O];
orders(yes) ->
    [ascending
%   ,descending
    ].

sort_unique(true, #qlc_sort{opts = SortOptions, keypos = sort}=Sort) ->
    Sort#qlc_sort{unique = false, 
                  opts = lists:keydelete(unique, 
                                         1, 
                                         lists:delete(unique, SortOptions))};
sort_unique(_, Sort) ->
    Sort.

sort_tmpdir(S, #qlc_opt{tmpdir = ""}) ->
    S;
sort_tmpdir(S, Opt) ->
    S#qlc_sort{tmpdir = Opt#qlc_opt.tmpdir}.

short_list(L) ->
    %% length(L) < 2: all elements are known be equal
    #prepared{qh = #qlc_list{l = L}, sorted = yes, is_unique_objects = true, 
              is_cached = true}.
    
find_const_positions(#qlc_table{info_fun = IF, lookup_fun = LU_fun}, 
                     PosFun, #qlc_opt{max_lookup = Max, lookup = Lookup}) 
           when is_function(LU_fun), is_function(PosFun), is_function(IF),
                Lookup =/= false ->
    case call(IF, keypos, undefined, [])  of
        undefined ->
            Indices = call(IF, indices, undefined, []),
            find_const_position_idx(Indices, PosFun, Max, []);
        KeyPos ->
            case pos_vals(KeyPos, PosFun(KeyPos), Max) of
                false ->
                    find_const_position_idx(IF(indices), PosFun, Max, []);
                PosValuesSkip ->
                    PosValuesSkip
            end
    end;
find_const_positions(_, _PosFun, _Opt0) ->
    {false, {some,[]}}.

find_const_position_idx([I | Is], PosFun, Max, L0) ->
    case pos_vals(I, PosFun(I), Max) of
        false ->
            find_const_position_idx(Is, PosFun, Max, L0);
        {{_Pos, Values}, _SkipFils}=PosValuesFils ->
            L = [{length(Values), PosValuesFils} | L0],
            find_const_position_idx(Is, PosFun, Max, L)
    end;
find_const_position_idx(_, _PosFun, _Max, []) ->
    {false, {some,[]}};
find_const_position_idx(_, _PosFun, _Max, L) ->
    [{_,PVF} | _] = lists:sort(L),
    PVF.

pos_vals(Pos, {usort_needed, Values, SkipFils}, Max) ->
    pos_vals_max(Pos, lists:usort(Values), SkipFils, Max);
pos_vals(Pos, {values, Values, SkipFils}, Max) ->
    pos_vals_max(Pos, Values, SkipFils, Max);
pos_vals(Pos, {Tag, Values}, Max) ->
    %% R10B
    pos_vals(Pos, {Tag, Values, {some,[]}}, Max);
pos_vals(_Pos, _, _Max) ->
    false.

%% length(Values) >= 1
pos_vals_max(Pos, Values, Skip, Max) when Max =:= -1; Max >= length(Values) ->
    {{Pos, Values}, Skip};
pos_vals_max(_Pos, _Value, _Skip, _Max) ->
    false.

skip_lookup_filters(Prep, []) ->
    Prep;
skip_lookup_filters(#prepared{qh = #qlc{qdata = Qdata0}=QLC}=Prep, SkipFs) ->
    Qdata = [case lists:member(QNum, SkipFs) of
                 true ->
                     ?qual_data(QNum, GoI, ?SKIP, fil);
                 false ->
                     Qd
             end || ?qual_data(QNum, GoI, _, _)=Qd <- Qdata0],
    Prep#prepared{qh = QLC#qlc{qdata = Qdata}}.

prep_join(Prep, QOpt, Opt) ->
    case join_opt(QOpt) of
        undefined ->
            check_join_option(Opt),
            Prep;
        EqualMatch ->
            {Ix, M} = case EqualMatch of
                          {Equal, Match} ->
                              {Ix0, _} = pjoin(Match, Prep, QOpt, Opt),
                              {_, M0} = pjoin(Equal, Prep, QOpt, Opt),
                              {Ix0, M0};
                          _ ->
                              pjoin(EqualMatch, Prep, QOpt, Opt)
                      end,
            SI = family_union(Prep#prepared.sort_info ++ M),
            Prep#prepared{join = {Ix, M}, sort_info = SI}
    end.

%% The parse transform ensures that only two tables are involved.
pjoin(QCsL, #prepared{qh = #qlc{qdata = QData}}, QOpt, 
      #qlc_opt{join = JoinOpt}) ->
    LuJoin = (JoinOpt =:= any) or (JoinOpt =:= lookup),
    MJoin = (JoinOpt =:= any) or (JoinOpt =:= merge),
    lists:foldl(fun(QCs, {Ix0, M0}) -> 
                        {Ix,M} = pref_join(QCs, QData, QOpt, LuJoin, MJoin),
                        {Ix0++Ix, M0++M}
                end, {[],[]}, QCsL).

pref_join([{Q1,Cs1},{Q2,Cs2}], QData, QOpt, LuJoin, MJoin) ->
    {Is1, Sort1} = join_qual_data(QData, Q1),
    {Is2, Sort2} = join_qual_data(QData, Q2),
    Lu1 = [pref_lookup(Q1, C1, Q2, C2, QOpt) ||
              LuJoin, C2 <- Is2, lists:member(C2, Cs2), C1 <- Cs1],
    Lu2 = [pref_lookup(Q2, C2, Q1, C1, QOpt) ||
              LuJoin, C1 <- Is1, lists:member(C1, Cs1), C2 <- Cs2],
    Merge = lists:append([pref_merge(Q1, C1, Q2, C2, Sort1, Sort2, QOpt) ||
                             MJoin, C1 <- Cs1, C2 <- Cs2]),
    {family(Lu1 ++ Lu2), family_union(Merge)}.

pref_lookup(Q1, C1, Q2, C2, QOpt) ->
    {{Q1,C1,Q2,C2},{lookup,eq_template_columns(QOpt, {Q1,C1})}}.

pref_merge(Q1, C1, Q2, C2, Sort1, Sort2, QOpt) ->
    Col1 = {Q1,C1},
    Col2 = {Q2,C2},
    DoSort = [QC || {{_QNum,Col}=QC,SortL} <- [{Col1,Sort1}, {Col2,Sort2}],
                    lists:keysearch({Col, ascending}, 1, SortL) =:= false],
    J = [{{Q1,C1,Q2,C2}, {merge,DoSort}}],
    %% true = (QOpt(template))(Col1, '==') =:= (QOpt(template))(Col2, '==')
    [{{Column, ascending}, J} || 
        Column <- equal_template_columns(QOpt, Col1)] ++ [{other, J}].

join_qual_data(QData, QNum) ->
    case lists:keysearch(QNum, 1, QData) of
        {value, ?qual_data(QNum, _, _, {gen, PrepLE})} ->
            #prepared{sort_info2 = SortInfo} = PrepLE,
            {join_indices(PrepLE), SortInfo}
    end.

join_indices(#prepared{qh = #qlc_table{info_fun = IF, 
                                       ms = no_match_spec,
                                       lu_vals = undefined}}) ->
    KpL = case call(IF, keypos, undefined, []) of
              undefined -> [];
              Kp -> [Kp]
          end,
    case call(IF, indices, undefined, []) of
        undefined -> KpL;
        Is0 -> KpL ++ Is0
    end;
join_indices(_Prep) ->
    [].

table_sort_info(#qlc_table{info_fun = IF}) ->
    case call(IF, is_sorted_key, undefined, []) of
        undefined -> 
            {[], no};
        false -> 
            {[], no};
        true -> 
            case call(IF, keypos, undefined, []) of
                undefined -> % strange
                    {[], no};
                KeyPos ->
                    {[{{KeyPos,ascending},[]}], no}
            end
    end.

sort_sort_info(#qlc_sort{keypos = sort, order = Ord0}) ->
    {[], sort_order(Ord0)};
sort_sort_info(#qlc_sort{keypos = {keysort,Kp0}, order = Ord0}) ->
    Kp = case Kp0 of
             [Pos | _] -> Pos;
             _ -> Kp0
         end,
    {[{{Kp,sort_order(Ord0)},[]}], no}.

sort_order(F) when is_function(F) ->
    no;
sort_order(Order) ->
    Order.

check_join_option(#qlc_opt{join = any}) ->
    ok;
check_join_option(#qlc_opt{join = Join}) ->
    erlang:error(no_join_to_carry_out, [{join,Join}]).

check_lookup_option(#qlc_opt{lookup = true}, false) ->
    erlang:error(no_lookup_to_carry_out, [{lookup,true}]);
check_lookup_option(_QOpt, _LuV) ->
    ok.

equal_template_columns(QOpt, QNumColumn) ->
    (QOpt(template))(QNumColumn, '==').

eq_template_columns(QOpt, QNumColumn) ->
    (QOpt(template))(QNumColumn, '=:=').

size_of_constant_prefix(QOpt, QNum) ->
    (QOpt(n_leading_constant_columns))(QNum).

constants(undefined, _QNum) ->
    no_column_fun;
constants(QOpt, QNum) ->
    (QOpt(constants))(QNum).

join_opt(undefined=U) ->
    U;
join_opt(QOpt) ->
    QOpt(join).

match_specs(undefined=U, _QNum) ->
    U;
match_specs(QOpt, QNum) ->
    (QOpt(match_specs))(QNum).

constant_columns(QOpt, QNum) ->
    (QOpt(constant_columns))(QNum).

size_of_qualifier(QOpt, QNum) ->
    (QOpt(size))(QNum).

%% Two optimizations are carried out:
%% 1. The first generator is never cached if the QLC expression itself
%% is cached. Since the answers do not need to be cached, the top-most
%% QLC expression is never cached either. Simple QLCs not holding any
%% options are removed. Simple QLCs are coalesced when possible.
%% 2. Merge join and lookup join is done if possible.

opt_le(#prepared{qh = #simple_qlc{le = LE0, optz = Optz0}=QLC}=Prep0, 
       GenNum) ->
    case LE0 of
        #prepared{qh = #simple_qlc{p = LE_Pvar, le = LE2, optz = Optz2}} ->
            %% Coalesce two simple QLC expressions.
            Cachez = case Optz2#optz.cache of
                         false -> Optz0#optz.cache;
                         Cache2 -> Cache2
                     end,
            Optz = Optz0#optz{cache = Cachez,
                              unique = Optz0#optz.unique or Optz2#optz.unique},
            PVar = if 
                       LE_Pvar =:= ?SIMPLE_QVAR -> QLC#simple_qlc.p;
                       true -> LE_Pvar
                   end,
            Prep = Prep0#prepared{qh = QLC#simple_qlc{p = PVar, le = LE2,
                                                      optz = Optz}},
            opt_le(Prep, GenNum);
        _ ->
            Optz1 = no_cache_of_first_generator(Optz0, GenNum),
            case {opt_le(LE0, 1), Optz1} of
                {LE, #optz{unique = false, cache = false}} ->
                    LE;
                {LE, _} ->
                    Prep0#prepared{qh = QLC#simple_qlc{le = LE, optz = Optz1}}
            end
    end;
opt_le(#prepared{qh = #qlc{qdata = Qdata0, optz = Optz0}=QLC}=Prep, GenNum) ->
    #optz{join_option = JoinOption, opt = Opt} = Optz0,
    JoinOption = Optz0#optz.join_option,
    {Join, DoSort} = 
        opt_join(Prep#prepared.join, JoinOption, Qdata0, Opt),
    F = fun(?qual_data(QNum, GoI, SI, {gen, #prepared{}=PrepLE}), GenNum1) ->
                NewPrepLE = maybe_sort(PrepLE, QNum, DoSort, Opt),
                {?qual_data(QNum, GoI, SI, {gen, opt_le(NewPrepLE, GenNum1)}),
                 GenNum1 + 1};
           (Qd, GenNum1) ->
                {Qd, GenNum1}
        end,
    {Qdata, _} = lists:mapfoldl(F, 1, Qdata0),
    Optz1 = no_cache_of_first_generator(Optz0, GenNum),
    Optz = Optz1#optz{fast_join = Join},
    Prep#prepared{qh = QLC#qlc{qdata = Qdata, optz = Optz}};
opt_le(#prepared{qh = #qlc_append{hl = HL}}=Prep, GenNum) ->
    Hs = lists:map(fun(H) -> opt_le(H, GenNum) end, HL),
    Prep#prepared{qh = #qlc_append{hl = Hs}};
opt_le(#prepared{qh = #qlc_sort{h = H}=Sort}=Prep, GenNum) ->
    Prep#prepared{qh = Sort#qlc_sort{h = opt_le(H, GenNum)}};
opt_le(Prep, _GenNum) ->
    Prep.

no_cache_of_first_generator(Optz, GenNum) when GenNum > 1 ->
    Optz;
no_cache_of_first_generator(Optz, 1) ->
    Optz#optz{cache = false}.

maybe_sort(LE, QNum, DoSort, Opt) ->
    case lists:keysearch(QNum, 1, DoSort) of
        {value, {QNum, Col}} ->
            TmpDir = Opt#qlc_opt.tmpdir,
            Opts = [{tmpdir,Dir} || Dir <- [TmpDir], Dir =/= ""],
            Sort = #qlc_sort{h = LE, keypos = {keysort, Col}, unique = false,
                             compressed = false, order = ascending,
                             opts = Opts, tmpdir = TmpDir},
            #prepared{qh = Sort, sorted = no, join = no};
        false ->
            LE
    end.

opt_join(Join, JoinOption, Qdata, Opt) ->
    %% prep_qlc_lc() assures that no unwanted join is carried out
    case Join of 
        {[{{Q1,C1,Q2,C2},[{lookup,_} | _]} | _], _} ->
            [Table2] = [Prep#prepared.qh || 
                           ?qual_data(QNum, _, _, {gen, Prep}) <- Qdata,
                           QNum =:= Q2],
            J = #qlc_join{kind = {lookup, Table2#qlc_table.lookup_fun}, 
                          q1 = Q1, c1 = C1, q2 = Q2, c2 = C2, opt = Opt},
            {J, []};
        {_, [{_KpOrder_or_other,[{{Q1,C1,Q2,C2},{merge,DoSort}}|_]}|_]} ->
            J = #qlc_join{kind = merge, opt = Opt,
                          q1 = Q1, c1 = C1, q2 = Q2, c2 = C2},
            {J, DoSort};
        {[],[]} when JoinOption =:= nested_loop ->
            {no, []};
        _ when JoinOption =/= any ->
            erlang:error(cannot_carry_out_join, [JoinOption]);
        _ ->
            {no, []}
    end.

%% -> {Objects, Post, LocalPost}
%% Post is a list of funs (closures) to run afterwards.
%% LocalPost should be run when all objects have been found (optimization).
setup_qlc(Prep, Setup) ->
    Post0 = [],
    setup_le(Prep, Post0, Setup).

setup_le(#prepared{qh = #simple_qlc{le = LE, optz = Optz}}, Post0, Setup) ->
    {Objs, Post, LocalPost} = setup_le(LE, Post0, Setup),
    unique_cache(Objs, Post, LocalPost, Optz);
setup_le(#prepared{qh = #qlc{lcf = QFun, qdata = Qdata, init_value = V, 
                             optz = Optz}}, Post0, Setup) ->
    {GoTo, FirstState, Post, LocalPost} = 
        setup_quals(Qdata, Post0, Setup, Optz),
    Objs = fun() -> QFun(FirstState, V, GoTo) end,
    unique_cache(Objs, Post, LocalPost, Optz);
setup_le(#prepared{qh = #qlc_table{post_fun = PostFun}=Table}, Post, Setup) ->
    H = table_handle(Table, Post, Setup),
    %% The pre fun has been called from table_handle():
    {H, [PostFun | Post], []};
setup_le(#prepared{qh = #qlc_append{hl = PrepL}}, Post0, Setup) ->
    F = fun(Prep, {Post1, LPost1}) -> 
                {Objs, Post2, LPost2} = setup_le(Prep, Post1, Setup),
                {Objs, {Post2, LPost1++LPost2}}
        end,
    {ObjsL, {Post, LocalPost}} = lists:mapfoldl(F, {Post0,[]}, PrepL),
    {fun() -> append_loop(ObjsL) end, Post, LocalPost};
setup_le(#prepared{qh = #qlc_sort{h = Prep, keypos = Kp, 
                                  unique = Unique, compressed = Compressed,
                                  order = Order, opts = SortOptions0, 
                                  tmpdir = TmpDir}}, Post0, Setup) ->
    SortOptions = sort_options_global_tmp(SortOptions0, TmpDir),
    case setup_le(Prep, Post0, Setup) of
        {L, Post, LocalPost} when is_list(L) ->
            Objs = sort_list(L, Order, Unique, Kp, SortOptions, Post),
            {Objs, Post, LocalPost};
        {Objs, Post, LocalPost} ->
            sort_handle(Objs, Kp, SortOptions, TmpDir, Compressed, 
                        Post, LocalPost)
    end;
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
              when (no_match_spec =:= MS); L =:= [] ->
    {L, Post, []};
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
                                   when is_list(L) ->
    {ets:match_spec_run(L, ets:match_spec_compile(MS)), Post, []};
setup_le(#prepared{qh = #qlc_list{l = H0, ms = MS}}, Post0, Setup) ->
    {Objs0, Post, LocalPost} = setup_le(H0, Post0, Setup),
    Objs = ets:match_spec_run(Objs0, ets:match_spec_compile(MS)),
    {Objs, Post, LocalPost}.

%% The goto table (a tuple) is created at runtime. It is accessed by
%% the generated code in order to find next clause to execute. For
%% generators there is also a fun; calling the fun runs the list
%% expression of the generator. There are two elements for a filter:
%% the first one is the state to go when the filter is false; the
%% other the state when the filter is true. There are three elements
%% for a generator G: the first one is the state of the generator
%% before G (or the stop state if there is no generator); the second
%% one is the state of the qualifier following the generator (or the
%% template if there is no next generator); the third one is the list
%% expression fun. 
%% There are also join generators which are "activated" when it is 
%% possbible to do a join.

setup_quals(Qdata, Post0, Setup, Optz) ->
    {GoTo0, Post1, LocalPost0} = 
        setup_quals(0, Qdata, [], Post0, [], Setup),
    GoTo1 = lists:keysort(1, GoTo0),
    FirstState0 = next_state(Qdata),
    {GoTo2, FirstState, Post, LocalPost1} =
        case Optz#optz.fast_join of
            #qlc_join{kind = merge, c1 = C1, c2 = C2, opt = Opt} = MJ ->
                MF = fun(_Rev, {H1, WH1}, {H2, WH2}) -> 
                             fun() -> 
                                  merge_join(WH1(H1), C1, WH2(H2), C2, Opt) 
                             end
                     end,
                setup_join(MJ, Qdata, GoTo1, FirstState0, MF, Post1);
            #qlc_join{kind = {lookup, LuF}, c1 = C1, c2 = C2} = LJ ->
                LF = fun(Rev, {H1, WH1}, {H2, WH2}) ->
                             {H, W} = if 
                                          Rev -> {H2, WH2};
                                          true -> {H1, WH1} 
                                      end,
                             fun() -> 
                                     lookup_join(W(H), C1, LuF, C2, Rev)
                             end
                     end,
                setup_join(LJ, Qdata, GoTo1, FirstState0, LF, Post1);
            no ->
                {flat_goto(GoTo1), FirstState0, Post1, []}
        end,
    GoTo = list_to_tuple(GoTo2),
    {GoTo, FirstState, Post, LocalPost0 ++ LocalPost1}.

setup_quals(GenLoopS, [?qual_data(_QNum,GoI,?SKIP,fil) | Qdata], 
            Gs, P, LP, Setup) ->
    %% ?SKIP causes runtime error. See also skip_lookup_filters().
    setup_quals(GenLoopS, Qdata, [{GoI,[?SKIP,?SKIP]} | Gs], P, LP, Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,_SI,fil) | Qdata], 
            Gs, P, LP, Setup) ->
    setup_quals(GenLoopS, Qdata, [{GoI,[GenLoopS,next_state(Qdata)]} | Gs], 
                P, LP, Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,_SI, {gen,#join{}}) | Qdata],
            Gs, P, LP, Setup) ->
    setup_quals(GenLoopS, Qdata, [{GoI,[?SKIP,?SKIP,?SKIP]} | Gs],P,LP,Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,SI,{gen,LE}) | Qdata], 
            Gs, P, LP, Setup) ->
    {V, NP, LP1} = setup_le(LE, P, Setup),
    setup_quals(SI + 1, Qdata, [{GoI, [GenLoopS,next_state(Qdata),V]} | Gs], 
                NP, LP ++ LP1, Setup);
setup_quals(GenLoopS, [], Gs, P, LP, _Setup) ->
    {[{1,[GenLoopS]} | Gs], P, LP}.

%% Finds the qualifier in Qdata that performs the join between Q1 and
%% Q2, and sets it up using the handles already set up for Q1 and Q2.
%% Removes Q1 and Q2 from GoTo0 and updates the join qualifier in GoTo0.
%% Note: the parse transform has given each generator three slots
%% in the GoTo table. The position of these slots within the GoTo table
%% is fixed (at runtime).
%% (Assumes there is only one join-generator in Qdata0.)
setup_join(J, Qdata, GoTo0, FirstState0, JoinFun, Post0) ->
    #qlc_join{q1 = QNum1a, q2 = QNum2a, opt = Opt} = J,
    {?qual_data(_QN,JGoI,JSI,_), Rev, QNum1, QNum2, WH1, WH2, _CsFun} = 
        find_join_data(Qdata, QNum1a, QNum2a),
    [{GoI1,SI1}] = [{GoI,SI} ||
                       ?qual_data(QNum,GoI,SI,_) <- Qdata, QNum =:= QNum1],
    [{GoI2,SI2}] = [{GoI,SI} ||
                       ?qual_data(QNum,GoI,SI,_) <- Qdata, QNum =:= QNum2],

    [H1] = [H || {GoI,[_Back,_Forth,H]} <- GoTo0, GoI =:= GoI1],
    [{BackH2,H2}] = 
           [{Back,H} || {GoI,[Back,_Forth,H]} <- GoTo0, GoI =:= GoI2],
    H0 = JoinFun(Rev, {H1,WH1}, {H2,WH2}),
    %% The qlc expression options apply to the introduced qlc expr as well.
    {H, Post, LocalPost} = 
         unique_cache(H0, Post0, [], join_unique_cache(Opt)),
    [JBack] = [Back || {GoI,[Back,_,_]} <- GoTo0, GoI =:= GoI1],
    JForth = next_after(Qdata, SI1, QNum2),
    GoTo1 = lists:map(fun({GoI,_}) when GoI =:= JGoI -> 
                              {JGoI, [JBack, JForth, H]};
                         ({GoI,_}) when GoI =:= GoI1; GoI =:= GoI2 ->
                              {GoI, [?SKIP,?SKIP,?SKIP]}; % not necessary
                         (Go) ->
                              Go
                      end, GoTo0),
    GoTo = lists:map(fun(S) when S =:= SI1 -> 
                             JSI;
                        (S) when S =:= SI2 -> 
                             next_after(Qdata, S, QNum2);
                        (S) when S =:= SI1+1 -> 
                             JSI+1;
                        (S) when S =:= SI2+1, SI1 + 1 =:= BackH2  -> 
                             JSI+1;
                        (S) when S =:= SI2+1 -> 
                             BackH2;
                        (S) -> S
                     end, flat_goto(GoTo1)),
    FirstState = if
                     SI1 =:= FirstState0 -> JSI;
                     true -> FirstState0
                 end,
    {GoTo, FirstState, Post, LocalPost}.

join_unique_cache(#qlc_opt{cache = Cache, unique = Unique}=Opt) ->
    #optz{cache = Cache, unique = Unique, opt = Opt}.

flat_goto(GoTo) ->
    lists:flatmap(fun({_,L}) -> L end, GoTo).

next_after([?qual_data(_, _, S, _) | Qdata], S, QNum2) ->
    case Qdata of
        [?qual_data(QNum2, _, _, _) | Qdata1] ->
            next_state(Qdata1);
        _ -> 
            next_state(Qdata)
    end;
next_after([_ | Qdata], S, QNum2) ->
    next_after(Qdata, S, QNum2).

next_state([?qual_data(_,_,_,{gen,#join{}}) | Qdata]) ->
    next_state(Qdata);
next_state([?qual_data(_,_,?SKIP,fil) | Qdata]) ->
    %% see skip_lookup_filters()
    next_state(Qdata);
next_state([?qual_data(_,_,S,_) | _]) ->
    S;
next_state([]) ->
    template_state().

find_join_data(Qdata, QNum1, QNum2) ->
    [QRev] = [{Q,Rev,QN1,QN2,H1,H2,CsF} || 
                 ?qual_data(_QN,_GoI,_SI,
                            {gen,#join{q1 = QN1,q2 = QN2, 
                                       wh1 = H1, wh2 = H2,
                                       cs_fun = CsF}})= Q <- Qdata,
                 if 
                     QN1 =:= QNum1, QN2 =:= QNum2 ->
                         not (Rev = false);
                     QN1 =:= QNum2, QN2 =:= QNum1 ->
                         Rev = true;
                     true -> 
                         Rev = false
                 end],
    QRev.

table_handle(#qlc_table{trav_fun = TraverseFun, trav_MS = TravMS, 
                        pre_fun = PreFun, lookup_fun = LuF, 
                        parent_fun = ParentFun, lu_vals = LuVals, ms = MS}, 
             Post, Setup) ->
    #setup{parent = Parent} = Setup,
    ParentValue = 
        if 
            ParentFun =:= undefined ->
                undefined;
            Parent =:= self() ->
                try
                    ParentFun() 
                catch Class:Reason ->
                    post_funs(Post),
                    erlang:raise(Class, Reason, erlang:get_stacktrace())
                end;
            true ->
                case monitor_request(Parent, {parent_fun, ParentFun}) of
                    error -> % parent has died
                        post_funs(Post),
                        exit(normal);
                    {value, Value} ->
                        Value;
                    {parent_fun_caught, Class, Reason, Stacktrace} ->
                        %% No use augmenting Stacktrace here.
                        post_funs(Post),
                        erlang:raise(Class, Reason, Stacktrace)
                end
        end,
    StopFun = 
        if 
            Parent =:= self() ->
                undefined;
            true ->
                Cursor = #qlc_cursor{c = {self(), Parent}},
                fun() -> delete_cursor(Cursor) end
        end,
    PreFunArgs = [{parent_value, ParentValue}, {stop_fun, StopFun}],
    _ = call(PreFun, PreFunArgs, ok, Post),
    case LuVals of
        {Pos, Vals} when MS =:= no_match_spec ->
            LuF(Pos, Vals);
        {Pos, Vals} ->
            case LuF(Pos, Vals) of
                [] -> 
                    [];
                Objs when is_list(Objs) -> 
                    ets:match_spec_run(Objs, 
                                       ets:match_spec_compile(MS));
                Error ->
                    throw_error(Error)
            end;
        _ when not TravMS ->
            TraverseFun;
        _ when MS =:= no_match_spec ->
            fun() -> TraverseFun([{'$1',[],['$1']}]) end;
        _ ->
            fun() -> TraverseFun(MS) end
    end.

-define(CHUNK_SIZE, 64*1024).

open_file(FileName, Extra, Post) ->
    case file:open(FileName, [read, raw, binary | Extra]) of
        {ok, Fd} ->
            {fun() -> 
                     case file:position(Fd, bof) of
                         {ok, 0} -> ok;
                         Error -> file_error(FileName, Error)
                     end,
                     TF = fun([], _) -> [];
                             (Ts, C) -> lists:reverse(Ts, C) 
                          end,
                     file_loop_read(<<>>, ?CHUNK_SIZE, 0, {Fd, FileName}, TF) 
             end, Fd};
        Error ->
            post_funs(Post),
            file_error(FileName, Error)
    end.

file_loop(Bin, Pos, Fd_FName, Ts, TF) ->
    case Bin of
        <<_:Pos/unit:8, Size:4/unit:8, B:Size/binary, _/binary>> ->
            Term = try
                       binary_to_term(B)
                   catch _:_ ->
                       {_Fd, FileName} = Fd_FName,
                       throw_reason({bad_object, FileName})
                   end,
             file_loop(Bin, Pos + 4 + Size, Fd_FName, [Term | Ts], TF);
        <<_:Pos/unit:8, Size:4/unit:8, B/binary>> when Ts =:= [] ->
             file_loop_read(Bin, Size - size(B) + 4, Pos, Fd_FName, TF);
        <<_:Pos/unit:8, Size:4/unit:8, _/binary>> ->
            C = fun() -> file_loop_read(Bin, Size+4, Pos, Fd_FName, TF) end,
            TF(Ts, C);
        B -> 
            C = fun() -> file_loop_read(B, ?CHUNK_SIZE, Pos, Fd_FName,TF) end,
            TF(Ts, C)
    end.

%% After power failures (and only then) files with corrupted Size
%% fields have been observed in a disk_log file. If file:read/2 is
%% asked to read a huge amount of data the emulator may crash. Nothing
%% has been done here to prevent such crashes (by inspecting
%% BytesToRead in some way) since temporary files will never be read
%% after a power failure.
file_loop_read(B, MinBytesToRead, Pos, {Fd, FileName}=Fd_FName, TF) ->
    BytesToRead = lists:max([?CHUNK_SIZE, MinBytesToRead]),
    case file:read(Fd, BytesToRead) of
        {ok, Bin} when size(B) =:= Pos ->
            file_loop(Bin, 0, Fd_FName, [], TF);
        {ok, Bin} ->
            case B of 
                <<_:Pos/unit:8, Size:4/unit:8, Tl/binary>> 
                               when size(Bin)+size(Tl) >= Size ->
                    {B1, B2} = split_binary(Bin, Size - size(Tl)),
                    {_, B0} = split_binary(B, Pos),
                    Foo = fun([T], Fun) -> [T | Fun] end,
                    %% TF should be applied exactly once.
                    [T | Fun] = file_loop(list_to_binary([B0, B1]), 0,
                                          Fd_FName, [], Foo),
                    true = is_function(Fun),
                    file_loop(B2, 0, Fd_FName, [T], TF);
                _ ->
                    file_loop(list_to_binary([B, Bin]), Pos, Fd_FName, [], TF)
            end;
        eof when size(B) =:= Pos ->
            TF([], foo);
        eof ->
            throw_reason({bad_object, FileName});
        Error ->
            file_error(FileName, Error)
    end.

sort_cursor_input(H, NoObjects) ->
    fun(close) ->
            ok;
       (read) ->
            sort_cursor_input_read(H, NoObjects)
    end.
                    
sort_cursor_list_output(TmpDir, Z) ->
    fun(close) ->
            {terms, []};
       ({value, NoObjects}) ->
            fun(BTerms) when length(BTerms) =:= NoObjects ->
                    {terms, BTerms};
               (BTerms) ->
                    FName = tmp_filename(TmpDir),
                    case file:open(FName, [write, raw, binary | Z]) of
                        {ok, Fd} ->
                            WFun = write_terms(FName, Fd),
                            WFun(BTerms);
                        Error ->
                            file_error(FName, Error)
                    end
            end
    end.

sort_options_global_tmp(S, "") ->
    S;
sort_options_global_tmp(S, TmpDir) ->
    [{tmpdir,TmpDir} | lists:keydelete(tmpdir, 1, S)].

tmp_filename(TmpDirOpt) ->
    U = "_",
    Node = node(),
    Pid = os:getpid(),
    {MSecs,Secs,MySecs} = erlang:now(),
    F = lists:concat([?MODULE,U,Node,U,Pid,U,MSecs,U,Secs,U,MySecs]),
    TmpDir = case TmpDirOpt of
                 "" ->
                     {ok, CurDir} = file:get_cwd(),
                     CurDir;
                 TDir ->
                     TDir
             end,
    filename:join(filename:absname(TmpDir), F).

write_terms(FileName, Fd) ->
    fun(close) ->
            file:close(Fd),
            {file, FileName};
       (BTerms) ->
            case file:write(Fd, size_bin(BTerms, [])) of
                ok ->
                    write_terms(FileName, Fd);
                Error ->
                    file:close(Fd),
                    file_error(FileName, Error)
            end
    end.

size_bin([], L) ->
    L;
size_bin([BinTerm | BinTerms], L) ->
    size_bin(BinTerms, [L, <<(size(BinTerm)):4/unit:8>> | BinTerm]).

sort_cursor_input_read([], NoObjects) ->
    {end_of_input, NoObjects};
sort_cursor_input_read([Object | Cont], NoObjects) ->
    {[term_to_binary(Object)], sort_cursor_input(Cont, NoObjects + 1)};
sort_cursor_input_read(F, NoObjects) when is_function(F) ->
    sort_cursor_input_read(F(), NoObjects);
sort_cursor_input_read(Term, _NoObjects) ->
    throw_error(Term).

unique_cache(L, Post, LocalPost, Optz) when is_list(L) ->
    case Optz#optz.unique of
        true -> 
            {unique_sort_list(L), Post, LocalPost};
        false ->
            %% If Optz#optz.cache then an ETS table could be used. 
            {L, Post, LocalPost}
    end;
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = false}) ->
    {H, Post, LocalPost};
unique_cache(H, Post, LocalPost, #optz{unique = true, cache = false}) ->
    E = ets:new(qlc, [set, private]),
    {fun() -> no_dups(H, E) end, [del_table(E) | Post], LocalPost};
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = true}) ->
    E = ets:new(qlc, [set, private]),
    {L, P} = unique_cache_post(E),
    {fun() -> cache(H, E, LocalPost) end, [P | Post], [L]};
unique_cache(H, Post, LocalPost, #optz{unique = true, cache = true}) ->
    UT = ets:new(qlc, [bag, private]),
    MT = ets:new(qlc, [set, private]),
    {L1, P1} = unique_cache_post(UT),
    {L2, P2} = unique_cache_post(MT),
    {fun() -> ucache(H, UT, MT, LocalPost) end, [P1, P2 | Post], [L1, L2]};
unique_cache(H, Post, LocalPost, #optz{unique = false, cache = list}=Optz) ->
    Ref = make_ref(),
    F = del_lcache(Ref),
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList} = Optz#optz.opt,
    {fun() -> lcache(H, Ref, LocalPost, TmpDir, MaxList) end, 
     [F | Post], [F]};
unique_cache(H, Post0, LocalPost0, #optz{unique = true, cache = list}=Optz) ->
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList} = Optz#optz.opt,
    Size = if
               MaxList >= 1 bsl 31 ->
                   (1 bsl 31) - 1;
               MaxList =:= 0 ->
                   1;
               true ->
                   MaxList
           end,
    SortOptions = [{size, Size}, {tmpdir, TmpDir}],
    USortOptions = [{unique, true} | SortOptions],
    {UH, Post1, LocalPost1} = 
        sort_handle(tag_objects(H, 1), {keysort, 1}, USortOptions, 
                    TmpDir, [], Post0, LocalPost0),
    {SH, Post, LocalPost} = sort_handle(UH, {keysort, 2}, SortOptions, 
                                        TmpDir, [], Post1, LocalPost1),
    %% Every traversal untags the objects...
    {fun() -> untag_objects(SH) end, Post, LocalPost}.

unique_cache_post(E) ->
    {empty_table(E), del_table(E)}.

unique_sort_list(L) ->
    E = ets:new(qlc, [set, private]),
    unique_list(L, E, []).

unique_list([], E, L) ->
    true = ets:delete(E),
    lists:reverse(L);
unique_list([Object | Objects], E, L) ->
    case ets:member(E, Object) of
        false ->
            true = ets:insert(E, {Object}),
            unique_list(Objects, E, [Object | L]);
        true ->
            unique_list(Objects, E, L)
    end.

sort_list(L, CFun, true, sort, _SortOptions, _Post) when is_function(CFun) ->
    lists:usort(CFun, L);
sort_list(L, CFun, false, sort, _SortOptions, _Post) when is_function(CFun) ->
    lists:sort(CFun, L);
sort_list(L, ascending, true, sort, _SortOptions, _Post) ->
    lists:usort(L);
sort_list(L, descending, true, sort, _SortOptions, _Post) ->
    lists:reverse(lists:usort(L));
sort_list(L, ascending, false, sort, _SortOptions, _Post) ->
    lists:sort(L);
sort_list(L, descending, false, sort, _SortOptions, _Post) ->
    lists:reverse(lists:sort(L));
sort_list(L, Order, Unique, {keysort, Kp}, _SortOptions, _Post) 
           when is_integer(Kp), is_atom(Order) ->
    case {Order, Unique} of
        {ascending, true} ->
            lists:ukeysort(Kp, L);
        {ascending, false} ->
            lists:keysort(Kp, L);
        {descending, true} ->
            lists:reverse(lists:ukeysort(Kp, L));
        {descending, false} ->
            lists:reverse(lists:keysort(Kp, L))
    end;
sort_list(L, _Order, _Unique, Sort, SortOptions, Post) ->
    In = fun(_) -> {L, fun(_) -> end_of_input end} end,
    Out = sort_list_output([]),
    TSortOptions = [{format,term} | SortOptions],
    do_sort(In, Out, Sort, TSortOptions, Post).

sort_list_output(L) ->
    fun(close) ->
            lists:append(lists:reverse(L));
       (Terms) ->
            sort_list_output([Terms | L])
    end.

sort_handle(H, Kp, SortOptions, TmpDir, Compressed, Post, LocalPost) ->
    In = sort_cursor_input(H, 0),
    Out = sort_cursor_list_output(TmpDir, Compressed),
    Reply = do_sort(In, Out, Kp, SortOptions, Post),
    case Reply of
        {file, FileName} ->
            {F, Fd} = open_file(FileName, Compressed, Post),
            P = fun() -> _ = file:close(Fd),
                         _ = file:delete(FileName)
                end,
            {F, [P | Post], LocalPost};
        {terms, BTerms} ->
            try 
                {lists:map(fun binary_to_term/1, BTerms), Post, LocalPost}
            catch Class:Reason ->
                post_funs(Post),
                erlang:raise(Class, Reason, erlang:get_stacktrace())
            end
    end.

do_sort(In, Out, Sort, SortOptions, Post) ->
    try
        case do_sort(In, Out, Sort, SortOptions) of
            {error, Reason} -> throw_reason(Reason);
            Reply -> Reply
        end
    catch Class:Term ->
        post_funs(Post),
        erlang:raise(Class, Term, erlang:get_stacktrace())
    end.

do_sort(In, Out, sort, SortOptions) ->
    file_sorter:sort(In, Out, SortOptions);
do_sort(In, Out, {keysort, KeyPos}, SortOptions) ->
    file_sorter:keysort(KeyPos, In, Out, SortOptions).

del_table(Ets) ->
    fun() -> true = ets:delete(Ets) end.

empty_table(Ets) ->
    fun() -> true = ets:delete_all_objects(Ets) end.

append_loop([[_ | _]=L]) ->
    L;
append_loop([F]) ->
    F();
append_loop([L | Hs]) ->
    append_loop(L, Hs).

append_loop([], Hs) ->
    append_loop(Hs);
append_loop([Object | Cont], Hs) ->    
    [Object | fun() -> append_loop(Cont, Hs) end];
append_loop(F, Hs) when is_function(F) ->
    append_loop(F(), Hs);
append_loop(Term, _Hs) ->
    throw_error(Term).

no_dups([]=Cont, UTab) ->
    true = ets:delete_all_objects(UTab),
    Cont;
no_dups([Object | Cont], UTab) ->
    case ets:member(UTab, Object)  of
        false ->
            true = ets:insert(UTab, {Object}),
            [Object | fun() -> no_dups(Cont, UTab) end];
        true ->
            no_dups(Cont, UTab)
    end;
no_dups(F, UTab) when is_function(F) ->
    no_dups(F(), UTab);
no_dups(Term, _UTab) ->
    throw_error(Term).

%% When all objects have been returned from a cached QLC expression,
%% the generators of the expression will never be called again, and so
%% the tables used by the generators (PostL) can be emptied.

cache(H, MTab, PostL) ->
    case ets:member(MTab, 0) of
        false ->
            true = ets:insert(MTab, {0}),
            cache(H, MTab, 1, PostL);
        true ->
            cache_recall(MTab, 1)
    end.

cache([]=Cont, _MTab, _SeqNo, PostL) ->
    local_post(PostL),
    Cont;
cache([Object | Cont], MTab, SeqNo, PostL) ->
    true = ets:insert(MTab, {SeqNo, Object}),
    [Object | fun() -> cache(Cont, MTab, SeqNo + 1, PostL) end];
cache(F, MTab, SeqNo, PostL) when is_function(F) ->
    cache(F(), MTab, SeqNo, PostL);
cache(Term, _MTab, _SeqNo, _PostL) ->
    throw_error(Term).

cache_recall(MTab, SeqNo) ->
    case ets:lookup(MTab, SeqNo) of
        []=Cont ->
            Cont;
        [{SeqNo, Object}] ->
            [Object | fun() -> cache_recall(MTab, SeqNo + 1) end]
    end.

ucache(H, UTab, MTab, PostL) ->
    case ets:member(MTab, 0) of
        false ->
            true = ets:insert(MTab, {0}),
            ucache(H, UTab, MTab, 1, PostL);
        true ->
            ucache_recall(UTab, MTab, 1)
    end.

ucache([]=Cont, _UTab, _MTab, _SeqNo, PostL) ->
    local_post(PostL),
    Cont;
ucache([Object | Cont], UTab, MTab, SeqNo, PostL) ->
    %% Always using 28 bits hash value...
    Hash = erlang:phash2(Object),
    case ets:lookup(UTab, Hash) of
        [] ->
            ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, PostL);
        HashSeqObjects ->
            case lists:keymember(Object, 3, HashSeqObjects) of
                true -> ucache(Cont, UTab, MTab, SeqNo, PostL);
                false -> ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, PostL)
            end
    end;
ucache(F, UTab, MTab, SeqNo, PostL) when is_function(F) ->
    ucache(F(), UTab, MTab, SeqNo, PostL);
ucache(Term, _UTab, _MTab, _SeqNo, _PostL) ->
    throw_error(Term).

ucache3(Object, Cont, Hash, UTab, MTab, SeqNo, PostL) ->
    true = ets:insert(UTab, {Hash, SeqNo, Object}),
    true = ets:insert(MTab, {SeqNo, Hash}),
    [Object | fun() -> ucache(Cont, UTab, MTab, SeqNo+1, PostL) end].

ucache_recall(UTab, MTab, SeqNo) ->
    case ets:lookup(MTab, SeqNo) of
        []=Cont ->
            Cont;
        [{SeqNo, Hash}] ->
            Object = case ets:lookup(UTab, Hash) of
                         [{Hash, SeqNo, Object0}] -> Object0;
                         HashSeqObjects -> 
                             {value, {Hash, SeqNo, Object0}} = 
                                 lists:keysearch(SeqNo, 2, HashSeqObjects),
                             Object0
                     end,
            [Object | fun() -> ucache_recall(UTab, MTab, SeqNo + 1) end]
    end.

-define(LCACHE_FILE(Ref), {Ref, '$_qlc_cache_tmpfiles_'}).

lcache(H, Ref, PostL, TmpDir, MaxList) ->
    Key = ?LCACHE_FILE(Ref),
    case get(Key) of
        undefined ->
            lcache1(H, {Key, PostL, TmpDir, MaxList}, MaxList, []);
        {file, _Fd, _TmpFile, F} ->
            F();
        L when is_list(L) ->
            L
    end.
    
lcache1([]=Cont, {Key, PostL, _TmpDir, _MaxList}, _Sz, Acc) ->
    local_post(PostL),
    case get(Key) of
        undefined -> 
            put(Key, lists:reverse(Acc));
        {file, Fd, TmpFile, _F} ->
            lcache_write(Fd, TmpFile, Acc)
    end,
    Cont;
lcache1(H, State, Sz, Acc) when Sz < 0 ->
    {Key, PostL, TmpDir, MaxList} = State,
    {FileName, Fd} = 
        case get(Key) of
            {file, Fd0, TmpFile, _F} ->
                {TmpFile, Fd0};
            undefined ->
                FName = tmp_filename(TmpDir),
                {F, Fd0} = open_file(FName, [write], PostL),
                put(Key, {file, Fd0, FName, F}),
                {FName, Fd0}
        end,
    lcache_write(Fd, FileName, Acc),
    lcache1(H, State, MaxList, []);
lcache1([Object | Cont], State, Sz0, Acc) ->
    Sz = decr_list_size(Sz0, Object),
    [Object | fun() -> lcache1(Cont, State, Sz, [Object | Acc]) end];
lcache1(F, State, Sz, Acc) when is_function(F) ->
    lcache1(F(), State, Sz, Acc);
lcache1(Term, _State, _Sz, _Acc) ->
    throw_error(Term).
    
lcache_write(Fd, FileName, L) ->
    (write_terms(FileName, Fd))(t2b(L, [])).

t2b([], Bs) ->
    Bs;
t2b([T | Ts], Bs) ->
    t2b(Ts, [term_to_binary(T) | Bs]).

del_lcache(Ref) ->
    fun() -> 
            Key = ?LCACHE_FILE(Ref),
            case get(Key) of
                undefined ->
                    ok;
                {file, Fd, TmpFile, _F} ->
                    _ = file:close(Fd),
                    _ = file:delete(TmpFile),
                    erase(Key);
                _L -> 
                    erase(Key)
            end
    end.

tag_objects([Object | Cont], I) ->
    [{Object, I} | fun() -> tag_objects(Cont, I + 1) end];
tag_objects([]=Cont, _I) ->
    Cont;
tag_objects(F, I) when is_function(F) ->
    tag_objects(F(), I);
tag_objects(T, _I) ->
    T.

untag_objects([{Object, _N} | Cont]) ->
    [Object | fun() -> untag_objects(Cont) end];
untag_objects([]=Cont) ->
    Cont;
untag_objects(F) when is_function(F) ->
    untag_objects(F());
untag_objects(T) ->
    %% Cannot happen.
    T.

%%% Merge join.
%%% Temporary files are used when many objects have the same key.

-define(JWRAP(E1, E2), [E1 | E2]).

-record(m, {id, tmpdir, max_list}).

merge_join([]=Cont, _C1, _T2, _C2, _Opt) ->
    Cont;
merge_join([E1 | L1], C1, L2, C2, Opt) ->
    #qlc_opt{tmpdir = TmpDir, max_list = MaxList} = Opt,
    M = #m{id = merge_join_id(), tmpdir = TmpDir, max_list = MaxList},
    merge_join2(E1, element(C1, E1), L1, C1, L2, C2, M);
merge_join(F1, C1, L2, C2, Opt) when is_function(F1) ->
    merge_join(F1(), C1, L2, C2, Opt);
merge_join(T1, _C1, _L2, _C2, _Opt) ->
    throw_error(T1).

merge_join1(_E2, _K2, []=Cont, _C1, _L2, _C2, M) ->
    end_merge_join(Cont, M);
merge_join1(E2, K2, [E1 | L1], C1, L2, C2, M) ->
    K1 = element(C1, E1),
    if 
        K1 == K2 ->
            same_keys2(E1, K1, L1, C1, L2, C2, E2, M);
        K1 > K2 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M);
        true -> % K1 < K2
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
merge_join1(E2, K2, F1, C1, L2, C2, M) when is_function(F1) ->
    merge_join1(E2, K2, F1(), C1, L2, C2, M);
merge_join1(_E2, _K2, T1, _C1, _L2, _C2, _M) ->
    throw_error(T1).

merge_join2(_E1, _K1, _L1, _C1, []=Cont, _C2, M) ->
    end_merge_join(Cont, M);
merge_join2(E1, K1, L1, C1, [E2 | L2], C2, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            same_keys2(E1, K1, L1, C1, L2, C2, E2, M);
        K1 > K2 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M);
        true -> % K1 < K2
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
merge_join2(E1, K1, L1, C1, F2, C2, M) when is_function(F2) ->
    merge_join2(E1, K1, L1, C1, F2(), C2, M);
merge_join2(_E1, _K1, _L1, _C1, T2, _C2, _M) ->
    throw_error(T2).

%% element(C2, E2_0) == K1
same_keys2(E1, K1, L1, C1, [], _C2, E2_0, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    loop_same_keys(E1, K1, L1, C1, [E2_0], Cont, M);
same_keys2(E1, K1, L1, C1, [E2 | L2]=L2_0, C2, E2_0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            same_keys1(E1, K1, L1, C1, E2, C2, E2_0, L2, M);
        K1 < K2 ->
            [?JWRAP(E1, E2_0) |
             fun() -> same_loop1(L1, K1, C1, E2_0, L2_0, C2, M) end]
    end;
same_keys2(E1, K1, L1, C1, F2, C2, E2_0, M) when is_function(F2) ->
    same_keys2(E1, K1, L1, C1, F2(), C2, E2_0, M);
same_keys2(E1, K1, L1, C1, T2, _C2, E2_0, M) ->
    Cont = fun(_L1b) -> throw_error(T2) end,
    loop_same_keys(E1, K1, L1, C1, [E2_0], Cont, M).

same_loop1([], _K1_0, _C1, _E2_0, _L2, _C2, M) ->
    end_merge_join([], M);
same_loop1([E1 | L1], K1_0, C1, E2_0, L2, C2, M) ->
    K1 = element(C1, E1),
    if
        K1 == K1_0 ->
            [?JWRAP(E1, E2_0) | 
             fun() -> same_loop1(L1, K1_0, C1, E2_0, L2, C2, M) end];
        K1_0 < K1 ->
            merge_join2(E1, K1, L1, C1, L2, C2, M)
    end;
same_loop1(F1, K1_0, C1, E2_0, L2, C2, M) when is_function(F1) ->
    same_loop1(F1(), K1_0, C1, E2_0, L2, C2, M);
same_loop1(T1, _K1_0, _C1, _E2_0, _L2, _C2, _M) ->
    throw_error(T1).

%% element(C2, E2_0) == K1, element(C2, E2) == K1_0
same_keys1(E1_0, _K1_0, [], _C1, E2, _C2, E2_0, _L2, M) ->
    end_merge_join([?JWRAP(E1_0, E2_0), ?JWRAP(E1_0, E2)], M);
same_keys1(E1_0, K1_0, [E1 | _]=L1, C1, E2, C2, E2_0, L2, M) ->
    K1 = element(C1, E1),
    if 
        K1_0 == K1 ->
            E2s = [E2, E2_0],
            Sz0 = decr_list_size(M#m.max_list, E2s),
            same_keys_cache(E1_0, K1_0, L1, C1, L2, C2, E2s, Sz0, M);
        K1_0 < K1  ->
            [?JWRAP(E1_0, E2_0), ?JWRAP(E1_0, E2) |
             fun() -> same_keys(K1_0, E1_0, L1, C1, L2, C2, M) end]
    end;
same_keys1(E1_0, K1_0, F1, C1, E2, C2, E2_0, L2, M) when is_function(F1) ->
    same_keys1(E1_0, K1_0, F1(), C1, E2, C2, E2_0, L2, M);
same_keys1(E1_0, K1_0, T1, C1, E2, _C2, E2_0, _L2, M) ->
    Cont = fun(_L1b) -> throw_error(T1) end,
    loop_same_keys(E1_0, K1_0, T1, C1, [E2, E2_0], Cont, M).

%% There is no such element E in L1 such that element(C1, E) == K1.
same_keys(_K1, _E1, _L1, _C1, []=Cont, _C2, M) ->
    end_merge_join(Cont, M);
same_keys(K1, E1, L1, C1, [E2 | L2], C2, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            [?JWRAP(E1, E2) | 
             fun() -> same_keys(K1, E1, L1, C1, L2, C2, M) end];
        K1 < K2 ->
            merge_join1(E2, K2, L1, C1, L2, C2, M)
    end;
same_keys(K1, E1, L1, C1, F2, C2, M) when is_function(F2) ->
    same_keys(K1, E1, L1, C1, F2(), C2, M);
same_keys(_K1, _E1, _L1, _C1, T2, _C2, _M) ->
    throw_error(T2).

%% There are at least two elements in [E1 | L1] that are to be combined
%% with the elements in E2s (length(E2s) > 1). This loop covers the case
%% when all elements in E2 with key K1 can be kept in RAM.
same_keys_cache(E1, K1, L1, C1, [], _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    loop_same_keys(E1, K1, L1, C1, E2s, Cont, M);
same_keys_cache(E1, K1, L1, C1, L2, C2, E2s, Sz0, M) when Sz0 < 0 ->
    ok = init_merge_join(M),
    Sz = M#m.max_list,
    C = fun() -> same_keys_file(E1, K1, L1, C1, L2, C2, [], Sz, M) end,
    write_same_keys(E1, E2s, M, C);
same_keys_cache(E1, K1, L1, C1, [E2 | L2], C2, E2s, Sz0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            Sz = decr_list_size(Sz0, E2),
            same_keys_cache(E1, K1, L1, C1, L2, C2, [E2 | E2s], Sz, M);
        K1 < K2 ->
            Cont = fun(L1b) -> merge_join1(E2, K2, L1b, C1, L2, C2, M) end,
            loop_same_keys(E1, K1, L1, C1, E2s, Cont, M)
    end;
same_keys_cache(E1, K1, L1, C1, F2, C2, E2s, Sz, M) when is_function(F2) ->
    same_keys_cache(E1, K1, L1, C1, F2(), C2, E2s, Sz, M);
same_keys_cache(E1, K1, L1, C1, T2, _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> throw_error(T2) end,
    loop_same_keys(E1, K1, L1, C1, E2s, Cont, M).

%% E2s holds all elements E2 in L2 such that element(E2, C2) == K1.
loop_same_keys(E1, _K1, [], _C1, E2s, _Cont, M) ->
    end_merge_join(loop_same(E1, E2s, []), M);
loop_same_keys(E1, K1, L1, C1, E2s, Cont, M) ->
    loop_same(E1, E2s, fun() -> loop_keys(K1, L1, C1, E2s, Cont, M) end).

loop_same(_E1, [], L) ->
    L;
loop_same(E1, [E2 | E2s], L) ->
    loop_same(E1, E2s, [?JWRAP(E1, E2) | L]).

loop_keys(K, [E1 | L1]=L1_0, C1, E2s, Cont, M) ->
    K1 = element(C1, E1),
    if 
        K1 == K ->
            loop_same_keys(E1, K1, L1, C1, E2s, Cont, M);
        K1 > K -> 
            Cont(L1_0)
    end;
loop_keys(K, F1, C1, E2s, Cont, M) when is_function(F1) ->
    loop_keys(K, F1(), C1, E2s, Cont, M);
loop_keys(_K, []=L1, _C1, _Es2, Cont, _M) ->
    Cont(L1);
loop_keys(_K, T1, _C1, _Es2, _Cont, _M) ->
    throw_error(T1).

%% This is for the case when a temporary file has to be used.
same_keys_file(E1, K1, L1, C1, [], _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> end_merge_join([], M) end,
    same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont);    
same_keys_file(E1, K1, L1, C1, L2, C2, E2s, Sz0, M) when Sz0 < 0 ->
    Sz = M#m.max_list,
    C = fun() -> same_keys_file(E1, K1, L1, C1, L2, C2, [], Sz, M) end,
    write_same_keys(E1, E2s, M, C);
same_keys_file(E1, K1, L1, C1, [E2 | L2], C2, E2s, Sz0, M) ->
    K2 = element(C2, E2),
    if
        K1 == K2 ->
            Sz = decr_list_size(Sz0, E2),
            same_keys_file(E1, K1, L1, C1, L2, C2, [E2 | E2s], Sz, M);
        K1 < K2 ->
            Cont = fun(L1b) ->
                           %% The temporary file could be truncated here.
                           merge_join1(E2, K2, L1b, C1, L2, C2, M)
                   end,
            same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont)
    end;
same_keys_file(E1, K1, L1, C1, F2, C2, E2s, Sz, M) when is_function(F2) ->
    same_keys_file(E1, K1, L1, C1, F2(), C2, E2s, Sz, M);
same_keys_file(E1, K1, L1, C1, T2, _C2, E2s, _Sz, M) ->
    Cont = fun(_L1b) -> throw_error(T2) end,
    same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont).

same_keys_file_write(E1, K1, L1, C1, E2s, M, Cont) ->
    C = fun() -> loop_keys_file(K1, L1, C1, Cont, M) end,
    write_same_keys(E1, E2s, M, C).

write_same_keys(_E1, [], _M, Cont) ->
    Cont();
write_same_keys(E1, Es2, M, Cont) ->
    write_same_keys(E1, Es2, M, [], Cont).

%% Avoids one (the first) traversal of the temporary file.
write_same_keys(_E1, [], M, E2s, Objs) ->
    ok = write_merge_join(M, E2s),
    Objs;
write_same_keys(E1, [E2 | E2s0], M, E2s, Objs) ->
    BE2 = term_to_binary(E2),
    write_same_keys(E1, E2s0, M, [BE2 | E2s], [?JWRAP(E1, E2) | Objs]).

loop_keys_file(K, [E1 | L1]=L1_0, C1, Cont, M) ->
    K1 = element(C1, E1),
    if
        K1 == K ->
            C = fun() -> loop_keys_file(K1, L1, C1, Cont, M) end,
            read_merge_join(M, E1, C);
        K1 > K ->
            Cont(L1_0)
    end;
loop_keys_file(K, F1, C1, Cont, M) when is_function(F1) ->
    loop_keys_file(K, F1(), C1, Cont, M);
loop_keys_file(_K, []=L1, _C1, Cont, _M) ->
    Cont(L1);
loop_keys_file(_K, T1, _C1, _Cont, _M) ->
    throw_error(T1).

end_merge_join(Reply, M) ->
    end_merge_join(M),
    Reply.

%% Normally post_funs() cleans up temporary files by calling funs in
%% Post. It seems impossible to do that with the temporary file(s)
%% used when many objects have the same key--such a file is created
%% after the setup when Post is prepared. There seems to be no real
%% alternative to using the process dictionary, at least as things
%% have been implemented so far. Probably all of Post could have been
%% put in the process dictionary...

-define(MERGE_JOIN_FILE, '$_qlc_merge_join_tmpfiles_').

init_merge_join(#m{id = MergeId, tmpdir = TmpDir}) ->
    case tmp_merge_file(MergeId) of
        {Fd, FileName} ->
            case file:truncate(Fd) of
                ok ->
                    ok;
                Error ->
                    file_error(FileName, Error)
            end;
        none ->
            FName = tmp_filename(TmpDir),
            case file:open(FName, [raw, binary, read, write]) of
                {ok, Fd} ->
                    TmpFiles = get(?MERGE_JOIN_FILE),
                    put(?MERGE_JOIN_FILE, [{MergeId, Fd, FName} | TmpFiles]),
                    ok;
                Error ->
                    file_error(FName, Error)
            end
    end.

write_merge_join(#m{id = MergeId}, BTerms) ->
    {Fd, FileName} = tmp_merge_file(MergeId),
    case file:write(Fd, size_bin(BTerms, [])) of
        ok ->
            ok;
        Error ->
            file_error(FileName, Error)
    end.

read_merge_join(#m{id = MergeId}, E1, Cont) ->
    {Fd, FileName} = tmp_merge_file(MergeId),
    case file:position(Fd, bof) of
        {ok, 0} -> 
            ok;
        Error -> 
            file_error(FileName, Error)
    end,
    Fun = fun([], _) ->
                  Cont();
             (Ts, C) ->
                  join_read_terms(E1, Ts, C)
          end,
    file_loop_read(<<>>, ?CHUNK_SIZE, 0, {Fd, FileName}, Fun).

join_read_terms(_E1, [], Objs) ->
    Objs;
join_read_terms(E1, [E2 | E2s], Objs) ->
    join_read_terms(E1, E2s, [?JWRAP(E1, E2) | Objs]).

end_merge_join(#m{id = MergeId}) ->
    case tmp_merge_file(MergeId) of
        none -> 
            ok;
        {Fd, FileName}  ->
            _ = file:close(Fd),
            _ = file:delete(FileName),
            put(?MERGE_JOIN_FILE, 
                lists:keydelete(MergeId, 1, get(?MERGE_JOIN_FILE)))
    end.

end_all_merge_joins() ->
    lists:foreach(
      fun(Id) -> end_merge_join(#m{id = Id}) end, 
      [Id || {Id, _Fd, _FileName} <- lists:flatten([get(?MERGE_JOIN_FILE)])]),
    erase(?MERGE_JOIN_FILE).

merge_join_id() ->
    case get(?MERGE_JOIN_FILE) of
        undefined ->
            put(?MERGE_JOIN_FILE, []);
        _ -> 
            ok
    end,
    make_ref().

tmp_merge_file(MergeId) ->
    TmpFiles = get(?MERGE_JOIN_FILE),
    case lists:keysearch(MergeId, 1, TmpFiles) of
        {value, {MergeId, Fd, FileName}} ->
            {Fd, FileName};
        false ->
            none
    end.

decr_list_size(Sz0, E) when is_integer(Sz0) ->
    Sz0 - erlang:external_size(E).

%%% End of merge join.    

lookup_join([E1 | L1], C1, LuF, C2, Rev) ->
    K1 = element(C1, E1),
    case LuF(C2, [K1]) of
        [] ->
            lookup_join(L1, C1, LuF, C2, Rev);
        [E2] when Rev ->
            [?JWRAP(E2, E1) | fun() -> lookup_join(L1, C1, LuF, C2, Rev) end];
        [E2] ->
            [?JWRAP(E1, E2) | fun() -> lookup_join(L1, C1, LuF, C2, Rev) end];
        E2s when is_list(E2s), Rev ->
            [?JWRAP(E2, E1) || E2 <- E2s] ++
                fun() -> lookup_join(L1, C1, LuF, C2, Rev) end;
        E2s when is_list(E2s) ->
            [?JWRAP(E1, E2) || E2 <- E2s] ++
                fun() -> lookup_join(L1, C1, LuF, C2, Rev) end;
        Term ->
            Term
    end;
lookup_join([]=Cont, _C1, _LuF, _C2, _Rev) ->
    Cont;
lookup_join(F1, C1, LuF, C2, Rev) when is_function(F1) ->
    lookup_join(F1(), C1, LuF, C2, Rev);
lookup_join(T1, _C1, _LuF, _C2, _Rev) ->
    T1.

post_funs(L) ->
    end_all_merge_joins(),
    local_post(L).

local_post(L) ->
    lists:foreach(fun(undefined) -> ok;
                     (F) -> catch (F)() 
                  end, L).

call(undefined, _Arg, Default, _Post) ->
    Default;
call(Fun, Arg, _Default, Post) ->
    try
        Fun(Arg) 
    catch Class:Reason ->
        post_funs(Post),
        erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.

grd(undefined, _Arg) ->
    false;
grd(Fun, Arg) ->
    case Fun(Arg) of
        true -> 
            true;
        _ ->
            false
    end.

family(L) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(L))).

family_union(L) ->
    R = sofs:relation(L,[{atom,[atom]}]),
    sofs:to_external(sofs:family_union(sofs:relation_to_family(R))).

file_error(File, {error, Reason}) ->
    throw_reason({file_error, File, Reason}).

throw_reason(Reason) ->
    throw_error(error(Reason)).

throw_error(Error) ->
    throw(Error).

error(Reason) ->
    {error, ?MODULE, Reason}.
