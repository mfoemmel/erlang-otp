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

%%% A query handle is a tuple {qlc_handle, Handle} where Handle is one
%%% of #qlc_append, #qlc_table, #qlc_sort, and #qlc_lc.

-record(qlc_handle, {h}).

get_handle(#qlc_handle{h = H}) ->
    H;
get_handle(L) when is_list(L) ->
    L;
get_handle(_) ->
    badarg.

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
         lu_vals,    % {Position,Values}; values to be looked up
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

%%% A query cursor is a tuple {qlc_cursor, Cursor} where Cursor is a pair
%%% {CursorPid, OwnerPid}.

-record(qlc_cursor, {c}).

-record(qlc_opt,
        {unique = false,  % bool()
         cache = false,   % bool()
         max_lookup = -1  % int() >= 0 | -1 (infinity)
        }).

-record(setup, {parent, ref = make_ref()}).

%%%
%%% Exported functions
%%%

-define(EXIT(_R), 
        case _R of 
            {_Reason,_Wh} ->
                case looks_like_Where(_Wh) of
                    true ->
                        {'EXIT',{foo,_Wh2}} = (catch erlang:fault(foo)),
                        exit({_Reason,_Wh++_Wh2});
                    false ->
                        exit(_R)
                end;
            _ ->
                exit(_R)
        end).

append(QHs) ->
    Hs = lists:map(fun(QH) -> 
                           case get_handle(QH) of
                               badarg -> erlang:fault(badarg, [QHs]);
                               H -> H
                           end end, QHs),
    #qlc_handle{h = #qlc_append{hl = Hs}}.

append(QH1, QH2) ->
    Hs = lists:map(fun(QH) -> 
                           case get_handle(QH) of
                               badarg -> erlang:fault(badarg, [QH1, QH2]);
                               H -> H
                           end end, [QH1, QH2]),
    #qlc_handle{h = #qlc_append{hl = Hs}}.

cursor(QH) ->
    cursor(QH, []).

cursor(QH, Options) ->
    case {options(Options, [unique_all, cache_all, spawn_options]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:fault(badarg, [QH, Options]);
        {[GUnique, GCache, SpawnOptions0], H} ->
            SpawnOptions = ensure_link(SpawnOptions0),
            case cursor_process(H, GUnique, GCache, SpawnOptions) of
                Pid when is_pid(Pid) ->
                    #qlc_cursor{c = {Pid, self()}};
                Error ->
                    Error
            end
    end.

delete_cursor(#qlc_cursor{c = {_, Owner}}=C) when Owner =/= self() ->
    erlang:fault(not_cursor_owner, [C]);
delete_cursor(#qlc_cursor{c = {Pid, _}}) ->
    stop_cursor(Pid);
delete_cursor(T) ->
    erlang:fault(badarg, [T]).

e(QH) ->
    eval(QH, []).

e(QH, Options) ->
    eval(QH, Options).

eval(QH) ->
    eval(QH, []).

eval(QH, Options) ->
    case {options(Options, [unique_all, cache_all]), get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:fault(badarg, [QH, Options]);
        {[GUnique, GCache], Handle} ->
            Prep = prepare_qlc(Handle, [], GUnique, GCache, exit),
            case setup_qlc(Prep, exit, #setup{parent = self()}) of
                {L, Post} when is_list(L) ->
                    table_post(Post),
                    L;
                {Objs, Post} when is_function(Objs) ->
                    H = ensure_collecting(Prep, Objs),
                    Ref = make_ref(),
                    Ret = (catch {H(), Ref}),
                    table_post(Post),
                    case Ret of
                        {Reply, Ref} ->
                            Reply;
                        {'EXIT', Reason} ->
                            exit(Reason);
                        Thrown ->
                            throw(Thrown)
                    end;
                Error ->
                    Error
            end
    end.

fold(Fun, Acc0, QH) ->
    fold(Fun, Acc0, QH, []).

fold(Fun, Acc0, QH, Options) ->
    case {options(Options, [unique_all, cache_all]), get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:fault(badarg, [Fun, Acc0, QH, Options]);
        {[GUnique, GCache], Handle} ->
            Prep = prepare_qlc(Handle, not_a_list, GUnique, GCache, exit),
            case setup_qlc(Prep, exit, #setup{parent = self()}) of
                {Objs, Post} when is_function(Objs); is_list(Objs) ->
                    Ref = make_ref(),
                    Ret = (catch {fold_loop(Fun, Objs, Acc0), Ref}),
                    table_post(Post),
                    case Ret of
                        {Reply, Ref} ->
                            Reply;
                        {'EXIT', Reason} ->
                            exit(Reason);
                        Thrown ->
                            throw(Thrown)
                    end;
                Error ->
                    Error
            end
    end.

format_error(not_a_query_list_comprehension) ->
    io_lib:format("argument is not a query list comprehension", []);
format_error({used_generator_variable, V}) ->
    io_lib:format("generated variable ~w must not be used in list expression",
                  [V]);
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
    case {options(Options, [unique_all, cache_all, flat, format, n_elements]),
          get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:fault(badarg, [QH, Options]);
        {[GUnique, GCache, Flat, Format, NElements], H} ->
            Prep = prepare_qlc(H, [], GUnique, GCache, exit),
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
            erlang:fault(badarg, [KeyPos, QH, Options])
    end.

-define(DEFAULT_NUM_OF_ANSWERS, 10).

next_answers(C) ->
    next_answers(C, ?DEFAULT_NUM_OF_ANSWERS).

next_answers(#qlc_cursor{c = {_, Owner}}=C, 
             NumOfAnswers) when Owner =/= self() ->
    erlang:fault(not_cursor_owner, [C, NumOfAnswers]);
next_answers(#qlc_cursor{c = {Pid, _}}=C, NumOfAnswers) ->
    N = case NumOfAnswers of
            all_remaining -> -1;
            _ when is_integer(NumOfAnswers), NumOfAnswers > 0 -> NumOfAnswers;
            _ -> erlang:fault(badarg, [C, NumOfAnswers])
        end,
    next_loop(Pid, [], N);
next_answers(T1, T2) ->
    erlang:fault(badarg, [T1, T2]).

parse_transform(Forms, Options) ->
    qlc_pt:parse_transform(Forms, Options).

%% The funcspecs qlc:q/1 and qlc:q/2 are known by erl_eval.erl.
q(QLC_lc) ->
    q(QLC_lc, []).

q(#qlc_lc{}=QLC_lc, Options) ->
    case options(Options, [unique, cache, max_lookup]) of
        [Unique, Cache, Max] ->
            Opt = #qlc_opt{unique = Unique, cache = Cache, max_lookup = Max},
            #qlc_handle{h = QLC_lc#qlc_lc{opt = Opt}};
        _ ->
            erlang:fault(badarg, [QLC_lc, Options])
    end;
q(T1, T2) ->
    erlang:fault(badarg, [T1, T2]).

sort(QH) ->
    sort(QH, []).

sort(QH, Options) ->
    case {options(Options, [tmpdir, order, unique, compressed, 
                            size, no_files]), get_handle(QH)} of
        {B1, B2} when B1 =:= badarg; B2 =:= badarg ->
            erlang:fault(badarg, [QH, Options]);
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
    case options(Options, [unique, cache, max_lookup]) of
        badarg ->
            erlang:fault(badarg, [Str, Options, Bindings]);
        [Unique, Cache, MaxLookup] ->
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
                                                 max_lookup = MaxLookup},
                                    #qlc_handle{h = QLC_lc#qlc_lc{opt = O}};
                                {not_ok, [{error, Error} | _]} ->
                                    error(Error)
                            end;
                        {ok, _ExprList} ->
                            erlang:fault(badarg, [Str, Options, Bindings]);
                        {error, ErrorInfo} ->
                            error(ErrorInfo)
                    end;
                {error, ErrorInfo, _EndLine} ->
                    error(ErrorInfo)
            end
    end;
string_to_handle(T1, T2, T3) ->    
    erlang:fault(badarg, [T1, T2, T3]).

table(TraverseFun, Options) ->
    case catch erlang:fun_info(TraverseFun, arity) of
        {arity, Arity} when Arity =< 1 ->
            case options(Options, [pre_fun, post_fun, info_fun, format_fun, 
                                   lookup_fun, parent_fun]) of
                [PreFun, PostFun, InfoFun, FormatFun, LookupFun, ParentFun] ->
                    T = #qlc_table{trav_fun = TraverseFun, pre_fun = PreFun,
                                   post_fun = PostFun, info_fun = InfoFun, 
                                   parent_fun = ParentFun,
                                   trav_MS = Arity =:= 1,
                                   format_fun = FormatFun, 
                                   lookup_fun = LookupFun},
                    #qlc_handle{h = T};
                badarg ->
                    erlang:fault(badarg, [TraverseFun, Options])
            end;
        _ ->
            erlang:fault(badarg, [TraverseFun, Options])
    end.

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
            {value, {Tag, U=undefined}} when Tag =:= format_fun;
                                             Tag =:= info_fun; 
                                             Tag =:= lookup_fun;
                                             Tag =:= parent_fun;
                                             Tag =:= post_fun;
                                             Tag =:= pre_fun ->
                {ok, U};
            {value, {Tag, Fun}} when Tag =:= info_fun; Tag =:= pre_fun ->
                case catch erlang:fun_info(Fun, arity) of
                    {arity, 1} ->
                        {ok, Fun};
                    _ ->
                        badarg
                end;
            {value, {post_fun, Fun}}  ->
                case catch erlang:fun_info(Fun, arity) of
                    {arity, 0} ->
                        {ok, Fun};
                    _ ->
                        badarg
                end;
            {value, {lookup_fun, Fun}}  ->
                case catch erlang:fun_info(Fun, arity) of
                    {arity, 2} ->
                        {ok, Fun};
                    _ ->
                        badarg
                end;
            {value, {max_lookup, Max}} when is_integer(Max), Max >= 0 ->
                {ok, Max};
            {value, {max_lookup, infinity}} ->
                {ok, -1};
            {value, {format_fun, Fun}} ->
                case catch erlang:fun_info(Fun, arity) of
                    {arity, 1} ->
                        {ok, Fun};
                    _ ->
                        badarg
                end;
            {value, {parent_fun, Fun}}  ->
                case catch erlang:fun_info(Fun, arity) of
                    {arity, 0} ->
                        {ok, Fun};
                    _ ->
                        badarg
                end;
            {value, {unique, Unique}} when Unique =:= true; 
                                           Unique =:= false ->
                {ok, Unique};
            {value, {cache, Cache}} when Cache =:= true; Cache =:= false ->
                {ok, Cache};
            {value, {unique_all, UniqueAll}} when UniqueAll =:= true; 
                                                  UniqueAll =:= false ->
                {ok, UniqueAll};
            {value, {cache_all, CacheAll}} when CacheAll =:= true; 
                                                CacheAll =:= false ->
                {ok, CacheAll};
            {value, {spawn_options, default}} ->
                {ok, default};
            {value, {spawn_options, SpawnOptions}} ->
                case is_proper_list(SpawnOptions) of
                    true -> 
                        {ok, ensure_link(SpawnOptions)};
                    false ->
                        badarg
                end;
            {value, {flat, Flat}} when Flat =:= true; Flat =:= false ->
                {ok, Flat};
            {value, {format, Format}} when Format =:= string;
                                           Format =:= abstract_code;
                                           Format =:= debug ->
                {ok, Format};
            {value, {n_elements, NElements}} when NElements =:= infinity; 
                                          integer(NElements), NElements > 0 ->
                {ok, NElements};
            {value, {order, Order}} when is_function(Order);
                                         (Order =:= ascending);
                                         (Order =:= descending) ->
                {ok, Order};
            {value, {compressed, Comp}} when Comp =:= true ->
                {ok, [compressed]};
            {value, {compressed, Comp}} when Comp =:= false ->
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
default_option(parent_fun) -> undefined;
default_option(spawn_options) -> default;
default_option(flat) -> true;
default_option(format) -> string;
default_option(n_elements) -> infinity;
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
atom_option(flat) -> {flat, true};
atom_option(Key) -> Key.

is_proper_list([_ | L]) ->
    is_proper_list(L);
is_proper_list(L) ->
    L =:= [].

ensure_link(default) ->
    [link];
ensure_link(SpawnOptions) ->
    case lists:member(link, SpawnOptions) of
	true -> 
	    SpawnOptions;
	false ->
	    [link | SpawnOptions]
    end.

is_keypos(Keypos) when integer(Keypos), Keypos > 0 ->
    true;
is_keypos([]) ->
    false;
is_keypos(L) ->
    is_keyposs(L).

is_keyposs([Kp | Kps]) when integer(Kp), Kp > 0 ->
    is_keyposs(Kps);
is_keyposs(Kps) ->
    Kps =:= [].

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% Optimizations to be carried out.
-record(optz,
        {unique = false, % bool()
         cache = false   % bool()
        }).

%% Prepared #qlc_lc.
-record(qlc,
        {lcf,       % fun() -> Val
         codef,
         qdata,     % with evaluated list expressions
         init_value,
         opt        % #optz
        }).

%% Prepared simple #qlc_lc.
-record(simple_qlc,
        {p,         % atom(), pattern variable
         le,
         line,
         init_value,
         opt        % #optz
         }).

-record(prepared,
        {qh,     % #qlc_append | #qlc_table | #qlc | #simple_qlc | 
                 % #qlc_sort | list()
         sorted = no, % Currently not used.
         is_unique_objects = false, % bool()
         is_cached = false          % bool()
        }).

ensure_collecting(Prep, Objs) ->
    case Prep#prepared.qh of
        #qlc{opt = #optz{unique = false, cache = false}} ->
            Objs;
        _ -> 
            fun() -> collect(Objs, []) end
    end.

%%% Cursor process functions.

cursor_process(H, GUnique, GCache, SpawnOptions) ->
    Parent = self(),
    Setup = #setup{parent = Parent},
    CF = fun() -> 
                 process_flag(trap_exit, true),
                 case prepare_qlc(H, not_a_list, GUnique, GCache, no_exit) of
                     #prepared{} = Prep ->
                         case setup_qlc(Prep, no_exit, Setup) of
                             {Objs, Post} when is_function(Objs); 
                                               is_list(Objs) ->
                                 Parent ! {self(), ok},
                                 wait_for_request(Parent, Post), 
                                 Ref = make_ref(),
                                 reply(Parent, Post, Ref, Objs);
                             ErrorOrExitOrThrown ->
                                 Parent ! {self(), ErrorOrExitOrThrown}
                         end;
                     ErrorOrExitOrThrown ->
                         Parent ! {self(), ErrorOrExitOrThrown}
                 end
         end,
    Pid = spawn_opt(CF, SpawnOptions),
    parent_fun(Pid, Parent, Setup#setup.ref).

%% Expect calls from tables calling the parent_fun and finally an 'ok'.
parent_fun(Pid, Parent, ParentRef) ->
    receive 
        {Pid, ok} -> Pid;
        {TPid, {parent_fun, ParentRef, Fun}} ->
            TPid ! {Parent, (catch {ParentRef, Fun()})},
            parent_fun(Pid, Parent, ParentRef);
        {Pid, {throw, Thrown}} -> throw(Thrown);
        {Pid, {exit, Reason}} -> ?EXIT(Reason);
        {Pid, Error} -> Error
    end.

reply(Parent, Post, _Ref, []) ->
    no_more(Parent, Post);
reply(Parent, Post, Ref, [Answer | Cont]) ->
    Parent ! {self(), {answer, Answer}},    
    wait_for_request(Parent, Post),
    reply(Parent, Post, Ref, Cont);
reply(Parent, Post, Ref, Cont) ->
    case catch {Ref, Cont()} of
        {Ref, Reply} ->
            reply(Parent, Post, Ref, Reply);
        {'EXIT', Reason} ->
            table_post(Post),
            Parent ! {self(), {exit, Reason}},
            exit(normal);
        Thrown ->
            table_post(Post),
            Parent ! {self(), {throw, Thrown}},
            exit(normal)
    end.

no_more(Parent, Post) ->
    Parent ! {self(), no_more},
    wait_for_request(Parent, Post),
    no_more(Parent, Post).

wait_for_request(Parent, Post) ->
    receive 
        {Parent, stop} ->
            table_post(Post),
            exit(normal);
        {Parent, more} ->
            ok;
        {'EXIT', Parent, _Reason} ->
            table_post(Post),
            exit(normal);
        _Ignored ->
            wait_for_request(Parent, Post)
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
    Vars = sets:from_list(qlc_pt:vars(Abstract)),
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

collect([], L) ->
    lists:reverse(L);
collect([Answer | Cont], L) ->
    collect(Cont, [Answer | L]);
collect(Cont, L) ->
    collect(Cont(), L).

fold_loop(Fun, [Obj | Cont], Acc) ->
    fold_loop(Fun, Cont, Fun(Obj, Acc));
fold_loop(_Fun, [], Acc) ->
    Acc;
fold_loop(Fun, Cont, Acc) ->
    fold_loop(Fun, Cont(), Acc).

next_loop(Pid, L, N) when N =/= 0 ->
    case monitor_request(Pid, more) of
        no_more ->
            lists:reverse(L);
        {answer, Answer} ->
            next_loop(Pid, [Answer | L], N - 1);
        {throw, Thrown} ->
            throw(Thrown);
        {exit, Reason} ->
            ?EXIT(Reason);
        error ->
            erlang:fault({qlc_cursor_pid_no_longer_exists, Pid})
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

looks_like_Where([{M,F,_A} | L]) when is_atom(M), is_atom(F) ->
    looks_like_Where(L);
looks_like_Where(T) ->
    T =:= [].

-define(TEMPLATE_STATE, 1).

template_state() ->
    ?TEMPLATE_STATE.

%% Qual = {gen, LE} | fil
-define(qual_data(QNum, GoToIndex, State, Qual), 
        {QNum, GoToIndex, State, Qual}).

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

le_info(#prepared{qh = #simple_qlc{le = LE, p = P, line = L, opt = Opt}}) ->
    QVar = term_to_binary({var, L, P}),
    {qlc, QVar, [{generate, QVar, le_info(LE)}], opt_info(Opt)};
le_info(#prepared{qh = #qlc{codef = CodeF, qdata = Qdata, opt = Opt}}) ->
    Code = CodeF(),
    TemplateState = template_state(),
    E = element(TemplateState, Code),
    {qlc, E, qual_info(Qdata, Code), opt_info(Opt)};
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
                                 opts = SortOptions}}) ->
    {sort, le_info(H), SortOptions};
le_info(#prepared{qh = #qlc_sort{h = H, keypos = {keysort, Kp}, 
                                 opts = SortOptions}}) ->
    {keysort, le_info(H), Kp, SortOptions};
le_info(#prepared{qh = #qlc_list{l = L, ms = no_match_spec}}) ->
    {list, L};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}}) when is_list(L) ->
    {list, {list, L}, MS};
le_info(#prepared{qh = #qlc_list{l = L, ms = MS}}) ->
    {list, le_info(L), MS}.

qual_info([?qual_data(QNum, _GoI, _SI, fil) | Qdata], Code) ->
    [element(QNum + 1, Code) | qual_info(Qdata, Code)];
qual_info([?qual_data(QNum, _GoI, _SI, {gen,LE}) | Qdata], Code) ->
    [{generate,element(QNum + 1, Code),le_info(LE)} | qual_info(Qdata, Code)];
qual_info([], _Code) ->
    [].

opt_info(#optz{unique = Unique, cache = Cache}) ->
    L1 = case Unique =:= default_option(unique) of
             true -> [];
             false -> [{unique, Unique}]
         end,
    case Cache =:= default_option(cache) of
        true -> L1;
        false -> [{cache, Cache} | L1]
    end.

prepare_qlc(H, InitialValue, GUnique, GCache, ActionOnError) ->
    GOpt = #qlc_opt{unique = GUnique, cache = GCache},
    case catch prep_le(H, GOpt) of
        {exit, Reason} when ActionOnError =:= exit ->
            exit(Reason);
        {throw, Thrown} when ActionOnError =:= exit ->
            throw(Thrown);
        #prepared{} = Prep0 ->
            case opt_le(Prep0, 1) of
                #prepared{qh = #qlc{} = QLC}=Prep ->
                    Prep#prepared{qh = QLC#qlc{init_value = InitialValue}};
                #prepared{qh = #simple_qlc{}=SimpleQLC}=Prep ->
                    Prep#prepared{qh = SimpleQLC#simple_qlc{init_value = InitialValue}};
                Prep ->
                    Prep
            end;
        Reply ->
            Reply
    end.

%%% The options given to append, q and table (unique and cache) as well
%%% as the type of expression (list, table, append, qlc...) are
%%% analyzed by prep_le. The results are is_unique_objects and
%%% is_cached. List expressions are evaluated.

-record(post, {local = [], other = []}).

prep_le(#qlc_lc{lc = LC_fun, opt = Opt0}, GOpt) ->
    Opt = Opt0#qlc_opt{unique = Opt0#qlc_opt.unique or GOpt#qlc_opt.unique,
                       cache = Opt0#qlc_opt.cache or GOpt#qlc_opt.cache},
    prep_qlc_lc(LC_fun(), Opt, GOpt);
prep_le(#qlc_table{info_fun = IF}=T, GOpt) ->
    case call(IF, num_of_objects, -1) of
        0 -> 
            short_list([]);
        _ -> 
            Prep = #prepared{qh = T, 
                             is_unique_objects = grd(IF, is_unique_objects)},
            may_create_simple(GOpt#qlc_opt{cache = false}, Prep)
    end;
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
            Cache = lists:all(fun(#prepared{is_cached = IM}) -> IM end,PrepL),
            %% The handles in hl are replaced by prepared handles:
            Prep = #prepared{qh = #qlc_append{hl = PrepL}, is_cached = Cache},
            may_create_simple(GOpt, Prep)
    end;
prep_le(#qlc_sort{h = H0}=Q, GOpt) ->
    case prep_le(H0, GOpt) of
        #prepared{sorted = yes}=Prep -> 
            Prep;
        #prepared{is_unique_objects = IsUniqueObjs} = Prep -> 
            %% The handle h is replaced by a prepared handle:
            S = sort_unique(IsUniqueObjs, Q#qlc_sort{h = Prep}),
            #prepared{qh = S, is_cached = true,
                      is_unique_objects = S#qlc_sort.unique or IsUniqueObjs}
    end;
prep_le([_, _ | _]=L, GOpt) ->
    Prep = #prepared{qh = #qlc_list{l = L}, is_cached = true},
    may_create_simple(GOpt#qlc_opt{cache = false}, Prep);
prep_le(L, _GOpt) when is_list(L) ->
    short_list(L);
prep_le(T, _GOpt) ->
    test_reply(foo, catch {foo, erlang:fault(badarg, [T])}, #post{}).

eval_le(LE_fun, GOpt) ->
    Ref = make_ref(),
    case get_handle(test_reply(Ref, catch {Ref, LE_fun()}, [])) of
        badarg -> 
            test_reply(Ref, catch {Ref, erlang:fault(badarg, [LE_fun])}, []);
        H -> 
            prep_le(H, GOpt)
    end.

prep_qlc_lc({simple_v1, PVar, LE_fun, L}, Opt, GOpt) ->
    prep_simple_qlc(PVar, L, eval_le(LE_fun, GOpt), Opt);
prep_qlc_lc({single_v1, QFun, CodeF, Qdata0, _, MS, PosFun}, Opt, GOpt) ->
    %% Filter optional:
    [?qual_data(QNum, GoI_G, SI_G, {gen, LE_fun}) | Filter] = Qdata0, 
    #prepared{qh = LE0} = Prep = eval_le(LE_fun, GOpt),
    LuV = find_const_positions(LE0, PosFun, Opt),
    case LE0 of
        #qlc_table{trav_MS = true} when MS =/= no_match_spec, LuV =/= false ->
            LE = LE0#qlc_table{lu_vals = LuV, ms = MS},
            may_create_simple(Opt, Prep#prepared{qh = LE});
        #qlc_table{} when LuV =/= false ->
            Prep1 = Prep#prepared{qh = LE0#qlc_table{lu_vals = LuV}},
            Qdata = [?qual_data(QNum, GoI_G, SI_G, {gen, Prep1}) | Filter],
            prep_qlc(QFun, CodeF, Qdata, Opt);
        #qlc_table{trav_MS = true} when MS =/= no_match_spec ->
            may_create_simple(Opt, Prep#prepared{qh = LE0#qlc_table{ms = MS}});
        #qlc_list{l = []} ->
            may_create_simple(Opt, Prep);
        #qlc_list{ms = no_match_spec} when MS =/= no_match_spec ->
            may_create_simple(Opt, Prep#prepared{qh = LE0#qlc_list{ms = MS}});
        #qlc_list{} when MS =/= no_match_spec ->
            ListMS = #qlc_list{l = Prep, ms = MS},
            may_create_simple(Opt, #prepared{qh = ListMS, is_cached = true});
        _ ->
            Qdata = [?qual_data(QNum, GoI_G, SI_G, {gen, Prep}) | Filter],
            prep_qlc(QFun, CodeF, Qdata, Opt)
    end;
prep_qlc_lc({qlc_v1, QFun, CodeF, Qdata, _QOpt}, Opt, GOpt) ->
    F = fun(?qual_data(_QNum, _GoI, _SI, fil)=Q) -> 
                Q;
           (?qual_data(QNum, GoI, SI, {gen, LE_fun})) -> 
                ?qual_data(QNum, GoI, SI, {gen, eval_le(LE_fun, GOpt)})
        end,
    prep_qlc(QFun, CodeF, lists:map(F, Qdata), Opt).

-define(SIMPLE_QVAR, 'SQV').

may_create_simple(#qlc_opt{unique = false, cache = false}, Prep) ->
    Prep;
may_create_simple(Opt, Prep) ->
    prep_simple_qlc(?SIMPLE_QVAR, 1, Prep, Opt).

prep_simple_qlc(PVar, Line, LE, #qlc_opt{unique = Unique, cache = Cache}) ->
    #prepared{is_cached = IsCached, is_unique_objects = IsUnique} = LE,
    Opts = #optz{unique = Unique and not IsUnique,
                 cache = Cache and not IsCached},
    QLC = #simple_qlc{p = PVar, le = LE, line = Line, 
                      init_value = not_a_list, opt = Opts},
    #prepared{qh = QLC, is_unique_objects = IsUnique or Unique, 
              is_cached = IsCached or Cache}.

prep_qlc(QFun, CodeF, Qdata, #qlc_opt{unique = Unique, cache = Cache}) ->
    Optz = #optz{unique = Unique, cache = Cache},
    QLC = #qlc{lcf = QFun, codef = CodeF, qdata = Qdata, 
               init_value = not_a_list, opt = Optz},
    #prepared{qh = QLC, is_unique_objects = Unique, is_cached = Cache}.

sort_unique(true, #qlc_sort{opts = SortOptions}=Sort) ->
    Sort#qlc_sort{unique = false, 
                  opts = lists:keydelete(unique, 1, SortOptions)};
sort_unique(false, Sort) ->
    Sort.

short_list(L) ->
    %% length(L) < 2: all elements are known be equal
    #prepared{qh = #qlc_list{l = L}, sorted = yes, is_unique_objects = true, 
              is_cached = true}.
    
find_const_positions(#qlc_table{info_fun = IF, lookup_fun = LU_fun}, 
                     PosFun, #qlc_opt{max_lookup = Max}) 
           when is_function(LU_fun), is_function(PosFun),is_function(IF) ->
    case IF(keypos) of
        undefined ->
            find_const_position_idx(IF(indices), PosFun, Max, []);
        KeyPos ->
            case pos_vals(KeyPos, PosFun(KeyPos), Max) of
                false ->
                    find_const_position_idx(IF(indices), PosFun, Max, []);
                PosValues ->
                    PosValues
            end
    end;
find_const_positions(_, _PosFun,_Opt0) ->
    false.

find_const_position_idx([I | Is], PosFun, Max, L0) ->
    case pos_vals(I, PosFun(I), Max) of
        false ->
            find_const_position_idx(Is, PosFun, Max, L0);
        {_Pos, Values}=PosValues ->
            L = [{length(Values), PosValues} | L0],
            find_const_position_idx(Is, PosFun, Max, L)
    end;
find_const_position_idx(_, _PosFun, _Max, []) ->
    false;
find_const_position_idx(_, _PosFun, _Max, L) ->
    [{_,PV} | _] = lists:sort(L),
    PV.

pos_vals(Pos, {values, Values}, Max) ->
    pos_vals_max(Pos, Values, Max);
pos_vals(Pos, {usort_needed, Values}, Max) ->
    pos_vals_max(Pos, lists:usort(Values), Max);
pos_vals(_Pos, _, _Max) ->
    false.

pos_vals_max(Pos, Values, Max) when Max =:= -1; Max >= length(Values) ->
    {Pos, Values};
pos_vals_max(_Pos, _Value, _Max) ->
    false.

%% The only "optimization" right now is that the first generator is
%% never cached if the QLC expression itself is cached. Since the
%% answers do not need to be cached, the top-most QLC expression is
%% never cached either. Simple QLCs not holding any options are
%% removed. Simple QLCs are coalesced when possible.

opt_le(#prepared{qh = #simple_qlc{le = LE0, opt = Opt0}=QLC}=Prep, GenNum) ->
    Opt1 = Opt0#optz{cache = Opt0#optz.cache and (GenNum > 1)},
    case {opt_le(LE0, 1), Opt1} of
        {#prepared{qh = #simple_qlc{p = LE_Pvar, le = LE2, opt = Opt2}}, _} ->
            Opt = Opt1#optz{cache = Opt1#optz.cache or Opt2#optz.cache,
                            unique = Opt1#optz.unique or Opt2#optz.unique},
            PVar = if 
                       LE_Pvar =:= ?SIMPLE_QVAR -> QLC#simple_qlc.p;
                       true -> LE_Pvar
                   end,
            Prep#prepared{qh = QLC#simple_qlc{p = PVar, le = LE2, opt = Opt}};
        {LE, #optz{unique = false, cache = false}} ->
            LE;
        {LE, _} ->
            Prep#prepared{qh = QLC#simple_qlc{le = LE, opt = Opt1}}
    end;
opt_le(#prepared{qh = #qlc{qdata = Qdata0, opt = Opt}=QLC}=Prep, GenNum) ->
    F = fun(?qual_data(QNum, GoI, SI, {gen, LE}), GenNum1) ->
                {?qual_data(QNum, GoI, SI, {gen, opt_le(LE, GenNum1)}), 
                 GenNum1 + 1};
           (Qd, GenNum1) ->
                {Qd, GenNum1}
        end,
    {Qdata, _} = lists:mapfoldl(F, 1, Qdata0),
    Opt1 = Opt#optz{cache = Opt#optz.cache and (GenNum > 1)},
    Prep#prepared{qh = QLC#qlc{qdata = Qdata, opt = Opt1}};
opt_le(#prepared{qh = #qlc_append{hl = HL}}=Prep, GenNum) ->
    Hs = lists:map(fun(H) -> opt_le(H, GenNum) end, HL),
    Prep#prepared{qh = #qlc_append{hl = Hs}};
opt_le(#prepared{qh = #qlc_sort{h = H}=Sort}=Prep, GenNum) ->
    Prep#prepared{qh = Sort#qlc_sort{h = opt_le(H, GenNum)}};
opt_le(Prep, _GenNum) ->
    Prep.

setup_qlc(Prep, ActionOnError, Setup) ->
    case catch setup_le(Prep, #post{}, Setup) of
        {exit, Reason} when ActionOnError =:= exit ->
            exit(Reason);
        {throw, Thrown} when ActionOnError =:= exit ->
            throw(Thrown);
        Reply ->
            Reply
    end.

setup_le(#prepared{qh = #simple_qlc{le = LE, opt = Opt}}, Post0, Setup) ->
    {Objs, Post} = setup_le(LE, Post0, Setup),
    unique_cache(Objs, Post, Opt);
setup_le(#prepared{qh = #qlc{lcf = QFun, qdata = Qdata, init_value = V, 
                             opt = Opt}}, Post0, Setup) ->
    {GoTo, FirstState, Post} = setup_quals(Qdata, Post0, Setup),
    unique_cache(fun() -> QFun(FirstState, V, GoTo) end, Post, Opt);
setup_le(#prepared{qh = #qlc_table{post_fun = PostFun}=Table}, Post, Setup) ->
    {table_handle(Table, Post, Setup), 
     Post#post{other = [PostFun | Post#post.other]}};
setup_le(#prepared{qh = #qlc_append{hl = PrepL}}, Post0, Setup) ->
    F = fun(Prep, {Os, Post1}) -> 
               {O, Post2} = setup_le(Prep, Post1, Setup), {[O | Os], Post2}
        end,
    {ObjsL0, Post} = lists:foldl(F, {[], Post0}, PrepL),
    ObjsL = lists:reverse(ObjsL0),
    {fun() -> append_loop(ObjsL) end, Post};
setup_le(#prepared{qh = #qlc_sort{h = Prep, keypos = Kp, 
                                  unique = Unique, compressed = Compressed,
                                  order = Order, opts = SortOptions, 
                                  tmpdir = TmpDir}}, Post0, Setup) ->
    case setup_le(Prep, Post0, Setup) of
        {L, Post} when is_list(L) ->
            {sort_list(L, Order, Unique, Kp, SortOptions, Post), Post};
        {Objs, Post} ->
            In = sort_cursor_input(Objs, 0),
            Out = sort_cursor_list_output(TmpDir, Compressed),
            Reply = do_sort(In, Out, Kp, SortOptions, Post),
            case Reply of
                {file, FileName} ->
                    {F, Fd} = open_file(FileName, Compressed, Post),
                    P = fun() -> _ = file:close(Fd),
                                 _ = file:delete(FileName)
                        end,
                    {F, Post#post{other = [P | Post#post.other]}};
                {terms, BTerms} ->
                    Ref = make_ref(),
                    R = (catch {Ref, lists:map(fun binary_to_term/1, BTerms)}),
                    {test_reply(Ref, R, Post), Post}
            end
    end;
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
              when (no_match_spec =:= MS); L =:= [] ->
    {L, Post};
setup_le(#prepared{qh = #qlc_list{l = L, ms = MS}}, Post, _Setup) 
                                   when is_list(L) ->
    {ets:match_spec_run(L, ets:match_spec_compile(MS)), Post};
setup_le(#prepared{qh = #qlc_list{l = H0, ms = MS}}, Post0, Setup) ->
    {Objs, Post} = setup_le(H0, Post0, Setup),
    {ets:match_spec_run(Objs, ets:match_spec_compile(MS)), Post}.

%% The goto table (a tuple) is created at runtime. It is accessed by
%% the generated code in order to find next clause to execute. For
%% generators there is also a fun; calling the fun runs the list
%% expression of the generator. There are two elements for a filter:
%% the first one is the state to go when the filter is false; the
%% other the state when the filter is true. There are three elements
%% for a generator G: the first one is the state of the generator
%% before G (or the template if there is no prior generator); the
%% second one is the state of the qualifier following the generator;
%% the third one is the list expression fun.

setup_quals(Qdata, Post0, Setup) ->
    {GoTo0, Post} = setup_quals(0, Qdata, [], Post0, Setup),
    GoTo1 = lists:flatmap(fun({_,L}) -> L end, lists:keysort(1, GoTo0)),
    GoTo = list_to_tuple(GoTo1),
    FirstState = next_state(Qdata),
    {GoTo, FirstState, Post}.

setup_quals(GenLoopS, [?qual_data(_QNum,GoI,_SI,fil) | Qdata], 
            Gs, Post, Setup) ->
    setup_quals(GenLoopS, Qdata, [{GoI,[GenLoopS,next_state(Qdata)]} | Gs], 
                Post, Setup);
setup_quals(GenLoopS, [?qual_data(_QNum,GoI,SI,{gen,LE}) | Qdata], 
            Gs, Post, Setup) ->
    {V, NPost} = setup_le(LE, Post, Setup),
    setup_quals(SI + 1, Qdata, [{GoI, [GenLoopS,next_state(Qdata),V]} | Gs], 
                NPost, Setup);
setup_quals(GenLoopS, [], Gs, Post, _Setup) ->
    {[{1,[GenLoopS]} | Gs], Post}.

next_state([?qual_data(_,_,S,_) | _]) ->
    S;
next_state([]) ->
    template_state().

table_handle(#qlc_table{trav_fun = TraverseFun, trav_MS = TravMS, 
                        pre_fun = PreFun, lookup_fun = LuF, 
                        parent_fun = ParentFun, lu_vals = LuVals, ms = MS}, 
             Post, Setup) ->
    Ref = make_ref(),
    #setup{parent = Parent, ref = ParentRef} = Setup,
    ParentValue = 
        if 
            ParentFun =:= undefined ->
                undefined;
            Parent =:= self() ->
                test_reply(Ref, catch {Ref, ParentFun()}, Post);
            true ->
                M = {parent_fun, ParentRef, ParentFun},
                case monitor_request(Parent, M) of
                    error -> % parent has died
                        table_post(Post),
                        exit(normal);
                    Reply ->
                        test_reply(ParentRef, Reply, Post)
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
    %% The functions in Post are normally called from table_post().
    _ = test_reply(Ref, catch {Ref, call(PreFun, PreFunArgs, ok)}, Post),
    case LuVals of
        {Pos, Vals} when MS =:= no_match_spec ->
            fun() -> LuF(Pos, Vals) end;
        {Pos, Vals} ->
            fun() ->
                    case LuF(Pos, Vals) of
                        [] -> 
                            [];
                        Objs -> 
                            ets:match_spec_run(Objs, 
                                               ets:match_spec_compile(MS))
                    end
            end;
        _ when not TravMS ->
            TraverseFun;
        _ when MS =:= no_match_spec ->
            fun() -> TraverseFun([{'$1',[],['$1']}]) end;
        _ ->
            fun() -> TraverseFun(MS) end
    end.

-define(CHUNK_SIZE, 64*1024).

open_file(FileName, Compressed, Post) ->
    case file:open(FileName, [read, raw, binary | Compressed]) of
        {ok, Fd} ->
            {fun() -> 
                     case file:position(Fd, bof) of
                         {ok, 0} -> ok;
                         Error -> file_error(FileName, Error)
                     end,
                     file_loop_read(<<>>, ?CHUNK_SIZE, {Fd, FileName}) 
             end, Fd};
        Error ->
            table_post(Post),
            file_error(FileName, Error)
    end.

file_loop(<<Size:4/unit:8, B:Size/binary, Bin/binary>>, Fd_FileName, Ts) ->
    case catch binary_to_term(B) of
        {'EXIT', _} ->
            {_Fd, FileName} = Fd_FileName,
            throw_error({bad_object, FileName});
        Term -> 
            file_loop(Bin, Fd_FileName, [Term | Ts])
    end;
file_loop(<<Size:4/unit:8, B/binary>>=Bin, Fd_FileName, []) ->
    file_loop_read(Bin, Size - size(B) + 4, Fd_FileName);
file_loop(<<Size:4/unit:8, _/binary>>=Bin, Fd_FileName, Ts) ->
    lists:reverse(Ts, fun() -> file_loop_read(Bin, Size+4, Fd_FileName) end);
file_loop(B, Fd_FileName, Ts) ->
    lists:reverse(Ts, fun() -> file_loop_read(B,?CHUNK_SIZE,Fd_FileName) end).

%% After power failures (and only then) files with corrupted Size
%% fields have been observed in a disk_log file. If file:read/2 is
%% asked to read a huge amount of data the emulator may crash. Nothing
%% has been done here to prevent such crashes (by inspecting
%% BytesToRead in some way) since temporary files will never be read
%% after a power failure.
file_loop_read(B, BytesToRead, {Fd, FileName}=Fd_FileName) ->
    case file:read(Fd, BytesToRead) of
        {ok, Bin} when size(B) =:= 0 ->
            file_loop(Bin, Fd_FileName, []);
        {ok, Bin} ->
            case B of 
                <<Size:4/unit:8, Tl/binary>> when size(Bin)+size(Tl) >= Size ->
                    {B1, B2} = split_binary(Bin, Size - size(Tl)),
                    [T | Fun] = file_loop(list_to_binary([B, B1]),
                                           Fd_FileName, []),
                    true = is_function(Fun),
                    file_loop(B2, Fd_FileName, [T]);
                _ ->
                    file_loop(list_to_binary([B, Bin]), Fd_FileName, [])
            end;
        eof when size(B) =:= 0 ->
            [];
        eof ->
            throw_error({bad_object, FileName});
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
            {terms, []}; % cannot happen
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
sort_cursor_input_read(F, NoObjects) ->
    sort_cursor_input_read(F(), NoObjects).

unique_cache(L, Post, Opt) when is_list(L) ->
    true = Opt#optz.unique, %% assertion
    {unique_sort_list(L), Post};
unique_cache(H, Post, #optz{unique = false, cache = false}) ->
    {H, Post};
unique_cache(H, Post, #optz{unique = true, cache = false}) ->
    E = ets:new(qlc, [set, private]),
    {fun() -> no_dups(H, E) end, 
     Post#post{other = [del_table(E) | Post#post.other]}};
unique_cache(H, Post, #optz{unique = false, cache = true}) ->
    E = ets:new(qlc, [set, private]),
    {fun() -> cache(H, E, Post#post.local) end, 
     unique_cache_post(Post#post{local = []}, E)};
unique_cache(H, Post, #optz{unique = true, cache = true}) ->
    UT = ets:new(qlc, [bag, private]),
    MT = ets:new(qlc, [set, private]),
    {fun() -> ucache(H, UT, MT, Post#post.local) end, 
     unique_cache_post(Post#post{local = []}, [UT, MT])}.

unique_cache_post(Post, [E | Es]) ->
    unique_cache_post(unique_cache_post(Post, E), Es);
unique_cache_post(#post{local = Local, other = Other}, E) ->
    #post{local = [empty_table(E) | Local], other = [del_table(E) | Other]}.

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

do_sort(In, Out, sort, SortOptions, Post) ->
    Ref = make_ref(),
    fs_reply(Ref, catch {Ref, file_sorter:sort(In, Out, SortOptions)}, Post);
do_sort(In, Out, {keysort, KeyPos}, SortOptions, Post) ->
    Ref = make_ref(),
    fs_reply(Ref, 
             catch {Ref, file_sorter:keysort(KeyPos, In, Out, SortOptions)}, 
             Post).

fs_reply(Ref, {Ref, {error, Reason}}, Post) ->
    table_post(Post),
    throw_error(Reason);
fs_reply(Ref, Reply, Post) ->
    test_reply(Ref, Reply, Post).

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
append_loop(F, Hs) ->
    append_loop(F(), Hs).

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
no_dups(F, UTab) ->
    no_dups(F(), UTab).

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
    table_post(PostL),
    Cont;
cache([Object | Cont], MTab, SeqNo, PostL) ->
    true = ets:insert(MTab, {SeqNo, Object}),
    [Object | fun() -> cache(Cont, MTab, SeqNo + 1, PostL) end];
cache(F, MTab, SeqNo, PostL) ->
    cache(F(), MTab, SeqNo, PostL).

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
    table_post(PostL),
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
ucache(F, UTab, MTab, SeqNo, PostL) ->
    ucache(F(), UTab, MTab, SeqNo, PostL).

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

test_reply(Ref, {Ref, Reply}, _Post) ->
    Reply;
test_reply(_Ref, {'EXIT', Reason}, Post) ->
    table_post(Post),
    throw({exit, Reason});
test_reply(_Ref, Thrown, Post) ->
    table_post(Post),
    throw({throw, Thrown}).

%% The pre fun has been called from table_handle().
table_post(#post{local = Local, other = Other}) ->
    table_post(Local),
    table_post(Other);
table_post(L) ->
    lists:foreach(fun(undefined) -> ok;
                     (F) -> catch (F)() end, L).

call(undefined, _Arg, Default) ->
    Default;
call(Fun, Arg, _Default) ->
    Fun(Arg).

grd(undefined, _Arg) ->
    false;
grd(Fun, Arg) ->
    case Fun(Arg) of
        true -> 
            true;
        _ ->
            false
    end.

file_error(File, {error, Reason}) ->
    throw_error({file_error, File, Reason}).

throw_error(Reason) ->
    throw(error(Reason)).

error(Reason) ->
    {error, ?MODULE, Reason}.
