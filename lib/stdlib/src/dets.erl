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
-module(dets).

%% Disk based linear hashing lookup dictionary.

%% Public.
-export([all/0,
         close/1,
         delete/2,
         delete_all_objects/1,
         delete_object/2,
         first/1,
         foldl/3,
         foldr/3,
         from_ets/2,
         info/1,
         info/2,
         init_table/2,
         insert/2,
	 is_dets_file/1,
         lookup/2,
         match/1,
         match/2,
         match/3,
         match_delete/2,
         match_object/1,
         match_object/2,
         match_object/3,
         member/2,
         next/2,
         open_file/1,
         open_file/2,
         pid2name/1,
         safe_fixtable/2,
         select/1,
         select/2,
         select/3,
         select_delete/2,
         slot/2,
         sync/1,
         to_ets/2,
         traverse/2,
         update_counter/3]).

%% Server export.
-export([start/0, stop/0]).

%% To be used by mnesia only (fixtable/2 also used from fixtable_server).
-export([do_match/2, fixtable/2]).

%% Internal.
-export([do_open_file/2, do_open_file/3]).

%% Debug.
-export([file_info/1,
	 fsck/1,
         fsck/2,
	 get_head_field/2,
	 view/1,
	 where/2,
	 verbose/0,
	 verbose/1
	]).

%% Not documented, or not ready for publication.
-export([lookup_keys/2]).


-compile({inline, [{einval,2},{badarg,2},{badarg_exit,2}]}).

-include_lib("kernel/include/file.hrl").

-include("dets.hrl").

%%% This is the implementation of the mnesia file storage. Each (non
%%% ram-copy) table is maintained in a corresponding .DAT file. The
%%% dat file is organized as a segmented linear hashlist. The head of
%%% the file with the split indicator, size etc is held in ram by the
%%% server at all times.
%%%
%%% The parts specific for formats up to and including 8(c) are
%%% implemented in dets_v8.erl, parts specific for format 9 are
%%% implemented in dets_v9.erl.

%%  The method of hashing is the so called linear hashing algorithm
%%  with segments. 
%%
%%  Linear hashing:
%%
%%         - n indicates next bucket to split (initially zero); 
%%         - m is the size of the hash table 
%%  
%%         - to insert: 
%%                - hash = key mod m 
%%                - if hash < n then hash = key mod 2m 
%%                - when the number of objects exceeds the initial size
%%                  of the hash table, each insertion of an object
%%                  causes bucket n to be split:
%%                      - add a new bucket to the end of the table 
%%                      - redistribute the contents of bucket n 
%%                        using hash = key mod 2m 
%%                      - increment n 
%%                      - if n = m then m = 2m, n = 0 
%%         - to search: 
%%                hash = key mod m 
%%                if hash < n then hash = key mod 2m 
%%                do linear scan of the bucket 
%%  

%%% If a file error occurs on a working dets file, update_mode is set
%%% to the error tuple. When in 'error' mode, the free lists are not
%%% written, and a repair is forced next time the file is opened.

-record(dets_cont, {
         what,        % object | bindings | select
	 no_objs,     % requested number of objects: default | integer() > 0
	 pos,         % file position, where to read next chunk, or eof
	 bin,         % small chunk not consumed, ends at 'pos'
	 alloc,       % the part of the file not yet scanned, mostly a binary
	 tab,
         arg
	 }).

-record(open_args, {
          file,
          type,
          keypos,
          repair,
          min_no_slots,
	  max_no_slots,
          ram_file,
          delayed_write,
          auto_save,
          access,
          version
         }).

-define(PATTERN_TO_OBJECT_MATCH_SPEC(Pat), [{Pat,[],['$_']}]).
-define(PATTERN_TO_BINDINGS_MATCH_SPEC(Pat), [{Pat,[],['$$']}]).
-define(PATTERN_TO_TRUE_MATCH_SPEC(Pat), [{Pat,[],[true]}]).

%%-define(DEBUGM(X, Y), io:format(X, Y)).
-define(DEBUGM(X, Y), true).

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%%-define(PROFILE(C), C).
-define(PROFILE(C), void).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all() ->
    dets_server:all().

close(Tab) ->  
    dets_server:close(Tab).

delete(Tab, Key) ->
    badarg(treq(Tab, {delete_key, Key}), [Tab, Key]).

delete_all_objects(Tab) ->
    badarg(treq(Tab, delete_all_objects), [Tab]).

delete_object(Tab, O) ->
    badarg(treq(Tab, {delete_object, O}), [Tab, O]).

%% Given a filename, fsck it. Debug.
fsck(Fname) ->
    fsck(Fname, default).

fsck(Fname, Version) ->
    catch begin
      {ok, Fd, FH} = read_file_header(Fname, read, false),
      ?DEBUGF("FileHeader: ~p~n", [FH]),	    
      {Bump, _Base} = constants(FH, Fname),
      fsck(Fd, make_ref(), Fname, FH, default, default, Bump, Version)
      end.

first(Tab) ->
    badarg_exit(treq(Tab, first), [Tab]).

fixtable(Tab, Bool) when Bool == true; Bool == false ->
    badarg(treq(Tab, {fixtable, Bool}), [Tab, Bool]);
fixtable(Tab, Term) ->
    erlang:fault(badarg, [Tab, Term]).

foldr(Fun, Acc, Tab) ->
    foldl(Fun, Acc, Tab).

foldl(Fun, Acc, Tab) ->
    Ref = make_ref(),
    do_traverse(Fun, Acc, Tab, Ref).

from_ets(DTab, ETab) ->
    ets:safe_fixtable(ETab, true),
    Spec = ?PATTERN_TO_OBJECT_MATCH_SPEC('_'),
    LC = ets:select(ETab, Spec, 100),
    InitFun = from_ets_fun(LC, ETab),
    Reply = treq(DTab, {initialize, InitFun}),
    ets:safe_fixtable(ETab, false),
    case Reply of 
        {thrown, Thrown} -> throw(Thrown);
        Else -> Else
    end.

from_ets_fun(LC, ETab) ->
    fun(close) ->
            ok;
       (read) when LC == '$end_of_table' ->
            end_of_input;
       (read) ->
            {L, C} = LC,
            {L, from_ets_fun(ets:select(C), ETab)}
    end.

info(Tab) ->
    case catch dets_server:get_pid(Tab) of
	{'EXIT', _Reason} ->
	    undefined;
	Pid ->
	    req(Pid, info)
    end.

info(Tab, owner) ->
    case catch dets_server:get_pid(Tab) of
	Pid when pid(Pid) ->
	    Pid;
	_ ->
	    undefined
    end;
info(Tab, users) -> % undocumented
    case dets_server:users(Tab) of
	[] ->
	    undefined;
	Users ->
	    Users
    end;
info(Tab, Tag) ->
    case catch dets_server:get_pid(Tab) of
	{'EXIT', _Reason} ->
	    undefined;
	Pid ->
	    req(Pid, {info, Tag})
    end.

init_table(Tab, InitFun) when function(InitFun) ->
    case treq(Tab, {initialize, InitFun}) of
        {thrown, Thrown} -> throw(Thrown);
        Else -> Else
    end;
init_table(Tab, InitFun) ->
    erlang:fault(badarg, [Tab, InitFun]).

insert(Tab, Objs) when list(Objs) ->
    badarg(treq(Tab, {insert, Objs}), [Tab, Objs]);
insert(Tab, Obj) ->
    badarg(treq(Tab, {insert, [Obj]}), [Tab, Obj]).

lookup(Tab, Key) ->
    badarg(treq(Tab, {lookup_keys, [Key]}), [Tab, Key]).

%% Not public.
lookup_keys(Tab, Keys) ->
    case check_list(Keys) of
	true -> badarg(treq(Tab, {lookup_keys, Keys}), [Tab, Keys]);
	false -> erlang:fault(badarg, [Tab, Keys])
    end.

match(Tab, Pat) ->
    badarg(safe_match(Tab, Pat, bindings), [Tab, Pat]).

match(Tab, Pat, N) ->
    badarg(init_chunk_match(Tab, Pat, bindings, N), [Tab, Pat, N]).
    
match(State) when State#dets_cont.what == bindings ->
    chunk_match(State);
match(Term) ->
    erlang:fault(badarg, [Term]).

match_delete(Tab, Pat) ->
    badarg(match_delete(Tab, Pat, delete), [Tab, Pat]).

match_delete(Tab, Pat, What) ->
    safe_fixtable(Tab, true),    
    case compile_match_spec(What, Pat) of
	{Spec, MP} ->
	    Proc = dets_server:get_pid(Tab),
	    R = req(Proc, {init_match_delete, MP, Spec}),
	    do_match_delete(Tab, Proc, R, What, 0);
	badarg ->
	    badarg
    end.

do_match_delete(Tab, _Proc, {done, N1}, select, N) when integer(N1) ->
    safe_fixtable(Tab, false),
    N + N1;
do_match_delete(Tab, _Proc, {done, N1}, _What, _N) when integer(N1) ->
    safe_fixtable(Tab, false),
    ok;
do_match_delete(Tab, _Proc, {done, R}, _What, _N) ->
    safe_fixtable(Tab, false),
    R;
do_match_delete(Tab, Proc, {cont, State, N1}, What, N) ->
    do_match_delete(Tab, Proc, req(Proc, {match_delete, State}), What, N+N1).

match_object(Tab, Pat) ->
    badarg(safe_match(Tab, Pat, object), [Tab, Pat]).

match_object(Tab, Pat, N) ->
    badarg(init_chunk_match(Tab, Pat, object, N), [Tab, Pat, N]).
    
match_object(State) when State#dets_cont.what == object ->
    chunk_match(State);
match_object(Term) ->
    erlang:fault(badarg, [Term]).

member(Tab, Key) ->
    badarg(treq(Tab, {member, Key}), [Tab, Key]).    

next(Tab, Key) ->
    badarg_exit(treq(Tab, {next, Key}), [Tab, Key]).

is_dets_file(FileName) ->
    case catch read_file_header(FileName, read, false) of
	{ok, Fd, FH} ->
	    file:close(Fd),
	    FH#fileheader.cookie == ?MAGIC;
	{error, {tooshort, _}} ->
	    false;
	{error, {not_a_dets_file, _}} ->
	    false;
	Other ->
	    Other
    end.

%% Assuming that a file already exists, open it with the
%% parameters as already specified in the file itself.
%% Return a ref leading to the file.
open_file(File) ->
    einval(dets_server:open_file(File), [File]).

open_file(Tab, Args) when list(Args) ->
    case catch defaults(Tab, Args) of
        OpenArgs when record(OpenArgs, open_args) ->
            einval(dets_server:open_file(Tab, OpenArgs),
		   [Tab, Args]);
	_ ->
	    erlang:fault(badarg, [Tab, Args])
    end;
open_file(Tab, Arg) ->
    open_file(Tab, [Arg]).

pid2name(Pid) ->
    dets_server:pid2name(Pid).

safe_fixtable(Tab, Bool) when Bool == true; Bool == false ->
    badarg(treq(Tab, {safe_fixtable, Bool}), [Tab, Bool]);
safe_fixtable(Tab, Term) ->
    erlang:fault(badarg, [Tab, Term]).

select(Tab, Pat) ->
    badarg(safe_match(Tab, Pat, select), [Tab, Pat]).

select(Tab, Pat, N) ->
    badarg(init_chunk_match(Tab, Pat, select, N), [Tab, Pat, N]).
    
select(State) when State#dets_cont.what == select ->
    chunk_match(State);
select(Term) ->
    erlang:fault(badarg, [Term]).

select_delete(Tab, Pat) ->
    badarg(match_delete(Tab, Pat, select), [Tab, Pat]).

slot(Tab, Slot) when integer(Slot), Slot >= 0 ->
    badarg(treq(Tab, {slot, Slot}), [Tab, Slot]);
slot(Tab, Term) ->
    erlang:fault(badarg, [Tab, Term]).

start() ->
    dets_server:start().

stop() ->
    dets_server:stop().

sync(Tab) ->
    badarg(treq(Tab, sync), [Tab]).

to_ets(DTab, ETab) ->
    case ets:info(ETab, protection) of
	undefined ->
	    erlang:fault(badarg, [DTab, ETab]);
        _ ->
	    Fun = fun(X, T) -> true = ets:insert(T, X), T end,
	    foldl(Fun, ETab, DTab)
    end.

traverse(Tab, Fun) ->
    Ref = make_ref(),
    TFun = 
	fun(O, Ack) ->
		case Fun(O) of
		    continue  ->
			Ack;
		    {continue, Val} ->
			[Val | Ack];
		    {done, Value} ->
			throw({Ref, [Value | Ack]});
		    Other ->
			throw({Ref, Other})
		end
	end,
    do_traverse(TFun, [], Tab, Ref).

update_counter(Tab, Key, C) ->
    badarg(treq(Tab, {update_counter, Key, C}), [Tab, Key, C]).

verbose() ->           
    verbose(true).

verbose(What) ->
    ok = dets_server:verbose(What),
    All = dets_server:all(),
    Fun = fun(Tab) -> treq(Tab, {set_verbose, What}) end,
    lists:foreach(Fun, All),
    All.

%% Where in the (open) table is Object located? 
%% The address of the first matching object is returned.
%% Format 9 returns the address of the object collection.
%% -> {ok, Address} | false
where(Tab, Object) ->
    badarg(treq(Tab, {where, Object}), [Tab, Object]).

do_traverse(Fun, Acc, Tab, Ref) ->
    safe_fixtable(Tab, true),    
    Proc = dets_server:get_pid(Tab),
    R = (catch do_trav(Proc, Acc, Fun, Ref)),
    safe_fixtable(Tab, false),
    case R of
	{Ref, Result} ->
	    Result;
	{'EXIT', Reason} ->
	    erlang:fault(Reason);
	Thrown ->
	    throw(Thrown)
    end.

do_trav(Proc, Acc, Fun, Ref) ->
    {Spec, MP} = compile_match_spec(object, '_'),
    case req(Proc, {match, MP, Spec, default}) of
	{done, {error, Reason}} ->
	    {Ref, {error, Reason}};
	{cont, State} ->
	    do_trav(State, Proc, Acc, Fun, Ref)
    end.
    
do_trav(#dets_cont{pos = eof}, _Proc, Acc, _Fun, Ref) ->
    {Ref, Acc};
do_trav(State, Proc, Acc, Fun, Ref) ->
    case req(Proc, {match, State}) of
	{cont, {Bins, NewState}} ->
	    do_trav_bins(NewState, Proc, Acc, Fun, Ref, Bins);
	Error ->
	    {Ref, Error}
    end.

do_trav_bins(State, Proc, Acc, Fun, Ref, []) ->
    do_trav(State, Proc, Acc, Fun, Ref);
do_trav_bins(State, Proc, Acc, Fun, Ref, [Bin | Bins]) ->
    %% Unpack one binary at a time, using the client's heap.
    case catch binary_to_term(Bin) of 
	{'EXIT', _} ->
	    {Ref, req(Proc, {corrupt, bad_object})};
	Term ->
	    NewAcc = Fun(Term, Acc),
	    do_trav_bins(State, Proc, NewAcc, Fun, Ref, Bins)
    end.

safe_match(Tab, Pat, What) ->
    safe_fixtable(Tab, true),
    R = do_safe_match(init_chunk_match(Tab, Pat, What, default), []),
    safe_fixtable(Tab, false),
    R.
    
do_safe_match({error, Error}, _L) ->
    {error, Error};
do_safe_match({L, C}, LL) ->
    do_safe_match(chunk_match(C), L++LL);
do_safe_match('$end_of_table', L) ->
    L;
do_safe_match(badarg, _L) ->
    badarg.

%% What = object | bindings | select
init_chunk_match(Tab, Pat, What, N) when integer(N), N >= 0; N == default ->
    case compile_match_spec(What, Pat) of
	{Spec, MP} ->
	    case req(dets_server:get_pid(Tab), {match, MP, Spec, N}) of
		{done, {error, Reason}} ->
		    {error, Reason};
		{done, L} ->
		    {L, #dets_cont{what = What, pos = eof}};
		{cont, State} ->
		    chunk_match(State#dets_cont{what = What, tab = Tab})
	    end;
	badarg ->
	    badarg
    end;
init_chunk_match(_Tab, _Pat, _What, _) ->
    badarg.

chunk_match(#dets_cont{pos = eof}) ->
    '$end_of_table';
chunk_match(State) ->
    Proc = dets_server:get_pid(State#dets_cont.tab),
    case req(Proc, {match, State}) of
	{cont, {Bins, NewState}} ->
	    Fun = NewState#dets_cont.arg,
	    foldl_bins(Bins, Fun, NewState, Proc, []);
	Error ->
	    Error
    end.

foldl_bins([], _Fun, State, _Proc, Terms) ->
    %% Preserve time order.
    {lists:reverse(Terms), State};

foldl_bins([Bin | Bins], Fun, State, Proc, Terms) ->
    case catch binary_to_term(Bin) of 
	{'EXIT', _} ->
	    req(Proc, {corrupt, bad_object});
	Term ->
	    NewTerms = Fun(Term, Terms),
	    foldl_bins(Bins, Fun, State, Proc, NewTerms)
    end.

check_list(L) ->
    case catch length(L) of
	{'EXIT', _} -> false;
	_ -> true
    end.

%% -> {Spec, binary()} | badarg
compile_match_spec(select, Spec) ->
    case catch ets:match_spec_compile(Spec) of
	X when binary(X) ->
	    {Spec, X};
	_ ->
	    badarg
    end;
compile_match_spec(object, Pat) ->
    compile_match_spec(select, ?PATTERN_TO_OBJECT_MATCH_SPEC(Pat));
compile_match_spec(bindings, Pat) ->
    compile_match_spec(select, ?PATTERN_TO_BINDINGS_MATCH_SPEC(Pat));
compile_match_spec(delete, Pat) ->
    compile_match_spec(select, ?PATTERN_TO_TRUE_MATCH_SPEC(Pat)).

%% Process the args list as provided to open_file/2
defaults(Tab, Args) ->
    Defaults = #open_args{file = to_list(Tab),
                          type = set,
                          keypos = 1,
                          repair = true, 
                          min_no_slots = default,
			  max_no_slots = default,
                          ram_file = false,
                          delayed_write = ?DEFAULT_CACHE,
                          auto_save = timer:minutes(?DEFAULT_AUTOSAVE),
                          access = read_write,
                          version = default},
    Fun = fun repl/2,
    lists:foldl(Fun, Defaults, Args).

to_list(T) when atom(T) -> atom_to_list(T);
to_list(T) -> T.

repl({access, A}, Defs) ->
    mem(A, [read, read_write]),
    Defs#open_args{access = A};
repl({auto_save, Int}, Defs) when integer(Int), Int >= 0 ->
    Defs#open_args{auto_save = Int};
repl({auto_save, infinity}, Defs) ->
    Defs#open_args{auto_save =infinity};
repl({cache_size, Int}, Defs) when integer(Int), Int >= 0 ->
    %% Recognized, but ignored.
    Defs;
repl({cache_size, infinity}, Defs) ->
    Defs;
repl({delayed_write, default}, Defs) ->
    Defs#open_args{delayed_write = ?DEFAULT_CACHE};
repl({delayed_write, {Delay,Size} = C}, Defs) when integer(Delay), Delay >= 0,
						   integer(Size), Size >= 0 ->
    Defs#open_args{delayed_write = C};
repl({estimated_no_objects, I}, Defs)  ->
    repl({min_no_slots, I}, Defs);
repl({file, File}, Defs) ->
    Defs#open_args{file = to_list(File)};
repl({keypos, P}, Defs) when integer(P), P > 0 ->
    Defs#open_args{keypos =P};
repl({max_no_slots, I}, Defs)  ->
    %% Version 9 only.
    MaxSlots = is_max_no_slots(I),
    is_comp_min_max(Defs#open_args{max_no_slots = MaxSlots});
repl({min_no_slots, I}, Defs)  ->
    MinSlots = is_min_no_slots(I),
    is_comp_min_max(Defs#open_args{min_no_slots = MinSlots});
repl({ram_file, Bool}, Defs) ->
    mem(Bool, [true, false]),
    Defs#open_args{ram_file = Bool};
repl({repair, T}, Defs) ->
    mem(T, [true, false, force]),
    Defs#open_args{repair = T};
repl({type, T}, Defs) ->
    mem(T, [set, bag, duplicate_bag]),
    Defs#open_args{type =T};
repl({version, Version}, Defs) ->
    V = is_version(Version),
    is_comp_min_max(Defs#open_args{version = V});
repl({_, _}, _) ->
    exit(badarg).

is_min_no_slots(default) -> default;
is_min_no_slots(I) when integer(I), I >= ?DEFAULT_MIN_NO_SLOTS -> I;
is_min_no_slots(I) when integer(I), I > 0 -> ?DEFAULT_MIN_NO_SLOTS.

is_max_no_slots(default) -> default;
is_max_no_slots(I) when integer(I), I > 0, I < 1 bsl 31 -> I.

is_comp_min_max(Defs) ->
    %% Bug: accepts version = default, when default means version 8.
    #open_args{max_no_slots = Max, min_no_slots = Min, version = V} = Defs,
    case V of
	8 when Max == default, Min == default -> Defs;
	_ when V =/= 8, Min == default -> Defs;
	_ when V =/= 8, Max == default -> Defs;
	_ -> V =/= 8, true = Min =< Max, Defs
    end.

is_version(default) -> default;
is_version(8) -> 8;
is_version(9) -> 9.

mem(X, L) ->
    case lists:member(X, L) of
	true -> true;
	false -> exit(badarg)
    end.

treq(Tab, R) ->
    case catch dets_server:get_pid(Tab) of
	Pid when pid(Pid) ->
	    req(Pid, R);
	_ ->
	    badarg
    end.

req(Proc, R) -> 
    Proc ! {self(), R},
    receive 
	{Proc, Reply} -> 
	    Reply;
	{'EXIT', Proc, Reason} ->
	    exit(Reason)
    end.

%% Inlined.
einval({error, {file_error, _, einval}}, A) ->
    erlang:fault(badarg, A);
einval(Reply, _A) ->
    Reply.

%% Inlined.
badarg(badarg, A) ->
    erlang:fault(badarg, A);
badarg(Reply, _A) ->
    Reply.

%% Inlined.
badarg_exit(badarg, A) ->
    erlang:fault(badarg, A);
badarg_exit({ok, Reply}, _A) ->
    Reply;
badarg_exit({error, Reply}, _A) ->
    exit(Reply).

%%%-----------------------------------------------------------------
%%% Server functions
%%%-----------------------------------------------------------------

do_open_file(Fname, Verbose) ->
    process_flag(trap_exit, true),
    case catch fopen(Fname) of
	{error, Reason} ->
	    Error = err({error, Reason}),
	    ?SERVER_NAME ! {self(), Error},
	    exit(normal);
	{ok, Head} ->
	    ?SERVER_NAME ! {self(), {ok, Head#head.name}},
	    maybe_put(verbose, Verbose),
	    open_file_loop(Head);
	{'EXIT', _Reason} = Error ->
	    ?SERVER_NAME ! {self(), Error},
	    exit(normal);
	Bad ->
	    error_logger:format
	      ("** dets: Bug was found in open_file/1, reply was ~w.~n", 
	       [Bad]),
	    ?SERVER_NAME ! {self(), {error, {dets_bug, Fname, Bad}}},
	    exit(normal)
    end.

do_open_file(Tab, OpenArgs, Verb) ->
    ?PROFILE(ep:do()),
    process_flag(trap_exit, true),
    case catch fopen(Tab, OpenArgs) of
	{error, {tooshort, _}} ->
	    file:delete(OpenArgs#open_args.file),
	    do_open_file(Tab, OpenArgs, Verb);
	{error, Reason} ->
	    Error = err({error, Reason}),
	    ?SERVER_NAME ! {self(), Error},
	    exit(normal);
	{ok, Head} ->
	    ?SERVER_NAME ! {self(), {ok, Tab}},
	    maybe_put(verbose, Verb),
	    open_file_loop(Head);
	{'EXIT', _Reason} = Error ->
	    ?SERVER_NAME ! {self(), Error},
	    exit(normal);
	Bad ->
	    error_logger:format
	      ("** dets: Bug was found in open_file/2, arguments were~n"
	       "** dets: ~w and reply was ~w.~n", 
	       [OpenArgs, Bad]),
	    Err = {error, {dets_bug, Tab, {open_file, OpenArgs}, Bad}},
	    ?SERVER_NAME ! {self(), Err},
	    exit(normal)
    end.

maybe_put(_, undefined) ->
    ignore;
maybe_put(K, V) ->
    put(K, V).

open_file_loop(Head) ->
    open_file_loop(Head, 0).

open_file_loop(Head, N) ->
    receive
	{From, Op} ->
	    case catch apply_op(Op, From, Head, N) of
		ok -> 
		    open_file_loop(Head, N);
		{N2, H2} when record(H2, head), integer(N2) ->
		    open_file_loop(H2, N2);
		H2 when record(H2, head) ->
		    open_file_loop(H2, N);
		{'EXIT', normal} ->
		    exit(normal);
		Bad ->
		    Name = Head#head.name,
		    error_logger:format
		      ("** dets: Bug was found when accessing table ~w,~n"
		       "** dets: operation was ~p and reply was ~w.~n",
		       [Name, Op, Bad]),
		    From ! {self(), {error, {dets_bug, Name, Op, Bad}}},
		    open_file_loop(Head, N)
	    end;
	{'EXIT', Pid, _Reason} ->
	    %% A process fixing the table exits.
	    H2 = remove_fix(Head, Pid, close),
	    open_file_loop(H2, N)
    end.

apply_op(Op, From, Head, N) ->
    case Op of
	{add_user, Tab, OpenArgs}->
            #open_args{file = Fname, type = Type, keypos = Keypos, 
                       ram_file = Ram, access = Access, 
		       version = Version} = OpenArgs,
            VersionOK = (Version == default) or (Head#head.version == Version),
	    %% min_no_slots and max_no_slots are not tested
	    Res = if
		      Tab == Head#head.name,
		      Head#head.keypos == Keypos,
		      Head#head.type == Type,
		      Head#head.ram_file == Ram,
		      Head#head.access == Access,
		      VersionOK == true,
		      Fname == Head#head.filename ->
			  {ok, Tab};
		      true ->
			  err({error, incompatible_arguments})
		  end,
	    From ! {self(), Res},
	    ok;
	auto_save ->
	    case Head#head.update_mode of
		saved ->
		    Head;
		dirty when N == 0 ->
		    %% The updates seems to have declined
		    dets_utils:vformat("** dets: Auto save of ~p\n", 
                                       [Head#head.name]), 
		    {NewHead, _Res} = perform_save(Head),
		    erlang:garbage_collect(),
		    {0, NewHead};
		dirty -> 
		    %% Reset counter and try later
		    start_auto_save_timer(Head#head.auto_save),
		    {0, Head};
		{error, _Reason} ->
		    Head
	    end;
	close  ->
	    From ! {self(), {closed, fclose(Head)}},
	    _NewHead = unlink_fixing_procs(Head),
	    ?PROFILE(ep:done()),
	    exit(normal);
	{close, Pid} -> 
	    %% Used from dets_server when Pid has closed the table,
	    %% but the table is still opened by some process.
	    NewHead = remove_fix(Head, Pid, close),
	    From ! {self(), {closed, ok}},
	    NewHead;
	{corrupt, Reason} ->
	    {H2, Error} = dets_utils:corrupt_reason(Head, Reason),
	    From ! {self(), Error},
	    H2;
	{delayed_write, WrTime} ->
	    delayed_write(Head, WrTime);
	delete_all_objects ->
	    {H2, Res} = fdelete_all_objects(Head),
	    From ! {self(), Res},
	    {0, H2};
	{delete_key, Key} when Head#head.update_mode =/= saved ->
	    {H2, Res} = fdelete_key(Head, Key),
	    From ! {self(), Res},
	    {N + 1, H2};
	{delete_object, Object} when Head#head.update_mode =/= saved ->
	    {H2, Res} = fdelete_object(Head, Object),
	    From ! {self(), Res},
	    {N + 1, H2};
	first ->
	    {H2, Res} = ffirst(Head),
	    From ! {self(), Res},
	    H2;
	{fixtable, Fixed} ->
	    NewHead = do_fixtable(Head, From, Fixed),
	    From ! {self(), ok},
	    NewHead;
	info ->
	    {H2, Res} = finfo(Head),
	    From ! {self(), Res},
	    H2;
	{info, Tag} ->
	    {H2, Res} = finfo(Head, Tag),
	    From ! {self(), Res},
	    H2;
        {initialize, InitFun} ->
            {H2, Res} = finit(Head, InitFun),
            From ! {self(), Res},
            H2;
	{insert, Objs} when Head#head.update_mode =/= saved ->
	    {H2, Res} = finsert(Head, Objs),
	    From ! {self(), Res},
	    {N + 1, H2};
	{lookup_keys, Keys} ->
	    {H2, Res} = flookup_keys(Head, Keys),
	    From ! {self(), Res},
	    H2;
	{match, State} ->
	    {H2, Res} = fmatch(Head, State),
	    From ! {self(), Res},
	    H2;
	{match, MP, Spec, Limit} ->
	    {H2, Res} = fselect(Head, MP, Spec, Limit),
	    From ! {self(), Res},
	    H2;
	may_grow when Head#head.update_mode =/= saved ->
	    %% Won't grow more if the table is full.
	    {H2, _Res} = (Head#head.mod):may_grow(Head, 0, many_times),
	    {N + 1, H2};
	{member, Key} ->
	    {H2, Res} = fmember(Head, Key),
	    From ! {self(), Res},
	    H2;
	{next, Key} ->
	    {H2, Res} = fnext(Head, Key),
	    From ! {self(), Res},
	    H2;
	{match_delete, State} when Head#head.update_mode =/= saved ->
	    {H2, Res} = fmatch_delete(Head, State),
	    From ! {self(), Res},
	    {N + 1, H2};
	{init_match_delete, MP, Spec} when Head#head.update_mode =/= saved ->
	    {H2, Res} = fmatch_delete_begin(Head, MP, Spec),
	    From ! {self(), Res},
	    {N + 1, H2};
	{safe_fixtable, Bool} ->
	    NewHead = do_safe_fixtable(Head, From, Bool),
	    From ! {self(), ok},
	    NewHead;
	{set_verbose, What} ->
	    set_verbose(What), 
	    From ! {self(), ok},
	    ok;
	{slot, Slot} ->
	    {H2, Res} = fslot(Head, Slot),
	    From ! {self(), Res},
	    H2;
	sync ->
	    {NewHead, Res} = perform_save(Head),
	    From ! {self(), Res},
	    erlang:garbage_collect(),
	    {0, NewHead};
	{update_counter, Key, Incr} when Head#head.update_mode =/= saved ->
	    {NewHead, Res} = do_update_counter(Head, Key, Incr),
	    From ! {self(), Res},
	    {N + 1, NewHead};
	write_cache ->
	    {H2, Res0} = (catch write_cache(Head)),
	    Res = if Res0 == [] -> ok; true -> Res0 end,
	    From ! {self(), Res},
	    H2;
	{where, Object} ->
	    {H2, Res} = where_is_object(Head, Object),
	    From ! {self(), Res},
	    H2;
	WriteOp when Head#head.access == read_write,
		     Head#head.update_mode == saved ->
	    case catch (Head#head.mod):mark_dirty(Head) of
		ok ->
		    start_auto_save_timer(Head#head.auto_save),
		    H2 = Head#head{update_mode = dirty},
		    apply_op(WriteOp, From, H2, 0);
		{NewHead, Error} ->
		    From ! {self(), Error},
		    NewHead
	    end;

	WriteOp when tuple(WriteOp), Head#head.access == read ->
	    Reason = {access_mode, Head#head.filename},
	    From ! {self(), err({error, Reason})},
	    ok
    end.

start_auto_save_timer(infinity) ->
    ok;
start_auto_save_timer(Millis) ->
    erlang:send_after(Millis, self(), {self(), auto_save}).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

constants(FH, FileName) ->
    Version = FH#fileheader.version,
    if 
        Version =< 8 ->
            dets_v8:constants();
        Version == 9 ->
            dets_v9:constants();
        true ->
            throw({error, {not_a_dets_file, FileName}})
    end.

%% -> {ok, Fd, fileheader()} | throw(Error)
read_file_header(FileName, Access, RamFile) ->
    BF = if
	     RamFile == true ->
		 case file:read_file(FileName) of
		     {ok, B} -> B;
		     Err -> dets_utils:file_error(FileName, Err)
		 end;
	     true ->
		 FileName
	 end,
    {ok, Fd} = dets_utils:open(BF, open_args(Access, RamFile)),
    {ok, <<Version:32>>} = 
        dets_utils:pread_close(Fd, FileName, ?FILE_FORMAT_VERSION_POS, 4),
    if 
        Version =< 8 ->
            dets_v8:read_file_header(Fd, FileName);
        Version == 9 ->
            dets_v9:read_file_header(Fd, FileName);
        true ->
            throw({error, {not_a_dets_file, FileName}})
    end.

fclose(Head) ->
    {Head1, Res} = perform_save(Head),
    case Head1#head.ram_file of
	true -> 
	    ignore;
	false -> 
	    file:close(Head1#head.fptr)
    end,
    Res.

%% -> {NewHead, Res}
perform_save(Head) when Head#head.update_mode == dirty ->
    catch begin
              {NewHead, []} = write_cache(Head),
              (Head#head.mod):do_perform_save(NewHead)
          end;
perform_save(Head) when Head#head.update_mode == saved ->
    ?DEBUGF("~p: Already saved.~n", [Head#head.name]),
    {Head, ok};
perform_save(Head) ->
    ?DEBUGF("~p: Table is in error state, no save performed.~n", 
	    [Head#head.name]),
    {Head, Head#head.update_mode}. % Or 'ok'?

%% -> {NewHead, Result}
fdelete_all_objects(Head) ->
    #head{fptr = Fd, name = Tab, filename = Fname, type = Type, keypos = Kp, 
	  ram_file = Ram, auto_save = Auto, min_no_slots = MinSlots,
	  max_no_slots = MaxSlots, cache = Cache} = Head, 
    CacheSz = dets_utils:cache_size(Cache),
    case catch (Head#head.mod):initiate_file(Fd, Tab, Fname, Type, Kp, 
					     MinSlots, MaxSlots, Ram, 
					     CacheSz, Auto) of
	{ok, NewHead} ->
	    {NewHead, ok};
	{error, Reason} ->
	    dets_utils:corrupt_reason(Head, Reason)
    end.

%% -> {NewHead, Reply}, Reply = ok | Error.
fdelete_key(Head, Key) ->
    do_delete(Head, [Key], delete_key).

%% -> {NewHead, Reply}, Reply = ok | badarg | Error.
fdelete_object(Head, Object) ->
    Kp = Head#head.keypos,
    case check_objects([Object], Kp) of
	true ->
	    do_delete(Head, [Object], delete_object);
	false ->
	    {Head, badarg}
    end.

ffirst(H) ->
    Ref = make_ref(),
    case catch {Ref, ffirst1(H)} of
	{Ref, {NH, R}} -> 
	    {NH, {ok, R}};
	{NH, R} -> 
	    {NH, {error, R}}
    end.

ffirst1(H) ->
    {NH, []} = write_cache(H),
    ffirst(NH, 0).

ffirst(H, Slot) ->
    case (H#head.mod):slot_objs(H, Slot) of
	'$end_of_table' -> {H, '$end_of_table'};
	[] -> ffirst(H, Slot+1);
	[X|_] -> {H, element(H#head.keypos, X)}
    end.

%% -> {NewHead, Reply}, Reply = ok | badarg | Error.
finsert(Head, Objects) ->
    case check_objects(Objects, Head#head.keypos) of
	true -> 
	    case catch update_cache(Head, Objects, insert) of
		{NewHead, []} ->
		    {NewHead, ok};
		Reply ->
		    Reply
	    end;
	false -> 
	    {Head, badarg}
    end.

do_safe_fixtable(Head, Pid, true) ->
    case Head#head.fixed of 
	false -> 
	    link(Pid),
	    Fixed = {erlang:now(), [{Pid, 1}]},
	    Ftab = dets_utils:get_freelists(Head),
	    Head#head{fixed = Fixed, freelists = {Ftab, Ftab}};
	{TimeStamp, Counters} ->
	    case lists:keysearch(Pid, 1, Counters) of
		{value, {Pid, Counter}} -> % when Counter > 1
		    NewCounters = lists:keyreplace(Pid, 1, Counters, 
						   {Pid, Counter+1}),
		    Head#head{fixed = {TimeStamp, NewCounters}};
		false ->
		    link(Pid),
		    Fixed = {TimeStamp, [{Pid, 1} | Counters]},
		    Head#head{fixed = Fixed}
	    end
    end;
do_safe_fixtable(Head, Pid, false) ->
    remove_fix(Head, Pid, false).

remove_fix(Head, Pid, How) ->
    case Head#head.fixed of 
	false -> 
	    Head;
	{TimeStamp, Counters} ->
	    case lists:keysearch(Pid, 1, Counters) of
		%% How == close when Pid closes the table.
		{value, {Pid, Counter}} when Counter == 1; How == close ->
		    unlink(Pid),
		    case lists:keydelete(Pid, 1, Counters) of
			[] -> 
			    check_growth(Head),
			    erlang:garbage_collect(),
			    Head#head{fixed = false, 
				  freelists = dets_utils:get_freelists(Head)};
			NewCounters ->
			    Head#head{fixed = {TimeStamp, NewCounters}}
		    end;
		{value, {Pid, Counter}} ->
		    NewCounters = lists:keyreplace(Pid, 1, Counters, 
						   {Pid, Counter-1}),
		    Head#head{fixed = {TimeStamp, NewCounters}};
		false ->
		    Head
	    end
    end.

unlink_fixing_procs(Head) ->
    case Head#head.fixed of
	false ->
	    Head;
	{_, Counters} ->
	    lists:map(fun({Pid, _Counter}) -> unlink(Pid) end, Counters),
	    Head#head{fixed = false, 
		      freelists = dets_utils:get_freelists(Head)}
    end.

%% -> NewHead
do_fixtable(Head, _Pid, false) when Head#head.fixed =/= false ->
    check_growth(Head),
    unlink_fixing_procs(Head);
do_fixtable(Head, Pid, true) ->
    do_safe_fixtable(Head, Pid, true);
do_fixtable(Head, _Pid, _Value) ->
    Head.

check_growth(Head) ->
    NoThings = no_things(Head),
    if
	NoThings > Head#head.next ->
	    self() ! {self(), may_grow}; % Catch up.
	true ->
	    ok
    end.

finfo(H) -> 
    case catch write_cache(H) of
	{H2, []} ->
	    Info = [{type, H2#head.type}, 
		    {keypos, H2#head.keypos}, 
		    {size, H2#head.no_objects},
		    {file_size, file_size(H2#head.fptr)},
		    {filename, H2#head.filename}],
	    {H2, Info};
	HeadError ->
	    HeadError
    end.

finfo(H, access) -> {H, H#head.access};
finfo(H, auto_save) -> {H, H#head.auto_save};
finfo(H, delayed_write) -> % undocumented
    {H, dets_utils:cache_size(H#head.cache)};
finfo(H, filename) -> {H, H#head.filename};
finfo(H, file_size) ->
    case catch write_cache(H) of
	{H2, []} ->
	    {H2, file_size(H#head.fptr)};
	HeadError ->
	    HeadError
    end;
finfo(H, fixed) -> 
    %% true if fixtable/2 has been called
    {H, not (H#head.fixed == false)}; 
finfo(H, hash) -> {H, H#head.hash_bif};
finfo(H, keypos) -> {H, H#head.keypos};
finfo(H, memory) -> finfo(H, file_size);
finfo(H, no_objects) -> finfo(H, size);
finfo(H, no_keys) ->
    case catch write_cache(H) of
	{H2, []} ->
	    {H2, H2#head.no_keys};
	HeadError ->
	    HeadError
    end;
finfo(H, pid) -> {H, self()};
finfo(H, ram_file) -> {H, H#head.ram_file};
finfo(H, safe_fixed) -> {H, H#head.fixed};
finfo(H, size) -> 
    case catch write_cache(H) of
	{H2, []} ->
	    {H2, H2#head.no_objects};
	HeadError ->
	    HeadError
    end;
finfo(H, no_slots) -> {H, (H#head.mod):no_slots(H)};
finfo(H, type) -> {H, H#head.type};
finfo(H, version) -> {H, H#head.version};
finfo(H, _) -> {H, undefined}.

file_size(F) -> 
    {ok, Pos} = file:position(F, eof),
    Pos.

%% -> {Head, Result}, Result = ok | Error | {thrown, Error}
finit(Head, _InitFun) when Head#head.access == read ->
    {Head, {error, {access_mode, Head#head.filename}}};
finit(Head, _InitFun) when Head#head.fixed =/= false ->
    {Head, {error, {fixed_table, Head#head.name}}};
finit(Head, InitFun) ->
    #head{fptr = Fd, type = Type, keypos = KeyPos, auto_save = Auto,
          cache = Cache, filename = Fname, access = Acc, ram_file = Ram,
	  min_no_slots = MinSlots, max_no_slots = MaxSlots,
          name = Tab, version = Version} = Head,
    CacheSz = dets_utils:cache_size(Cache),
    SlotNumbers = {MinSlots, bulk_init, MaxSlots},
    case catch fsck_try(Fd, Tab, Type, KeyPos, Fname, InitFun, SlotNumbers, 
                        not_used, Version) of
        ok ->
            OpenArgs = #open_args{file = Fname, type = Type, keypos = KeyPos,
                                  repair = false, min_no_slots = MinSlots,
				  max_no_slots = MaxSlots, ram_file = Ram, 
				  delayed_write = CacheSz, auto_save = Auto, 
				  access = Acc, version = default},
            case catch fopen(Tab, OpenArgs) of
                {ok, NewHead} ->
                    {NewHead, ok};
                Else ->
                    %% Head#head.fptr has been closed...
                    {Head, Else}
            end;
        Else ->
            %% Head#head.fptr has been closed...
            {Head, Else}
    end.

%% -> {NewHead, [LookedUpObject]} | {NewHead, Error}
flookup_keys(Head, Keys) ->
    catch do_flookup_keys(Head, Keys).

do_flookup_keys(Head, Keys) ->
    case dets_utils:cache_lookup(Head#head.type, Keys, Head) of
	{NewHead, false} ->
	    update_cache(NewHead, Keys, lookup);
	{NewHead, {Found, []}} ->
	    {NewHead, Found};
	{Head1, {Found, Ks}} ->
	    {Head2, R} = direct_lookup(Head1, Ks),
	    {Head2, Found ++ R}
    end.

fmatch(Head, C) ->
    case scan(Head, binary, C) of
	{scan_error, Reason} ->
	    dets_utils:corrupt_reason(Head, Reason);
	{Ts, NC} ->
	    {Head, {cont, {Ts, NC}}}
    end.

%% -> {NewHead, Res}
fmatch_delete_begin(Head, MP, Spec)  ->
    KeyPos = Head#head.keypos,
    MFun = fun(Term) ->
		   [true] == ets:match_spec_run([Term], MP)
	   end,
    case find_all_keys(Spec, KeyPos, []) of
	[] -> 
	    catch do_fmatch_delete_var_keys(Head, MFun, Spec);
	List ->
	    Keys = lists:usort(List),
            do_fmatch_con_keys(Head, Keys, MFun)
    end.

%% A note: It is possible that the binary in C contains the first part
%% of _one_ term. If that term is deleted here, the binary will not be
%% affected, which means that next call to scan/1 will find that term
%% active (unless it was free already...). The (remaining) terms of
%% the slot where the term resided will then be traversed once more.
fmatch_delete(Head, C) ->
    case scan(Head, term, C) of
	{scan_error, Reason} ->
	    {NewHead, Error} = dets_utils:corrupt_reason(Head, Reason),
	    {NewHead, {done, Error}};
	{[], _} ->
	    {Head, {done, 0}};
	{RTs, NC} ->
	    MFun = C#dets_cont.arg,
	    Terms = lists:filter(MFun, RTs),
	    do_fmatch_delete(Head, Terms, NC)
    end.

do_fmatch_delete_var_keys(Head, _MFun, ?PATTERN_TO_TRUE_MATCH_SPEC('_'))  ->
    %% Handle the case where the file is emptied efficiently.
    %% Empty the cache just to get the number of objects right.
    {Head1, []} = write_cache(Head),
    N = Head1#head.no_objects,
    case fdelete_all_objects(Head1) of
	{NewHead, ok} ->
	    {NewHead, {done, N}};
	{NewHead, Error} ->
	    {NewHead, {done, Error}}
    end;
do_fmatch_delete_var_keys(Head, MFun, _Spec) ->
    {NewHead, []} = write_cache(Head),
    C0 = init_scan(NewHead, default),
    {NewHead, {cont, C0#dets_cont{arg = MFun}, 0}}.

do_fmatch_con_keys(Head, Keys, MFun) ->
    case flookup_keys(Head, Keys) of
	{NewHead, ReadTerms} when list(ReadTerms) ->
	    Terms = lists:filter(MFun, ReadTerms),
	    do_fmatch_delete(NewHead, Terms, fixed);
	{NewHead, Error} ->
	    {NewHead, {done, Error}}
    end.

do_fmatch_delete(Head, Terms, What) ->
    N = length(Terms),
    case do_delete(Head, Terms, delete_object) of
	{NewHead, ok} when What == fixed ->
	    {NewHead, {done, N}};
	{NewHead, ok} ->
	    {NewHead, {cont, What, N}};
	{NewHead, Res} ->
	    {NewHead, {done, Res}}
    end.

do_delete(Head, Things, What) ->
    case catch update_cache(Head, Things, What) of
	{NewHead, []} ->
	    {NewHead, ok};
	HeadError ->
	    HeadError
    end.

fmember(Head, Key) ->
    catch do_fmember(Head, Key).

do_fmember(Head, Key) ->
    Keys = [Key],
    {NewHead, LookedUpObjects} = 
	case dets_utils:cache_lookup(Head#head.type, Keys, Head) of
	    {Head1, false} ->
		update_cache(Head1, Keys, lookup);
	    {Head1, {Found, []}} ->
		{Head1, Found};
	    {Head1, {[], Ks}} ->
		direct_lookup(Head1, Ks)
	end,
    {NewHead, LookedUpObjects =/= []}.

fnext(Head, Key) ->
    Slot = (Head#head.mod):db_hash(Key, Head),
    Ref = make_ref(),
    case catch {Ref, fnext(Head, Key, Slot)} of
	{Ref, {H, R}} -> 
	    {H, {ok, R}};
	{H, R} -> 
	    {H, {error, R}}
    end.

fnext(H, Key, Slot) ->
    {NH, []} = write_cache(H),
    case (H#head.mod):slot_objs(NH, Slot) of
	'$end_of_table' -> {NH, '$end_of_table'};
	L -> fnext_search(NH, Key, Slot, L)
    end.

fnext_search(H, K, Slot, L) ->
    Kp = H#head.keypos,
    case beyond_key(K, Kp, L) of
	[] -> fnext_slot(H, K, Slot+1);
	L2 -> {H, element(H#head.keypos, hd(L2))}
    end.

%% We've got to continue to search for the next key in the next slot
fnext_slot(H, K, Slot) ->
    case (H#head.mod):slot_objs(H, Slot) of
	'$end_of_table' -> {H, '$end_of_table'};
	[] -> fnext_slot(H, K, Slot+1);
	L -> {H, element(H#head.keypos, hd(L))}
    end.

beyond_key(_K, _Kp, []) -> [];
beyond_key(K, Kp, [H|T]) when element(Kp, H) /= K ->
    beyond_key(K, Kp, T);
beyond_key(K, Kp, [H|T]) when element(Kp, H) == K ->
    beyond_key2(K, Kp, T).

beyond_key2(_K, _Kp, []) -> [];
beyond_key2(K, Kp, [H|T]) when element(Kp, H) == K ->
    beyond_key2(K, Kp, T);
beyond_key2(_K, _Kp, L) ->
    L.

%% Open an already existing file, no arguments
%% -> {ok, head()} | throw(Error)
fopen(Fname) ->
    case file:read_file_info(Fname) of
	{ok, _} ->
	    Tab = make_ref(),
	    Acc = read_write,
	    Ram = false, 
	    {ok, Fd, FH} = read_file_header(Fname, Acc, Ram),
            Mod = FH#fileheader.mod,
	    case Mod:check_file_header(FH, Fd) of
		{error, not_closed} ->
		    io:format(user,"dets: file ~p not properly closed, "
			      "repairing ...~n", [Fname]),
                    {Bump, _Base} = constants(FH, Fname),
                    Version = default,
                    case fsck(Fd, Tab, Fname, FH, default, default, 
			      Bump, Version) of
                        ok ->
                            fopen(Fname);
                        Error ->
                            throw(Error)
                    end;
		{ok, Head, ExtraInfo} ->
		    open_final(Head, Fname, Acc, Ram, ?DEFAULT_CACHE, 
			       Tab, ExtraInfo, FH);
		{error, Reason} ->
		    file:close(Fd),
		    throw({error, {Reason, Fname}})
	    end;
	Error ->
	    dets_utils:file_error(Fname, Error)
    end.

%% Open and possibly create and initialize a file
%% -> {ok, head()} | throw(Error)
fopen(Tab, OpenArgs) ->
    FileName = OpenArgs#open_args.file,
    case filelib:is_file(FileName) of
	true ->
	    fopen_existing_file(Tab, OpenArgs);
	false when OpenArgs#open_args.access == read ->
	    throw({error, {file_error, FileName, enoent}});
	false ->
	    fopen_init_file(Tab, OpenArgs)
    end.

fopen_existing_file(Tab, OpenArgs) ->
    #open_args{file = Fname, type = Type, keypos = Kp, repair = Rep,
               min_no_slots = MinSlots, max_no_slots = MaxSlots,
               ram_file = Ram, delayed_write = CacheSz, auto_save =
               Auto, access = Acc, version = Version} =
        OpenArgs,
    {ok, Fd, FH} = read_file_header(Fname, Acc, Ram),
    {Bump0, _Base} = constants(FH, Fname),
    Do = case (FH#fileheader.mod):check_file_header(FH, Fd) of
	     _ when Rep == force ->
		 M = ", repair forced.",
		 {repair, M, Bump0};
	     {ok, _Head, need_compacting} when Rep == true ->
		 %% The file needs to be compacted due to a very big
		 %% and fragmented free_list. Version 8 only.
		 M = " is now compacted ...",
		 {repair, M, Bump0};
	     {ok, Head, ExtraInfo} ->
		 {final, Head, ExtraInfo};
	     {error, Reason} when Acc == read ->
		 file:close(Fd),
		 throw({error, {Reason, Fname}});
	     {error, not_closed} when Rep == true ->
		 %% Have to repair the file
		 M = " not properly closed, repairing ...",
		 {repair, M, Bump0};
	     {error, not_closed} when Rep == false ->
		 file:close(Fd),
		 throw({error, {needs_repair, Fname}});
	     {error, version_bump} when Rep == true ->
                 %% Version 8 only
		 M = " old version, upgrading ...",
		 {repair, M, 1};
	     {error, Reason} ->
		 file:close(Fd),
		 throw({error, {Reason, Fname}})
	 end,
    case Do of
	_ when FH#fileheader.type =/= Type ->
	    file:close(Fd),
	    throw({error, {type_mismatch, Fname}});
	_ when FH#fileheader.keypos =/= Kp ->
	    file:close(Fd),
	    throw({error, {keypos_mismatch, Fname}});
	{repair, Mess, Bump} ->
	    io:format(user, "dets: file ~p~s~n", [Tab, Mess]),
            case fsck(Fd, Tab, Fname, FH, MinSlots, MaxSlots, Bump, Version) of
                ok ->
                    %% No need to update 'version'.
                    fopen(Tab, OpenArgs#open_args{repair = false});
                Error ->
                    throw(Error)
            end;
	_ when FH#fileheader.version =/= Version, Version =/= default ->
	    file:close(Fd),
	    throw({error, {version_mismatch, Fname}});
	{final, H, EI} ->
	    H1 = H#head{auto_save = Auto},
	    open_final(H1, Fname, Acc, Ram, CacheSz, Tab, EI, FH)
    end.

%% -> {ok, head()} | throw(Error)
open_final(Head, Fname, Acc, Ram, CacheSz, Tab, ExtraInfo, FH) ->
    Fd = Head#head.fptr,
    #fileheader{mod = Mod, version = Version, 
		min_no_slots = MinSlots, max_no_slots = MaxSlots} = FH,
    Mod:cache_segps(Fd, Head#head.filename, Head#head.next),
    {Bump, Base} = constants(FH, Fname),
    Head1 = Head#head{access = Acc,
		      ram_file = Ram,
		      filename = Fname,
		      name = Tab,
		      cache = dets_utils:new_cache(CacheSz),
		      min_no_slots = MinSlots,
		      max_no_slots = MaxSlots,
		      bump = Bump,
		      base = Base,
		      version = Version,
		      mod = Mod},
    Ftab = case Acc of
	       read_write -> Mod:init_freelist(Head1, ExtraInfo);
	       read -> false
	   end,
    check_growth(Head1),
    NewHead = Head1#head{freelists = Ftab},
    {ok, NewHead}.

%% -> {ok, head()} | throw(Error)
fopen_init_file(Tab, OpenArgs) ->
    #open_args{file = Fname, type = Type, keypos = Kp, 
               min_no_slots = MinSlotsArg, max_no_slots = MaxSlotsArg, 
	       ram_file = Ram, delayed_write = CacheSz, 
	       auto_save = Auto, version = UseVersion} = OpenArgs,
    MinSlots = choose_no_slots(MinSlotsArg, ?DEFAULT_MIN_NO_SLOTS),
    MaxSlots = choose_no_slots(MaxSlotsArg, ?DEFAULT_MAX_NO_SLOTS),
    FileSpec = if
		   Ram == true -> [];
		   true -> Fname
	       end,
    {ok, Fd} = dets_utils:open(FileSpec, open_args(read_write, Ram)),
    Version = if
                  UseVersion == default ->
                      case os:getenv("DETS_USE_FILE_FORMAT") of
                          "8" -> 8;
                          _ -> 9
                      end;
                  true ->
                      UseVersion
              end,
    Mod = version2module(Version),
    case catch Mod:initiate_file(Fd, Tab, Fname, Type, Kp, 
				 MinSlots, MaxSlots, Ram, CacheSz, Auto) of
	{error, Reason} when Ram == true ->
	    file:close(Fd),
	    throw({error, Reason});
	{error, Reason} ->
	    file:close(Fd),
	    file:delete(Fname),
	    throw({error, Reason});
	Reply ->
	    Reply
    end.

open_args(Access, RamFile) ->
    A1 = case Access of
	     read -> [];
	     read_write -> [write]
	 end,
    A2 = case RamFile of
	     true -> [ram];
	     false -> [raw]
	   end,
    A1 ++ A2 ++ [binary, read].

version2module(V) when V =< 8 -> dets_v8;
version2module(9) -> dets_v9.

module2version(dets_v8) -> 8;
module2version(dets_v9) -> 9;
module2version(not_used) -> 9.

%% -> ok | Error
%% Fd is closed.
fsck(Fd, Tab, Fname, FH, MinSlotsArg, MaxSlotsArg, Bump, Version) ->
    %% MinSlots and MaxSlots are the option values.
    #fileheader{type = Type, keypos = Kp, mod = Mod,
		min_no_slots = MinSlotsFile, max_no_slots = MaxSlotsFile} = FH,
    EstNoSlots0 = file_no_things(FH),
    MinSlots = choose_no_slots(MinSlotsArg, MinSlotsFile),
    MaxSlots = choose_no_slots(MaxSlotsArg, MaxSlotsFile),
    EstNoSlots = lists:min([MaxSlots, lists:max([MinSlots, EstNoSlots0])]),
    SlotNumbers = {MinSlots, EstNoSlots, MaxSlots},
    %% When repairing: We first try and sort on slots using MinSlots.
    %% If the number of objects (keys) turns out to be significantly
    %% different from NoSlots, we try again with the correct number of
    %% objects (keys).
    case catch fsck_try(Fd, Tab, Type, Kp, Fname, Bump, 
			SlotNumbers, Mod, Version) of
        {try_again, BetterNoSlots} ->
	    BetterSlotNumbers = {MinSlots, BetterNoSlots, MaxSlots},
            case catch fsck_try(Fd, Tab, Type, Kp, Fname, Bump, 
				BetterSlotNumbers, Mod, Version) of
                {try_again, _} ->
                    file:close(Fd),
                    {error, {cannot_repair, Fname}};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

choose_no_slots(default, NoSlots) -> NoSlots;
choose_no_slots(NoSlots, _) -> NoSlots.

%% -> ok | {try_again, integer()} (if Init is Bump) | throw(Error)
%% Fd is closed unless {try_again, _} is returned.
%% Initiating a table using a fun and repairing (or converting) a
%% file are completely different things, but nevertheless the same
%% method is used in both cases...
%% Mod is the module to use for reading input when repairing.
fsck_try(Fd, Tab, Type, KeyPos, Fname, Init, SlotNumbers, Mod, Version) ->
    Tmp = lists:concat([Fname, ".TMP"]),
    file:delete(Tmp),
    {MinSlots, EstNoSlots, MaxSlots} = SlotNumbers,
    InitSlots = if EstNoSlots == bulk_init -> MinSlots; true -> EstNoSlots end,
    OpenArgs = #open_args{file = Tmp, type = Type, keypos = KeyPos, 
                          repair = false, min_no_slots = InitSlots,
			  max_no_slots = MaxSlots,
                          ram_file = false, delayed_write = ?DEFAULT_CACHE,
                          auto_save = infinity, access = read_write,
                          version = Version},
    case catch fopen(Tab, OpenArgs) of
	{error, Reason} -> 
	    file:close(Fd),
	    throw({error, Reason});
	{ok, Head} ->
            case catch fsck_try_est(Head, Fd, Fname, SlotNumbers, Init, Mod) of
                {ok, NewHead} ->
                    R = case fclose(NewHead) of
                            ok ->
				dets_utils:rename(Tmp, Fname);
                            Error ->
                                Error
                        end,
                    if 
			R == ok -> ok;
			true ->
			    file:delete(Tmp),
			    throw(R)
		    end;
                {try_again, _} = Return ->
                    file:delete(Tmp),
                    Return;
                Error ->
                    file:delete(Tmp),
                    throw(Error)
            end
    end.

%% -> {ok, NewHead} | {try_again, integer()} (if Init is Bump) | throw(Error)
fsck_try_est(Head, Fd, Fname, SlotNumbers, Init, Mod) ->
    Cntrs = ets:new(dets_repair, []),
    IsBulkInit = case SlotNumbers of 
		     {_, bulk_init, _} -> true;
		     _ -> false
		 end,
    Input = case IsBulkInit of
                true -> 
                    Ref = make_ref(),
                    KeyPos = Head#head.keypos,
                    BulkFun = (Head#head.mod):bulk_objects(Head, KeyPos),
                    bulk_input(Head, BulkFun, Init, Cntrs, Ref, 0);
                false ->
                    Bump = Init,
                    {_, Base} = Mod:constants(),
                    State0 = do_fsck(Base, Fd, [], 0),
                    fsck_input(Head, State0, Fd, Bump, Cntrs, Mod)
            end,
    OldV = module2version(Mod),
    Output = (Head#head.mod):fsck_output(OldV, Head, SlotNumbers, Cntrs),
    TmpDir = filename:dirname(Fname),
    Reply = (catch file_sorter:sort(Input, Output, 
				    [{format, fun(X) -> X end},
				     {tmpdir, TmpDir}])),
    L = ets:tab2list(Cntrs),
    ets:delete(Cntrs),
    erlang:garbage_collect(),
    case Reply of 
        {ok, NoDups, H1} ->
            file:close(Fd),
            SizeData = lists:reverse(lists:sort(L)),
            case fsck_copy(SizeData, H1, IsBulkInit, NoDups) of
                {H2, ok} ->
                    {ok, H2#head{update_mode = dirty}};
                {H2, R2} ->
                    throw(R2)
            end;
        {try_again, _} = Return ->
            close_files(L, Head),
            Return;
        Else ->
            file:close(Fd),
            close_files(L, Head),
            throw(Else)
    end.

fsck_copy([SzData | L], Head, Bulk, NoDups) ->
    {LogSz, Pos, FileName, Fd, NoObjects} = SzData,
    close_tmp(Fd),
    Out = Head#head.fptr,
    {ok, Pos} = file:position(Out, Pos),
    CR = file:copy({FileName, [raw,binary]}, Out),
    file:delete(FileName),
    Size = ?POW(LogSz-1),
    ExpectedSize = Size * NoObjects,
    case CR of 
	{ok, Copied} when Copied == ExpectedSize;
			  NoObjects == 0 -> % the segments
	    fsck_copy(L, Head, Bulk, NoDups);
	{ok, Copied} when Bulk == true, Head#head.version == 8 ->
	    NoZeros = ExpectedSize - Copied,
	    Dups = NoZeros div Size,
	    Addr = Pos+Copied,
	    NewHead = free_n_objects(Head, Addr, Size-1, NoDups),
	    NewNoDups = NoDups - Dups,
	    fsck_copy(L, NewHead, Bulk, NewNoDups);
	{ok, _Copied} -> % should never happen
	    close_files(L, Head),
	    Reason = if Bulk == true -> initialization_failed; 
			true -> repair_failed end,
            NR = {error, {Reason, Head#head.filename}},
	    {Head, NR};
	FError ->
	    close_files(L, Head),
	    NR = {error, {file_error, FileName, FError}},
	    {Head, NR}
    end;
fsck_copy([], Head, _Bulk, NoDups) when NoDups =/= 0 ->
    Error = {error, {initialization_failed, Head#head.filename}},
    {Head, Error};
fsck_copy([], Head, _Bulk, _NoDups) ->
    {Head, ok}.

free_n_objects(Head, _Addr, _Size, 0) ->
    Head;
free_n_objects(Head, Addr, Size, N) ->
    NewHead = dets_utils:free(Head, Addr, Size),
    NewAddr = Addr + Size + 1,
    free_n_objects(NewHead, NewAddr, Size, N-1).

close_files(L, Head) ->
    Fun = fun({_Size, _Pos, FileName, Fd, _No}) ->
		  close_tmp(Fd),
		  file:delete(FileName);
	     (_) ->
		  ok
	  end,
    lists:foreach(Fun, L),
    file:close(Head#head.fptr).

close_tmp(Fd) ->
    file:close(Fd).

bulk_input(Head, BulkFun, InitFun, Cntrs, Ref, Seq) ->
    fun(close) ->
	    ok;
       (read) ->
	    case catch {Ref, InitFun(read)} of
		{Ref, end_of_input} ->
		    end_of_input;
		{Ref, {L0, NewInitFun}} when list(L0), function(NewInitFun) ->
		    {L, NSeq} = lists:mapfoldl(BulkFun, Seq, L0),
		    {count_input(Cntrs, L, []), 
		     bulk_input(Head, BulkFun, NewInitFun, Cntrs, Ref, NSeq)};
		{Ref, Value} ->
		    {error, {init_fun, Value}};
		Error ->
		    throw({thrown, Error})
	    end
    end.

%% Fd is not closed.
fsck_input(Head, State, Fd, Bump, Cntrs, Mod) ->
    fun(close) ->
	    ok;
       (read) ->
	    case State of
		done ->
		    end_of_input;
		{done, L, Seq} ->
		    R = count_input(Cntrs, L, []),
		    {R, fsck_input(Head, done, Fd, Bump, Cntrs, Mod)};
		{cont, L, Bin, Pos, Seq} ->
		    R = count_input(Cntrs, L, []),
		    NewState = do_fsck(Bin, Pos, Fd, Head, Mod, Bump, [],Seq),
		    {R, fsck_input(Head, NewState, Fd, Bump, Cntrs, Mod)}
	    end
    end.

%% The ets table Cntrs is used for counting objects per size.
count_input(Cntrs, [[LogSz | B] | Ts], L) ->
    case catch ets:update_counter(Cntrs, LogSz, 1) of
	N when integer(N) -> ok;
	_Badarg -> true = ets:insert(Cntrs, {LogSz, 1})
    end,
    count_input(Cntrs, Ts, [B | L]);
count_input(_Cntrs, [], L) ->
    L.

do_fsck(Pos, F, L, Seq) ->
    case file:position(F, Pos) of
	{ok, _} ->
	    read_more_bytes(<<>>, 0, Pos, F, L, Seq);
	_Error ->
	    {done, L}
    end.

do_fsck(Bin, Pos, F, Head, Mod, _Bump, L, Seq) ->
    case Mod:fsck_objs(Bin, Head#head.keypos, Head, L, Seq) of
        {more, NewBin, Sz, NL, NSeq} ->
            read_more_bytes(NewBin, Sz, Pos, F, NL, NSeq);
        {new, Skip, NL, NSeq} ->
            NewPos = Pos + Skip,
            do_fsck(NewPos, F, NL, NSeq)
    end.

read_more_bytes(B, Min, Pos, F, L, Seq) ->
    Max = if 
	      Min < ?CHUNK_SIZE -> ?CHUNK_SIZE; 
	      true -> Min 
	  end,
    case dets_utils:read_n(F, Max) of
	eof ->
	    {done, L, Seq};
	Bin ->
	    NewPos = Pos + size(Bin),
	    {cont, L, list_to_binary([B, Bin]), NewPos, Seq}
    end.

%% -> {Head, Result}
fselect(Head, MP, Spec, N) ->
    KeyPos = Head#head.keypos,
    case find_all_keys(Spec, KeyPos, []) of
	[] -> 
	    %% Complete match
	    MFun = fun(Term) ->
			   case ets:match_spec_run([Term], MP) of
			       [] ->
				   false;
			       [Else] ->
				   {true, Else}
			   end
		   end,
	    catch chunk_begin(Head, MFun, N);
	List ->
	    Keys = lists:usort(List),
	    {NewHead, Reply} = flookup_keys(Head, Keys),
	    case Reply of
		Objs when list(Objs) ->
		    MatchingObjs = ets:match_spec_run(Objs, MP),
		    {NewHead, {done, MatchingObjs}};
		Error ->
		    {NewHead, {done, Error}}
	    end
    end.

find_all_keys([], _, Ks) ->
    Ks;
find_all_keys([{H,_,_} | T], KeyPos, Ks) when tuple(H) ->
    case size(H) of
	Enough when Enough >= KeyPos ->
	    Key = element(KeyPos, H),
	    case contains_variable(Key) of
		true -> 
		    [];
		false ->
		    find_all_keys(T, KeyPos, [Key | Ks])
	    end;
	_ ->
	    find_all_keys(T, KeyPos, Ks)
    end;
find_all_keys(_, _, _) ->
    [].

%% {Head, Cont} | throw({Head, Error})
chunk_begin(Head, MFun, N) ->
    Fun = 
	fun(O, Ack) ->
		case MFun(O) of
		    {true, ObjOrBs} ->
			[ObjOrBs | Ack];
		    false ->
			Ack
		end
	end,
    {NewHead, []} = write_cache(Head),
    C0 = init_scan(NewHead, N),
    {NewHead, {cont, C0#dets_cont{arg = Fun}}}.

fslot(H, Slot) ->
    catch begin
      {NH, []} = write_cache(H),
      Objs = (H#head.mod):slot_objs(H, Slot),
      {NH, Objs}
    end.

do_update_counter(Head, _Key, _Incr) when Head#head.type =/= set ->
    {Head, badarg};
do_update_counter(Head, Key, Incr) ->
    case flookup_keys(Head, [Key]) of
	{H1, [O]} ->
	    Kp = H1#head.keypos,
	    case catch try_update_tuple(O, Kp, Incr) of
		{'EXIT', _} ->
		    {H1, badarg};
		{New, Term} ->
		    case finsert(H1, [Term]) of
			{H2, ok} -> 
			    {H2, New};
			Reply -> 
			    Reply
		    end
	    end;
	{H1, []} ->
	    {H1, badarg};
	HeadError ->
	    HeadError
    end.
    
try_update_tuple(O, _Kp, {Pos, Incr}) ->
    try_update_tuple2(O, Pos, Incr);
try_update_tuple(O, Kp, Incr) ->
    try_update_tuple2(O, Kp+1, Incr).

try_update_tuple2(O, Pos, Incr) ->
    New = element(Pos, O) + Incr,
    {New, setelement(Pos, O, New)}.

set_verbose(true) ->
    put(verbose, yes);
set_verbose(_) ->
    erase(verbose).

where_is_object(Head, Object) ->
    Keypos = Head#head.keypos,
    case check_objects([Object], Keypos) of
	true ->
	    case catch write_cache(Head) of
		{NewHead, []} ->
		    {NewHead, (Head#head.mod):find_object(NewHead, Object)};
		HeadError ->
		    HeadError
	    end;
	false ->
	    {Head, badarg}
    end.

check_objects([T | Ts], Kp) when tuple(T), size(T) >= Kp ->
    check_objects(Ts, Kp);
check_objects(L, _Kp) ->
    L == [].

no_things(Head) when Head#head.no_keys == undefined ->
    Head#head.no_objects;
no_things(Head) ->
    Head#head.no_keys.

file_no_things(FH) when FH#fileheader.no_keys == undefined ->
    FH#fileheader.no_objects;
file_no_things(FH) ->
    FH#fileheader.no_keys.

%% -> {Head, [LookedUpObject]} | throw({Head, Error})
%% Lookup of keys bypassing the cache.
direct_lookup(Head, Ks) ->
    (Head#head.mod):direct_lookup(Head, Ks).

%% -> {Head, [LookedUpObject]} | throw({Head, Error})
update_cache(Head, KeysOrObjects, What) ->
    Cache = Head#head.cache,
    F = if 
	    What == delete_key; What == lookup ->
		fun(K, Seq) -> {K, {Seq, What}} end;
	    What == delete_object; What == insert ->
		Kp2 = Head#head.keypos,
		fun(O, Seq) -> {element(Kp2, O), {Seq, {What, O}}} end
	end,
    #cache{cache = C, csize = Size0, wrtime = WrTime, 
	   tsize = Size, delay = Delay} = Cache,
    %% The size is used as a sequence number here; it increases monotonically.
    {NewC, NewSize} = cache_binary(KeysOrObjects, F, C, Size0),
    NewCache = if
		   What == insert ->
		       NewIns = Cache#cache.inserts + length(KeysOrObjects),
		       Cache#cache{cache = NewC, csize = NewSize,
				   inserts = NewIns};
		   true ->
		       Cache#cache{cache = NewC, csize = NewSize}
	       end,
    Head1 = Head#head{cache = NewCache},
    if 
	NewSize >= Size; What == lookup ->
	    %% The cache is considered full, or lookup.
	    write_cache(Head1);
	WrTime == undefined ->
	    %% Empty cache. Schedule a delayed write.
	    Now = now(), Me = self(),
	    erlang:send_after(Delay, Me, {Me, {delayed_write, Now}}),
	    {Head1#head{cache = NewCache#cache{wrtime = Now}}, []};
	Size0 == 0 ->
	    %% Empty cache that has been written after the currently
	    %% scheduled delayed write.
	    {Head1#head{cache = NewCache#cache{wrtime = now()}}, []};
	true ->
	    %% Cache is not empty, delayed write has been scheduled.
	    {Head1, []}
    end.

cache_binary([KO | KOs], F, C, Sz) ->
    I = F(KO, Sz),
    NSz = Sz + erlang:external_size(I),
    cache_binary(KOs, F, [I | C], NSz);
cache_binary([], _F, C, Sz) ->
    {C, Sz}.

%% Called after some delay.
%% -> NewHead
delayed_write(Head, WrTime) ->
    Cache = Head#head.cache,
    LastWrTime = Cache#cache.wrtime,
    if
	LastWrTime == WrTime ->
	    %% The cache has not been emptied during the last delay.
	    case catch write_cache(Head) of
		{Head2, []} ->
		    NewCache = (Head2#head.cache)#cache{wrtime = undefined},
		    Head2#head{cache = NewCache};
		{NewHead, _Error} -> % Head.update_mode has been updated
		    NewHead
	    end;
	true ->
	    %% The cache was emptied during the delay.
	    %% Has anything been written since then?
	    if 
		Cache#cache.csize == 0 ->
		    %% No, further delayed write not needed.
		    NewCache = Cache#cache{wrtime = undefined},
		    Head#head{cache = NewCache};
		true ->
		    %% Yes, schedule a new delayed write.
		    {MS1,S1,M1} = WrTime,
		    {MS2,S2,M2} = LastWrTime,
		    WrT = M1+1000000*(S1+1000000*MS1),
		    LastWrT = M2+1000000*(S2+1000000*MS2),
		    When = round((LastWrT - WrT)/1000), Me = self(),
		    erlang:send_after(When, Me, 
				      {Me, {delayed_write, LastWrTime}}),
		    Head
	    end
    end.

%% -> {NewHead, [LookedUpObject]} | throw({NewHead, Error})
write_cache(Head) when Head#head.update_mode == dirty ->
    (Head#head.mod):write_cache(Head);
write_cache(Head) ->
    {Head, []}.

%% Implement the ets match algorithm in erlang itself
%% return false | {true, Bindings}
%% Max 10 variables :-(

do_match(Obj, Pat) ->
    case do_match(Obj, Pat, {no,no,no,no,no,no,no,no,no,no}) of
	false -> false;
	{bs, Bs} -> {true, Bs}
    end.

add_binding(Pos, Bs, Obj) ->
    case catch setelement(Pos, Bs, Obj) of
	{'EXIT', _} ->
	    Bs2 = grow_tuple(Bs, size(Bs)),
	    add_binding(Pos, Bs2, Obj);
	Other -> 
	    Other
    end.

grow_tuple(Tup, Sz) ->
    L = tuple_to_list(Tup),
    L2 = lists:duplicate(Sz + 10, no),
    list_to_tuple(L ++ L2).

binding(Pos, Tup) ->
    case catch element(Pos, Tup) of
	{'EXIT', _} ->
	    Bs2 = grow_tuple(Tup, size(Tup)),
	    {no, Bs2};
	Other -> Other
    end.

do_match(X, X, Bs) -> 
    {bs, Bs};
do_match([H1 | T1], [H2 | T2], Bs) -> 
    case do_match(H1, H2, Bs) of
	{bs, Bs2} -> do_match(T1, T2, Bs2);
	false -> false
    end;
do_match(Tup1, Tup2, Bs) when tuple(Tup1),tuple(Tup2), 
                              size(Tup1) == size(Tup2) ->
    e_match(Tup1, Tup2, size(Tup1), Bs);
do_match(_Obj, '_', Bs) -> {bs, Bs}; % optimization
do_match(Obj, Pat, Bs) -> 
    case is_var(Pat) of
	{true, Pos} when integer(Pos) ->
	    case binding(Pos + 1, Bs) of
		no ->
		    {bs, add_binding(Pos + 1, Bs, Obj)};
		{no, Bs2} ->
		    {bs, add_binding(Pos + 1, Bs2, Obj)};
		Obj ->
		    {bs, Bs};
		_ ->
		    false
	    end;
	{true, wild} ->
	    {bs, Bs};
	false ->
	    false
    end.

e_match(_, _, 0, Bs) -> {bs, Bs};
e_match(T1, T2, Pos, Bs) ->
    case do_match(element(Pos, T1), element(Pos, T2), Bs) of
	{bs, Bs2} -> e_match(T1, T2, Pos-1, Bs2);
	false -> false
    end.

is_var(X) when atom(X) ->
    case atom_to_list(X) of
	[$$, Dig] when $0 =< Dig, Dig =< $9 -> {true, Dig - $0};
	[$_] -> {true, wild};
	[$$, Dig | Tail] -> accumulate_digs(lists:reverse([Dig |Tail]), 0, 1);
	_ -> false
    end;
is_var(_) -> false.

accumulate_digs([], Ack, _) -> {true, Ack};
accumulate_digs([Dig|T], Ack, Pow) when $0 =< Dig, Dig =< $9 ->
    accumulate_digs(T, Ack + (Dig - $0) * Pow, Pow * 10);
accumulate_digs(_,_,_) -> false.

contains_variable('_') ->
    true;
contains_variable(A) when atom(A) ->
    case atom_to_list(A) of
	[$$ | T] ->
	    case (catch list_to_integer(T)) of
		{'EXIT', _} ->
		    false;
		_ ->
		    true
	    end;
	_ ->
	    false
    end;
contains_variable(T) when tuple(T) ->
    contains_variable(tuple_to_list(T));
contains_variable([]) ->
    false;
contains_variable([H|T]) ->
    case contains_variable(H) of
	true ->
	    true;
	false ->
	    contains_variable(T)
    end;
contains_variable(_) ->
    false.

%%% Scan the file from start to end by reading chunks.

%% -> dets_cont()
init_scan(Head, NoObjs) ->
    L = dets_utils:all_allocated(Head),
    Pos = case L of
	      <<>> -> 
		  eof;
	      {From, _To, _L} ->
		  From
	  end,
    #dets_cont{no_objs = NoObjs, pos = Pos, bin = <<>>, alloc = L}.

%% -> {[RTerm], dets_cont()} | {scan_error, Reason}
%% RTerm = {Pos, Next, Size, Status, Term}
scan(_Head, _Repr, C) when size(C#dets_cont.alloc) == 0 ->
    {[], C};
scan(Head, Repr, C) -> % when record(C, dets_cont)
    #dets_cont{no_objs = No, pos = Pos, alloc = L0, bin = Bin} = C,
    {From, To, L} = L0,
    R = case No of
	    default ->
		0;
	    _ when integer(No) ->
		-No-1
	end,
    scan(Bin, Head, From, To, Pos, L, [], R, {Repr, C, Head#head.type}).

scan(Bin, H, From, To, Pos, L, Ts, R, {_Repr, C0, _Type} = C) ->
    case (H#head.mod):scan_objs(Bin, From, To, L, Ts, R, C) of
        {more, B, NFrom, NTo, NL, NTs, NR, Sz} ->
            scan_read(B, H, NFrom, NTo, Pos, Sz, NL, NTs, NR, C);
        {read, SkipPos, NTo, NL, NTs, NR} ->
	    scan_read(<<>>, H, SkipPos, NTo, SkipPos, 0, NL, NTs, NR, C);
        {stop, B, NFrom, NTo, NL, NTs} ->
            A = {NFrom, NTo, NL},
            {NTs, C0#dets_cont{pos = Pos, bin = B, alloc = A}};
        {finished, NTs, E} ->
            {NTs, C0#dets_cont{pos = eof, bin = E, alloc = E}};
        bad_object ->
            {scan_error, bad_object}
    end.

scan_read(Bin, _H, From, To, Pos, _Min, L0, Ts, 
	  R, {_Repr, C, _Type}) when R >= ?CHUNK_SIZE ->
    %% We may have read (much) more than CHUNK_SIZE, if there are holes.
    %% If Bin is non-empty, it contains the first part of _one_ term.
    L = {From, To, L0},
    {Ts, C#dets_cont{pos = Pos, bin = Bin, alloc = L}};
scan_read(B, H, From, To, Pos, Min, L, Ts, R, C) ->
    Max = if 
	      Min < ?CHUNK_SIZE -> ?CHUNK_SIZE; 
	      true -> Min 
	  end,
    case dets_utils:pread_n(H#head.fptr, Pos, Max) of
	eof ->
	    {scan_error, premature_eof};
	Bin ->
	    NewPos = Pos + size(Bin),
	    NewBin = if 
			 size(B) == 0 -> Bin;
			 true -> list_to_binary([B, Bin])
		     end,
	    scan(NewBin, H, From, To, NewPos, L, Ts, R, C)
    end.

err(Error) ->
    case get(verbose) of
	yes -> 
	    error_logger:format("** dets: failed with ~w~n", [Error]),
	    Error;
	undefined  ->
	    Error
    end.

%%%%%%%%%%%%%%%%%  DEBUG functions %%%%%%%%%%%%%%%%

file_info(FileName) ->
    case catch read_file_header(FileName, read, false) of
	{ok, Fd, FH} ->
	    file:close(Fd),
            (FH#fileheader.mod):file_info(FH);
	Other ->
	    Other
    end.

get_head_field(Fd, Field) ->
    dets_utils:read_4(Fd, Field).

%% Dump the contents of a DAT file to the tty
%% internal debug function which ignores the closed properly thingie
%% and just tries anyway

view(FileName) ->
    case catch read_file_header(FileName, read, false) of
        {ok, Fd, FH} ->
	    Mod = FH#fileheader.mod,
	    case Mod:check_file_header(FH, Fd) of
		{ok, H0, ExtraInfo} ->
		    Ftab = Mod:init_freelist(H0, ExtraInfo),
		    {_Bump, Base} = constants(FH, FileName),
		    H = H0#head{freelists=Ftab, base = Base},
		    v_free_list(H),
		    Mod:v_segments(H),
		    file:close(Fd);
		X ->
		    file:close(Fd),
		    X
	    end;
	X -> 
	    X
    end.

v_free_list(Head) ->
    io:format("FREE LIST ...... \n",[]),
    io:format("~p~n", [dets_utils:all_free(Head)]),
    io:format("END OF FREE LIST \n",[]).
