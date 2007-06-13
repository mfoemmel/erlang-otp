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
-module(ets).

%% Interface to the Term store BIF's
%% ets == Erlang Term Store

-export([delete/1,
	 file2tab/1,
	 filter/3,
	 foldl/3, foldr/3,
	 info/1,
	 info/2,
	 match_delete/2,
	 tab2file/2,
	 from_dets/2,
	 to_dets/2,
	 init_table/2,
	 test_ms/2,
	 tab2list/1,
         table/1,
         table/2,
	 fun2ms/1,
	 match_spec_run/2,
	 repair_continuation/2]).

-export([i/0, i/1, i/2, i/3]).

-deprecated([{fixtable,2}]).

%% The following functions used to be found in this module, but
%% are now BIFs (i.e. implemented in C).
%%
%% all/0
%% new/2
%% delete/2
%% first/1
%% fixtable/2
%% lookup/2
%% lookup_element/3
%% insert/2
%% is_compiled_ms/1
%% last/1
%% next/2
%% prev/2
%% rename/2
%% slot/2
%% match/1
%% match/2
%% match/3
%% match_object/1
%% match_object/2
%% match_object/3
%% match_spec_compile/1
%% match_spec_run_r/3
%% select/1
%% select/2
%% select/3
%% select_reverse/1
%% select_reverse/2
%% select_reverse/3
%% select_delete/2
%% update_counter/3
%%


match_spec_run(List,CompiledMS) ->
    lists:reverse(ets:match_spec_run_r(List,CompiledMS,[])).

% $end_of_table is an allowed continuation in ets...
repair_continuation('$end_of_table',_) ->
    '$end_of_table';
% ordered_set
repair_continuation(Untouched = {Table,Lastkey,L1,N2,Bin,L2,N3,N4}, MS) when
(is_atom(Table) or is_integer(Table)),
is_list(L1),
is_integer(N2),
is_binary(Bin),
size(Bin) =:= 0,
is_list(L2),
is_integer(N3),
is_integer(N4) ->
    case ets:is_compiled_ms(Bin) of
	true ->
	    Untouched;
	false ->
	    {Table,Lastkey,L1,N2,ets:match_spec_compile(MS),L2,N3,N4}
    end;

% set/bag/duplicate_bag
repair_continuation(Untouched = {Table,N1,N2,Bin,L,N3}, MS) when
(is_atom(Table) or is_integer(Table)),
is_integer(N1),
is_integer(N2),
is_binary(Bin),
size(Bin) =:= 0,
is_list(L),
is_integer(N3) ->
    case ets:is_compiled_ms(Bin) of
	true ->
	    Untouched;
	false ->
	    {Table,N1,N2,ets:match_spec_compile(MS),L,N3}
    end.

fun2ms(ShellFun) when is_function(ShellFun) ->
    % Check that this is really a shell fun...
    case erl_eval:fun_data(ShellFun) of
        {fun_data,ImportList,Clauses} ->
            case ms_transform:transform_from_shell(
                   ?MODULE,Clauses,ImportList) of
                {error,[{_,[{_,_,Code}|_]}|_],_} ->
                    io:format("Error: ~s~n",
                              [ms_transform:format_error(Code)]),
                    {error,transform_error};
                Else ->
                    Else
            end;
        false ->
            exit({badarg,{?MODULE,fun2ms,
                          [function,called,with,real,'fun',
                           should,be,transformed,with,
                           parse_transform,'or',called,with,
                           a,'fun',generated,in,the,
                           shell]}})
    end.


foldl(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    First = ets:first(T),
    try
        do_foldl(F, Accu, First, T)
    after
        ets:safe_fixtable(T, false)
    end.

do_foldl(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldl(F,
		     lists:foldl(F, Accu0, ets:lookup(T, Key)),
		     ets:next(T, Key), T)
    end.

foldr(F, Accu, T) ->
    ets:safe_fixtable(T, true),
    Last = ets:last(T),
    try
        do_foldr(F, Accu, Last, T)
    after 
        ets:safe_fixtable(T, false)
    end.

do_foldr(F, Accu0, Key, T) ->
    case Key of
	'$end_of_table' ->
	    Accu0;
	_ ->
	    do_foldr(F,
		     lists:foldr(F, Accu0, ets:lookup(T, Key)),
		     ets:prev(T, Key), T)
    end.

from_dets(EtsTable, DetsTable) ->
    case (catch dets:to_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:fault(Reason,[EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:fault(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:fault(EReason,[EtsTable,DetsTable]);
	EtsTable ->
	    true;
	Unexpected -> %% Dets bug?
	    erlang:fault(Unexpected,[EtsTable,DetsTable])
    end.

to_dets(EtsTable, DetsTable) ->
    case (catch dets:from_ets(DetsTable, EtsTable)) of
	{error, Reason} ->
	    erlang:fault(Reason,[EtsTable,DetsTable]);
	{'EXIT', {Reason1, _Stack1}} ->
	    erlang:fault(Reason1,[EtsTable,DetsTable]);
	{'EXIT', EReason} ->
	    erlang:fault(EReason,[EtsTable,DetsTable]);
	ok ->
	    DetsTable;
	Unexpected -> %% Dets bug?
	    erlang:fault(Unexpected,[EtsTable,DetsTable])
    end.

test_ms(Term,MS) ->
    case erlang:match_spec_test(Term,MS,table) of
	{ok, Result, _Flags, _Messages} ->
	    {ok, Result}; 
	{error, Errors} ->
	    {error, Errors}
    end.

init_table(Table, Fun) ->
    ets:delete_all_objects(Table),
    init_table_continue(Table, Fun(read)).

init_table_continue(_Table, end_of_input) ->
    true;
init_table_continue(Table, {List,Fun}) when is_list(List), is_function(Fun) ->
    case (catch init_table_sub(Table, List)) of
	{'EXIT', Reason} ->
	    (catch Fun(close)),
	    exit(Reason);
	true ->
	    init_table_continue(Table,Fun(read))
    end;
init_table_continue(_Table, Error) ->
    exit(Error).

init_table_sub(_Table,[]) ->
    true;
init_table_sub(Table, [H|T]) ->
    ets:insert(Table,H),
    init_table_sub(Table,T).

match_delete(Table, Pattern) ->
    ets:select_delete(Table,[{Pattern,[],[true]}]),
    true.

delete(T) when is_atom(T) ; is_integer(T) ->
    ets:db_delete(T).

info(T) when is_atom(T) ; is_integer(T) ->
    local_info(T, node()).

local_info(T, Node) ->
    case catch ets:db_info(T, memory) of
	undefined -> undefined;
	{'EXIT', _} -> undefined;
	Mem ->
	    [{memory, Mem}, {owner, info(T, owner)}, 
	     {name,info(T, name)},
	     {size, info(T, size)}, {node, Node},
	     {named_table, info(T, named_table)},
	     {type, info(T, type)}, 
	     {keypos, info(T, keypos)},
	     {protection, info(T, protection)}]
    end.

info(T, What) when is_atom(T) ; is_integer(T) ->
    local_info(T, What, node()).

local_info(T, What, Node) ->
    case What of 
	node ->
	    %% Use a bif call to determine if the table exists
	    case (catch ets:db_info(T, type)) of
		undefined ->
		    undefined;
		{'EXIT',_} ->
		    undefined;
		_ ->
		    Node
	    end;
	named_table ->
	    %% Use a bif call to determine if the table exists
	    case (catch ets:db_info(T, type)) of
		undefined ->
		    undefined;
		{'EXIT',_} ->
		    undefined;
		_ ->
		    if
			is_atom(T) -> 
			    true;
			true -> 
			    false
		    end
	    end;
	_ ->
	    case (catch ets:db_info(T, What)) of
	        undefined -> 
		    undefined;
		{'EXIT',_} -> 
		    undefined;
		Result -> 
		    Result
	    end
    end.

%% Produce a list of {Key,Value} tuples from a table

tab2list(T) ->
    ets:match_object(T, '_').

filter(Tn, F, A) when is_atom(Tn) ; is_integer(Tn) ->
    do_filter(Tn,ets:first(Tn),F,A, []).

do_filter(_Tab, '$end_of_table', _,_, Ack) -> 
    Ack;
do_filter(Tab, Key, F, A, Ack) ->
    case apply(F, [ets:lookup(Tab, Key) | A]) of
	false ->
	    do_filter(Tab, ets:next(Tab, Key), F,A,Ack);
	true ->
            Ack2 = ets:lookup(Tab, Key) ++ Ack,
	    do_filter(Tab, ets:next(Tab, Key), F,A,Ack2);
	{true, Value} ->
	    do_filter(Tab, ets:next(Tab, Key), F,A,[Value | Ack])
    end.

    
%% Dump a table to a file using the disk_log facility
tab2file(Tab, File) ->
    file:delete(File),
    Name = make_ref(),
    case {disk_log:open([{name, Name}, {file, File}]),
	  local_info(Tab, node())} of
	{{ok, Name}, undefined} ->
	    disk_log:close(Name),
	    {error, badtab};
	{_, undefined} ->
	    {error, badtab};
	{{ok, Name}, Info0} ->
	    %% For backwards compatibility, the table parameters should be a tuple.
	    Info = list_to_tuple(Info0),
	    ok = disk_log:log(Name, Info),
	    tab2file(Tab, ets:first(Tab), Name)
    end.
tab2file(Tab, K, Name) ->
    case get_objs(Tab, K, 10, []) of
	{'$end_of_table', Objs} ->
	    disk_log:log_terms(Name, Objs),
	    disk_log:close(Name);
	{Next, Objs} ->
	    disk_log:log_terms(Name, Objs),
	    tab2file(Tab, Next, Name)
    end.

get_objs(_Tab, K, 0, Ack) ->
    {K, lists:reverse(Ack)};
get_objs(_Tab, '$end_of_table', _, Ack) ->
    {'$end_of_table', lists:reverse(Ack)};
get_objs(Tab, K, I, Ack) ->
    Os = ets:lookup(Tab, K),
    get_objs(Tab, ets:next(Tab, K), I-1, Os ++ Ack).

%% Restore a table from a file, given that the file was written with
%% the tab2file/2 function from above

file2tab(File) ->
    Name  = make_ref(),
    case disk_log:open([{name, Name}, {file, File}, {mode, read_only}]) of
	{ok, Name} ->
	    init_file2tab(Name);
	{repaired, Name, _,_} ->
	    init_file2tab(Name);
	_Other ->
	    {error, badfile}
    end.

init_file2tab(Name) ->
    case disk_log:chunk(Name, start) of
	{error, Reason} ->
	    file2tab_error(Name, Reason);
	eof ->
	    file2tab_error(Name, eof);
	{Cont, [Info | Tail]} ->
	    case catch mk_tab(tuple_to_list(Info)) of
		{'EXIT', _} ->
		    file2tab_error(Name, "Can't create table");
		Tab ->
		    fill_tab(Cont, Name, Tab, Tail),
		    disk_log:close(Name),
		    {ok, Tab}
	    end
    end.

fill_tab(C, Name, Tab, [H|T]) ->
    ets:insert(Tab, H),
    fill_tab(C, Name, Tab, T);
fill_tab(C, Name, Tab, []) ->
    case disk_log:chunk(Name, C) of
	{error, Reason} ->
	    ets:db_delete(Tab),
	    file2tab_error(Name, Reason);
	eof ->
	    ok;
	{C2, Objs} ->
	    fill_tab(C2, Name, Tab, Objs)
    end.

file2tab_error(Name, Reason) ->
    disk_log:close(Name),
    {error, Reason}.

mk_tab(I) ->
    {value, {name, Name}} = lists:keysearch(name, 1, I),
    {value, {type, Type}} = lists:keysearch(type, 1, I),
    {value, {protection, P}} = lists:keysearch(protection, 1, I),
    {value, {named_table, Val}} = lists:keysearch(named_table, 1, I),
    {value, {keypos, Kp}} = lists:keysearch(keypos, 1, I),
    ets:new(Name, [Type, P, {keypos, Kp} | named_table(Val)]).

named_table(true) -> [named_table];
named_table(false) -> [].

table(Tab) ->
    table(Tab, []).

table(Tab, Opts) ->
    case options(Opts, [traverse, n_objects]) of
        {badarg,_} ->
            erlang:fault(badarg, [Tab, Opts]);
        [[Traverse, NObjs], QlcOptions] ->
            TF = case Traverse of
                     first_next -> 
                         fun() -> qlc_next(Tab, ets:first(Tab)) end;
                     last_prev -> 
                         fun() -> qlc_prev(Tab, ets:last(Tab)) end;
                     select -> 
                         fun(MS) -> qlc_select(ets:select(Tab, MS, NObjs)) end;
                     {select, MS} ->
                         fun() -> qlc_select(ets:select(Tab, MS, NObjs)) end
                 end,
            PreFun = fun(_) -> ets:safe_fixtable(Tab, true) end,
            PostFun = fun() -> ets:safe_fixtable(Tab, false) end,
            InfoFun = fun(Tag) -> table_info(Tab, Tag) end,
            LookupFun = 
                case Traverse of 
                    {select, _MS} ->
                        undefined;
                    _ -> 
                        fun(_Pos, [K]) ->
                                ets:lookup(Tab, K);
                           (_Pos, Ks) -> 
                                lists:flatmap(fun(K) -> ets:lookup(Tab, K) 
                                              end, Ks) 
                        end
                end,
            FormatFun = 
                fun(all) ->
                        As = case Opts of
                                 [] -> [Tab];
                                 _ -> [Tab, Opts]
                             end,
                        {?MODULE, table, As};
                   ({match_spec, MS}) ->
                        {?MODULE, table, 
                         [Tab, [{traverse, {select, MS}} | 
                                listify(Opts)]]};
                   ({lookup, _KeyPos, [Value]}) ->
                        io_lib:format("~w:lookup(~w, ~w)", 
                                      [?MODULE, Tab, Value]);
                   ({lookup, _KeyPos, Values}) ->
                        io_lib:format("lists:flatmap(fun(V) -> "
                                      "~w:lookup(~w, V) end, ~w)", 
                                      [?MODULE, Tab, Values])
                end,
            qlc:table(TF, [{pre_fun, PreFun}, {post_fun, PostFun}, 
                           {info_fun, InfoFun}, {format_fun, FormatFun},
                           {lookup_fun, LookupFun}] ++ QlcOptions)
    end.
         
table_info(Tab, num_of_objects) ->
    info(Tab, size);
table_info(Tab, keypos) ->
    info(Tab, keypos);
table_info(Tab, is_unique_objects) ->
    info(Tab, type) =/= duplicate_bag;
table_info(Tab, is_sorted_key) ->
    info(Tab, type) =:= ordered_set;
table_info(_Tab, _) ->
    undefined.

qlc_next(_Tab, '$end_of_table') ->
    [];
qlc_next(Tab, Key) ->
    ets:lookup(Tab, Key) ++ fun() -> qlc_next(Tab, ets:next(Tab, Key)) end.

qlc_prev(_Tab, '$end_of_table') ->
    [];
qlc_prev(Tab, Key) ->
    ets:lookup(Tab, Key) ++ fun() -> qlc_prev(Tab, ets:prev(Tab, Key)) end.

qlc_select('$end_of_table') -> 
    [];
qlc_select({Objects, Cont}) -> 
    Objects ++ fun() -> qlc_select(ets:select(Cont)) end.

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options, [Key | Keys], L) when is_list(Options) ->
    V = case lists:keysearch(Key, 1, Options) of
            {value, {n_objects, default}} ->
                {ok, default_option(Key)};
            {value, {n_objects, NObjs}} when is_integer(NObjs),
                                             NObjs >= 1 ->
                {ok, NObjs};
            {value, {traverse, select}} ->
                {ok, select};
            {value, {traverse, {select, MS}}} ->
                {ok, {select, MS}};
            {value, {traverse, first_next}} ->
                {ok, first_next};
            {value, {traverse, last_prev}} ->
                {ok, last_prev};
	    {value, {Key, _}} ->
		badarg;
	    false ->
		Default = default_option(Key),
		{ok, Default}
	end,
    case V of
	badarg ->
	    {badarg, Key};
	{ok,Value} ->
	    NewOptions = lists:keydelete(Key, 1, Options),
	    options(NewOptions, Keys, [Value | L])
    end;
options(Options, [], L) ->
    [lists:reverse(L), Options].

default_option(traverse) -> select;
default_option(n_objects) -> 100.

listify(L) when is_list(L) ->
    L;
listify(T) ->
    [T].

%% End of table/2.

%% Print info about all tabs on the tty
i() ->
    hform('id', 'name', 'type', 'size', 'mem', 'owner'),
    io:format(" -------------------------------------"
	      "---------------------------------------\n"),
    lists:foreach(fun prinfo/1, tabs()),
    ok.

tabs() ->
    lists:sort(ets:all()).

prinfo(Tab) ->
    case catch prinfo2(Tab) of
	{'EXIT', _} ->
	    io:format("~-10s ... unreadable \n", [to_string(Tab)]);
	ok -> 
	    ok
    end.
prinfo2(Tab) ->
    Name = ets:info(Tab, name),
    Type = ets:info(Tab, type),
    Size = ets:info(Tab, size),
    Mem = ets:info(Tab, memory),
    Owner = ets:info(Tab, owner),
    hform(Tab, Name, Type, Size, Mem, is_reg(Owner)).

is_reg(Owner) ->
    case process_info(Owner, registered_name) of
	{registered_name, Name} -> Name;
	_ -> Owner
    end.

%%% Arndt: this code used to truncate over-sized fields. Now it
%%% pushes the remaining entries to the right instead, rather than
%%% losing information.
hform(A0, B0, C0, D0, E0, F0) ->
    [A,B,C,D,E,F] = [to_string(T) || T <- [A0,B0,C0,D0,E0,F0]],
    A1 = pad_right(A, 15),
    B1 = pad_right(B, 17),
    C1 = pad_right(C, 5),
    D1 = pad_right(D, 6),
    E1 = pad_right(E, 8),
    %% no need to pad the last entry on the line
    io:format(" ~s ~s ~s ~s ~s ~s\n", [A1,B1,C1,D1,E1,F]).

pad_right(String, Len) ->
    if
	length(String) >= Len ->
	    String;
	true ->
	    [Space] = " ",
	    String ++ lists:duplicate(Len - length(String), Space)
    end.

to_string(X) ->
    lists:flatten(io_lib:format("~p", [X])).

%% view a specific table 
i(Tab) ->
    i(Tab, 40).
i(Tab, Height) ->
    i(Tab, Height, 80).
i(Tab, Height, Width) ->
    First = ets:first(Tab),
    display_items(Height, Width, Tab, First, 1, 1).

display_items(Height, Width, Tab, '$end_of_table', Turn, Opos) -> 
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, eot, Tab, '$end_of_table', Turn, Opos);
display_items(Height, Width, Tab, Key, Turn, Opos) when Turn < Height ->
    do_display(Height, Width, Tab, Key, Turn, Opos);
display_items(Height, Width, Tab, Key, Turn, Opos) when Turn >=  Height ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, normal, Tab, Key, Turn, Opos).

choice(Height, Width, P, Mode, Tab, Key, Turn, Opos) ->
    case get_line(P, "c\n") of
	"c\n" when Mode =:= normal ->
	    do_display(Height, Width, Tab, Key, 1, Opos);
	"c\n" when is_tuple(Mode), element(1, Mode) =:= re ->
	    {re, Re} = Mode,
	    re_search(Height, Width, Tab, Key, Re, 1, Opos);
	"q\n" ->
	    quit;
	"k\n" ->
	    ets:delete(Tab);
	[$p|Digs]  ->
	    catch case catch list_to_integer(nonl(Digs)) of
		      {'EXIT', _} ->
			  io:format("Bad digits \n", []);
		      Number when Mode =:= normal ->
			  print_number(Tab, ets:first(Tab), Number);
		      Number when Mode =:= eot ->
			  print_number(Tab, ets:first(Tab), Number);
		      Number -> %% regexp
			  {re, Re} = Mode,
			  print_re_num(Tab, ets:first(Tab), Number, Re)
		  end,
	    choice(Height, Width, P, Mode, Tab, Key, Turn, Opos);
	[$/|Regexp]   -> %% from regexp
	    re_search(Height, Width, Tab, ets:first(Tab), nonl(Regexp), 1, 1);
	_  ->
	    choice(Height, Width, P, Mode, Tab, Key, Turn, Opos)
    end.

get_line(P, Default) ->
    case io:get_line(P) of
	"\n" ->
	    Default;
	L ->
	    L
    end.

nonl(S) -> string:strip(S, right, $\n).

print_number(Tab, Key, Num) ->
    Os = ets:lookup(Tab, Key),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_number(Tab, ets:next(Tab, Key), Num - Len)
    end.

do_display(Height, Width, Tab, Key, Turn, Opos) ->
    Objs = ets:lookup(Tab, Key),
    do_display_items(Height, Width, Objs, Opos),
    Len = length(Objs),
    display_items(Height, Width, Tab, ets:next(Tab, Key), Turn+Len, Opos+Len).

do_display_items(Height, Width, [Obj|Tail], Opos) ->
    do_display_item(Height, Width, Obj, Opos),
    do_display_items(Height, Width, Tail, Opos+1);
do_display_items(_Height, _Width, [], Opos) ->
    Opos.

do_display_item(_Height, Width, I, Opos)  ->
    L = to_string(I),
    L2 = if
	     length(L) > Width - 8 ->
                 string:substr(L, 1, Width-13) ++ "  ...";
	     true ->
		 L
	 end,
    io:format("<~-4w> ~s~n", [Opos,L2]).

re_search(Height, Width, Tab, '$end_of_table', Re, Turn, Opos) ->
    P = 'EOT  (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Tab, '$end_of_table', Turn, Opos);

re_search(Height, Width, Tab, Key, Re, Turn, Opos) when Turn < Height ->
    re_display(Height, Width, Tab, Key, ets:lookup(Tab, Key), Re, Turn, Opos);

re_search(Height, Width, Tab, Key, Re, Turn, Opos)  ->
    P = '(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->',
    choice(Height, Width, P, {re, Re}, Tab, Key, Turn, Opos).

re_display(Height, Width, Tab, Key, [], Re, Turn, Opos) ->
    re_search(Height, Width, Tab, ets:next(Tab, Key), Re, Turn, Opos);
re_display(Height, Width, Tab, Key, [H|T], Re, Turn, Opos) ->
    Str = to_string(H),
    case regexp:match(Str, Re) of
	{match,_,_} ->
	    do_display_item(Height, Width, H, Opos),
	    re_display(Height, Width, Tab, Key, T, Re, Turn+1, Opos+1);
	_ ->
	    re_display(Height, Width, Tab, Key, T, Re, Turn, Opos)
    end.

print_re_num(_,'$end_of_table',_,_) -> ok;
print_re_num(Tab, Key, Num, Re) ->
    Os = re_match(ets:lookup(Tab, Key), Re),
    Len = length(Os),
    if 
	(Num - Len) < 1 ->
	    O = lists:nth(Num, Os),
	    io:format("~p~n", [O]); %% use ppterm here instead
	true ->
	    print_re_num(Tab, ets:next(Tab, Key), Num - Len, Re)
    end.

re_match([], _) -> [];
re_match([H|T], Re) ->
    case regexp:match(to_string(H), Re) of
	{match,_,_} -> 
	    [H|re_match(T,Re)];
	_ ->
	    re_match(T, Re)
    end.

