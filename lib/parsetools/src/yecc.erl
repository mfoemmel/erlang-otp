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
%% Yacc like LALR-1 parser generator for Erlang.
%% Ref: Aho & Johnson: "LR Parsing", ACM Computing Surveys, vol. 6:2, 1974.
%% Auxiliary files: yeccgramm.yrl, yeccparser.erl, yeccpre.hrl, yeccscan.erl.
%%
%% Line number support added to token format and error messages
%% Supports Tobbe's tokenizer
%% Now with improved code generation suggested by rv
%%
%% Generates 'better' internal variable names beginning with "__"  /cww 951206
%%
%% Improved messages about parse action conflicts, and 'Verbose' flag to
%% decide whether to print 'resolved conflicts' messages or not.
%% N.B. this means the optional includefile name has become the *fourth*
%% (optional) parameter to the call to yecc.   /cww 951221
%%
%% 1997-01-27
%% Kenneth Lundin
%% changed to use the platform portable file manipulation functions
%% in new module filename
%%
%% 1997-11-25
%% Hakan Mattsson (hakan@erix.ericsson.se)
%% Replaced linear searches with hash lookups in ets tables. The
%% memory consumption has now been reduced from 107M to 33M bytes 
%% parser and the elapsed time from 88 to 12 minutes for the SQL
%% grammar generation. Added a check for duplicate (non) terminals,
%% The huge yeccgoto/2 function has been replaced with several
%% yeccgoto_RULE/1 functions, in order to make the generated parser
%% faster. pre_ets generates old code with yeccgoto/2 functions.

-module(yecc).
-export([yecc/2, yecc/3, yecc/4, format_error/1, compile/3]).
-export([doit/5]).

-include("erl_compile.hrl").

-define(pre_ets, 1).
-ifdef(pre_ets).
  -define(select(Old, New), Old).
-else.
  -define(select(Old, New), New).
-endif.    

%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator:

yecc(Infile, Outfile) ->
    yecc(Infile, Outfile, false, []).

% Verbose = true/false
yecc(Infile, Outfile, Verbose) ->
    yecc(Infile, Outfile, Verbose, []).

yecc(Infilex, Outfilex, Verbose, Includefilex) ->
    Pid = spawn_link(?MODULE, doit,
		     [self(), Infilex, Outfilex, Verbose, Includefilex]),
    receive
	{?MODULE, Pid, Result} -> Result;
	{'EXIT', Pid, Reason} -> exit(Reason)
    end.

doit(Parent, Infilex, Outfilex, Verbose, Includefilex) ->
    statistics(runtime),
    Infile = strip_extension(Infilex,".yrl"),
    Outfile = strip_extension(Outfilex,".erl"),
    Includefile = strip_extension(Includefilex,".hrl"),
    Inport = open_infile(Infile),
    case parse_grammar(Inport, Infile, 1, [], [], [], [], [], ok) of
	error ->
	    file:close(Inport),
	    exit({yecc, syntax_error});
	Grammar ->
	    Nonterminals = pick_nonterminals(Grammar),
	    Terminals = pick_terminals(Grammar, Nonterminals),
	    Rootsymbol = pick_rootsymbol(Grammar, Nonterminals),
	    Endsymbol = pick_endsymbol(Grammar, Nonterminals, Terminals),
	    Rules = pick_rules(Grammar, Rootsymbol),
	    check_missing_rules(Nonterminals, Rules),
	    check_unused_terminals(Terminals, Rules),
	    Precedences = pick_precedences(Grammar),

	    Allterminals = [Endsymbol | Terminals],
	    TerminalTab = create_terminal_table(Allterminals),
	    {StateTab, GotoTab} =
		compute_states(Nonterminals, TerminalTab,
			       Rootsymbol, Endsymbol, Rules),

	    io:format("Computing parse actions ...~n", []),
	    PrecTab = create_precedence_table(Precedences),
	    Parse_actions = compute_parse_actions(StateTab, GotoTab,
						  TerminalTab,
						  Rules, PrecTab),
	    case find_action_conflicts(Parse_actions, Allterminals,
				       Rules, Verbose, ok) of
		error ->
		    exit({yecc, grammar_not_lalr});
		ok ->
		    Sorted = sort_parse_actions(Parse_actions),
		    Outport = open_outfile(Outfile),
		    output_header(Outport, Outfile, Inport, Grammar, Includefile),
		    file:close(Inport),
		    io:nl(Outport),
		    output_parse_actions(Outport, Sorted, Rules, GotoTab),
		    output_goto(Outport, Nonterminals, GotoTab),
		    io:nl(Outport),
		    file:close(Outport),
		    io:format("~s.erl~n", [Outfile]),
		    Res = statistics(runtime),
		    unlink(Parent),
		    Parent ! {?MODULE, self(), Res},
		    exit(normal)
	    end
    end.

open_infile(Infile0) ->
    Infile = lists:concat([Infile0, ".yrl"]),
    case catch file:open(Infile, read) of
	{error, _} ->
	    io:format("Cannot find/read input file ~s!~n", [Infile]),
	    exit({yecc, bad_input_file});
	{'EXIT', _} ->
	    io:format("Cannot find/read input file ~s!~n",  [Infile]),
	    exit({yecc, bad_input_file});
	{ok, Inport1} ->
	    io:format("Parsing input file ...~n", []),
	    Inport1
    end.

open_outfile(Outfile0) ->
    Outfile = lists:concat([Outfile0, ".erl"]),
    case catch file:open(Outfile, write) of
	{error, _} ->
	    io:format("Cannot open/write file ~s!~n", [Outfile]),
	    exit({yecc, bad_output_file});
	{'EXIT', _} ->
	    io:format("Cannot open/write file ~s!~n", [Outfile]),
	    exit({yecc, bad_output_file});
	{ok, Outport1} ->
	    io:format("Writing file ...~n", []),
	    Outport1
    end.

duplicates(List) ->
    Unique = lists:usort(List),
    {Unique, List -- Unique}.

pick_nonterminals(Grammar) ->
    case lists:keysearch('Nonterminals', 1, Grammar) of
	false ->
	    exit({yecc, nonterminals_missing});
	{value, {_, Nonterminals}} ->
	    case duplicates(Nonterminals) of
		{Unique, []} ->
		    ['ACCEPT' | Unique];
		{Unique, Duplicates} ->
		    io:format("Duplicate non-terminals: ~w~n", [Duplicates]),
		    ['ACCEPT' | Unique]
	    end
    end.

pick_terminals(Grammar, Nonterminals) ->
    case lists:keysearch('Terminals', 1, Grammar) of
	false ->
	    exit({yecc, terminals_missing});
	{value, {_, Terminals1}} ->
	    case duplicates(Terminals1) of
		{Unique, []} ->
		    case intersect(Nonterminals, Unique) of
			[] ->
			    ['$empty' | Unique];
			Common ->
			    exit({yecc, {terminals_and_nonterminals_in_common, Common}})
		    end;
		{Unique, Duplicates} ->
 		    io:format("Duplicate terminals: ~w~n", [Duplicates]),
		    ['$empty' | Unique]
		end
    end.

pick_rootsymbol(Grammar, Nonterminals) ->
    case lists:keysearch('Rootsymbol', 1, Grammar) of
	false ->
	    exit({yecc, rootsymbol_missing});
	{value, {_, [Rootsymbol1]}} ->
	    case lists:member(Rootsymbol1, Nonterminals) of
		false ->
		    exit({yecc, bad_rootsymbol});
		true ->
		    Rootsymbol1
	    end
    end.

pick_endsymbol(Grammar, Nonterminals, Terminals) ->
    case lists:keysearch('Endsymbol', 1, Grammar) of
	false ->
	    '$end';
	{value, {_, [Endsymbol1]}} ->
	    case lists:member(Endsymbol1, Nonterminals) of
		false ->
		    case lists:member(Endsymbol1, Terminals) of
			false ->
			    Endsymbol1;
			true ->
			    exit({yecc, endsymbol_is_a_terminal})
		    end;
		true ->
		    exit({yecc, endsymbol_is_a_nonterminal})
	    end
    end.

pick_rules(Grammar, Rootsymbol) ->
    case lists:keysearch('Rules', 1, Grammar) of
	false ->
	exit({yecc, no_grammar_rules});
    {value, {_, Rules1}} ->
	[{['ACCEPT', Rootsymbol], {form, []}}
	 | Rules1]
    end.

pick_precedences(Grammar) ->
    case lists:keysearch('Precedences', 1, Grammar) of
	false ->
	    [];
	{value, {_, Prec}} ->
	    Prec
    end.

parse_grammar(Inport, Infile, Line, Nonterminals, Prec, Allsymbols, Result, Rules, Flag) ->
    case yeccscan:scan(Inport, '', Line) of
	{eof, _} ->
	    if
		Flag == error ->
		    error;
		true ->
		    [{'Precedences', Prec}, {'Rules', lists:reverse(Rules)}
		     | Result]
	    end;
	{error, {Error_line, Mod, What}, Next_line} ->
	    io:format("~s.yrl:~w: ~s scanning input.~n",
		      [Infile, Error_line, apply(Mod, format_error, [What])]),
	    parse_grammar(Inport, Infile, Next_line, Nonterminals,
			  Prec, Allsymbols, Result, Rules, error);
	{ok, Input, Next_line} ->
	    case yeccparser:parse(Input) of
		{error, {Error_line, Mod, Message}} ->
		    report_error(Infile, Error_line,
				 apply(Mod, format_error, [Message])),
		    parse_grammar(Inport, Infile, Next_line, Nonterminals, Prec,
				  Allsymbols, Result, Rules, error);
		{ok, {rule, Rule, {erlang_code, Tokens}}} ->
		    case lists:member(hd(Rule), Nonterminals) of
			false ->
			    report_error(Infile, Line,
					  lists:concat(["undefined nonterminal: ",
							hd(Rule)])),
			    parse_grammar(Inport, Infile, Next_line, Nonterminals, Prec,
					  Allsymbols, Result, Rules, error);
			true ->
			    case check_rhs(tl(Rule), Allsymbols) of
				undef_symbols ->
				    report_error(Infile, Line,
						  "undefined rhs symbol(s) in rule:"),
				    print_rule(Rule),
				    io:nl(),
				    parse_grammar(Inport, Infile, Next_line,
						  Nonterminals,
						  Prec, Allsymbols, Result,
						  Rules, error);
				illegal_empty ->
				    report_error(Infile, Line,
						  "yecc: illegal use of empty symbol in rhs of rule:"),
				    print_rule(Rule),
				    io:nl(),
				    parse_grammar(Inport, Infile, Next_line,
						  Nonterminals,
						  Prec, Allsymbols, Result,
						  Rules, error);
				ok ->
				    Nmbr_of_daughters =
					case Rule of
					    [_, '$empty']  ->
						0;
					    _ ->
						length(Rule) - 1
					end,
				    Tokens1 =
					subst_dollar_vars(Tokens,
							  Nmbr_of_daughters,
							  Rule),
				    case catch erl_parse:parse_exprs(add_roberts_dot(Tokens1, 0)) of
					{error, {Error_line, Mod, What}} ->
					    report_error(Infile, Error_line,
							 apply(Mod,
							       format_error,
							       [What])),
					    io:format("Bad Erlang code following yecc rule: ~w~n",
						      [Error_line]),
					    print_rule(Rule),
					    io:nl(),
					    parse_grammar(Inport, Infile,
							  Next_line, 
							  Nonterminals,
							  Prec,
							  Allsymbols,
							  Result,
							  Rules, error);
					{ok, Exprs} ->
					    parse_grammar(Inport, Infile,
							  Next_line, 
							  Nonterminals,
							  Prec,
							  Allsymbols,
							  Result,
							  [{Rule, Exprs} | Rules],
							  Flag);
					{'EXIT', _} ->
					    io:format("Bad Erlang code following rule~n",
						      []),
					    print_rule(Rule),
					    io:format("here:~n~w~n", [Tokens]),
					    parse_grammar(Inport, Infile,
							  Next_line,
							  Nonterminals,
							  Prec,
							  Allsymbols,
							  Result,
							  Rules, error)
				    end
			    end
		    end;
		{ok, {'Nonterminals', Symbols}} ->
		    parse_grammar(Inport, Infile, Next_line, Symbols, Prec,
				  Allsymbols,
				  [{'Nonterminals', Symbols} | Result],
				  Rules, Flag);
		{ok, {'Left', [N, Op]}} ->
		    parse_grammar(Inport, Infile, Next_line, Nonterminals,
				  [{Op, N, left} | Prec], Allsymbols, Result,
				  Rules, Flag);
		{ok, {'Unary', [N, Op]}} ->
		    parse_grammar(Inport, Infile, Next_line, Nonterminals,
				  [{Op, N, unary} | Prec],
				  Allsymbols, Result, Rules, Flag);
		{ok, {'Right', [N, Op]}} ->
		    parse_grammar(Inport, Infile, Next_line, Nonterminals,
				  [{Op, N, right} | Prec], Allsymbols, Result,
				  Rules, Flag);
		{ok, {'Terminals', Symbols}} ->
		    if
			Nonterminals == [] ->
			    io:format("Terminals defined before Nonterminals.~n",
				      []),
			    parse_grammar(Inport, Infile, Next_line, [], Prec,
					  Symbols,
					  [{'Terminals', Symbols} | Result],
					  Rules, error);
			true ->
			    parse_grammar(Inport, Infile, Next_line,
					  Nonterminals, Prec,
					  ['$empty'
					   | lists:append(Nonterminals, Symbols)],
					  [{'Terminals', Symbols} | Result],
					  Rules, Flag)
		    end;
		{ok, {'Erlang', [code]}} ->
		    if
			Flag == error ->
			    error;
			true ->
			    [{'Erlang', code},
			     {'Precedences', Prec},
			     {'Rules', lists:reverse(Rules)}
			     | Result]
		    end;
		{ok, Other} ->
		    parse_grammar(Inport, Infile, Next_line, Nonterminals,
				  Prec, Allsymbols,
				  [Other | Result], Rules, Flag)
	    end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert to atom to string if necessary

strip_extension(File, Ext) when atom(File) ->
    strip_extension(atom_to_list(File),Ext);
strip_extension(File, Ext) ->
    case filename:extension(File) of
	Ext -> filename:rootname(File);
	_Other -> File
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_roberts_dot([], Line) ->
    [{'dot', Line}];
add_roberts_dot([{'dot', Line} | _], _) ->
    [{'dot', Line}];
add_roberts_dot([Token | Tokens], _) ->
    [Token | add_roberts_dot(Tokens, element(2, Token))].

report_error(Source_file, Line, Message) ->
    io:format("~s", [lists:concat([Source_file, ":", Line, ": ",
				   format_error(Message), "\n"])]).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_missing_rules([], _) ->
    ok;
check_missing_rules([Nt | Nts], Rules) ->
    case check_missing_rules1(Rules, Nt) of
	ok ->
	    check_missing_rules(Nts, Rules);
	_ ->
	    io:format("*** Warning: no syntax rule for nonterminal symbol '~w'!~n", [Nt]),
	    check_missing_rules(Nts, Rules)
    end.

check_missing_rules1([], _) ->
    not_ok;
check_missing_rules1([{[Nt | _], _} | _], Nt) ->
    ok;
check_missing_rules1([_ | Rules], Nt) ->
    check_missing_rules1(Rules, Nt).

check_unused_terminals([], _) ->
    ok;
check_unused_terminals([T | Ts], Rules) ->
    case check_unused_terminals1(Rules, T) of
	ok ->
	    check_unused_terminals(Ts, Rules);
	_ ->
	    io:format("*** Warning: terminal symbol '~w' not used!~n", [T]),
	    check_unused_terminals(Ts, Rules)
    end.

check_unused_terminals1(_, '$empty') ->
    ok;
check_unused_terminals1([], _) ->
    not_ok;
check_unused_terminals1([{[_ | Daughters], _} | Rules], Terminal) ->
    case lists:member(Terminal, Daughters) of
	true ->
	    ok;
	_ ->
	    check_unused_terminals1(Rules, Terminal)
    end.

check_rhs(['$empty'], Allsymbols) ->
    case lists:member('$empty', Allsymbols) of
	true ->
	    ok;
	false ->
	    undef_symbols
    end;
check_rhs(Rhs, Allsymbols) ->
    case lists:member('$empty', Rhs) of
	true ->
	    illegal_empty;
	false ->
	    case subset(Rhs, Allsymbols) of
		true ->
		    ok;
		false ->
		    undef_symbols
	    end
    end.

subst_dollar_vars([], _, _) ->
    [];
subst_dollar_vars([H | T], Nmbr_of_daughters, Rule) ->
    [subst_dollar_vars(H, Nmbr_of_daughters, Rule)
     | subst_dollar_vars(T, Nmbr_of_daughters, Rule)];
subst_dollar_vars({atom, Pos, Atom}, Nmbr_of_daughters, Rule) ->
    case atom_to_list(Atom) of
	[$$ | Rest] ->
	    case catch list_to_integer(Rest) of
		{_, _} ->
		    {atom, Pos, Atom};
		N when N > 0, N =< Nmbr_of_daughters ->
		    {var, Pos, list_to_atom(lists:append("__", Rest))};
		_ ->
		    io:format("!!! Warning: Constituent variable ~w undefined in rule~n",
			      [Atom]),
		    print_rule(Rule),
		    {atom, Pos, '$undefined'}
	    end;
	_ ->
	    {atom, Pos, Atom}
    end;
subst_dollar_vars(Tuple, Nmbr_of_daughters, Rule) when tuple(Tuple) ->
    list_to_tuple(subst_dollar_vars(tuple_to_list(Tuple), Nmbr_of_daughters,
				    Rule));
subst_dollar_vars(Something_else, _, _) ->
    Something_else.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing parse states and goto table from grammar.
% Start item: {0 bor 1, [Endsymbol], [Rootsymbol]} ==
% (['ACCEPT', '.', Rootsymbol], {'$'}) in Aho & Johnson
% where '$end' is the default end of input symbol of the
% scanner if no 'Endsymbol' has been declared in the syntax file.

compute_states(Nonterminals, TerminalTab, Rootsymbol, Endsymbol, Rules) ->
    io:format("Preparing the rules ...~n", []),
    IndexedTab = make_rule_index(Nonterminals, Rules),
    Lc_table = make_left_corner_table(Nonterminals, TerminalTab, Rules),

    StartItem0 = [{1,[Endsymbol],[Rootsymbol]}],
    StartItem = gb_trees:from_orddict([{1,{[Endsymbol],[Rootsymbol]}}]),
    State0 = compute_closure(StartItem0, StartItem, TerminalTab, IndexedTab, Lc_table),

    StateTab = ets:new(yecc_states, []),
    RulePointerTab = ets:new(yecc_rule_pointers, [duplicate_bag]),
    GotoTab = ets:new(yecc_goto, [duplicate_bag]),
    FirstState = {0, State0},
    insert_state(StateTab, RulePointerTab, FirstState),

    io:format("Computing states and goto table ...~n", []),
    compute_states1([{0, get_current_symbols(State0)}],
		    StateTab, RulePointerTab, FirstState, GotoTab,
		    TerminalTab, IndexedTab, Lc_table).

insert_state(StateTab, RulePointerTab, State) ->
    ets:insert(StateTab, State),
    {N, Items} = State,
    insert_rule_pointer(Items, RulePointerTab, N).

insert_rule_pointer([{Rule_pointer, Lookahead, Rhs} | Items], RulePointerTab, N) ->
    ets:insert(RulePointerTab, {Rule_pointer, N}),
    insert_rule_pointer(Items, RulePointerTab, N);
insert_rule_pointer([], RulePointerTab, N) ->
    ok.

compute_states1([], StateTab, RulePointerTab, CurrState, GotoTab, _, _, _) ->
    {StateTab, GotoTab};
compute_states1([{N, Symbols} | Try], StateTab, RulePointerTab, CurrState, GotoTab, TerminalTab, Rules,
		Lc_table) ->
    case ets:lookup(StateTab, N) of
	[{_N, S}] ->
	    compute_states2(Symbols, N, S, Try, StateTab, RulePointerTab, CurrState, GotoTab, TerminalTab, Rules,
			    Lc_table);
	_ ->
	    exit({yecc, error})
    end.

compute_states2([], _, _, Try, StateTab, RulePointerTab, CurrState, GotoTab, TerminalTab, Rules, Lc_table) ->
    compute_states1(Try, StateTab, RulePointerTab, CurrState, GotoTab, TerminalTab, Rules, Lc_table);
compute_states2([Sym | Syms], N, S, Try, StateTab, RulePointerTab, CurrState, GotoTab, TerminalTab,
		Rules, Lc_table) ->
    case compute_state(S, Sym, [], TerminalTab, Rules, Lc_table) of
	[] ->
	    compute_states2(Syms, N, S, Try, StateTab, RulePointerTab, CurrState, GotoTab,
			    TerminalTab, Rules, Lc_table);
	New_state ->
	    case check_states(New_state, StateTab, RulePointerTab) of
		add ->
		    {M, _} = CurrState,
		    %% io:format("Adding state ~w~n", [M + 1]),
		    Current_symbols = get_current_symbols(New_state),
		    Next = M + 1,
		    NextState = {Next, New_state},
		    insert_state(StateTab, RulePointerTab, NextState),
		    ets:insert(GotoTab, {{N, Sym}, Next}),
		    ets:insert(GotoTab, {Sym, {N, Next}}),
		    compute_states2(Syms, N, S,
				    [{Next, Current_symbols} | Try],
				    StateTab, RulePointerTab, NextState,
				    GotoTab,
				    TerminalTab, Rules, Lc_table);
		{old, M} ->
		    %% io:format("Identical to old state ~w~n", [M]),
		    maybe_insert_goto(GotoTab, N, Sym, M),
		    compute_states2(Syms, N, S, Try, StateTab, RulePointerTab, CurrState,
				    GotoTab,
				    TerminalTab, Rules, Lc_table);
		{merge, M, Change_list} ->
		    %% io:format("Merging with state ~w~n", [M]),
		    New_current = lists:usort(Change_list),
		    Try1 = case lists:keysearch(M, 1, Try) of
			       false ->
				   [{M, New_current} | Try];
			       {value, {_, Old_current}} ->
				   case ord_subset(New_current, Old_current) of
				       true ->
					   Try;
				       false ->
					   [{M, ord_merge(New_current, Old_current)}
					    | lists:keydelete(M, 1, Try)]
				   end
			   end,
		    merge_states(New_state, StateTab, RulePointerTab, M),
		    maybe_insert_goto(GotoTab, N, Sym, M),
		    compute_states2(Syms, N, S, Try1,
				    StateTab, RulePointerTab, CurrState,
				    GotoTab,
				    TerminalTab, Rules, Lc_table)
	    end
    end.

maybe_insert_goto(GotoTab, From, Sym, To) ->
    FromTo = {From, To},
    FromToList = ets:lookup(GotoTab, Sym),
    case lists:keysearch(FromTo, 2, FromToList) of
	{value, _} -> ignore;
	false -> ets:insert(GotoTab, {Sym, FromTo})
    end,	
    FromSym = {From, Sym}, 
    ToList = ets:lookup(GotoTab, FromSym),
    case lists:keysearch(To, 2, ToList) of
	{value, _} -> ignore;
	false -> ets:insert(GotoTab, {FromSym, To})
    end.

%% Create an ets table for faster lookups
create_terminal_table(Allterminals) ->
    TerminalTab = ets:new(yecc_terminals, []),
    init_terminal_table(TerminalTab, Allterminals).

init_terminal_table(TerminalTab, [H | T]) ->
    ets:insert(TerminalTab, {H}),
    init_terminal_table(TerminalTab, T);
init_terminal_table(TerminalTab, []) ->
    TerminalTab.
    
is_terminal(Tab, Terminal) ->
    ets:member(Tab, Terminal).
		
get_current_symbols(State) ->
    lists:usort(get_current_symbols1(State)).

get_current_symbols1([]) ->
    [];
get_current_symbols1([{_, _, Rhs} | Items]) ->
    case Rhs of
	[] ->
	    get_current_symbols1(Items);
	[Symbol | _] ->
	    [Symbol | get_current_symbols1(Items)]
    end.

compute_state([], _, NewState0, TerminalTab, Rules, Lc_table) ->
    NewState = gb_trees:from_orddict(NewState0),
    NewItems = [{I,X,Y} || {I,{X,Y}} <- NewState0],
    lists:keysort(1, compute_closure(NewItems, NewState, TerminalTab, Rules,
				     Lc_table));
compute_state([{Rule_pointer, Lookahead, Rhs} | Items], Symbol, New_state,
	      TerminalTab, Rules, Lc_table) ->
    case Rhs of
	[] ->
	    compute_state(Items, Symbol, New_state, TerminalTab, Rules, Lc_table);
	[Symbol | Rhs1] ->
	    compute_state(Items, Symbol,
			  [{Rule_pointer + 1,{Lookahead,Rhs1}} | New_state],
			  TerminalTab, Rules, Lc_table);
	_ ->
	    compute_state(Items, Symbol, New_state, TerminalTab, Rules, Lc_table)
    end.

compute_closure([], State, TerminalTab, Rules, Lc_table) ->
    [{I,X,Y} || {I,{X,Y}} <- gb_trees:to_list(State)];
compute_closure([Item|Items], State, TerminalTab, Rules, Lc_table) ->
    {Rule_pointer,Lookahead,Rhs} = Item,
    case Rhs of
	[] ->
	    compute_closure(Items, State, TerminalTab, Rules, Lc_table);
	[Category | Followers] ->
	    case ets:lookup(Rules, Category) of
		[{_, Expanding_rules}] ->
		    New_lookahead = compute_lookahead(Followers, Lookahead,
						      TerminalTab, Lc_table),
		    compute_closure1(Expanding_rules, New_lookahead,
				     Items, State, TerminalTab, Rules, Lc_table);
		[] ->
		    compute_closure(Items, State, TerminalTab, Rules, Lc_table)
	    end
    end.

compute_closure1([], _, Items, State, TerminalTab, Rules, Lc_table) ->
    compute_closure(Items, State, TerminalTab, Rules, Lc_table);
compute_closure1([{Rule_nmbr, Rule} | Tail], New_lookahead, Items,
		 State, TerminalTab, Rules, Lc_table) ->
    {Rule_pointer, Rhs} = case Rule of
			      [_ , '$empty' | Rhs0] ->
				  {Rule_nmbr bor 2, Rhs0};
			      [_ | Rhs0] ->
				  {Rule_nmbr bor 1, Rhs0}
			  end,
    case gb_trees:lookup(Rule_pointer, State) of
	{value,{Lookahead2,OldRhs}} ->
	    case ord_subset(New_lookahead, Lookahead2) of
		true ->				%Old
		    compute_closure1(Tail, New_lookahead, Items, State,
				     TerminalTab, Rules, Lc_table);
		false ->			%Merge
		    Lookahead = ord_merge(Lookahead2, New_lookahead),
		    NewState = gb_trees:update(Rule_pointer,
					       {Lookahead,OldRhs}, State),
		    compute_closure1(Tail, New_lookahead,
				     [{Rule_pointer, New_lookahead, Rhs} | Items],
				     NewState,
				     TerminalTab, Rules, Lc_table)
	    end;
	none ->					%New
	    NewItem = {Rule_pointer, New_lookahead, Rhs},
	    compute_closure1(Tail, New_lookahead,
			     [NewItem|Items],
			     gb_trees:insert(Rule_pointer, {New_lookahead,Rhs}, State),
			     TerminalTab, Rules, Lc_table)
    end.

%% Check if some old state is a superset of our New_state
check_states(New_state, StateTab, RulePointerTab) ->
    [{Rule_pointer, _, _} | _] = New_state,
    StateNumbers =  ets:lookup(RulePointerTab, Rule_pointer),
    check_state(StateNumbers, New_state, StateTab, RulePointerTab).

check_state([{_, N} | StateNumbers], New_state, StateTab, RulePointerTab) ->
    [{_N, Old_state}] =  ets:lookup(StateTab, N),
    case catch check_state1(New_state, Old_state) of
	add ->
	    check_state(StateNumbers, New_state, StateTab, RulePointerTab);
	old ->
	    {old, N};
	Change_list ->
	    {merge, N, Change_list}
    end;
check_state([], New_state, StateTab, RulePointerTab) ->
    add.

check_state1([{Rule_pointer, Lookahead1, Rhs} | Items1],
	     [{Rule_pointer, Lookahead2, _} | Items2]) ->
    case ord_subset(Lookahead1, Lookahead2) of
	true ->
	    check_state1(Items1, Items2);
	false ->
	    case Rhs of
		[] ->
		    check_state2(Items1, Items2);
		[Symbol | _] ->
		    [Symbol | check_state2(Items1, Items2)]
	    end
    end;
check_state1([], []) ->
    old;
check_state1(_, _) ->
    throw(add).

check_state2([{Rule_pointer, Lookahead1, Rhs} | Items1],
	     [{Rule_pointer, Lookahead2, _} | Items2]) ->
    case ord_subset(Lookahead1, Lookahead2) of
	true ->
	    check_state2(Items1, Items2);
	false ->
	    case Rhs of
		[] ->
		    check_state2(Items1, Items2);
		[Symbol | _] ->
		    [Symbol | check_state2(Items1, Items2)]
	    end
    end;
check_state2([], []) ->
    [];
check_state2(_, _) ->
    throw(add).

merge_states(New_state, StateTab, RulePointerTab, M) ->
    case ets:lookup(StateTab, M) of
	[] ->
	    ignore;
	[{_M, Old_state}] ->
	    MergedState = merge_states1(New_state, Old_state),
	    insert_state(StateTab, RulePointerTab, {M, MergedState})
    end.

merge_states1([Item1 | Items1], [Item2 | Items2]) when element(2, Item1) == element(2, Item2) ->
    [Item1 | merge_states1(Items1, Items2)];
merge_states1([{Rule_pointer, Lookahead1, Rhs} | Items1],
	      [{_, Lookahead2, _} | Items2]) ->
    [{Rule_pointer, ord_merge(Lookahead1, Lookahead2), Rhs}
     | merge_states1(Items1, Items2)];
merge_states1([], []) ->
    [].


% Lookahead computation is complicated by the possible existence
% of null string rewriting rules, such as  A -> '$empty'.
compute_lookahead([], Old_lookahead, _, _) ->
    Old_lookahead;
compute_lookahead(['$empty' | Followers], Old_lookahead, TerminalTab, Lc_table) ->
    compute_lookahead(Followers, Old_lookahead, TerminalTab, Lc_table);
compute_lookahead([Symbol | Symbols], Old_lookahead, TerminalTab, Lc_table) ->
    case is_terminal(TerminalTab, Symbol) of
	true ->
	    [Symbol];
	false ->
	    case ets:lookup(Lc_table, Symbol) of
		[{_Symbol, ['$empty' | Left_corners]}] ->
		    ord_merge(Left_corners,
			      compute_lookahead(Symbols, Old_lookahead,
						TerminalTab, Lc_table));
		[{_Symbol, Left_corners}] ->
		    Left_corners;
		[] ->
		    exit({Symbol, missing_in_left_corner_table})
	    end
    end.

make_left_corner_table(Nonterminals, TerminalTab, Rules) ->
    LeftHandTab = make_left_hand_rules(Rules),
    LcTab = ets:new(yecc_left_corner, []),
    make_left_corner_table(Nonterminals, TerminalTab, LcTab, LeftHandTab).
    
%% Build table for fast lookup of left hand side of grammar rules
make_left_hand_rules(Rules) ->
    RuleTab = ets:new(yecc_left_hand_rules, [bag]),
    insert_left_hand_rules(Rules, RuleTab).

insert_left_hand_rules([{[Lhs | Rhs], _} | Rules], RuleTab) ->
    ets:insert(RuleTab, {Lhs, Rhs}),
    insert_left_hand_rules(Rules, RuleTab);
insert_left_hand_rules([], RuleTab) ->
    RuleTab.

make_left_corner_table([NT | Tail], TerminalTab, LcTab, LeftHandTab) ->
    Corner = top_find_left_corners([NT], TerminalTab, LeftHandTab, [[NT]], [], []),
    ets:insert(LcTab, {NT, Corner}),
    make_left_corner_table(Tail, TerminalTab, LcTab, LeftHandTab);
make_left_corner_table([], _, LcTab, _) ->
    LcTab.

% Which are the possible lower left corner terminals of a (sub)tree
% dominated by a given head/root symbol?

top_find_left_corners(Followers, TerminalTab, LeftHandTab, Found1, Found2, Try) ->
    Head = hd(Followers),
    case Head of
	'bit' -> ok;
	'bits' -> ok;
	_ -> ok
    end,
    Rules = ets:lookup(LeftHandTab, Head),
    find_left_corners(Rules, Followers, TerminalTab, LeftHandTab, Found1, Found2, Try).
    
find_left_corners([{Head, Rhs} | Tail], Followers, TerminalTab, LeftHandTab, Found1, Found2, Try) ->
    Sisters = tl(Followers), % Skip Head
    Expansion = case Rhs of
		    ['$empty'] ->
			if
			    Sisters == [] ->
				['$empty'];
			    true ->
				Sisters
			end;
		    [NonEmpty] ->
			[NonEmpty | Sisters];
		    _ ->
			case find_prefix(Rhs, Found1) of
			    true ->
				[];
			    false ->
				lists:append(Rhs, Sisters)
			end
		end,
    case Expansion of
	[Symbol2 | Symbols] ->
	    Terminal = ets:lookup(TerminalTab, Symbol2),
	    if
		Terminal /= [] ->
		    %% Terminal, add to Found2
		    find_left_corners(Tail, Followers, TerminalTab, LeftHandTab,
				      Found1, add_if_not_there(Symbol2, Found2), Try);
		true ->
		    %% Nonterminal
		    case lists:member(Expansion, Found1) of
			false ->
			    %% Add to Found1 and Try
			    find_left_corners(Tail, Followers, TerminalTab, LeftHandTab,
					      [Expansion | Found1], Found2, [Expansion | Try]);
			true ->
			    find_left_corners(Tail, Followers, TerminalTab, LeftHandTab,
					      Found1, Found2, Try)
		    end
	    end;
	[] ->
	    find_left_corners(Tail, Followers, TerminalTab, LeftHandTab,
			      Found1, Found2, Try)
    end;
find_left_corners([], _, _, _, _, Found2, []) ->
    case lists:member('$empty', Found2) of
	true ->
	    ['$empty' | lists:sort(lists:delete('$empty', Found2))];
	false ->
	    lists:sort(Found2)
    end;
find_left_corners([], _Followers, TerminalTab, LeftHandTab, Found1, Found2, [First | Rest]) ->
    top_find_left_corners(First, TerminalTab, LeftHandTab, Found1, Found2, Rest).

find_prefix(X, []) ->
    false;
find_prefix(X, [H | T]) ->
    case lists:prefix(X, H) of
	true ->
	    true;
	false ->
	    find_prefix(X, T)
    end.

% Which grammar rules can be used to expand a given non terminal symbol?
make_rule_index(Nonterminals, Rules) ->
    RuleTab = ets:new(yecc_rule_index, []),
    insert_rule_index(Nonterminals, Rules, RuleTab).

insert_rule_index([Nonterminal | Tail], Rules, RuleTab) ->
    Rule = compute_rule_index(Rules, Nonterminal, 0),
    ets:insert(RuleTab, {Nonterminal, Rule}),
    insert_rule_index(Tail, Rules, RuleTab);
insert_rule_index([], _, RuleTab) ->
    RuleTab.

compute_rule_index([{[Nonterminal | Daughters], _} | Rules], Nonterminal, N) ->
    [{N, [Nonterminal | Daughters]}
     | compute_rule_index(Rules, Nonterminal, N + 256)];
compute_rule_index([_ | Rules], Nonterminal, N) ->
    compute_rule_index(Rules, Nonterminal, N + 256);
compute_rule_index([], _, _) ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing parse action table from list of states and goto table:

compute_parse_actions(StateTab, GotoTab, TerminalTab, Rules, PrecTab) ->
    compute_parse_actions(0, StateTab, GotoTab, TerminalTab, Rules, PrecTab).

compute_parse_actions(N, StateTab, GotoTab, TerminalTab, Rules, PrecTab) ->
    case ets:lookup(StateTab, N) of
	[{N, StateN}] ->
	    [{N, 
	     compute_parse_actions1(StateN, N, GotoTab, TerminalTab, Rules, PrecTab)}
	     | compute_parse_actions(N + 1, StateTab, GotoTab, TerminalTab, Rules, PrecTab)];
	[] ->
	    []
    end.

compute_parse_actions1([], _, _, _, _, _) ->
    [];
compute_parse_actions1([{Rule_pointer, Lookahead, Rhs} | Items], N, GotoTab,
		       TerminalTab, Rules, PrecTab) ->
    case Rhs of
	[] ->
	    case rule(Rule_pointer bsr 8, Rules) of
		['ACCEPT' | _] ->
		    [{Lookahead, accept}
		     | compute_parse_actions1(Items, N, GotoTab, TerminalTab,
					      Rules, PrecTab)];
		[Head , '$empty'] ->
		    [{Lookahead,
		      {reduce, Rule_pointer bsr 8, Head, 0, {0, none}}}
		     | compute_parse_actions1(Items, N, GotoTab, TerminalTab,
					      Rules, PrecTab)];
		[Head | Daughters] ->
		    [{Lookahead,
		      {reduce, Rule_pointer bsr 8, Head, length(Daughters),
		       get_prec(Daughters, PrecTab)}}
		     | compute_parse_actions1(Items, N, GotoTab, TerminalTab,
					      Rules, PrecTab)]
	    end;
	[Symbol | _] ->
	    case is_terminal(TerminalTab, Symbol) of
		true ->
		    Prec1 = case rule(Rule_pointer bsr 8, Rules) of
				[Head, Symbol] ->
				    get_prec([Head, Symbol], PrecTab);
				_ ->
				    get_prec([Symbol], PrecTab)
			    end,
		    [{[Symbol], {shift, goto(N, Symbol, GotoTab), Prec1}}
		     | compute_parse_actions1(Items, N, GotoTab, TerminalTab,
					      Rules, PrecTab)];
		false ->
		    compute_parse_actions1(Items, N, GotoTab, TerminalTab, Rules,
					   PrecTab)
	    end
    end.

get_prec(Symbols, PrecTab) ->
    get_prec1(Symbols, PrecTab, {0, none}).

get_prec1([], _, P) ->
    P;
get_prec1([Symbol | T], PrecTab, P) ->
    case ets:lookup(PrecTab, Symbol) of
	[] ->
	    get_prec1(T, PrecTab, P);
	[{_, N, Ass}] ->
	    get_prec1(T, PrecTab, {N, Ass})
    end.

create_precedence_table(Precedences) ->
    PrecTab = ets:new(yecc_precedences, []),
    init_precedence_table(PrecTab, Precedences).

init_precedence_table(PrecTab, [H | T]) ->
    ets:insert(PrecTab, H),
    init_precedence_table(PrecTab, T);
init_precedence_table(PrecTab, []) ->
    PrecTab.
    
goto(From, Symbol, GotoTab) ->
    case ets:lookup(GotoTab, {From, Symbol}) of
	[{_, To}] ->
	    To;
	[] ->
	    exit({yecc, From, Symbol, error_in_goto_table})
    end.

% To detect shift-reduce, and reduce-reduce conflicts
find_action_conflicts([], _, _, _, Error_flag) ->
    Error_flag;
find_action_conflicts([{N, Actions} | Tail], Terminals, Rules, Verbose,
		      Error_flag) ->
    Flag1 = find_action_conflicts1(Terminals, N, Actions, Rules, Verbose,
				   Error_flag),
    find_action_conflicts(Tail, Terminals, Rules, Verbose, Flag1).

find_action_conflicts1([], _, _, _, _, Error_flag) ->
    Error_flag;
find_action_conflicts1([T | Terminals], N, Actions, Rules, Verbose,
		       Error_flag) ->
    Flag1 = find_action_conflicts2(Actions, T, N, [], Rules, Verbose,
				   Error_flag),
    find_action_conflicts1(Terminals, N, Actions, Rules, Verbose, Flag1).

% Modified to resolve shift-reduce conflicts
find_action_conflicts2([], _, _, _, _, _, Flag) ->
    Flag;
find_action_conflicts2([{Lookahead, Action} | Actions], Terminal, N, Found,
		       Rules, Verbose, Flag) ->
    case lists:member(Terminal, Lookahead) of
	true ->
	    case Found of
		[] ->
		    find_action_conflicts2(Actions, Terminal, N, [Action],
					   Rules, Verbose, Flag);
		[Action] ->
		    find_action_conflicts2(Actions, Terminal, N, Found, Rules,
					   Verbose, Flag);
		[{shift, _, {P1, Ass1}}] ->
		    case Action of
			{reduce, _, _, _, {P2, Ass2}} ->
			    if
				Verbose == true ->
				    report_action_conflict(Terminal, N, Found,
							   Action, Rules);
				true ->
				    do_nothing
			    end,
			    {Found1, Flag1} =
				if
				    P1 > P2 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{Found, Flag};
				    P2 > P1 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of reduce.~n',
							  []);
					    true ->
						do_nothing
					end,
					{[Action], Flag};
				    Ass1 == left, Ass2 == left ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of reduce.~n',
							  []);
					    true ->
						do_nothing
					end,
					{[Action], Flag};
				    Ass1 == right, Ass2 == right ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{Found, Flag};
				    P1 == 0, P2 == 0 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{Found, Flag};
				    true ->
					if
					    Verbose =/= true ->
						report_action_conflict(Terminal,
								       N, Found,
								       Action,
								       Rules);
					    true ->
						do_nothing
					end,
					{[Action | Found], error}
				end,
			    find_action_conflicts2(Actions, Terminal, N,
						   Found1, Rules, Verbose,
						   Flag1);
			_ ->
			    report_action_conflict(Terminal, N, Found, Action,
						   Rules),
			    find_action_conflicts2(Actions, Terminal, N,
						   [Action | Found], Rules,
						   Verbose, error)
		    end;
		[{reduce, _, Categ1, _, {P1, Ass1}}] ->
		    case Action of
			{shift, _, {P2, Ass2}} ->
			    if
				Verbose == true ->
				    report_action_conflict(Terminal, N, Found,
							   Action, Rules);
				true ->
				    do_nothing

			    end,
			    {Found1, Flag1} =
				if
				    P1 > P2 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of reduce.~n',
							  []);
					    true ->
						do_nothing
					end,
					{Found, Flag};
				    P2 > P1 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{[Action], Flag};
				    Ass1 == left, Ass2 == left ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of reduce.~n',
							  []);
					    true ->
						do_nothing
					end,
					{Found, Flag};
				    Ass1 == right, Ass2 == right ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{[Action], Flag};
				    P1 == 0, P2 == 0 ->
					if
					    Verbose == true ->
						io:format('Resolved in favor of shift.~n',
							  []);
					    true ->
						do_nothing
					end,
					{[Action], Flag};
				    true ->
					if
					    Verbose =/= true ->
						report_action_conflict(Terminal,
								       N, Found,
								       Action,
								       Rules);
					    true ->
						do_nothing
					end,
					{[Action | Found], error}
				end,
			    find_action_conflicts2(Actions, Terminal, N,
						   Found1, Rules, Verbose,
						   Flag1);
			{reduce, _, Categ2, _, {P2, _}} ->
			    if
				Verbose == true ->
				    report_action_conflict(Terminal, N, Found,
							   Action, Rules);
				true ->
				    do_nothing

			    end,
			    if
				P1 > P2 ->
				    if
					Verbose == true ->
					    io:format('Resolved in favor of ~w.~n',
						      [Categ1]);
					true ->
					    do_nothing
				    end,
				    find_action_conflicts2(Actions, Terminal,
							   N, Found, Rules,
							   Verbose, Flag);
				P2 > P1 ->
				    if
					Verbose == true ->
					    io:format('Resolved in favor of ~w.~n',
						      [Categ2]);
					true ->
					    do_nothing
				    end,
				    find_action_conflicts2(Actions, Terminal,
							   N, [Action], Rules,
							   Verbose, Flag);
				P1 == P2 ->
				    if
					Verbose =/= true ->
					    report_action_conflict(Terminal,
								   N, Found,
								   Action,
								   Rules);
					true ->
					    do_nothing
				    end,
				    find_action_conflicts2(Actions, Terminal,
							   N, [Action | Found],
							   Rules, Verbose,
							   error)
			    end;
			_ ->
			    report_action_conflict(Terminal, N, Found, Action,
						   Rules),
			    find_action_conflicts2(Actions, Terminal, N,
						   [Action | Found], Rules,
						   Verbose, error)
		    end;
		_ ->
		    case lists:member(Action, Found) of
			true ->
			    find_action_conflicts2(Actions, Terminal, N, Found,
						   Rules, Verbose, Flag);
			false ->
			    report_action_conflict(Terminal, N, Found, Action,
						   Rules),
			    find_action_conflicts2(Actions, Terminal, N,
						   [Action | Found], Rules,
						   Verbose, error)
		    end
	    end;
	false ->
	    find_action_conflicts2(Actions, Terminal, N, Found, Rules,
				   Verbose, Flag)
    end.

% Sort parse actions: according to operator precedences if there are any,
% otherwise accept and shift actions first,
% then the reduce actions with the one with most lookahead terminals last;
% this is to implement the "reduce as default" optimization in Aho & Johnson.
sort_parse_actions([]) ->
    [];
sort_parse_actions([{N, La_actions} | Tail]) ->
    [{N, sort_parse_actions1(La_actions)}
     | sort_parse_actions(Tail)].

sort_parse_actions1([]) ->
    [];
sort_parse_actions1([La_action | La_actions]) ->
    case lists:member(La_action, La_actions) of
	true ->
	    sort_parse_actions1(La_actions);
	false ->
	    insert_parse_action(La_action, sort_parse_actions1(La_actions))
    end.

insert_parse_action(La_action, []) ->
    [La_action];
insert_parse_action({Lookahead1, Action1}, [{Lookahead2, Action2} | Tail]) ->
    case Action1 of
	accept ->
	    [{Lookahead1, Action1}, {Lookahead2, Action2} | Tail];
	{shift, _, {P1, Ass1}} ->
	    case Action2 of
		{reduce, _, _, _, {P2, Ass2}} ->
		    case lists:member(hd(Lookahead1), Lookahead2) of
			true ->
			    if
				P1 > P2 ->
				    [{Lookahead1, Action1},
				     {Lookahead2, Action2} | Tail];
				P1 == P2 ->
				    if
					Ass1 == left, Ass2 == left ->
					    [{Lookahead2, Action2} | Tail];
					true ->
					    [{Lookahead1, Action1},
					     {Lookahead2, Action2} | Tail]
				    end;
				true ->
				    [{Lookahead2, Action2} | Tail]
			    end;
			false ->
			    if
				Tail == [] ->
				    [{Lookahead1, Action1},
				     {Lookahead2, Action2}];
				true ->
				    [{Lookahead2, Action2}
				     | insert_parse_action({Lookahead1,
							    Action1},
							   Tail)]
			    end
		    end;
		_ ->
		    [{Lookahead2, Action2}
		     | insert_parse_action({Lookahead1, Action1}, Tail)]
	    end;
	{reduce, _, _, _, {P1, Ass1}} ->
	    case Action2 of
		accept ->
		    [{Lookahead2, Action2}
		     | insert_parse_action({Lookahead1, Action1}, Tail)];
		{shift, _, {P2, Ass2}} ->
		    case lists:member(hd(Lookahead2), Lookahead1) of
			true ->
			    if
				P1 > P2 ->
				    insert_parse_action({Lookahead1, Action1},
							Tail);
				P1 == P2 ->
				    if
					Ass1 == left, Ass2 == left ->
					    insert_parse_action({Lookahead1,
								 Action1},
								Tail);
					true ->
					    [{Lookahead2, Action2}
					     | insert_parse_action({Lookahead1,
								    Action1},
								   Tail)]
				    end;
				true ->
				    [{Lookahead2, Action2}
				     | insert_parse_action({Lookahead1, Action1},
							   Tail)]
			    end;
			false ->
			    [{Lookahead2, Action2}
			     | insert_parse_action({Lookahead1, Action1},
						   Tail)]
		    end;
		{reduce, _, _, _, {P2, _}} ->
		    case intersect(Lookahead1, Lookahead2) of
			[] ->
			    if
				length(Lookahead1) =< length(Lookahead2) ->
				    [{Lookahead1, Action1},
				     {Lookahead2, Action2} | Tail];
				true ->
				    [{Lookahead2, Action2}
				     | insert_parse_action({Lookahead1, Action1},
							   Tail)]
			    end;
			_ ->
			    if
				P1 > P2 ->
				    [{Lookahead1, Action1},
				     {Lookahead2, Action2} | Tail];
				P1 < P2 ->
				    [{Lookahead2, Action2}
				     | insert_parse_action({Lookahead1, Action1},
							   Tail)];
				P1 == P2 ->
				    exit({yecc, Action1, Action2,
					  unresolved_parse_action_conflict})
			    end
		    end
	    end
    end.

report_action_conflict(Symbol, N, Found, New_action, Rules) ->
    io:format("*** Parse action conflict scanning symbol '~s' in state ~w:~n",
	      [Symbol, N]),
    case Found of
	[{reduce, Rule_nmbr1, _, _, _} | _] ->
	    R1 = rule(Rule_nmbr1, Rules),
	    io:format("   Reduce to '~w' from ~w (rule ~w)~n      vs.~n",
		      [hd(R1), tl(R1), Rule_nmbr1]),
	    case New_action of
		{reduce, Rule_nmbr2, _, _, _} ->
		    R2 = rule(Rule_nmbr2, Rules),
		    io:format("   reduce to '~w' from ~w (rule ~w).~n",
			      [hd(R2), tl(R2), Rule_nmbr2]);
		{shift, New_state, _} ->
		    io:format("   shift to state ~w, adding right sisters to '~w'.~n",
			      [New_state, lists:last(tl(R1))]);
		_ ->
		    io:format('   ~w.~n', [New_action])
	    end;
	[{shift, New_state, _} | _] ->
	    case New_action of
		{reduce, Rule_nmbr, _, _, _} ->
		    R = rule(Rule_nmbr, Rules),
		    io:format("   Reduce to '~w' from ~w (rule ~w)~n      vs.~n",
			      [hd(R), tl(R), Rule_nmbr]),
		    io:format("   shift to state ~w, adding right sisters to '~w'.~n",
			      [New_state, lists:last(tl(R))]);
		_ ->
		    io:format('   ~w vs. ~w.~n', [Found, New_action])
	    end;
	_ ->
	    io:format('   ~w vs. ~w.~n', [Found, New_action])
    end.

rule(Rule_pointer, Rules) ->
    element(1, lists:nth(Rule_pointer + 1, Rules)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code generation:

output_header(Outport, Outfile, Inport, Grammar, []) ->
    io:format(Outport, "-module(~s).~n",
	      [filename:basename(Outfile)]),
    io:format(Outport,
	      "-define(THIS_MODULE, ~s).~n",
	      [filename:basename(Outfile)]),
    io:format(Outport,
	      "-export([parse/1, parse_and_scan/1, format_error/1]).~n",
	      []),
    case lists:keysearch('Erlang', 1, Grammar) of
	{value, {_, code}} ->
	    include1([], Inport, Outport);
	_ ->
	    do_nothing
    end,
    io:nl(Outport),
    include(lists:concat([code:lib_dir(parsetools),
			  "/include/yeccpre.hrl"]),
	    Outport);
output_header(Outport, Outfile, Inport, Grammar, Includefile) ->
    io:format(Outport, "-module(~s).~n",
	      [filename:basename(Outfile)]),
    include(lists:concat([Includefile, ".hrl"]),
	    Outport),
    io:nl(Outport),
    case lists:keysearch('Erlang', 1, Grammar) of
	{value, {_, code}} ->
	    include1([], Inport, Outport);
	_ ->
	    do_nothing
    end.

output_goto(Port, [Nonterminal | Tail], GotoTab) ->
    case ets:lookup(GotoTab, Nonterminal) of
	[] ->
	    ignore;
	List0 ->
	    List = ?select(lists:keysort(2, List0), List0),
	    output_goto1(Port, List, Nonterminal)
    end,
    output_goto(Port, Tail, GotoTab);
output_goto(Port, [], _) ->
    ?select(io:format(Port, 'yeccgoto(__Symbol, __State) ->~n', []), ok),
    ?select(io:format(Port, ' exit({__Symbol, __State, missing_in_goto_table}).~n~n', []), ok).

output_goto1(Port, [{Nonterminal, {From, To}} | Tail], _Nonterminal) ->
    ?select(io:format(Port, 'yeccgoto(~w, ~w) ->~n', [Nonterminal, From]),
	    io:format(Port, "'yeccgoto_~s'(~w) ->", [atom_to_list(Nonterminal), From])),
    io:format(Port, ' ~w;~n', [To]),
    output_goto1(Port, Tail, Nonterminal);
output_goto1(Port, [], Nonterminal) ->
    ?select(ok, io:format(Port, "'yeccgoto_~s'(__State) ->", [atom_to_list(Nonterminal)])),
    ?select(ok, io:format(Port, ' exit({~w, __State, missing_in_goto_table}).~n~n', [Nonterminal])).

% This produces a lot of function clauses; optimized as to reduce actions
output_parse_actions(Port, [{State, La_actions} | Tail], Rules, GotoTab) ->
    output_parse_actions1(Port, State, La_actions, Rules, GotoTab),
    output_parse_actions(Port, Tail, Rules, GotoTab);
output_parse_actions(Port, [], _, _) ->
    io:format(Port, 'yeccpars2(__Other, _, _, _, _, _, _) ->~n', []),
    io:format(Port,
	      ' exit({parser, __Other, missing_state_in_action_table}).~n~n',
	      []).

output_parse_actions1(Port, State, [], _, _) ->
    io:format(Port, 'yeccpars2(~w, _, _, _, __T, _, _) ->~n', [State]),
    io:format(Port, ' yeccerror(__T);~n', []);
output_parse_actions1(Port, State,
		      [{_, {reduce, Rule_nmbr, Head, Nmbr_of_daughters, _}}],
		      Rules, GotoTab) ->
    Code = code(Rule_nmbr, Rules),
    Open_stack =
	case Code of
	    [{var, _, '_1'}] when Nmbr_of_daughters == 1 ->
		io:format(Port,
			  'yeccpars2(~w, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->~n',
			  [State]),
		false;
	    _ ->
		io:format(Port, 'yeccpars2(~w, __Cat, __Ss, ~s, __T, __Ts, __Tzr) ->~n',
			  [State, pp_exprs([create_stack(Nmbr_of_daughters)])]),
		case length(Code) of
		    1 ->
			io:format(Port, ' __Val = ~s,~n', [pp_exprs(Code)]);
		    _ ->
			io:format(Port, ' __Val = begin~n  ~s~n  end,~n',
				  [pp_exprs(Code)])
		end,
		true
	end,
    if
	Nmbr_of_daughters > 1 ->
	    io:format(Port, ' __Nss = lists:nthtail(~w, __Ss),~n',
		      [Nmbr_of_daughters - 1]);
	true ->
	    nothing
    end,
    case Nmbr_of_daughters of
	0 ->
	    Next_state = goto_lookup(State, Head, GotoTab),
	    io:format(Port,
		      ' yeccpars2(~w, __Cat, [~w | __Ss], ~s, __T, __Ts, __Tzr);~n',
		      [Next_state, State,
		       if Open_stack == true -> "[__Val | __Stack]";
			   true -> "__Stack" end]);
	1 ->
	    Goto = ?select(' yeccpars2(yeccgoto(~w, hd(__Ss)), __Cat, __Ss, ~s, __T, __Ts, __Tzr);~n',
			   " yeccpars2('yeccgoto_~s'(hd(__Ss)), __Cat, __Ss, ~s, __T, __Ts, __Tzr);~n"),
	    io:format(Port, Goto,		      
		      [?select(Head, atom_to_list(Head)),
			       if Open_stack == true -> "[__Val | __Stack]";
				  true -> "__Stack" end]);
	_ ->
	    Goto = ?select(' yeccpars2(yeccgoto(~w, hd(__Nss)), __Cat, __Nss, ~s, __T, __Ts, __Tzr);~n',
			   " yeccpars2('yeccgoto_~s'(hd(__Nss)), __Cat, __Nss, ~s, __T, __Ts, __Tzr);~n"),
	    
	    io:format(Port, Goto,
		      [?select(Head, atom_to_list(Head)),
		       if Open_stack == true -> "[__Val | __Stack]";
			  true -> "__Stack" end])
    end;
output_parse_actions1(Port, State, [{Lookahead, Action} | Tail], Rules,
		      GotoTab) ->
    output_parse_actions2(Port, State, Lookahead, Action, Rules, GotoTab),
    output_parse_actions1(Port, State, Tail, Rules, GotoTab).

output_parse_actions2(_, _, [], _, _, _) ->
    ok;
output_parse_actions2(Port, State, [Terminal | Tail], Action, Rules, GotoTab) ->
    output_parse_actions3(Port, State, Terminal, Action, Rules, GotoTab),
    output_parse_actions2(Port, State, Tail, Action, Rules, GotoTab).

output_parse_actions3(Port, State, Terminal,
		      {reduce, Rule_nmbr, Head, Nmbr_of_daughters, _},
		      Rules, GotoTab) ->
    Code = code(Rule_nmbr, Rules),
    Open_stack =
	case Code of
	    [{var, _, '__1'}] when Nmbr_of_daughters == 1 ->
		io:format(Port,
			  'yeccpars2(~w, ~s, __Ss, __Stack, __T, __Ts, __Tzr) ->~n',
			  [State, quoted_atom(Terminal)]),
		false;
	    _ ->
		io:format(Port, 'yeccpars2(~w, ~s, __Ss,~s, __T, __Ts, __Tzr) ->~n',
			  [State, quoted_atom(Terminal),
			   pp_exprs([create_stack(Nmbr_of_daughters)])]),
		case length(Code) of
		    1 ->
			io:format(Port, ' __Val = ~s,~n', [pp_exprs(Code)]);
		    _ ->
			io:format(Port, ' __Val = begin~n  ~s~n  end,~n',
				  [pp_exprs(Code)])
		end,
		true
	end,
    if
	Nmbr_of_daughters > 1 ->
	    io:format(Port, ' __Nss = lists:nthtail(~w, __Ss),~n',
		      [Nmbr_of_daughters - 1]);
	true ->
	    nothing
    end,
    case Nmbr_of_daughters of
	0 ->
	    Next_state = goto_lookup(State, Head, GotoTab),
	    io:format(Port,
		      ' yeccpars2(~w, ~s, [~w | __Ss], ~s, __T, __Ts, __Tzr);~n',
		      [Next_state, quoted_atom(Terminal), State,
		       if Open_stack == true -> "[__Val | __Stack]";
			   true -> "__Stack" end]);
	1 ->
	    Goto = ?select(' yeccpars2(yeccgoto(~w, hd(__Ss)), ~w, __Ss, ~s, __T, __Ts, __Tzr);~n',
			   " yeccpars2('yeccgoto_~s'(hd(__Ss)), ~w, __Ss, ~s, __T, __Ts, __Tzr);~n"),
	    io:format(Port, Goto,
		      [?select(Head, atom_to_list(Head)),
		       Terminal, if Open_stack == true -> "[__Val | __Stack]";
				    true -> "__Stack" end]);
	_ ->
	    Goto = ?select(' yeccpars2(yeccgoto(~w, hd(__Nss)), ~w, __Nss, ~s, __T, __Ts, __Tzr);~n',
			   " yeccpars2('yeccgoto_~s'(hd(__Nss)), ~w, __Nss, ~s, __T, __Ts, __Tzr);~n"),
	    io:format(Port, Goto,
		      [?select(Head, atom_to_list(Head)),
		       Terminal,
		       if Open_stack == true -> "[__Val | __Stack]";
			  true -> "__Stack" end])
    end;
output_parse_actions3(Port, State, Terminal, {shift, New_state, _}, Rules, _) ->
    io:format(Port, 'yeccpars2(~w, ~s, __Ss, __Stack, __T, __Ts, __Tzr) ->~n',
	      [State, quoted_atom(Terminal)]),
    io:format(Port,
	      ' yeccpars1(__Ts, __Tzr, ~w, [~w | __Ss], [__T | __Stack]);~n',
	      [New_state, State]);
output_parse_actions3(Port, State, Terminal, accept, _, _) ->
    io:format(Port, 'yeccpars2(~w, ~s, _, __Stack, _, _, _) ->~n',
	      [State, quoted_atom(Terminal)]),
    io:format(Port, ' {ok, hd(__Stack)};~n', []);
output_parse_actions3(Port, State, Terminal, error, _, _) ->
    io:format(Port, 'yeccpars2(~w, ~s, _, _, __T, _, _) ->~n',
	      [State, quoted_atom(Terminal)]),
%    io:format(Port, ' yeccerror(element(2, __T), __T);~n', []);
    io:format(Port, ' yeccerror(__T);~n', []);
output_parse_actions3(Port, State, Terminal, Other, _, _) ->
    io:format(Port, 'yeccpars2(~w, ~s, _, _, _, _, _) ->~n',
	      [State, quoted_atom(Terminal)]),
    io:format(Port, ' ~w;~n', [Other]).

quoted_atom(Atom) ->
    [$'|atom_to_list(Atom)++"'"].
    
create_stack(0) ->
    {var, 0, '__Stack'};
create_stack(N) when N > 0 ->
    {cons, 0, {var, 0, list_to_atom(lists:append("__", integer_to_list(N)))},
     create_stack(N - 1)}.

code(Rule_nmbr, Rules) ->
    element(2, lists:nth(Rule_nmbr + 1, Rules)).

goto_lookup(From, Symbol, GotoTab) ->
    case ets:lookup(GotoTab, {From, Symbol}) of
	[{_, To}] ->
	    To;
	[] ->
	    io:format('yecc error: ~w, ~w not in goto table!~n',
		      [From, Symbol]),
	    exit({yecc, bad_goto_table})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries:

add_if_not_there(X, L) ->
    case lists:member(X, L) of
	true ->
	    L;
	false ->
	    [X | L]
    end.

intersect([], L) ->
    [];
intersect([H | T], L) ->
    case lists:member(H, L) of
	true ->
	    [H | intersect(T, L)];
	false ->
	    intersect(T, L)
    end.

ord_merge([H1|Es1], [H2|_]=Es2) when H1 < H2 ->
    [H1|ord_merge(Es1, Es2)];
ord_merge([H1|Es1], [H2|Es2]) when H1 =:= H2 ->
    [H1|ord_merge(Es1, Es2)];
ord_merge([H1|_]=Es1, [H2|Es2]) ->
    [H2|ord_merge(Es1, Es2)];
ord_merge([], Es2) ->
    Es2;
ord_merge(Es1, []) ->
    Es1.

ord_subset([H | T1], [H | T2]) ->
    ord_subset(T1, T2);
ord_subset([H1 | _], [H2 | _]) when H1 < H2 ->
    false;
ord_subset([], _) ->
    true;
ord_subset(L, []) ->
    false;
ord_subset(L, [_ | T]) ->
    ord_subset(L, T).

subset([], _) ->
    true;
subset([H | T], L) ->
    case lists:member(H, L) of
	true ->
	    subset(T, L);
	false ->
	    false
    end.

print_rule([Lhs | Rhs]) ->
    io:format('~w ->', [Lhs]),
    print_rule1(Rhs).

print_rule1([]) ->
    io:nl();
print_rule1([H | T]) ->
    io:format(' ~w', [H]),
    print_rule1(T).

include(File, Outport) ->
    case catch file:open(File, read) of
	{error, _} ->
	    io:format("Cannot find/read input file ~s!~n", [File]),
	    exit({include, bad_input_file});
	{'EXIT', _} ->
	    io:format("Cannot find/read input file ~s!~n", [File]),
	    exit({include, bad_input_file});
	{ok, Inport} ->
	    Line = io:get_line(Inport, ''),
	    include1(Line, Inport, Outport),
	    file:close(Inport)
    end.

include1(eof, _, _) ->
    ok;
include1(Line, Inport, Outport) ->
    io:put_chars(Outport, Line),
    include1(io:get_line(Inport, ''), Inport, Outport).

pp_exprs([]) ->
    [];
pp_exprs([H]) ->
    [" ", erl_pp:expr(H)];
pp_exprs([H | T]) ->
    [" ", erl_pp:expr(H), "," | pp_exprs(T)].

%%% Interface to erl_compile.

compile(Input, Output, #options{verbose=Verbose, includes=Includes}) ->
    Args = [Input, Output, Verbose|include_option(Includes)],
    case catch apply(yecc, yecc, Args) of
	{'EXIT', {yecc, Reason}} ->
	    error;
	_Other ->
	    ok
    end.

include_option([]) ->
    [];
include_option([Include|_]) ->
    [Include].

