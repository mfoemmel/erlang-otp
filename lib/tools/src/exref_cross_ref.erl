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
%%
%% Functions that create a cross reference graph
%% from module files.

-module(exref_cross_ref).

-export([xref_module/2, xref_directory/2, xref_directory_module/3, del_module/2]).

-include("exref_state.hrl").
-include_lib("kernel/include/file.hrl").




xref_module([M | Ms], S) when atom(M) ->
    {_OK, S1} = xref_file("", M, S),
    xref_module(Ms, S1);
xref_module(M, S) when atom(M) ->
    {_OK, S1} = xref_file("", M, S),
    xref_auto(S1);
xref_module([], S) ->
    xref_auto(S).

xref_directory(Dir, S) ->
    {_OK, S1} = xref_dir(Dir, S),
    xref_auto(S1).

xref_dir(Path, S) ->
    case file:list_dir(Path) of
	{ok, Files} -> 
	    xref_files(lists:sort(Files), Path, S);
	Error ->
	    exref_io:warning(Path, unknown, "Could not stat~n", [], S),
	    {false, S}
    end.


%%
%% Do modules Mods in directory Dir
%%

xref_directory_module(Dir, Mods, S) ->
    {_OK, S1} = xref_dir_mod(Mods, Dir, S),
    xref_auto(S1).

%%
%% Do all auto included modules
%%
xref_auto(S) ->
    case lists:member(auto, S#state.options) of
	false ->
	    {true, S};
	true -> 
	    case S#state.auto of
		[] ->
		    {true, S};
		Auto ->
		    S1 = S#state{auto = []},
		    xref_module(Auto, S1)
	    end
    end.

%%
%% Ref modules in a directory
%%
xref_dir_mod([M | Ms], Dir, S) when atom(M) ->
    {_OK, S1} = xref_file(Dir, M, S),
    xref_dir_mod(Ms, Dir, S1);
xref_dir_mod(M, Dir, S) when atom(M) ->
    xref_file(Dir, M, S);
xref_dir_mod([], _, S) ->
    {true, S}.

%%
%% Ref files in a directory
%%
xref_files([File|Fs], Path, S) ->
    case filename:extension(File) == ".erl" of
	true ->
	    Mod = list_to_atom(filename:basename(File, ".erl")),
	    {_OK, S1} = xref_file(Path, Mod, S),
	    xref_files(Fs, Path, S1);
	false ->
	    case lists:member(recursive, S#state.options) of
		true ->
		    Dir = filename:join(Path, File),
		    case file:read_file_info(Dir) of
			{ok, FileInfo} ->
			    case FileInfo#file_info.type of
				directory ->
				    case FileInfo#file_info.access of
					read ->
					    {_OK, S1} = xref_dir(Dir, S),
					    xref_files(Fs, Path, S1);
					read_write ->
					    {_OK, S1} = xref_dir(Dir, S),
					    xref_files(Fs, Path, S1);
					_ ->
					    xref_files(Fs, Path, S)
				    end;
				_ ->
				    xref_files(Fs, Path, S)
			    end;
			{error, Reason} ->
			    exref_io:warning(Dir,
				    "",
				    "Not accessible. Reason: ~p.~n",
				    [Reason],
				    S),
			    xref_files(Fs, Path, S)
		    end;
		false ->
		    xref_files(Fs, Path, S)
	    end
    end;
xref_files([], _, S) -> 
    {true, S}.

%%
%% Open a erlang file and load it into to call graph
%%
xref_file(Dir, Mod, S) ->
    case include_module(Mod, S) of
	false ->
	    {true, S};
	true ->
	    case find_file(Dir, Mod, S) of
		{ok, AbsPathFile, AbsPath} ->
		    S1 = S#state{paths = [{AbsPath, Mod} | S#state.paths]},
		    exref_io:verbose("exref: ~s~n", [AbsPathFile], S1),
		    case epp:open(AbsPathFile,
				  [AbsPath | S1#state.includes],
				  S1#state.defs) of
			{ok, Fd} ->
			    S2 = S1#state{module = "",
					  file = "",
					  imports = [],
					  exports = []},
			    {OK, S3} = xref_fd(Fd,S2),

			    insert_def(S3, module_info, 0, {exported,compiler}, 0),
			    insert_def(S3, module_info, 1, {exported,compiler}, 0),
			    insert_def(S3, record_index, 2, {exported,compiler}, 0),
			    insert_def(S3, record_info, 2, {exported,compiler}, 0),

			    epp:close(Fd),
			    S4 = S3#state{module = undefined},
			    {OK, S4};
			Error ->
			    exref_io:warning(AbsPathFile,
				    "", 
				    "Could not access~n",
				    [],
				    S1),
			    {false, S1}
		    end;
		Error ->
		    exref_io:warning(lists:concat([Mod,".erl"]),
			    "", 
			    "File not found~n",
			    [],
			    S),
		    {false, S}
	    end
    end.


%%
%% Do cross reference
%%
xref_fd(Fd,S) ->
    case epp:parse_erl_form(Fd) of
	{ok, F} ->
	    xref_form(F,Fd,S);
	{eof,_} ->
	    {true, S};
	{error, Why} ->
	    exref_io:report_error(Why, S),
	    S1 = case S#state.module of
		     undefined ->
			 S;
		     M ->
			 STmp = S#state{failed = [M | S#state.failed]},
			 Opts = S#state.options,
			 STmp2 = STmp#state{options =
					    lists:delete(verbose, Opts)},
			 STmp3 = del_module(M, STmp2),
			 STmp3#state{options = Opts}
	    end,
	    {false, S1}
    end.

%%
%% Handle forms
%%
xref_form({function,Line,F,A,Cs},Fd,S) ->
    V = insert_def(S, F, A, Line),
    {_OK, S1} = xref_clauses(Cs, {S,V}),
    xref_fd(Fd,S1);

xref_form({attribute,Line,record,{Name,Inits}},Fd,S) ->
    V = insert_def(S, Name,0,record,Line),
    {_OK, S1} = xref_seq(Inits, {S,V}),
    xref_fd(Fd,S1);
	
xref_form({attribute,_,K,V},Fd,S) ->
    case K of
	file ->
	    {File,_} = V,
	    S1 = S#state{file = File},
	    xref_fd(Fd,S1);
	module ->
	    S1 = S#state{module = V, modules = [V | S#state.modules]},
	    xref_fd(Fd,S1);
	export ->
	    case S#state.exports of
		all ->
		    xref_fd(Fd, S);
		Exp ->
		    S1 = S#state{exports = add_funcs(V,S#state.module, Exp)},
		    xref_fd(Fd,S1)
	    end;
	import ->
	    {Mod,Fs} = V,
	    S1 = S#state{imports = add_funcs(Fs, Mod, S#state.imports)},
	    xref_fd(Fd,S1);
	compile ->
	    case V of
		export_all ->
		    S1 = S#state{exports = all},
		    xref_fd(Fd, S1);
		[export_all] ->
		    S1 = S#state{exports = all},
		    xref_fd(Fd, S1);
		_ ->
		    xref_fd(Fd, S)
	    end;
	_ ->
	    xref_fd(Fd, S)
    end;
xref_form(_, Fd, S) ->
    xref_fd(Fd, S).



%%
%% Extract function calls from a ERLANG form
%%
xref_clauses([{clause,_,P,G,B} | Cs], {S, V}) ->
    {_OK, S1} = xref_seq(G, {S, V}),
    {_Ok, S2} = xref_seq(B, {S1, V}),
    xref_clauses(Cs, {S2, V});
xref_clauses([], {S, V}) ->
    {true, S}.


%% Allow lists of lists; guard disjunctions have that form
xref_seq([E|Es], {S, V}) when list(E) ->
    {_OK, S1} = xref_seq(E, {S, V}), 
    xref_seq(Es, {S1, V});
xref_seq([E|Es], {S, V}) ->
    {_OK, S1} = xref_expr(E, {S, V}), 
    xref_seq(Es, {S1, V});
xref_seq([], {S, V}) ->
    {true, S}.


xref_if_clauses([{clause,_,[],G,B}|Cs], {S, V}) ->
    {_OK, S1} = xref_seq(G, {S, V}),
    {_Ok, S2} = xref_seq(B, {S1, V}),
    xref_if_clauses(Cs, {S2, V});
xref_if_clauses([], {S, V}) ->
    {true, S}.


xref_expr({cons,_,H,T}, {S, V}) ->
    {_OK, S1} = xref_expr(H, {S, V}), 
    xref_expr(T, {S1, V});
xref_expr({tuple,_,Es}, SV) -> 
    xref_seq(Es, SV);
xref_expr({arith,_,Op,L,R}, {S, V}) ->
    {_OK, S1} = xref_expr(L, {S, V}),
    xref_expr(R, {S1, V});
xref_expr({arith,_,Op,A}, SV) ->
    xref_expr(A, SV);
xref_expr({call,Line,{atom,_,F},As}, {S,V}) -> 
    A = length(As),
    case exref_internal:internal(F, A) of
	true ->
	    {_OK, S1} = bif(F, As, Line, {S, V}),
	    xref_seq(As, {S1, V});
	false ->
	    case lists:keysearch({F,A}, 1, S#state.imports) of
		false ->
		    {_OK, S1} = insert_call(V, {S#state.module,F,A}, Line, S),
		    xref_seq(As, {S1, V});
		{value,{{F,A},M}} ->
		    {_OK, S1} = insert_call(V, {M,F,A}, Line, S),
		    xref_seq(As, {S1, V})
	    end
    end;
xref_expr({call,Line,{remote,_,{atom,_,erlang},{_,_,F}},As}, SV) ->
    bif(F, As, Line, SV);
xref_expr({call,Line,{remote,_,{atom,_,M},{atom,_,F}},As}, {S,V}) ->
    {_OK, S1} = insert_call(V, {M,F,length(As)}, Line, S),
    xref_seq(As, {S1, V});
xref_expr({call,Line,{remote,_,ModuleExpr,{atom,_,F}},As}, {S,V}) ->
    {_OK, S1} = xref_expr(ModuleExpr,{S,V}),
    exref_io:warning(filename:basename(S1#state.file),
		     Line,
		     "variable module",
		     [],
		     S1),
    {_Ok, S2} = insert_call(V, {'$M_EXPR',F,length(As)}, Line, S1),
    xref_seq(As, {S2, V});
xref_expr({call,Line,{remote,_,{atom,_,M},FunctionExpr},As}, {S,V}) ->
    {_OK, S1} = xref_expr(FunctionExpr,{S,V}),
    exref_io:warning(filename:basename(S1#state.file),
		     Line,
		     "variable function",
		     [],
		     S1),
    {_Ok, S2} = insert_call(V, {M,'$F_EXPR',length(As)}, Line, S1),
    xref_seq(As, {S2, V});
xref_expr({call,Line,{remote,_,ModuleExpr,FunctionExpr},As}, {S,V}) ->
    {_OK, S1} = xref_expr(ModuleExpr,{S,V}),
    {_Ok, S2} = xref_expr(FunctionExpr,{S1, V}),
    exref_io:warning(filename:basename(S2#state.file),
		     Line,
		     "variable module and function",
		     [],
		     S2),
    {_Okk, S3} = insert_call(V, {'$M_EXPR','$F_EXPR',length(As)}, Line, S2),
    xref_seq(As, {S3, V});
xref_expr({call,Line,MaybeFun,As}, {S, V}) ->
    {_OK, S1} = xref_expr(MaybeFun, {S, V}),
    xref_seq(As, {S1, V});
xref_expr({match,_,Lhs,Rhs}, SV) ->
    xref_expr(Rhs, SV);
xref_expr({block,_,Es}, SV) -> 
    xref_seq(Es, SV);
xref_expr({'if',_,Cs}, SV) -> 
    xref_if_clauses(Cs, SV);
xref_expr({'case',_,E,Cs}, {S, V}) -> 
    {_OK, S1} = xref_expr(E, {S, V}),
    xref_clauses(Cs, {S1, V});
xref_expr({'receive',_,Cs}, SV) -> 
    xref_clauses(Cs, SV);
xref_expr({'receive',_,Cs,TX,TB}, {S, V}) -> 
    {_OK, S1} = xref_expr(TX, {S, V}), 
    {_Ok, S2} = xref_seq(TB, {S1, V}), 
    xref_clauses(Cs, {S2, V});
xref_expr({send,_,T,M}, {S, V}) ->
    {_OK, S1} = xref_expr(T, {S, V}), 
    xref_expr(M, {S1, V});
xref_expr({'catch', _, E}, SV) -> 
    xref_expr(E, SV);
xref_expr({atom,_,_}, {S, V}) ->
    {true, S};
xref_expr({integer,_,_}, {S, V}) ->
    {true, S};
xref_expr({float,_,_}, {S, V}) ->
    {true, S};
xref_expr({string,_,_}, {S, V}) ->
    {true, S};
xref_expr({var,_,_}, {S, V}) ->
    {true, S};
xref_expr({nil,_}, {S, V}) ->
    {true, S};
xref_expr({record_init,_,_,_}, {S, V}) ->
    {true, S};
xref_expr({record,_,_,Record_tuple}, SV) when list(Record_tuple) ->
	xref_seq(Record_tuple, SV);
xref_expr({record,_,Expr,_,Record_tuple}, {S, V}) when list(Record_tuple) ->
    {_OK, S1} = xref_expr(Expr, {S, V}),
    xref_seq(Record_tuple, {S1, V});
xref_expr({record_field,_,_}, {S, V}) -> % declaration of field with no default
    {true, S};
xref_expr({record_field,_,_,MaybeCall}, SV) -> 
    xref_expr(MaybeCall, SV);
xref_expr({record_field,_,_,_,_}, {S, V}) ->
    {true, S};
xref_expr({record_index,_,_,_Expr}, {S, V}) ->  % _Expr but not now
    {true, S};

xref_expr({op,_,_,E1,E2}, {S, V}) -> 
	{_OK, S1} = xref_expr(E1, {S, V}),
	xref_expr(E2, {S1, V});
xref_expr({op,_,_,MaybeCall}, SV) -> 
	xref_expr(MaybeCall, SV);
xref_expr({lc,_,E1,E2}, {S, V}) ->
	{_OK, S1} = xref_expr(E1, {S, V}),
	xref_seq(E2, {S1, V});
xref_expr({generate,_,E1,E2}, {S, V}) ->
	{_OK, S1} = xref_expr(E1, {S, V}),
	xref_expr(E2, {S1, V});
xref_expr({'fun',_,{clauses,Clauses}}, SV) ->
	xref_clauses(Clauses, SV);
xref_expr({'fun',Line,{function,F,A}}, {S,V}) ->
    insert_call(V, {S#state.module,F,A}, Line, S);
xref_expr({'query',_Line, Es}, SV) ->
    xref_expr(Es, SV);
xref_expr(X, {S, V}) ->
    exref_io:warning(S#state.file,
		     unknown,
		     "*** strange abstract form: ~p~n",
		     [X],
		     S),
    {true, S}.








bif(apply, As, Line, SV) -> mfa(As, apply, Line, SV);
bif(spawn, As, Line, SV) -> mfa(As, spawn, Line, SV);
bif(spawn_link, As, Line, SV) -> mfa(As, spawn_link, Line, SV);
bif(Name, As, Line, {S, V}) ->
    {_OK, S1} = insert_call(V,{erlang,Name,length(As)},Line,S),
    xref_seq(As, {S1, V}).



mfa(As, Bif, Line, {S,V}) ->
    A = length(As),
    case As of
	[_,{atom,_,M},{atom,_,F},Args]  ->
	    {_OK, S1} = args_mfa(Args,Bif,{M,F,A},Line,{S,V}),
	    xref_seq(As, {S1, V});
	[{atom,_,M},{atom,_,F},Args] ->
	    {_OK, S1} = args_mfa(Args,Bif,{M,F,A},Line,{S,V}),
	    xref_seq(As, {S1, V});
	[{tuple,[{atom,_,M},{atom,_,F}]},Args] ->
	    {_OK, S1} = args_mfa(Args,Bif,{M,F,A},Line,{S,V}),
	    xref_seq(As, {S1, V});
	_ ->
	    exref_io:warning(filename:basename(S#state.file),
			     Line,
			     "variable ~w",
			     [Bif],
			     S),
	    {_OK, S1} = insert_call(V,{erlang,Bif,A},Line,S),
	    xref_seq(As, {S1,V})
    end.


args_mfa({nil,_},Bif,{M,F,_},Line,{S,V}) ->
    insert_call(V,{M,F,0},Line,S);
args_mfa({cons,_,_,T},Bif,{M,F,A},Line,{S,V}) ->
    case cons_length(T) of
	-1 -> 
	    exref_io:warning(filename:basename(S#state.file),
			     Line,
			     "variable args ~w",
			     [Bif],
			     S),
	    insert_call(V,{erlang,Bif,A},Line,S);
	N ->
	    insert_call(V,{M,F,N+1},Line,S)
    end;
args_mfa(_,Bif,{M,F,A},Line,{S,V}) ->
    exref_io:warning(filename:basename(S#state.file),
		     Line,
		     "variable args ~w",
		     [Bif],
		     S),
    insert_call(V,{erlang,Bif,A},Line,S).



cons_length({cons,_,_,T}) ->
    case cons_length(T) of
	-1 -> 
	    -1;
	N  ->
	    N+1
    end;
cons_length({nil,_}) ->
    0;
cons_length(_) -> 
    -1.



%%
%% Create a vertex in the call graph
%%

insert_def(S, F, A, Line) ->
    insert_def(S, F, A, false, Line).

insert_def(S, F, A, ExtraFlag, Line) ->
    TempFlag = case S#state.exports of
	       all -> exported;
	       Exports -> 
		   case lists:keysearch({F,A}, 1, Exports) of
		       false -> local;
		       _ -> exported
		   end
	   end,
    Flag = case ExtraFlag of
	       false -> TempFlag;
	       {_,_}-> ExtraFlag;
	       _ -> {TempFlag,ExtraFlag}
	   end,
    digraph:add_vertex(S#state.fgraph,
		       {S#state.module,F,A},
		       {Flag,filename:basename(S#state.file),
		       Line}).
		       
insert_use(S, MFA) ->
    {_OK, S1} = add_auto(element(1,MFA), S),
    case digraph:vertex(S1#state.fgraph, MFA) of
	false ->
	    {digraph:add_vertex(S#state.fgraph, MFA), S1};
	_ -> {MFA, S1}
    end.

%%
%%  Insert a call into the function call graph
%%
insert_call(V, MFA, Line, S) ->
    {W, S1} = insert_use(S, MFA),
    digraph:add_edge(S1#state.fgraph, V, W, Line),
    {insert_module(S1, element(1, V), element(1,W)), S1}.

%%
%% Insert module module dependencies in the module graph
%%

insert_module(S, M1, M2) ->
    digraph:add_vertex(S#state.mgraph, M1, S#state.file),
    case M1 of 
	M2 ->
	    true; % don't register explicit calls to own module
	_ ->
	    digraph:add_vertex(S#state.mgraph, M2),
	    digraph:add_edge(S#state.mgraph, {M1,M2}, M1, M2, []),
	    true
    end.
    

%%
%% Add list of {Fun,Arity} to a list of {{Fun,Arity},Module}
%% or all
%%
add_funcs([{Fun,Arity}|Es], Module, Funs) ->
    add_funcs(Es, Module, [{{Fun,Arity},Module}|Funs]);
add_funcs([], _, Funs) ->  Funs.

%%
%% Add module to list of modules to auto include
%%
add_auto('$M_EXPR', S) ->
    {true, S};
add_auto(Module, S) ->
    case lists:member(auto, S#state.options) of
	false ->
	    {false, S};
	_ -> 
	    case lists:member(Module, S#state.modules) of
		true ->
		    {true, S};
		false ->
		    Auto = S#state.auto,
		    case lists:member(Module, Auto) of
			true ->
			    {true, S};
			false ->
			    S1 = S#state{auto = [Module | Auto]},
			    {true, S1}
		    end
	    end
    end.

%%
%% Check if module should be loaded.
%% Returns:
%% true | false
%%   false if:
%%     Mod is already loaded |
%%     we already tried to load Mod, but failed.
%%     Mod is in the excludes list |
%%     the no_lib option is set and Mod is a lib |
%%
include_module(Mod, S) ->
    case lists:member(Mod, S#state.modules) of
	true ->    % Mod already loaded
	    false;
	false ->   % Mod not loaded.
	    case lists:member(Mod, S#state.failed) of
		true ->    % We already failed to load Mod.
		    false;
		false ->   % We haven't failed to load Mod previously.
		    case lists:member(Mod, S#state.excludes) of
			true ->    % Mod is excluded
			    false;

			false ->   % Mod is not excluded (nor loaded/failed).
			    case lists:member(no_libs, S#state.options) of
				true ->     % Don't load libs. Check if Mod is a lib.
				    case is_lib(Mod) of
					true ->    % Mod is a lib (but no_libs).
					    false;
					false ->   % Mod is not a lib
					    true
				    end;
				false ->    % Don't care if Mod is a lib.
				    true
			    end

		    end
	    end
    end.

%%
%% Check if a module is a lib.
%% Returns:
%%    true | false
%%
%%
is_lib(M) ->
    case code:which(M) of
	non_existing ->
	    false;
	preloaded ->
	    true;
	File ->
	    lists:prefix(code:lib_dir(), File)
    end.




%%
%% delete module(s)
%%
% Takes a single module (atom) or a list of modules and a state record.
% 
% Returns a state (S or S1).
% If the call to xref_module fails it throws an exception, which is not
% caught here.
%
del_module([M|Ms], S) when atom(M) ->
    CurrentModules = S#state.modules,     % FutureModules = CurrentModules - [M|Ms].
    FutureModules = [ X || X <- CurrentModules, lists:member(X, [M|Ms]) == false],

    case FutureModules /= CurrentModules of
	true ->        % The new list of modules is different from the old.
                       % Clear state.modules, clear the digraphs,
                       % clear the paths of the deleted modules from the
                       % state, and reanalyse.
                       % Return the new state.
	    S1 = S#state{modules = []},
	    {_OK, S2} = delete_paths([M|Ms], S1),

	    S3 = S2#state{fgraph=digraph:new(), mgraph=digraph:new()},
	    digraph:delete(S#state.fgraph),
	    digraph:delete(S#state.mgraph),

	    {_Ok, S4} = xref_module(FutureModules, S3),
	    S4;

	false ->    % FutureModules == CurrentModules: return old state.
	    S
    end;
del_module([], S) -> 
    S;
del_module(M, S) when atom(M) ->
    del_module([M], S);
del_module([], S) -> 
    S.

delete_paths([M|Ms], S) ->
    S1 = S#state{paths = lists:keydelete(M, 2, S#state.paths)},
    delete_paths(Ms, S1);
delete_paths([], S) ->
    {true, S}.






%%
%% Search for files
%%

% A file may be searched for in the directorys specified in the Erlang
% environment (returned by code:get_path()), or among the files already
% loaded (stored in field paths of the state S). The latter case occurs
% when delete_module/1 is called. This call causes all modules to be
% deleted whereafter some are reloaded.
% If the search option is not set, search for the module only among those
% already loaded. If the search option is set, allow directorys found by
% code:get_path() as well.

% find_file(Dir, Module)
% Dir: directory name, atom
% Module: module name, string
% Returns: {ok, FileName, AbsolutePath}
% FileName: Absolute path and name of file, string
% AbsolutePath: Absolute path, string
%
find_file("", Module, S) ->
    case lists:member(search, S#state.options) of
	true ->
	    search_file(lists:append([S#state.paths, code:get_path()]), Module);
	false ->
	    search_file(S#state.paths, Module)
    end;
find_file(Dir, Module, S) ->
    access_file(filename:absname(Dir), Module).

%%
%% Scan Dir list for file F
%%
% The first 2 clauses handle the situation where the module should be
% reloaded after a delete_module operation on another module (all modules
% are deleted and some reloaded). The module is then found among the
% {AbsPath, Module} entries emanating from the process dictionary (paths).
% The 3rd clause handles the case when the module should be loaded fresh.
%
search_file([{AbsPath, Module}|Paths], Module) ->
    Dir = source_dir(AbsPath),
    case access_file(Dir, Module) of
	{ok, File, Dir} ->
	    {ok, File, Dir};
	Error ->
	    search_file(Paths, Module)
    end;
search_file([{_AbsPath, Mod}|Paths], Module) ->
    search_file(Paths, Module);
search_file([D|Ds], Module) ->
    Dir = source_dir(D),
    case access_file(Dir, Module) of
	{ok, File, Dir} -> {ok, File, Dir};
	Error -> search_file(Ds, Module)
    end;
search_file([], _) -> {error, search_file}.


%%
%% Check if file exists, is regular and may be read
%%
% The absolute path, AbsPath, must be known.
access_file(AbsPath, Module) when atom(Module)->
    Module1 = lists:concat([Module, ".erl"]),
    File = filename:join(AbsPath, Module1),

    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    case FileInfo#file_info.type of
		regular ->
		    case FileInfo#file_info.access of
			read ->
			    {ok,File,AbsPath};
			read_write ->
			    {ok,File,AbsPath};
			_Other ->
			    {error, access_file}
		    end;
		_Other ->
		    {error, access_file}
	    end;
	{error, _} ->
	    {error, access_file}
    end;
access_file(AbsPath, Module) when list(Module) ->
    File = filename:join(AbsPath, lists:concat([Module, ".erl"])),

    case file:read_file_info(File) of
	{ok, FileInfo} ->
	    case FileInfo#file_info.type of
		regular ->
		    case FileInfo#file_info.access of
			read ->
			    {ok,File,AbsPath};
			read_write ->
			    {ok,File,AbsPath};
			_Other ->
			    {error, access_file}
		    end;
		_Other ->
		    {error, access_file}
	    end;
	{error, _} ->
	    {error, access_file}
    end;
access_file(AbsPath, Module) ->
    {error, access_file}.


%%
%% Convert a ebin directory to src directory
%%
source_dir(D) ->
    Dir = if 
	      atom(D) -> atom_to_list(D);
	      true    -> D
    end,
    case filename:basename(Dir) == "ebin" of
	false ->
	    Dir;
	true ->
	    filename:join(filename:rootname(Dir, "ebin"), "src")
    end.


