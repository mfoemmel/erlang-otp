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
%% Purpose : Maintain atom, import, export tables, and other tables for assembler.

-module(beam_dict).

-export([new/0,opcode/2,highest_opcode/1,
	 atom/2,local/4,export/4,import/4,
	 string/2,lambda/5,literal/2,
	 atom_table/1,local_table/1,export_table/1,import_table/1,
	 string_table/1,lambda_table/1,literal_table/1]).

-import(lists, [map/2]).

-record(asm,
	{atoms = gb_trees:empty(),		%{Atom,Index}
	 exports = [],				%[{F, A, Label}]
	 locals = [],				%[{F, A, Label}]
	 imports = gb_trees:empty(),		%{{M,F,A},Index}
	 strings = [],				%String pool
	 lambdas = [],				%[{...}]
	 literals = dict:new(),			%Format: {Literal,Number}
	 next_atom = 1,
	 next_import = 0,
	 string_offset = 0,
	 next_literal = 0,			%Number of next literal
	 highest_opcode = 0
	}).

new() ->
    #asm{}.

%% Remember the highest opcode.

opcode(Op, Dict) when Dict#asm.highest_opcode > Op -> Dict;
opcode(Op, Dict) -> Dict#asm{highest_opcode=Op}.

%% Returns the highest opcode encountered.

highest_opcode(#asm{highest_opcode=Op}) -> Op.

%% Returns the index for an atom (adding it to the atom table if necessary).
%%    atom(Atom, Dict) -> {Index,Dict'}

atom(Atom, #asm{atoms=Atoms0,next_atom=NextIndex}=Dict) when is_atom(Atom) ->
    case gb_trees:lookup(Atom, Atoms0) of
	{value,Index} ->
	    {Index,Dict};
	none ->
	    Atoms = gb_trees:insert(Atom, NextIndex, Atoms0),
	    {NextIndex,Dict#asm{atoms=Atoms,next_atom=NextIndex+1}}
    end.

%% Remembers an exported function.
%%    export(Func, Arity, Label, Dict) -> Dict'

export(Func, Arity, Label, Dict0) when is_atom(Func),
				       is_integer(Arity),
				       is_integer(Label) ->
    {Index, Dict1} = atom(Func, Dict0),
    Dict1#asm{exports = [{Index, Arity, Label}| Dict1#asm.exports]}.

%% Remembers a local function.
%%    local(Func, Arity, Label, Dict) -> Dict'

local(Func, Arity, Label, Dict0) when is_atom(Func),
				      is_integer(Arity),
				      is_integer(Label) ->
    {Index,Dict1} = atom(Func, Dict0),
    Dict1#asm{locals=[{Index,Arity,Label}|Dict1#asm.locals]}.

%% Returns the index for an import entry (adding it to the import table if necessary).
%%    import(Mod, Func, Arity, Dict) -> {Index,Dict'}

import(Mod0, Name0, Arity, #asm{imports=Imp0,next_import=NextIndex}=D0)
  when is_atom(Mod0), is_atom(Name0), is_integer(Arity) ->
    {Mod,D1} = atom(Mod0, D0),
    {Name,D2} = atom(Name0, D1),
    MFA = {Mod,Name,Arity},
    case gb_trees:lookup(MFA, Imp0) of
	{value,Index} ->
	    {Index,D2};
	none ->
	    Imp = gb_trees:insert(MFA, NextIndex, Imp0),
	    {NextIndex,D2#asm{imports=Imp,next_import=NextIndex+1}}
    end.

%% Returns the index for a string in the string table (adding the string to the
%% table if necessary).
%%    string(String, Dict) -> {Offset, Dict'}

string(Str, Dict) when is_list(Str) ->
    #asm{strings=Strings,string_offset=NextOffset} = Dict,
    case old_string(Str, Strings) of
	none ->
	    NewDict = Dict#asm{strings=Strings++Str,
			       string_offset=NextOffset+length(Str)},
	    {NextOffset,NewDict};
	Offset when is_integer(Offset) ->
	    {NextOffset-Offset,Dict}
    end.

%% Returns the index for a funentry (adding it to the table if necessary).
%%    lambda(Dict, Lbl, Index, Uniq, NumFree) -> {Index,Dict'}

lambda(Lbl, Index, OldUniq, NumFree, #asm{lambdas=Lambdas0}=Dict) ->
    OldIndex = length(Lambdas0),
    Lambdas = [{Lbl,{OldIndex,Lbl,Index,NumFree,OldUniq}}|Lambdas0],
    {OldIndex,Dict#asm{lambdas=Lambdas}}.

%% Returns the index for a a literal (adding it to the atom table if necessary).
%%    literal(Literal, Dict) -> {Index,Dict'}

literal(Lit, #asm{literals=Tab0,next_literal=NextIndex}=Dict) ->
    case dict:find(Lit, Tab0) of
	{ok,Index} ->
	    {Index,Dict};
	error ->
	    Tab = dict:store(Lit, NextIndex, Tab0),
	    {NextIndex,Dict#asm{literals=Tab,next_literal=NextIndex+1}}
    end.


%% Returns the atom table.
%%    atom_table(Dict) -> {LastIndex,[Length,AtomString...]}

atom_table(#asm{atoms=Atoms,next_atom=NumAtoms}) ->
    Sorted = lists:keysort(2, gb_trees:to_list(Atoms)),
    Fun = fun({A,_}) ->
		  L = atom_to_list(A),
		  [length(L)|L]
	  end,
    AtomTab = map(Fun, Sorted),
    {NumAtoms-1,AtomTab}.

%% Returns the table of local functions.
%%    local_table(Dict) -> {NumLocals,[{Function, Arity, Label}...]}

local_table(#asm{locals = Locals}) ->
    {length(Locals),Locals}.

%% Returns the export table.
%%    export_table(Dict) -> {NumExports,[{Function, Arity, Label}...]}

export_table(#asm{exports = Exports}) ->
    {length(Exports),Exports}.

%% Returns the import table.
%%    import_table(Dict) -> {NumImports, [{Module, Function, Arity}...]}

import_table(#asm{imports=Imp,next_import=NumImports}) ->
    Sorted = lists:keysort(2, gb_trees:to_list(Imp)),
    ImpTab = [MFA || {MFA,_} <- Sorted],
    {NumImports,ImpTab}.

string_table(#asm{strings=Strings,string_offset=Size}) ->
    {Size,Strings}.

lambda_table(#asm{locals=Loc0,lambdas=Lambdas0}) ->
    Lambdas1 = sofs:relation(Lambdas0),
    Loc = sofs:relation([{Lbl,{F,A}} || {F,A,Lbl} <- Loc0]),
    Lambdas2 = sofs:relative_product1(Lambdas1, Loc),
    Lambdas = [<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32>> ||
		  {{_,Lbl,Index,NumFree,OldUniq},{F,A}} <- sofs:to_external(Lambdas2)],
    {length(Lambdas),Lambdas}.

%% Returns the literal table.
%%  literal_table(Dict) -> {NumLiterals,[TermSize,TermInExternalFormat...]}

literal_table(#asm{literals=Tab,next_literal=NumLiterals}) ->
    L0 = dict:fold(fun(Lit, Num, Acc) ->
			   [{Num,my_term_to_binary(Lit)}|Acc]
		   end, [], Tab),
    L1 = lists:sort(L0),
    L = [[<<(size(Term)):32>>,Term] || {_,Term} <- L1],
    {NumLiterals,L}.

my_term_to_binary(Term) ->
    term_to_binary(Term, [{minor_version,1}]).

%% Search for string Str in the string pool Pool.
%%   old_string(Str, Pool) -> none | Index

old_string([C|Str]=Str0, [C|Pool]) ->
    case lists:prefix(Str, Pool) of
	true -> length(Pool)+1;
	false -> old_string(Str0, Pool)
    end;
old_string([_|_]=Str, [_|Pool]) ->
    old_string(Str, Pool);
old_string([_|_], []) -> none.
