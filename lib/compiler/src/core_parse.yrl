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
%%     $Id: core_parse.yrl,v 1.1.1.1 1999/11/25 13:52:22 erlang Exp $
%%
%% Core Erlang YECC parser grammar


Nonterminals

module_definition module_export module_attribute module_body
attribute_list attribute
exported_names exported_name

function_definition function_definitions

constant constants atomic_constant tuple_constant cons_constant
tail_constant

pattern patterns real_pattern atomic_pattern tuple_pattern
cons_pattern tail_pattern

expressions expression real_expression literal atomic_literal nil
tuple cons tail values_expr

sequence let_expr case_expr fun_expr application
application_op local_function_reference remote_function_reference
internal_function_reference catch_expr try_expr receive_expr variable
variables clauses clause

annotation_list.


Terminals

%% Separators

'(' ')' '{' '}' '[' ']' '|' ',' '->' '=' '/' '<' '>' ':' '-|'

%% Keywords (atoms are assumed to always be single-quoted).

module export attributes fdef local remote internal do then 'let' in
'case' 'of' 'end' 'when' 'fun' 'try' 'catch' 'receive' 'after'

%% Literal tokens (provided by the tokeniser):

integer float atom char string var.

%% Literal tokens NOT provided by the tokenise:

nil -> '[' ']' : {nil,tok_line('$1')}. 

%% Declare the start rule for parsing

Rootsymbol module_definition.


%% Grammar

module_definition ->
    module atom module_export module_attribute module_body 'end' :
	#c_mdef{name=tok_val('$2'),exports='$3',attributes='$4',body='$5'}.
module_definition ->
    '(' module atom module_export module_attribute module_body 'end'
	'-|' annotation_list ')' :
	#c_mdef{anno='$9',
		name=tok_val('$3'),exports='$4',attributes='$5',body='$6'}.

module_export -> export '[' ']' : [].
module_export -> export '[' exported_names ']' : '$3'.

exported_names -> exported_name ',' exported_names : ['$1' | '$3'].
exported_names -> exported_name : ['$1'].

exported_name -> atom '/' integer : {tok_val('$1'),tok_val('$3')}.

module_attribute -> attributes '[' ']' : [].
module_attribute -> attributes '[' attribute_list ']' : '$3'.

attribute_list -> attribute ',' attribute_list : ['$1' | '$3'].
attribute_list -> attribute : ['$1'].

attribute -> atom '=' constant : {tok_val('$1'), '$3'}.

module_body ->
    function_definitions : '$1'.

annotation_list -> '[' ']' : [].
annotation_list -> '[' constants ']' : '$2'.

function_definitions ->
    function_definition function_definitions : ['$1' | '$2'].
function_definitions ->
    '$empty' : [].

function_definition ->
    fdef atom '/' integer '=' fun_expr :
	#c_fdef{func=tok_val('$2'),arity=tok_val('$4'),body='$6'}.
function_definition ->
    fdef atom '/' integer '=' '(' fun_expr '-|' annotation_list ')' :
	#c_fdef{anno='$9',func=tok_val('$2'),arity=tok_val('$4'),body='$7'}.

%% Constant terms for annotations and attributes.

constant -> atomic_constant : '$1'.
constant -> tuple_constant : '$1'.
constant -> cons_constant : '$1'.

constants -> constant ',' constants : ['$1' | '$3'].
constants -> constant : ['$1'].

atomic_constant -> integer : tok_val('$1').
atomic_constant -> float : tok_val('$1').
atomic_constant -> atom : tok_val('$1').
atomic_constant -> char : tok_val('$1').
atomic_constant -> string : tok_val('$1').
atomic_constant -> nil : [].

tuple_constant -> '{' '}' : {}.
tuple_constant -> '{' constants '}' : list_to_tuple('$2').

cons_constant -> '[' constant tail_constant : ['$2'|'$3'].

tail_constant -> ']' : [].
tail_constant -> '|' constant ']' : '$2'.
tail_constant -> ',' constant tail_constant : ['$2'|'$3'].

%% Patterns

pattern -> real_pattern : '$1'.
pattern -> '(' real_pattern '-|' annotation_list ')' :
	core_lib:set_anno('$2', '$4').

patterns -> pattern ',' patterns : ['$1' | '$3'].
patterns -> pattern : ['$1'].

real_pattern -> atomic_pattern : '$1'.
real_pattern -> var : #c_var{name=tok_val('$1')}.
real_pattern -> tuple_pattern : '$1'.
real_pattern -> cons_pattern : '$1'.
real_pattern -> var '=' pattern :
		    #c_alias{var=#c_var{name=tok_val('$1')},pat='$3'}.

atomic_pattern -> atomic_literal : '$1'.

tuple_pattern -> '{' '}' : #c_tuple{es=[]}.
tuple_pattern -> '{' patterns '}' : #c_tuple{es='$2'}.

cons_pattern -> '[' pattern tail_pattern :
		    #c_cons{head='$2',tail='$3'}.

tail_pattern -> ']' : #c_nil{}.
tail_pattern -> '|' pattern ']' : '$2'.
tail_pattern -> ',' pattern tail_pattern :
		    #c_cons{head='$2',tail='$3'}.

%% Expressions

expression -> real_expression : '$1'.
expression -> '(' real_expression '-|' annotation_list ')' :
	core_lib:set_anno('$2', '$4').

expressions -> expression ',' expressions : ['$1' | '$3'].
expressions -> expression : ['$1'].

real_expression -> literal : '$1'.
real_expression -> var : #c_var{name=tok_val('$1')}.
real_expression -> local_function_reference : '$1'.
real_expression -> values_expr : '$1'.
real_expression -> fun_expr : '$1'.
real_expression -> sequence : '$1'.
real_expression -> let_expr : '$1'.
real_expression -> case_expr : '$1'.
real_expression -> receive_expr : '$1'.
real_expression -> application : '$1'.
real_expression -> catch_expr : '$1'.
real_expression -> try_expr : '$1'.

literal -> atomic_literal : '$1'.
literal -> tuple : '$1'.
literal -> cons : '$1'.

atomic_literal -> integer : #c_int{val=tok_val('$1')}.
atomic_literal -> float : #c_float{val=tok_val('$1')}.
atomic_literal -> atom : #c_atom{name=tok_val('$1')}.
atomic_literal -> char : #c_char{val=tok_val('$1')}.
atomic_literal -> string : #c_string{val=tok_val('$1')}.
atomic_literal -> nil : #c_nil{}.

tuple -> '{' '}' : #c_tuple{es=[]}.
tuple -> '{' expressions '}' : #c_tuple{es='$2'}.

cons -> '[' expression tail : #c_cons{head='$2',tail='$3'}.

tail -> ']' : #c_nil{}.
tail -> '|' expression ']' : '$2'.
tail -> ',' expression tail : #c_cons{head='$2',tail='$3'}.

values_expr -> '<' '>' : #c_values{es=[]}.
values_expr -> '<' expressions '>' : #c_values{es='$2'}.

variable -> var : #c_var{name=tok_val('$1')}.
variable -> '(' var '-|' annotation_list ')' :
	#c_var{anno='$4',name=tok_val('$2')}.

local_function_reference ->
    local atom '/' integer : #c_local{name=tok_val('$2'),arity=tok_val('$4')}.

fun_expr -> 'fun' '(' ')' '->' expression :
	#c_fun{vars=[],body='$5'}.
fun_expr -> 'fun' '(' variables ')' '->' expression :
	#c_fun{vars='$3',body='$6'}.

sequence -> do expression then expression :
	#c_seq{arg='$2',body='$4'}.

let_expr -> 'let' '<' '>' '=' expression in expression :
	#c_let{vars=[],arg='$5',body='$7'}.
let_expr -> 'let' '<' variables '>' '=' expression in expression :
	#c_let{vars='$3',arg='$6',body='$8'}.

variables -> variable ',' variables : ['$1' | '$3'].
variables -> variable : ['$1'].

case_expr -> 'case' expression 'of' clauses 'end' :
	#c_case{arg='$2',clauses='$4'}.

clauses -> clause clauses : ['$1' | '$2'].
clauses -> clause : ['$1'].

clause -> '<' '>' 'when' expression '->' expression :
	#c_clause{pats=[],guard='$6',body='$6'}.
clause -> '<' patterns '>' 'when' expression '->' expression :
	#c_clause{pats='$2',guard='$5',body='$7'}.

receive_expr -> 'receive' clauses 'end' : 
	#c_receive{clauses='$2',
		   timeout=#c_atom{name=infinity},
		   action=#c_atom{name=true}}.
receive_expr -> 'receive' clauses 'after' expression '->' expression 'end' :
	#c_receive{clauses='$2',timeout='$4',action='$6'}.
receive_expr -> 'receive' 'after' expression '->' expression 'end' :
	#c_receive{clauses=[],timeout='$3',action='$5'}.
receive_expr -> 'receive' 'end' :
	#c_receive{clauses=[],
		   timeout=#c_atom{name=infinity},action=#c_atom{name=true}}.

application -> '(' application_op ')' '(' ')' :
	#c_call{op='$2',args=[]}.
application -> '(' application_op ')' '(' expressions ')' :
	#c_call{op='$2',args='$5'}.

application_op -> expression : '$1'.
application_op -> remote_function_reference : '$1'.
application_op -> internal_function_reference : '$1'.

remote_function_reference ->
    remote atom ':' atom '/' integer :
	#c_remote{mod=tok_val('$2'),name=tok_val('$4'),arity=tok_val('$6')}.

internal_function_reference ->
    internal atom '/' integer :
	#c_internal{name=tok_val('$2'),arity=tok_val('$4')}.

catch_expr -> 'catch' expression : #c_catch{body='$2'}. 

try_expr -> 'try' expression 'catch' '(' variable ',' variable ')' '->'
    expression :
	#c_try{expr='$2',vars=['$5','$7'],body='$10'}.

%% ====================================================================== %%


Erlang code.

-export([abstract/1,abstract/2,normalise/1]).

-include("core_parse.hrl").

tok_val(T) -> element(3, T).
tok_line(T) -> element(2, T).

abstract(T, N) -> abstract(T).

abstract(Term) -> core_lib:make_literal(Term).

normalise(Core) -> core_lib:literal_value(Core).
