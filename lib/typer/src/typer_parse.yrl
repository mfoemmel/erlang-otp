Nonterminals 
top_type top_types type record_field record_fields.

Terminals 
atom integer
'(' ')' '[' ']' '{' '}' '<' '>'
',' '|' '.' '=' '#' '->'.
 
Rootsymbol top_type .
Endsymbol dot.

Left 100 '|'.

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> type                          : '$1'.
top_type -> type '|' top_type             : lift_unions('$1','$3').

type -> atom                              : {type, atom, [get_atom('$1')]}.
type -> atom '(' ')'                      : build_type('$1', []).
type -> atom '(' top_type ')'             : build_type('$1', ['$3']).
type -> atom '(' top_type ',' top_type ')': build_type('$1', ['$3', '$5']).
type -> '[' ']'                           : {type, nil, []}.
type -> '[' top_type ']'                  : {type, list, ['$2']}.
type -> '[' top_type ',' '.' '.' '.' ']'  : {type, nonempty_list, ['$2']}.
type -> '(' '(' ')' '->' top_type ')'     : {type, 'fun', [[], '$5']}.
type -> '(' '(' top_types ')' '->' top_type ')' : {type, 'fun', ['$3', '$6']}.
type -> '{' '}'                           : {type, tuple, []}.
type -> '{' top_types '}'                 : {type, tuple, '$2'}.
type -> '#' atom '{' '}'                  : {type, record, [get_atom('$2')]}.
type -> '#' atom '{' record_fields '}'  : {type, record, [get_atom('$2')|'$4']}.
type -> integer                           : {type, integer, [get_int('$1')]}.
type -> '<' top_types '>'                 : {type, product, '$2'}.
type -> '(' integer '.' '.' integer ')'   : 
	  {type, range, [get_int('$2'), get_int('$5')]}.

record_fields -> record_field                   : ['$1'].
record_fields -> record_field ',' record_fields : ['$1'|'$3'].

record_field -> atom '=' top_type               : {get_atom('$1'), '$3'}.

Erlang code.

build_type({atom,_,any}, []) -> {type, any, []};
build_type({atom,_,atom}, []) -> {type, atom, []};
build_type({atom,_,binary}, []) -> {type, binary, []};
build_type({atom,_,bool}, []) -> {type, bool, []};
build_type({atom,_,byte}, []) -> {type, byte, []};
build_type({atom,_,char}, []) -> {type, char, []};
build_type({atom,_,float}, []) -> {type, float, []};
build_type({atom,_,function}, []) -> {type, 'fun', []};
build_type({atom,_,identifier}, []) -> {type, identifier, []};
build_type({atom,_,integer}, []) -> {type, integer, []};
build_type({atom,_,list}, []) -> {type, list, []};
build_type({atom,_,mfa}, []) -> {type, mfa, []};
build_type({atom,_,neg_integer}, []) -> {type, neg_integer, []};
build_type({atom,_,non_neg_integer}, []) -> {type, non_neg_integer, []};
build_type({atom,_,none}, []) -> {type, none, []};
build_type({atom,_,nonempty_list}, [C,T]) -> {type, cons, [C,T]};
build_type({atom,_,nonempty_possibly_improper_list}, []) -> 
    {type, cons, []};
build_type({atom,_,nonempty_posssibly_improper_list}, [C,T]) -> 
    {type, cons, [C, T]};
build_type({atom,_,number}, []) -> {type, number, []};
build_type({atom,_,pid}, []) -> {type, pid, []};
build_type({atom,_,port}, []) -> {type, port, []};
build_type({atom,_,pos_integer}, []) -> {type, pos_integer, []};
build_type({atom,_,possibly_improper_list}, [C,T]) -> 
    {type, pos_improper_list, [C,T]};
build_type({atom,_,possibly_improper_list}, []) -> 
    {type, pos_improper_list, []};
build_type({atom,_,ref}, []) -> {type, ref, []};
build_type({atom,_,string}, []) -> {type, string, []};
build_type({atom,_,tuple}, []) -> {type, tuple, []};
build_type({atom,La,_}, _) -> error_bad_decl(La,type).

lift_unions(T1, {type, union, List}) ->
    {type, union, [T1|List]};
lift_unions(T1, T2 = {type, _, _}) ->
    {type, union, [T1, T2]}.

get_atom({atom, _, Atom}) -> Atom.

get_int({integer, _, Integer}) -> Integer.

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).
