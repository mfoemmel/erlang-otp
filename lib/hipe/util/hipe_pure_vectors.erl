%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%		       FAST DECLARATIVE VECTORS
%%
%% Non-destructive, slow vectors. (Updates are O(vec_size), lookups O(1))
%%
%% NOTE: Vectors start at index 1 !
%%   Should this be changed to 0-based indexing? (Better for n-dim arrays?)
%%
%% Operations:
%%   vector_to_list(Vec)
%%   list_to_vector(Xs)
%%   set(Vec,Index,Elt)
%%   get(Vec,Index)
%%   vsize(Vec): returns the size of the vector (integer)
%%   is_vector(Vec): returns 'true' or 'false'
%   verify_vector(Vec): returns 'true' or 'false'. Traverses the vector
%%       and checks that the data structure is valid (i.e., unbuggy :-)

-module(hipe_pure_vectors).
-compile(export_all).
-export([set/3,
	 set_all/3,
	 get/2,
	 get_all/2,
	 vsize/1,
	 is_vector/1,
	 verify_vector/1,
	 vector_to_list/1,
	 list_to_vector/1,
	 empty/1, 
	 empty/2,
	 init/1,
	 init/2,
	 list/1,
	 first_index/0
	]).

%% Vector representation: simply a tuple.
%% - this is a compatibility library for hipe's fast vectors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vsize(V) -> size(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

first_index() -> 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(Vec) ->
    add_indices(vector_to_list(Vec),1).

add_indices([],_N) -> [];
add_indices([X|Xs],N) ->
    [{N,X}|add_indices(Xs,N+1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_vector(X) -> is_vector(X).

is_vector(X) when is_tuple(X) -> true;
is_vector(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to_vector(Xs) ->
    list_to_tuple(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vector_to_list(V) ->
    tuple_to_list(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set(Vec,Ix,V) ->
    setelement(Ix,Vec,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_all(Vec,[],[]) -> Vec;
set_all(Vec,[Ix|Ixs],[V|Vs]) ->
    set_all(set(Vec,Ix,V),Ixs,Vs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Vec,Ix) when is_integer(Ix), Ix > 0, Ix =< size(Vec) ->
    element(Ix,Vec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all(_Vec,[]) -> [];
get_all(Vec,[Ix|Ixs]) ->
    [get(Vec,Ix)|get_all(Vec,Ixs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(N) -> empty(N).

init(N,E) -> empty(N,E).

empty(N) -> empty(N,'').

empty(N,Elt) ->
    list_to_vector(mklist(N,Elt)).


mklist(0,_Elt) -> [];
mklist(N,Elt) when N > 0 ->
    [Elt|mklist(N-1,Elt)].
