%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%		       FAST DECLARATIVE VECTORS
%%
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
%%   verify_vector(Vec): returns 'true' or 'false'. Traverses the vector
%%       and checks that the data structure is valid (i.e., unbuggy :-)

-module(hipe_beam_vectors).
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

%% TODO: fix this
is_vector(X) when is_tuple(X) -> true;
is_vector(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to_vector(Xs) ->
  vector:from_list(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vector_to_list(V) ->
  vector:to_list(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set(Vec,Ix,V) ->
    vector:set(Ix,Vec,V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_all(Vec,[],[]) -> Vec;
set_all(Vec,[Ix|Ixs],[V|Vs]) ->
    set_all(set(Vec,Ix,V),Ixs,Vs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Vec,Ix) when is_integer(Ix), Ix > 0, Ix =< size(Vec) ->
    vector:get(Ix,Vec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all(_Vec,[]) -> [];
get_all(Vec,[Ix|Ixs]) ->
    [get(Vec,Ix)|get_all(Vec,Ixs)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(N) -> vector:from_list( lists:duplicate(N,'')).

init(N,E) ->vector:from_list( lists:duplicate(N,E)).

empty(N) -> vector:from_list( lists:duplicate(N,'')).

empty(N,Elt) -> vector:from_list( lists:duplicate(N,Elt)).
