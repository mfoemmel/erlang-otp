%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			  VECTORS IN ERLANG
%
% Abstract interface to vectors.


%%% XXX: replace the pure version with one based on gb_trees!
-define(impl,hipe_pure_vectors).
%-define(impl,hipe_beam_vectors).

%-define(VERIFY,1).
%-define(pimpl,hipe_pure_vectors).

-module(hipe_vectors).
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

-ifdef(VERIFY).
set({V1,V2},B,C) ->
%%  io:format("Set"),
  R1 = ?pimpl:set(V1,B,C),
  R2 = ?impl:set(V2,B,C),
%%  io:format(" ~w ~w\n",[R1,R2]),
  {R1,R2}.
-else.
set(V,B,C) ->
  ?impl:set(V,B,C).
-endif.

set_all(A,B,C) ->
    ?impl:set_all(A,B,C).

-ifdef(VERIFY).
get({V1,V2},B) ->
%%  io:format("Get"),
  R1 = ?pimpl:get(V1,B),
  R2 = ?impl:get(V2,B),
  case R1 of
    R2 -> R1;
    _ ->
      io:format("Got ~w\n ~w\n",[R1,R2]),
      io:format("V1 ~w\n V2 ~w\n",[V1,V2]),
      R1
  end.

-else.
get(A,B) ->
    ?impl:get(A,B).
-endif.

get_all(A,B) ->
    ?impl:get_all(A,B).

-ifdef(VERIFY).
vsize({V1,V2}) ->
%%  io:format("Size"),
  R1 =  ?impl:vsize(V1),
  R2 =  ?impl:vsize(V2),
  R1 = R2,
%%  io:format(" ~w ~w\n",[R1,R2]),
  R1.
-else.
vsize(V) ->
    ?impl:vsize(V).
-endif.

is_vector(V) ->
    ?impl:is_vector(V).

verify_vector(V) ->
    ?impl:verify_vector(V).

vector_to_list(V) ->
    ?impl:vector_to_list(V).

list_to_vector(V) ->
    ?impl:list_to_vector(V).

empty(E) ->
    ?impl:empty(E).

empty(A,B) ->
    ?impl:empty(A,B).

-ifdef(VERIFY).
init(Xs) ->
  io:format("Init"),
  V1 = ?pimpl:init(Xs),
  V2 = ?impl:init(Xs),
  io:format(" ~w ~w\n",[V1,V2]),
  {V1,V2}.
-else.
init(Xs) ->
    ?impl:init(Xs).
-endif.

-ifdef(VERIFY).
init(A,B) ->
  io:format("Init/2"),
  V1 = ?pimpl:init(A,B),
  V2 = ?impl:init(A,B),
  io:format(" ~w ~w\n",[V1,V2]),
  {V1,V2}.
-else.
init(A,B) ->
    ?impl:init(A,B).
-endif.

-ifdef(VERIFY).
list({V1,V2}) ->
  io:format("List"),  
  R1 = ?pimpl:list(V1),
  R2 = ?impl:list(V2),
  R1 = R2,
  io:format(" ~w ~w\n",[R1,R2]),
  R1.
    
-else.
list(V) ->
    ?impl:list(V).
-endif.

first_index() ->
    ?impl:first_index().
