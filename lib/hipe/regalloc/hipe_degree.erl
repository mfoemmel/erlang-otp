%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% File    : hipe_degree.erl
%% Created : 06 Feb 2000 by Andreas Wallin <d96awa@dis.uu.se>
%%-----------------------------------------------------------------------
%% @author   Andreas Wallin <d96awa@dis.uu.se>
%%-----------------------------------------------------------------------
%% @doc
%%
%%   Auxiliary file for the iterated register coalescing allocator
%%   keeping track of how many nodes a temporary interferes with.
%%
%% @end
%%-----------------------------------------------------------------------

-module(hipe_degree).

-export([new/2, inc/2, dec/2, is_trivially_colorable/3, degree/2]).

%%% define exactly one of these to choose underlying implementation
-define(USE_ARRAYS,1).
%-define(USE_VECTORS,1).

-ifdef(USE_ARRAYS).
-define(NEW(NRELEMENTS,INIT),hipe_bifs:array(NRELEMENTS,INIT)).
-define(GET(ARRAY,INDEX),hipe_bifs:array_sub(ARRAY,INDEX)).
-define(SET(ARRAY,INDEX,VALUE),begin hipe_bifs:array_update(ARRAY,INDEX,VALUE),ARRAY end).
-endif.

-ifdef(USE_VECTORS).
-define(NEW(NRELEMENTS,INIT),hipe_vectors_wrapper:empty(NRELEMENTS,INIT)).
-define(GET(ARRAY,INDEX),hipe_vectors_wrapper:get(ARRAY,INDEX)).
-define(SET(ARRAY,INDEX,VALUE),hipe_vectors_wrapper:set(ARRAY,INDEX,VALUE)).
-endif.

%%%----------------------------------------------------------------------
%% Function:    new
%%
%% Description: Creates a new degree data structure. This structure 
%%               contains the degree of all temporaries. The degree
%%               is the number of temporaries that it interfere with.
%%
%% Parameters:
%%   No_temporaries --  Number of temporaries.
%%                        (This is because the temporary number is
%%                        used as an index in for vectors.)
%%
%% Returns: 
%%   A new degree data structure.
%%%----------------------------------------------------------------------
new(No_temporaries, Target) ->
    Degree = ?NEW(No_temporaries, 0),
    % Initiate all precoloured to inf degree
    precoloured_to_inf_degree(Target:all_precoloured(), Degree).

%%%----------------------------------------------------------------------
%% Function:    inc
%%
%% Description: Increases the degree with one for one node/temporary.
%%
%% Parameters:
%%   Node           --  The node/temporary that you which to increase
%%                       the degree on.
%%   Degree         --  The degree data structure.
%%
%% Returns: 
%%   An updated degree data structure.
%%%----------------------------------------------------------------------
inc(Node, Degree) when is_integer(Node) ->
    case ?GET(Degree, Node) of
	inf ->
	    Degree;
	Node_degree ->
	    ?SET(Degree, Node, Node_degree + 1)
    end.

%%%----------------------------------------------------------------------
%% Function:    dec
%%
%% Description: Decreases the degree with one for one node/temporary.
%%
%% Parameters:
%%   Node           --  The node/temporary that you which to decrease
%%                       the degree on.
%%   Degree         --  The degree data structure.
%%
%% Returns: 
%%   An updated degree data structure.
%%%----------------------------------------------------------------------
dec(Node, Degree) when is_integer(Node) ->
    case ?GET(Degree, Node) of
	inf ->
	    Degree;
	Node_degree ->
	    ?SET(Degree, Node, Node_degree - 1)
    end.

%%%----------------------------------------------------------------------
%% @spec is_trivially_colorable(term(), fixnum(), term()) -> bool()
%%
%% @doc Succeeds if a node is a trivially colorable node. That is, it has
%%      degree less than K.
%% @end
%% Parameters:
%%   Node           --  The node/temporary that you which know if it's 
%%                       simple
%%   Degree         --  The degree data structure.
%%%----------------------------------------------------------------------

is_trivially_colorable(Node, K, Degree) ->
    case degree(Node, Degree) of
	inf ->
	    false;
	Node_degree ->
	    K > Node_degree
    end.

%%%----------------------------------------------------------------------
%% Function:    degree
%%
%% Description: Tells what degree a node/temporary has.
%%
%% Parameters:
%%   Node           --  The node/temporary that you which know the 
%%                       degree of.
%%   Degree         --  The degree data structure.
%%
%% Returns: 
%%   The degree of Node
%%%----------------------------------------------------------------------
degree(Node, Degree) when is_integer(Node) ->
    ?GET(Degree, Node).


%% Set degree of Node to Value
set_degree(Node, Value, Degree) ->
    ?SET(Degree, Node, Value).


%% All precoloured temporaries shall have infinite degree
precoloured_to_inf_degree([], Degree) -> Degree;
precoloured_to_inf_degree([P|Ps], Degree) ->
    precoloured_to_inf_degree(Ps, set_degree(P, inf, Degree)).
