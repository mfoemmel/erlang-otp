%%%----------------------------------------------------------------------
%%% File    : hipe_degree.erl
%%% Author  : Andreas Wallin <d96awa@ida.dis.uu.se>
%%% Purpose : Keep track of how many nodes a temporary interferes with.
%%% Created : 06 Feb 2000 by Andreas Wallin <d96awa@ida.dis.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_degree).
-author("Andreas Wallin").
-export([new/2, 
	 inc/2, 
	 dec/2, 
	 is_simple/3, 
	 degree/2]).


%%%----------------------------------------------------------------------
% Function:    new
%
% Description: Creates a new degree data structure. This structure 
%               contains the degree of all temporaries. The degree
%               is the number of temporaries that it interfere with.
%
% Parameters:
%   No_temporaries --  Number of temporaries.
%                        (This is because the temporary number is
%                        used as an index in for vectors.)
%
% Returns: 
%   A new degree data structure.
%%%----------------------------------------------------------------------
new(No_temporaries, Target) ->
    Degree = hipe_vectors_wrapper:empty(No_temporaries, 0),
    % Initiate all precolored to inf degree
    precolored_to_inf_degree(Target:all_precolored(), Degree).


%%%----------------------------------------------------------------------
% Function:    inc
%
% Description: Increases the degree with one for one node/temporary.
%
% Parameters:
%   Node           --  The node/temporary that you which to increase
%                       the degree on.
%   Degree         --  The degree data structure.
%
% Returns: 
%   An updated degree data structure.
%%%----------------------------------------------------------------------
inc(Node, Degree) when integer(Node) ->
    case hipe_vectors_wrapper:get(Degree, Node) of
	inf ->
	    Degree;
	Node_degree ->
	    hipe_vectors_wrapper:set(Degree, Node, Node_degree + 1)
    end.

%%%----------------------------------------------------------------------
% Function:    dec
%
% Description: Decreases the degree with one for one node/temporary.
%
% Parameters:
%   Node           --  The node/temporary that you which to decrease
%                       the degree on.
%   Degree         --  The degree data structure.
%
% Returns: 
%   An updated degree data structure.
%%%----------------------------------------------------------------------
dec(Node, Degree) when integer(Node) ->
    case hipe_vectors_wrapper:get(Degree, Node) of
	inf ->
	    Degree;
	Node_degree ->
	    hipe_vectors_wrapper:set(Degree, Node, Node_degree - 1)
    end.


%%%----------------------------------------------------------------------
% Function:    is_simple
%
% Description: Test if a node is a simple node. That is, it has less
%               than K in degree.
%
% Parameters:
%   Node           --  The node/temporary that you which know if it's 
%                       simple
%   Degree         --  The degree data structure.
%
% Returns: 
%   true  --  If the node is simple
%   false --  otherwise
%%%----------------------------------------------------------------------
is_simple(Node, K, Degree) ->
  Res =
    case degree(Node, Degree) of
	inf ->
	    false;
	Node_degree ->
	    K > Node_degree
    end,
  Res.

%%%----------------------------------------------------------------------
% Function:    degree
%
% Description: Tells what degree a node/temporary has.
%
% Parameters:
%   Node           --  The node/temporary that you which know the 
%                       degree of.
%   Degree         --  The degree data structure.
%
% Returns: 
%   The degree of Node
%%%----------------------------------------------------------------------
degree(Node, Degree) when integer(Node) ->
    hipe_vectors_wrapper:get(Degree, Node).


% Set degree of Node to Value
set_degree(Node, Value, Degree) ->
    hipe_vectors_wrapper:set(Degree, Node, Value).


% All precolored temporaries shall have infinite degree
precolored_to_inf_degree([], Degree) -> Degree;
precolored_to_inf_degree([P|Ps], Degree) ->
    precolored_to_inf_degree(Ps, set_degree(P, inf, Degree)).
