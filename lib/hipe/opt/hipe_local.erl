%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%			LOCAL TRANSFORMATIONS
%
% The following module implements local transformations (within a basic block)
% Services:
% - deletes comments.
% - performs forward and backward copy propagation within basic blocks,
%   and deletes dead code.
%
% Notes:
%
% *** UNFINISHED ***
% - at present, icode only. how to port to rtl/sparc?
% - the definition of a 'pure' operation is hazy
% - dead code should use sets rather than lists

-module(hipe_local).
%-export([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following are generic and/or debugging support.

-define(cfg,hipe_rtl_cfg).
-define(bb,hipe_bb).
-define(code,hipe_rtl).
-define(liveness,hipe_rtl_liveness).

%-define(debug(Str,Args),ok).
-define(debug(Str,Args),io:format(Str,Args)).

%-define(report(Str,Args),ok).
-define(report(Str,Args),io:format(Str,Args)).

-define(banner(Str),ok).
%-define(banner(Str),io:format(Str)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Change these depending on what level they are used.
%  You may have to rewrite a bit more than just changing the called
% module, etc.

% Comment removal:

-define(is_comment(X),hipe_icode:is_comment(X)).

% Copy propagation:

-define(is_copy(X),hipe_rtl:is_move(X)).
-define(copy_src(X),hipe_rtl:move_src(X)).
-define(copy_dst(X),hipe_rtl:move_dst(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%cfg(CFG) ->
%    exit(nyi).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Delete comments :-)
% - annoying little bastards

%remove_comments([]) -> [];
%remove_comments([I|Is]) ->
%    case ?is_comment(I) of
%	true ->
%	    remove_comments(Is);
%	false ->
%	    [I|remove_comments(Is)]
%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Forward copy propagation
%
% Given "x = e --> y = x --> use y"
% we can replace y with x when:
% * x is not defined along the paths
% * y is not defined in the second path
%
% Notes:
% - we maintain a list of copies, which are represented
%   as {Use,Def}. Thus, when "y = x" is found, we insert "{x,y}"
%   into the list
% - if a variable X is defined, every pair containing x (in either
%   position) is removed
% - adding constant propagation:
%   * record "x = c" in list
%   * trans_instr is rewritten to handle constant folding

%fwd_cprop([],Cp) -> [];
%fwd_cprop([I|Is],Cp0) ->
%    {NewI,Cp1} = cprop(I,Cp0),
%    [NewI | fwd_cprop(Is,Cp1) ].

%%cprop(I,Cp0) ->
%%    {D,U} = def_use(I),
%%    Cp1 = add_if_copy(I,kill_defs(D,Cp0)),
%%    NewI = trans_instr(U,I,Cp0),
%%    { NewI, Cp1 }.

%kill_defs([],Cp) -> Cp;
%kill_defs([D|Ds],Cp) ->
%    kill_defs(Ds,kill_def(D,Cp)).

%kill_def(D,[]) -> [];
%kill_def(D,[{D,_}|Xs]) -> kill_def(D,Xs);
%kill_def(D,[{_,D}|Xs]) -> kill_def(D,Xs);
%kill_def(D,[X|Xs]) -> [X|kill_def(D,Xs)].

%resolve_all(Us,Cp) -> [ {U,resolve(U,Cp)} || U <- Us ].

%resolve(U,[]) -> U;
%resolve(U,[{U,V}|Xs]) -> resolve(V,Xs);
%resolve(U,[_|Xs]) -> resolve(U,Xs).

%add_if_copy(I,Cp) ->
%    case ?is_copy(I) of
%	true ->
%	    [ {?copy_dst(I), ?copy_src(I)} | Cp];
%	false ->
%	    Cp
%    end.

trans_instr(U,I,Cp) ->
    ?code:subst_uses(resolve_all(U,Cp),I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Backward copy propagation
%
% Given  "x = e ---> y = x --->"
%                a          b
% such that
% * x is dead in b
% * x or y are not used or defined in a
% then we can (a) delete y = x, (b) set y = e
%
% Not yet implemented -- the problem is that we must delete the
% copy instruction as well as replacing x by y. This can be done
% by walking the list of already processed instructions and killing
% off the move, but that seems inelegant and quadratic or worse.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Dead code elimination

%dead_code([],LiveOut) -> { [], LiveOut };
%dead_code([I|Is],LiveOut) ->
%    { NewIs, Live } = dead_code(Is,LiveOut),
%    case live(I,Live) of
%	{yes,NewLive} ->
%	    { [I|NewIs], NewLive };
%	no ->
%	    { NewIs, Live }
%    end.

%live(I,Live) ->
%    {D,U} = def_use(I),
%    Dset = set:init(D),
%    Uset = set:init(U),
%    case delible(I) of
%	true ->
%	    case empty(intersect(Dset,Live)) of
%		true ->
%		    no;
%		false ->
%		    {yes,union(diff(Live,Dset),Uset)}
%	    end;
%	false ->
%	    {yes,union(diff(Live,D),U)}
%    end.

%delible(I) ->
%    case hipe_rtl:type(I) of
%	move -> true;
%	alu -> true;
%	fp -> true;
%	load_tagged -> true;
%	load -> true;
%	load_atom -> true;
%	_ -> false
%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Common subexpressions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** SUPPORT ***

%def_use(I) ->
%    D = [ hipe_rtl:var_name(X) || X <- hipe_rtl:defines(I) ],
%    U = [ hipe_rtl:var_name(X) || X <- hipe_rtl:uses(I) ],
%    {D,U}.

%def_use_sets(I) ->
%    {D,U} = def_use(I),
%    { set:init(D), set:init(U) }.

%empty(X) -> not set:non_empty(X).

%union(X,Y) -> set:union(X,Y).

%diff(X,Y) -> set:diff(X,Y).

%intersect(X,Y) -> set:intersect(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% *** TESTING ***

%test(MFA) ->
%    nyi.
