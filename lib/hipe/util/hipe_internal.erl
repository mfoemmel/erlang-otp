%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HIPE_INTERNAL
%

-module(hipe_internal).

-export([fault/2,
	 fault/1,
	 send/2,
	 keysearch/3,
	 call_fun/2,
	 not_loaded/0,
	 apply/3]).

fault(Reason) ->
  erlang:fault(Reason).
fault(Reason,Args) ->
  erlang:fault(Reason,Args).

apply(M,F,Args) ->
  erlang:apply(M,F,Args).

not_loaded() ->
  exit({beam_code_not_loaded, fix_sparc_loader}).

send(P, M) ->
   P ! M.

keysearch(Key, N, [H|T]) when element(N, H) == Key ->
    {value, H};
keysearch(Key, N, [H|T]) ->
    keysearch(Key, N, T);
keysearch(Key, N, []) -> false.

call_fun(F,[]) ->
  F();
call_fun(F,[A]) ->
  F(A);
call_fun(F,[A1,A2]) ->
  F(A1,A2);
call_fun(F, [A1,A2,A3]) ->
  F(A1,A2,A3);
call_fun(F, [A1, A2, A3, A4]) ->
  F(A1, A2, A3, A4);
call_fun(F, [A1, A2, A3, A4, A5]) ->
  F(A1, A2, A3, A4, A5);
call_fun(F, [A1, A2, A3, A4, A5, A6]) -> 
  F(A1, A2, A3, A4, A5, A6);
call_fun(F, [A1, A2, A3, A4, A5, A6,A7]) -> 
  F(A1, A2, A3, A4, A5, A6, A7).
