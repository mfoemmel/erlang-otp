-module(primes).
-export([main/1]).

main([Prog,N]) ->
    case catch primes(list_to_integer(N)) of
	{'EXIT',Reason} ->
	    erlang:display({error,Reason});
	Other ->
	    ok
    end,
    halt();
main(Args) ->
    erlang:display({bad_args,Args}),
    halt().

primes(N) ->
    sieve(seq(2, N)).

seq(I, Max) when I =< Max ->
    [I|seq(I+1, Max)];
seq(_, _) -> [].

sieve([Prime|Rest]) ->
    erlang:display(Prime),
    sieve(sieve(Prime, Rest));
sieve([]) -> ok.

sieve(Prime, [H|T]) when H rem Prime =:= 0 ->
    sieve(Prime, T);
sieve(Prime, [H|T]) ->
    [H|sieve(Prime, T)];
sieve(Prime, []) -> [].
