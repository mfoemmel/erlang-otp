%% Dummy copy_files.erl defined to provide backward compatibility for
%% applications that use both the new SSL (3.0 or later), and the
%% older versions of SSL.

-module(copy_files).

-export([start/1]).

start(Args) ->
    io:fwrite("Error: unexpected call of copy_files:start/1:~n"
	      "Args = ~p~n", [Args]),
    ok.


