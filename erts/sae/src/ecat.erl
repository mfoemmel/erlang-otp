-module(ecat).

-export([main/1]).
    
%% to build
%%    ecc ecat.erl
%%    elink -o ecat -m ecat erlang erl_open_port -s ecat main

%% try cat ecat.erl | ./ecat

 
main(_) ->   
    process_flag(trap_exit, true),
    Port = (catch erlang:open_port({fd,0,1}, [eof,binary])),
    loop(Port),
    close(Port),
    erlang:halt().
     
loop(Port) ->
    case read(Port) of
	eof ->
	    true;
	{ok, X} ->
	    write(Port, [X]),
	    loop(Port);
	{'EXIT', AA} ->
	    true
    end.

read(Port) ->
    receive
	{Port, {data, Bytes}} ->
	    {ok, Bytes};
	{Port, eof} ->
	    eof;
	{'EXIT', AA}->
	    true
    end.

write(Port, X) ->
    Port ! {self(), {command, X}}.

close(Port) ->
    Port ! {self(), close},
    receive
	Any -> 
	    true
    end.




