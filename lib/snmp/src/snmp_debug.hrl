
-ifdef(snmp_debug).
-define(debug(F,A),
	io:format("~p:~p:~p:" ++ F ++ "~n",[self(),?MODULE,?LINE]++A)).

%% Same as 'debug' but without the ending newline ('~n').
-define(debug_b(F,A),
	io:format("~p:~p:~p:" ++ F,[self(),?MODULE,?LINE]++A)).
%% To be used together with 'debug_b'. Note: NO ending newline ('~n')..
-define(debug_e(F,A),
	io:format(F,A)).
-else.
-define(debug(F,A),ok).
-define(debug_b(F,A),ok).
-define(debug_e(F,A),ok).
-endif.




