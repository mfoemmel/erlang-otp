%% The simplest of buttons

Top = etk:toplevel([]),
Button = etk:button(Top, [{text, "Press me"},
			  {command,fun() ->
                                      io:format("Hello~n")
                                   end}]),
tk:pack([Button]).

