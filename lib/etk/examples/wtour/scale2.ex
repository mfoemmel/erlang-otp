%%   An scale widget with options

Top = etk:toplevel([]),
Scale = etk:scale(Top, 
		  [{label,"Flow"},{from,-1000},{to,1000},
		   {command, fun(S) ->
			   io:format("Scale = ~w~n", [S])
		             end}]),
tk:pack([Scale]).









