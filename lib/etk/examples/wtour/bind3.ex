%% bind3.ex
%% Accessing event parameters
%% tony

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x100+100+100"]),	

Entry = etk:entry(Top, [{relief, sunken}]),

tk:bind(Entry, "<ButtonPress-1>", ['%x','%y'], 
	fun(X,Y) -> io:format("pressed ~p ~p\n", [X, Y]) end),

tk:pack([Entry]).





