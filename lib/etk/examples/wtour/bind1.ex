%% bind1.ex
%% Event binding
%% tony

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x100+100+100"]),	

Entry = etk:entry(Top, [{relief, sunken}]),

tk:bind(Entry, "<ButtonPress-2>", [], 
	fun() -> io:format("You pressed button2\n") end),

tk:bind(Entry, "<Tab>", [], 
	fun() -> io:format("You pressed tab\n") end),

tk:pack([Entry]).





