%% bind2.ex
%% Enter and Leave

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x100+100+100"]),	

Entry = etk:entry(Top, [{relief, sunken}]),

tk:bind(Entry, "<Enter>", [], 
	fun() -> io:format("Enter\n") end),

tk:bind(Entry, "<Leave>", [], 
	fun() -> io:format("Leave\n") end),

tk:pack([Entry]).





