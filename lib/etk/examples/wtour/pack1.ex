%% pack1.ex

Top = etk:toplevel([]),
tk:wm([geometry, Top, "200x200+100+100"]),	
Label1 = etk:label(Top, [{text, "Label #1"}, {background, red}]),
Label2 = etk:label(Top, [{text, "Label #2"}, {background, blue}]),
Label3 = etk:label(Top, [{text, "Label #3"}, {background, green}]),

tk:pack(Label1, [{side, top}]),
tk:pack(Label2, [{side, left}]),
tk:pack(Label3, [{side, bottom}]).



