%% Radio buttons

Top = etk:toplevel([]),
B1  = etk:radiobutton(Top, [{text,"Red"},{variable,"v1"},{value,1}]),
B2  = etk:radiobutton(Top, [{text, "Green"}, {variable,"v1"},{value,2}]),
B3  = etk:radiobutton(Top, [{text,"Blue"},{variable,"v1"},{value,3}]),
tk:pack([B1,B2,B3]).



