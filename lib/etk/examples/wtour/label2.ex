%% A label with options

Top = etk:toplevel([]),
Label = etk:label(Top, [{text, "A sunken label"},{relief,sunken}]),
tk:pack([Label]).

