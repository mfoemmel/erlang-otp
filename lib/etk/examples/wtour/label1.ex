%% The simplest of labels

Top = etk:toplevel([]),
Label = etk:label(Top, [{text, "A label"}]),
tk:pack([Label]).
