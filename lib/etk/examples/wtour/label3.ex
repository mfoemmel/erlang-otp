%% A label with a bit map instead of text

Top = etk:toplevel([]),
Label = etk:button(Top, [{bitmap, "@joe.xbm"},
                         {relief,raised},{borderwidth,2}]),
tk:pack([Label]).

