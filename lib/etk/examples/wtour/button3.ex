%% A simple check button

Top = etk:toplevel([]),
Button = etk:checkbutton(Top, [{text, "Select me"}]),
tk:pack([Button]).


