%% group1.ex
%% Using frames to grup widgets

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x100+100+100"]),	

Frame = etk:frame(Top, [{relief,ridge},{borderwidth,2}]),
tk:pack([Frame]),

Label = etk:label(Frame, [{text, "Filename"}]),

Entry = etk:entry(Frame, [{relief, sunken}]),

tk:pack([Label, Entry, {side,left}, {padx,10}, {pady,10}]).



