%% frame3.ex
%% Frames with options

Top = etk:toplevel([]),
tk:wm([geometry, Top, "200x200+100+100"]),	
Frame1 = etk:frame(Top, [{relief,ridge},{borderwidth,3}]),
Frame2 = etk:frame(Top, [{relief,groove},{borderwidth,3}]),

tk:pack([Frame1, Frame2,{side,top}, {expand,yes},{fill,both},
	 {padx,30},{pady,30}]).



