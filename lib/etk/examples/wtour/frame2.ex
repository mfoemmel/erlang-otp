%% frame2.ex
%% Frames with options

Top = etk:toplevel([]),
tk:wm([geometry, Top, "200x200+100+100"]),	
Frame1 = etk:frame(Top, [{relief,raised},{borderwidth,3},{background, red}]),
Frame2 = etk:frame(Top, [{relief,sunken},{borderwidth,3},{background, blue}]),

tk:pack([Frame1, Frame2,{side,top}, {expand,yes},{fill,both}]).



