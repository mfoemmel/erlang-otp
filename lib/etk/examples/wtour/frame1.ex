%% frame1.ex
%% Basic frames

Top = etk:toplevel([]),
tk:wm([geometry, Top, "200x200+100+100"]),	
Frame1 = etk:frame(Top, [{background, red}]),
Frame2 = etk:frame(Top, [{background, blue}]),

tk:pack([Frame1, Frame2,{expand,yes},{fill,both}]).



