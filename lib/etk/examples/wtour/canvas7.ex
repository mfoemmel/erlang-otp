%% canvas7.ex
%% canvas with embedded widgets

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),


tk:cmd(Canvas, [create, rectangle,20,20,80,80, {fill,red}]),
tk:cmd(Canvas, [create, rectangle,60,60,120,120,{fill,green}]),
tk:cmd(Canvas, [create, rectangle,40,40,100,100, {fill,blue}]),

B = etk:button(Canvas, [{text, "Embedded button"}]),
tk:cmd(Canvas, [create,window,100,75,{window, B}]).




























