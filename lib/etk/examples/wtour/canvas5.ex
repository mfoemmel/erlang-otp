%% canvas5.ex
%% canvas tags

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),


tk:cmd(Canvas, [create, rectangle,20,20,80,80, {fill,red}]),
tk:cmd(Canvas, [create, rectangle,60,60,120,120,{fill,green},{tag,cats}]),
tk:cmd(Canvas, [create, rectangle,40,40,100,100, {fill,blue}]),

tk:cmd(Canvas, [move,cats,100,0]).



























