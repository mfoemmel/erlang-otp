%% canvas4.ex
%% canvas item stacking
%% click on a box to raise it
%% tiny

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),


R1 = tk:cmd(Canvas, [create, rectangle,20,20,80,80, {fill,red}]),
R2 = tk:cmd(Canvas, [create, rectangle,60,60,120,120, {fill,green}]),
R3 = tk:cmd(Canvas, [create, rectangle,40,40,100,100, {fill,blue}]).
























