%% canvas1.ex

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x300+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas]),
tk:cmd(Canvas, [create,line,10,10,200,200]).






