%% canvas3.ex
%% click on the bomb
%% this doesn't work
%% tiny

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),

B = tk:cmd(Canvas, [create, bitmap,100,50,{bitmap,"@bomb.xbm"},
		    {background,grey}]),
tk:bind_ctag(Canvas, B, "<ButtonPress-1>", [],
		fun() -> io:format("BOOM\n") end).





















