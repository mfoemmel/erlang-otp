%% canvas2.ex

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),
tk:cmd(Canvas, [create, arc,10,10,50,50,{fill,red}]),
tk:cmd(Canvas, [create, bitmap,100,50,{bitmap,"@bomb.xbm"},{background,white}]),
tk:cmd(Canvas, [create, line,10,100,40,140,{fill,blue}]),
tk:cmd(Canvas, [create, oval,150,150,170,200,{fill,yellow}]),
tk:cmd(Canvas, [create, polygon,200,10,210,50,280,20, {fill,green}]),
tk:cmd(Canvas, [create, rectangle,10,200,30,250, {fill,cyan}]),
tk:cmd(Canvas, [create, text,100,250, {text,"Some random text"}]).




















