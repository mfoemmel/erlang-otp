%% canvas6.ex
%% canvas with scrollbar

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, [{scrollregion, "-10c -10c 50c 20c"}]),

tk:cmd(Canvas, [create, rectangle, 100,100,400,400, {fill,red}]),
tk:cmd(Canvas, [create, rectangle, 300,300,600,600, {fill,green}]),
tk:cmd(Canvas, [create, rectangle, 200,200,500,500, {fill,blue}]),

Scrollbar1 = etk:scrollbar(Canvas, 
			   [{relief, sunk},
			    {command,fun(Args) ->
				    tk:cmd(Canvas, ["yview" | Args])
			             end}]),
Scrollbar2= etk:scrollbar(Canvas, 
			   [{orient,horiz},{relief, sunk},
			    {command,fun(Args) ->
				    tk:cmd(Canvas, ["xview" | Args])
			             end}]),

tk:pack(Scrollbar2, [{side,bottom}, {fill,x}]),
tk:pack(Scrollbar1, [{side,right}, {fill,y}]),

tk:pack(Canvas, [{expand,yes},{fill,both}]),

io:format("scrollbar2|~p|\n", [Scrollbar2]),

tk:cmd(Canvas, [config, {xscrollcommand,
	fun(From,To) -> tk:cmd(Scrollbar2,[set,From,To]) end},
		        {yscrollcommand, 
	fun(From,To) -> tk:cmd(Scrollbar1,[set,From,To]) end}]).

%% this line crashes it



























