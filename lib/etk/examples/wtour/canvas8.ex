%% canvas8.ex
%% Drawing

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Canvas = etk:canvas(Top, []),
tk:pack([Canvas, {fill,both},{expand,yes}]),

tk:bind(Canvas, "<B1-Motion>", ['%x', '%y'],
	fun(X, Y) ->
	   tk:cmd(Canvas, [create, rectangle, X, Y, X, Y, {width, 5}])
	end).

	



























