%% canvas81.ex
%% Drawing into an image
%%

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),

catch tk:image([delete, image1]),
tk:image([create, photo, image1,{width,300},{height,370}]),
L = etk:label(Top, [{image,image1}]),
tk:pack([L, {fill,both},{expand,yes}]),

tk:bind(L, "<B1-Motion>", ['%x', '%y'],
	fun(X, Y) ->
	   tk:cmd(image1, [put,"{black}",{to},X,Y,X+2,Y+2])
	end).
