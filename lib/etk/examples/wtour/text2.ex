%% text2.ex
%% Bacis text with scrollbars

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Text    = Top ++ ".txt",
ScrollY = Top ++ ".sy",
tk:text(Text,
        [{yscrollcommand,
         fun(From,To) -> tk:cmd(ScrollY, ["set",From,To]) end},
         {wrap, "word"}]),
tk:scrollbar(ScrollY,
        [{relief, "flat"}, {width, 10},
         {command, fun(Args) ->
 			    tk:cmd(Text, ["yview" | Args])
		   end}]),

tk:pack(ScrollY,[{side, "right"}, {fill, "y"}]),
tk:pack(Text, [{expand, "yes"}, {fill, "both"}, {'after', ScrollY}]),
   
tk:cmd(Text,[insert,insert,"This is a text wdiget, with an attached scrollbar.\n"]),
tk:cmd(Text,[insert,insert,"Try scrolling\n\n\n\n\n\n\n\n\n\n\n\n\n\n"]),
tk:cmd(Text,[insert,insert,"\n\n\n\n\n\n\n\n\n\n\n\n\n\nMe\n"]).






	



























