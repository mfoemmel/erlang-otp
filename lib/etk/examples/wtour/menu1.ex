%% menu1.ex -- basic drop down menu

Top = etk:toplevel([]),
MenuButton = etk:menubutton(Top, []),
Menu = etk:menu(MenuButton, [{tearoff, 0}]),
tk:cmd(Menu, [add,command,{label,"Open"},
	      {command, fun() ->
		            io:format("Open\n",[])
	                end}]),
tk:cmd(Menu, [add,command,{label,"Close"}]),
tk:cmd(Menu, [add,separator]),
tk:cmd(Menu, [add,command,{label,"Quit"}]),
tk:cmd(MenuButton, ["configure",{menu,Menu},{text, "File"}]),
tk:pack(MenuButton, []).








