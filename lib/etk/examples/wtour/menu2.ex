%% Complex menu structures are difficult to create by hand.

%% Its much easier like this ...

%% 

F = fun(T) -> {button,[{label,T},
	               {func, fun() -> io:format("pressed ~p\n",[T]) end}]}
    end,
M = {menubar,[
	      {menu,"One","left",
	       [F("zip"),F("zap"),F("zop")]},
	      {menu,"Two","left",
	       [F("plink"),F("plank"),F("plonk")]},
	      {menu, "Three", "left",
	       [{submenu, "More ...",
		 [F("a"), F("b"), F("c")]},
		{submenu, "Stuff ...",
		 [F("boo"), F("Zlurp"), F("zlock")]}]}]},

etk:start(),
W = etk:toplevel([]),
tk:wm([maxsize, W, 10000, 10000]),
tk:wm([geometry, W, "550x100+100+200"]),
tk:wm([title, W, "Escape"]),
MenuBar = etk_menu:create_menubar(W, M),
tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
W.




















