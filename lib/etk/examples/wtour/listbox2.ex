%% listbox2.ex with a scrollbar
%% Tony? can we do this etk: I use tk??

Top = etk:toplevel([]),

ListBox = Top ++ ".list",
ScrollY = Top ++ ".scroll",

tk:listbox(ListBox,
            [{yscrollcommand,
              fun(From,To) -> 
		     tk:cmd(ScrollY, ["set",From,To]) 
	      end}
	    ]),
tk:scrollbar(ScrollY,
	     [{command, 
	       fun(Args) ->
		      tk:cmd(ListBox, ["yview" | Args])
	       end}]),

tk:pack(ScrollY, [{side, "right"}, {fill, "y"}]),
tk:pack(ListBox, [{expand, "yes"}, {fill, "both"}, {'after', ScrollY}]),

Vals = ["First list item","Second list item","Third list item",
	"Fourth list item","Fifth list item","Sixth list item",
	"Seventh list item","Eighth list item","Ninth list item",
	"Tenth list item","Eleventh list item","Twelfth list item",
	"Thirteenth list item","Fourteenth list item","Fifteenth list item",
	"Sixteenth list item","Seventeenth list item","Eighteenth list item",
	"Ninteenth list item"],

lists:foreach(fun(Item) ->
		     tk:cmd(ListBox, [insert,'end',Item])
	      end, Vals).




