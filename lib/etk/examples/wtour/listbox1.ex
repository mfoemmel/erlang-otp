%% listbox1.ex

Top = etk:toplevel([]),
ListBox = etk:listbox(Top, []),
tk:pack([ListBox]),
tk:cmd(ListBox, [insert,'end',"First List Item"]),
tk:cmd(ListBox, [insert,'end',"Second List Item"]),
tk:cmd(ListBox, [insert,'end',"Third List Item"]).







