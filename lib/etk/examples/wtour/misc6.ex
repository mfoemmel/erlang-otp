%%
%% Linking buttons in to different windows
%%

W1 = etk:toplevel([]),
L1 = etk:label(W1, [{text, "Label1"}, {relief, ridge}]),
B1 = etk:button(W1, [{text,"Button1"},
		     {command, fun() -> io:format("I am doing it!\n") end}]),
tk:pack([L1,B1]),

W2 = etk:toplevel([]),
L2 = etk:label(W2, [{text, "Label2"}, {relief, raised}]),
B2 = etk:button(W2, [{text,"Button2"}, {bg,red},{fg,white},
		     {command, fun() -> io:format("Do it!\n") end}]),
tk:pack([B2,L2]),
%% Make B2 master
tk:join(B1, node(), B2).



