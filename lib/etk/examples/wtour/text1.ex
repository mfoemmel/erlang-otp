%% text1.ex
%% Bacis text

Top = etk:toplevel([]),
tk:wm([geometry, Top, "300x370+100+100"]),	

Text = etk:text(Top, []),
tk:pack([Text]),

tk:cmd(Text,[insert,current,"This is a text wdiget displaying some text.\n"]),
tk:cmd(Text,[insert,current,"Try editing the text in the widget.\n"]).



	



























