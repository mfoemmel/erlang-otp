%% A message widget

Top = etk:toplevel([]),
Msg = etk:message(Top, [{text, "This is a message. Note how the lines wrap"},
                        {aspect, 200},{justify,center}]),

tk:pack([Msg]).
