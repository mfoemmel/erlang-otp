%% A raised and disabled button
%% Tony? can click it if it's disabled

Top = etk:toplevel([]),
Button = etk:button(Top, [{text, "Press me"},
                          {relief,raised},
                          {state,disabled},
                          {command, fun() ->
                                        io:format("Hello~n")
                                    end}]),
tk:pack([Button]).


