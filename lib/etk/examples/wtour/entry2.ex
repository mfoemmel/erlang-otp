%%   An entry widget with options

Top = etk:toplevel([]),
Entry = etk:entry(Top,[{relief,sunken},
                       {font, "-*-helvetica-*-r-*-*-*-240-*-*-*-*-*-*"}]),
tk:pack([Entry]),
tk:cmd(Entry, [insert,0,"I'm here!!"]).

