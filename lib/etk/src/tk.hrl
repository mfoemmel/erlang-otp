%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%
%% TK command numbers
%%
-define(ERL_TK_BIND,          1).
-define(ERL_TK_DESTROY,       2).
-define(ERL_TK_LOWER,         3).
-define(ERL_TK_RAISE,         4).
-define(ERL_TK_BELL,          5).
-define(ERL_TK_BUTTON,        6).
-define(ERL_TK_CHECKBUTTON,   7).
-define(ERL_TK_RADIOBUTTON,   8).
-define(ERL_TK_LABEL,         9).
-define(ERL_TK_UPDATE,        11).
-define(ERL_TK_WINFO,         12).
-define(ERL_TK_WM,            13).
-define(ERL_TK_BINDTAGS,      14).
-define(ERL_TK_CANVAS,        17).
-define(ERL_TK_CLIPBOARD,     18).
-define(ERL_TK_ENTRY,         19).
-define(ERL_TK_FRAME,         20).
-define(ERL_TK_FOCUS,         21).
-define(ERL_TK_GRAB,          22).
-define(ERL_TK_IMAGE,         23).
-define(ERL_TK_LISTBOX,       24).
-define(ERL_TK_MENU,          25).
-define(ERL_TK_MENUBUTTON,    26).
-define(ERL_TK_MESSAGE,       27).
-define(ERL_TK_OPTION,        28).
-define(ERL_TK_PACK,          29).
-define(ERL_TK_PLACE,         30).
-define(ERL_TK_SCALE,         31).
-define(ERL_TK_SCROLLBAR,     32).
-define(ERL_TK_SELECTION,     33).
-define(ERL_TK_TEXT,          34).
-define(ERL_TK_TK,            35).
-define(ERL_TK_CMD,           36).
-define(ERL_TK_GRID,          37).
-define(ERL_TK_TOPLEVEL,      38).

-define(ERL_TK_SETVAR,        46).
-define(ERL_TK_GETVAR,        47).
-define(ERL_TK_EVENT,         48).
-define(ERL_TK_WLINK,         49).
-define(ERL_TK_OPERATION,     50).

-define(TK_ERL_OK,            0).
-define(TK_ERL_ERROR,         1).
-define(TK_ERL_EVENT,         2).
-define(TK_ERL_INVOKE,        3).
-define(TK_ERL_TKERROR,       4).
-define(TK_ERL_TKSCREEN,      5).
-define(TK_ERL_OPERATION,     6).


-define(TK_OP_OFFSET,         0).
-define(TK_OP_OPTION,         1).
-define(TK_OP_STRING,         2).

-define(TK_IDLE_TIME,       300).

-record(tkRgb, {
		red = 0,
		green = 0,
		blue = 0 }).

%%
%%
%%
-define(tkset(Var,Value),
	ets:insert(tkPriv, {Var,Value}), Value).

-define(tkget(Var),
	ets:lookup_element(tkPriv, Var, 2)).
