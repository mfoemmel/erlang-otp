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
%% Erlang tk layer

-module(etk).

-export([start/0, start/1, stop/0]).

%% widgets
-export([button/2, canvas/2, checkbutton/2, entry/2, frame/2,
	 label/2, listbox/2, menu/2, menubutton/2,
	 message/2, radiobutton/2, scale/2, scrollbar/2, text/2,
	 toplevel/2, toplevel/1]).

%% tk
-export([bell/0, bell/1,
	 bindtags/1, bindtags/2,
	 destroy/1, focus/1, 
	 lower/1, lower/2,
	 raise/1, raise/2,
	 bind/1, bind/2, bind/4, unbind/2,
	 bind_tag/2, bind_tag/3, bind_tag/5, unbind_tag/3,
	 bind_ctag/2, bind_ctag/3, bind_ctag/5, unbind_ctag/3,
	 pack/1, pack/2,
	 place/1, place/2,
	 grid/1, grid/2, 
	 tk/1, tk/2,
	 wm/1, wm/2,
	 update/0, update/1,
	 selection/1,
	 clipboard/1, 
	 grab/1,
	 option/1, 
	 image/1, 
	 winfo/1,
	 cmd/2,
	 rcmd/2,
	 setvar/2,
	 getvar/1]).

%% utils
-export([cget/2, rcget/2, configure/2,
	 childrenof/1, parentof/1, toplevelof/1, classof/1]).


start() ->
    tk:start([{withdrawn,true}]).

start(Opts) ->
    tk:start(Opts).

stop() ->
    tk:stop().

%% Top level's are linked by default to the creator
toplevel(Opts) ->
    W = tk:mkpath("", t),
    tk:toplevel(W, Opts),
    tk:wlink(W),
    W.

toplevel(Parent, Opts) -> 
    W = tk:mkpath(Parent, t),
    tk:toplevel(W, Opts),
    tk:wlink(W),
    W.

button(Parent, Opts) ->
    W = tk:mkpath(Parent,b),
    tk:button(W, Opts),
    W.

canvas(Parent, Opts) ->
    W = tk:mkpath(Parent,c),
    tk:canvas(W, Opts),
    W.

checkbutton(Parent, Opts) ->
    W = tk:mkpath(Parent,cb),
    tk:checkbutton(W, Opts),
    W.

entry(Parent, Opts) ->
    W = tk:mkpath(Parent,e),
    tk:entry(W, Opts),
    W.

frame(Parent, Opts) ->
    W = tk:mkpath(Parent,f),
    tk:frame(W, Opts),
    W.

label(Parent, Opts) ->
    W = tk:mkpath(Parent,l),
    tk:label(W, Opts),
    W.

listbox(Parent, Opts) ->
    W = tk:mkpath(Parent,li),
    tk:listbox(W, Opts),
    W.

menu(Parent, Opts) ->
    W = tk:mkpath(Parent,m),
    tk:menu(W, Opts),
    W.

menubutton(Parent, Opts) ->
    W = tk:mkpath(Parent,mb),
    tk:menubutton(W, Opts),
    W.

message(Parent, Opts) ->
    W = tk:mkpath(Parent,me),
    tk:message(W, Opts),
    W.

radiobutton(Parent, Opts) ->
    W = tk:mkpath(Parent,r),
    tk:radiobutton(W, Opts),
    W.

scale(Parent, Opts) ->
    W = tk:mkpath(Parent,s),
    tk:scale(W, Opts),
    W.

scrollbar(Parent, Opts) ->
    W = tk:mkpath(Parent,sb),
    tk:scrollbar(W, Opts),
    W.

text(Parent, Opts) ->
    W = tk:mkpath(Parent,tx),
    tk:text(W, Opts),
    W.

%%
%% Tk wrappers
%%

bell() -> tk:bell().
bell(W) -> tk:bell(W).
bindtags(W) -> tk:bindtags(W).
bindtags(W, Tags) -> tk:bindtags(W, Tags).
destroy(Ws) -> tk:destroy(Ws).
focus(Opts) -> tk:focus(Opts).
lower(W) -> tk:lower(W).
lower(W, Other) -> tk:lower(W, Other).
raise(W) -> tk:raise(W).
raise(W, Other) -> tk:raise(W, Other).
bind(W) -> tk:bind(W).
bind(W, Event) -> tk:bind(W, Event).
bind(W, Event, Template, Fun) -> tk:bind(W, Event, Template, Fun).
unbind(W, Event) -> tk:unbind(W, Event).
bind_tag(W,Tag,Event,Template,Fun) -> tk:bind_tag(W,Tag,Event,Template,Fun).
bind_tag(W,Tag, Event) -> tk:bind_tag(W, Tag, Event).
bind_tag(W,Tag) -> tk:bind_tag(W, Tag).
unbind_tag(W,Tag,Event) -> tk:unbind_tag(W,Tag,Event).
bind_ctag(W,Tag,Event,Template,Fun) -> tk:bind_ctag(W,Tag,Event,Template,Fun).
bind_ctag(W,Tag, Event) -> tk:bind_ctag(W, Tag, Event).
bind_ctag(W,Tag) -> tk:bind_ctag(W, Tag).
unbind_ctag(W,Tag,Event) -> tk:unbind_ctag(W,Tag,Event).
pack(W) -> tk:pack(W).
pack(W, Opts) -> tk:pack(W, Opts).
place(W) -> tk:place(W).
place(W, Opts) -> tk:place(W, Opts).
grid(W) -> tk:grid(W).
grid(W,Opts) -> tk:grid(W, Opts).
tk(Opts) -> tk:tk(Opts).
tk(Opts, Arg) -> tk:tk(Opts, Arg).
wm(Opts) -> tk:wm(Opts).
wm(W, Opts) -> tk:wm(W, Opts).
update() -> tk:update().
update(Flag) -> tk:update(Flag).
selection(Opts) -> tk:selection(Opts).
clipboard(Opts) -> tk:clipboard(Opts).
grab(Opts) -> tk:grab(Opts).
option(Opts) -> tk:option(Opts).
image(Opts) -> tk:image(Opts).
winfo(Opts) -> tk:winfo(Opts).
cmd(W, Opts) -> tk:cmd(W, Opts).
rcmd(W, Opts) -> tk:rcmd(W, Opts).
setvar(Var,Value) -> tk:setvar(Var,Value).
getvar(Var) -> tk:getvar(Var).

%% utils
parentof(W) -> tk:parentof(W).
childrenof(W) -> tk:childrenof(W).
toplevelof(W) -> tk:toplevelof(W).
classof(W) -> tk:classof(W).
cget(W, Opt) -> tk:cget(W, Opt).
rcget(W, Opt) -> tk:rcget(W, Opt).
configure(W, Opts) -> tk:configure(W, Opts).

