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
%% ------------------------------------------------------------
%% Simple Browser
%% ------------------------------------------------------------

-module(browser).

-export([start/2,init/3]).

-export([start/0,start/2,init/3]).

start() ->
    spawn(browser,init,[self(),text(),items()]),	
    receive
	{browser,Result} -> Result
    end.

text() -> "Pick an erlangian: ".

items() ->
    lists:sort(["marcus","bjorn","anders","dalle","henrik","mk",
		"keisu","klas","patric","ola","torpvret","lelle",
		"eklas","mbj","janne","martin","kent","pippi",
		"gunilla","uwiger","macr"]).


start(Text,Items) ->
    spawn(browser,init,[self(),Text,Items]),	
    receive
	{browser,Result} -> Result
    end.

init(Pid,Text,Items) ->
    S=gs:start(),
    Win=gs:window(S,[{width,250},{height,270},{title,"Browser"}]),
    Lbl=gs:label(Win,[{label,{text,Text}},{width,250}]),
    Entry=gs:entry(Win,[{y,35},{width,240},{x,5},
			{keypress,true},{setfocus,true}]),
    Lb=gs:listbox(Win,[{x,5},{y,65},{width,160},{height,195},
		       {vscroll,right},{click,true},{doubleclick,true}]),
    Ok=gs:button(Win,[{label,{text,"OK"}},{width,40},{x,185},{y,175}]),
    Cancel=gs:button(Win,[{label,{text,"Cancel"}},{x,175},{y,225},{width,65}]),
    gs:config(Lb,[{items,Items}]),
    gs:config(Win,{map,true}),
    browser_loop(Pid,Ok,Cancel,Entry,Lb).

browser_loop(Pid,Ok,Cancel,Entry,Lb) ->
    receive
	{gs,Ok,click,_,_} ->
	    Txt=gs:read(Entry,text),
	    Pid ! {browser,{ok,Txt}};
	{gs,Cancel,click,_,_} ->
	    Pid ! {browser,cancel};
	{gs,Entry,keypress,_,['Return'|_]} ->
	    Txt=gs:read(Entry,text),
	    Pid ! {browser,{ok,Txt}};
	{gs,Entry,keypress,_,_} ->
	    browser_loop(Pid,Ok,Cancel,Entry,Lb);
	{gs,Lb,click,_,[Idx, Txt|_]} ->
	    gs:config(Entry,{text,Txt}),
	    browser_loop(Pid,Ok,Cancel,Entry,Lb);
	{gs,Lb,doubleclick,_,[Idx, Txt|_]} ->
	    Pid ! {browser,{ok,Txt}};
	{gs,_,destroy,_,_} ->
	    Pid ! {browser,cancel};
	X ->
	    io:format("Got X=~w~n",[X]),
	    browser_loop(Pid,Ok,Cancel,Entry,Lb)
    end.

%% ------------------------------------------------------------
%% end of browser.erl
