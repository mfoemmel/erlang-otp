%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	messages.erl
%%  Module   :	messages
%%  Purpose  :  General messages, both text and graphical
%%  Notes    : 
%%  History  :	* 1996-07-10 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(messages).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.4 $').
-export([debug/4, error/4, file_chooser/1,
	 file_chooser_init/2, warning/2]).

-include("gsb.hrl").
%% -import([]).

%% ____________________________________________________________________
%%
%%  debug(Format, Args, Module, Fun)
%%  Returns : ok / {error, format}
%%  Args    : Format - String as in io:format
%%            Args   - List with variables
%%            Module - String with calling module name
%%            Fun    - String with calling function name
%% ____________________________________________________________________

debug(Format, Args, Module, Fun) ->
    Guard = ?DEBUG,
    case Guard of
	true ->
	    io:format(lists:concat(["~nDebug: in ~w:~w~n", Format, "~n"]),
		      lists:append([Module, Fun], Args));
	false ->
	    ok
    end.

%% ____________________________________________________________________
%%
%%  error(Format, Args, Module, Fun)    
%%  Returns : ok / {error, format}
%%  Args    : Format - String as in io:format
%%            Args   - List with variables
%%            Module - String with calling module name
%%            Fun    - String with calling function name
%% ____________________________________________________________________

error(Format, Args, Module, Fun) ->
    io:format(lists:concat(["~nError: in ~w:~w~n", Format, "~n"]),
	      lists:append([Module, Fun], Args)).

%% ____________________________________________________________________
%%
%%  warning(Type, Gs, String)<function>    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

warning(GS, String) ->
  gs:create(window, win, GS, [{width, 300}, {height, 100}]),
  gs:create(button, ok, win, [{label, {text, "Ok"}}, {width, 70},
			      {x,75}, {y, 50}]),
  gs:create(button, cancel, win, [{label, {text, "Cancel"}}, {width, 70},
				  {x,155}, {y, 50}]),
  gs:create(label, text, win, [{label, {text, String}}, {anchor, center},
			       {width, 300}, {align, center}, {x,150},
			       {y, 25}]),
  gs:config(win, {map, true}),
  warning_loop().

warning_loop() ->
  receive
    {gs, ok, click, _, _} ->
      gs:destroy(win),
      ok;
    {gs, cancel, click,_,_} ->
      gs:destroy(win),
      cancel;
    Other ->
      warning_loop()
  end.

%% ____________________________________________________________________
%%
%%  file_chooser(GS, Text)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

file_chooser(Text) ->
  spawn_link(messages,
	     file_chooser_init, [Text, self()]),
  receive
    {file_chooser, Reply} ->
      Reply
  end.


file_chooser_init(Text, Pid) ->
  GS = gs:start(),
  gs:create(window, window, GS, [{width, 200}, {height, 85}, {title, ""}]),
  gs:create(label, label, window,
	    [{width, 200},{align, w}, {label, {text, Text}}]),
  gs:create(entry, entry, window, [{width, 200},{y,25},{keypress, true},
				   {setfocus, true}]),
  gs:create(button, ok_button, window,
		 [{y, 55},{click, true}, {label, {text, "Ok"}}]),
  gs:create(button, cancel_button,  window,
		     [{y,55},{x,100},
		      {click, true}, {label, {text, "Cancel"}}]),
  gs:config(window, {map, true}),
  file_chooser_loop(Pid).

file_chooser_loop(Pid)->
  receive
    {gs, entry, keypress, Data, ['Return'|_]} ->
      Pid!{file_chooser,{ok, gs:read(entry, text)}};
    {gs, ok_button, click, Data, Other}  ->
      Pid!{file_chooser,{ok, gs:read(entry, text)}};
    {gs, cancel_button, click, Data, Other}  ->
      Pid!{file_chooser,{cancel, gs:read(entry, text)}};
    All ->
      file_chooser_loop(Pid)
  end.
      
