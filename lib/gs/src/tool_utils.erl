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
-module(tool_utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Erlang Tools
%
%%% Description %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Miscellaneous auxiliary functions.
%
%%% Constants%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Minimum size of help windows
-define(wwin,300).
-define(hwin,150).
%
% Button sizes
-define(wbut,50).
-define(hbut,30).
%
-define(pad,10).
%
%%% Exports %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
-export([open_help/2,
	 file_dialog/1,
	 options_filename/1,
	 appstate_filename/2,
	 mkdir_for_file/1]).

-export([notify/2,
	 confirm/2,
	 confirm_exit/2,
	 request/2]).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%--Open help files----------------------------------------------------------

%----------------------------------------
% open_help(S,File)
%   S    = pid() GS
%   File = string() | nofile
% View the help file File, which can be an URL, an HTML file or a text
% file.
% This function is OS dependant.
% Unix: Assumes Netscape is up & running, and use Netscape remote commands
%   to display the file.
% NT: If File is a file, use the NT command 'start' which will open the
%   default tool for viewing the file.
%   If File is an URL, try to view it using Netscape.exe which requires that
%   the path Netscape.exe must be in TBD.
%   (TEMPORARY solution..., can be done better)
%----------------------------------------
open_help(S,nofile) ->
    notify(S,"Sorry, no help information exists");
open_help(S,File) ->
    case file_type(File) of

	%% Local file
	local ->
	    Cmd = case os:type() of
		      {unix,_AnyType} ->
			  "netscape -remote \"openURL(file:" ++ File ++ ")\"";

		      {win32,_AnyType} ->
			  "start " ++ nativename(File)
		  end,
	    spawn(os,cmd,[Cmd]);

	%% URL
	remote ->
	    Cmd = case os:type() of
		      {unix,_AnyType} ->
			  "netscape -remote \"openURL(" ++ File ++ ")\"";

		      {win32,_AnyType} ->
			  "netscape.exe -h " ++ urlname(File)
		  end,
	    spawn(os,cmd,[Cmd]);


	%% Otherwise, issue message window
	{error,Reason} ->
	    notify(S,[toolbar_lib:error_string(Reason),File])
    end.

file_dialog(Opts) ->
    tool_file_dialog:start(Opts).

%----------------------------------------
% file_type(File) -> local | remote | {error,nofile} | {error,raccess}
%   File - string() HTML file
% Returns local if File is an existing, readable file
% Returns remote if File is a remote URL (ie begins with 'http:')
% Returns {error,nofile} if File can not be found
% Returns {error,raccess} is File can be found but not read
%----------------------------------------
file_type(File) ->
    case File of
	%% http://...
	[$h,$t,$t,$p,$:,$/,$/|_] ->
	    remote;
	%% Local file
	_ ->
	    %% HTML files can have a tag (<name>.html#tag), this must be
	    %% removed when checking if the file exists
	    File2 = html_no_tag(File),
            case file:file_info(File2) of

        	%% File exists...
	        {ok,{_Size,regular,Access,_LastAccess,_LastModify,_,_}} ->
	            if		
         		%% ...but is read protected
		        Access/=read,Access/=read_write ->
        		    {error,raccess};
		
        		%% ...and is possible to read
        		true ->
        		    local
        	    end;
	
		%% File does not exist
		Error -> % {error,file_info}
		    {error,nofile}
	    end
    end.
			
%----------------------------------------
% html_no_tag(File) -> string()
%   File - string()
% If File looks like <name>.html#tag this function will return <name>.html
% Otherwise it returns File
%----------------------------------------
html_no_tag(File) ->
    case filename:extension(File) of
	[$.,$h,$t,$m,$l,$#|Tag] ->
	    lists:sublist(File,1,length(File)-length(Tag)-1);
	_ ->
	    File
    end.


%%%--Help windows-------------------------------------------------------------

%----------------------------------------
% notify(S,Strings) -> ok
%   S       = pid() GS
%   Strings = string() | [string()]
% A notification window contains a message to the user.
% Will lock the GUI until the user confirms the message by
% pressing the 'Ok' button.
%----------------------------------------
notify(S,Strings) ->
    W    = required_width(Strings,?wwin),
    Htop = round(2*?hwin/3),
    Hbot = ?hwin-Htop,

    %% Open a new window
    Win = gs:create(window,S,[{width,W},{height,?hwin},
			      {title,"Notification"},
			      {data, notifywin}]),

    %% Top frame containing a label
    Top = gs:create(frame,Win,[{width,W},{height,Htop},{x,0},{y,0},
			       {data,notifywin},{keypress,true}]),
    Lbl = gs:create(label,Top,[{width,W},{height,Htop-2*?pad},{x,0},{y,?pad},
			       {align,c},{justify,center},
			       {data,notifywin},{keypress,true}]),
  
    gs:config(Lbl,{label,{text,insert_newlines(Strings)}}),

    %% Bottom frame containing an 'Ok' button
    Bot = gs:create(frame,Win,[{width,W},{height,Hbot},{x,0},{y,Htop}]),
    gs:create(button,Bot,[{width,?wbut},{height,?hbut},
			  {x,W/2-?wbut/2},{y,Hbot/2-?hbut/2},
			  {label,{text,"Ok"}},
			  {data,notifywin},{keypress,true}]),

    gs:config(Win,{map,true}),
    
    event_loop(Win,null).


%----------------------------------------
% confirm(S,Strings) -> ok | cancel
%   S      = pid() GS
%   String = string() | [string()]
% A confirmation window contains a message to the user.
% Will lock the GUI until the user confirs the message by
% pressing the 'Ok' or 'Cancel' button. A WM destroy will be treated as a 'Cancel'.
%----------------------------------------
confirm(S,Strings) ->
    W    = required_width(Strings,?wwin),
    Htop = round(2*?hwin/3),
    Hbot = ?hwin-Htop,

    %% Open a new window
    Win = gs:create(window,S,[{width,W},{height,?hwin},
			      {title,"Confirmation"}]),

    %% Top frame containing a label
    Top = gs:create(frame,Win,[{width,W},{height,Htop},{x,0},{y,0},
			       {data,helpwin},{keypress,true}]),
    Lbl = gs:create(label,Top,[{width,W},{height,Htop-2*?pad},{x,0},{y,?pad},
			       {align,c},{justify,center},
			       {data,helpwin},{keypress,true}]),

    gs:config(Lbl,{label,{text,insert_newlines(Strings)}}),

    %% Bottom frame containing the 'Ok' and 'Cancel' buttons
    Bot = gs:create(frame,Win,[{width,W},{height,Hbot},{x,0},{y,Htop}]),
    gs:create(button,Bot,[{width,?wbut},{height,?hbut},
			  {x,W/4-?wbut/2},{y,Hbot/2-?hbut/2},
			  {label,{text,"Ok"}},
			  {data,helpwin},{keypress,true}]),
    gs:create(button,Bot,[{width,?wbut},{height,?hbut},
			  {x,3*W/4-?wbut/2},{y,Hbot/2-?hbut/2},
			  {label,{text,"Cancel"}},
			  {data,helpwin},{keypress,true}]),

    gs:config(Win,{map,true}),
    
    event_loop(Win,null).



%%% confirm_exit  /2
%%%
%%% confirm_exit returns yes, no or cancel depending 
%%% on the user reaction on the given message.
%%%
%%% Pre:
%%%    S       ==  pid from GS
%%%    String  ==  string  ||  [strings]
%%%
%%% Def:
%%%    confirm_exit  ==  yes  ||  no  ||  cancel
%%%

confirm_exit (S, Strings) ->
    W    = required_width (Strings, ?wwin),
    Htop = round (2 * ?hwin / 3),
    Hbot = ?hwin - Htop,

    %% Open a new window
    Win = gs:create (window, S, [{width, W}, {height, ?hwin},
				 {title, "Confirmation"}]),

    %% Top frame containing a label
    Top = gs:create(frame, Win, [{width, W}, {height, Htop}, 
				 {x, 0}, {y, 0},
				 {data, helpwin}, {keypress, true}]),
    Lbl = gs:create(label, Top, [{width, W}, {height, Htop-2*?pad}, 
				 {x, 0}, {y, ?pad},
				 {align, c}, {justify, center},
				 {data, helpwin}, {keypress, true}]),

    gs:config(Lbl, {label, {text, insert_newlines(Strings)}}), 

    %% Bottom frame containing the 'Ok' and 'Cancel' buttons
    Bot = gs:create(frame, Win, [{width, W}, {height, Hbot}, 
				 {x, 0}, {y, Htop}]), 
    gs:create(button, Bot, [{width, ?wbut}, {height, ?hbut}, 
			    {x, W/2 - 3*?wbut/2 - 5}, {y, Hbot/2-?hbut/2}, 
			    {label, {text, "Yes"}}, 
			    {data, confirm_exit}, {keypress, true}]), 
    gs:create(button, Bot, [{width, ?wbut}, {height, ?hbut}, 
			    {x, W/2 - ?wbut/2}, {y, Hbot/2-?hbut/2}, 
			    {label, {text, "No"}}, 
			    {data, confirm_exit}, {keypress, true}]), 
    gs:create(button, Bot, [{width, ?wbut}, {height, ?hbut}, 
			    {x, W/2 + ?wbut/2 + 5}, {y, Hbot/2-?hbut/2}, 
			    {label, {text, "Cancel"}}, 
			    {data, confirm_exit}, {keypress, true}]), 

    gs:config(Win, {map, true}), 

    event_loop(Win, null).


%----------------------------------------
% request(S,String) -> {ok,string()} | cancel
%   S      = pid() GS
%   String = string() | [string()]
% A request window contains a message to the user, requesting
% the user to provide some information.
% Will lock the GUI until the user cancels the request by pressing
% the 'Cancel' button or confirms any given (non-empty) information by
% pressing the 'Ok' button. A WM destroy event will be treated as a 'Cancel'
%----------------------------------------
request(S,Strings) ->
    W    = required_width(Strings,?wwin),
    H    = round(?hwin/3),

    %% Open a new window
    Win = gs:create(window,S,[{width,W},{height,?hwin},
			      {title,"Request"}]),

    %% Top frame containing a label
    Top = gs:create(frame,Win,[{width,W},{height,H},{x,0},{y,0},
			       {data,helpwin},{keypress,true}]),
    Lbl = gs:create(label,Top,[{width,W},{height,H-2*?pad},{x,0},{y,?pad},
			       {align,c},{justify,center},
			       {data,helpwin},{keypress,true}]),

    gs:config(Lbl,{label,{text,insert_newlines(Strings)}}),

    %% Middle frame containing an entry
    Mdl = gs:create(frame,Win,[{width,W},{height,H},{x,0},{y,H},
			       {data,helpwin},{keypress,true}]),
    Entry = gs:create(entry,Mdl,[{width,W-2*?pad},{height,H-2*?pad},
                                 {x,?pad},{y,?pad},
				 {data,helpwin},{keypress,true}]),

    %% Bottom frame containing a 'Cancel' and an 'Ok'button
    Bot = gs:create(frame,Win,[{width,W},{height,H},{x,0},{y,2*H}]),
    gs:create(button,Bot,[{width,?wbut},{height,?hbut},
				{x,W/4-?wbut/2},{y,H/2-?hbut/2},
				{label,{text,"Ok"}},
				{data,helpwin},{keypress,true}]),

    gs:create(button,Bot,[{width,?wbut},{height,?hbut},
				{x,3*W/4-?wbut/2},{y,H/2-?hbut/2},
				{label,{text,"Cancel"}},
				{data,helpwin}]),

    gs:config(Win,{map,true}),

    event_loop(Win,Entry).



event_loop(Win,Entry) ->
    receive

	%%
	%% Request window
	%%

        %% 'Ok' pressed in request window
	{gs,_Obj,_Event,helpwin,["Ok"|_]} when Entry/=null ->
	    case gs:read(Entry,text) of
		"" ->
		    event_loop(Win,Entry);
		Info ->
		    gs:destroy(Win),
		    {ok,Info}
	    end;

	%% 'Return' pressed in request window
	{gs,_Obj,_Event,helpwin,['Return'|_]} when Entry/=null ->
	    case gs:read(Entry,text) of
		"" ->
		    event_loop(Win,Entry);
		Info ->
		    gs:destroy(Win),
		    {ok,Info}
	    end;

	%%
	%% Notify window
	%%

        %% 'Ok' pressed in notify window
	{gs,_Obj,_Event,notifywin,["Ok"|_]} ->
	    gs:destroy(Win),
	    ok;

	%% 'Window manager destroy' received in notify window
	{gs,_Obj,destroy,notifywin,_} ->
	    gs:destroy(Win),
	    ok;


	%%
	%% Confirm window
	%%

        %% 'Ok' pressed in confirm window
	{gs,_Obj,_Event,helpwin,["Ok"|_]} ->
	    gs:destroy(Win),
	    ok;

        %% 'Return' pressed in notify or confirm window
	{gs,_Obj,_Event,helpwin,['Return'|_]} ->
	    gs:destroy(Win),
	    ok;


	%% Confirm Exit window
	%%

        %% 'Save' pressed in confirm_exit window
	{gs, _Obj, _Event, confirm_exit, ["Yes"|_]} ->
	    gs:destroy(Win),
	    yes;

        %% 'Return' pressed in confirm_exit window
	{gs, _Obj, _Event, confirm_exit, ['Return'|_]} ->
	    gs:destroy(Win),
	    yes;

        %% 'Exit' pressed in confirm_exit window
	{gs, _Obj, _Event, confirm_exit, ["No"|_]} ->
	    gs:destroy(Win),
	    no;

        %% 'Exit' pressed in confirm_exit window
	{gs, _Obj, _Event, confirm_exit, ["Cancel"|_]} ->
	    gs:destroy(Win),
	    cancel;


	%%
	%% Common or partly common events
	%%

        %% 'Cancel pressed in confirm or request window
	{gs,_Obj,_Event,helpwin,["Cancel"|_]} ->
	    gs:destroy(Win),
	    cancel;
	
	%% 'Window manager destroy' received in notify, 
	%% confirm,confirm_exit or request window
	{gs,_Obj,destroy,_,_} ->
	    gs:destroy(Win),
	    cancel;

	%% Flush any other GS events
	{gs,_Obj,_Event,_Data,_Arg} ->
            event_loop(Win,Entry)
    end.

%----------------------------------------
% required_width(Strings,Min) -> Req
%   Strings   = string() | [string()]
%   Min = Req = integer()
% Returns the minimum required width in pixels for a help window,
% which is the maximum of Min and the required width for Strings.
% NOTE: Font dependant really!
%----------------------------------------
required_width([First|Rest],Min) when list(First) ->
    Req = 7*length(First), % 7 pixels per character
    if
        Req>Min ->
	    required_width(Rest,Req);
	true ->
	    required_width(Rest,Min)
    end;
required_width([],Min) ->
    Min;
required_width(String,Min) ->
    Req = 7*length(String),
    if
        Req>Min ->
	    Req;
        true ->
            Min
    end.

%----------------------------------------
% insert_newlines(Strings) => string()
%   Strings - string() | [string()]
% If Strings is a list of strings, return a string where all these strings
% are concatenated with newlines in between, otherwise return Strings.
%----------------------------------------
insert_newlines([String|Rest]) when list(String), Rest/=[]->
    String ++ "\n" ++ insert_newlines(Rest);
insert_newlines([Last]) ->
    [Last];
insert_newlines(Other) ->
    Other.
nativename(Name) ->
  case os:type() of
    {win32,_} ->
      {ok, Value, _} = regexp:gsub(Name,"/","\\"),
      Value;
    _ ->
      Name
  end.

urlname(Name) ->
  {ok, Value, _} = regexp:gsub(Name,"\\\\","/"),
  Value.

%%
%% options_filename(Basename) - Returns a complete pathname to a file
%%   to store user specific options in.
%%
%%   The returned pathname will be to a file in a subdirectory under
%%   the calling user's home directory.
%%
%% Arguments
%%   Basename		A (string) name that is related to the 
%% 			application that needs to save data. 
%%
%% Returns		A (string) absolute pathname.
%%

-define(TOOLSDIR, ".erlang_tools").
-define(TOOLSOPT_SUFFIX, ".opts").


appstate_filename(Application, Filename) ->
    {ok, [[ResultDir]]} = init:get_argument(home),
    filename:join([ResultDir, ?TOOLSDIR, Application, Filename]).


options_filename(Basename) ->
    Filename = Basename ++ ?TOOLSOPT_SUFFIX,
    {ok, [[ResultDir]]} = init:get_argument(home),
    filename:join([ResultDir, ?TOOLSDIR, Filename]).



%% Tries to create the directory DirName. Missing parent directories
%% ARE created.  The function sucessively tries to create all parent
%% directories, and may if there is a problem along the way result in
%% some directories being created, but still exiting with an error
%% message.
%%

mkdir_p(DirName) ->

    %% The fun Makedir is used in lists:foldl to succesively 
    %% create the directories that make up the entire DirName
    Makedir = fun(Comp, Acc) ->
		      Dir = filename:join([Acc, Comp]),
		      case file:make_dir(Dir) of
			  {error, eexist} -> Dir ;
			  {error, _Reason} -> exit({error, mkdir_failed});
			  ok -> Dir
		      end
	      end,
    

    [Drive | Rest]  = filename:split(DirName),

    lists:foldl(Makedir, Drive, Rest).


%%
%% Utilizes mkdir_p to create a directory necessary for creating the specified
%% file.
%%

mkdir_for_file(FileName) ->
    All = filename:split(FileName),
    Butlast = butlast(All),
    DirName = filename:join(Butlast),
    mkdir_p(DirName).


%%
%% Missing lists-library function.
%%

butlast([X]) -> [];
butlast([Hd|Tl]) -> [Hd | butlast(Tl)].
    
    
