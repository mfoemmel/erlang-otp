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
%%% Purpose : A generic file dialog toolkit.

-module(tool_genfd).

-compile(export_all).

%% Trick: Store Dir without trailing "/" => then up (cd ..) becomes
%%        easy, just: filename:dirname(Dir)!

%% Hidden is list of {Dir,[filename()]}
-record(state,{dir, hidden=[], extensions=[], owner, save=false,file=""}).

-define(UPW,35).
-define(UPH,30).
-define(ENTRYH,30).

create(Parent,Options) ->
    FDid = spawn_link(?MODULE,init,[Parent,self(),Options]),
    receive
	fdok ->
	    FDid
    end.

close(FD) ->
    FD ! close.

hide(FD,Dir,File) ->
    FD ! {hide,Dir,File}.

show(FD,Dir,File) ->
    FD ! {show,Dir,File}.

get_files(FD) ->
    req(FD,get_files).

get_all(FD) ->
    req(FD,get_all).

set_dir(FD,Dir) ->
    FD ! {set_dir,Dir}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------


init(Parent,Owner,Options) ->
    gs:frame(frame,Parent,[{packer_x,[{fixed,?UPW},{stretch,1}]},
			  {packer_y,[{fixed,?UPH},{fixed,?ENTRYH},
				     {stretch,1}]},
			  {pack_x,2},{pack_y,2}]),
    Fup = filename:dirname(code:which(?MODULE)) ++ "/fup.bm",
    gs:button(up,frame,[{label,{image, Fup}},{pack_x,1},{pack_y,1}]),
    gs:label(infodir,frame,[{pack_x,2},{pack_y,1},{align,w},
			    {label,{text," Dir:"}}]),
    gs:label(l1,frame,[{label, {text,"File:"}},{align,e},
		       {pack_x,1},{pack_y,2}]),

    gs:entry(entry,frame,[{keypress,true},{pack_x,2},{pack_y,2}]),
    gs:listbox(lb,frame,[{pack_x,{1,2}},{pack_y,3},{selectmode,single},
			 {vscroll,right},{click,true},{doubleclick,true}]),
    {value, D} = gs:assq(dir,Options),
    absolute = filename:pathtype(D),
    {value, E} = gs:assq(extensions,Options),
    {value, Hid} = gs:assq(hidden,Options),

    case  gs:assq(save,Options) of
	{value, true} ->
	    {value, SaveFile} = gs:assq(file,Options),
	    State = #state{dir=filename:join(filename:split(D)),
			   extensions=E,hidden=Hid, owner=Owner,
			   save=true, file=SaveFile};
	{value,false} ->
	    State = #state{dir=filename:join(filename:split(D)),
			   extensions=E,hidden=Hid, owner=Owner,
			   save=false}
    end,
    refresh(State),
    Owner ! fdok,
    loop(State).


req(FD,Req) ->
    FD ! {self(),Req},
    receive
	{reply,Reply} ->
	    Reply
    end.

hide_impl(State,Dir,File) ->
    H = State#state.hidden,
    NewHidden = case gs:assq(Dir,H) of
		    {value,Files} ->
			lists:keyreplace(Dir,1,H,{Dir,[File|Files]});
		    _ ->
			[{Dir,[File]}|H]
		end,
    State#state{hidden=NewHidden}.

show_impl(State,Dir,File) ->
    H = State#state.hidden,
    NewHidden = case gs:assq(Dir,H) of
		    {value,Files} ->
			lists:keyreplace(Dir,1,H,{Dir,Files--[File]});
		    _ ->
			H
		end,
    State#state{hidden=NewHidden}.


loop(State) ->
    receive
	{hide,Dir,File} ->
	    S2 = hide_impl(State,Dir,File),
	    if Dir == State#state.dir -> refresh(S2);
	       true -> true
	    end,
	    loop(S2);
	{show,Dir,File} ->
	    S2 = show_impl(State,Dir,File),
	    if Dir == State#state.dir -> refresh(S2);
	       true -> true
	    end,
	    loop(S2);
	close ->
	    bye;
	{set_dir,Dir} ->
	    S2 = State#state{dir=Dir},
	    refresh(S2),
	    loop(S2);
	{From,get_files} ->
	    From ! {reply,{selection,{State#state.dir,selection()}}},
	    loop(State);
	{From,get_all} ->
	    From ! {reply,{selection,{State#state.dir,get_all()}}},
	    loop(State);
	{gs,up,click,_,_} ->
	    S2 = try_to_enter(State,filename:dirname(State#state.dir)),
	    loop(S2);
	{gs,lb,doubleclick,_,[Idx,ItemTxt|_]} ->
	    S2 = try_to_enter(State,ItemTxt),
	    loop(S2);
	{gs,entry,keypress,_,['Return'|_]} ->
	    FileOrDir=gs:read(entry,text),
	    S2 = try_to_enter(State,FileOrDir),
	    loop(S2);
	{gs,entry,keypress,_,_} ->
	    loop(State);
	{gs,lb,click,_,[Idx,ItemTxt|_]} ->
	    EntryText = case lists:last(ItemTxt) of
			    $/ -> "";
			    Nope ->
				ItemTxt
			end,
	    gs:config(entry,{text,EntryText}),
	    loop(State);
	Other ->
	    io:format("filedialog loop got other:~p~n",[Other]),
	    loop(State)
    end.

%% Returns: {Dirs,Files}
selection() ->
    case gs:read(entry,text) of
	"" ->
	    sort_selected(gs:read(lb,selection),[],[]);
	File ->
	    {[],[File]}
    end.

get_all() ->
    sort_selected(lists:seq(0,gs:read(lb,size)-1),[],[]).

sort_selected([],Dirs,Files) ->
     {Dirs,Files};
sort_selected([Idx|Is],Dirs,Files) ->
    FileOrDir = gs:read(lb,{get,Idx}),
    case lists:last(FileOrDir) of
	$/ ->
	    sort_selected(Is,[drop_last(FileOrDir)|Dirs],Files);
	Afile ->
	    sort_selected(Is,Dirs,[FileOrDir|Files])
    end.
	    
refresh(#state{dir=Dir,hidden=H,extensions=E,
	       save=Save,file=SaveFile}) ->
    gs:config(frame,{cursor,busy}),
    Items = get_files(Dir,H,E),
    gs:config(lb,clear),
    case Save of
	true -> gs:config(entry,{text,SaveFile});
	false -> gs:config(entry,{text,""})
    end,
    gs:config(lb,[{items,Items}]),
    gs:config(lb,{selection,clear}),
    gs:config(infodir,{label,{text,[" Dir: "|Dir]}}),    
    gs:config(frame,{cursor,parent}),
    Items.

drop_last(L) ->
    drop_last(L,1).

drop_last(L,N) ->
    lists:sublist(L,length(L)-N).

count(Char,[]) ->
    0;
count(H,[H|T]) -> 1+count(H,T);
count(H,[_|T]) -> count(H,T).

%% Returns: List of files and directories.
%% Directories end with "/".
%% Extensions is a list of ".erl", ".hrl" or the empty list for all files.
get_files(Dir,Hidden,Extensions) -> 
    {ok,Files} = list_dir(Dir),
    CurHidden = case gs:assq(Dir,Hidden) of
		     {value, V} -> V;
		     false -> []
		 end,
    F2 = lists:filter(fun (F) -> not lists:member(F,CurHidden) end,
		      Files),
    get_files(Dir,F2,[],[],Extensions).

get_files(_,[],Dirs,Files,_) ->
    lists:sort(Dirs) ++ lists:sort(Files);
get_files(Dir,[File|Files],ResDirs,ResFiles,Extensions) ->
    case file:file_info(filename:join(Dir,File)) of
	{ok,{_,directory,_,_,_,_,_}} ->
	    get_files(Dir,Files,[File++"/"|ResDirs],ResFiles,Extensions);
	{ok,{_,regular,_,_,_,_,_}} when Extensions == [] ->
	    get_files(Dir,Files,ResDirs,[File|ResFiles],Extensions);
	{ok,{_,regular,_,_,_,_,_}} ->
	    case lists:member(filename:extension(File),
			      Extensions) of
		true -> 
		    get_files(Dir,Files,ResDirs,[File|ResFiles],Extensions);
		false ->
		    get_files(Dir,Files,ResDirs,ResFiles,Extensions)
	    end;
	Q -> % dead links (and other stuff?)
%	    io:format("file:~s/~s ignored~n",[Dir,File]),
	    get_files(Dir,Files,ResDirs,ResFiles,Extensions)
    end.

tell(#state{owner=OwnerPid},Msg) ->
    OwnerPid ! {file_dialog,Msg}.

try_to_enter(State,DirOrFile) ->
    case check_file(State, DirOrFile) of
	{file,Dir2,File2} ->
	    S2 = State#state{dir=Dir2},
	    gs:config(entry,{text,File2}),
	    refresh(S2),
	    tell(S2,{selection,{Dir2,{[],[File2]}}}),
	    S2;
	{dir,Dir2} ->
	    S2 = State#state{dir=Dir2},
	    refresh(S2),
	    S2;
	{error,What} ->
	    gs:config(infodir,[{label,{text,["Error: "|What]}},beep]),
	    State
    end.

cleanup(FileOrDir) ->
    case lists:last(FileOrDir) of
	$/ ->
	    case count($/,FileOrDir) of
		1 -> FileOrDir;
		G ->
		    cleanup(drop_last(FileOrDir))
	    end;
	Q ->
	    case lists:suffix("..",FileOrDir) of
		true ->
		    case drop_last(FileOrDir,3) of
			"" -> "/";
			Else ->
			    cleanup(filename:dirname(Else))
		    end;
		false ->
		    case lists:suffix(".",FileOrDir) of
			true ->
			    case drop_last(FileOrDir,2) of
				"" -> "/";
				Else ->
				    cleanup(Else)
			    end;
			false ->
			    FileOrDir
		    end
	    end
    end.
    

%%----------------------------------------------------------------------
%% Returns: {file,Dir2,File2}|{dir,Dir2}|{error,ErrorString}
%%----------------------------------------------------------------------
check_file(#state{dir=Dir},DirOrFile) ->	
    Path = case filename:pathtype(DirOrFile) of
	       absolute ->
		   cleanup(DirOrFile);
	       relative ->
		   cleanup(filename:join(Dir,DirOrFile));
	       volumerelative ->
		   DirOrFile ++ "/"
	   end,
    case file:file_info(Path) of
	{ok,{_,directory,_,_,_,_,_}} ->
	    {dir,Path};
	{ok,{_,regular,_,_,_,_,_}} ->
	    FileName = filename:basename(Path),
	    {file,filename:dirname(Path),FileName};
	_ ->
	    {error,"Unknown file or directory"}
    end.

%% patched version.
list_dir([Drive,$:]) ->
    file:list_dir([Drive,$:,$/]);
list_dir(Dir) ->
    file:list_dir(Dir).

