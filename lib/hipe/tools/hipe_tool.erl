%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% ====================================================================
%%  Filename : 	hipe_tool.erl
%%  Module   :	hipe_tool
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2002-03-13 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2007/05/10 16:29:46 $
%%              $Revision: 1.13 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_tool).
-export([start/0,loop/1]).

-define(WINDOW_WIDTH, 920).
-define(WINDOW_HEIGHT, 460).
-define(DEFAULT_BG_COLOR, {217,217,217}).
-define(POLL_INTERVAL, 5000).
-define(FONT, {screen, 12}).
-define(HEADER_FONT, {screen, [bold], 12}).
-define(NORMAL_FG_COLOR, {0,0,0}).

-record(state, {win_created=false,	% :: bool(),
		mindex=0,		% :: integer(),
		mod,			% :: atom(),
		funs=[],		% :: list(),
		mods=[],		% :: list(),
		options=[o2],		% :: list(),
		compiling=false		% :: bool()
	       }).

start() ->
  spawn(fun () -> init() end).

init() ->
  process_flag(trap_exit, true), 
  gs:start(),
  S = init_window(#state{}),
  loop(S).

loop(State) ->
    receive
      {gs, code_listbox, click, Data, [Idx, Txt | _]} ->
	NewState = update_module_box(State,Idx,Data,Txt),
	hipe_tool:loop(NewState);
      {gs, module_listbox, click, Data, [Idx, _Txt | _]} ->
	NewState = update_fun(State,Idx,Data),
	hipe_tool:loop(NewState);
      {gs,compmod, click, _,_} ->
	hipe_tool:loop(	compile(State));
      {gs,prof,click,[],["Turn off\nProfiling"]} ->
	hipe_profile:prof_module_off(State#state.mod),
	 hipe_tool:loop(
	      update_module_box(State,
				State#state.mindex,
				State#state.mods,"")); 
      {gs,prof,click,[],_} ->
	hipe_profile:prof_module(State#state.mod),
	 hipe_tool:loop(
	      update_module_box(State,
				State#state.mindex,
				State#state.mods,"")); 

      {gs, win, configure, _, _} ->
	gs:config(win, [{width, ?WINDOW_WIDTH}, {height, ?WINDOW_HEIGHT}]),
	hipe_tool:loop(State);
      show_window when State#state.win_created =:= true ->
	gs:config(win, [raise]),
	hipe_tool:loop(State);
      show_window when State#state.win_created =:= false ->
	hipe_tool:loop((init_window(State))#state{win_created=true});

      {gs, _Id, click, close_menu, _Args} ->
	gs:destroy(win),
	hipe_tool:loop(State#state{win_created=false});

      {gs, _Id, keypress, _Data, [c, _, 0, 1 | _]} ->
	gs:destroy(win),
	hipe_tool:loop(State#state{win_created=false});
	
      {gs, _Id, keypress, _Data, ['C', _, 1, 1 | _]} ->
	gs:destroy(win),
	hipe_tool:loop(State#state{win_created=false});

      {gs, _Id, keypress, _Data, _Args} ->
	hipe_tool:loop(State);

      {gs, _, destroy, _, _} ->
	hipe_tool:loop(State#state{win_created=false});

      {compilation_done,_Res,Sender} ->
	case State#state.compiling of
	  Sender ->
	    catch gs:config(compmod,[ {enable, true}]),
	    update_text(compiling,""),
	    hipe_tool:loop(
	      update_module_box(State,
				State#state.mindex,
				State#state.mods,""));  

	  _ ->
	    hipe_tool:loop(State)
	end;


      {'EXIT', _Pid, _Reason} ->
	exit(normal);

      _Other ->
	io:format("HiPE window received message ~p ~n", [_Other]),
	hipe_tool:loop(State)

    after 
	?POLL_INTERVAL ->
	    hipe_tool:loop(update_code_listbox(State))
    end.


init_window(State) ->
  create_window(State),
  gs:config(win, [{map,true}]),
  update_code_listbox(State#state{win_created=true}).

create_window(State) ->
    gs:window(win, gs:start(), [{width, ?WINDOW_WIDTH},
				{height, ?WINDOW_HEIGHT},
				{bg, ?DEFAULT_BG_COLOR},
				{title, "[HiPE] Code list"},
				{configure, true},
				{destroy, true},
				{cursor, arrow},
				{keypress, true}
			       ]),
  create_menu(State),
  Xpos = 4, 
  Ypos1 = 60, 
  Width =  (?WINDOW_WIDTH - (Xpos*4)) div 3,
  create_labels([{mods,Ypos1-20,"Loaded Modules"}], Xpos + 1 + 3),
  Xpos2 = Xpos*2+Width,
  create_labels([{mod,Ypos1-20,"Module:"++atom_to_list(State#state.mod)},
		 {ver,Ypos1,""},
		 {time,Ypos1+20,""},
		 {native,Ypos1+40,""},
		 {compiling,Ypos1+60,""}
		],Xpos2),
  create_labels([{function,Ypos1-20,"Function:"},
		 {nativefun,Ypos1,""}
		 ],
		Xpos*3+Width*2),

  Ypos = 240,
  Height1 = ?WINDOW_HEIGHT - Ypos1 - Xpos,
  Height = ?WINDOW_HEIGHT - Ypos - Xpos,
  gs:listbox(code_listbox, win, [{x, Xpos},
				   {y, Ypos1},
				   {width, Width},
				   {height, Height1},
				   {bg, {255,255,255}},
				   {vscroll, right},
				   {hscroll, true},
				   {click, true}
			      ]),
  gs:listbox(module_listbox, win, [{x, Xpos*2+Width},
				   {y, Ypos},
				 {width, Width},
				 {height, Height},
				 {bg, {255,255,255}},
				 {vscroll, right},
				 {hscroll, true},
				 {click, true}
				  ]),
  gs:listbox(profile_listbox, win, [{x, Xpos*3+Width*2},
				   {y, Ypos1+40},
				 {width, Width},
				 {height, Height-60},
				 {bg, {255,255,255}},
				 {vscroll, right},
				 {hscroll, true},
				 {click, true}
				  ]),
  
  gs:button(compmod,win,[{label,{text,"Compile\nModule"}},
			 {justify,center},
			{x,Xpos*2+Width*1},
			 {height,60},
			 {y,Ypos-80}]),
  gs:button(prof,win,[{label,{text,"Profile\nModule"}},
			 {justify,center},
			{x,Xpos*2+Width*1+100},
			 {height,60},
			 {y,Ypos-80}]),
  gs:button(clearprof,win,[{label,{text,"Clear\nProfile"}},
			 {justify,center},
			{x,Xpos*2+Width*1+200},
			 {height,60},
			 {y,Ypos-80}]),
  gs:editor(edoc,win,[{x,Xpos*3+Width*2},
		  {y, Ypos},{width,Width},{height,Height},
                        {insert,{'end',"Edit this text!"}},
		      {vscroll,right},
		      {hscroll, true},
		     {wrap,none}]),
  State.

create_menu(State) ->
  gs:menubar(menubar, win, [{bg, ?DEFAULT_BG_COLOR}]),
  create_sub_menus([{mbutt,fmenu," File",
		     [{" Close    Ctrl-C ",close_menu}]},
		    {mbuttc,cmenu, " Compile ",
		     [{" Compile Module", comp_mod}]},
		    {mbuttp,pmenu, " Profile ",
		     [{" Profile Module", prof_mod}]},
		    {mbutte,emenu," Edoc",
		     [separator]},
		    {mbutta,amenu," Analyze ",[separator]},
		    {mbuttb,bmenu," Benchmark ",[separator]},		    
		    {mbuttj,jmenu," Jit ",[separator]}
		   ]),



  State.
create_menuitems(Parent,[{Text,Data}|Rest]) ->

  gs:menuitem(Parent, [{bg, ?DEFAULT_BG_COLOR},
		     {fg, {178, 34, 34}},
		     {label, {text, Text}},
		     {data, Data},
		     {underline, 1}
		    ]),
  create_menuitems(Parent,Rest);
create_menuitems(Parent,[separator|Rest]) ->
  gs:menuitem(Parent, [{itemtype, separator}]),
  create_menuitems(Parent,Rest);
create_menuitems(_,[]) -> ok.
create_sub_menus([{Parent,Name,Text,Items}|Rest]) ->
  gs:menubutton(Parent, menubar, [{bg, ?DEFAULT_BG_COLOR},
				 {fg, {178, 34, 34}},  % firebrick
				 {label, {text, Text}},
				 {underline, 1}
				]),
  gs:menu(Name, Parent, [{bg, ?DEFAULT_BG_COLOR},
			{fg, {178, 34, 34}}]),
  create_menuitems(Name,Items),
  create_sub_menus(Rest);
create_sub_menus([]) -> ok.

  
create_labels([{Name,Y,Text}|Rest],Xpos) ->    
  gs:label(Name, win, [{width, (?WINDOW_WIDTH - 16) div 3},
		       {height, 20},
		       {x, Xpos + 1 + 3},
		       {y, Y},
		       {bg, ?DEFAULT_BG_COLOR},
		       {fg, ?NORMAL_FG_COLOR},
		       {font, ?HEADER_FONT},
		       {align, w},
		       {label, {text, Text}}
		      ]),
  create_labels(Rest,Xpos);
create_labels([],_) -> ok.


update_code_listbox(State) ->
  Mods = lists:sort(mods()),
  case State#state.win_created of
    false ->
      State;
    true ->
      case Mods =:= State#state.mods of
	true -> State;
	false ->
	  update_text(mods,
		      "Loaded Modules ("++
		      integer_to_list(length(Mods))++")"),
	  catch gs:config(code_listbox, [{data, Mods},
					 {items, Mods},
					 {selection, 0}
					]),
	update_module_box(State#state{mods=Mods},0,Mods,"")  
      end
  end.
  

update_fun(State,Idx,Data)->
  case State#state.win_created of
    false ->
      State;
    true ->
      Fun = {M,F,A} = get_selection(Idx,Data,{hipe_tool,start,0}),
      update_text(function,"Function: "++mfa_to_string(Fun)),
      case in_native(F,A,native_code(M)) of
	true -> update_text(nativefun,"Native");
	_ -> update_text(nativefun,"Emulated")
      end,
		
      State
  end.

get_selection(Idx,Data,Default) ->
  case catch lists:nth(Idx+1,Data) of
    {'EXIT',_} ->
      Default;
    Val -> Val
  end.

update_module_box(State,Idx,Data,_Txt) ->
  case State#state.win_created of
    false ->
      State;
    true ->
      Mod = get_selection(Idx,Data,hipe_tool),
      %% io:format("~w\n",[Mod:module_info()]),

      Info = Mod:module_info(),
      Funs = lists:usort((funs(Mod))), 
      MFAs = mfas(Mod,Funs),
      ModText = atom_to_list(Mod),
      update_text(mod, "Module:"++ModText),
      update_text(compmod, "Compile\nModule\n"++ModText),
      Options = get_compile(Info),
      update_text(ver,get_version(Options)),
      update_text(time,get_time(Options)),
      NativeCode = native_code(Mod),

      Prof = is_profiled(Mod),
      if Prof -> 
	  update_text(prof,"Turn off\nProfiling");
	 true -> update_text(prof,"Profile\n"++ModText)
      end,
      
      Mode = get_mode(Funs, NativeCode),
      
      update_text(native, Mode),
      Items = fun_names(Mod, Funs, NativeCode, Prof),

      catch gs:config(module_listbox, [{data, MFAs},
				       {items, Items},
				       {selection, 0}
				      ]),
      ProfData = [mfa_to_string(element(1,X))++" "++
				integer_to_list(element(2,X))
		  || X <- hipe_profile:res(), element(2,X)>0],
      catch gs:config(profile_listbox, [{data, ProfData},
				       {items, ProfData},
				       {selection, 0}
				      ]),
      get_edoc(Mod),
      update_fun(
	State#state{mindex=Idx,mod=Mod,funs=Funs},
	0,MFAs)
  end.

update_text(Lab,Text) ->
  catch gs:config(Lab,[ {label, {text, Text}}]).
%% ____________________________________________________________________
%% @spec () -> [mod()]    
%% @doc	 Returns a list of all loaded modules. 
%@ ____________________________________________________________________
mods() ->
  [element(1,X) || X <- code:all_loaded()].

funs(Mod) -> 
  Mod:module_info(functions).

native_code(Mod) ->
  catch Mod:module_info(native_addresses).

mfas(Mod,Funs) ->
  [{Mod,F,A} || {F,A} <- Funs].

fun_names(M,Funs, NativeCode, Prof) ->
  [list_to_atom(atom_to_list(F)++
		"/"++integer_to_list(A) ++
		(case in_native(F,A,NativeCode) of
		     true -> " [native] ";
		     false -> ""
		 end)
		++
		if Prof -> 
			(catch integer_to_list(hipe_bifs:call_count_get({M,F,A})));
		   true -> ""
		end) ||
      {F,A} <- Funs].

in_native(F,A,NativeCode) ->
  lists:any(fun({Fun,Arity,_})->
		(Fun =:= F andalso Arity =:= A)
	    end,
	    NativeCode).

mfa_to_string({M,F,A}) ->
  atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A).

get_mode(Funs,NativeCode) ->
  case NativeCode of
    [] -> "Emulated";
    InNative when is_list(InNative) ->
      if length(InNative) =:= length(Funs) ->
	  "Native";
	 true -> "Mixed"
      end
  end.

get_time(Comp) ->
  case lists:keysearch(time,1,Comp) of
    {value,{_,{Y,Month,D,H,Min,S}}} ->
      integer_to_list(Y) ++
	"-" ++ integer_to_list(Month) ++
	"-" ++ integer_to_list(D) ++ " " ++
	integer_to_list(H) ++ ":" ++ integer_to_list(Min) ++
	":"  ++ integer_to_list(S);
    _ -> ""
  end.

get_version(Comp) ->
  case lists:keysearch(version,1,Comp) of
    {value,{_,V}} when is_list(V)-> V;
    _ -> ""
  end.
get_cwd(Options) ->
  case lists:keysearch(cwd,1,Options) of
    {value,{_,V}} when is_atom(V) -> atom_to_list(V);
    {value,{_,V}} -> V; 
    _ -> ""
  end.
get_options(Comp) ->
  case lists:keysearch(options,1,Comp) of
    {value,{_,V}} when is_list(V)-> V;
    _ -> ""
  end.

get_compile(Info) ->
  case lists:keysearch(compile,1,Info) of
    {value,{_,O}} when is_list(O)-> O;
    _ -> []
  end.

is_profiled(Mod) ->
  case hipe_bifs:call_count_get({Mod,module_info,0}) of
    false -> false;
    C when is_integer(C) -> true
  end.

compile(State) ->
  catch gs:config(compmod,[ {enable, false}]),
  update_text(compiling,"Compiling..."),
  Parent = self(),
  P = spawn(fun() ->
	    c(Parent,State#state.mod,State#state.options)
	end),
  State#state{compiling=P}.
	
c(Parent,Mod,Options) ->
  Res = hipe:c(Mod,Options),
  Parent ! {compilation_done,Res,self()},
  ok.

get_edoc(Mod) ->
  Info = Mod:module_info(),
  Comp = get_compile(Info), 
  Options = get_options(Comp),
  Dir = get_cwd(Options),
  File = 
    case Dir of
      "" ->  atom_to_list(Mod) ++ ".erl";
      _ -> Dir ++"/" ++ atom_to_list(Mod) ++ ".erl"
    end,
  % io:format("Get ~s\n",[File]),
  Text = case catch edoc(File,[{xml_export,
				xmerl_text},no_output]) of
	   {'EXIT',_} -> "error";
	   T -> T
	 end,
  gs:config(edoc,{enable, true}),
  gs:config(edoc,clear),
  gs:config(edoc,{insert, {insert, Text}}),
  gs:config(edoc,{enable, false}).


edoc(Name, Opts) -> 
  Doc = edoc:get_doc([Name, Opts]),
  %% Comments = edoc:read_comments(Name, Opts),
  %% Text = edoc:forms(Forms, Comments, Name, Opts),
  edoc:layout([Doc, Opts]).
