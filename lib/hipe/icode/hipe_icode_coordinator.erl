%%%-------------------------------------------------------------------
%%% File    : hipe_icode_coordinator.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%          This module coordinates an icode pass
%%% Created : 20 Feb 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_coordinator).

-export([coordinate/4]).

coordinate(CallGraph,Escaping,NonEscaping,Mod) ->
  ServerPid = initialize_server(Escaping,Mod),
  Clean = [MFA || {MFA, _} <- Escaping],
  coordinate(Clean,[],Clean++NonEscaping,CallGraph,gb_trees:empty(),ServerPid,Mod),
  receive
    {stop,AnsPid} ->
      ServerPid ! stop,
      AnsPid ! {done,self()}
  end.

coordinate([],Available,[],_CG,PM,ServerPid,Mod) ->
  lists:foreach(fun(MFA) -> gb_trees:get(MFA,PM) ! 
			      {done,final_funs(ServerPid,Mod)} end, Available);
coordinate(Needed,Available,Busy,CG,PM,ServerPid,Mod) ->
  receive 
    {stop,AnsPid} ->
      ServerPid ! stop,
      AnsPid ! {done,self()};
    Message1 -> 
      %%io:format("Receiving Message1:~n~p~n~n", [Message1]),
      handle_message(Message1,Needed,Available,Busy,CG,PM,ServerPid,Mod)
  end.

handle_message(Message,Needed,Available,Busy,CG,PM,ServerPid,Mod) ->
  case Message of
    {call,NewCallTypes,MFA} ->
      case lists:member(MFA,Available++Busy) of
	true ->
	  case update_call_type(MFA,NewCallTypes,ServerPid) of
	    do_restart ->
	      case lists:member(MFA,Available) of
		true ->
		  restart_fun(MFA,PM,ServerPid),
		  coordinate(Needed,Available -- [MFA],[MFA|Busy],CG,PM,ServerPid,Mod);
		false ->
		  case lists:member(MFA,Needed) of
		    true ->
		      coordinate(Needed,Available,Busy,CG,PM,ServerPid,Mod);
		    false ->
		      coordinate([MFA|Needed],Available,Busy,CG,PM,ServerPid,Mod)
		  end
	      end;
	    no_change ->
	      coordinate(Needed,Available,Busy,CG,PM,ServerPid,Mod)
	  end;
	false ->
	  coordinate(Needed,Available,Busy,CG,PM,ServerPid,Mod)
      end;
    {ready,{MFA,Pid}} ->
      NewPM = gb_trees:insert(MFA,Pid,PM), 
      case lists:member(MFA,Needed) of
	true ->
	  restart_fun(MFA,NewPM,ServerPid),
	  coordinate(Needed -- [MFA],Available,Busy,CG,NewPM,ServerPid,Mod);
	false ->
	  coordinate(Needed,[MFA|Available],Busy--[MFA],CG,NewPM,ServerPid,Mod)
      end; 
    {done,NewReturnType,MFA} ->
      case update_return_type(MFA,NewReturnType,ServerPid) of
	do_restart ->
	  AV2 = Available ++ [MFA],
	  B2 = Busy -- [MFA],
	  Restarts  = hipe_digraph:get_parents(MFA,CG),
	  N2 =  ordsets:from_list(Restarts ++ Needed),
	  Now = [Avail || Avail <- N2, lists:member(Avail,AV2)],
	  lists:foreach(fun(MFA) -> restart_fun(MFA,PM,ServerPid) end, Now),
	  coordinate(N2 -- Now, AV2 -- Now, B2 ++ Now, 
		     CG, PM, ServerPid,Mod);
	no_change ->
	  case lists:member(MFA,Needed) of
	    true ->
	      restart_fun(MFA,PM,ServerPid),
	      coordinate(Needed -- [MFA], Available, Busy, CG, PM, ServerPid,Mod);
	    false ->
	      coordinate(Needed, [MFA|Available], Busy -- [MFA], 
			 CG, PM, ServerPid, Mod)
	  end
      end
  end.

initialize_server(Escaping, Mod) ->
  Pid = spawn_link(fun() -> info_server(Mod) end),
  lists:foreach(fun({MFA,_}) -> 
		    Pid ! {set_escaping,MFA}
		end,Escaping),
  Pid.
 
safe_get_args(MFA,Cfg,Pid,Mod) ->
  Mod:replace_nones(get_args(MFA,Cfg,Pid)).

get_args(MFA,Cfg,Pid) ->
  Ref = make_ref(),
  Pid ! {get_call,MFA,Cfg,self(),Ref},
  receive
    {Ref,Types} ->
      Types
  end.

safe_get_res(MFA,Pid,Mod) ->
  Mod:replace_nones(get_res(MFA,Pid)).

get_res(MFA,Pid) ->
  Ref = make_ref(),
  Pid ! {get_return,MFA,self(),Ref},
  receive
    {Ref,Types} ->
      Types
  end.


update_return_type(MFA,NewType,Pid) ->
  Ref = make_ref(),
  Pid ! {update_return,MFA,NewType,self(),Ref}, 
  receive 
    {Ref,Ans} ->
      Ans
  end.

update_call_type(MFA,NewTypes,Pid) ->
  Ref = make_ref(),
  Pid ! {update_call,MFA,NewTypes,self(),Ref}, 
  receive 
    {Ref,Ans} ->
      Ans
  end.

restart_fun(MFA,PM,ServerPid) ->
  gb_trees:get(MFA,PM) ! {analyse,analysis_funs(ServerPid)}.

analysis_funs(Pid) ->
  Self = self(),
  ArgsFun = fun(MFA,Cfg) -> get_args(MFA,Cfg,Pid) end,
  GetResFun = fun(MFA,Args) -> 
		  Self ! {call,Args,MFA},
		  get_res(MFA,Pid)
	      end,
  FinalFun = fun(MFA,RetTypes) ->
		 Self ! {done,RetTypes,MFA} 
	     end,
  {ArgsFun,GetResFun,FinalFun}. 

final_funs(Pid,Mod) ->
  ArgsFun = fun(MFA,Cfg) -> safe_get_args(MFA,Cfg,Pid,Mod) end,
  GetResFun = fun(MFA,_) ->   
		  safe_get_res(MFA,Pid,Mod)
	      end,
  FinalFun = fun(_,_) -> ok end,
  {ArgsFun,GetResFun,FinalFun}. 

info_server(Mod) ->
  info_server_loop(gb_trees:empty(),gb_trees:empty(),Mod).

info_server_loop(CallInfo,ReturnInfo,Mod) ->
  receive
    {update_return,MFA,NewInfo,Pid,Ref} ->
      NewReturnInfo = handle_update(MFA,ReturnInfo,NewInfo,Pid,Ref,Mod),
      info_server_loop(CallInfo,NewReturnInfo,Mod);
    {update_call,MFA,NewInfo,Pid,Ref} ->
      NewCallInfo = handle_update(MFA,CallInfo,NewInfo,Pid,Ref,Mod),
      info_server_loop(NewCallInfo,ReturnInfo,Mod);
    {get_return,MFA,Pid,Ref} ->
      Ans = 
	case gb_trees:lookup(MFA,ReturnInfo) of 
	  none ->
	    Mod:return_none();
	  {value,Types} ->
	    Mod:return__info(Types)
	end,
      Pid ! {Ref,Ans},
      info_server_loop(CallInfo,ReturnInfo,Mod);
    {get_call,MFA,Cfg,Pid,Ref} ->
      Ans = 
	case gb_trees:lookup(MFA,CallInfo) of 
	  none ->
	    Mod:return_none_args(Cfg,MFA);
	  {value,escaping} ->
	    Mod:return_any_args(Cfg,MFA);
	  {value,Types} ->
	    Mod:return__info(Types)
	end,
      Pid ! {Ref,Ans},
      info_server_loop(CallInfo,ReturnInfo,Mod);
    {set_escaping,MFA} ->
      info_server_loop(gb_trees:enter(MFA,escaping,CallInfo),ReturnInfo,Mod);
    stop ->
      ok
  end.

handle_update(MFA,Tree,NewInfo,Pid,Ref,Mod) ->
  case gb_trees:lookup(MFA,Tree) of
    none ->
      %%io:format("First Type: ~w ~w~n", [NewType, MFA]),
      Pid ! {Ref,do_restart},
      ResType = Mod:new__info(NewInfo);
    {value,escaping} ->
      Pid ! {Ref, no_change},
      ResType = escaping;
    {value,OldInfo} ->
      %%io:format("New Type: ~w ~w~n", [NewType,MFA]),
      %%io:format("Old Type: ~w ~w~n", [OldType,MFA]),
      case Mod:update__info(NewInfo,OldInfo) of
	{true, ResType} ->
	  Pid ! {Ref, no_change};
	{false, ResType} ->
	  Pid ! {Ref,do_restart}
      end 
  end,
  gb_trees:enter(MFA,ResType,Tree).

