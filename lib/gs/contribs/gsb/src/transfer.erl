%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	transfer.erl
%%  Module   :	transfer
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-10-29 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(transfer).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.2 $').
-export([make/1]).

make(Filename) ->
  {ok,List} = file:consult(Filename),
  {ok,Out} = file:open(Filename, write),
  write_top(Out, List),
  file:close(Out).

write_top(Fd,[{N,C,G,M,E,Props,Events,Children}|Rest]) ->
  NewProps = remove(Props, []),
  io:format(Fd,"{~w,~w,~w,~w,~w,~w,~w,[",[N,C,G,M,E,NewProps,Events]),
  write_children(Fd,Children),
  io:format(Fd,"]}. ~n", []),
  write_top(Fd,Rest);
write_top(Fd,[]) ->true.

write_children(Fd,[{N,C,G,M,E,Props,Events,Children}]) ->
  NewProps = remove(Props,[]),
  io:format(Fd,"{~w,~w,~w,~w,~w,~w,~w,[",[N,C,G,M,E,NewProps,Events]),
  write_children(Fd,Children),
  io:format(Fd,"]}",[]);
write_children(Fd,[{N,C,G,M,E,Props,Events,Children}|Rest]) ->
  NewProps = remove(Props,[]),
  io:format(Fd,"{~w,~w,~w,~w,~w,~w,~w,[",[N,C,G,M,E,NewProps,Events]),
  write_children(Fd,Children),
  io:format(Fd,"]},",[]),
  write_children(Fd, Rest);
write_children(Fd,[]) -> true.

remove([{activebg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{activefg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{activebw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{highlightbg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{highlightfg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{highlightbw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{disabledbg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{disabledfg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{disabledbw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{insertbg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{insertfg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{insertbw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{selectbg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{selectfg,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{selectbw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{anchor,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{relief,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{bw,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{padx,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{pady,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{wraplength,_}|Rest],Acc) -> remove(Rest,Acc);
remove([{xselection,_}|Rest],Acc) -> remove(Rest,Acc);
remove([Other|Rest],Acc) -> remove(Rest,[Other|Acc]);
remove([],Acc) -> Acc.
