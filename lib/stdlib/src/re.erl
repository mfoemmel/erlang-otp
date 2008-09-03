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
-module(re).
-export([grun/2,grun/3,replace/3,replace/4]).

replace(Subject,RE,Replacement) ->
    replace(Subject,RE,Replacement,[]).
replace(Subject,RE,Replacement,Options) ->
    {NewOpt,Convert,Unicode} =
	case (catch process_repl_params(Options,iodata,false)) of
	    badopt ->
		erlang:error(badarg,[Subject,RE,Replacement,Options]);
	    Good ->
		Good
	end,
    FlatSubject = 
	case is_binary(Subject) of
	    true ->
		Subject;
	    false ->
		case Unicode of
		    true ->
			to_utf8(Subject);
		    false ->
			iolist_to_binary(Subject)
		end
	end,
    case do_replace(FlatSubject,Subject,RE,Replacement,NewOpt) of
	{error, Err} ->
	    {error, Err};
	IoList ->
	    case Convert of
		iodata ->
		    IoList;
		binary ->
		    iolist_to_binary(IoList);
		list ->
		    case Unicode of
			false ->
			    binary_to_list(iolist_to_binary(IoList));
			true ->
			    utf8_to_list(iolist_to_binary(IoList))
		    end
	    end
    end.
    

do_replace(FlatSubject,Subject,RE,Replacement,Options) ->
    case re:run(FlatSubject,RE,Options) of
	{error, Error} ->
	    {error, Error};
	nomatch ->
	    Subject;
	{match,[Mlist|T]} when is_list(Mlist) ->
	    apply_mlist(FlatSubject,Replacement,[Mlist|T]);
	{match,Slist} ->
	    apply_mlist(FlatSubject,Replacement,[Slist])
    end.

process_repl_params([],Convert,Unicode) ->
    {[],Convert,Unicode};
process_repl_params([unicode|T],C,_U) ->
    {NT,NC,NU} = process_repl_params(T,C,true), 
    {[unicode|NT],NC,NU};
process_repl_params([{capture,_,_}|_],_,_) ->
    throw(badopt);
process_repl_params([{capture,_}|_],_,_) ->
    throw(badopt);
process_repl_params([{return,iodata}|T],_C,U) ->
    process_repl_params(T,iodata,U);
process_repl_params([{return,list}|T],_C,U) ->
    process_repl_params(T,list,U);
process_repl_params([{return,binary}|T],_C,U) ->
    process_repl_params(T,binary,U);
process_repl_params([{return,_}|_],_,_) ->
    throw(badopt);
process_repl_params([H|T],C,U) ->
    {NT,NC,NU} = process_repl_params(T,C,U),
    {[H|NT],NC,NU}.

apply_mlist(Subject,Replacement,Mlist) ->
    do_mlist(Subject,Subject,0,precomp_repl(iolist_to_binary(Replacement)),Mlist).


precomp_repl(<<>>) ->
    [];
precomp_repl(<<$\\,X,Rest/binary>>) when X < $1 ; X > $9 ->
    % Escaped character
    case precomp_repl(Rest) of
	[BHead | T0] when is_binary(BHead) ->
	    [<<X,BHead/binary>> | T0];
	Other ->
	    [<<X>> | Other]
    end;
precomp_repl(<<$\\,Rest/binary>>) when byte_size(Rest) > 0->
    {NS,NRest} = pick_int(Rest),
    [list_to_integer(NS) | precomp_repl(NRest)];
precomp_repl(<<$&,Rest/binary>>) ->
    [0 | precomp_repl(Rest)];
precomp_repl(<<X,Rest/binary>>) ->
    case precomp_repl(Rest) of
	[BHead | T0] when is_binary(BHead) ->
	    [<<X,BHead/binary>> | T0];
	Other ->
	    [<<X>> | Other]
    end.
    


pick_int(<<X,R/binary>>) when X >= $0, X =< $9 ->
    {Found,Rest} = pick_int(R),
    {[X|Found],Rest};
pick_int(Bin) ->
    {[],Bin}.

do_mlist(_,<<>>,_,_,[]) ->
    []; %Avoid empty binary tail
do_mlist(_,Subject,_,_,[]) ->
    Subject;
do_mlist(Whole,Subject,Pos,Repl,[[{MPos,Count} | Sub] | Tail]) 
  when MPos > Pos ->
    EatLength = MPos - Pos,
    <<Untouched:EatLength/binary, Rest/binary>> = Subject,
    [Untouched | do_mlist(Whole,Rest, MPos, Repl, 
			  [[{MPos,Count} | Sub] | Tail])];
do_mlist(Whole,Subject,Pos,Repl,[[{MPos,Count} | Sub] | Tail]) 
  when MPos =:= Pos ->
    EatLength = Count,
    <<_:EatLength/binary,Rest/binary>> = Subject,
    NewData = do_replace(Whole,Repl,[{MPos,Count} | Sub]),
    [NewData | do_mlist(Whole,Rest,Pos+EatLength,Repl,Tail)].


do_replace(_,[Bin],_) when is_binary(Bin) ->
    Bin;
do_replace(Subject,Repl,SubExprs0) ->
    SubExprs = list_to_tuple(SubExprs0),
    [ case Part of
	  N when is_integer(N) ->
	      if
		  tuple_size(SubExprs) =< N ->
		      <<>>;
		  true ->
		      {SPos,SLen} = element(N+1,SubExprs),
		      if 
			  SPos < 0 ->
			      <<>>;
			  true ->
			      <<_:SPos/binary,Res:SLen/binary,_/binary>> = Subject,
			      Res
		      end
	      end;
	  Other ->
	      Other
      end || Part <- Repl ].


check_for_unicode({re_pattern,_,1,_},_) ->
    true;
check_for_unicode({re_pattern,_,0,_},_) ->
    false;
check_for_unicode(_,L) ->
    lists:member(unicode,L).
    
% SelectReturn = false | all | stirpfirst | none 
% ConvertReturn = index | list | binary
% {capture, all} -> all (untouchded)
% {capture, first} -> kept in argumentt list and Select all
% {capture, all_but_first} -> removed from argument list and selects stripfirst
% {capture, none} ->  removed from argument list and selects none
% {capture, []} -> removed from argument list and selects none
% {capture,[...]} -> 0 added to selection list and selects stripfirst
% SelectReturn false is same as all in the end.

% Call as process_parameters([],0,false,index,NeedClean)

process_parameters([],InitialOffset, SelectReturn, ConvertReturn,_) ->
    {[], InitialOffset, SelectReturn, ConvertReturn};
process_parameters([{offset, N} | T],_Init0,Select0,Return0,CC) ->
    process_parameters(T,N,Select0,Return0,CC);
process_parameters([global | T],Init0,Select0,Return0,CC) ->
    process_parameters(T,Init0,Select0,Return0,CC);
process_parameters([{capture,Values,Type}|T],Init0,Select0,_Return0,CC) ->
    process_parameters([{capture,Values}|T],Init0,Select0,Type,CC);
process_parameters([{capture,Values}|T],Init0,Select0,Return0,CC) ->
    % First process the rest to see if capture was already present
    {NewTail, Init1, Select1, Return1} = 
	process_parameters(T,Init0,Select0,Return0,CC),
    case Select1 of
	false ->
	    case Values of
		all ->
		    {[{capture,all} | NewTail], Init1, all, Return0}; 
		first ->
		    {[{capture,first} | NewTail], Init1, all, Return0};
		all_but_first ->
		    {[{capture,all} | NewTail], Init1, stripfirst, Return0};
		none ->
		    {[{capture,first} | NewTail], Init1, none, Return0};
		[] ->
		    {[{capture,first} | NewTail], Init1, none, Return0};
		List when is_list(List) ->
		    {[{capture,[0|List]} | NewTail], 
		     Init1, stripfirst, Return0};
		_ ->
		    throw(badlist)
	    end;
	_ ->
	    % Found overriding further down list, ignore this one
	    {NewTail, Init1, Select1, Return1}
    end;
process_parameters([H|T],Init0,Select0,Return0,true) ->
    case copt(H) of
	true ->
	    process_parameters(T,Init0,Select0,Return0,true);
	false ->
	    {NewT,Init,Select,Return} =
		process_parameters(T,Init0,Select0,Return0,true),	
	    {[H|NewT],Init,Select,Return}
    end;
process_parameters([H|T],Init0,Select0,Return0,false) ->
    {NewT,Init,Select,Return} =
		process_parameters(T,Init0,Select0,Return0,false),
    {[H|NewT],Init,Select,Return};
process_parameters(_,_,_,_,_) ->
    throw(badlist).

postprocess(nomatch,_,_,_,_) ->
    nomatch;
postprocess({match,[]},_,_,_,_) ->
    nomatch;
postprocess({match,_},none,_,_,_) ->
    match;
postprocess({match,M},Any,binary,Flat,Uni) ->
    binarify(postprocess({match,M},Any,index,Flat,Uni),Flat);
postprocess({match,M},Any,list,Flat,Uni) ->
    listify(postprocess({match,M},Any,index,Flat,Uni),Flat,Uni);
postprocess({match,M},all,index,_,_) ->
    {match,M};
postprocess({match,M},false,index,_,_) ->
    {match,M};
postprocess({match,M},stripfirst,index,_,_) ->
    {match, [ T || [_|T] <- M ]}.

binarify({match,M},Flat) ->
    {match, [ [ case {I,L} of
		    {-1,0} ->
			<<>>;
		    {SPos,SLen} ->
			<<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
			Res
		end || {I,L} <- One ] || One <- M ]}.
listify({match,M},Flat,Uni) ->
    {match, [ [ case {I,L} of
	    {_,0} ->
		[];
	    {SPos,SLen} ->
		case Uni of
		    true ->
			<<_:SPos/binary,Res:SLen/binary,_/binary>> = Flat,
			utf8_to_list(Res);
		    false ->
			Start = SPos + 1,
			End = SPos + SLen,
			binary_to_list(Flat,Start,End)
		end
	end || {I,L} <- One ] || One <- M ]}.


grun(A,B) ->
    grun(A,B,[]).

grun(Subject,RE,{Options,NeedClean}) when is_binary(Subject) ->
    do_grun(Subject,Subject,check_for_unicode(RE,Options),RE,{Options,NeedClean});

grun(Subject,RE,{Options,NeedClean}) ->
    Unicode = check_for_unicode(RE,Options),
    FlatSubject = 
	case Unicode of
	    true ->
		to_utf8(Subject);
	    false ->
		iolist_to_binary(Subject)
	end,
    do_grun(FlatSubject,Subject,Unicode,RE,{Options,NeedClean}).

do_grun(FlatSubject,Subject,Unicode,RE,{Options0,NeedClean}) ->
    {StrippedOptions, InitialOffset,
     SelectReturn, ConvertReturn} = 
	case (catch 
		  process_parameters(Options0, 0, false, index, NeedClean)) of
	    badlist ->
		erlang:error(badarg,[Subject,RE,Options0]);
	    CorrectReturn ->
		CorrectReturn
	end,
    try
	postprocess(loopexec(FlatSubject,RE,InitialOffset,byte_size(FlatSubject),
			     Unicode,StrippedOptions),
		    SelectReturn,ConvertReturn,FlatSubject,Unicode)
    catch
	error:ErReason ->
	    erlang:raise(error,ErReason,erlang:get_stacktrace());
	_:_ ->
	    erlang:error(badarg,[Subject,RE,Options0])
    end.
    
int_to_utf8(I) when I =< 16#7F ->
    <<I>>;
int_to_utf8(I) when I =< 16#7FF ->
    B2 = I band 16#3f,
    B1 = (I bsr 6) band 16#1f,  
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when I =< 16#FFFF ->
    B3 = I band 16#3f,
    B2 = (I bsr 6) band 16#3f,
    B1 = (I bsr 12) band 16#f,
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when I =< 16#10FFFF ->
    B4 = I band 16#3f,
    B3 = (I bsr 6) band 16#3f,
    B2 = (I bsr 12) band 16#3f,
    B1 = (I bsr 18) band 16#7,
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(_) ->
    exit(unsupported_utf8).

to_utf8(X) when is_binary(X) ->
    X;
to_utf8([]) ->
    <<>>;
to_utf8(I) when is_integer(I) ->
    int_to_utf8(I);
to_utf8([Head | Tail]) ->
    HBin = to_utf8(Head),
    TBin = to_utf8(Tail),
    <<HBin/binary,TBin/binary>>.

utf8_to_list(<<>>) ->
    [];
utf8_to_list(Bin) ->
    N = utf8_siz(Bin),
    <<X:N/binary,Rest/binary>> = Bin,
    [utf8_to_int(X) | utf8_to_list(Rest)].
utf8_siz(<<0:1,_:7,_/binary>>) ->
    1;
utf8_siz(<<1:1,1:1,0:1,_:5,_/binary>>) ->
    2;
utf8_siz(<<1:1,1:1,1:1,0:1,_:4,_/binary>>) ->
    3;
utf8_siz(<<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>) ->
    4.

utf8_to_int(<<0:1,B:7>>) ->
    B;
utf8_to_int(<<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>) ->
    (B1 bsl 6) bor B2;
utf8_to_int(<<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>) ->
    (B1 bsl 12) bor (B2 bsl 6) bor B3;
utf8_to_int(<<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,
             B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>) ->
    Res = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
    case Res of
        X when X > 16#10FFFF ->
            exit(unsupported_utf8);
        Other ->
            Other
    end;
utf8_to_int(_) ->
    exit(unsupported_utf8).


loopexec(_,_,X,Y,_,_) when X > Y ->
    {match,[]};
loopexec(Subject,RE,X,Y,Unicode,Options) ->
    case re:run(Subject,RE,[{offset,X}]++Options) of
	nomatch ->
	    {match,[]};
	{match,[{A,B}|More]} ->
	    {match,Rest} = 
		case B>0 of
		    true ->
			loopexec(Subject,RE,A+B,Y,Unicode,Options);
		    false ->
			{match,M} = 
			    case re:run(Subject,RE,[{offset,X},notempty,
						anchored]++Options) of
				nomatch ->
				    {match,[]};
				{match,Other} ->
				    {match,Other}
			    end,
			NewA = case M of 
				   [{_,NStep}|_] when NStep > 0 ->
				       A+NStep;
				   _ ->
				       forward(Subject,A,1,Unicode)
			       end,
			{match,MM} = loopexec(Subject,RE,NewA,Y,
					      Unicode,Options),
			case M of 
			    [] ->
				{match,MM};
			    _ ->
				{match,[M | MM]}
			end
		end,
	    {match,[[{A,B}|More] | Rest]}
    end.
    
forward(_Chal,A,0,_) ->
    A;
forward(_Chal,A,N,false) ->
    A+N;
forward(Chal,A,N,true) ->
    <<_:A/binary,Tl/binary>> = Chal,
    Forw = case Tl of
	       <<1:1,1:1,0:1,_:5,_/binary>>  ->
		   2;
	       <<1:1,1:1,1:1,0:1,_:4,_/binary>>  ->
		   3;
	       <<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>  ->
		   4;
	       _ ->
		   1
	   end,
    forward(Chal,A+Forw,N-1,true).

copt(caseless) ->
    true;
copt(dollar_endonly) ->
    true;
copt(dotall) ->
    true;
copt(extended) ->
    true;
copt(firstline) ->
    true;
copt(multiline) ->
    true;
copt(no_auto_capture) ->
    true;
copt(dupnames) ->
    true;
copt(ungreedy) ->
    true;
copt(unicode) ->
    true;
copt(_) ->
    false.
