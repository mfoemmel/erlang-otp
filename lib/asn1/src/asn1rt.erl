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
-module(asn1rt).

%% Runtime functions for ASN.1 (i.e encode, decode)

%%-compile(export_all).
-export([encode/2,encode/3,decode/3]).
    
encode(Module,{Type,Term}) ->
    encode(Module,Type,Term).

encode(Module,Type,Term) ->
    case catch apply(Module,encoding_rule,[]) of
	{'EXIT',_} ->
	    {error,{asn1,{undef,Module}}};
	Rule ->
	    encode(Rule,Module,Type,Term)
    end.
	    
encode(ber,Module,Type,Term) ->
    Call = list_to_atom(lists:concat(['enc_',Type])),
    case catch apply(Module,Call,[Term,[]]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Call}}};
	{'EXIT',{error,Reason}} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,{asn1,Reason}};
	{Bytes,Len} ->
	    {ok,Bytes};
%%	    {ok,binary_to_list(Bytes)};%% for test_server binaries
	X ->
	    {ok,X}
%%	    {ok,binary_to_list(X)} %% for test_server binaries
    end;
encode(ber_bin,Module,Type,Term) ->
    Call = list_to_atom(lists:concat(['enc_',Type])),
    case catch apply(Module,Call,[Term,[]]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Call}}};
	{'EXIT',{error,Reason}} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,{asn1,Reason}};
	{Bytes,Len} ->
%%	    if 
%%		list(Bytes) ->
%%%%		    {ok,binary_to_list(list_to_binary(Bytes))};
%%		    {ok,list_to_binary(Bytes)};
%%		true ->
%%%%		    {ok,binary_to_list(Bytes)}
%%		    {ok,Bytes}
%%	    end;
	    {ok,Bytes};
	X ->
	    {ok,X}
%%	    if 
%%		list(X) ->
%%%%		    {ok,binary_to_list(list_to_binary(X))};
%%		    {ok,list_to_binary(X)};
%%		true ->
%%%%		    {ok,binary_to_list(X)}
%%		    {ok,X}
%%	    end
    end;
encode(per,Module,Type,Term) ->
    Call = list_to_atom(lists:concat(['enc_',Type])),
    case catch asn1rt_per:complete(apply(Module,Call,[Term])) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Call}}};
	{'EXIT',{error,Reason}} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,{asn1,Reason}};
	X ->
	    {ok,X}
    end.

%%decode(Module,Type,Bytes) ->
%%    Call = list_to_atom(lists:concat(['dec_',Type])),
%%    case apply(Module,encoding_rule,[]) of
%%	per ->
%%	    case catch apply(Module,Call,[Bytes,mandatory]) of
%%		{'EXIT',undef} ->
%%		    {error,{asn1,{undef,Module,Call}}};
%%		{'EXIT',{error,Reason}} ->
%%		    {error,Reason};
%%		{'EXIT',Reason} ->
%%		    {error,Reason};
%%		{X,_Rest} ->
%%		    {ok,X};
%%		{X,_Rest,_Len} ->
%%		    {ok,X}
%%	    end;
%%	_->
%%	    case catch apply(Module,Call,
%%			     [list_to_binary(Bytes),mandatory]) of
%%		{'EXIT',undef} ->
%%		    {error,{asn1,{undef,Module,Call}}};
%%		{'EXIT',{error,Reason}} ->
%%		    {error,Reason};
%%		{'EXIT',Reason} ->
%%		    {error,Reason};
%%		{X,_Rest} ->
%%		    {ok,X};
%%		{X,_Rest,_Len} ->
%%		    {ok,X}
%%	    end
%%    end.

decode(Module,Type,Bytes) ->
    Call = list_to_atom(lists:concat(['dec_',Type])),
    case catch apply(Module,Call,[Bytes,mandatory]) of
	{'EXIT',undef} ->
	    {error,{asn1,{undef,Module,Call}}};
	{'EXIT',{error,Reason}} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,Reason};
	{X,_Rest} ->
	    {ok,X};
	{X,_Rest,_Len} ->
	    {ok,X}
    end.
	


