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


%%------------------------------------------------------------
%% Configuration macros
-define(CORBAMOD, corba).
-define(ORBNAME, orber).
-define(COMPILERVSN, "4.0.7").
-define(CORBAHRL, "corba.hrl").
-define(CALL, "call").
-define(CAST, "cast").
-define(IFRREGID, "register").
-define(IFRTYPESHRL, "ifr_types.hrl").

-define(GENSERVMOD, gen_server).


%%------------------------------------------------------------
%% Usefull macros

-define(ifthen(P,ACTION), if P -> ACTION; true->true end).


%%------------------------------------------------------------
%% Option macros

-define(ifopt(G,OPT,ACTION), 
	case icgen:get_opt(G,OPT) of true -> ACTION; _ -> ok end).

-define(ifopt2(G,OPT,ACT1,ACT2), 
	case icgen:get_opt(G,OPT) of true -> ACT1; _ -> ACT2 end).

-define(ifnopt(G,OPT,ACTION), 
	case icgen:get_opt(G,OPT) of false -> ACTION; _ -> ok end).


%% Internal record
-record(id_of, {id, type, tk}).

%%--------------------------------------------------------------------
%% The generator object definition

-record(genobj, {symtab, impl, options, warnings, auxtab,
		 tktab, pragmatab, c_typedeftab, keywtab,
		 skelfile=[], skelfiled=[], skelscope=[],
		 stubfile=[], stubfiled=[], stubscope=[],
		 includefile=[], includefiled=[],
		 interfacefile=[],interfacefiled=[],
		 helperfile=[],helperfiled=[],
		 holderfile=[],holderfiled=[], 
		 filestack=0, do_gen=true, sysfile=false}).

%%--------------------------------------------------------------------
%% The scooped id definition
-record(scoped_id,	{type=local, line=-1, id=""}).








%%--------------------------------------------------------------------
%% Secret macros
%%
%%	NOTE these macros are not general, they cannot be used
%%	everywhere.
%%
-define(lookup(T,K), case ets:lookup(T, K) of [{_X, _Y}] -> _Y; _->[] end).
-define(insert(T,K,V), ets:insert(T, {K, V})).




%%---------------------------------------------------------------------
%%
%% Java specific macros
%%
%%
-define(ERLANGPACKAGE,"com.ericsson.otp.erlang.").
-define(ICPACKAGE,"com.ericsson.otp.ic.").





