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
%% Purpose : Basically Core Erlang as records.

%% It would be nice to incorporate some generic functions as well but
%% this could make including this file difficult.
%% N.B. the annotation field is ALWAYS the first field!

-record(c_int, {anno=[],val}).
-record(c_float, {anno=[],val}).
-record(c_atom, {anno=[],name}).
-record(c_char, {anno=[],val}).
-record(c_string, {anno=[],val}).
-record(c_var, {anno=[],name}).
-record(c_nil, {anno=[]}).

-record(c_tuple, {anno=[],es}).
-record(c_cons, {anno=[],head,tail}).
-record(c_vector, {anno=[],es}).		%Only used for multiple values

-record(c_alias, {anno=[],var,pat}).

-record(c_local, {anno=[],name,arity}).
-record(c_remote, {anno=[],mod,name,arity}).
-record(c_internal, {anno=[],name,arity}).

-record(c_mdef, {anno=[],name,exports,attributes,body}).
-record(c_fdef, {anno=[],func,arity,body}).

-record(c_let, {anno=[],vars,arg,body}).
-record(c_seq, {anno=[],arg,body}).
-record(c_case, {anno=[],arg,clauses}).
-record(c_clause, {anno=[],pat,guard,body}).
-record(c_fun, {anno=[],vars,body}).
-record(c_call, {anno=[],op,args}).
-record(c_try, {anno=[],expr,vars,body}).
-record(c_catch, {anno=[],body}).
-record(c_receive, {anno=[],clauses,timeout,action}).

%%c_get_anno(Thing) -> element(2, Thing).
%%c_set_anno(Thing, Anno) -> setelement(2, Thing, Anno).
