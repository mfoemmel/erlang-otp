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
%% ------------------------------------------------------------
%%
%% Database interface for `gtk'.
%% 
%% ------------------------------------------------------------

-module(gtk_db).

-compile(export_all).
%-export([init/0, insert/3, lookup/3, counter/2]).
%-export([insert_widget/8, insert_event/3]).

-include("gtk.hrl").


%% ------------------------------------------------------------
%%                      INITIALIZATION
%% ------------------------------------------------------------

init(Opts) ->
    put(events,ets:new(gtk_db, [public, set])),
    put(kids,ets:new(gtk_db, [public, bag])),
    put(defaults,ets:new(gtk_db, [public, bag])),
    put(deleted,ets:new(gtk_db, [public, bag])),
    put(options,ets:new(gtk_db, [public, set])),
    DB=ets:new(gtk_db, [public, set]).

%% -----------------------------------------------------------------
%%                 PRIMITIVE DB INTERFACE
%% -----------------------------------------------------------------

insert(DB, Key, Value) ->
    ets:insert(DB, {Key, Value}).


lookup(DB, Key) ->
    Result =
	case ets:lookup(DB, Key) of
	    [{Key, Value}] -> Value;
	    _ -> undefined
	end,
    Result.


delete(DB, Key) ->
    ets:delete(DB, Key).


dump(DB) ->
    ets:match(DB, '$1').



%% -----------------------------------------------------------------
%%               NOT SO PRIMITIVE DB INTERFACE
%% -----------------------------------------------------------------

%% -----------------------------------------------------------------
%%                  HANDLE EVENTS
%% -----------------------------------------------------------------
insert_event(DB, Gtkid, Etype, Edata) ->
    ID = Gtkid#gtkid.id,
    Rdata =
	case Edata of
	    [] -> opt(DB,ID,data);
	    Other1 -> Edata
	end,
    Events = lookup_events(DB, ID),
    case lists:keysearch(Etype, 2, Events) of
	{value, {Etag, _, _}} ->
	    NewEvents =
		lists:keyreplace(Etype, 2, Events, {Etag, Etype, Rdata}),
	    ets:insert(get(events), {{events, ID}, NewEvents}),
	    [$#, gtk:to_ascii(ID), " ", Etag];
	Other2 ->
	    Etag = etag(Etype),
	    NewEvents = [{Etag, Etype, Rdata} | Events],
	    ets:insert(get(events), {{events, ID}, NewEvents}),
	    [$#, gtk:to_ascii(ID), " ", Etag]
    end.

etag(Etype) ->
    case Etype of
	click -> "c";
	doubleclick -> "dc";
	configure -> "co";
	enter -> "e";
	leave -> "l";
	motion -> "m";
	buttonpress -> "bp";
	buttonrelease -> "br";
	focus -> "f";
	destroy -> "d";
	keypress -> "kp";
	keyrelease -> "kr"
    end.

lookup_events(DB, ID) ->
    case lookup(get(events), {events, ID}) of
	undefined -> [];
	Events -> Events
    end.
   
lookup_event(DB, ID, Etag) ->
    case lists:keysearch(Etag, 1, lookup_events(DB, ID)) of
	{value, {Etag, Etype, Edata}} ->
	    {Etype, Edata};
	Other ->
	    nonexisting_event
    end.

delete_event(DB, Gtkid, Etype) ->
    ID = Gtkid#gtkid.id,
    NewEvents = lists:keydelete(Etype, 2, lookup_events(DB, ID)),
    ets:insert(get(events), {{events, ID}, NewEvents}).

%% -----------------------------------------------------------------
%%                  HANDLE BUTTON GROUPS
%% -----------------------------------------------------------------
insert_bgrp(DB, Key) ->
    case ets:lookup(DB, Key) of
	[] ->
	    {Bgrp, RG, Owner} = Key,
	    insert(DB, Key, {0, RG}),
	    RG;
	[{_, {Counter, RG}}] ->
	    insert(DB, Key, {Counter+1, RG}),
	    RG
    end.


lookup_bgrp(DB, Key) ->
    case ets:lookup(DB, Key) of
	[{_, {_, RG}}] ->
	    RG;
	Other ->
	    gs:error("Something strange in lookup_bgrp ~p~n", [Other]),
	    true
    end.


delete_bgrp(DB, Key) ->
    case ets:lookup(DB, Key) of
	[] ->
	    true;
	[{_, {0, RG}}] ->
	    delete(DB, Key),
	    true;
	[{_, {Counter, RG}}] ->
	    insert(DB, Key, {Counter-1, RG}),
	    true
    end.


%% -----------------------------------------------------------------
%%  insert things

update_widget(DB, Gtkid) ->
    ID = Gtkid#gtkid.id,
    insert(DB, ID, Gtkid),
    Gtkid.

insert_gs(DB,Gtkid) ->
    update_widget(DB,Gtkid).
    
insert_widget(DB, Gtkid) ->
    ID = Gtkid#gtkid.id,
    insert_kid(DB, Gtkid#gtkid.parent, ID),
    insert(DB, ID, Gtkid),
    Gtkid.

insert_widget(DB, ID, Gtkid) ->
    insert(DB, ID, Gtkid),
    Gtkid.

insert_kid(DB, Parent, Kid) ->
    ets:insert(get(kids), {{kids, Parent},Kid}).

delete_kid(DB, Parent, Kid) ->
    ets:match_delete(get(kids), {{kids, Parent},Kid}).

lookup_kids(DB, Parent) ->
    ril(ets:match(get(kids), {{kids, Parent},'$1'})).

%%----------------------------------------------------------------------
%% Options are stored as {{Id,Opt},Val}
%%----------------------------------------------------------------------
insert_opt(DB,Id,{default,ObjType,Opt}) ->
    insert_def(Id,ObjType,Opt);
insert_opt(DB,#gtkid{id=Id},{Key,Val}) ->
    ets:insert(get(options),{{Id,Key},Val});
insert_opt(DB,Id,{Key,Val}) ->
    ets:insert(get(options),{{Id,Key},Val}).

insert_opts(DB,Id,[]) -> done;
insert_opts(DB,Id,[Opt|Opts]) ->
    insert_opt(DB,Id,Opt),
    insert_opts(DB,Id,Opts).

insert_def(#gtkid{id=ID},ObjType,{Key,Val}) ->
    insert_def(ID,ObjType,{Key,Val});
insert_def(ID,ObjType,{Key,Val}) ->
    Def = get(defaults),
    ets:match_delete(Def,{{ID,ObjType},{Key,'_'}}),
    ets:insert(Def,{{ID,ObjType},{Key,Val}}).

lookup_def(ID,ObjType,Key) ->
    case ets:match(get(defaults),{{ID,ObjType},{Key,'$1'}}) of
	[] -> false;
	[[Val]] -> {value,Val}
    end.

opt(DB,#gtkid{id=Id},Opt) -> opt(DB,Id,Opt);
opt(DB,Id,Opt) ->
     [{_, Value}] = ets:lookup(get(options), {Id,Opt}),
    Value.

opt_or_not(DB,#gtkid{id=Id},Opt) -> opt_or_not(DB,Id,Opt);
opt_or_not(DB,Id,Opt) ->
    case ets:lookup(get(options), {Id,Opt}) of
	[{_, Value}] -> {value, Value};
	Q -> false
    end.

opt(DB,#gtkid{id=Id},Opt,ElseVal) -> opt(DB,Id,Opt,ElseVal);
opt(DB,Id,Opt,ElseVal) ->
    case ets:lookup(get(options), {Id,Opt}) of
	[{_, Value}] ->
	    Value;
	Q -> ElseVal
    end.

%%----------------------------------------------------------------------
%% Returns: list of {Key,Val}
%%----------------------------------------------------------------------
lookup_opts(DB,Id) ->
    lists:sort(lists:map({erlang,list_to_tuple},[],
			 ets:match(get(options),{{Id,'$1'},'$2'}))).

default_container_opts(DB,Id,ChildType) ->
    L =	ets:match(get(defaults),{{Id,'$1'},'$2'}),
    lists:sort(fix_def_for_container(L,ChildType)).
    
default_opts(DB,Id,ChildType) ->
    L1 = ets:lookup(get(defaults),{Id,ChildType}),
    L2 = ets:lookup(get(defaults),{Id,all}),
    lists:sort(fix_def(L1,L2)).

fix_def([{_,Opt}|Opts],Opts2) ->
    [Opt|fix_def(Opts,Opts2)];
fix_def([],[]) -> [];
fix_def([],Opts) ->
    fix_def(Opts,[]).
    
%%----------------------------------------------------------------------
%% Purpose: Extracs {default,ObjType,DefsultOpt} for the ChildType
%% and keeps default options since it is a container object.
%% Returns: list of options
%%----------------------------------------------------------------------
fix_def_for_container([[all,{Key,Val}]|Opts],ChildType) ->
    [{{default,all,Key},Val},{Key,Val}
     |fix_def_for_container(Opts,ChildType)];
fix_def_for_container([[ChildType,{Key,Val}]|Opts],ChildType) ->
    [{{default,ChildType,Key},Val},{Key,Val}
     |fix_def_for_container(Opts,ChildType)];
fix_def_for_container([[ChildType2,{Key,Val}]|Opts],ChildType) ->
    [{{default,ChildType2,Key},Val}|fix_def_for_container(Opts,ChildType2)];
fix_def_for_container([],_) -> [].

%% -----------------------------------------------------------------
%%  lookup things

lookup_id(DB, Name, Owner) when atom(Name) ->
    lookup(DB, {Owner, Name});

lookup_id(DB, ID, Owner) ->
    ID.


lookup_gtkid(DB, Name, Owner) when atom(Name) ->
    ID = lookup(DB, {Owner, Name}),
    lookup(DB, ID);

lookup_gtkid(DB, ID, Owner) ->
    lookup(DB, ID).


lookup_gtkid(DB, Name) when atom(Name) ->
    exit({'must use owner',Name});

lookup_gtkid(DB, ID) ->
    lookup(DB, ID).


lookup_ids(DB, Pid) ->
    ril(ets:match(DB, {'$1', {gtkid,'_','_','_',Pid,'_','_'}})).

lookup_item(DB, TkW, Item) ->
    [[Id]] = ets:match(DB, {'$1', {gtkid,'_',TkW, Item,'_','_','_'}}),
    Id.



%% -----------------------------------------------------------------
%% counters

counter(DB, Key) ->
    Result =
	case ets:lookup(DB, Key) of
	    [{Key, Value}] -> Value+1;
	    _ -> 0
	end,
    ets:insert(DB, {Key, Result}),
    Result.


%% -----------------------------------------------------------------
%% delete things

delete_widgets(DB, [ID | Rest]) ->
    delete_widget(DB, ID),
    delete_widgets(DB, Rest);
delete_widgets(_, []) ->
    true.


delete_widget(DB, #gtkid{id = ID}) ->
    delete_widget(DB, ID);
delete_widget(DB, ID) ->
    delete_widgets(DB, lookup_kids(DB, ID)),
    delete_id(DB, ID).

delete_gtkid(DB,Gtkid) ->
    delete_id(DB,Gtkid).

delete_id(DB, ID) ->
    case lookup_gtkid(DB, ID) of
	undefined ->
	    true;
	Gtkid     ->
	    gtk:worker_do({match_delete,[{get(options),[{{ID,'_'},'_'}]},
					 {get(defaults),[{{ID,'_'},'_'}]}]}),
	    ets:insert(get(deleted),{deleted,ID}),
	    delete(DB, ID)
    end,
    ets:delete(get(kids), {kids, ID}),
    delete(get(events), {events, ID}),
    true.
    
get_deleted(DB) ->
    Dd = get(deleted),
    R=fix_deleted(ets:lookup(Dd,deleted)),
    ets:delete(Dd,deleted),
    R.

fix_deleted([{_,Id}|Dd]) ->
    [Id | fix_deleted(Dd)];
fix_deleted([]) -> [].

%% -----------------------------------------------------------------
%% odd stuff

%% check if an event is in the database, used by read_option
is_inserted(DB, #gtkid{id = ID}, What) ->
    is_inserted(DB, ID, What);
is_inserted(DB, ID, What) ->
    case lookup(get(events), {events, ID}) of
	undefined -> false;
	Events -> 
	    case lists:keysearch(What, 2, Events) of
		{value, _} -> true;
		Other      -> false
	    end
    end.
    
%% -----------------------------------------------------------------
%%                    PRIMITIVES
%% -----------------------------------------------------------------

%% remove irritating lists
ril([[Foo] | Rest]) -> [Foo | ril(Rest)];
ril([]) -> [].



