%%--------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File    : orber_ifr_utils.erl
%% Author  : Per Danielsson <pd@gwaihir>
%% Purpose : Common function for the Interface Repository
%% Created : 16 Oct 1997 by Per Danielsson <pd@gwaihir>
%%----------------------------------------------------------------------

-module(orber_ifr_utils).

-export([
	 select/2,
	 index/2,
	 construct/3,
	 get_object/1,
	 set_object/1,
	 get_field/2,
	 set_field/3,
	 write_result/1,
	 read_result/1,
	 ifr_transaction_read/1,
	 ifr_transaction_write/1,
	 ifr_transaction_read_write/1,
	 makeref/1,
	 unique/0,
	 existence_check/2,
	 create_repository/0,
	 init_DB/2
	]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").

%%%======================================================================
%%% Internal stuff

%%%----------------------------------------------------------------------
%%% Make a record selection.
%%%
%%% This code *must* be amended whenever a new record is added in the
%%% files ifr_objects.hrl or ../include/ifr_types.hrl

select(Record,Field) when record(Record,ir_IRObject) ->
    select(Record,record_info(fields,ir_IRObject),Field);
select(Record,Field) when record(Record,ir_Contained) ->
    select(Record,record_info(fields,ir_Contained),Field);
select(Record,Field) when record(Record,ir_Container) ->
    select(Record,record_info(fields,ir_Container),Field);
select(Record,Field) when record(Record,ir_IDLType) ->
    select(Record,record_info(fields,ir_IDLType),Field);
select(Record,Field) when record(Record,ir_Repository) ->
    select(Record,record_info(fields,ir_Repository),Field);
select(Record,Field) when record(Record,ir_ModuleDef) ->
    select(Record,record_info(fields,ir_ModuleDef),Field);
select(Record,Field) when record(Record,ir_ConstantDef) ->
    select(Record,record_info(fields,ir_ConstantDef),Field);
select(Record,Field) when record(Record,ir_TypedefDef) ->
    select(Record,record_info(fields,ir_TypedefDef),Field);
select(Record,Field) when record(Record,ir_StructDef) ->
    select(Record,record_info(fields,ir_StructDef),Field);
select(Record,Field) when record(Record,ir_UnionDef) ->
    select(Record,record_info(fields,ir_UnionDef),Field);
select(Record,Field) when record(Record,ir_EnumDef) ->
    select(Record,record_info(fields,ir_EnumDef),Field);
select(Record,Field) when record(Record,ir_AliasDef) ->
    select(Record,record_info(fields,ir_AliasDef),Field);
select(Record,Field) when record(Record,ir_PrimitiveDef) ->
    select(Record,record_info(fields,ir_PrimitiveDef),Field);
select(Record,Field) when record(Record,ir_StringDef) ->
    select(Record,record_info(fields,ir_StringDef),Field);
select(Record,Field) when record(Record,ir_SequenceDef) ->
    select(Record,record_info(fields,ir_SequenceDef),Field);
select(Record,Field) when record(Record,ir_ArrayDef) ->
    select(Record,record_info(fields,ir_ArrayDef),Field);
select(Record,Field) when record(Record,ir_ExceptionDef) ->
    select(Record,record_info(fields,ir_ExceptionDef),Field);
select(Record,Field) when record(Record,ir_AttributeDef) ->
    select(Record,record_info(fields,ir_AttributeDef),Field);
select(Record,Field) when record(Record,ir_OperationDef) ->
    select(Record,record_info(fields,ir_OperationDef),Field);
select(Record,Field) when record(Record,ir_InterfaceDef) ->
    select(Record,record_info(fields,ir_InterfaceDef),Field);
select([],_) -> [];
select(Record,Field) ->
    ?ifr_exception("Unknow record type:", Record).

-define(ELEMENT_OFFSET, 2).

select(Record,Fields,Field) ->
    Index = index(Fields,Field),
    element(?ELEMENT_OFFSET + Index, Record).

index(List,Element) ->
    index(List,Element,0).

index([H|T],Element,Index) when H == Element ->
    Index;
index([H|T],Element,Index) ->
    index(T,Element,Index+1);
index([],Element,Index) ->
    ?ifr_exception("index error",{Element,Index}).

%%%----------------------------------------------------------------------
%%% Construct a record.
%%%
%%% This code *must* be amended whenever a new record is added in the
%%% files ifr_objects.hrl or ../include/ifr_types.hrl

construct(Record,Field,Value) when record(Record,ir_IRObject) ->
    construct(Record,record_info(fields,ir_IRObject),Field,Value);
construct(Record,Field,Value) when record(Record,ir_Contained) ->
    construct(Record,record_info(fields,ir_Contained),Field,Value);
construct(Record,Field,Value) when record(Record,ir_Container) ->
    construct(Record,record_info(fields,ir_Container),Field,Value);
construct(Record,Field,Value) when record(Record,ir_IDLType) ->
    construct(Record,record_info(fields,ir_IDLType),Field,Value);
construct(Record,Field,Value) when record(Record,ir_Repository) ->
    construct(Record,record_info(fields,ir_Repository),Field,Value);
construct(Record,Field,Value) when record(Record,ir_ModuleDef) ->
    construct(Record,record_info(fields,ir_ModuleDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_ConstantDef) ->
    construct(Record,record_info(fields,ir_ConstantDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_TypedefDef) ->
    construct(Record,record_info(fields,ir_TypedefDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_StructDef) ->
    construct(Record,record_info(fields,ir_StructDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_UnionDef) ->
    construct(Record,record_info(fields,ir_UnionDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_EnumDef) ->
    construct(Record,record_info(fields,ir_EnumDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_AliasDef) ->
    construct(Record,record_info(fields,ir_AliasDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_PrimitiveDef) ->
    construct(Record,record_info(fields,ir_PrimitiveDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_StringDef) ->
    construct(Record,record_info(fields,ir_StringDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_SequenceDef) ->
    construct(Record,record_info(fields,ir_SequenceDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_ArrayDef) ->
    construct(Record,record_info(fields,ir_ArrayDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_ExceptionDef) ->
    construct(Record,record_info(fields,ir_ExceptionDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_AttributeDef) ->
    construct(Record,record_info(fields,ir_AttributeDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_OperationDef) ->
    construct(Record,record_info(fields,ir_OperationDef),Field,Value);
construct(Record,Field,Value) when record(Record,ir_InterfaceDef) ->
    construct(Record,record_info(fields,ir_InterfaceDef),Field,Value);
construct(Record,Field,Value) ->
    ?ifr_exception("Unknow record type:", Record).

construct(Record,Fields,Field,Value) ->
    Index = index(Fields,Field),
    setelement(?ELEMENT_OFFSET + Index,Record,Value).

%%%----------------------------------------------------------------------
%%% Read an object from the database

get_object(Objref) ->
%%% Use mnesia:dirty_read/1. It is much faster than doing a transaction.
    case mnesia:dirty_read(Objref) of
	[Res] ->
	    Res;
	[] ->
	    [];
	Other ->
	    ?ifr_exception("reading from DB failed:", Other)
    end.
%%% This is the old code, with a transaction. We might have to revert back
%%% to this at some future time...
%%    _F = ?read_function(Objref),
%%    read_result(ifr_transaction_read(_F)).

%%%----------------------------------------------------------------------
%%% Write an object to the database

set_object(Object) ->
    _F = ?write_function(Object),
    write_result(ifr_transaction_write(_F)).

%%%----------------------------------------------------------------------
%%% Get the value of a field in a record in the DB

get_field(Objref,FieldName) ->
    Object = get_object(Objref),
    select(Object,FieldName).

%%%----------------------------------------------------------------------
%%% Atomically set the value of a field in a record in the DB

set_field(Objref,FieldName,Value) ->
    _F = fun() -> Object = get_object(Objref),
		  New_object = construct(Object,FieldName,Value),
		  mnesia:write(New_object)
	 end,
    write_result(ifr_transaction_write(_F)).

%%%----------------------------------------------------------------------
%%% Write a list of records to the database

mnesia_write_list([]) -> true;
mnesia_write_list([Obj | Obj_tail]) ->
    mnesia:write(Obj),
    mnesia_write_list(Obj_tail).    

%%%----------------------------------------------------------------------
%%% Check a write transaction

write_result(?write_check(Wres)) -> ok;
write_result(Wres) ->
    ?ifr_exception("writing to DB failed:", Wres).

%%%----------------------------------------------------------------------
%%% Extract the data from a read

read_result(?read_check_1(Qres)) when length([Qres]) == 1 -> Qres;
read_result(?read_check_2(Qres)) -> [];
read_result(Qres) ->
    ?ifr_exception("reading from DB failed:", Qres).

%%%----------------------------------------------------------------------
%%% Execute a transaction or a dirty read/write.
%%%
%%% Since nested transctions will upgrade the inner activity to the
%%% same kind as the outer, we cannot use the check the result in the
%%% above simplistic manner. Therefore we will not mix transaction
%%% with async_dirty (or any of the other transaction-like
%%% activities). A rather extensive rewrite of the query extraction
%%% code must be done first.

ifr_transaction_read(Fun) ->			% read synchronously
    Tr = mnesia:transaction(Fun),
    ?debug_print("Transaction_read: ", Tr),
    {atomic, _} = Tr,
    Tr.
ifr_transaction_write(Fun) ->			% write synchronously
    Tr = mnesia:transaction(Fun),
    ?debug_print("Transaction_write: ", Tr),
    {atomic, _} = Tr,
    Tr.
ifr_transaction_read_write(Fun) ->		% write synchronously
    Tr = mnesia:transaction(Fun),
    ?debug_print("Transaction_read_write: ", Tr),
    {atomic, _} = Tr,
    Tr.

%%%----------------------------------------------------------------------
%%% Make an object reference from an object

makeref(Obj) ->
    [ObjType, ObjID | _] = tuple_to_list(Obj),
    {ObjType, ObjID}.

%%%----------------------------------------------------------------------
%%% Make a unique tag.
%%%
%%% The call to term_to_binary is made to hide the representation of the
%%% unique tag. I do this because the tuple generated takes a lot of space
%%% when I dump the database. A binary is simply printed as #Bin, which
%%% is much less obtrusive.
%%% The code has been moved to a macro defined in orber_ifr.hrl, so we
%%% can use a simpler uniqification code when debugging.

%%unique() -> term_to_binary({node(), now()}).
unique() -> ?unique_code.

%%%----------------------------------------------------------------------
%%% Check for an existing object with the Id of the object which is
%%% about to be created.

existence_check({ObjType, ObjID}, Id) ->
    Rep = case ObjType of
	      ir_Repository ->
		  {ObjType, ObjID};
	      _ ->
		  orber_ifr_contained:'_get_containing_repository'({ObjType,
								    ObjID})
	  end,
    case orber_ifr_repository:lookup_id(Rep, Id) of
	[] ->
	    ok;
	_ ->
	    ?ifr_exception("Name clash: ", Id)
    end.

%%======================================================================
%% Database initialization

init_DB(Timeout, Options) ->
    AllTabs = mnesia:system_info(tables),
    Func = case Options of
	       {localCopy, IFR_storage_type} ->
		   ?ifr_record_tuple_list_local(IFR_storage_type);
	       _ ->
		   ?ifr_record_tuple_list(Options)
    end,
    DB_tables_created = lists:map(fun({T,F}) -> F() end, Func),
    db_error_check(DB_tables_created,"Database table creation failed."),
    Wait = mnesia:wait_for_tables(?ifr_object_list, Timeout),
    db_error_check([Wait],"Database table waiting failed.").



%init_DB(Timeout, Options) ->
%    AllTabs = mnesia:system_info(tables),
%    DB_tables_created = lists:map(fun({T,F}) ->
%					  case lists:member(T,AllTabs) of
%					      true ->
%						  ok;
%					      _ ->
%						  F()
%					  end
%				  end,
%				  ?ifr_record_tuple_list(Options)),
%    db_error_check(DB_tables_created,"Database table creation failed."),
%    Wait = mnesia:wait_for_tables(?ifr_object_list, Timeout),
%    db_error_check([Wait],"Database table waiting failed.").

db_error_check(Checkval,Message) ->
    case lists:any(fun(X) -> X/= ok end, Checkval) of
	true ->
	    ?ifr_exception(Message, Checkval);
	false ->
	    ok
    end.   

%%%----------------------------------------------------------------------
%%% Create an interface repository. This function should only be called
%%% once, after the database has been set up and initialized.

create_repository() ->
    _R = fun() ->
		 Pat = mnesia:table_info(ir_Repository, wild_pattern),
		 [X#ir_Repository.ir_Internal_ID ||
		     X <- mnesia:match_object(Pat)]
	 end,
    case ifr_transaction_read(_R) of
%	{atomic,[]} ->
	?read_check_2(_) ->
	    PrimitiveDefs = create_primitivedefs(),
	    New_repository = #ir_Repository{ir_Internal_ID = unique(),
					    def_kind = dk_Repository,
					    contents = [],
					    primitivedefs = PrimitiveDefs},
	    F = ?write_function(New_repository),
	    ifr_transaction_write(F),
	    {ir_Repository,New_repository#ir_Repository.ir_Internal_ID};
%	{atomic,[Rep_ID]} ->
	?read_check_1(Rep_ID) ->
%%	    ?debug_print("An Interface Repository already exists:", [Rep_ID]),
	    {ir_Repository,Rep_ID};
	Err ->
	    ?ifr_exception("Cannot create Interface Repository:",Err)
    end.

create_primitivedefs() ->
    lists:map(fun(Pk) ->
		      orber_ifr_repository:create_primitivedef(Pk)
	      end,
	      [pk_void,pk_short,pk_long,pk_ushort,pk_ulong,
	       pk_float,pk_double,pk_boolean,pk_char,pk_octet,pk_any,
	       pk_TypeCode,pk_Principal,pk_string,pk_objref]).
