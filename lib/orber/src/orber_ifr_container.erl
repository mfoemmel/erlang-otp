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
%% File    : orber_ifr_container.erl
%% Author  : Per Danielsson <pd@gwaihir>
%% Purpose : Code for Container
%% Created : 14 May 1997 by Per Danielsson <pd@gwaihir>
%%----------------------------------------------------------------------

-module(orber_ifr_container).

-export(['_get_def_kind'/1,
	 destroy/1,
	 cleanup_for_destroy/1,			%not in CORBA 2.0
	 lookup/2,
	 contents/3,
	 lookup_name/5,
	 describe_contents/4,
	 make_absolute_name/2,			%not in CORBA 2.0
	 make_containing_repository/1,		%not in CORBA 2.0
	 add_to_container/2,			%not in CORBA 2.0
	 create_module/4,
	 create_constant/6,
	 create_struct/5,
	 create_union/6,
	 create_enum/5,
	 create_alias/5,
	 create_interface/5,
	 create_exception/5
	]).

-import(orber_ifr_utils,[get_field/2,select/2,construct/3,makeref/1,unique/0]).
-import(lists,[map/2,filter/2,flatten/1,sublist/2]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").


%%%======================================================================
%%% Container (IRObject)

%%%----------------------------------------------------------------------
%%% Interfaces inherited from IRObject

'_get_def_kind'(ObjRef) ->
    orber_ifr_irobject:'_get_def_kind'(ObjRef).

%%% Note, that the destroy function is meant to be called within a
%%% transaction called in the destroy function of an object which
%%% inherits from Container. A Container should only be destroyed by
%%% destroying the object that inherits from a Container. An attempt
%%% to call this function in user code will result in unpredictable
%%% results.

%%% Don't type check the object reference. We need to be able to handle several
%%% types of objects that inherit from Container.

destroy(Container_objref) ->
    ObjList = cleanup_for_destroy(Container_objref),
    orber_ifr_irobject:destroy([Container_objref | ObjList]).

cleanup_for_destroy(Container_objref) ->
    Contents = get_field(Container_objref, contents),
    map(fun destroy_thing/1, Contents) ++ Contents.

%%% Destroy objects which inherit from Contained, i.e. objects that populate
%%% the contents list of a Container.

destroy_thing({ObjType,ObjID}) when ObjType == ir_ModuleDef ->
    orber_ifr_moduledef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_ConstantDef ->
    orber_ifr_constantdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_TypedefDef ->
    orber_ifr_typedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_StructDef ->
    orber_ifr_structdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_UnionDef ->
    orber_ifr_uniondef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_EnumDef ->
    orber_ifr_enumdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_AliasDef ->
    orber_ifr_aliasdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_ExceptionDef ->
    orber_ifr_exceptiondef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_AttributeDef ->
    orber_ifr_attributedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_OperationDef ->
    orber_ifr_operationdef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) when ObjType == ir_InterfaceDef ->
    orber_ifr_interfacedef:cleanup_for_destroy({ObjType,ObjID});
destroy_thing({ObjType,ObjID}) ->
    ?debug_print("Unknown object in Container contents: ", [{ObjType,ObjID}]),
    true.

%%%----------------------------------------------------------------------
%%% Non-inherited interfaces

lookup(ObjRef, Search_name) ->
    Contents = contents(ObjRef, dk_All, false),

    %% We now have the contents (a list of object references).
    %% Let's find all objects with the correct name.

    case filter(fun({Type,ID}) ->
		   orber_ifr_contained:'_get_absolute_name'({Type,ID}) ==
		       Search_name
	   end,
	   Contents) of
	[Obj] ->
	    Obj;
	X ->
	    X
    end.


contents(ObjRef, Limit_type, Exclude_inherited) ->
    Contents = 
	get_field(ObjRef, contents) ++
	inherited_contents(ObjRef,Exclude_inherited),
    AllContents = 
	Contents ++
	flatten(subcontents(Limit_type,Contents)),
    limit_contents(Limit_type,AllContents).       


subcontents(_,[]) -> [];
subcontents(Limit_type,Contents) ->
    map(fun(ObjRef) -> contents(ObjRef,Limit_type) end, Contents).
    
contents({ir_Repository,ObjID},Limit_type) ->
    orber_ifr_repository:contents({ir_Repository,ObjID},Limit_type,false);
contents({ir_ModuleDef,ObjID},Limit_type) ->
    orber_ifr_moduledef:contents({ir_ModuleDef,ObjID},Limit_type,false);
contents({ir_InterfaceDef,ObjID},Limit_type) ->
    orber_ifr_interfacedef:contents({ir_InterfaceDef,ObjID},Limit_type,false);
contents(_,_) -> [].

limit_contents(dk_All,Contents) -> Contents;
limit_contents(Limit_type,Contents) ->
    filter(fun(Obj_Ref) -> '_get_def_kind'(Obj_Ref) == Limit_type end,
	   Contents).


lookup_name(ObjRef, Search_name, Levels_to_search,
			Limit_type, Exclude_inherited) ->
    Contents = get_field(ObjRef, contents),
    AllContents = Contents ++ inherited_contents(ObjRef, Exclude_inherited),
    lookup_name(AllContents, Search_name, Levels_to_search, Limit_type).
    
inherited_contents({ir_InterfaceDef,ObjID}, false) ->
    map(fun(ObjRef) -> get_field(ObjRef,contents) end,
       orber_ifr_interfacedef:'_get_base_interfaces'({ir_InterfaceDef,ObjID}));
inherited_contents(_, false) -> [];
inherited_contents(_, true) -> [].

lookup_name(Contents, Search_name, Level, Limit_type) ->
    filter(fun(X) ->
		   (orber_ifr_contained:'_get_id'(X) == Search_name)
		   and
		   ('_get_def_kind'(X) == Limit_type)
	   end, Contents) ++
	sublookup_name(Contents, Search_name, Level - 1, Limit_type).

sublookup_name([],_,_,_) -> [];
sublookup_name(_,_,0,_) -> [];
sublookup_name(Contents, Search_name, Level, Limit_type) ->
    map(fun(X) ->
		Conts = subcontents(X),
		lookup_name(Conts, Search_name, Level - 1, Limit_type)
	end, Contents).

subcontents({ir_Repository,ObjID}) ->
    get_field({ir_Repository,ObjID}, contents);
subcontents({ir_ModuleDefObjType,ObjID}) ->
    get_field({ir_ModuleDef,ObjID}, contents);
subcontents({ir_InterfaceDef,ObjID}) ->
    get_field({ir_InterfaceDef,ObjID}, contents);
subcontents(_) -> [].

describe_contents(ObjRef, Limit_type, Exclude_inherited,
		  Max_returned_objs) ->
    Limited_contents = contents(ObjRef,Limit_type,Exclude_inherited),
    Descriptions = map({orber_ifr_contained,describe},Limited_contents),
    describe_contents(Descriptions,Max_returned_objs).

describe_contents(Desc,-1) -> Desc;
describe_contents(Desc,Max) -> sublist(Desc,Max).


%% This is a kludge. Se p. 6-11 in CORBA 2.0.
make_absolute_name({ObjType,ObjID}, Name) ->
    case ObjType of
	ir_Repository ->
	    "::" ++ Name;
	_ ->
	    orber_ifr_contained:'_get_absolute_name'({ObjType,ObjID}) ++
		"::" ++ Name
    end.

%% This is a kludge. Se p. 6-15 in CORBA 2.0.
make_containing_repository({ObjType,ObjID}) ->
    case ObjType of
	ir_Repository ->
	    {ir_Repository,ObjID};
	_ ->
	    orber_ifr_contained:'_get_containing_repository'({ObjType, ObjID})
    end.

add_to_container(ContainerRef,Object) ->
    F = fun() ->
		[Container_obj] = mnesia:read(ContainerRef),
		ObjectRef = makeref(Object),
		New_container_obj =
		    construct(Container_obj,contents,
			      [ObjectRef | select(Container_obj,contents)]),
		mnesia:write(New_container_obj),
		mnesia:write(Object)
	end,
    orber_ifr_utils:ifr_transaction_write(F).

create_module(ObjRef, Id, Name, Version) ->
    ?exists_check(ObjRef, Id),
    New_module = #ir_ModuleDef{ir_Internal_ID = unique(),
			       def_kind = dk_Module,
			       contents = [],
			       id = Id,
			       name = Name,
			       version = Version,
			       defined_in = ObjRef,
			       absolute_name =
			       make_absolute_name(ObjRef, Name),
			       containing_repository =
			       make_containing_repository(ObjRef)},
    add_to_container(ObjRef,New_module),
    makeref(New_module).

create_constant(ObjRef, Id, Name, Version, Type, Value) ->
    ?exists_check(ObjRef, Id),
    IDL_typecode = get_field(Type,type),
    {Typecode, _} = Value,
    case IDL_typecode == Typecode of
	false ->
	    ?ifr_exception("Wrong type in create_constant ",
			   {ObjRef, Id, Name, Version, Type, Value});
	true ->
	    New_constant = #ir_ConstantDef{ir_Internal_ID = unique(),
					   def_kind = dk_Constant,
					   id = Id,
					   name = Name,
					   version = Version,
					   defined_in = ObjRef,
					   absolute_name =
					   make_absolute_name(ObjRef, Name),
					   containing_repository =
					   make_containing_repository(ObjRef),
					   type = get_field(Type,type),
					   type_def = Type,
					   value = Value},
	    add_to_container(ObjRef,New_constant),
	    makeref(New_constant)
    end.

create_struct(ObjRef, Id, Name, Version, Members) ->
    ?exists_check(ObjRef, Id),
    New_struct = #ir_StructDef{ir_Internal_ID = unique(),
			       def_kind = dk_Struct,
			       id = Id,
			       name = Name,
			       version = Version,
			       defined_in = ObjRef,
			       absolute_name =
			       make_absolute_name(ObjRef, Name),
			       containing_repository =
			       make_containing_repository(ObjRef),
			       type = {tk_struct, Id, Name,
				       map(fun(#structmember{name=MemName,
							     type=Type}) ->
						   {MemName,Type} end,
						 Members)},
			       members = Members},
    add_to_container(ObjRef, New_struct),
    makeref(New_struct).

create_union(ObjRef, Id, Name, Version,
	     Discriminator_type, Members) ->
    ?exists_check(ObjRef, Id),
    Discriminator_type_code = get_field(Discriminator_type, type),
    New_union = #ir_UnionDef{ir_Internal_ID = unique(),
			     def_kind = dk_Union,
			     id = Id,
			     name = Name,
			     version = Version,
			     defined_in = ObjRef,
			     absolute_name =
			     make_absolute_name(ObjRef, Name),
			     containing_repository =
			     make_containing_repository(ObjRef),
			     type = {tk_union, Id, Name,
				     Discriminator_type_code, -1,
				     map(fun(#unionmember{name=MemName,
							  label=Label,
							  type=Type}) ->
						 {Label,MemName,Type} end,
					 Members)},
			     discriminator_type = Discriminator_type_code,
			     discriminator_type_def = Discriminator_type,
			     members = Members},
    add_to_container(ObjRef, New_union),
    makeref(New_union).

create_enum(ObjRef, Id, Name, Version, Members) ->
    ?exists_check(ObjRef, Id),
    New_enum = #ir_EnumDef{ir_Internal_ID = unique(),
			   def_kind = dk_Enum,
			   id = Id,
			   name = Name,
			   version = Version,
			   defined_in = ObjRef,
			   absolute_name =
			   make_absolute_name(ObjRef, Name),
			   containing_repository =
			   make_containing_repository(ObjRef),
			   type = {tk_enum, Id, Name, Members},
			   members = Members},
    add_to_container(ObjRef, New_enum),
    makeref(New_enum).

create_alias(ObjRef, Id, Name, Version, Original_type) ->
    ?exists_check(ObjRef, Id),
    New_alias = #ir_AliasDef{ir_Internal_ID = unique(),
			     def_kind = dk_Alias,
			     id = Id,
			     name = Name,
			     version = Version,
			     defined_in = ObjRef,
			     absolute_name =
			     make_absolute_name(ObjRef, Name),
			     containing_repository =
			     make_containing_repository(ObjRef),
			     type = {tk_alias, Id, Name,
				     get_field(Original_type,type)},
			     original_type_def = Original_type},
    add_to_container(ObjRef, New_alias),
    makeref(New_alias).

create_interface(ObjRef, Id, Name, Version, Base_interfaces) ->
    ?exists_check(ObjRef, Id),
    New_interface = #ir_InterfaceDef{ir_Internal_ID = unique(),
				     def_kind = dk_Interface,
				     contents = [],
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = ObjRef,
				     absolute_name =
				     make_absolute_name(ObjRef,Name),
				     containing_repository =
				     make_containing_repository(ObjRef),
				     type = {tk_objref, Id, Name},
				     base_interfaces = Base_interfaces},
    add_to_container(ObjRef, New_interface),
    makeref(New_interface).

create_exception(ObjRef, Id, Name, Version, Members) ->
    ?exists_check(ObjRef, Id),
    New_exception = #ir_ExceptionDef{ir_Internal_ID = unique(),
				     def_kind = dk_Exception,
				     id = Id,
				     name = Name,
				     version = Version,
				     defined_in = ObjRef,
				     absolute_name =
				     make_absolute_name(ObjRef,Name),
				     containing_repository =
				     make_containing_repository(ObjRef),
				     type = {tk_except, Id, Name,
					    map(fun(#structmember{name=MemName,
								  type=Type})
						   ->
							{MemName,Type} end,
						Members)},
				     members = Members},
    add_to_container(ObjRef, New_exception),
    makeref(New_exception).
