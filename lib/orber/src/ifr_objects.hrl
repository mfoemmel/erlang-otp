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
%%%----------------------------------------------------------------------
%%% File    : ir_objects.hrl
%%% Author  : Per Danielsson <pd@erix.ericsson.se>
%%% Purpose : Record definitions for the IR DB 
%%% Created : 19 Mar 1997 by Per Danielsson <pd@erix.ericsson.se>
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% *********************************************************************
%%% *									*
%%% *                        PLEASE NOTE				*
%%% *									*
%%% * If a record is removed or added in this file, the corresponding   *
%%% * database initialization code _MUST_ be updated accordingly.	*
%%% *									*
%%% * The initialization code is defined in a macro in this file.	*
%%% *									*
%%% * Also remember to update select/2 in orber_ifr.erl when adding	*
%%% * or deleting a record in this file.				*
%%% *									*
%%% *********************************************************************
%%%----------------------------------------------------------------------

%% Interface objects

%% There are eight interface objects in an interface repository:
%% Repository, ModuleDef, InterfaceDef, AttributeDef, OperationDef,
%% TypedefDef, ConstantDef and ExceptionDef (CORBA V2.0, page 6-5/6).

% The other objects defined here are used to build the above objects
% (CORBA V2.0, page 6-7).

% Object references are stored as mnesia object IDs, i.e. a tuple with
% the table name and the ir_Internal_ID.

% Inheritance strategy. We incorporate the inherited object into the
% inheriting object. The record element 'inherited_objects' is a list
% of objects that "this" object inherits from (i.e. full object
% records and not object references).

% The record element 'ir_Internal_ID' is a tag that uniquely
% identifies a record. See the function orber_ifr:unique().

						% IRObject, page 6-9
-record(ir_IRObject,	 {ir_Internal_ID,def_kind}).

						% Contained, page 6-9
-record(ir_Contained,    {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  id,
			  name,
			  version,
			  defined_in,
			  absolute_name,
			  containing_repository}).

						% Container, page 6-10
-record(ir_Container,    {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  contents}).

						% IDLType, page 6-15
-record(ir_IDLType,      {ir_Internal_ID,	%[IRObject]
			  def_kind,		%from IRObject
			  type}).

						% Repository, page 6-16
-record(ir_Repository,   {ir_Internal_ID,	%[Container]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  primitivedefs}).

						% ModuleDef, page 6-17
-record(ir_ModuleDef,    {ir_Internal_ID,	%[Container,Contained]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository %from Contained
			 }).

						% ConstantDef, page 6-17
-record(ir_ConstantDef,  {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  type_def,
			  value}).

						% TypedefDef, page 6-18
-record(ir_TypedefDef,   {ir_Internal_ID,	%[Contained,IDLType]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type			%from IDLType
			 }).

						% StructDef, page 6-19
-record(ir_StructDef,    {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  members}).

						% UnionDef, page 6-19
-record(ir_UnionDef,     {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  discriminator_type,
			  discriminator_type_def,
			  members}).

						% EnumDef, page 6-20
-record(ir_EnumDef,      {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  members}).

						% AliasDef, page 6-21
-record(ir_AliasDef,     {ir_Internal_ID,	%[TypedefDef]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  original_type_def}).

						% PrimitiveDef, page 6-21
-record(ir_PrimitiveDef, {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  kind}).

						% StringDef, page 6-22
-record(ir_StringDef,    {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  bound}).

						% SequenceDef, page 6-22
-record(ir_SequenceDef,  {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  bound,
			  element_type,
			  element_type_def}).

						% ArrayDef, page 6-23
-record(ir_ArrayDef,     {ir_Internal_ID,	%[IDLType]
			  def_kind,		%from IRObject
			  type,			%from IDLType
			  length,
			  element_type,
			  element_type_def}).

						% ExceptionDef, page 6-23
-record(ir_ExceptionDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  members}).

						% AttributeDef, page 6-24
-record(ir_AttributeDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,
			  type_def,
			  mode}).

						% OperationDef, page 6-25
-record(ir_OperationDef, {ir_Internal_ID,	%[Contained]
			  def_kind,		%from IRObject
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  result,
			  result_def,
			  params,
			  mode,
			  contexts,
			  exceptions}).

						% InterfaceDef, page 6-27
-record(ir_InterfaceDef, {ir_Internal_ID,	%[Container,Contained,IDLType]
			  def_kind,		%from IRObject
			  contents,		%from Container
			  id,			%from Contained
			  name,			%from Contained
			  version,		%from Contained
			  defined_in,		%from Contained
			  absolute_name,	%from Contained
			  containing_repository, %from Contained
			  type,			%from IDLType
			  base_interfaces}).

						% TypeCode, page 6-33
% TypeCodes cannot be defined as records, since each type code has a
% quite unique structure depending on the type. The old TypeCode
% record definition is left here as a comment in case we want to
% change back to the old style.

%% ir_TypeCode does not have a field ir_Internal_ID. TypeCodes are
%% never explicitly written to the database as separate DB-records.
%% TypeCodes are stored as full records whenever they are used in an
%% IFR-object.
%%-record(ir_TypeCode,     {kind,
%%			  parameter_list}).

						% ORB, page 6-39
-record(ir_ORB,          {ir_Internal_ID,   % *** Do we need any attributes
			  dummy}).	    % for this table? ORB is a pseudo-
					    % object so perhaps the table is
					    % unnecessary?

%%%----------------------------------------------------------------------
%%% 'ifr_object_list' is used by other modules. Do NOT remove or rename
%%% this list!
%%% An addition or deletion of a record above must be duplicated here in
%%% this list and in the macro 'ifr_record_tuple_list' below.
-define(ifr_object_list, [ir_ModuleDef,
			  ir_Contained,
			  ir_AttributeDef,
			  ir_Repository,
			  ir_OperationDef,
			  ir_InterfaceDef,
			  ir_TypedefDef,
			  ir_Container,
			  ir_EnumDef,
			  ir_UnionDef,
			  ir_StringDef,
			  ir_ORB,
			  ir_IDLType,
			  ir_ExceptionDef,
			  ir_IRObject,
			  ir_PrimitiveDef,
			  ir_ArrayDef,
			  ir_AliasDef,
			  ir_ConstantDef,
			  ir_StructDef,
			  ir_SequenceDef]).

-define(cr_fun_tuple(Table, Options),
	{Table,
	 fun() ->
		 ?debug_print("creating ", Table),
		 case mnesia:create_table(Table,[{attributes,
						  record_info(fields,
							      Table)}]++Options)of
		     {atomic,ok} ->
			 ok;
		     R ->
			 R
		 end
	 end}
       ).

-define(cr_fun_tuple_local(Table, IFR_storage_type),
	{Table,
	 fun() ->
		 ?debug_print("adding ", Table),
		 case mnesia:add_table_copy(Table,node(), IFR_storage_type)of
		     {atomic,ok} ->
			 ok;
		     R ->
			 R
		 end
	 end}
       ).

-define(ifr_record_tuple_list(Options),
        [?cr_fun_tuple(ir_IRObject, Options),
         ?cr_fun_tuple(ir_Contained, Options),
         ?cr_fun_tuple(ir_Container, Options),
         ?cr_fun_tuple(ir_IDLType, Options),
         ?cr_fun_tuple(ir_Repository, Options),
         ?cr_fun_tuple(ir_ModuleDef, Options),
         ?cr_fun_tuple(ir_ConstantDef, Options),
         ?cr_fun_tuple(ir_TypedefDef, Options),
         ?cr_fun_tuple(ir_StructDef, Options),
         ?cr_fun_tuple(ir_UnionDef, Options),
         ?cr_fun_tuple(ir_EnumDef, Options),
         ?cr_fun_tuple(ir_AliasDef, Options),
         ?cr_fun_tuple(ir_PrimitiveDef, Options),
         ?cr_fun_tuple(ir_StringDef, Options),
         ?cr_fun_tuple(ir_SequenceDef, Options),
         ?cr_fun_tuple(ir_ArrayDef, Options),
         ?cr_fun_tuple(ir_ExceptionDef, Options),
         ?cr_fun_tuple(ir_AttributeDef, Options),
         ?cr_fun_tuple(ir_OperationDef, Options),
         ?cr_fun_tuple(ir_InterfaceDef, Options),
%        ?cr_fun_tuple(ir_TypeCode, Options),
         ?cr_fun_tuple(ir_ORB, Options)]).


-define(ifr_record_tuple_list_local(IFR_storage_type),
        [?cr_fun_tuple_local(ir_IRObject, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Contained, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Container, IFR_storage_type),
         ?cr_fun_tuple_local(ir_IDLType, IFR_storage_type),
         ?cr_fun_tuple_local(ir_Repository, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ModuleDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ConstantDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_TypedefDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_StructDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_UnionDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_EnumDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_AliasDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_PrimitiveDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_StringDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_SequenceDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ArrayDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ExceptionDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_AttributeDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_OperationDef, IFR_storage_type),
         ?cr_fun_tuple_local(ir_InterfaceDef, IFR_storage_type),
%        ?cr_fun_tuple_local(ir_TypeCode, IFR_storage_type),
         ?cr_fun_tuple_local(ir_ORB, IFR_storage_type)]).
