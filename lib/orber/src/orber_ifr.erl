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
%% File    : corba_ir_impl.erl
%% Author  : Per Danielsson <pd@erix.ericsson.se>
%% Purpose : Interface Repository for CORBA
%% Created :  7 Mar 1997 by Per Danielsson <pd@erix.ericsson.se>
%%----------------------------------------------------------------------

%%% NOTES:
%%%
%%%  For details about known deficiencies in this CORBA IFR
%%%  implementation, see the file ../doc/src/notes.txt.
%%%

-module(orber_ifr).

-export([
%%% Public interfaces:
	 init/2,
	 find_repository/0,
	 'IRObject__get_def_kind'/1,
	 %%'IRObject_destroy'/1,
	 'Contained__get_def_kind'/1,
	 %%'Contained_destroy'/1,
	 'Contained__get_id'/1,
	 'Contained__set_id'/2,
	 'Contained__get_name'/1,
	 'Contained__set_name'/2,
	 'Contained__get_version'/1,
	 'Contained__set_version'/2,
	 'Contained__get_defined_in'/1,
	 'Contained__get_absolute_name'/1,
	 'Contained__get_containing_repository'/1,
	 'Contained_describe'/1,
	 'Contained_move'/4,
	 'Container__get_def_kind'/1,
	 'Container_destroy'/1,
	 'Container_lookup'/2,
	 'Container_contents'/3,
	 'Container_lookup_name'/5,
	 'Container_describe_contents'/4,
	 'Container_create_module'/4,
	 'Container_create_constant'/6,
	 'Container_create_struct'/5,
	 'Container_create_union'/6,
	 'Container_create_enum'/5,
	 'Container_create_alias'/5,
	 'Container_create_interface'/5,
	 'Container_create_exception'/5,
	 'IDLType__get_def_kind'/1,
	 'IDLType_destroy'/1,
	 'IDLType__get_type'/1,
	 'Repository__get_def_kind'/1,
	 'Repository_destroy'/1,
	 'Repository_lookup'/2,
	 'Repository_contents'/3,
	 'Repository_lookup_name'/5,
	 'Repository_describe_contents'/4,
	 'Repository_create_module'/4,
	 'Repository_create_constant'/6,
	 'Repository_create_struct'/5,
	 'Repository_create_union'/6,
	 'Repository_create_enum'/5,
	 'Repository_create_alias'/5,
	 'Repository_create_interface'/5,
	 'Repository_create_exception'/5,
	 'Repository_lookup_id'/2,
	 'Repository_get_primitive'/2,
	 'Repository_create_string'/2,
	 'Repository_create_sequence'/3,
	 'Repository_create_array'/3,
	 'Repository_create_idltype'/2,		%not in CORBA 2.0
	 'ModuleDef__get_def_kind'/1,
	 'ModuleDef_destroy'/1,
	 'ModuleDef_lookup'/2,
	 'ModuleDef_contents'/3,
	 'ModuleDef_lookup_name'/5,
	 'ModuleDef_describe_contents'/4,
	 'ModuleDef_create_module'/4,
	 'ModuleDef_create_constant'/6,
	 'ModuleDef_create_struct'/5,
	 'ModuleDef_create_union'/6,
	 'ModuleDef_create_enum'/5,
	 'ModuleDef_create_alias'/5,
	 'ModuleDef_create_interface'/5,
	 'ModuleDef_create_exception'/5,
	 'ModuleDef__get_id'/1,
	 'ModuleDef__set_id'/2,
	 'ModuleDef__get_name'/1,
	 'ModuleDef__set_name'/2,
	 'ModuleDef__get_version'/1,
	 'ModuleDef__set_version'/2,
	 'ModuleDef__get_defined_in'/1,
	 'ModuleDef__get_absolute_name'/1,
	 'ModuleDef__get_containing_repository'/1,
	 'ModuleDef_describe'/1,
	 'ModuleDef_move'/4,
	 'ConstantDef__get_def_kind'/1,
	 'ConstantDef_destroy'/1,
	 'ConstantDef__get_id'/1,
	 'ConstantDef__set_id'/2,
	 'ConstantDef__get_name'/1,
	 'ConstantDef__set_name'/2,
	 'ConstantDef__get_version'/1,
	 'ConstantDef__set_version'/2,
	 'ConstantDef__get_defined_in'/1,
	 'ConstantDef__get_absolute_name'/1,
	 'ConstantDef__get_containing_repository'/1,
	 'ConstantDef_describe'/1,
	 'ConstantDef_move'/4,
	 'ConstantDef__get_type'/1,
	 'ConstantDef__get_type_def'/1,
	 'ConstantDef__set_type_def'/2,
	 'ConstantDef__get_value'/1,
	 'ConstantDef__set_value'/2,
	 'TypedefDef__get_def_kind'/1,
	 'TypedefDef_destroy'/1,
	 'TypedefDef__get_id'/1,
	 'TypedefDef__set_id'/2,
	 'TypedefDef__get_name'/1,
	 'TypedefDef__set_name'/2,
	 'TypedefDef__get_version'/1,
	 'TypedefDef__set_version'/2,
	 'TypedefDef__get_defined_in'/1,
	 'TypedefDef__get_absolute_name'/1,
	 'TypedefDef__get_containing_repository'/1,
	 'TypedefDef_describe'/1,
	 'TypedefDef_move'/4,
	 'TypedefDef__get_type'/1,
	 'StructDef__get_def_kind'/1,
	 'StructDef_destroy'/1,
	 'StructDef__get_id'/1,
	 'StructDef__set_id'/2,
	 'StructDef__get_name'/1,
	 'StructDef__set_name'/2,
	 'StructDef__get_version'/1,
	 'StructDef__set_version'/2,
	 'StructDef__get_defined_in'/1,
	 'StructDef__get_absolute_name'/1,
	 'StructDef__get_containing_repository'/1,
	 'StructDef_describe'/1,
	 'StructDef_move'/4,
	 'StructDef__get_type'/1,
	 'StructDef__get_members'/1,
	 'StructDef__set_members'/2,
	 'UnionDef__get_def_kind'/1,
	 'UnionDef_destroy'/1,
	 'UnionDef__get_id'/1,
	 'UnionDef__set_id'/2,
	 'UnionDef__get_name'/1,
	 'UnionDef__set_name'/2,
	 'UnionDef__get_version'/1,
	 'UnionDef__set_version'/2,
	 'UnionDef__get_defined_in'/1,
	 'UnionDef__get_absolute_name'/1,
	 'UnionDef__get_containing_repository'/1,
	 'UnionDef_describe'/1,
	 'UnionDef_move'/4,
	 'UnionDef__get_type'/1,
	 'UnionDef__get_discriminator_type'/1,
	 'UnionDef__get_discriminator_type_def'/1,
	 'UnionDef__set_discriminator_type_def'/2,
	 'UnionDef__get_members'/1,
	 'UnionDef__set_members'/2,
	 'EnumDef__get_def_kind'/1,
	 'EnumDef_destroy'/1,
	 'EnumDef__get_id'/1,
	 'EnumDef__set_id'/2,
	 'EnumDef__get_name'/1,
	 'EnumDef__set_name'/2,
	 'EnumDef__get_version'/1,
	 'EnumDef__set_version'/2,
	 'EnumDef__get_defined_in'/1,
	 'EnumDef__get_absolute_name'/1,
	 'EnumDef__get_containing_repository'/1,
	 'EnumDef_describe'/1,
	 'EnumDef_move'/4,
	 'EnumDef__get_type'/1,
	 'EnumDef__get_members'/1,
	 'EnumDef__set_members'/2,
	 'AliasDef__get_def_kind'/1,
	 'AliasDef_destroy'/1,
	 'AliasDef__get_id'/1,
	 'AliasDef__set_id'/2,
	 'AliasDef__get_name'/1,
	 'AliasDef__set_name'/2,
	 'AliasDef__get_version'/1,
	 'AliasDef__set_version'/2,
	 'AliasDef__get_defined_in'/1,
	 'AliasDef__get_absolute_name'/1,
	 'AliasDef__get_containing_repository'/1,
	 'AliasDef_describe'/1,
	 'AliasDef_move'/4,
	 'AliasDef__get_type'/1,
	 'AliasDef__get_original_type_def'/1,
	 'AliasDef__set_original_type_def'/2,
	 'PrimitiveDef__get_def_kind'/1,
	 'PrimitiveDef_destroy'/1,
	 'PrimitiveDef__get_type'/1,
	 'PrimitiveDef__get_kind'/1,
	 'StringDef__get_def_kind'/1,
	 'StringDef_destroy'/1,
	 'StringDef__get_type'/1,
	 'StringDef__get_bound'/1,
	 'StringDef__set_bound'/2,
	 'SequenceDef__get_def_kind'/1,
	 'SequenceDef_destroy'/1,
	 'SequenceDef__get_type'/1,
	 'SequenceDef__get_bound'/1,
	 'SequenceDef__set_bound'/2,
	 'SequenceDef__get_element_type'/1,
	 'SequenceDef__get_element_type_def'/1,
	 'SequenceDef__set_element_type_def'/2,
	 'ArrayDef__get_def_kind'/1,
	 'ArrayDef_destroy'/1,
	 'ArrayDef__get_type'/1,
	 'ArrayDef__get_length'/1,
	 'ArrayDef__set_length'/2,
	 'ArrayDef__get_element_type'/1,
	 'ArrayDef__get_element_type_def'/1,
	 'ArrayDef__set_element_type_def'/2,
	 'ExceptionDef__get_def_kind'/1,
	 'ExceptionDef_destroy'/1,
	 'ExceptionDef__get_id'/1,
	 'ExceptionDef__set_id'/2,
	 'ExceptionDef__get_name'/1,
	 'ExceptionDef__set_name'/2,
	 'ExceptionDef__get_version'/1,
	 'ExceptionDef__set_version'/2,
	 'ExceptionDef__get_defined_in'/1,
	 'ExceptionDef__get_absolute_name'/1,
	 'ExceptionDef__get_containing_repository'/1,
	 'ExceptionDef_describe'/1,
	 'ExceptionDef_move'/4,
	 'ExceptionDef__get_type'/1,
	 'ExceptionDef__get_members'/1,
	 'ExceptionDef__set_members'/2,
	 'AttributeDef__get_def_kind'/1,
	 'AttributeDef_destroy'/1,
	 'AttributeDef__get_id'/1,
	 'AttributeDef__set_id'/2,
	 'AttributeDef__get_name'/1,
	 'AttributeDef__set_name'/2,
	 'AttributeDef__get_version'/1,
	 'AttributeDef__set_version'/2,
	 'AttributeDef__get_defined_in'/1,
	 'AttributeDef__get_absolute_name'/1,
	 'AttributeDef__get_containing_repository'/1,
	 'AttributeDef_describe'/1,
	 'AttributeDef_move'/4,
	 'AttributeDef__get_type'/1,
	 'AttributeDef__get_type_def'/1,
	 'AttributeDef__set_type_def'/2,
	 'AttributeDef__get_mode'/1,
	 'AttributeDef__set_mode'/2,
	 'OperationDef__get_def_kind'/1,
	 'OperationDef_destroy'/1,
	 'OperationDef__get_id'/1,
	 'OperationDef__set_id'/2,
	 'OperationDef__get_name'/1,
	 'OperationDef__set_name'/2,
	 'OperationDef__get_version'/1,
	 'OperationDef__set_version'/2,
	 'OperationDef__get_defined_in'/1,
	 'OperationDef__get_absolute_name'/1,
	 'OperationDef__get_containing_repository'/1,
	 'OperationDef_describe'/1,
	 'OperationDef_move'/4,
	 'OperationDef__get_result'/1,
	 'OperationDef__get_result_def'/1,
	 'OperationDef__set_result_def'/2,
	 'OperationDef__get_params'/1,
	 'OperationDef__set_params'/2,
	 'OperationDef__get_mode'/1,
	 'OperationDef__set_mode'/2,
	 'OperationDef__get_contexts'/1,
	 'OperationDef__set_contexts'/2,
	 'OperationDef__get_exceptions'/1,
	 'OperationDef__set_exceptions'/2,
	 'InterfaceDef__get_def_kind'/1,
	 'InterfaceDef_destroy'/1,
	 'InterfaceDef_lookup'/2,
	 'InterfaceDef_contents'/3,
	 'InterfaceDef_lookup_name'/5,
	 'InterfaceDef_describe_contents'/4,
	 'InterfaceDef_create_module'/4,
	 'InterfaceDef_create_constant'/6,
	 'InterfaceDef_create_struct'/5,
	 'InterfaceDef_create_union'/6,
	 'InterfaceDef_create_enum'/5,
	 'InterfaceDef_create_alias'/5,
	 'InterfaceDef_create_interface'/5,
	 'InterfaceDef_create_exception'/5,
	 'InterfaceDef__get_id'/1,
	 'InterfaceDef__set_id'/2,
	 'InterfaceDef__get_name'/1,
	 'InterfaceDef__set_name'/2,
	 'InterfaceDef__get_version'/1,
	 'InterfaceDef__set_version'/2,
	 'InterfaceDef__get_defined_in'/1,
	 'InterfaceDef__get_absolute_name'/1,
	 'InterfaceDef__get_containing_repository'/1,
	 'InterfaceDef_describe'/1,
	 'InterfaceDef_move'/4,
	 'InterfaceDef__get_type'/1,
	 'InterfaceDef__get_base_interfaces'/1,
	 'InterfaceDef__set_base_interfaces'/2,
	 'InterfaceDef_is_a'/2,
	 'InterfaceDef_describe_interface'/1,
	 'InterfaceDef_create_attribute'/6,
	 'InterfaceDef_create_operation'/9,
	 %%'TypeCode_equal'/2,
	 %%'TypeCode_kind'/1,
	 %%'TypeCode_id'/1,
	 %%'TypeCode_name'/1,
	 %%'TypeCode_member_count'/1,
	 %%'TypeCode_member_name'/2,
	 %%'TypeCode_member_type'/2,
	 %%'TypeCode_member_label'/2,
	 %%'TypeCode_discriminator_type'/1,
	 %%'TypeCode_default_index'/1,
	 %%'TypeCode_length'/1,
	 %%'TypeCode_content_type'/1,
	 %%'TypeCode_param_count'/1,
	 %%'TypeCode_parameter'/2,
	 'ORB_create_struct_tc'/3,
	 'ORB_create_union_tc'/4,
	 'ORB_create_enum_tc'/3,
	 'ORB_create_alias_tc'/3,
	 'ORB_create_exception_tc'/3,
	 'ORB_create_interface_tc'/2,
	 'ORB_create_string_tc'/1,
	 'ORB_create_sequence_tc'/2,
	 'ORB_create_recursive_sequence_tc'/2,
	 'ORB_create_array_tc'/2,
%%% "Methods" of the IFR "objects"
	 get_def_kind/1,
	 destroy/1,
	 get_id/1,
	 set_id/2,
	 get_name/1,
	 set_name/2,
	 get_version/1,
	 set_version/2,
	 get_defined_in/1,
	 get_absolute_name/1,
	 get_containing_repository/1,
	 describe/1,
	 move/4,
	 lookup/2,
	 contents/3,
	 lookup_name/5,
	 describe_contents/4,
	 create_module/4,
	 create_constant/6,
	 create_struct/5,
	 create_union/6,
	 create_enum/5,
	 create_alias/5,
	 create_interface/5,
	 create_exception/5,
	 get_type/1,
	 lookup_id/2,
	 get_primitive/2,
	 create_string/2,
	 create_sequence/3,
	 create_array/3,
	 create_idltype/2,		%not in CORBA 2.0
	 get_type_def/1,
	 set_type_def/2,
	 get_value/1,
	 set_value/2,
	 get_members/1,
	 set_members/2,
	 get_discriminator_type/1,
	 get_discriminator_type_def/1,
	 set_discriminator_type_def/2,
	 get_original_type_def/1,
	 set_original_type_def/2,
	 get_kind/1,
	 get_bound/1,
	 set_bound/2,
	 get_element_type/1,
	 get_element_type_def/1,
	 set_element_type_def/2,
	 get_length/1,
	 set_length/2,
	 get_mode/1,
	 set_mode/2,
	 get_result/1,
	 get_result_def/1,
	 set_result_def/2,
	 get_params/1,
	 set_params/2,
	 get_contexts/1,
	 set_contexts/2,
	 get_exceptions/1,
	 set_exceptions/2,
	 get_base_interfaces/1,
	 set_base_interfaces/2,
	 is_a/2,
	 describe_interface/1,
	 create_attribute/6,
	 create_operation/9
 	]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").

%%%======================================================================
%%% Internal stuff

%%%----------------------------------------------------------------------
%%% Catch an exception


exceptioncatcher(Module,Fun,Arglist) ->
    apply(Module,Fun,Arglist).

%    case catch apply(Module,Fun,Arglist) of
%	{exception, Exception} ->
%	    {exception, Exception};    
%	Val ->
%	    Val
%    end.

%%%======================================================================
%%% Public interfaces to the IFR

%% Initialize the database
%%init(Nodes, Timeout) ->
%%    exceptioncatcher(orber_ifr_utils,init_DB,[Timeout, [{disc_copies, Nodes}]]).

init(Timeout, Options) ->
    exceptioncatcher(orber_ifr_utils,init_DB,[Timeout, Options]).

%%% Find the repository
find_repository() ->
    exceptioncatcher(orber_ifr_utils,create_repository,[]).

'IRObject__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_irobject,'_get_def_kind',[Objref]).
%%'IRObject_destroy'(Objref) ->
%%    exceptioncatcher(orber_ifr_irobject,destroy,[Objref]).

'Contained__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_def_kind',[Objref]).
%%'Contained_destroy'(Objref) ->
%%    exceptioncatcher(orber_ifr_contained,destroy,[Objref]).
'Contained__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_id',[Objref]).
'Contained__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_contained,'_set_id',[Objref,Id]).
'Contained__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_name',[Objref]).
'Contained__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_contained,'_set_name',[Objref,Name]).
'Contained__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_version',[Objref]).
'Contained__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_contained,'_set_version',[Objref,Version]).
'Contained__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_defined_in',[Objref]).
'Contained__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_absolute_name',[Objref]).
'Contained__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_contained,'_get_containing_repository',
		     [Objref]).
'Contained_describe'(Objref) ->
    exceptioncatcher(orber_ifr_contained,describe,[Objref]).
'Contained_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_contained,move,
		     [Objref,New_container,New_name,New_version]).

'Container__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_container,'_get_def_kind',[Objref]).
'Container_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_container,'_destroy',[Objref]).
'Container_lookup'(Objref,Search_name) ->
    exceptioncatcher(orber_ifr_container,lookup,[Objref,Search_name]).
'Container_contents'(Objref,Limit_type,Exclude_inherited) ->
    exceptioncatcher(orber_ifr_container,contents,
		     [Objref,Limit_type,Exclude_inherited]).
'Container_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			Exclude_inherited) ->
    exceptioncatcher(orber_ifr_container,lookup_name,
		     [Objref,Search_name,Levels_to_search,Limit_type,
		      Exclude_inherited]).
'Container_describe_contents'(Objref,Limit_type,Exclude_inherited,
					 Max_returned_objs) ->
    exceptioncatcher(orber_ifr_container,describe_contents,
		     [Objref,Limit_type,Exclude_inherited,Max_returned_objs]).
'Container_create_module'(Objref,Id,Name,Version) ->
    exceptioncatcher(orber_ifr_container,create_module,
		     [Objref,Id,Name,Version]).
'Container_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    exceptioncatcher(orber_ifr_container,create_constant,
		     [Objref,Id,Name,Version,Type,Value]).
'Container_create_struct'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_container,create_struct,
		     [Objref,Id,Name,Version,Members]).
'Container_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    exceptioncatcher(orber_ifr_container,create_union,
		     [Objref,Id,Name,Version,Discriminator_type,Members]).
'Container_create_enum'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_container,create_enum,
		     [Objref,Id,Name,Version,Members]).
'Container_create_alias'(Objref,Id,Name,Version,Original_type) ->
    exceptioncatcher(orber_ifr_container,create_alias,
		     [Objref,Id,Name,Version,Original_type]).
'Container_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    exceptioncatcher(orber_ifr_container,create_interface,
		     [Objref,Id,Name,Version,Base_interfaces]).
'Container_create_exception'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_container,create_exception,
		     [Objref,Id,Name,Version,Members]).

'IDLType__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_idltype,'_get_def_kind',[Objref]).
'IDLType_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_idltype,destroy,[Objref]).
'IDLType__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_idltype,'_get_type',[Objref]).

'Repository__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_repository,'_get_def_kind',[Objref]).
'Repository_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_repository,destroy,[Objref]).
'Repository_lookup'(Objref,Search_name) ->
    exceptioncatcher(orber_ifr_repository,lookup,[Objref,Search_name]).
'Repository_contents'(Objref,Limit_type,Exclude_inherited) ->
    exceptioncatcher(orber_ifr_repository,contents,
		     [Objref,Limit_type,Exclude_inherited]).
'Repository_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			 Exclude_inherited) ->
    exceptioncatcher(orber_ifr_repository,lookup_name,
		     [Objref,Search_name,Levels_to_search,Limit_type,
		      Exclude_inherited]).
'Repository_describe_contents'(Objref,Limit_type,Exclude_inherited,
			       Max_returned_objs) ->
    exceptioncatcher(orber_ifr_repository,describe_contents,
		     [Objref,Limit_type,Exclude_inherited,Max_returned_objs]).
'Repository_create_module'(Objref,Id,Name,Version) ->
    exceptioncatcher(orber_ifr_repository,create_module,
		     [Objref,Id,Name,Version]).
'Repository_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    exceptioncatcher(orber_ifr_repository,create_constant,
		     [Objref,Id,Name,Version,Type,Value]).
'Repository_create_struct'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_repository,create_struct,
		     [Objref,Id,Name,Version,Members]).
'Repository_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    exceptioncatcher(orber_ifr_repository,create_union,
		     [Objref,Id,Name,Version,Discriminator_type,Members]).
'Repository_create_enum'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_repository,create_enum,
		     [Objref,Id,Name,Version,Members]).
'Repository_create_alias'(Objref,Id,Name,Version,Original_type) ->
    exceptioncatcher(orber_ifr_repository,create_alias,
		     [Objref,Id,Name,Version,Original_type]).
'Repository_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    exceptioncatcher(orber_ifr_repository,create_interface,
		     [Objref,Id,Name,Version,Base_interfaces]).
'Repository_create_exception'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_repository,create_exception,
		     [Objref,Id,Name,Version,Members]).
'Repository_lookup_id'(Objref,Search_id) ->
    exceptioncatcher(orber_ifr_repository,lookup_id,[Objref,Search_id]).
'Repository_get_primitive'(Objref,Kind) ->
    exceptioncatcher(orber_ifr_repository,get_primitive,[Objref,Kind]).
'Repository_create_string'(Objref,Bound) ->
    exceptioncatcher(orber_ifr_repository,create_string,[Objref,Bound]).
'Repository_create_sequence'(Objref,Bound,Element_type) ->
    exceptioncatcher(orber_ifr_repository,create_sequence,
		     [Objref,Bound,Element_type]).
'Repository_create_array'(Objref,Length,Element_type) ->
    exceptioncatcher(orber_ifr_repository,create_array,
		     [Objref,Length,Element_type]).
'Repository_create_idltype'(Objref,Typecode) ->
    exceptioncatcher(orber_ifr_repository,create_idltype,[Objref,Typecode]).

'ModuleDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_def_kind',[Objref]).
'ModuleDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,destroy,[Objref]).
'ModuleDef_lookup'(Objref,Search_name) ->
    exceptioncatcher(orber_ifr_moduledef,lookup,[Objref,Search_name]).
'ModuleDef_contents'(Objref,Limit_type,Exclude_inherited) ->
    exceptioncatcher(orber_ifr_moduledef,contents,
		     [Objref,Limit_type,Exclude_inherited]).
'ModuleDef_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			Exclude_inherited) ->
    exceptioncatcher(orber_ifr_moduledef,lookup_name,
		     [Objref,Search_name,Levels_to_search,Limit_type,
		      Exclude_inherited]).
'ModuleDef_describe_contents'(Objref,Limit_type,Exclude_inherited,
			      Max_returned_objs) ->
    exceptioncatcher(orber_ifr_moduledef,describe_contents,
		     [Objref,Limit_type,Exclude_inherited,Max_returned_objs]).
'ModuleDef_create_module'(Objref,Id,Name,Version) ->
    exceptioncatcher(orber_ifr_moduledef,create_module,
		     [Objref,Id,Name,Version]).
'ModuleDef_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    exceptioncatcher(orber_ifr_moduledef,create_constant,
		     [Objref,Id,Name,Version,Type,Value]).
'ModuleDef_create_struct'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_moduledef,create_struct,
		     [Objref,Id,Name,Version,Members]).
'ModuleDef_create_union'(Objref,Id,Name,Version,Discriminator_type,Members) ->
    exceptioncatcher(orber_ifr_moduledef,create_union,
		     [Objref,Id,Name,Version,Discriminator_type,Members]).
'ModuleDef_create_enum'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_moduledef,create_enum,
		     [Objref,Id,Name,Version,Members]).
'ModuleDef_create_alias'(Objref,Id,Name,Version,Original_type) ->
    exceptioncatcher(orber_ifr_moduledef,create_alias,
		     [Objref,Id,Name,Version,Original_type]).
'ModuleDef_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    exceptioncatcher(orber_ifr_moduledef,create_interface,
		     [Objref,Id,Name,Version,Base_interfaces]).
'ModuleDef_create_exception'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_moduledef,create_exception,
		     [Objref,Id,Name,Version,Members]).
'ModuleDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_id',[Objref]).
'ModuleDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_moduledef,'_set_id',[Objref,Id]).
'ModuleDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_name',[Objref]).
'ModuleDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_moduledef,'_set_name',[Objref,Name]).
'ModuleDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_version',[Objref]).
'ModuleDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_moduledef,'_set_version',[Objref,Version]).
'ModuleDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_defined_in',[Objref]).
'ModuleDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_absolute_name',[Objref]).
'ModuleDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,'_get_containing_repository',
		     [Objref]).
'ModuleDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_moduledef,describe,[Objref]).
'ModuleDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_moduledef,move,
		     [Objref,New_container,New_name,New_version]).

'ConstantDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_def_kind',[Objref]).
'ConstantDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,destroy,[Objref]).
'ConstantDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_id',[Objref]).
'ConstantDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_constantdef,'_set_id',[Objref,Id]).
'ConstantDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_name',[Objref]).
'ConstantDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_constantdef,'_set_name',[Objref,Name]).
'ConstantDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_version',[Objref]).
'ConstantDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_constantdef,'_set_version',[Objref,Version]).
'ConstantDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_defined_in',[Objref]).
'ConstantDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_absolute_name',[Objref]).
'ConstantDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_containing_repository',
		     [Objref]).
'ConstantDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,describe,[Objref]).
'ConstantDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_constantdef,move,
		     [Objref,New_container,New_name,New_version]).
'ConstantDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_type',[Objref]).
'ConstantDef__get_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_type_def',[Objref]).
'ConstantDef__set_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_constantdef,'_set_type_def',[Objref,TypeDef]).
'ConstantDef__get_value'(Objref) ->
    exceptioncatcher(orber_ifr_constantdef,'_get_value',[Objref]).
'ConstantDef__set_value'(Objref,Value) ->
    exceptioncatcher(orber_ifr_constantdef,'_set_value',[Objref,Value]).

'TypedefDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_def_kind',[Objref]).
'TypedefDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,destroy,[Objref]).
'TypedefDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_id',[Objref]).
'TypedefDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_typedef,'_set_id',[Objref,Id]).
'TypedefDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_name',[Objref]).
'TypedefDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_typedef,'_set_name',[Objref,Name]).
'TypedefDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_version',[Objref]).
'TypedefDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_typedef,'_set_version',[Objref,Version]).
'TypedefDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_defined_in',[Objref]).
'TypedefDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_absolute_name',[Objref]).
'TypedefDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_containing_repository',[Objref]).
'TypedefDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,describe,[Objref]).
'TypedefDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_typedef,move,
		     [Objref,New_container,New_name,New_version]).
'TypedefDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_typedef,'_get_type',[Objref]).

'StructDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_def_kind',[Objref]).
'StructDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,destroy,[Objref]).
'StructDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_id',[Objref]).
'StructDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_structdef,'_set_id',[Objref,Id]).
'StructDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_name',[Objref]).
'StructDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_structdef,'_set_name',[Objref,Name]).
'StructDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_version',[Objref]).
'StructDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_structdef,'_set_version',[Objref,Version]).
'StructDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_defined_in',[Objref]).
'StructDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_absolute_name',[Objref]).
'StructDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_containing_repository',
		     [Objref]).
'StructDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,describe,[Objref]).
'StructDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_structdef,move,
		     [Objref,New_container,New_name,New_version]).
'StructDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_type',[Objref]).
'StructDef__get_members'(Objref) ->
    exceptioncatcher(orber_ifr_structdef,'_get_members',[Objref]).
'StructDef__set_members'(Objref,Members) ->
    exceptioncatcher(orber_ifr_structdef,'_set_members',[Objref,Members]).

'UnionDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_def_kind',[Objref]).
'UnionDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,destroy,[Objref]).
'UnionDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_id',[Objref]).
'UnionDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_uniondef,'_set_id',[Objref,Id]).
'UnionDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_name',[Objref]).
'UnionDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_uniondef,'_set_name',[Objref,Name]).
'UnionDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_version',[Objref]).
'UnionDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_uniondef,'_set_version',[Objref,Version]).
'UnionDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_defined_in',[Objref]).
'UnionDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_absolute_name',[Objref]).
'UnionDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_containing_repository',[Objref]).
'UnionDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,describe,[Objref]).
'UnionDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_uniondef,move,
		     [Objref,New_container,New_name,New_version]).
'UnionDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_type',[Objref]).
'UnionDef__get_discriminator_type'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_discriminator_type',[Objref]).
'UnionDef__get_discriminator_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_discriminator_type_def',
		     [Objref]).
'UnionDef__set_discriminator_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_uniondef,'_set_discriminator_type_def',
		     [Objref,TypeDef]).
'UnionDef__get_members'(Objref) ->
    exceptioncatcher(orber_ifr_uniondef,'_get_members',[Objref]).
'UnionDef__set_members'(Objref,Members) ->
    exceptioncatcher(orber_ifr_uniondef,'_set_members',[Objref,Members]).

'EnumDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_def_kind',[Objref]).
'EnumDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,destroy,[Objref]).
'EnumDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_id',[Objref]).
'EnumDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_enumdef,'_set_id',[Objref,Id]).
'EnumDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_name',[Objref]).
'EnumDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_enumdef,'_set_name',[Objref,Name]).
'EnumDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_version',[Objref]).
'EnumDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_enumdef,'_set_version',[Objref,Version]).
'EnumDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_defined_in',[Objref]).
'EnumDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_absolute_name',[Objref]).
'EnumDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_containing_repository',[Objref]).
'EnumDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,describe,[Objref]).
'EnumDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_enumdef,move,
		     [Objref,New_container,New_name,New_version]).
'EnumDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_type',[Objref]).
'EnumDef__get_members'(Objref) ->
    exceptioncatcher(orber_ifr_enumdef,'_get_members',[Objref]).
'EnumDef__set_members'(Objref,Members) ->
    exceptioncatcher(orber_ifr_enumdef,'_set_members',[Objref,Members]).

'AliasDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_def_kind',[Objref]).
'AliasDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,destroy,[Objref]).
'AliasDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_id',[Objref]).
'AliasDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_aliasdef,'_set_id',[Objref,Id]).
'AliasDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_name',[Objref]).
'AliasDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_aliasdef,'_set_name',[Objref,Name]).
'AliasDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_version',[Objref]).
'AliasDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_aliasdef,'_set_version',[Objref,Version]).
'AliasDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_defined_in',[Objref]).
'AliasDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_absolute_name',[Objref]).
'AliasDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_containing_repository',[Objref]).
'AliasDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,describe,[Objref]).
'AliasDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_aliasdef,move,
		     [Objref,New_container,New_name,New_version]).
'AliasDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_type',[Objref]).
'AliasDef__get_original_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_aliasdef,'_get_original_type_def',[Objref]).
'AliasDef__set_original_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_aliasdef,'_set_original_type_def',
		     [Objref,TypeDef]).

'PrimitiveDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_primitivedef,'_get_def_kind',[Objref]).
'PrimitiveDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_primitivedef,destroy,[Objref]).
'PrimitiveDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_primitivedef,'_get_type',[Objref]).
'PrimitiveDef__get_kind'(Objref) ->
    exceptioncatcher(orber_ifr_primitivedef,'_get_kind',[Objref]).

'StringDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_stringdef,'_get_def_kind',[Objref]).
'StringDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_stringdef,destroy,[Objref]).
'StringDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_stringdef,'_get_type',[Objref]).
'StringDef__get_bound'(Objref) ->
    exceptioncatcher(orber_ifr_stringdef,'_get_bound',[Objref]).
'StringDef__set_bound'(Objref,Bound) ->
    exceptioncatcher(orber_ifr_stringdef,'_set_bound',[Objref,Bound]).

'SequenceDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,'_get_def_kind',[Objref]).
'SequenceDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,destroy,[Objref]).
'SequenceDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,'_get_type',[Objref]).
'SequenceDef__get_bound'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,'_get_bound',[Objref]).
'SequenceDef__set_bound'(Objref,Bound) ->
    exceptioncatcher(orber_ifr_sequencedef,'_set_bound',[Objref,Bound]).
'SequenceDef__get_element_type'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,'_get_element_type',[Objref]).
'SequenceDef__get_element_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_sequencedef,'_get_element_type_def',[Objref]).
'SequenceDef__set_element_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_sequencedef,'_set_element_type_def',
		     [Objref,TypeDef]).

'ArrayDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,'_get_def_kind',[Objref]).
'ArrayDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,destroy,[Objref]).
'ArrayDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,'_get_type',[Objref]).
'ArrayDef__get_length'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,'_get_length',[Objref]).
'ArrayDef__set_length'(Objref,Length) ->
    exceptioncatcher(orber_ifr_arraydef,'_set_length',[Objref,Length]).
'ArrayDef__get_element_type'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,'_get_element_type',[Objref]).
'ArrayDef__get_element_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_arraydef,'_get_element_type_def',[Objref]).
'ArrayDef__set_element_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_arraydef,'_set_element_type_def',
		     [Objref,TypeDef]).

'ExceptionDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_def_kind',[Objref]).
'ExceptionDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,destroy,[Objref]).
'ExceptionDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_id',[Objref]).
'ExceptionDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_set_id',[Objref,Id]).
'ExceptionDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_name',[Objref]).
'ExceptionDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_set_name',[Objref,Name]).
'ExceptionDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_version',[Objref]).
'ExceptionDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_set_version',[Objref,Version]).
'ExceptionDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_defined_in',[Objref]).
'ExceptionDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_absolute_name',[Objref]).
'ExceptionDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_containing_repository',
		     [Objref]).
'ExceptionDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,describe,[Objref]).
'ExceptionDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_exceptiondef,move,
		     [Objref,New_container,New_name,New_version]).
'ExceptionDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_type',[Objref]).
'ExceptionDef__get_members'(Objref) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_get_members',[Objref]).
'ExceptionDef__set_members'(Objref,Members) ->
    exceptioncatcher(orber_ifr_exceptiondef,'_set_members',[Objref,Members]).

'AttributeDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_def_kind',[Objref]).
'AttributeDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,destroy,[Objref]).
'AttributeDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_id',[Objref]).
'AttributeDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_attributedef,'_set_id',[Objref,Id]).
'AttributeDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_name',[Objref]).
'AttributeDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_attributedef,'_set_name',[Objref,Name]).
'AttributeDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_version',[Objref]).
'AttributeDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_attributedef,'_set_version',[Objref,Version]).
'AttributeDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_defined_in',[Objref]).
'AttributeDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_absolute_name',[Objref]).
'AttributeDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_containing_repository',
		     [Objref]).
'AttributeDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,describe,[Objref]).
'AttributeDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_attributedef,move,
		     [Objref,New_container,New_name,New_version]).
'AttributeDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_type',[Objref]).
'AttributeDef__get_type_def'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_type_def',[Objref]).
'AttributeDef__set_type_def'(Objref,TypeDef) ->
    exceptioncatcher(orber_ifr_attributedef,'_set_type_def',[Objref,TypeDef]).
'AttributeDef__get_mode'(Objref) ->
    exceptioncatcher(orber_ifr_attributedef,'_get_mode',[Objref]).
'AttributeDef__set_mode'(Objref,Mode) ->
    exceptioncatcher(orber_ifr_attributedef,'_set_mode',[Objref,Mode]).

'OperationDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_def_kind',[Objref]).
'OperationDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,destroy,[Objref]).
'OperationDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_id',[Objref]).
'OperationDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_id',[Objref,Id]).
'OperationDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_name',[Objref]).
'OperationDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_name',[Objref,Name]).
'OperationDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_version',[Objref]).
'OperationDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_version',[Objref,Version]).
'OperationDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_defined_in',[Objref]).
'OperationDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_absolute_name',[Objref]).
'OperationDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_containing_repository',
		     [Objref]).
'OperationDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,describe,[Objref]).
'OperationDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_operationdef,move,
		     [Objref,New_container,New_name,New_version]).
'OperationDef__get_result'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_result',[Objref]).
'OperationDef__get_result_def'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_result_def',[Objref]).
'OperationDef__set_result_def'(Objref,ResultDef) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_result_def',
		     [Objref,ResultDef]).
'OperationDef__get_params'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_params',[Objref]).
'OperationDef__set_params'(Objref,Params) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_params',[Objref,Params]).
'OperationDef__get_mode'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_mode',[Objref]).
'OperationDef__set_mode'(Objref,Mode) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_mode',[Objref,Mode]).
'OperationDef__get_contexts'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_contexts',[Objref]).
'OperationDef__set_contexts'(Objref,Contexts) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_contexts',[Objref,Contexts]).
'OperationDef__get_exceptions'(Objref) ->
    exceptioncatcher(orber_ifr_operationdef,'_get_exceptions',[Objref]).
'OperationDef__set_exceptions'(Objref,Exceptions) ->
    exceptioncatcher(orber_ifr_operationdef,'_set_exceptions',
		     [Objref,Exceptions]).

'InterfaceDef__get_def_kind'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_def_kind',[Objref]).
'InterfaceDef_destroy'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,destroy,[Objref]).
'InterfaceDef_lookup'(Objref,Search_name) ->
    exceptioncatcher(orber_ifr_interfacedef,lookup,[Objref,Search_name]).
'InterfaceDef_contents'(Objref,Limit_type,Exclude_inherited) ->
    exceptioncatcher(orber_ifr_interfacedef,contents,
		     [Objref,Limit_type,Exclude_inherited]).
'InterfaceDef_lookup_name'(Objref,Search_name,Levels_to_search,Limit_type,
			   Exclude_inherited) ->
    exceptioncatcher(orber_ifr_interfacedef,lookup_name,
		     [Objref,Search_name,Levels_to_search,Limit_type,
		      Exclude_inherited]).
'InterfaceDef_describe_contents'(Objref,Limit_type,Exclude_inherited,
				 Max_returned_objs) ->
    exceptioncatcher(orber_ifr_interfacedef,describe_contents,
		     [Objref,Limit_type,Exclude_inherited,Max_returned_objs]).
'InterfaceDef_create_module'(Objref,Id,Name,Version) ->
    exceptioncatcher(orber_ifr_interfacedef,create_module,
		     [Objref,Id,Name,Version]).
'InterfaceDef_create_constant'(Objref,Id,Name,Version,Type,Value) ->
    exceptioncatcher(orber_ifr_interfacedef,create_constant,
		     [Objref,Id,Name,Version,Type,Value]).
'InterfaceDef_create_struct'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_interfacedef,create_struct,
		     [Objref,Id,Name,Version,Members]).
'InterfaceDef_create_union'(Objref,Id,Name,Version,Discriminator_type,
			    Members) ->
    exceptioncatcher(orber_ifr_interfacedef,create_union,
		     [Objref,Id,Name,Version,Discriminator_type,Members]).
'InterfaceDef_create_enum'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_interfacedef,create_enum,
		     [Objref,Id,Name,Version,Members]).
'InterfaceDef_create_alias'(Objref,Id,Name,Version,Original_type) ->
    exceptioncatcher(orber_ifr_interfacedef,create_alias,
		     [Objref,Id,Name,Version,Original_type]).
'InterfaceDef_create_interface'(Objref,Id,Name,Version,Base_interfaces) ->
    exceptioncatcher(orber_ifr_interfacedef,create_interface,
		     [Objref,Id,Name,Version,Base_interfaces]).
'InterfaceDef_create_exception'(Objref,Id,Name,Version,Members) ->
    exceptioncatcher(orber_ifr_interfacedef,create_exception,
		     [Objref,Id,Name,Version,Members]).
'InterfaceDef__get_id'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_id',[Objref]).
'InterfaceDef__set_id'(Objref,Id) ->
    exceptioncatcher(orber_ifr_interfacedef,'_set_id',[Objref,Id]).
'InterfaceDef__get_name'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_name',[Objref]).
'InterfaceDef__set_name'(Objref,Name) ->
    exceptioncatcher(orber_ifr_interfacedef,'_set_name',[Objref,Name]).
'InterfaceDef__get_version'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_version',[Objref]).
'InterfaceDef__set_version'(Objref,Version) ->
    exceptioncatcher(orber_ifr_interfacedef,'_set_version',[Objref,Version]).
'InterfaceDef__get_defined_in'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_defined_in',[Objref]).
'InterfaceDef__get_absolute_name'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_absolute_name',[Objref]).
'InterfaceDef__get_containing_repository'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_containing_repository',
		     [Objref]).
'InterfaceDef_describe'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,describe,[Objref]).
'InterfaceDef_move'(Objref,New_container,New_name,New_version) ->
    exceptioncatcher(orber_ifr_interfacedef,move,
		     [Objref,New_container,New_name,New_version]).
'InterfaceDef__get_type'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_type',[Objref]).
'InterfaceDef__get_base_interfaces'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,'_get_base_interfaces',[Objref]).
'InterfaceDef__set_base_interfaces'(Objref,BaseInterfaces) ->
    exceptioncatcher(orber_ifr_interfacedef,'_set_base_interfaces',
		     [Objref,BaseInterfaces]).
'InterfaceDef_is_a'(Objref,Interface_id) ->
    exceptioncatcher(orber_ifr_interfacedef,is_a,[Objref,Interface_id]).
'InterfaceDef_describe_interface'(Objref) ->
    exceptioncatcher(orber_ifr_interfacedef,describe_interface,[Objref]).
'InterfaceDef_create_attribute'(Objref,Id,Name,Version,Type,Mode) ->
    exceptioncatcher(orber_ifr_interfacedef,create_attribute,
		     [Objref,Id,Name,Version,Type,Mode]).
'InterfaceDef_create_operation'(Objref,Id,Name,Version,Result,Mode,Params,
				Exceptions,Contexts) ->
    exceptioncatcher(orber_ifr_interfacedef,create_operation,
		     [Objref,Id,Name,Version,Result,Mode,Params,Exceptions,
		      Contexts]).

%%'TypeCode_equal'(Objref,Tc) ->
%%    exceptioncatcher(orber_ifr_typecode,equal,[Objref,Tc]).
%%'TypeCode_kind'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,kind,[Objref]).
%%'TypeCode_id'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,id,[Objref]).
%%'TypeCode_name'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,name,[Objref]).
%%'TypeCode_member_count'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,member_count,[Objref]).
%%'TypeCode_member_name'(Objref,Index) ->
%%    exceptioncatcher(orber_ifr_typecode,member_name,[Objref,Index]).
%%'TypeCode_member_type'(Objref,Index) ->
%%    exceptioncatcher(orber_ifr_typecode,member_type,[Objref,Index]).
%%'TypeCode_member_label'(Objref,Index) ->
%%    exceptioncatcher(orber_ifr_typecode,member_label,[Objref,Index]).
%%'TypeCode_discriminator_type'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,discriminator_type,[Objref]).
%%'TypeCode_default_index'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,default_index,[Objref]).
%%'TypeCode_length'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,length,[Objref]).
%%'TypeCode_content_type'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,content_type,[Objref]).
%%'TypeCode_param_count'(Objref) ->
%%    exceptioncatcher(orber_ifr_typecode,param_count,[Objref]).
%%'TypeCode_parameter'(Objref,Index) ->
%%    exceptioncatcher(orber_ifr_typecode,parameter,[Objref,Index]).

'ORB_create_struct_tc'(Id,Name,Members) ->
    exceptioncatcher(orber_ifr_orb,create_struct_tc,[Id,Name,Members]).
'ORB_create_union_tc'(Id,Name,Discriminator_type,Members) ->
    exceptioncatcher(orber_ifr_orb,create_union_tc,
		     [Id,Name,Discriminator_type,Members]).
'ORB_create_enum_tc'(Id,Name,Members) ->
    exceptioncatcher(orber_ifr_orb,create_enum_tc,[Id,Name,Members]).
'ORB_create_alias_tc'(Id,Name,Original_type) ->
    exceptioncatcher(orber_ifr_orb,create_alias_tc,
		     [Id,Name,Original_type]).
'ORB_create_exception_tc'(Id,Name,Members) ->
    exceptioncatcher(orber_ifr_orb,create_exception_tc,
		     [Id,Name,Members]).
'ORB_create_interface_tc'(Id,Name) ->
    exceptioncatcher(orber_ifr_orb,create_interface_tc,[Id,Name]).
'ORB_create_string_tc'(Bound) ->
    exceptioncatcher(orber_ifr_orb,create_string_tc,[Bound]).
'ORB_create_sequence_tc'(Bound,Element_type) ->
    exceptioncatcher(orber_ifr_orb,create_sequence_tc,
		     [Bound,Element_type]).
'ORB_create_recursive_sequence_tc'(Bound,Offset) ->
    exceptioncatcher(orber_ifr_orb,create_recursive_sequence_tc,
		     [Bound,Offset]).
'ORB_create_array_tc'(Length,Element_type) ->
    exceptioncatcher(orber_ifr_orb,create_array_tc,
		     [Length,Element_type]).

%%%---------------------------------------------------------------
%%% "Methods" of the IFR "objects"

get_def_kind(Objref) ->
    dispatch([Objref],[ir_IRObject,ir_Contained,ir_Container,ir_IDLType,
		       ir_Repository,ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_PrimitiveDef,ir_StringDef,ir_SequenceDef,ir_ArrayDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_def_kind').

destroy(Objref) ->
    dispatch([Objref],[				% Destroying an
		       %%ir_IRObject,		% ir_IRObject, an
		       %%ir_Contained,		% ir_Contained or an
		       %%ir_Container,		% ir_Container directly
						% is not allowed
		       ir_IDLType,
		       ir_Repository,ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_PrimitiveDef,ir_StringDef,ir_SequenceDef,ir_ArrayDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     destroy).

%%%---------------------------------------------------------------
%%%

get_id(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_id').

set_id(Objref,Id) ->
    dispatch([Objref,Id],[ir_Contained,
			  ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
			  ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
			  ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
			  ir_InterfaceDef],
	     '_set_id').

get_name(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_name').

set_name(Objref,Name) ->
    dispatch([Objref,Name],[ir_Contained,
			    ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
			    ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
			    ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
			    ir_InterfaceDef],
	     '_set_name').

get_version(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_version').

set_version(Objref,Version) ->
    dispatch([Objref,Version],[ir_Contained,
			       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
			       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
			       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
			       ir_InterfaceDef],
	     '_set_version').

get_defined_in(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_defined_in').

get_absolute_name(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_absolute_name').

get_containing_repository(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     '_get_containing_repository').

describe(Objref) ->
    dispatch([Objref],[ir_Contained,
		       ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
		       ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
		       ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
		       ir_InterfaceDef],
	     describe).

move(Objref,New_container,New_name,New_version) ->
    dispatch([Objref,New_container,New_name,New_version],
	     [ir_Contained,
	      ir_ModuleDef,ir_ConstantDef,ir_TypedefDef,
	      ir_StructDef,ir_UnionDef,ir_EnumDef,ir_AliasDef,
	      ir_ExceptionDef,ir_AttributeDef,ir_OperationDef,
	      ir_InterfaceDef],
	     move).

%%%---------------------------------------------------------------
%%% 

lookup(Objref,Search_name) ->
    dispatch([Objref,Search_name],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     lookup).

contents(Objref,Limit_type,Exclude_inherited) ->
    dispatch([Objref,Limit_type,Exclude_inherited],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     contents).

lookup_name(Objref,Search_name,Levels_to_search,Limit_type,Exclude_inherited) ->
    dispatch([Objref,Search_name,Levels_to_search,Limit_type,Exclude_inherited],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     lookup_name).

describe_contents(Objref,Limit_type,Exclude_inherited,Max_returned_objs) ->
    dispatch([Objref,Limit_type,Exclude_inherited,Max_returned_objs],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     describe_contents).

create_module(Objref,Id,Name,Version) ->
    dispatch([Objref,Id,Name,Version],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_module).

create_constant(Objref,Id,Name,Version,Type,Value) ->
    dispatch([Objref,Id,Name,Version,Type,Value],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_constant).

create_struct(Objref,Id,Name,Version,Members) ->
    dispatch([Objref,Id,Name,Version,Members],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_struct).

create_union(Objref,Id,Name,Version,Discriminator_type,Members) ->
    dispatch([Objref,Id,Name,Version,Discriminator_type,Members],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_union).

create_enum(Objref,Id,Name,Version,Members) ->
    dispatch([Objref,Id,Name,Version,Members],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_enum).

create_alias(Objref,Id,Name,Version,Original_type) ->
    dispatch([Objref,Id,Name,Version,Original_type],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_alias).

create_interface(Objref,Id,Name,Version,Base_interfaces) ->
    dispatch([Objref,Id,Name,Version,Base_interfaces],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_interface).

create_exception(Objref,Id,Name,Version,Members) ->
    dispatch([Objref,Id,Name,Version,Members],
	     [ir_Container,ir_Repository,ir_ModuleDef,ir_InterfaceDef],
	     create_exception).

%%%---------------------------------------------------------------
%%% 

get_type(Objref) ->
    dispatch([Objref],[ir_IDLType,ir_ConstantDef,ir_TypedefDef,ir_StructDef,
		      ir_UnionDef,ir_EnumDef,ir_AliasDef,ir_PrimitiveDef,
		      ir_StringDef,ir_SequenceDef,ir_ArrayDef,ir_ExceptionDef,
		      ir_AttributeDef,ir_InterfaceDef],
	     '_get_type').

%%%---------------------------------------------------------------
%%% 

lookup_id(Objref,Search_id) ->
    dispatch([Objref,Search_id],[ir_Repository],
	     lookup_id).

get_primitive(Objref,Kind) ->
    dispatch([Objref,Kind],[ir_Repository],
	     get_primitive).

create_string(Objref,Bound) ->
    dispatch([Objref,Bound],[ir_Repository],
	     create_string).

create_sequence(Objref,Bound,Element_type) ->
    dispatch([Objref,Bound,Element_type],[ir_Repository],
	     create_sequence).

create_array(Objref,Length,Element_type) ->
    dispatch([Objref,Length,Element_type],[ir_Repository],
	     create_array).

create_idltype(Objref,Typecode) ->		%not in CORBA 2.0
    dispatch([Objref,Typecode],[ir_Repository],
	     create_idltype).

%%%---------------------------------------------------------------
%%% 

get_type_def(Objref) ->
    dispatch([Objref],[ir_ConstantDef,ir_AttributeDef],
	     '_get_type_def').

set_type_def(Objref,TypeDef) ->
    dispatch([Objref,TypeDef],[ir_ConstantDef,ir_AttributeDef],
	     '_set_type_def').

get_value(Objref) ->
    dispatch([Objref],[ir_ConstantDef],
	     '_get_value').

set_value(Objref,Value) ->
    dispatch([Objref,Value],[ir_ConstantDef],
	     '_set_value').

%%%---------------------------------------------------------------
%%% 

get_members(Objref) ->
    dispatch([Objref],[ir_StructDef,ir_UnionDef,ir_EnumDef,ir_ExceptionDef],
	     '_get_members').

set_members(Objref,Members) ->
    dispatch([Objref,Members],
	     [ir_StructDef,ir_UnionDef,ir_EnumDef,ir_ExceptionDef],
	     '_set_members').

%%%---------------------------------------------------------------
%%% 

get_discriminator_type(Objref) ->
    dispatch([Objref],[ir_UnionDef],
	     '_get_discriminator_type').

get_discriminator_type_def(Objref) ->
    dispatch([Objref],[ir_UnionDef],
	     '_get_discriminator_type_def').

set_discriminator_type_def(Objref,TypeDef) ->
    dispatch([Objref,TypeDef],[ir_UnionDef],
	     '_set_discriminator_type_def').

%%%---------------------------------------------------------------
%%% 

get_original_type_def(Objref) ->
    dispatch([Objref],[ir_AliasDef],
	     '_get_original_type_def').

set_original_type_def(Objref,TypeDef) ->
    dispatch([Objref,TypeDef],[ir_AliasDef],
	     '_set_original_type_def').

%%%---------------------------------------------------------------
%%% 

get_kind(Objref) ->
    dispatch([Objref],[ir_PrimitiveDef],
	     '_get_kind').

%%%---------------------------------------------------------------
%%% 

get_bound(Objref) ->
    dispatch([Objref],[ir_StringDef,ir_SequenceDef],
	     '_get_bound').

set_bound(Objref,Bound) ->
    dispatch([Objref,Bound],[ir_StringDef,ir_SequenceDef],
	     '_set_bound').

%%%---------------------------------------------------------------
%%% 

get_element_type(Objref) ->
    dispatch([Objref],[ir_SequenceDef,ir_ArrayDef],
	     '_get_element_type').

get_element_type_def(Objref) ->
    dispatch([Objref],[ir_SequenceDef,ir_ArrayDef],
	     '_get_element_type_def').

set_element_type_def(Objref,TypeDef) ->
    dispatch([Objref,TypeDef],[ir_SequenceDef,ir_ArrayDef],
	     '_set_element_type_def').

%%%---------------------------------------------------------------
%%% 

get_length(Objref) ->
    dispatch([Objref],[ir_ArrayDef],
	     '_get_length').

set_length(Objref,Length) ->
    dispatch([Objref,Length],[ir_ArrayDef],
	     '_set_length').

%%%---------------------------------------------------------------
%%% 

get_mode(Objref) ->
    dispatch([Objref],[ir_AttributeDef,ir_OperationDef],
	     '_get_mode').

set_mode(Objref,Mode) ->
    dispatch([Objref,Mode],[ir_AttributeDef,ir_OperationDef],
	     '_set_mode').

%%%---------------------------------------------------------------
%%% 

get_result(Objref) ->
    dispatch([Objref],[ir_OperationDef],
	     '_get_result').

get_result_def(Objref) ->
    dispatch([Objref],[ir_OperationDef],
	     '_get_result_def').

set_result_def(Objref,ResultDef) ->
    dispatch([Objref,ResultDef],[ir_OperationDef],
	     '_set_result_def').

get_params(Objref) ->
    dispatch([Objref],[ir_OperationDef],
	     '_get_params').

set_params(Objref,Params) ->
    dispatch([Objref,Params],[ir_OperationDef],
	     '_set_params').

get_contexts(Objref) ->
    dispatch([Objref],[ir_OperationDef],
	     '_get_contexts').

set_contexts(Objref,Contexts) ->
    dispatch([Objref,Contexts],[ir_OperationDef],
	     '_set_contexts').

get_exceptions(Objref) ->
    dispatch([Objref],[ir_OperationDef],
	     '_get_exceptions').

set_exceptions(Objref,Exceptions) ->
    dispatch([Objref,Exceptions],[ir_OperationDef],
	     '_set_exceptions').

%%%---------------------------------------------------------------
%%% 

get_base_interfaces(Objref) ->
    dispatch([Objref],[ir_InterfaceDef],
	     '_get_base_interfaces').

set_base_interfaces(Objref,BaseInterfaces) ->
    dispatch([Objref,BaseInterfaces],[ir_InterfaceDef],
	     '_set_base_interfaces').

is_a(Objref,Interface_id) ->
    dispatch([Objref,Interface_id],[ir_InterfaceDef],
	     is_a).

describe_interface(Objref) ->
    dispatch([Objref],[ir_InterfaceDef],
	     describe_interface).

create_attribute(Objref,Id,Name,Version,Type,Mode) ->
    dispatch([Objref,Id,Name,Version,Type,Mode],[ir_InterfaceDef],
	     create_attribute).

create_operation(Objref,Id,Name,Version,Result,Mode,Params,Exceptions,Contexts) ->
    dispatch([Objref,Id,Name,Version,Result,Mode,Params,Exceptions,Contexts],
	     [ir_InterfaceDef],
	     create_operation).

dispatch(Parameterlist, Dispatchlist, Operation) ->
    [{ObjType, _} | _] = Parameterlist,
    case lists:keysearch(ObjType, 1,
			 [{ir_IRObject,orber_ifr_irobject},
			  {ir_Contained,orber_ifr_contained},
			  {ir_Container,orber_ifr_container},
			  {ir_IDLType,orber_ifr_idltype},
			  {ir_Repository,orber_ifr_repository},
			  {ir_ModuleDef,orber_ifr_moduledef},
			  {ir_ConstantDef,orber_ifr_constantdef},
			  {ir_TypedefDef,orber_ifr_typedef},
			  {ir_StructDef,orber_ifr_structdef},
			  {ir_UnionDef,orber_ifr_uniondef},
			  {ir_EnumDef,orber_ifr_enumdef},
			  {ir_AliasDef,orber_ifr_aliasdef},
			  {ir_PrimitiveDef,orber_ifr_primitivedef},
			  {ir_StringDef,orber_ifr_stringdef},
			  {ir_SequenceDef,orber_ifr_sequencedef},
			  {ir_ArrayDef,orber_ifr_arraydef},
			  {ir_ExceptionDef,orber_ifr_exceptiondef},
			  {ir_AttributeDef,orber_ifr_attributedef},
			  {ir_OperationDef,orber_ifr_operationdef},
			  {ir_InterfaceDef,orber_ifr_interfacedef}])
	of
	{value, {_, Module}} ->
	    exceptioncatcher(Module,Operation,Parameterlist);
	_ ->
	    ?ifr_exception("Unknown dispatch: ",
			   {Parameterlist,Dispatchlist,Operation})
    end.
