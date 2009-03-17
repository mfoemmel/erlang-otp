%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: oe_OrberIFR
%% Source: /net/shelob/ldisk/daily_build/otp_prebuild_r13a.2009-03-16_22/otp_src_R13A/lib/orber/src/OrberIFR.idl
%% IC vsn: 4.2.20
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module(oe_OrberIFR).
-ic_compiled("4_2_20").


-include_lib("orber/include/ifr_types.hrl").

%% Interface functions

-export([oe_register/0, oe_unregister/0, oe_get_module/5]).
-export([oe_dependency/0]).



oe_register() ->
    OE_IFR = orber_ifr:find_repository(),

    register_tests(OE_IFR),

    _OE_1 = oe_get_top_module(OE_IFR, "IDL:OrberApp:1.0", "OrberApp", "1.0"),

    _OE_2 = orber_ifr:'ModuleDef_create_interface'(_OE_1, "IDL:OrberApp/IFR:1.0", "IFR", "1.0", []),

    orber_ifr:'InterfaceDef_create_operation'(_OE_2, "IDL:OrberApp/IFR/get_absolute_name:1.0", "get_absolute_name", "1.0", orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,0}), 'OP_NORMAL', [#parameterdescription{name="TypeID", type={tk_string,0}, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,0}), mode='PARAM_IN'}
], [], []),

    orber_ifr:'InterfaceDef_create_operation'(_OE_2, "IDL:OrberApp/IFR/get_user_exception_type:1.0", "get_user_exception_type", "1.0", orber_ifr:'Repository_create_idltype'(OE_IFR, tk_TypeCode), 'OP_NORMAL', [#parameterdescription{name="TypeID", type={tk_string,0}, type_def=orber_ifr:'Repository_create_idltype'(OE_IFR, {tk_string,0}), mode='PARAM_IN'}
], [], []),

    ok.


%% General IFR registration checks.
register_tests(OE_IFR)->
  re_register_test(OE_IFR),
  include_reg_test(OE_IFR).


%% IFR type Re-registration checks.
re_register_test(OE_IFR)->
  case orber_ifr:'Repository_lookup_id'(OE_IFR,"IDL:OrberApp/IFR:1.0") of
    []  ->
      true;
    _ ->
      exit({allready_registered,"IDL:OrberApp/IFR:1.0"})
 end.


%% No included idl-files detected.
include_reg_test(_OE_IFR) -> true.


%% Fetch top module reference, register if unregistered.
oe_get_top_module(OE_IFR, ID, Name, Version) ->
  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of
    [] ->
      orber_ifr:'Repository_create_module'(OE_IFR, ID, Name, Version);
    Mod  ->
      Mod
   end.

%% Fetch module reference, register if unregistered.
oe_get_module(OE_IFR, OE_Parent, ID, Name, Version) ->
  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of
    [] ->
      orber_ifr:'ModuleDef_create_module'(OE_Parent, ID, Name, Version);
    Mod  ->
      Mod
   end.



oe_unregister() ->
    OE_IFR = orber_ifr:find_repository(),

    oe_destroy(OE_IFR, "IDL:OrberApp/IFR:1.0"),
    oe_destroy_if_empty(OE_IFR, "IDL:OrberApp:1.0"),
    ok.


oe_destroy_if_empty(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    case orber_ifr:contents(Ref, 'dk_All', 'true') of
		[] ->
		    orber_ifr:destroy(Ref),
		    ok;
		_ ->
		    ok
	    end
    end.

oe_destroy(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    orber_ifr:destroy(Ref),
	    ok
    end.



%% Idl file dependency list function
oe_dependency() ->

    [].

