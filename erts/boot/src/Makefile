# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include ../../vsn.mk

BEAM_EVM=$(ERL_TOP)/bin/$(TARGET)/beam_evm
ERLANG_EARS=$(ERL_TOP)/erts/boot/src/
STDLIB_INCLUDE=$(ERL_TOP)/lib/stdlib/include
ERLANG_BEAM=$(ERL_TOP)/lib/kernel/ebin/erlang.beam

# We override settings in otp.mk
ERL_COMPILE_FLAGS = -I $(STDLIB_INCLUDE)
ECC_FLAGS = -W

MODS = boot_tools coff boot_pds boot_code_loader bin_io boot_fprim\
       boot_linker ecc elink

FAKES = fake_demand_epp.beam fake_error_handler.beam fake_demand_init.beam

BEAM = ${MODS:%=%.beam}

ifneq ($(findstring vxworks,$(TARGET)),vxworks)
debug opt: ${BEAM} ${FAKES} elink escript ear esh
else
debug opt:
endif

examples:
	$(MAKE) -f Makefile.examples

##----------------------------------------------------------------------
## Will also make erlang.ear and ecc.

elink: ${FAKES} ${BEAM}
	echo "boot_tools:make_tools(), halt()." | erl -env ERL_BEAM_EVM $(BEAM_EVM)
	echo "erlang.ear, ecc, and elink have been made"

##----------------------------------------------------------------------
## Build escript ear and esh using ecc and elink

escript: escript.beam
	./elink -t unix -d -o escript -S escript -m escript.beam 

escript.beam: escript.erl
	./ecc $(ECC_FLAGS) escript.erl

## Build ear

ear: ear.beam elink 
	./elink -t unix -d -o ear -S ear -m ear.beam

##----------------------------------------------------------------------

esh: esh.beam shell.beam
	./elink -t unix -d -o esh -S esh -m esh.beam shell.beam

esh.beam: esh.erl
	./ecc $(ECC_FLAGS) esh.erl

shell.beam: shell.erl
	./ecc $(ECC_FLAGS) shell.erl

##----------------------------------------------------------------------

fake_error_handler.beam: fake_error_handler.erl
	echo "compile:file(fake_error_handler), halt()." | erl

fake_demand_init.beam: fake_demand_init.erl
	echo "compile:file(fake_demand_init), halt()." | erl

fake_demand_epp.beam: fake_demand_epp.erl
	echo "compile:file(fake_demand_epp), halt()." | erl

boot_linker.beam: stub.hrl boot_linker.erl
	erlc boot_linker.erl

stub.hrl: 
	echo "boot_tools:make_stub(), halt()." | erl 

##----------------------------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

RELSYSDIR = $(RELEASE_PATH)/erts-$(VSN)

release_spec: opt
ifneq ($(findstring vxworks,$(TARGET)),vxworks)
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) erlang.ear $(RELSYSDIR)/erlang.ear
	$(INSTALL_DIR) $(RELSYSDIR)/bin
	$(INSTALL_SCRIPT) ecc elink escript ear esh $(RELSYSDIR)/bin
endif

bootstrap: ${BEAM} ${FAKES} elink
ifneq ($(findstring vxworks,$(TARGET)),vxworks)
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA) erlang.ear $(RELSYSDIR)/erlang.ear
	$(INSTALL_DIR) $(RELSYSDIR)/bin
	$(INSTALL_PROGRAM) ecc elink $(RELSYSDIR)/bin
endif

release_docs_spec:

clean:
	rm -f  *.beam stub.hrl sae1 sae2 sae3 all_defined.erl
	rm -f *.ear elink escript
	rm -f ecc ear esh
	rm -rf rpm
