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
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

KERNEL = $(ERL_TOP)/lib/kernel/ebin

PRIMES_MOD = $(ERL_TOP)/erts/sae/ebin/primes.beam
PRIMES_BIN = $(ERL_TOP)/erts/sae/bin/primes
PRIMES_BUNDLE = $(ERL_TOP)/erts/sae/bundles/primes.bundle

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/sae

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES= \
	ring0 \
	elink \
        esh \
	erl_prim_loader \
	prim_file \
	fake_ecc_code \
	init \
	erl_root_dir \
	fake_ecc_error_handler \
        make_sae_tools \
	erlang \
	erl_open_port \
	fake_ecc_init

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR)) ecc

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include -I$(ERL_TOP)/lib/kernel/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)


clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/init.beam: ../../../lib/kernel/src/init.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/erl_prim_loader.beam: ../../../lib/kernel/src/erl_prim_loader.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/prim_file.beam: ../../../lib/kernel/src/prim_file.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/erlang.beam: ../../../lib/kernel/src/erlang.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/erl_open_port.beam: ../../../lib/kernel/src/erl_open_port.erl
	$(ERLC) -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

BINDIR=$(ERL_TOP)/bin/$(TARGET)
START_ERL = PATH=$(PATH):$(BINDIR) ROOTDIR=$(ERL_TOP) BINDIR=$(BINDIR) \
 EMU=beam PROGNAME=erl $(ERL_TOP)/bin/$(TARGET)/erlexec

ecc: ${MODS:%=%.beam}
	$(START_ERL) -noshell -pa ../ebin -run make_sae_tools all -s erlang halt
	## this command patches ecc and esh with the
	## correct path to the sources
	PATH=$(PATH):$(BINDIR) ./ecc -install $(ERL_TOP)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_docs_spec:
