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
	primes \
	ebundle

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -I../include -I../../kernel/include

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES) $(PRIMES_BIN)


clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

$(PRIMES_BIN): $(PRIMES_MOD)
	erl -noshell -pa ../ebin -s ebundle unix $(PRIMES_BIN) $(PRIMES_MOD)

$(PRIMES_BUNDLE): $(PRIMES_MOD)
	erl -noshell -pa ../ebin -s ebundle win32 $(PRIMES_BUNDLE) $(PRIMES_MOD)


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# If .beam files in lib aren't built we use this rule.
$(ERL_TOP)/lib/%.beam:
	cd $(@D)/../src && $(MAKE) ../ebin/$(@F)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_docs_spec:






