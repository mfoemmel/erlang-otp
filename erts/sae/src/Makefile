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

# ----------------------------------------------------
# Bundle definitions.
# ----------------------------------------------------

ERL_BUNDLE_MODS = otp_pre_init init erl_prim_loader
ERL_BUNDLE_FILES = $(ERL_BUNDLE_MODS:%=$(KERNEL)/%.$(EMULATOR))
ERL_BUNDLE = $(ERL_TOP)/erts/sae/bundles/erl.$(EMULATOR).bundle

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
	ebundle

BUNDLES= $(ERL_BUNDLE)

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

debug opt: $(TARGET_FILES) $(BUNDLES)


clean:
	rm -f $(TARGET_FILES)
	rm -f core

docs:

$(ERL_BUNDLE): $(ERL_BUNDLE_FILES)
	erl -noshell -pa ../ebin -s ebundle make_bundle $(ERL_BUNDLE) $(ERL_BUNDLE_FILES)


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






