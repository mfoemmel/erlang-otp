# $Id$

ifndef EBIN
EBIN = ../ebin
endif

ifndef DOCS
DOCS = ../doc
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
# Please keep this list sorted.
MODULES=hipe_rtl_to_sparc \
	hipe_sparc \
	hipe_sparc_assemble \
	hipe_sparc_cfg \
	hipe_sparc_defuse \
	hipe_sparc_encode \
	hipe_sparc_finalise \
	hipe_sparc_frame \
	hipe_sparc_liveness_all \
	hipe_sparc_liveness_fpr \
	hipe_sparc_liveness_gpr \
	hipe_sparc_main \
	hipe_sparc_pp \
	hipe_sparc_ra \
	hipe_sparc_ra_finalise \
	hipe_sparc_ra_ls \
	hipe_sparc_ra_naive \
	hipe_sparc_ra_postconditions \
	hipe_sparc_ra_postconditions_fp \
	hipe_sparc_registers

HRL_FILES=hipe_sparc.hrl
ERL_FILES=$(MODULES:%=%.erl)
TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += +warn_obsolete_guard

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin

release_docs_spec:

# Please keep this list sorted.
$(EBIN)/hipe_rtl_to_sparc.beam: ../rtl/hipe_rtl.hrl
$(EBIN)/hipe_sparc_assemble.beam: ../main/hipe.hrl ../../kernel/src/hipe_ext_format.hrl ../rtl/hipe_literals.hrl ../misc/hipe_sdi.hrl
$(EBIN)/hipe_sparc_cfg.beam: ../flow/cfg.hrl ../flow/cfg.inc
$(EBIN)/hipe_sparc_frame.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_sparc_liveness_all.beam: ../flow/liveness.inc
$(EBIN)/hipe_sparc_liveness_fpr.beam: ../flow/liveness.inc
$(EBIN)/hipe_sparc_liveness_gpr.beam: ../flow/liveness.inc
$(EBIN)/hipe_sparc_registers.beam: ../rtl/hipe_literals.hrl

$(TARGET_FILES): hipe_sparc.hrl
