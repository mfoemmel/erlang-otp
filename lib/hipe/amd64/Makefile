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
MODULES=hipe_amd64 \
	hipe_amd64_assemble \
	hipe_amd64_cfg \
	hipe_amd64_defuse \
	hipe_amd64_encode \
	hipe_amd64_frame \
	hipe_amd64_liveness \
	hipe_amd64_main \
	hipe_amd64_postpass \
	hipe_amd64_pp \
	hipe_amd64_ra \
	hipe_amd64_ra_dummy \
	hipe_amd64_ra_finalise \
	hipe_amd64_ra_ls_postconditions \
	hipe_amd64_ra_ls \
	hipe_amd64_ra_postconditions \
	hipe_amd64_ra_sse2_postconditions \
	hipe_amd64_ra_x87_ls \
	hipe_amd64_registers \
	hipe_amd64_x87 \
	hipe_rtl_to_amd64

HRL_FILES=hipe_amd64.hrl
ERL_FILES=$(MODULES:%=%.erl)
TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS +=
ERL_COMPILE_FLAGS +=

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
$(EBIN)/hipe_amd64.beam: hipe_amd64.hrl
$(EBIN)/hipe_amd64_assemble.beam: ../main/hipe.hrl ../rtl/hipe_literals.hrl hipe_amd64.hrl ../../kernel/src/hipe_ext_format.hrl ../misc/hipe_sdi.hrl
$(EBIN)/hipe_amd64_cfg.beam: ../flow/cfg.inc ../main/hipe.hrl hipe_amd64.hrl
$(EBIN)/hipe_amd64_defuse.beam: hipe_amd64.hrl
$(EBIN)/hipe_amd64_frame.beam: ../main/hipe.hrl hipe_amd64.hrl
$(EBIN)/hipe_amd64_liveness.beam: ../flow/liveness.inc
$(EBIN)/hipe_amd64_main.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_postpass.beam: hipe_amd64.hrl
$(EBIN)/hipe_amd64_pp.beam: hipe_amd64.hrl
$(EBIN)/hipe_amd64_ra.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_ra_dummy.beam: ../main/hipe.hrl hipe_amd64.hrl
$(EBIN)/hipe_amd64_ra_finalise.beam: hipe_amd64.hrl
$(EBIN)/hipe_amd64_ra_ls.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_ra_ls_postconditions.beam: ../main/hipe.hrl hipe_amd64.hrl
$(EBIN)/hipe_amd64_ra_postconditions.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_ra_sse2_postconditions.beam: ../main/hipe.hrl
$(EBIN)/hipe_amd64_registers.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_rtl_to_amd64.beam: hipe_amd64.hrl
