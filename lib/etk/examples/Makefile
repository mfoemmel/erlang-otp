
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk


opt clean debug docs: 

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/etk-$(VSN)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/examples
	tar cf - mines  | (cd $(RELSYSDIR)/examples; tar xf - )
	tar cf - wtour | (cd $(RELSYSDIR)/examples; tar xf - )	
	$(INSTALL_DATA) Makefile README $(RELSYSDIR)/examples

release_docs_spec:

release_src_spec: release_spec
