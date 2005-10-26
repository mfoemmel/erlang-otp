include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(GS_VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

MODULES= \
	ssh_test_lib \
	ssh_SUITE \
	ssh_cm_SUITE \
	ssh_file_SUITE \
	ssh_sftp_SUITE \
	ssh_ssh_SUITE \
	ssh_sshd_SUITE \
	ssh_transport_SUITE \
	ssh_userauth_SUITE

ERL_FILES= $(MODULES:%=%.erl)

TARGET_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR))
INSTALL_PROGS= $(TARGET_FILES)

EMAKEFILE=Emakefile

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/ssh_test

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -I$(ERL_TOP)/lib/test_server/include \
		-I$(ERL_TOP)/lib/ssh/src
EBIN = .

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

make_emakefile:
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make'\
	> $(EMAKEFILE)
	$(ERL_TOP)/make/make_emakefile $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES)\
	>> $(EMAKEFILE)

tests debug opt: make_emakefile
	erl $(ERL_FLAGS) -make

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES)
	rm -f core
docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_tests_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)
	$(INSTALL_DATA)  $(ERL_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) ssh.spec $(INSTALL_PROGS) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)
	@tar cf - *_SUITE_data | (cd $(RELSYSDIR); tar xf -)

release_docs_spec:

