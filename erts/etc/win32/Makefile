include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

include ../../vsn.mk

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELEASE_PATH= ../../release/$(TARGET)
RELSYSDIR = $(RELEASE_PATH)/erts-$(VSN)
ROOTSYSDIR = $(RELEASE_PATH)

BINDIR = $(ERL_TOP)/bin/$(TARGET)
RUNTIME = $(ERL_TOP)/erts/emulator/beam
SYS = $(ERL_TOP)/erts/emulator/sys/win32
OBJ = $(ERL_TOP)/erts/obj/$(TARGET)
ROOTDIR = $(ERL_TOP)/erts

INSTALL_PROGS = \
	$(BINDIR)/inet_gethost.exe \
	$(BINDIR)/erl.exe \
	$(BINDIR)/werl.exe \
	$(BINDIR)/heart.exe \
	$(BINDIR)/erlc.exe \
#	$(BINDIR)/erl_stub16.exe \
	$(BINDIR)/erlsrv.exe \
	$(BINDIR)/start_erl.exe 

INSTALL_SRC = ./start_erl.c ./Nmakefile.start_erl

INSTALL_LIBS =  $(BINDIR)/erl_dll.lib

INSTALL_README  = \
	README

INSTALL_ICONS = ./beam_icon.ico  ./erl_icon.ico  ./hrl_icon.ico

opt debug all clean depend:
	@echo Nothing to do for "'"$@"'" on $(TARGET)

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec:
	$(INSTALL_DIR) $(RELSYSDIR)/bin
	$(INSTALL_DIR) $(RELSYSDIR)/src
	$(INSTALL_DIR) $(ROOTSYSDIR)/usr/include
	$(INSTALL_DIR) $(ROOTSYSDIR)/usr/lib
	$(INSTALL_DIR) $(ROOTSYSDIR)/usr/lib/icons
	$(INSTALL_PROGRAM) $(INSTALL_PROGS) $(RELSYSDIR)/bin
	$(INSTALL_DATA) $(INSTALL_SRC) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(INSTALL_ICONS) $(ROOTSYSDIR)/usr/lib/icons

release_docs release_docs_spec docs:

