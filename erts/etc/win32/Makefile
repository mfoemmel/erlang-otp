TARGET := target_must_be_set
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

INSTALL_NOTES = \
	$(ROOTDIR)/notes.html \
	$(ROOTDIR)/notes_history.html

INSTALL_PROGS = \
	$(BINDIR)/inet_gethost.exe \
	$(BINDIR)/erl.exe \
	$(BINDIR)/werl.exe \
	$(BINDIR)/heart.exe \
	$(BINDIR)/erlc.exe \
	$(BINDIR)/erl_stub16.exe \
	$(BINDIR)/erlsrv.exe \
	$(BINDIR)/start_erl.exe \
	$(BINDIR)/$(EMULATOR)$(EMULATOR_NUMBER)$(TYPEMARKER).dll

INSTALL_SRC = ./start_erl.c ./Nmakefile.start_erl

INSTALL_WS2 = 	ws2setup.exe

INSTALL_INCLUDE = $(ERL_TOP)/erts/emulator/obsolete/driver.h \
   $(RUNTIME)/erl_driver.h $(SYS)/driver_int.h 
INSTALL_LIBS =  $(BINDIR)/erl_dll.lib

INSTALL_README  = \
	README

opt debug all:
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
	$(INSTALL_PROGRAM) $(INSTALL_PROGS) $(RELSYSDIR)/bin
	$(INSTALL_DATA) $(INSTALL_NOTES) $(RELSYSDIR)
	$(INSTALL_DATA) $(INSTALL_SRC) $(RELSYSDIR)/src
	$(INSTALL_PROGRAM) $(INSTALL_WS2) $(ROOTSYSDIR)
	$(INSTALL_DATA) $(INSTALL_INCLUDE) $(ROOTSYSDIR)/usr/include
	$(INSTALL_DATA) $(INSTALL_LIBS) $(ROOTSYSDIR)/usr/lib

release_docs release_docs_spec docs:

