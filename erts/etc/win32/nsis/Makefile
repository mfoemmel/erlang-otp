include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
include $(ERL_TOP)/erts/vsn.mk

VERSION_HEADER = erlang.nsh
MAKENSIS = makensis

# This is not the way we usually do in our makefiles, 
# but making release is the ONLY thing we do with this one,
# Its not called during ordinary recursive make.
all: release

opt debug depend:
	@echo Nothing to do for "'"$@"'" on $(TARGET)

clean:
	rm -f $(VERSION_HEADER)

include $(ERL_TOP)/make/otp_release_targets.mk

TARGET_DIR = $(RELEASE_PATH)
WTESTROOT=$(shell (cygpath -d $(RELEASE_PATH) 2>/dev/null || cygpath -w $(RELEASE_PATH)))
WTARGET_DIR=$(shell (cygpath -d $(TARGET_DIR) 2>/dev/null || cygpath -d $(TARGET_DIR)))

release_spec:
	@echo '!define MUI_VERSION "$(SYSTEM_VSN)"' > $(VERSION_HEADER)
	@echo '!define ERTS_VERSION "$(VSN)"' >> $(VERSION_HEADER)
	@echo '!define TESTROOT "$(WTESTROOT)"' >> $(VERSION_HEADER)
	@echo '!define OUTFILEDIR "$(WTARGET_DIR)"' >> $(VERSION_HEADER)
	@if [ -f $(TESTROOT)/docs/index.html ]; then \
		@echo '!define HAVE_DOCS 1' >> $(VERSION_HEADER); \
	fi
	$(MAKENSIS) erlang.nsi	

release_docs release_docs_spec docs:

