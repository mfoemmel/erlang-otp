#
# Invoke with GNU make or clearmake -C gnu.
#

# FIXME let configure put in this last part TARGET

include $(ERL_TOP)/make/target.mk

debug opt shared purify quantify purecov:
	$(MAKE) -f $(TARGET)/Makefile TYPE=$@

clean depend docs release release_docs tests release_tests check:
	$(MAKE) -f $(TARGET)/Makefile $@
