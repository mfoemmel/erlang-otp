#
# Invoke with GNU make or clearmake -C gnu.
#

include $(ERL_TOP)/make/run_make.mk
include modules.mk

conf: 
	cd ../..; $(MAKE) conf

setup:
	(cd ../../priv/lib && \
		rm -f $(STD_DRV).so $(MT_DRV).so && \
		ln -s $(TARGET)/$(STD_DRV).so && \
		ln -s $(TARGET)/$(MT_DRV).so )

info:
	$(MAKE) -f $(TARGET)/Makefile $@
