
ifeq ($(TARGET),)

TARGET := $(shell $(ERL_TOP)/erts/autoconf/config.guess)

else

endif

