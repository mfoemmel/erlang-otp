# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#
#
# Running this Makefile without arguments to automatically create
# the Makefiles for your specific operating system.  The results will
# be saved in a platform dependent sub-directory, so you can safely
# configure for several platforms in the same clearcase view.
#
# Run 'clearmake makefiles' if you just want to rebuild the Makefiles,
# without determining the configuration parameters.  This is useful if
# you've changed a Makefile.in file.
#
TARGET := $(shell './config.guess')

ifeq ($(findstring vxworks,$(TARGET)),vxworks)
do_vxworks:
	HOST_TYPE=$(HOST_TYPE) WIND_BASE=$(WIND_BASE) ./configure.vxworks $(TARGET)
else

ifneq (,$(findstring linux,$(TARGET)))
CONFIG_FLAGS = --enable-elib-malloc
endif

ifneq (,$(findstring univel-sysv4,$(TARGET)))
CONFIG_FLAGS = --enable-elib-malloc --x-libraries=/usr/lib/X11
endif

ifeq ($(TARGET),free_source)
CONFIG_FLAGS += --host=$(TARGET)
endif

CONFIG_FLAGS += --cache-file $(ERL_TOP)/erts/autoconf/$(TARGET)/config.cache 
CONFIG_FLAGS += --srcdir=$(ERL_TOP)/erts

do_configure: config.h.in configure
	@if test ! -d $(TARGET); then mkdir $(TARGET); fi
	@if test x$(ERL_TOP) = x || test ! -d $(ERL_TOP) ; then \
	  echo "You need to set the environment variable ERL_TOP!" && false ; \
	 fi
	ERL_TOP=$(ERL_TOP) \
	  CONFIG_STATUS=$(ERL_TOP)/erts/autoconf/$(TARGET)/config.status \
	  CFLAGS="" ./configure $(CONFIG_FLAGS)

clean:
	rm -f config.log configure config.h.in
	rm -rf $(TARGET)

makefiles:
	$(TARGET)/config.status

configure: configure.in aclocal.m4
	autoconf

config.h.in: acconfig.h configure.in aclocal.m4
	@if test x$(ERL_TOP) = x || test ! -d $(ERL_TOP) ; then \
	  echo "You need to set the environment variable ERL_TOP!" && false ; \
	 fi
	autoheader --localdir=$(ERL_TOP)/erts/autoconf configure.in \
	  > config.h.in

$(TARGET)/config.h: do_configure

win32:
	cd $(ERL_TOP) && $(MAKE) do_configure
	EMULATOR=beam ./configure.win32 $(TARGET)

.PHONY: win32
endif
