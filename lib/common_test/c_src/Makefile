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

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

PRIV = ../priv
BUILD_RX = $(PRIV)/build_rx_lib
RX = $(PRIV)/rx-1.5

ifneq ($(TARGET),win32)

# ====================================================
# UNIX / LINUX
# ====================================================

SOLIBS = erl_rx_driver.so

CC = gcc
LD = ld

ERLINCL = $(ERL_TOP)/erts/emulator/beam

CFLAGS	= -g -O2 -Wall -D_THREAD_SAFE -I$(ERLINCL) -I$(RX)/rx -fPIC
LDFLAGS =  -G -L$(RX)/rx -lrx -lc

#
# Files
#

OBJ = erl_rx_driver.o

#
# Rules
#

rx:
	$(BUILD_RX) $(PRIV) $(MAKE)

erl_rx_driver.so: rx $(OBJ)
	$(LD) $(OBJ) $(LDFLAGS) -o $@
%.o: %.c
	$(CC) -c $(CFLAGS) $<

debug opt: $(SOLIBS)

docs:

clean:
	rm -f $(SOLIBS)
	rm -f $(OBJ)
	rm -f core

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_PROGRAM) $(SOLIBS) $(PRIV)/lib

release_docs_spec:

else

# ====================================================
# WIN32
# ====================================================

rx:

erl_rx_driver.so: rx 

%.o: %.c

debug opt: 

docs:

clean:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt

release_docs_spec:

endif