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
SHELL=/bin/sh

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

#
# Macros
#

SUB_DIRECTORIES = main regalloc util rtl icode sparc flow opt misc app \
		  x86

#
# Default Subdir Targets
#
include $(ERL_TOP)/make/otp_subdir.mk


all-subdirs: 
	-for dir in $(SUB_DIRECTORIES ); do \
		(cd $$dir; $(MAKE) $(MAKETARGET) EBIN=../ebin; cd ..); \
	done

distclean:
	$(MAKE) MAKETARGET="distclean" all-subdirs
realclean:
	$(MAKE) MAKETARGET="realclean" all-subdirs
