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

# ----------------------------------------------------
# Target for building only the files needed by the Book generation
# ----------------------------------------------------
texmake: $(TEX_FILES) $(PSFIG_FILES)


# ----------------------------------------------------
# Standard release target
# ----------------------------------------------------

ifeq ($(TESTROOT),)

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(ERL_TOP)/release/$(TARGET) \
		$(TARGET_MAKEFILE)  $@_spec

else

release release_docs release_tests release_html:
	$(MAKE) $(MFLAGS) RELEASE_PATH=$(TESTROOT) $(TARGET_MAKEFILE)  $@_spec

endif
