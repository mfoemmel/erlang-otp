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
OTP_MAKE_ROOT=/home/super/otp/otp_make
include $(OTP_MAKE_ROOT)/otp.mk

#
# Release Macros
#

#
# Common macros
#

SGML_FILES=

FIG_FILES = 

USERS_GUIDE  =

HTML_FILES= GSB.html

GIF_FILES= user_draw.gif user_main.gif user_save_as_softwidget.gif \
	user_draw2.gif user_option.gif

TEX_FILES= $(SGML_FILES:.sgml=.tex) $(USERS_GUIDE:.sgml=.tex)
DVI_FILES= $(USERS_GUIDE:.sgml=.dvi)
PSFIG_FILES= $(FIG_FILES:.fig=.ps) 
PS_FILES= $(USERS_GUIDE:.sgml=.ps)


#
# Make Rules
#
all $(DEFAULT_OPT_TARGETS) $(DEFAULT_DEBUG_TARGETS): $(HTML_FILES) $(TEX_FILES) $(PSFIG_FILES) $(DVI_FILES) $(PS_FILES) 

clean:
	@rm -f *.toc *.log *.aux *.tex sgmls_output sgmls_errs $(TEX_FILES) $(DVI_FILES) $(PSFIG_FILES) $(PS_FILES) 

# 
# Release Targets
#
include $(OTP_MAKE_ROOT)/otp_release_targets.mk

release_variant: opt.$(TARGET)
	$(MKPATH.$(TARGET)) $(RELEASE_PATH)/$(TARGET)/lib/gs-$(VSN)/contribs/gsb/doc
	$(INSTALLFILES.$(TARGET)) $(HTML_FILES) $(GIF_FILES) $(RELEASE_PATH)/$(TARGET)/lib/gs-$(VSN)/contribs/gsb/doc


