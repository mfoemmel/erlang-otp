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


# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(MEGACO_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/megaco_test

ifeq ($(findstring win32,$(TARGET)),win32)

MAKEFILE_SRC = Makefile.win32.src

else

MAKEFILE_SRC = Makefile.src

endif

ifeq ($(TT_DIR),)
TT_DIR = /tmp
endif


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

include modules.mk

HRL_FILES = megaco_test_lib.hrl

ERL_FILES = $(MODULES:%=%.erl)

SOURCE = $(ERL_FILES) $(HRL_FILES) 

TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))

SPECS = megaco.spec megaco.spec.vxworks 


EMAKEFILE  = Emakefile
MAKE_EMAKE = $(wildcard $(ERL_TOP)/make/make_emakefile)

ifeq ($(MAKE_EMAKE),)
BUILDTARGET   = $(TARGET_FILES)
RELTEST_FILES = $(SPECS) $(SOURCE) $(TARGET_FILES)
else
BUILDTARGET   = emakebuild
RELTEST_FILES = $(EMAKEFILE) $(SPECS) $(SOURCE)
endif


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

# ifeq ($(USE_HIPE),true)
# ERL_COMPILE_FLAGS += +native
# endif

ERL_FLAGS += \
           -pa $(ERL_TOP)/lib/megaco/ebin \
           -pa $(ERL_TOP)/lib/et/ebin

ERL_COMPILE_FLAGS += \
           -I$(ERL_TOP)/lib/test_server/include

EBIN = .

ERL_PATH = -pa ../../megaco/examples/simple \
           -pa ../../megaco/ebin \
           -pa ../../et/ebin

ifndef SUITE
SUITE = megaco_SUITE
endif

ifeq ($(NO_EHALT),true)
EHALT =
else
EHALT = -s erlang halt
endif

ifeq ($(USE_ET_VIEWER),true)
ET_VIEWER =
else
ET_VIEWER = -s et_viewer
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(BUILDTARGET)

.PHONY: emakebuild

emakebuild: $(EMAKEFILE)

targets: $(EMAKEFILE)
	erl $(ERL_FLAGS) -make

old_targets: $(TARGET_FILES)

$(EMAKEFILE): Makefile
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) '*_SUITE_make' > $(EMAKEFILE)
	$(MAKE_EMAKE) $(ERL_COMPILE_FLAGS) -o$(EBIN) $(MODULES) >> $(EMAKEFILE)

clean:
	rm -f $(EMAKEFILE)
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

make:
	erl $(ERL_PATH) -make

time: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -run megaco_codec_test tt $(TT_DIR) \
            -s erlang halt

timeo: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -run megaco_codec_test tt_official $(TT_DIR) \
            -s erlang halt

timet: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -run megaco_codec_test tt_texts $(TT_DIR) \
            -s erlang halt

timeb: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -run megaco_codec_test tt_bins $(TT_DIR) \
            -s erlang halt

ex: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_examples_test \
            -s erlang halt

codec: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_test \
            -s erlang halt

codec_tickets: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_codec_test tickets \
            -s erlang halt

conf: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_config_test \
            -s erlang halt

app: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_app_test \
            -s erlang halt

appup: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_appup_test \
            -s erlang halt

mess: make
	erl -noshell -sname megaco $(ERL_PATH) \
            $(ET_VIEWER) \
            -s megaco_test_lib t megaco_mess_test \
            $(EHALT)

mib: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_mib_test \
            $(EHALT)

mreq: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_mreq_test \
            $(EHALT)

ack: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_ack_test \
            $(EHALT)

tid: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_binary_term_id_test \
            -s erlang halt

tickets: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib tickets $(SUITE) \
            -s erlang halt

test: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

utest: make
	erl -noshell -sname megaco  $(ERL_PATH) \
            -s et_viewer \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

ftest: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_filter \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

gnuplot_gif: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_call_flow_test gnuplot_gif \
            -s erlang halt

display_text_msgs: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_codec_test display_text_messages \
            -s erlang halt


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(RELTEST_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)

