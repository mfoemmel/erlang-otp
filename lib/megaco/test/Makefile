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

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(USE_MEGACO_TEST_CODE),true)
ERL_COMPILE_FLAGS += -DMEGACO_TEST_CODE
endif

ifeq ($(USE_MEGACO_HIPE),true)
ERL_COMPILE_FLAGS += +native
endif

ERL_FLAGS += \
           -pa $(ERL_TOP)/lib/megaco/ebin \
           -pa $(ERL_TOP)/lib/et/ebin

ERL_COMPILE_FLAGS += \
           -pa $(ERL_TOP)/lib/test_server/ebin \
           -I$(ERL_TOP)/lib/test_server/include

ERL_PATH = -pa ../../megaco/examples/simple \
           -pa ../../megaco/ebin \
           -pa ../../et/ebin

ifndef SUITE
SUITE = megaco_SUITE
endif

ifeq ($(NO_EHALT),true)
EHALT =
else
EHALT = -s init stop
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES) 
	rm -f errs core *~

docs:

info:
	@echo "ERL_COMPILE_FLAGS = $(ERL_COMPILE_FLAGS)"
	@echo ""

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

make: $(TARGET_FILES)

time1: make
	erl -noshell -sname megaco_time1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt $(TT_DIR) \
            -s erlang halt

time2: make
	erl -noshell -sname megaco_time2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt $(TT_DIR) \
            -s erlang halt

timeo1: make
	erl -noshell -sname megaco_timeo1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_official $(TT_DIR) \
            -s erlang halt

timeo2: make
	erl -noshell -sname megaco_timeo2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_official $(TT_DIR) \
            -s erlang halt

timet1: make
	erl -noshell -sname megaco_timet1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_texts $(TT_DIR) \
            -s erlang halt

timet2: make
	erl -noshell -sname megaco_timet2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_texts $(TT_DIR) \
            -s erlang halt

timeb1: make
	erl -noshell -sname megaco_timeb1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_bins $(TT_DIR) \
            -s erlang halt

timeb2: make
	erl -noshell -sname megaco_timeb2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_bins $(TT_DIR) \
            -s erlang halt

codec: make
	erl -noshell -sname megaco_codec $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_test \
            $(EHALT)

codec1: make
	erl -noshell -sname megaco_codec1 $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_v1_test \
            $(EHALT)

codec1_tickets: make
	erl -noshell -sname megaco_codec1_tickets $(ERL_PATH) \
            -s megaco_codec_v1_test tickets \
            -s erlang halt

codec2: make
	erl -noshell -sname megaco_codec2 $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_v2_test \
            $(EHALT)

codec2_tickets: make
	erl -noshell -sname megaco_codec2_tickets $(ERL_PATH) \
            -s megaco_codec_v2_test tickets \
            -s erlang halt

conf: make
	erl -noshell -sname megaco_conf $(ERL_PATH) \
            -s megaco_test_lib t megaco_config_test \
            -s erlang halt

app: make
	erl -noshell -sname megaco_app $(ERL_PATH) \
            -s megaco_test_lib t megaco_app_test \
            -s erlang halt

appup: make
	erl -noshell -sname megaco_appup $(ERL_PATH) \
            -s megaco_test_lib t megaco_appup_test \
            -s erlang halt

mess: make
	erl -noshell -sname megaco_mess $(ERL_PATH) \
            -s et_viewer \
            -s megaco_test_lib t megaco_mess_test \
            -s erlang halt

mib: make
	erl -noshell -sname megaco_mib $(ERL_PATH) \
            -s megaco_test_lib t megaco_mib_test \
            $(EHALT)

mreq: make
	erl -noshell -sname megaco_mreq $(ERL_PATH) \
            -s megaco_test_lib t megaco_mreq_test \
            $(EHALT)

pending: make
	erl -noshell -sname megaco_pending $(ERL_PATH) \
            -s megaco_test_lib t megaco_pending_limit_test \
            $(EHALT)

trans: make
	erl -noshell -sname megaco_trans $(ERL_PATH) \
            -s megaco_test_lib t megaco_trans_test \
            $(EHALT)

actions: make
	erl -noshell -sname megaco_actions $(ERL_PATH) \
            -s megaco_test_lib t megaco_actions_test \
            $(EHALT)

pl: make
	erl -noshell -sname megaco_pl $(ERL_PATH) \
            -s megaco_test_lib t megaco_pending_limit_test \
            $(EHALT)

load: make
	erl -noshell -sname megaco_load $(ERL_PATH) \
            -s megaco_test_lib t megaco_load_test \
            $(EHALT)

ex: make
	erl -noshell -sname megaco_ex $(ERL_PATH) \
            -s megaco_test_lib t megaco_examples_test \
            $(EHALT)

# mess: make
# 	erl -noshell -sname megaco $(ERL_PATH) \
#             -s et_viewer \
#             -s megaco_test_lib t megaco_mess_test \
#             -s erlang halt

tid: make
	erl -noshell -sname megaco_tid $(ERL_PATH) \
            -s megaco_test_lib t megaco_binary_term_id_test \
            -s erlang halt

tickets: make
	erl -noshell -sname megaco_tickets $(ERL_PATH) \
            -s megaco_test_lib tickets $(SUITE) \
            -s erlang halt

test: make
	erl -noshell -sname megaco_test $(ERL_PATH) \
            -s megaco_test_lib t $(SUITE) \
            $(EHALT)

utest: make
	erl -noshell -sname megaco_utest $(ERL_PATH) \
            -s et_viewer \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

ftest: make
	erl -noshell -sname megaco_ftest $(ERL_PATH) \
            -s megaco_filter \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

gnuplot_gif: make
	erl -noshell -sname megaco_gnuplot_gif $(ERL_PATH) \
            -s megaco_call_flow_test gnuplot_gif \
            -s erlang halt

display_text_msgs1: make
	erl -noshell -sname megaco_display_text_msgs1 $(ERL_PATH) \
            -s megaco_codec_v1_test display_text_messages \
            -s erlang halt

display_text_msgs2: make
	erl -noshell -sname megaco_display_text_msgs2 $(ERL_PATH) \
            -s megaco_codec_v2_test display_text_messages \
            -s erlang halt

node:
	erl -sname megaco $(ERL_PATH)


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(HRL_FILES) $(ERL_FILES) \
                        $(RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)

