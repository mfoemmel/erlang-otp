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

COVER_SPEC_FILE = megaco.cover


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

ifeq ($(MERL),)
MERL = erl
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
	@echo "ERL  = $(ERL)"
	@echo "MERL = $(MERL)"
	@echo ""

# ----------------------------------------------------
# Special Targets
# ----------------------------------------------------

make: $(TARGET_FILES)

sdp: make
	$(MERL) -noshell -sname megaco_sdp $(ERL_PATH) \
            -s megaco_test_lib t megaco_sdp_test \
            $(EHALT)

config: make
	$(MERL) -noshell -sname megaco_config $(ERL_PATH) \
            -s megaco_test_lib t megaco_config_test \
            $(EHALT)

time1: make
	$(MERL) -noshell -sname megaco_time1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt $(TT_DIR) \
            -s erlang halt

time2: make
	$(MERL) -noshell -sname megaco_time2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt $(TT_DIR) \
            -s erlang halt

timeo1: make
	$(MERL) -noshell -sname megaco_timeo1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_official $(TT_DIR) \
            -s erlang halt

timeo2: make
	$(MERL) -noshell -sname megaco_timeo2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_official $(TT_DIR) \
            -s erlang halt

timet1: make
	$(MERL) -noshell -sname megaco_timet1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_texts $(TT_DIR) \
            -s erlang halt

timet2: make
	$(MERL) -noshell -sname megaco_timet2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_texts $(TT_DIR) \
            -s erlang halt

timeb1: make
	$(MERL) -noshell -sname megaco_timeb1 $(ERL_PATH) \
            -run megaco_codec_v1_test tt_bins $(TT_DIR) \
            -s erlang halt

timeb2: make
	$(MERL) -noshell -sname megaco_timeb2 $(ERL_PATH) \
            -run megaco_codec_v2_test tt_bins $(TT_DIR) \
            -s erlang halt

codec: make
	$(MERL) -noshell -sname megaco_codec $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_test \
            $(EHALT)

codec1: make
	$(MERL) -noshell -sname megaco_codec1 $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_v1_test \
            $(EHALT)

codec1_tickets: make
	$(MERL) -noshell -sname megaco_codec1_tickets $(ERL_PATH) \
            -s megaco_codec_v1_test tickets \
            -s erlang halt

codec2: make
	$(MERL) -noshell -sname megaco_codec2 $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_v2_test \
            $(EHALT)

codec2_tickets: make
	$(MERL) -noshell -sname megaco_codec2_tickets $(ERL_PATH) \
            -s megaco_codec_v2_test tickets \
            -s erlang halt

codec3a: make
	$(MERL) -noshell -sname megaco_codec3a $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_prev3a_test \
            $(EHALT)

codec3a_tickets: make
	$(MERL) -noshell -sname megaco_codec3a_tickets $(ERL_PATH) \
            -s megaco_codec_prev3a_test tickets \
            -s erlang halt

codec3b: make
	$(MERL) -noshell -sname megaco_codec3b $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_prev3b_test \
            $(EHALT)

codec3b_tickets: make
	$(MERL) -noshell -sname megaco_codec3b_tickets $(ERL_PATH) \
            -s megaco_codec_prev3b_test tickets \
            -s erlang halt

codec3c: make
	$(MERL) -noshell -sname megaco_codec3c $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_prev3c_test \
            $(EHALT)

codec3c_tickets: make
	$(MERL) -noshell -sname megaco_codec3c_tickets $(ERL_PATH) \
            -s megaco_codec_prev3c_test tickets \
            -s erlang halt

conf: make
	$(MERL) -noshell -sname megaco_conf $(ERL_PATH) \
            -s megaco_test_lib t megaco_config_test \
            -s erlang halt

dm: make
	$(MERL) -noshell -sname megaco_dm $(ERL_PATH) \
            -s megaco_test_lib t megaco_digit_map_test \
            -s erlang halt

app: make
	$(MERL) -noshell -sname megaco_app $(ERL_PATH) \
            -s megaco_test_lib t megaco_app_test \
            -s erlang halt

appup: make
	$(MERL) -noshell -sname megaco_appup $(ERL_PATH) \
            -s megaco_test_lib t megaco_appup_test \
            -s erlang halt

mess: make
	$(MERL) -noshell -sname megaco_mess $(ERL_PATH) \
            -s et_viewer \
            -s megaco_test_lib t megaco_mess_test \
            -s erlang halt

mib: make
	$(MERL) -noshell -sname megaco_mib $(ERL_PATH) \
            -s megaco_test_lib t megaco_mib_test \
            $(EHALT)

mreq: make
	$(MERL) -noshell -sname megaco_mreq $(ERL_PATH) \
            -s megaco_test_lib t megaco_mreq_test \
            $(EHALT)

pending: make
	$(MERL) -noshell -sname megaco_pending $(ERL_PATH) \
            -s megaco_test_lib t megaco_pending_limit_test \
            $(EHALT)

trans: make
	$(MERL) -noshell -sname megaco_trans $(ERL_PATH) \
            -s megaco_test_lib t megaco_trans_test \
            $(EHALT)

actions: make
	$(MERL) -noshell -sname megaco_actions $(ERL_PATH) \
            -s megaco_test_lib t megaco_actions_test \
            $(EHALT)

pl: make
	$(MERL) -noshell -sname megaco_pl $(ERL_PATH) \
            -s megaco_test_lib t megaco_pending_limit_test \
            $(EHALT)

udp: make
	$(MERL) -noshell -sname megaco_pl $(ERL_PATH) \
            -s megaco_test_lib t megaco_udp_test \
            $(EHALT)

tcp: make
	$(MERL) -noshell -sname megaco_pl $(ERL_PATH) \
            -s megaco_test_lib t megaco_tcp_test \
            $(EHALT)

load: make
	$(MERL) -noshell -sname megaco_load $(ERL_PATH) \
            -s megaco_test_lib t megaco_load_test \
            $(EHALT)

ex: make
	$(MERL) -noshell -sname megaco_ex $(ERL_PATH) \
            -s megaco_test_lib t megaco_examples_test \
            $(EHALT)

# mess: make
# 	$(MERL) -noshell -sname megaco $(ERL_PATH) \
#             -s et_viewer \
#             -s megaco_test_lib t megaco_mess_test \
#             -s erlang halt

tid: make
	$(MERL) -noshell -sname megaco_tid $(ERL_PATH) \
            -s megaco_test_lib t megaco_binary_term_id_test \
            -s erlang halt

tickets: make
	$(MERL) -noshell -sname megaco_tickets $(ERL_PATH) \
            -s megaco_test_lib tickets $(SUITE) \
            -s erlang halt

test: make
	$(MERL) -noshell -sname megaco_test $(ERL_PATH) \
            -s megaco_test_lib t $(SUITE) \
            $(EHALT)

utest: make
	$(MERL) -noshell -sname megaco_utest $(ERL_PATH) \
            -s et_viewer \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

ftest: make
	$(MERL) -noshell -sname megaco_ftest $(ERL_PATH) \
            -s megaco_filter \
            -s megaco_test_lib t $(SUITE) \
            -s erlang halt

gnuplot_gif: make
	$(MERL) -noshell -sname megaco_gnuplot_gif $(ERL_PATH) \
            -s megaco_call_flow_test gnuplot_gif \
            -s erlang halt

display_v1: make
	$(MERL) -noshell -sname megaco_display_text_msgs_v1 $(ERL_PATH) \
            -s megaco_codec_v1_test display_text_messages \
            -s erlang halt

display_v2: make
	$(MERL) -noshell -sname megaco_display_text_msgs_v2 $(ERL_PATH) \
            -s megaco_codec_v2_test display_text_messages \
            -s erlang halt

generate_v2: make
	$(MERL) -noshell -sname megaco_generate_text_msgs_v2 $(ERL_PATH) \
            -s megaco_codec_v2_test generate_text_messages \
            -s erlang halt

display_prev3a: make
	$(MERL) -noshell -sname megaco_display_text_msgs_prev3a $(ERL_PATH) \
            -s megaco_codec_prev3a_test display_text_messages \
            -s erlang halt

display_prev3b: make
	$(MERL) -noshell -sname megaco_display_text_msgs_prev3b $(ERL_PATH) \
            -s megaco_codec_prev3b_test display_text_messages \
            -s erlang halt

generate_prev3b: make
	$(MERL) -noshell -sname megaco_generate_text_msgs_prev3b $(ERL_PATH) \
            -s megaco_codec_prev3b_test generate_text_messages \
            -s erlang halt

display_prev3c: make
	$(MERL) -noshell -sname megaco_display_text_msgs_prev3c $(ERL_PATH) \
            -s megaco_codec_prev3c_test display_text_messages \
            -s erlang halt

generate_prev3c: make
	$(MERL) -noshell -sname megaco_generate_text_msgs_prev3c $(ERL_PATH) \
            -s megaco_codec_prev3c_test generate_text_messages \
            -s erlang halt

node:
	$(MERL) -sname megaco $(ERL_PATH)


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(COVER_SPEC_FILE) \
                        $(HRL_FILES) $(ERL_FILES) \
                        $(RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)

