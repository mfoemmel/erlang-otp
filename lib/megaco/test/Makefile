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

ERL_FILES = $(MODULES:%=%.erl)

TARGET_FILES = $(MODULES:%=%.$(EMULATOR))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ERL_TOP)/lib/megaco/ebin \
	 -pa $(ERL_TOP)/lib/et/ebin

ERL_COMPILE_FLAGS += -pa $(ERL_TOP)/lib/test_server/ebin \
	-I$(ERL_TOP)/lib/test_server/include

ERL_PATH = -pa ../../megaco/examples/simple \
           -pa ../../megaco/ebin \
           -pa ../../et/ebin

ifndef SUITE
SUITE = megaco_SUITE
endif


# ----------------------------------------------------
# Targets
# ----------------------------------------------------

tests debug opt: $(TARGET_FILES)

clean:
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

codec: make
	erl -noshell -sname megaco $(ERL_PATH) \
            -s megaco_test_lib t megaco_codec_test tickets \
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
            -s et_viewer \
            -s megaco_test_lib t megaco_mess_test

# mess: make
# 	erl -noshell -sname megaco $(ERL_PATH) \
#             -s et_viewer \
#             -s megaco_test_lib t megaco_mess_test \
#             -s erlang halt

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


# ----------------------------------------------------
# Release Targets
# ---------------------------------------------------- 

include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: 

release_docs_spec:

release_tests_spec: tests
	$(INSTALL_DIR)  $(RELSYSDIR)
	$(INSTALL_DATA) $(TEST_SPEC_FILE) $(ERL_FILES) $(RELSYSDIR)
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)
	chmod -f -R u+w $(RELSYSDIR)

