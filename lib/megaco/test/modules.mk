#-*-makefile-*-   ; force emacs to enter makefile-mode

TEST_SPEC_FILE = megaco.spec

MODULES = \
	megaco_SUITE \
	megaco_app_test \
	megaco_appup_test \
	megaco_actions_test \
	megaco_binary_term_id_test \
	megaco_call_flow_test \
	megaco_codec_test \
	megaco_codec_test_lib \
	megaco_codec_v1_test \
	megaco_codec_v2_test \
	megaco_config_test \
	megaco_examples_test \
	megaco_load_test \
	megaco_mess_test \
	megaco_mess_user_test \
	megaco_mib_test \
	megaco_mreq_test \
	megaco_pending_limit_test \
	megaco_sdp_test \
	megaco_tcp_test \
	megaco_trans_test \
	megaco_udp_test \
	megaco_test_generator \
	megaco_test_mgc \
	megaco_test_mg \
	megaco_test_lib


INTERNAL_HRL_FILES = \
	megaco_test_lib.hrl



