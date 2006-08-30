#-*-makefile-*-   ; force emacs to enter makefile-mode

BEHAVIOUR_MODULES = \
	megaco_encoder \
	megaco_transport

MODULES = \
	$(BEHAVIOUR_MODULES) \
	megaco \
	megaco_config \
	megaco_digit_map \
	megaco_erl_dist_encoder \
	megaco_filter \
	megaco_messenger \
	megaco_messenger_misc \
	megaco_misc_sup \
	megaco_monitor \
	megaco_sdp \
	megaco_sup \
	megaco_stats \
	megaco_trans_sender \
	megaco_trans_sup \
	megaco_user_default 

EXTERNAL_HRL_FILES = \
        ../../include/megaco.hrl \
        ../../include/megaco_message_v1.hrl \
        ../../include/megaco_message_v2.hrl \
        ../../include/megaco_message_prev3a.hrl \
        ../../include/megaco_message_prev3b.hrl \
        ../../include/megaco_message_prev3c.hrl \
        ../../include/megaco_sdp.hrl

INTERNAL_HRL_FILES = \
	megaco_internal.hrl \
	megaco_message_internal.hrl


