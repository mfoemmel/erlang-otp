#-*-makefile-*-   ; force emacs to enter makefile-mode

MODULES = \
	megaco \
	megaco_ack_sender \
	megaco_acks_sup \
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
	megaco_user

EXTERNAL_HRL_FILES = \
        ../../include/megaco.hrl \
        ../../include/megaco_message_v1.hrl \
        ../../include/megaco_sdp.hrl

INTERNAL_HRL_FILES = \
	megaco_internal.hrl \
	megaco_message_internal.hrl


