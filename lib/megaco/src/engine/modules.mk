#-*-makefile-*-   ; force emacs to enter makefile-mode

MODULES = \
	megaco \
	megaco_config \
	megaco_digit_map \
	megaco_erl_dist_encoder \
	megaco_filter \
	megaco_messenger \
	megaco_misc_sup \
	megaco_monitor \
	megaco_sdp \
	megaco_sup \
	megaco_stats \
	megaco_user

EXTERNAL_HRL_FILES = \
        ../../include/megaco.hrl \
	../../include/megaco_message_v1.hrl

INTERNAL_HRL_FILES = \
	megaco_internal.hrl \
	megaco_message_internal.hrl


