#-*-makefile-*-   ; force emacs to enter makefile-mode

BEHAVIOUR_MODULES = \
	megaco_encoder \
	megaco_transport

MODULES = \
	$(BEHAVIOUR_MODULES) \
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

INTERNAL_HRL_FILES = \
	megaco_message_internal.hrl


