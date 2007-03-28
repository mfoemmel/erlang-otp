#-*-makefile-*-   ; force emacs to enter makefile-mode

BEHAVIOUR_MODULES = \
	snmpm_user \
	snmpm_network_interface

MODULES = \
	$(BEHAVIOUR_MODULES) \
	snmpm \
	snmpm_conf \
	snmpm_config \
	snmpm_mpd \
	snmpm_misc_sup \
	snmpm_net_if \
	snmpm_server \
	snmpm_server_sup \
	snmpm_supervisor \
	snmpm_user_default \
	snmpm_usm

INTERNAL_HRL_FILES = \
	snmpm_usm \
	snmpm_atl \
	snmpm_internal

