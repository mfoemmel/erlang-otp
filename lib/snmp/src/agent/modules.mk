#-*-makefile-*-   ; force emacs to enter makefile-mode

BEHAVIOUR_MODULES = \
	snmpa_authentication_service \
	snmpa_error_report \
	snmpa_network_interface \
	snmpa_network_interface_filter \
	snmpa_notification_filter \
	snmpa_set_mechanism

MODULES = \
	$(BEHAVIOUR_MODULES) \
	snmpa \
	snmpa_acm \
	snmpa_agent \
	snmpa_agent_sup \
	snmpa_app \
	snmpa_conf \
	snmpa_error \
	snmpa_error_io \
	snmpa_error_logger \
	snmpa_general_db \
	snmpa_local_db \
	snmpa_mib \
	snmpa_mib_data \
	snmpa_mib_lib \
	snmpa_misc_sup \
	snmpa_mpd \
	snmpa_net_if \
	snmpa_net_if_filter \
	snmpa_set \
	snmpa_set_lib \
	snmpa_supervisor \
	snmpa_svbl \
	snmpa_symbolic_store \
	snmpa_target_cache \
	snmpa_trap \
	snmpa_usm \
	snmpa_vacm \
	snmp_community_mib \
	snmp_framework_mib \
	snmp_generic \
	snmp_generic_mnesia \
	snmp_index \
	snmp_notification_mib \
	snmp_shadow_table \
	snmp_standard_mib \
	snmp_target_mib \
	snmp_user_based_sm_mib \
	snmp_view_based_acm_mib


INTERNAL_HRL_FILES = \
	snmpa_vacm \
	snmpa_atl  \
	snmpa_internal

EXT_HRL_FILES = 
