#-*-makefile-*-   ; force emacs to enter makefile-mode

$(EBIN)/snmpm_user.$(EMULATOR): \
	snmpm_user.erl

$(EBIN)/snmpm_network_interface.$(EMULATOR): \
	snmpm_network_interface.erl

$(EBIN)/snmpm.$(EMULATOR): \
	snmpm.erl

$(EBIN)/snmpm_config.$(EMULATOR): \
	snmpm_config.erl \
	../../include/snmp_types.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpm_mpd.$(EMULATOR): \
	snmpm_mpd.erl \
	../../include/snmp_types.hrl \
	../../include/SNMP-MPD-MIB.hrl \
	../../include/SNMPv2-TM.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmpm_misc_sup.$(EMULATOR): \
	snmpm_misc_sup.erl \
	../misc/snmp_debug.hrl

$(EBIN)/snmpm_net_if.$(EMULATOR): \
	../../include/snmp_types.hrl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl \
	snmpm_net_if.erl \
	snmpm_network_interface.erl

$(EBIN)/snmpm_server.$(EMULATOR): \
	../../include/snmp_types.hrl \
	../../include/STANDARD-MIB.hrl \
	../misc/snmp_verbosity.hrl \
	snmpm_server.erl

$(EBIN)/snmpm_supervisor.$(EMULATOR): \
	snmpm_supervisor.erl

$(EBIN)/snmpm_user_default.$(EMULATOR): \
	snmpm_user_default.erl \
	snmpm_user.erl

$(EBIN)/snmpm_usm.$(EMULATOR): \
	snmpm_usm.erl \
	snmpm_usm.hrl \
	../../include/snmp_types.hrl \
	../../include/SNMP-USER-BASED-SM-MIB.hrl \
	../misc/snmp_verbosity.hrl 


