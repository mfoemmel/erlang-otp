#-*-makefile-*-   ; force emacs to enter makefile-mode

$(EBIN)/snmp_conf.$(EMULATOR): \
	snmp_conf.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_config.$(EMULATOR): \
	snmp_config.erl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_log.$(EMULATOR): \
	snmp_log.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_misc.$(EMULATOR): \
	snmp_misc.erl \
	snmp_verbosity.hrl \
	../compile/snmpc_misc.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_note_store.$(EMULATOR): \
	snmp_note_store.erl \
	../misc/snmp_debug.hrl \
	../misc/snmp_verbosity.hrl

$(EBIN)/snmp_pdu.$(EMULATOR): \
	snmp_pdu.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_usm.$(EMULATOR): \
	snmp_usm.erl \
	snmp_verbosity.hrl \
	../../include/snmp_types.hrl

$(EBIN)/snmp_verbosity.$(EMULATOR): \
	snmp_verbosity.erl


