#-*-makefile-*-   ; force emacs to enter makefile-mode

snmpc_mib_gram.erl: snmpc_mib_gram.yrl

$(EBIN)/snmpc.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc.erl \
	snmpc.hrl

$(EBIN)/snmpc_lib.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_lib.erl \
	snmpc.hrl

$(EBIN)/snmpc_tok.$(EMULATOR): \
	snmpc_tok.erl

$(EBIN)/snmpc_misc.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_misc.erl

$(EBIN)/snmpc_mib_to_hrl.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_mib_to_hrl.erl

$(EBIN)/snmpc_mib_gram.$(EMULATOR): \
	../../include/snmp_types.hrl \
	snmpc_mib_gram.erl

