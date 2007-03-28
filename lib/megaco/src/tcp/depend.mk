#-*-makefile-*-   ; force emacs to enter makefile-mode

$(EBIN)/megaco_tcp.$(EMULATOR): megaco_tcp.erl \
	megaco_tcp.hrl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_accept.$(EMULATOR): megaco_tcp_accept.erl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_connection.$(EMULATOR): megaco_tcp_connection.erl \
	megaco_tcp.hrl \
	../app/megaco_internal.hrl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_sup.$(EMULATOR): megaco_tcp_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_connection_sup.$(EMULATOR): megaco_tcp_connection_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

$(EBIN)/megaco_tcp_accept_sup.$(EMULATOR): megaco_tcp_accept_sup.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl

