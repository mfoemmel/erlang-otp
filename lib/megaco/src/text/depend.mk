#-*-makefile-*-   ; force emacs to enter makefile-mode

megaco_text_parser.erl: megaco_text_parser.yrl megaco_text_parser.hrl

$(EBIN)/megaco_compact_text_encoder.$(EMULATOR): megaco_compact_text_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl megaco_text_gen.hrl

$(EBIN)/megaco_pretty_text_encoder.$(EMULATOR): megaco_pretty_text_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl megaco_text_gen.hrl

$(EBIN)/megaco_text_parser.$(EMULATOR): megaco_text_parser.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl megaco_text_parser.hrl

$(EBIN)/megaco_text_scanner.$(EMULATOR): megaco_text_scanner.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	../engine/megaco_message_internal.hrl \
	megaco_text_tokens.hrl
