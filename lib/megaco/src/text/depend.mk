#-*-makefile-*-   ; force emacs to enter makefile-mode

megaco_text_parser_v1.erl: \
	megaco_text_parser_v1.yrl \
	megaco_text_parser_v1.hrl
megaco_text_parser_v2.erl: \
	megaco_text_parser_v2.yrl \
	megaco_text_parser_v2.hrl
megaco_text_parser_prev3a.erl: \
	megaco_text_parser_prev3a.yrl \
	megaco_text_parser_prev3a.hrl

megaco_text_mini_parser.erl: \
	megaco_text_mini_parser.yrl \
	megaco_text_mini_parser.hrl

$(EBIN)/megaco_compact_text_encoder.$(EMULATOR): \
	megaco_compact_text_encoder.erl

$(EBIN)/megaco_compact_text_encoder.$(EMULATOR): \
	megaco_compact_text_encoder.erl

$(EBIN)/megaco_compact_text_encoder_v1.$(EMULATOR): \
	megaco_compact_text_encoder_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v1.hrl

$(EBIN)/megaco_compact_text_encoder_v2.$(EMULATOR): \
	megaco_compact_text_encoder_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v2.hrl

$(EBIN)/megaco_compact_text_encoder_prev3a.$(EMULATOR): \
	megaco_compact_text_encoder_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3a.hrl

$(EBIN)/megaco_pretty_text_encoder.$(EMULATOR): \
	megaco_pretty_text_encoder.erl

$(EBIN)/megaco_pretty_text_encoder_v1.$(EMULATOR): \
	megaco_pretty_text_encoder_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v1.hrl

$(EBIN)/megaco_pretty_text_encoder_v2.$(EMULATOR): \
	megaco_pretty_text_encoder_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_v2.hrl

$(EBIN)/megaco_pretty_text_encoder_prev3a.$(EMULATOR): \
	megaco_pretty_text_encoder_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_gen_prev3a.hrl

$(EBIN)/megaco_text_parser_v1.$(EMULATOR): \
	megaco_text_parser_v1.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_v1.hrl

$(EBIN)/megaco_text_parser_v2.$(EMULATOR): \
	megaco_text_parser_v2.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_v2.hrl

$(EBIN)/megaco_text_parser_prev3a.$(EMULATOR): \
	megaco_text_parser_prev3a.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	$(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl \
	megaco_text_tokens.hrl \
	megaco_text_parser_prev3a.hrl

$(EBIN)/megaco_text_scanner.$(EMULATOR): megaco_text_scanner.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
	../engine/megaco_message_internal.hrl \
	megaco_text_tokens.hrl

$(EBIN)/megaco_text_mini_decoder.$(EMULATOR): \
	megaco_text_mini_decoder.erl \
	../engine/megaco_message_internal.hrl \
	megaco_text_tokens.hrl

