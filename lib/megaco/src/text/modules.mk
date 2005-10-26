#-*-makefile-*-   ; force emacs to enter makefile-mode

MODULES = \
	megaco_compact_text_encoder \
	megaco_compact_text_encoder_v1 \
	megaco_compact_text_encoder_v2 \
	megaco_compact_text_encoder_prev3a \
	megaco_compact_text_encoder_prev3b \
	megaco_pretty_text_encoder \
	megaco_pretty_text_encoder_v1 \
	megaco_pretty_text_encoder_v2 \
	megaco_pretty_text_encoder_prev3a \
	megaco_pretty_text_encoder_prev3b \
	megaco_text_mini_decoder \
	megaco_text_scanner 


INTERNAL_HRL_FILES = \
	megaco_text_gen_v1.hrl \
	megaco_text_gen_v2.hrl \
	megaco_text_gen_prev3a.hrl \
	megaco_text_gen_prev3b.hrl \
	megaco_text_parser_v1.hrl \
	megaco_text_parser_v2.hrl \
	megaco_text_parser_prev3a.hrl \
	megaco_text_parser_prev3b.hrl \
	megaco_text_mini_parser.hrl \
	megaco_text_tokens.hrl 


INTERNAL_YRL_FILES = \
	megaco_text_parser_v1.yrl \
	megaco_text_parser_v2.yrl \
	megaco_text_parser_prev3a.yrl \
	megaco_text_parser_prev3b.yrl \
	megaco_text_mini_parser.yrl

