#-*-makefile-*-   ; force emacs to enter makefile-mode

PARSER_SRC = snmpc_mib_gram.yrl

PARSER_MODULE = $(PARSER_SRC:%.yrl=%)

MODULES = \
	$(PARSER_MODULE) \
	snmpc \
	snmpc_lib \
	snmpc_mib_to_hrl \
	snmpc_misc \
	snmpc_tok


INTERNAL_HRL_FILES = \
	snmpc.hrl \
	snmpc_misc.hrl
