#-*-makefile-*-   ; force emacs to enter makefile-mode

$(BER_ASN1_SPEC).erl $(BER_ASN1_SPEC).hrl: $(BER_ASN1_SPEC).set.asn $(ASN1_SPEC).asn
	$(ERLC) -bber +noobj $(BER_ASN1_SPEC).set.asn

$(EBIN)/$(BER_ASN1_SPEC).$(EMULATOR): $(BER_ASN1_SPEC).erl \
	$(BER_ASN1_SPEC).hrl

$(BER_BIN_ASN1_SPEC).erl $(BER_BIN_ASN1_SPEC).hrl: $(BER_BIN_ASN1_SPEC).set.asn $(ASN1_SPEC).asn
	$(ERLC) -bber_bin +noobj $(BER_BIN_ASN1_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_SPEC).$(EMULATOR): $(BER_BIN_ASN1_SPEC).erl \
	$(BER_BIN_ASN1_SPEC).hrl

$(PER_ASN1_SPEC).erl $(PER_ASN1_SPEC).hrl: $(PER_ASN1_SPEC).set.asn $(ASN1_SPEC).asn
	$(ERLC) -bper +noobj $(PER_ASN1_SPEC).set.asn

$(EBIN)/$(PER_ASN1_SPEC).$(EMULATOR): $(PER_ASN1_SPEC).erl \
	$(PER_ASN1_SPEC).hrl

$(EBIN)/megaco_ber_bin_encoder.$(EMULATOR): megaco_ber_bin_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_ber_encoder.$(EMULATOR): megaco_ber_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_per_encoder.$(EMULATOR): megaco_per_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_encoder.$(EMULATOR): megaco_binary_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_name_resolver.$(EMULATOR): megaco_binary_name_resolver.erl

$(EBIN)/megaco_binary_term_id.$(EMULATOR): megaco_binary_term_id.erl

$(EBIN)/megaco_binary_term_id_gen.$(EMULATOR): megaco_binary_term_id_gen.erl

$(EBIN)/megaco_binary_transformer.$(EMULATOR): megaco_binary_transformer.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl
