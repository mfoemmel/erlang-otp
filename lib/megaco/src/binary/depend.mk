#-*-makefile-*-   ; force emacs to enter makefile-mode

# Flag description:
# 
# +optimize
# For ber_bin this means "optimize" (whatever that is),
# but for per_bin it means that a stage in the encode
# is done in the asn1 driver
#
# +driver
# For ber_bin this means that part of the decode is done
# in the asn1 driver
#
# +asn1config
# This is only used by the ber_bin, and means that 
# some partial decode functions will be created 
# (as described by the asn1config file)
#

BER_V1_FLAGS         = +noobj
BER_BIN_V1_FLAGS     = +noobj +asn1config +optimize
BER_BIN_DRV_V1_FLAGS = +noobj +asn1config +optimize +driver
BER_V2_FLAGS         = +noobj
BER_BIN_V2_FLAGS     = +noobj +optimize
BER_BIN_DRV_V2_FLAGS = +noobj +optimize +driver
PER_V1_FLAGS         = +noobj
PER_BIN_V1_FLAGS     = +noobj
PER_BIN_DRV_V1_FLAGS = +noobj +optimize
PER_V2_FLAGS         = +noobj
PER_BIN_V2_FLAGS     = +noobj
PER_BIN_DRV_V2_FLAGS = +noobj +optimize

# --- Version 1 ---

$(BER_ASN1_V1_SPEC).erl $(BER_ASN1_V1_SPEC).hrl: \
	$(BER_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(BER_ASN1_V1_SPEC):"
	$(ERLC) -bber $(BER_V1_FLAGS) $(BER_ASN1_V1_SPEC).set.asn

$(EBIN)/$(BER_ASN1_V1_SPEC).$(EMULATOR): \
	$(BER_ASN1_V1_SPEC).erl \
	$(BER_ASN1_V1_SPEC).hrl

$(BER_BIN_ASN1_V1_SPEC).erl $(BER_BIN_ASN1_V1_SPEC).hrl: \
	$(BER_BIN_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(BER_BIN_ASN1_V1_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_V1_FLAGS) $(BER_BIN_ASN1_V1_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_V1_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_V1_SPEC).erl \
	$(BER_BIN_ASN1_V1_SPEC).hrl

$(BER_BIN_DRV_ASN1_V1_SPEC).erl $(BER_BIN_DRV_ASN1_V1_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(BER_BIN_DRV_ASN1_V1_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_DRV_V1_FLAGS) $(BER_BIN_DRV_ASN1_V1_SPEC).set.asn

$(EBIN)/$(BER_BIN_DRV_ASN1_V1_SPEC).$(EMULATOR): \
	$(BER_BIN_DRV_ASN1_V1_SPEC).erl \
	$(BER_BIN_DRV_ASN1_V1_SPEC).hrl

$(PER_ASN1_V1_SPEC).erl $(PER_ASN1_V1_SPEC).hrl: \
	$(PER_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(PER_ASN1_V1_SPEC):"
	$(ERLC) -bper $(PER_V1_FLAGS) $(PER_ASN1_V1_SPEC).set.asn

$(EBIN)/$(PER_ASN1_V1_SPEC).$(EMULATOR): \
	$(PER_ASN1_V1_SPEC).erl \
	$(PER_ASN1_V1_SPEC).hrl

$(PER_BIN_ASN1_V1_SPEC).erl $(PER_BIN_ASN1_V1_SPEC).hrl: \
	$(PER_BIN_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(PER_BIN_ASN1_V1_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_V1_FLAGS) $(PER_BIN_ASN1_V1_SPEC).set.asn

$(EBIN)/$(PER_BIN_ASN1_V1_SPEC).$(EMULATOR): \
	$(PER_BIN_ASN1_V1_SPEC).erl \
	$(PER_BIN_ASN1_V1_SPEC).hrl

$(PER_BIN_DRV_ASN1_V1_SPEC).erl $(PER_BIN_DRV_ASN1_V1_SPEC).hrl: \
	$(PER_BIN_DRV_ASN1_V1_SPEC).set.asn \
	$(ASN1_V1_SPEC).asn
	@echo "$(PER_BIN_DRV_ASN1_V1_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_DRV_V1_FLAGS) $(PER_BIN_DRV_ASN1_V1_SPEC).set.asn

$(EBIN)/$(PER_BIN_DRV_ASN1_V1_SPEC).$(EMULATOR): \
	$(PER_BIN_DRV_ASN1_V1_SPEC).erl \
	$(PER_BIN_DRV_ASN1_V1_SPEC).hrl

# --- Version 2 ---

$(BER_ASN1_V2_SPEC).erl $(BER_ASN1_V2_SPEC).hrl: \
	$(BER_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(BER_ASN1_V2_SPEC):"
	$(ERLC) -bber $(BER_V2_FLAGS) $(BER_ASN1_V2_SPEC).set.asn

$(EBIN)/$(BER_ASN1_V2_SPEC).$(EMULATOR): \
	$(BER_ASN1_V2_SPEC).erl \
	$(BER_ASN1_V2_SPEC).hrl

$(BER_BIN_ASN1_V2_SPEC).erl $(BER_BIN_ASN1_V2_SPEC).hrl: \
	$(BER_BIN_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(BER_BIN_ASN1_V2_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_V2_FLAGS) $(BER_BIN_ASN1_V2_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_V2_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_V2_SPEC).erl \
	$(BER_BIN_ASN1_V2_SPEC).hrl

$(BER_BIN_DRV_ASN1_V2_SPEC).erl $(BER_BIN_DRV_ASN1_V2_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(BER_BIN_DRV_ASN1_V2_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_DRV_V2_FLAGS) $(BER_BIN_DRV_ASN1_V2_SPEC).set.asn

$(EBIN)/$(BER_BIN_DRV_ASN1_V2_SPEC).$(EMULATOR): \
	$(BER_BIN_DRV_ASN1_V2_SPEC).erl \
	$(BER_BIN_DRV_ASN1_V2_SPEC).hrl

$(PER_ASN1_V2_SPEC).erl $(PER_ASN1_V2_SPEC).hrl: \
	$(PER_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(PER_ASN1_V2_SPEC):"
	$(ERLC) -bper $(PER_V2_FLAGS) $(PER_ASN1_V2_SPEC).set.asn

$(EBIN)/$(PER_ASN1_V2_SPEC).$(EMULATOR): \
	$(PER_ASN1_V2_SPEC).erl \
	$(PER_ASN1_V2_SPEC).hrl

$(PER_BIN_ASN1_V2_SPEC).erl $(PER_BIN_ASN1_V2_SPEC).hrl: \
	$(PER_BIN_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(PER_BIN_ASN1_V2_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_V2_FLAGS) $(PER_BIN_ASN1_V2_SPEC).set.asn

$(EBIN)/$(PER_BIN_ASN1_V2_SPEC).$(EMULATOR): \
	$(PER_BIN_ASN1_V2_SPEC).erl \
	$(PER_BIN_ASN1_V2_SPEC).hrl

$(PER_BIN_DRV_ASN1_V2_SPEC).erl $(PER_BIN_DRV_ASN1_V2_SPEC).hrl: \
	$(PER_BIN_DRV_ASN1_V2_SPEC).set.asn \
	$(ASN1_V2_SPEC).asn
	@echo "$(PER_BIN_DRV_ASN1_V2_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_DRV_V2_FLAGS) $(PER_BIN_DRV_ASN1_V2_SPEC).set.asn

$(EBIN)/$(PER_BIN_DRV_ASN1_V2_SPEC).$(EMULATOR): \
	$(PER_BIN_DRV_ASN1_V2_SPEC).erl \
	$(PER_BIN_DRV_ASN1_V2_SPEC).hrl


# --

$(EBIN)/megaco_ber_encoder.$(EMULATOR): megaco_ber_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_ber_bin_encoder.$(EMULATOR): megaco_ber_bin_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

# $(EBIN)/megaco_ber_bin_drv_encoder.$(EMULATOR): \
# 	megaco_ber_bin_drv_encoder.erl \
# 	$(MEGACO_INCLUDEDIR)/megaco.hrl \
#         $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_per_encoder.$(EMULATOR): megaco_per_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_per_bin_encoder.$(EMULATOR): megaco_per_bin_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

# $(EBIN)/megaco_per_bin_drv_encoder.$(EMULATOR): \
#	megaco_per_bin_drv_encoder.erl \
#	$(MEGACO_INCLUDEDIR)/megaco.hrl \
#	$(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_encoder.$(EMULATOR): megaco_binary_encoder.erl \
	$(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_name_resolver_v1.$(EMULATOR): \
	megaco_binary_name_resolver_v1.erl

$(EBIN)/megaco_binary_name_resolver_v2.$(EMULATOR): \
	megaco_binary_name_resolver_v2.erl

$(EBIN)/megaco_binary_term_id.$(EMULATOR): megaco_binary_term_id.erl

$(EBIN)/megaco_binary_term_id_gen.$(EMULATOR): megaco_binary_term_id_gen.erl

$(EBIN)/megaco_binary_transformer_v1.$(EMULATOR): \
	megaco_binary_transformer_v1.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v1.hrl

$(EBIN)/megaco_binary_transformer_v2.$(EMULATOR): \
	megaco_binary_transformer_v2.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_v2.hrl

