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

BER_V1_FLAGS             = +noobj
BER_BIN_V1_FLAGS         = +noobj +asn1config +optimize
BER_BIN_DRV_V1_FLAGS     = +noobj +asn1config +optimize +driver
BER_V2_FLAGS             = +noobj
BER_BIN_V2_FLAGS         = +noobj +asn1config +optimize
BER_BIN_DRV_V2_FLAGS     = +noobj +asn1config +optimize +driver
BER_PREV3A_FLAGS         = +noobj
BER_BIN_PREV3A_FLAGS     = +noobj +asn1config +optimize
BER_BIN_DRV_PREV3A_FLAGS = +noobj +asn1config +optimize +driver
BER_PREV3B_FLAGS         = +noobj
BER_BIN_PREV3B_FLAGS     = +noobj +asn1config +optimize
BER_BIN_DRV_PREV3B_FLAGS = +noobj +asn1config +optimize +driver
BER_PREV3C_FLAGS         = +noobj
BER_BIN_PREV3C_FLAGS     = +noobj +asn1config +optimize
BER_BIN_DRV_PREV3C_FLAGS = +noobj +asn1config +optimize +driver
PER_V1_FLAGS             = +noobj
PER_BIN_V1_FLAGS         = +noobj
PER_BIN_DRV_V1_FLAGS     = +noobj +optimize
PER_V2_FLAGS             = +noobj
PER_BIN_V2_FLAGS         = +noobj
PER_BIN_DRV_V2_FLAGS     = +noobj +optimize
PER_PREV3A_FLAGS         = +noobj
PER_BIN_PREV3A_FLAGS     = +noobj
PER_BIN_DRV_PREV3A_FLAGS = +noobj +optimize
PER_PREV3B_FLAGS         = +noobj
PER_BIN_PREV3B_FLAGS     = +noobj
PER_BIN_DRV_PREV3B_FLAGS = +noobj +optimize
PER_PREV3C_FLAGS         = +noobj
PER_BIN_PREV3C_FLAGS     = +noobj
PER_BIN_DRV_PREV3C_FLAGS = +noobj +optimize

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
	$(BER_BIN_ASN1_V1_SPEC).asn1config \
	$(ASN1_V1_SPEC).asn
	@echo "$(BER_BIN_ASN1_V1_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_V1_FLAGS) $(BER_BIN_ASN1_V1_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_V1_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_V1_SPEC).erl \
	$(BER_BIN_ASN1_V1_SPEC).hrl

$(BER_BIN_DRV_ASN1_V1_SPEC).erl $(BER_BIN_DRV_ASN1_V1_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_V1_SPEC).set.asn \
	$(BER_BIN_DRV_ASN1_V1_SPEC).asn1config \
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
	$(BER_BIN_ASN1_V2_SPEC).asn1config \
	$(ASN1_V2_SPEC).asn
	@echo "$(BER_BIN_ASN1_V2_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_V2_FLAGS) $(BER_BIN_ASN1_V2_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_V2_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_V2_SPEC).erl \
	$(BER_BIN_ASN1_V2_SPEC).hrl

$(BER_BIN_DRV_ASN1_V2_SPEC).erl $(BER_BIN_DRV_ASN1_V2_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_V2_SPEC).set.asn \
	$(BER_BIN_DRV_ASN1_V2_SPEC).asn1config \
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


# --- Version 3 ---

# -- (prev3a) --

$(BER_ASN1_PREV3A_SPEC).erl $(BER_ASN1_PREV3A_SPEC).hrl: \
	$(BER_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(BER_ASN1_PREV3A_SPEC):"
	$(ERLC) -bber $(BER_PREV3A_FLAGS) $(BER_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3A_SPEC).erl \
	$(BER_ASN1_PREV3A_SPEC).hrl

$(BER_BIN_ASN1_PREV3A_SPEC).erl $(BER_BIN_ASN1_PREV3A_SPEC).hrl: \
	$(BER_BIN_ASN1_PREV3A_SPEC).set.asn \
	$(BER_BIN_ASN1_PREV3A_SPEC).asn1config \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(BER_BIN_ASN1_PREV3A_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_PREV3A_FLAGS) $(BER_BIN_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_PREV3A_SPEC).erl \
	$(BER_BIN_ASN1_PREV3A_SPEC).hrl

$(BER_BIN_DRV_ASN1_PREV3A_SPEC).erl $(BER_BIN_DRV_ASN1_PREV3A_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_PREV3A_SPEC).set.asn \
	$(BER_BIN_DRV_ASN1_PREV3A_SPEC).asn1config \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(BER_BIN_DRV_ASN1_PREV3A_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_DRV_PREV3A_FLAGS) $(BER_BIN_DRV_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(BER_BIN_DRV_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(BER_BIN_DRV_ASN1_PREV3A_SPEC).erl \
	$(BER_BIN_DRV_ASN1_PREV3A_SPEC).hrl

$(PER_ASN1_PREV3A_SPEC).erl $(PER_ASN1_PREV3A_SPEC).hrl: \
	$(PER_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(PER_ASN1_PREV3A_SPEC):"
	$(ERLC) -bper $(PER_PREV3A_FLAGS) $(PER_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3A_SPEC).erl \
	$(PER_ASN1_PREV3A_SPEC).hrl

$(PER_BIN_ASN1_PREV3A_SPEC).erl $(PER_BIN_ASN1_PREV3A_SPEC).hrl: \
	$(PER_BIN_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(PER_BIN_ASN1_PREV3A_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_PREV3A_FLAGS) $(PER_BIN_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(PER_BIN_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(PER_BIN_ASN1_PREV3A_SPEC).erl \
	$(PER_BIN_ASN1_PREV3A_SPEC).hrl

$(PER_BIN_DRV_ASN1_PREV3A_SPEC).erl $(PER_BIN_DRV_ASN1_PREV3A_SPEC).hrl: \
	$(PER_BIN_DRV_ASN1_PREV3A_SPEC).set.asn \
	$(ASN1_PREV3A_SPEC).asn
	@echo "$(PER_BIN_DRV_ASN1_PREV3A_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_DRV_PREV3A_FLAGS) $(PER_BIN_DRV_ASN1_PREV3A_SPEC).set.asn

$(EBIN)/$(PER_BIN_DRV_ASN1_PREV3A_SPEC).$(EMULATOR): \
	$(PER_BIN_DRV_ASN1_PREV3A_SPEC).erl \
	$(PER_BIN_DRV_ASN1_PREV3A_SPEC).hrl

# -- (prev3b) --

$(BER_ASN1_PREV3B_SPEC).erl $(BER_ASN1_PREV3B_SPEC).hrl: \
	$(BER_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(BER_ASN1_PREV3B_SPEC):"
	$(ERLC) -bber $(BER_PREV3B_FLAGS) $(BER_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3B_SPEC).erl \
	$(BER_ASN1_PREV3B_SPEC).hrl

$(BER_BIN_ASN1_PREV3B_SPEC).erl $(BER_BIN_ASN1_PREV3B_SPEC).hrl: \
	$(BER_BIN_ASN1_PREV3B_SPEC).set.asn \
	$(BER_BIN_ASN1_PREV3B_SPEC).asn1config \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(BER_BIN_ASN1_PREV3B_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_PREV3B_FLAGS) $(BER_BIN_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_PREV3B_SPEC).erl \
	$(BER_BIN_ASN1_PREV3B_SPEC).hrl

$(BER_BIN_DRV_ASN1_PREV3B_SPEC).erl $(BER_BIN_DRV_ASN1_PREV3B_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_PREV3B_SPEC).set.asn \
	$(BER_BIN_DRV_ASN1_PREV3B_SPEC).asn1config \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(BER_BIN_DRV_ASN1_PREV3B_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_DRV_PREV3B_FLAGS) $(BER_BIN_DRV_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(BER_BIN_DRV_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(BER_BIN_DRV_ASN1_PREV3B_SPEC).erl \
	$(BER_BIN_DRV_ASN1_PREV3B_SPEC).hrl

$(PER_ASN1_PREV3B_SPEC).erl $(PER_ASN1_PREV3B_SPEC).hrl: \
	$(PER_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(PER_ASN1_PREV3B_SPEC):"
	$(ERLC) -bper $(PER_PREV3B_FLAGS) $(PER_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3B_SPEC).erl \
	$(PER_ASN1_PREV3B_SPEC).hrl

$(PER_BIN_ASN1_PREV3B_SPEC).erl $(PER_BIN_ASN1_PREV3B_SPEC).hrl: \
	$(PER_BIN_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(PER_BIN_ASN1_PREV3B_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_PREV3B_FLAGS) $(PER_BIN_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(PER_BIN_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(PER_BIN_ASN1_PREV3B_SPEC).erl \
	$(PER_BIN_ASN1_PREV3B_SPEC).hrl

$(PER_BIN_DRV_ASN1_PREV3B_SPEC).erl $(PER_BIN_DRV_ASN1_PREV3B_SPEC).hrl: \
	$(PER_BIN_DRV_ASN1_PREV3B_SPEC).set.asn \
	$(ASN1_PREV3B_SPEC).asn
	@echo "$(PER_BIN_DRV_ASN1_PREV3B_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_DRV_PREV3B_FLAGS) $(PER_BIN_DRV_ASN1_PREV3B_SPEC).set.asn

$(EBIN)/$(PER_BIN_DRV_ASN1_PREV3B_SPEC).$(EMULATOR): \
	$(PER_BIN_DRV_ASN1_PREV3B_SPEC).erl \
	$(PER_BIN_DRV_ASN1_PREV3B_SPEC).hrl


# -- (prev3c) --

$(BER_ASN1_PREV3C_SPEC).erl $(BER_ASN1_PREV3C_SPEC).hrl: \
	$(BER_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(BER_ASN1_PREV3C_SPEC):"
	$(ERLC) -bber $(BER_PREV3C_FLAGS) $(BER_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(BER_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(BER_ASN1_PREV3C_SPEC).erl \
	$(BER_ASN1_PREV3C_SPEC).hrl

$(BER_BIN_ASN1_PREV3C_SPEC).erl $(BER_BIN_ASN1_PREV3C_SPEC).hrl: \
	$(BER_BIN_ASN1_PREV3C_SPEC).set.asn \
	$(BER_BIN_ASN1_PREV3C_SPEC).asn1config \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(BER_BIN_ASN1_PREV3C_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_PREV3C_FLAGS) $(BER_BIN_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(BER_BIN_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(BER_BIN_ASN1_PREV3C_SPEC).erl \
	$(BER_BIN_ASN1_PREV3C_SPEC).hrl

$(BER_BIN_DRV_ASN1_PREV3C_SPEC).erl $(BER_BIN_DRV_ASN1_PREV3C_SPEC).hrl: \
	$(BER_BIN_DRV_ASN1_PREV3C_SPEC).set.asn \
	$(BER_BIN_DRV_ASN1_PREV3C_SPEC).asn1config \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(BER_BIN_DRV_ASN1_PREV3C_SPEC):"
	$(ERLC) -bber_bin $(BER_BIN_DRV_PREV3C_FLAGS) $(BER_BIN_DRV_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(BER_BIN_DRV_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(BER_BIN_DRV_ASN1_PREV3C_SPEC).erl \
	$(BER_BIN_DRV_ASN1_PREV3C_SPEC).hrl

$(PER_ASN1_PREV3C_SPEC).erl $(PER_ASN1_PREV3C_SPEC).hrl: \
	$(PER_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(PER_ASN1_PREV3C_SPEC):"
	$(ERLC) -bper $(PER_PREV3C_FLAGS) $(PER_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(PER_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(PER_ASN1_PREV3C_SPEC).erl \
	$(PER_ASN1_PREV3C_SPEC).hrl

$(PER_BIN_ASN1_PREV3C_SPEC).erl $(PER_BIN_ASN1_PREV3C_SPEC).hrl: \
	$(PER_BIN_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(PER_BIN_ASN1_PREV3C_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_PREV3C_FLAGS) $(PER_BIN_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(PER_BIN_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(PER_BIN_ASN1_PREV3C_SPEC).erl \
	$(PER_BIN_ASN1_PREV3C_SPEC).hrl

$(PER_BIN_DRV_ASN1_PREV3C_SPEC).erl $(PER_BIN_DRV_ASN1_PREV3C_SPEC).hrl: \
	$(PER_BIN_DRV_ASN1_PREV3C_SPEC).set.asn \
	$(ASN1_PREV3C_SPEC).asn
	@echo "$(PER_BIN_DRV_ASN1_PREV3C_SPEC):"
	$(ERLC) -bper_bin $(PER_BIN_DRV_PREV3C_FLAGS) $(PER_BIN_DRV_ASN1_PREV3C_SPEC).set.asn

$(EBIN)/$(PER_BIN_DRV_ASN1_PREV3C_SPEC).$(EMULATOR): \
	$(PER_BIN_DRV_ASN1_PREV3C_SPEC).erl \
	$(PER_BIN_DRV_ASN1_PREV3C_SPEC).hrl


# -------------

$(EBIN)/megaco_ber_encoder.$(EMULATOR): megaco_ber_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_ber_bin_encoder.$(EMULATOR): megaco_ber_bin_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_per_encoder.$(EMULATOR): megaco_per_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_per_bin_encoder.$(EMULATOR): megaco_per_bin_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_encoder_lib.$(EMULATOR): megaco_binary_encoder_lib.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_encoder.$(EMULATOR): megaco_binary_encoder.erl \
        $(MEGACO_ENGINEDIR)/megaco_message_internal.hrl

$(EBIN)/megaco_binary_name_resolver_v1.$(EMULATOR): \
	megaco_binary_name_resolver_v1.erl

$(EBIN)/megaco_binary_name_resolver_v2.$(EMULATOR): \
	megaco_binary_name_resolver_v2.erl

$(EBIN)/megaco_binary_name_resolver_prev3a.$(EMULATOR): \
	megaco_binary_name_resolver_prev3a.erl

$(EBIN)/megaco_binary_name_resolver_prev3b.$(EMULATOR): \
	megaco_binary_name_resolver_prev3b.erl

$(EBIN)/megaco_binary_name_resolver_prev3c.$(EMULATOR): \
	megaco_binary_name_resolver_prev3c.erl

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

$(EBIN)/megaco_binary_transformer_prev3a.$(EMULATOR): \
	megaco_binary_transformer_prev3a.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3a.hrl

$(EBIN)/megaco_binary_transformer_prev3b.$(EMULATOR): \
	megaco_binary_transformer_prev3b.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3b.hrl

$(EBIN)/megaco_binary_transformer_prev3c.$(EMULATOR): \
	megaco_binary_transformer_prev3c.erl \
        $(MEGACO_INCLUDEDIR)/megaco.hrl \
        $(MEGACO_INCLUDEDIR)/megaco_message_prev3c.hrl

