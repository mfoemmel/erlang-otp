#-*-makefile-*-   ; force emacs to enter makefile-mode

MODULES = \
	megaco_ber_bin_media_gateway_control \
	megaco_ber_media_gateway_control \
	megaco_per_media_gateway_control \
	megaco_ber_bin_encoder \
	megaco_ber_encoder \
	megaco_binary_encoder \
	megaco_binary_name_resolver \
	megaco_binary_term_id \
	megaco_binary_term_id_gen \
        megaco_binary_transformer \
	megaco_per_encoder

INTERNAL_HRL_FILES = 

ASN1_SPEC         = MEDIA-GATEWAY-CONTROL
BER_ASN1_SPEC     = megaco_ber_media_gateway_control
BER_BIN_ASN1_SPEC = megaco_ber_bin_media_gateway_control
PER_ASN1_SPEC     = megaco_per_media_gateway_control

