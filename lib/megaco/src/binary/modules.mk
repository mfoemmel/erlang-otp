#-*-makefile-*-   ; force emacs to enter makefile-mode

MODULES = \
	megaco_binary_encoder \
	megaco_ber_encoder \
	megaco_ber_media_gateway_control_v1 \
	megaco_ber_media_gateway_control_v2 \
	megaco_ber_bin_encoder \
	megaco_ber_bin_media_gateway_control_v1 \
	megaco_ber_bin_media_gateway_control_v2 \
	megaco_ber_bin_drv_media_gateway_control_v1 \
	megaco_ber_bin_drv_media_gateway_control_v2 \
	megaco_per_encoder \
	megaco_per_media_gateway_control_v1 \
	megaco_per_media_gateway_control_v2 \
	megaco_per_bin_encoder \
	megaco_per_bin_media_gateway_control_v1 \
	megaco_per_bin_media_gateway_control_v2 \
	megaco_per_bin_drv_media_gateway_control_v1 \
	megaco_per_bin_drv_media_gateway_control_v2 \
	megaco_binary_name_resolver_v1 \
	megaco_binary_name_resolver_v2 \
	megaco_binary_term_id \
	megaco_binary_term_id_gen \
	megaco_binary_transformer_v1\
	megaco_binary_transformer_v2 

INTERNAL_HRL_FILES = 

ASN1_V1_SPEC      = MEDIA-GATEWAY-CONTROL-v1
ASN1_V2_SPEC      = MEDIA-GATEWAY-CONTROL-v2

BER_ASN1_V1_SPEC         = megaco_ber_media_gateway_control_v1
BER_BIN_ASN1_V1_SPEC     = megaco_ber_bin_media_gateway_control_v1
BER_BIN_DRV_ASN1_V1_SPEC = megaco_ber_bin_drv_media_gateway_control_v1
PER_ASN1_V1_SPEC         = megaco_per_media_gateway_control_v1
PER_BIN_ASN1_V1_SPEC     = megaco_per_bin_media_gateway_control_v1
PER_BIN_DRV_ASN1_V1_SPEC = megaco_per_bin_drv_media_gateway_control_v1

BER_ASN1_V2_SPEC         = megaco_ber_media_gateway_control_v2
BER_BIN_ASN1_V2_SPEC     = megaco_ber_bin_media_gateway_control_v2
BER_BIN_DRV_ASN1_V2_SPEC = megaco_ber_bin_drv_media_gateway_control_v2
PER_ASN1_V2_SPEC         = megaco_per_media_gateway_control_v2
PER_BIN_ASN1_V2_SPEC     = megaco_per_bin_media_gateway_control_v2
PER_BIN_DRV_ASN1_V2_SPEC = megaco_per_bin_drv_media_gateway_control_v2

