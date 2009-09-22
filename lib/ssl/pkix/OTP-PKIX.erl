%% =====================================================================
%% This module was formed by merging the following modules:
%% 
%% 		`'OTP-PKIX''
%% 		`asn1rt'
%% 		`asn1rt_ber_bin_v2'
%% 		`asn1rt_check'
%% 		`asn1rt_driver_handler'
%% 
%% Created by Igor 2009-09-21, 12:53:28.
%% 

-module('OTP-PKIX').

-export(['ansi-X9-62'/0, anyExtendedKeyUsage/0,
	 anyPolicy/0, 'c-TwoCurve'/0, c2onb191v4/0, c2onb191v5/0,
	 c2onb239v4/0, c2onb239v5/0, c2pnb163v1/0, c2pnb163v2/0,
	 c2pnb163v3/0, c2pnb176w1/0, c2pnb208w1/0, c2pnb272w1/0,
	 c2pnb304w1/0, c2pnb368w1/0, c2tnb191v1/0, c2tnb191v2/0,
	 c2tnb191v3/0, c2tnb239v1/0, c2tnb239v2/0, c2tnb239v3/0,
	 c2tnb359v1/0, c2tnb431r1/0,
	 'characteristic-two-field'/0, 'common-name'/0,
	 dec_AAControls/2, dec_ACClearAttrs/2,
	 dec_AccessDescription/2, dec_AdministrationDomainName/2,
	 dec_AlgorithmIdentifier/2, dec_AnotherName/2, dec_Any/2,
	 dec_AttCertIssuer/2, dec_AttCertValidityPeriod/2,
	 dec_AttCertVersion/2, dec_AttrSpec/2, dec_Attribute/2,
	 dec_AttributeCertificate/2,
	 dec_AttributeCertificateInfo/2, dec_AttributeType/2,
	 dec_AttributeTypeAndValue/2, dec_AttributeValue/2,
	 dec_AuthorityInfoAccessSyntax/2,
	 dec_AuthorityKeyIdentifier/2, dec_BaseCRLNumber/2,
	 dec_BaseDistance/2, dec_BasicConstraints/2,
	 dec_Boolean/2, dec_BuiltInDomainDefinedAttribute/2,
	 dec_BuiltInDomainDefinedAttributes/2,
	 dec_BuiltInStandardAttributes/2, dec_CPSuri/2,
	 dec_CRLDistributionPoints/2, dec_CRLNumber/2,
	 dec_CRLReason/2, dec_CertPolicyId/2, dec_Certificate/2,
	 dec_CertificateIssuer/2, dec_CertificateList/2,
	 dec_CertificatePolicies/2,
	 dec_CertificateSerialNumber/2,
	 'dec_Characteristic-two'/2, dec_ClassList/2,
	 dec_Clearance/2, dec_CommonName/2, dec_CountryName/2,
	 dec_Curve/2, dec_DHPublicKey/2, dec_DSAPublicKey/2,
	 dec_DigestAlgorithmIdentifier/2, dec_DigestInfo/2,
	 dec_DirectoryString/2, dec_DisplayText/2,
	 dec_DistinguishedName/2, dec_DistributionPoint/2,
	 dec_DistributionPointName/2, dec_DomainComponent/2,
	 dec_DomainParameters/2, 'dec_Dss-Parms'/2,
	 'dec_Dss-Sig-Value'/2, 'dec_ECDSA-Sig-Value'/2,
	 dec_ECPVer/2, dec_ECParameters/2, dec_ECPoint/2,
	 dec_EDIPartyName/2, dec_EcpkParameters/2,
	 dec_EmailAddress/2, dec_ExtKeyUsageSyntax/2,
	 dec_ExtendedNetworkAddress/2, dec_Extension/2,
	 'dec_Extension-Any'/2, dec_ExtensionAttribute/2,
	 dec_ExtensionAttributes/2,
	 dec_ExtensionORAddressComponents/2,
	 dec_ExtensionPhysicalDeliveryAddressComponents/2,
	 dec_Extensions/2, dec_FieldElement/2, dec_FieldID/2,
	 dec_FreshestCRL/2, dec_GeneralName/2,
	 dec_GeneralNames/2, dec_GeneralSubtree/2,
	 dec_GeneralSubtrees/2, dec_HoldInstructionCode/2,
	 dec_Holder/2, dec_IetfAttrSyntax/2,
	 dec_InhibitAnyPolicy/2, dec_InvalidityDate/2,
	 dec_IssuerAltName/2, dec_IssuerSerial/2,
	 dec_IssuingDistributionPoint/2, 'dec_KEA-Parms-Id'/2,
	 'dec_KEA-PublicKey'/2, dec_KeyIdentifier/2,
	 dec_KeyPurposeId/2, dec_KeyUsage/2,
	 dec_LocalPostalAttributes/2, dec_Name/2,
	 dec_NameConstraints/2, dec_NetworkAddress/2,
	 dec_NoticeReference/2, dec_NumericUserIdentifier/2,
	 dec_ORAddress/2, dec_ObjId/2, dec_ObjectDigestInfo/2,
	 dec_OrganizationName/2, dec_OrganizationalUnitName/2,
	 dec_OrganizationalUnitNames/2, dec_OtherPrimeInfo/2,
	 dec_OtherPrimeInfos/2, dec_PDSName/2,
	 dec_PDSParameter/2, dec_Pentanomial/2,
	 dec_PersonalName/2, dec_PhysicalDeliveryCountryName/2,
	 dec_PhysicalDeliveryOfficeName/2,
	 dec_PhysicalDeliveryOfficeNumber/2,
	 dec_PhysicalDeliveryOrganizationName/2,
	 dec_PhysicalDeliveryPersonalName/2,
	 dec_PolicyConstraints/2, dec_PolicyInformation/2,
	 dec_PolicyMappings/2, dec_PolicyQualifierId/2,
	 dec_PolicyQualifierInfo/2, dec_PostOfficeBoxAddress/2,
	 dec_PostalCode/2, dec_PosteRestanteAddress/2,
	 dec_PresentationAddress/2, 'dec_Prime-p'/2,
	 dec_PrivateDomainName/2, dec_PrivateKeyUsagePeriod/2,
	 dec_ProxyInfo/2, dec_PublicKeyAlgorithm/2,
	 dec_RDNSequence/2, dec_RSAPrivateKey/2,
	 dec_RSAPublicKey/2, dec_ReasonFlags/2,
	 dec_RelativeDistinguishedName/2, dec_RoleSyntax/2,
	 dec_SSLAttributeTypeAndValue/2, dec_SSLCertificate/2,
	 'dec_SSLCharacteristic-two'/2, dec_SSLExtension/2,
	 dec_SSLExtensionAttribute/2,
	 dec_SSLExtensionAttributes/2, dec_SSLExtensions/2,
	 dec_SSLFieldID/2, dec_SSLSubjectPublicKeyInfo/2,
	 'dec_SSLSubjectPublicKeyInfo-Any'/2,
	 dec_SSLTBSCertificate/2, dec_SecurityCategory/2,
	 dec_SignatureAlgorithm/2,
	 'dec_SignatureAlgorithm-Any'/2, dec_SkipCerts/2,
	 dec_StreetAddress/2, dec_SubjectAltName/2,
	 dec_SubjectDirectoryAttributes/2,
	 dec_SubjectInfoAccessSyntax/2,
	 dec_SubjectKeyIdentifier/2, dec_SubjectPublicKeyInfo/2,
	 dec_SvceAuthInfo/2, dec_TBSCertList/2,
	 dec_TBSCertificate/2, dec_Target/2, dec_TargetCert/2,
	 dec_Targets/2, dec_TeletexCommonName/2,
	 dec_TeletexDomainDefinedAttribute/2,
	 dec_TeletexDomainDefinedAttributes/2,
	 dec_TeletexOrganizationName/2,
	 dec_TeletexOrganizationalUnitName/2,
	 dec_TeletexOrganizationalUnitNames/2,
	 dec_TeletexPersonalName/2, dec_TerminalIdentifier/2,
	 dec_TerminalType/2, dec_Time/2, dec_Trinomial/2,
	 dec_UnformattedPostalAddress/2, dec_UniqueIdentifier/2,
	 dec_UniquePostalName/2, dec_UserNotice/2, dec_V2Form/2,
	 dec_ValidationParms/2, dec_Validity/2,
	 'dec_VersionPKCS-1'/2, dec_VersionPKIX1Explicit88/2,
	 dec_X121Address/2, dec_X520CommonName/2,
	 dec_X520LocalityName/2, dec_X520OrganizationName/2,
	 dec_X520OrganizationalUnitName/2, dec_X520Pseudonym/2,
	 dec_X520SerialNumber/2, dec_X520StateOrProvinceName/2,
	 dec_X520Title/2, dec_X520countryName/2,
	 dec_X520dnQualifier/2, dec_X520name/2,
	 dec_authorityInfoAccess/3, dec_authorityKeyIdentifier/3,
	 dec_basicConstraints/3, dec_cRLDistributionPoints/3,
	 dec_cRLNumber/3, dec_cRLReasons/3,
	 dec_certificateIssuer/3, dec_certificatePolicies/3,
	 dec_commonName/3, dec_countryName/3,
	 dec_deltaCRLIndicator/3, dec_dh/3, dec_dnQualifier/3,
	 dec_domainComponent/3, dec_dsa/3, 'dec_dsa-with-sha1'/3,
	 'dec_ec-public-key'/3, 'dec_ecdsa-with-sha1'/3,
	 dec_emailAddress/3, dec_extKeyUsage/3,
	 'dec_field-characteristic-two'/3,
	 'dec_field-prime-field'/3, dec_freshestCRL/3,
	 dec_generationQualifier/3, dec_givenName/3,
	 'dec_gn-basis'/3, dec_holdInstructionCode/3,
	 dec_inhibitAnyPolicy/3, dec_initials/3,
	 dec_invalidityDate/3, dec_issuerAltName/3,
	 dec_issuingDistributionPoint/3, dec_kea/3,
	 dec_keyUsage/3, dec_localityName/3,
	 'dec_md2-with-rsa-encryption'/3,
	 'dec_md5-with-rsa-encryption'/3, dec_name/3,
	 dec_nameConstraints/3, dec_organizationName/3,
	 dec_organizationalUnitName/3, dec_policyConstraints/3,
	 dec_policyMappings/3, 'dec_pp-basis'/3,
	 dec_privateKeyUsagePeriod/3, dec_pseudonym/3,
	 'dec_rsa-encryption'/3, dec_serialNumber/3,
	 'dec_sha1-with-rsa-encryption'/3,
	 dec_stateOrProvinceName/3, dec_subjectAltName/3,
	 dec_subjectDirectoryAttributes/3,
	 dec_subjectInfoAccess/3, dec_subjectKeyIdentifier/3,
	 dec_surname/3, dec_title/3, 'dec_tp-basis'/3,
	 'dec_x400-common-name'/3,
	 'dec_x400-extended-network-address'/3,
	 'dec_x400-extension-OR-address-components'/3,
	 'dec_x400-extension-physical-delivery-address-components'/3,
	 'dec_x400-local-postal-attributes'/3,
	 'dec_x400-pds-name'/3,
	 'dec_x400-physical-delivery-country-name'/3,
	 'dec_x400-physical-delivery-office-name'/3,
	 'dec_x400-physical-delivery-office-number'/3,
	 'dec_x400-physical-delivery-organization-name'/3,
	 'dec_x400-physical-delivery-personal-name'/3,
	 'dec_x400-post-office-box-address'/3,
	 'dec_x400-postal-code'/3,
	 'dec_x400-poste-restante-address'/3,
	 'dec_x400-street-address'/3,
	 'dec_x400-teletex-common-name'/3,
	 'dec_x400-teletex-domain-defined-attributes'/3,
	 'dec_x400-teletex-personal-name'/3,
	 'dec_x400-terminal-type'/3,
	 'dec_x400-unformatted-postal-address'/3,
	 'dec_x400-unique-postal-name'/3, decode/2,
	 decode_TBSCert_exclusive/1, decode_disp/2,
	 decode_part/2, dhpublicnumber/0, 'ecdsa-with-SHA1'/0,
	 ellipticCurve/0, enc_AAControls/2, enc_ACClearAttrs/2,
	 enc_AccessDescription/2, enc_AdministrationDomainName/2,
	 enc_AlgorithmIdentifier/2, enc_AnotherName/2, enc_Any/2,
	 enc_AttCertIssuer/2, enc_AttCertValidityPeriod/2,
	 enc_AttCertVersion/2, enc_AttrSpec/2, enc_Attribute/2,
	 enc_AttributeCertificate/2,
	 enc_AttributeCertificateInfo/2, enc_AttributeType/2,
	 enc_AttributeTypeAndValue/2, enc_AttributeValue/2,
	 enc_AuthorityInfoAccessSyntax/2,
	 enc_AuthorityKeyIdentifier/2, enc_BaseCRLNumber/2,
	 enc_BaseDistance/2, enc_BasicConstraints/2,
	 enc_Boolean/2, enc_BuiltInDomainDefinedAttribute/2,
	 enc_BuiltInDomainDefinedAttributes/2,
	 enc_BuiltInStandardAttributes/2, enc_CPSuri/2,
	 enc_CRLDistributionPoints/2, enc_CRLNumber/2,
	 enc_CRLReason/2, enc_CertPolicyId/2, enc_Certificate/2,
	 enc_CertificateIssuer/2, enc_CertificateList/2,
	 enc_CertificatePolicies/2,
	 enc_CertificateSerialNumber/2,
	 'enc_Characteristic-two'/2, enc_ClassList/2,
	 enc_Clearance/2, enc_CommonName/2, enc_CountryName/2,
	 enc_Curve/2, enc_DHPublicKey/2, enc_DSAPublicKey/2,
	 enc_DigestAlgorithmIdentifier/2, enc_DigestInfo/2,
	 enc_DirectoryString/2, enc_DisplayText/2,
	 enc_DistinguishedName/2, enc_DistributionPoint/2,
	 enc_DistributionPointName/2, enc_DomainComponent/2,
	 enc_DomainParameters/2, 'enc_Dss-Parms'/2,
	 'enc_Dss-Sig-Value'/2, 'enc_ECDSA-Sig-Value'/2,
	 enc_ECPVer/2, enc_ECParameters/2, enc_ECPoint/2,
	 enc_EDIPartyName/2, enc_EcpkParameters/2,
	 enc_EmailAddress/2, enc_ExtKeyUsageSyntax/2,
	 enc_ExtendedNetworkAddress/2, enc_Extension/2,
	 'enc_Extension-Any'/2, enc_ExtensionAttribute/2,
	 enc_ExtensionAttributes/2,
	 enc_ExtensionORAddressComponents/2,
	 enc_ExtensionPhysicalDeliveryAddressComponents/2,
	 enc_Extensions/2, enc_FieldElement/2, enc_FieldID/2,
	 enc_FreshestCRL/2, enc_GeneralName/2,
	 enc_GeneralNames/2, enc_GeneralSubtree/2,
	 enc_GeneralSubtrees/2, enc_HoldInstructionCode/2,
	 enc_Holder/2, enc_IetfAttrSyntax/2,
	 enc_InhibitAnyPolicy/2, enc_InvalidityDate/2,
	 enc_IssuerAltName/2, enc_IssuerSerial/2,
	 enc_IssuingDistributionPoint/2, 'enc_KEA-Parms-Id'/2,
	 'enc_KEA-PublicKey'/2, enc_KeyIdentifier/2,
	 enc_KeyPurposeId/2, enc_KeyUsage/2,
	 enc_LocalPostalAttributes/2, enc_Name/2,
	 enc_NameConstraints/2, enc_NetworkAddress/2,
	 enc_NoticeReference/2, enc_NumericUserIdentifier/2,
	 enc_ORAddress/2, enc_ObjId/2, enc_ObjectDigestInfo/2,
	 enc_OrganizationName/2, enc_OrganizationalUnitName/2,
	 enc_OrganizationalUnitNames/2, enc_OtherPrimeInfo/2,
	 enc_OtherPrimeInfos/2, enc_PDSName/2,
	 enc_PDSParameter/2, enc_Pentanomial/2,
	 enc_PersonalName/2, enc_PhysicalDeliveryCountryName/2,
	 enc_PhysicalDeliveryOfficeName/2,
	 enc_PhysicalDeliveryOfficeNumber/2,
	 enc_PhysicalDeliveryOrganizationName/2,
	 enc_PhysicalDeliveryPersonalName/2,
	 enc_PolicyConstraints/2, enc_PolicyInformation/2,
	 enc_PolicyMappings/2, enc_PolicyQualifierId/2,
	 enc_PolicyQualifierInfo/2, enc_PostOfficeBoxAddress/2,
	 enc_PostalCode/2, enc_PosteRestanteAddress/2,
	 enc_PresentationAddress/2, 'enc_Prime-p'/2,
	 enc_PrivateDomainName/2, enc_PrivateKeyUsagePeriod/2,
	 enc_ProxyInfo/2, enc_PublicKeyAlgorithm/2,
	 enc_RDNSequence/2, enc_RSAPrivateKey/2,
	 enc_RSAPublicKey/2, enc_ReasonFlags/2,
	 enc_RelativeDistinguishedName/2, enc_RoleSyntax/2,
	 enc_SSLAttributeTypeAndValue/2, enc_SSLCertificate/2,
	 'enc_SSLCharacteristic-two'/2, enc_SSLExtension/2,
	 enc_SSLExtensionAttribute/2,
	 enc_SSLExtensionAttributes/2, enc_SSLExtensions/2,
	 enc_SSLFieldID/2, enc_SSLSubjectPublicKeyInfo/2,
	 'enc_SSLSubjectPublicKeyInfo-Any'/2,
	 enc_SSLTBSCertificate/2, enc_SecurityCategory/2,
	 enc_SignatureAlgorithm/2,
	 'enc_SignatureAlgorithm-Any'/2, enc_SkipCerts/2,
	 enc_StreetAddress/2, enc_SubjectAltName/2,
	 enc_SubjectDirectoryAttributes/2,
	 enc_SubjectInfoAccessSyntax/2,
	 enc_SubjectKeyIdentifier/2, enc_SubjectPublicKeyInfo/2,
	 enc_SvceAuthInfo/2, enc_TBSCertList/2,
	 enc_TBSCertificate/2, enc_Target/2, enc_TargetCert/2,
	 enc_Targets/2, enc_TeletexCommonName/2,
	 enc_TeletexDomainDefinedAttribute/2,
	 enc_TeletexDomainDefinedAttributes/2,
	 enc_TeletexOrganizationName/2,
	 enc_TeletexOrganizationalUnitName/2,
	 enc_TeletexOrganizationalUnitNames/2,
	 enc_TeletexPersonalName/2, enc_TerminalIdentifier/2,
	 enc_TerminalType/2, enc_Time/2, enc_Trinomial/2,
	 enc_UnformattedPostalAddress/2, enc_UniqueIdentifier/2,
	 enc_UniquePostalName/2, enc_UserNotice/2, enc_V2Form/2,
	 enc_ValidationParms/2, enc_Validity/2,
	 'enc_VersionPKCS-1'/2, enc_VersionPKIX1Explicit88/2,
	 enc_X121Address/2, enc_X520CommonName/2,
	 enc_X520LocalityName/2, enc_X520OrganizationName/2,
	 enc_X520OrganizationalUnitName/2, enc_X520Pseudonym/2,
	 enc_X520SerialNumber/2, enc_X520StateOrProvinceName/2,
	 enc_X520Title/2, enc_X520countryName/2,
	 enc_X520dnQualifier/2, enc_X520name/2,
	 enc_authorityInfoAccess/3, enc_authorityKeyIdentifier/3,
	 enc_basicConstraints/3, enc_cRLDistributionPoints/3,
	 enc_cRLNumber/3, enc_cRLReasons/3,
	 enc_certificateIssuer/3, enc_certificatePolicies/3,
	 enc_commonName/3, enc_countryName/3,
	 enc_deltaCRLIndicator/3, enc_dh/3, enc_dnQualifier/3,
	 enc_domainComponent/3, enc_dsa/3, 'enc_dsa-with-sha1'/3,
	 'enc_ec-public-key'/3, 'enc_ecdsa-with-sha1'/3,
	 enc_emailAddress/3, enc_extKeyUsage/3,
	 'enc_field-characteristic-two'/3,
	 'enc_field-prime-field'/3, enc_freshestCRL/3,
	 enc_generationQualifier/3, enc_givenName/3,
	 'enc_gn-basis'/3, enc_holdInstructionCode/3,
	 enc_inhibitAnyPolicy/3, enc_initials/3,
	 enc_invalidityDate/3, enc_issuerAltName/3,
	 enc_issuingDistributionPoint/3, enc_kea/3,
	 enc_keyUsage/3, enc_localityName/3,
	 'enc_md2-with-rsa-encryption'/3,
	 'enc_md5-with-rsa-encryption'/3, enc_name/3,
	 enc_nameConstraints/3, enc_organizationName/3,
	 enc_organizationalUnitName/3, enc_policyConstraints/3,
	 enc_policyMappings/3, 'enc_pp-basis'/3,
	 enc_privateKeyUsagePeriod/3, enc_pseudonym/3,
	 'enc_rsa-encryption'/3, enc_serialNumber/3,
	 'enc_sha1-with-rsa-encryption'/3,
	 enc_stateOrProvinceName/3, enc_subjectAltName/3,
	 enc_subjectDirectoryAttributes/3,
	 enc_subjectInfoAccess/3, enc_subjectKeyIdentifier/3,
	 enc_surname/3, enc_title/3, 'enc_tp-basis'/3,
	 'enc_x400-common-name'/3,
	 'enc_x400-extended-network-address'/3,
	 'enc_x400-extension-OR-address-components'/3,
	 'enc_x400-extension-physical-delivery-address-components'/3,
	 'enc_x400-local-postal-attributes'/3,
	 'enc_x400-pds-name'/3,
	 'enc_x400-physical-delivery-country-name'/3,
	 'enc_x400-physical-delivery-office-name'/3,
	 'enc_x400-physical-delivery-office-number'/3,
	 'enc_x400-physical-delivery-organization-name'/3,
	 'enc_x400-physical-delivery-personal-name'/3,
	 'enc_x400-post-office-box-address'/3,
	 'enc_x400-postal-code'/3,
	 'enc_x400-poste-restante-address'/3,
	 'enc_x400-street-address'/3,
	 'enc_x400-teletex-common-name'/3,
	 'enc_x400-teletex-domain-defined-attributes'/3,
	 'enc_x400-teletex-personal-name'/3,
	 'enc_x400-terminal-type'/3,
	 'enc_x400-unformatted-postal-address'/3,
	 'enc_x400-unique-postal-name'/3, encode/2,
	 encode_disp/2, encoding_rule/0,
	 'extended-network-address'/0,
	 'extension-OR-address-components'/0,
	 'extension-physical-delivery-address-components'/0,
	 getdec_SupportedAttributeTypeAndValues/2,
	 getdec_SupportedCharacteristicTwos/2,
	 getdec_SupportedExtensionAttributes/2,
	 getdec_SupportedExtensions/2,
	 getdec_SupportedFieldIds/2,
	 getdec_SupportedPublicKeyAlgorithms/2,
	 getdec_SupportedSignatureAlgorithms/2,
	 getenc_SupportedAttributeTypeAndValues/2,
	 getenc_SupportedCharacteristicTwos/2,
	 getenc_SupportedExtensionAttributes/2,
	 getenc_SupportedExtensions/2,
	 getenc_SupportedFieldIds/2,
	 getenc_SupportedPublicKeyAlgorithms/2,
	 getenc_SupportedSignatureAlgorithms/2, gnBasis/0,
	 holdInstruction/0, 'id-aca'/0,
	 'id-aca-accessIdentity'/0,
	 'id-aca-authenticationInfo'/0,
	 'id-aca-chargingIdentity'/0, 'id-aca-encAttrs'/0,
	 'id-aca-group'/0, 'id-ad'/0, 'id-ad-caIssuers'/0,
	 'id-ad-caRepository'/0, 'id-ad-ocsp'/0,
	 'id-ad-timeStamping'/0, 'id-at'/0, 'id-at-clearance'/0,
	 'id-at-commonName'/0, 'id-at-countryName'/0,
	 'id-at-dnQualifier'/0, 'id-at-generationQualifier'/0,
	 'id-at-givenName'/0, 'id-at-initials'/0,
	 'id-at-localityName'/0, 'id-at-name'/0,
	 'id-at-organizationName'/0,
	 'id-at-organizationalUnitName'/0, 'id-at-pseudonym'/0,
	 'id-at-role'/0, 'id-at-serialNumber'/0,
	 'id-at-stateOrProvinceName'/0, 'id-at-surname'/0,
	 'id-at-title'/0, 'id-ce'/0,
	 'id-ce-authorityKeyIdentifier'/0,
	 'id-ce-basicConstraints'/0,
	 'id-ce-cRLDistributionPoints'/0, 'id-ce-cRLNumber'/0,
	 'id-ce-cRLReasons'/0, 'id-ce-certificateIssuer'/0,
	 'id-ce-certificatePolicies'/0,
	 'id-ce-deltaCRLIndicator'/0, 'id-ce-extKeyUsage'/0,
	 'id-ce-freshestCRL'/0, 'id-ce-holdInstructionCode'/0,
	 'id-ce-inhibitAnyPolicy'/0, 'id-ce-invalidityDate'/0,
	 'id-ce-issuerAltName'/0,
	 'id-ce-issuingDistributionPoint'/0, 'id-ce-keyUsage'/0,
	 'id-ce-nameConstraints'/0, 'id-ce-policyConstraints'/0,
	 'id-ce-policyMappings'/0,
	 'id-ce-privateKeyUsagePeriod'/0,
	 'id-ce-subjectAltName'/0,
	 'id-ce-subjectDirectoryAttributes'/0,
	 'id-ce-subjectKeyIdentifier'/0,
	 'id-ce-targetInformation'/0,
	 'id-characteristic-two-basis'/0, 'id-domainComponent'/0,
	 'id-dsa'/0, 'id-dsa-with-sha1'/0, 'id-ecPublicKey'/0,
	 'id-ecSigType'/0, 'id-emailAddress'/0, 'id-fieldType'/0,
	 'id-holdinstruction-callissuer'/0,
	 'id-holdinstruction-none'/0,
	 'id-holdinstruction-reject'/0,
	 'id-keyExchangeAlgorithm'/0, 'id-kp'/0,
	 'id-kp-OCSPSigning'/0, 'id-kp-clientAuth'/0,
	 'id-kp-codeSigning'/0, 'id-kp-emailProtection'/0,
	 'id-kp-serverAuth'/0, 'id-kp-timeStamping'/0, 'id-pe'/0,
	 'id-pe-aaControls'/0, 'id-pe-ac-auditIdentity'/0,
	 'id-pe-ac-proxying'/0, 'id-pe-authorityInfoAccess'/0,
	 'id-pe-subjectInfoAccess'/0, 'id-pkix'/0,
	 'id-publicKeyType'/0, 'id-qt'/0, 'id-qt-cps'/0,
	 'id-qt-unotice'/0, 'id-sha1'/0, info/0,
	 'local-postal-attributes'/0, md2/0,
	 md2WithRSAEncryption/0, md5/0, md5WithRSAEncryption/0,
	 'pds-name'/0, 'physical-delivery-country-name'/0,
	 'physical-delivery-office-name'/0,
	 'physical-delivery-office-number'/0,
	 'physical-delivery-organization-name'/0,
	 'physical-delivery-personal-name'/0, 'pkcs-1'/0,
	 'pkcs-9'/0, 'post-office-box-address'/0,
	 'postal-code'/0, 'poste-restante-address'/0, ppBasis/0,
	 'prime-field'/0, prime192v1/0, prime192v2/0,
	 prime192v3/0, prime239v1/0, prime239v2/0, prime239v3/0,
	 prime256v1/0, primeCurve/0, rsaEncryption/0,
	 sha1WithRSAEncryption/0, 'street-address'/0,
	 'teletex-common-name'/0,
	 'teletex-domain-defined-attributes'/0,
	 'teletex-organization-name'/0,
	 'teletex-organizational-unit-names'/0,
	 'teletex-personal-name'/0, 'terminal-type'/0, tpBasis/0,
	 'ub-common-name'/0, 'ub-common-name-length'/0,
	 'ub-country-name-alpha-length'/0,
	 'ub-country-name-numeric-length'/0,
	 'ub-domain-defined-attribute-type-length'/0,
	 'ub-domain-defined-attribute-value-length'/0,
	 'ub-domain-defined-attributes'/0,
	 'ub-domain-name-length'/0, 'ub-e163-4-number-length'/0,
	 'ub-e163-4-sub-address-length'/0,
	 'ub-emailaddress-length'/0, 'ub-extension-attributes'/0,
	 'ub-generation-qualifier-length'/0,
	 'ub-given-name-length'/0, 'ub-initials-length'/0,
	 'ub-integer-options'/0, 'ub-locality-name'/0,
	 'ub-match'/0, 'ub-name'/0,
	 'ub-numeric-user-id-length'/0, 'ub-organization-name'/0,
	 'ub-organization-name-length'/0,
	 'ub-organizational-unit-name'/0,
	 'ub-organizational-unit-name-length'/0,
	 'ub-organizational-units'/0, 'ub-pds-name-length'/0,
	 'ub-pds-parameter-length'/0,
	 'ub-pds-physical-address-lines'/0,
	 'ub-postal-code-length'/0, 'ub-pseudonym'/0,
	 'ub-serial-number'/0, 'ub-state-name'/0,
	 'ub-surname-length'/0, 'ub-terminal-id-length'/0,
	 'ub-title'/0, 'ub-unformatted-address-length'/0,
	 'ub-x121-address-length'/0,
	 'unformatted-postal-address'/0,
	 'unique-postal-name'/0]).

%% =====================================================================
%% 
%% The following code stems from module `'OTP-PKIX''.
%% 

%% Generated by the Erlang ASN.1 BER_V2-compiler version, utilizing bit-syntax:1.6.11
%% Purpose: encoder and decoder to the types in mod OTP-PKIX

-record('Curve', {a, b, seed = asn1_NOVALUE}).

-record('ECParameters',
	{version, fieldID, curve, base, order,
	 cofactor = asn1_NOVALUE}).

-record('Pentanomial', {k1, k2, k3}).

-record('Characteristic-two', {m, basis, parameters}).

-record('ECDSA-Sig-Value', {r, s}).

-record('FieldID', {fieldType, parameters}).

-record('ValidationParms', {seed, pgenCounter}).

-record('DomainParameters',
	{p, g, q, j = asn1_NOVALUE,
	 validationParms = asn1_NOVALUE}).

-record('RSAPublicKey', {modulus, publicExponent}).

-record('Dss-Sig-Value', {r, s}).

-record('Dss-Parms', {p, q, g}).

-record('ACClearAttrs', {acIssuer, acSerial, attrs}).

-record('AAControls',
	{pathLenConstraint = asn1_NOVALUE,
	 permittedAttrs = asn1_NOVALUE,
	 excludedAttrs = asn1_NOVALUE,
	 permitUnSpecified = asn1_DEFAULT}).

-record('SecurityCategory', {type, value}).

-record('Clearance',
	{policyId, classList = asn1_DEFAULT,
	 securityCategories = asn1_NOVALUE}).

-record('RoleSyntax',
	{roleAuthority = asn1_NOVALUE, roleName}).

-record('SvceAuthInfo',
	{service, ident, authInfo = asn1_NOVALUE}).

-record('IetfAttrSyntax',
	{policyAuthority = asn1_NOVALUE, values}).

-record('TargetCert',
	{targetCertificate, targetName = asn1_NOVALUE,
	 certDigestInfo = asn1_NOVALUE}).

-record('AttCertValidityPeriod',
	{notBeforeTime, notAfterTime}).

-record('IssuerSerial',
	{issuer, serial, issuerUID = asn1_NOVALUE}).

-record('V2Form',
	{issuerName = asn1_NOVALUE,
	 baseCertificateID = asn1_NOVALUE,
	 objectDigestInfo = asn1_NOVALUE}).

-record('ObjectDigestInfo',
	{digestedObjectType, otherObjectTypeID = asn1_NOVALUE,
	 digestAlgorithm, objectDigest}).

-record('Holder',
	{baseCertificateID = asn1_NOVALUE,
	 entityName = asn1_NOVALUE,
	 objectDigestInfo = asn1_NOVALUE}).

-record('AttributeCertificateInfo',
	{version, holder, issuer, signature, serialNumber,
	 attrCertValidityPeriod, attributes,
	 issuerUniqueID = asn1_NOVALUE,
	 extensions = asn1_NOVALUE}).

-record('AttributeCertificate',
	{acinfo, signatureAlgorithm, signatureValue}).

-record('IssuingDistributionPoint',
	{distributionPoint = asn1_NOVALUE,
	 onlyContainsUserCerts = asn1_DEFAULT,
	 onlyContainsCACerts = asn1_DEFAULT,
	 onlySomeReasons = asn1_NOVALUE,
	 indirectCRL = asn1_DEFAULT,
	 onlyContainsAttributeCerts = asn1_DEFAULT}).

-record('AccessDescription',
	{accessMethod, accessLocation}).

-record('DistributionPoint',
	{distributionPoint = asn1_NOVALUE,
	 reasons = asn1_NOVALUE, cRLIssuer = asn1_NOVALUE}).

-record('PolicyConstraints',
	{requireExplicitPolicy = asn1_NOVALUE,
	 inhibitPolicyMapping = asn1_NOVALUE}).

-record('GeneralSubtree',
	{base, minimum = asn1_DEFAULT, maximum = asn1_NOVALUE}).

-record('NameConstraints',
	{permittedSubtrees = asn1_NOVALUE,
	 excludedSubtrees = asn1_NOVALUE}).

-record('BasicConstraints',
	{cA = asn1_DEFAULT, pathLenConstraint = asn1_NOVALUE}).

-record('EDIPartyName',
	{nameAssigner = asn1_NOVALUE, partyName}).

-record('AnotherName', {'type-id', value}).

-record('PolicyMappings_SEQOF',
	{issuerDomainPolicy, subjectDomainPolicy}).

-record('NoticeReference',
	{organization, noticeNumbers}).

-record('UserNotice',
	{noticeRef = asn1_NOVALUE,
	 explicitText = asn1_NOVALUE}).

-record('PolicyQualifierInfo',
	{policyQualifierId, qualifier}).

-record('PolicyInformation',
	{policyIdentifier, policyQualifiers = asn1_NOVALUE}).

-record('PrivateKeyUsagePeriod',
	{notBefore = asn1_NOVALUE, notAfter = asn1_NOVALUE}).

-record('AuthorityKeyIdentifier',
	{keyIdentifier = asn1_NOVALUE,
	 authorityCertIssuer = asn1_NOVALUE,
	 authorityCertSerialNumber = asn1_NOVALUE}).

-record('DigestInfo', {digestAlgorithm, digest}).

-record('OtherPrimeInfo',
	{prime, exponent, coefficient}).

-record('RSAPrivateKey',
	{version, modulus, publicExponent, privateExponent,
	 prime1, prime2, exponent1, exponent2, coefficient,
	 otherPrimeInfos = asn1_NOVALUE}).

-record('TeletexDomainDefinedAttribute', {type, value}).

-record('PresentationAddress',
	{pSelector = asn1_NOVALUE, sSelector = asn1_NOVALUE,
	 tSelector = asn1_NOVALUE, nAddresses}).

-record('ExtendedNetworkAddress_e163-4-address',
	{number, 'sub-address' = asn1_NOVALUE}).

-record('PDSParameter',
	{'printable-string' = asn1_NOVALUE,
	 'teletex-string' = asn1_NOVALUE}).

-record('UnformattedPostalAddress',
	{'printable-address' = asn1_NOVALUE,
	 'teletex-string' = asn1_NOVALUE}).

-record('TeletexPersonalName',
	{surname, 'given-name' = asn1_NOVALUE,
	 initials = asn1_NOVALUE,
	 'generation-qualifier' = asn1_NOVALUE}).

-record('ExtensionAttribute',
	{'extension-attribute-type',
	 'extension-attribute-value'}).

-record('BuiltInDomainDefinedAttribute', {type, value}).

-record('PersonalName',
	{surname, 'given-name' = asn1_NOVALUE,
	 initials = asn1_NOVALUE,
	 'generation-qualifier' = asn1_NOVALUE}).

-record('BuiltInStandardAttributes',
	{'country-name' = asn1_NOVALUE,
	 'administration-domain-name' = asn1_NOVALUE,
	 'network-address' = asn1_NOVALUE,
	 'terminal-identifier' = asn1_NOVALUE,
	 'private-domain-name' = asn1_NOVALUE,
	 'organization-name' = asn1_NOVALUE,
	 'numeric-user-identifier' = asn1_NOVALUE,
	 'personal-name' = asn1_NOVALUE,
	 'organizational-unit-names' = asn1_NOVALUE}).

-record('ORAddress',
	{'built-in-standard-attributes',
	 'built-in-domain-defined-attributes' = asn1_NOVALUE,
	 'extension-attributes' = asn1_NOVALUE}).

-record('AlgorithmIdentifier',
	{algorithm, parameters = asn1_NOVALUE}).

-record('TBSCertList',
	{version = asn1_NOVALUE, signature, issuer, thisUpdate,
	 nextUpdate = asn1_NOVALUE,
	 revokedCertificates = asn1_NOVALUE,
	 crlExtensions = asn1_NOVALUE}).

-record('TBSCertList_revokedCertificates_SEQOF',
	{userCertificate, revocationDate,
	 crlEntryExtensions = asn1_NOVALUE}).

-record('CertificateList',
	{tbsCertList, signatureAlgorithm, signature}).

-record('Extension',
	{extnID, critical = asn1_DEFAULT, extnValue}).

-record('SubjectPublicKeyInfo',
	{algorithm, subjectPublicKey}).

-record('Validity', {notBefore, notAfter}).

-record('TBSCertificate',
	{version = asn1_DEFAULT, serialNumber, signature,
	 issuer, validity, subject, subjectPublicKeyInfo,
	 issuerUniqueID = asn1_NOVALUE,
	 subjectUniqueID = asn1_NOVALUE,
	 extensions = asn1_NOVALUE}).

-record('Certificate',
	{tbsCertificate, signatureAlgorithm, signature}).

-record('AttributeTypeAndValue', {type, value}).

-record('Attribute', {type, values}).

-record('Extension-Any',
	{extnID, critical = asn1_DEFAULT, extnValue}).

-record('SSLExtension',
	{extnID, critical = asn1_DEFAULT, extnValue}).

-record('SSLExtensionAttribute',
	{extensionAttributeType, extensionAttributeValue}).

-record('SSLCharacteristic-two',
	{m, basis, parameters}).

-record('SSLFieldID', {fieldType, parameters}).

-record('PublicKeyAlgorithm',
	{algorithm, parameters = asn1_NOVALUE}).

-record('SignatureAlgorithm-Any',
	{algorithm, parameters = asn1_NOVALUE}).

-record('SignatureAlgorithm',
	{algorithm, parameters = asn1_NOVALUE}).

-record('SSLSubjectPublicKeyInfo-Any',
	{algorithm, subjectPublicKey}).

-record('SSLSubjectPublicKeyInfo',
	{algorithm, subjectPublicKey}).

-record('SSLSubjectPublicKeyInfo_algorithm',
	{algo, parameters = asn1_NOVALUE}).

-record('SSLAttributeTypeAndValue', {type, value}).

-record('SSLTBSCertificate',
	{version = asn1_DEFAULT, serialNumber, signature,
	 issuer, validity, subject, subjectPublicKeyInfo,
	 issuerUniqueID = asn1_NOVALUE,
	 subjectUniqueID = asn1_NOVALUE,
	 extensions = asn1_NOVALUE}).

-record('SSLCertificate',
	{tbsCertificate, signatureAlgorithm, signature}).

%%<<< -asn1_info([{vsn, '1.6.11'}, {module, 'OTP-PKIX'},
%%<<<             {options,
%%<<<              [ber_bin_v2, report_errors,
%%<<<               {cwd,
%%<<<                "/net/isildur/ldisk/daily_build/otp_prebuild_r"
%%<<<                "13b02.2009-09-21_11/otp_src_R13B02/lib/ssl/pk"
%%<<<                "ix"},
%%<<<               {outdir,
%%<<<                "/net/isildur/ldisk/daily_build/otp_prebuild_r"
%%<<<                "13b02.2009-09-21_11/otp_src_R13B02/lib/ssl/pk"
%%<<<                "ix"},
%%<<<               inline, asn1config, noobj, optimize, compact_bit_string,
%%<<<               der, {i, "."},
%%<<<               {i,
%%<<<                "/net/isildur/ldisk/daily_build/otp_prebuild_r"
%%<<<                "13b02.2009-09-21_11/otp_src_R13B02/lib/ssl/pk"
%%<<<                "ix"}]}]).

encoding_rule() -> ber_bin_v2.

encode(Type, Data) ->
    case catch encode_disp(Type, Data) of
      {'EXIT', {error, Reason}} -> {error, Reason};
      {'EXIT', Reason} -> {error, {asn1, Reason}};
      {Bytes, _Len} -> {ok, Bytes};
      Bytes -> {ok, Bytes}
    end.

decode(Type, Data) ->
    case catch decode_disp(Type, element(1, decode(Data)))
	of
      {'EXIT', {error, Reason}} -> {error, Reason};
      {'EXIT', Reason} -> {error, {asn1, Reason}};
      Result -> {ok, Result}
    end.

decode_partial_incomplete(Type, Data0, Pattern) ->
    {Data, _RestBin} = decode_primitive_incomplete(Pattern,
						   Data0),
    case catch decode_partial_inc_disp(Type, Data) of
      {'EXIT', {error, Reason}} -> {error, Reason};
      {'EXIT', Reason} -> {error, {asn1, Reason}};
      Result -> {ok, Result}
    end.

decode_part(Type, Data0) ->
    case catch decode_inc_disp(Type,
			       element(1, decode(Data0)))
	of
      {'EXIT', {error, Reason}} -> {error, Reason};
      {'EXIT', Reason} -> {error, {asn1, Reason}};
      Result -> {ok, Result}
    end.

encode_disp('Curve', Data) -> enc_Curve(Data);
encode_disp('ECPVer', Data) -> enc_ECPVer(Data);
encode_disp('ECParameters', Data) ->
    enc_ECParameters(Data);
encode_disp('EcpkParameters', Data) ->
    enc_EcpkParameters(Data);
encode_disp('ECPoint', Data) -> enc_ECPoint(Data);
encode_disp('FieldElement', Data) ->
    enc_FieldElement(Data);
encode_disp('Pentanomial', Data) ->
    enc_Pentanomial(Data);
encode_disp('Trinomial', Data) -> enc_Trinomial(Data);
encode_disp('Characteristic-two', Data) ->
    'enc_Characteristic-two'(Data);
encode_disp('Prime-p', Data) -> 'enc_Prime-p'(Data);
encode_disp('ECDSA-Sig-Value', Data) ->
    'enc_ECDSA-Sig-Value'(Data);
encode_disp('FieldID', Data) -> enc_FieldID(Data);
encode_disp('KEA-Parms-Id', Data) ->
    'enc_KEA-Parms-Id'(Data);
encode_disp('ValidationParms', Data) ->
    enc_ValidationParms(Data);
encode_disp('DomainParameters', Data) ->
    enc_DomainParameters(Data);
encode_disp('DHPublicKey', Data) ->
    enc_DHPublicKey(Data);
encode_disp('RSAPublicKey', Data) ->
    enc_RSAPublicKey(Data);
encode_disp('Dss-Sig-Value', Data) ->
    'enc_Dss-Sig-Value'(Data);
encode_disp('Dss-Parms', Data) -> 'enc_Dss-Parms'(Data);
encode_disp('DSAPublicKey', Data) ->
    enc_DSAPublicKey(Data);
encode_disp('ProxyInfo', Data) -> enc_ProxyInfo(Data);
encode_disp('ACClearAttrs', Data) ->
    enc_ACClearAttrs(Data);
encode_disp('AttrSpec', Data) -> enc_AttrSpec(Data);
encode_disp('AAControls', Data) -> enc_AAControls(Data);
encode_disp('SecurityCategory', Data) ->
    enc_SecurityCategory(Data);
encode_disp('ClassList', Data) -> enc_ClassList(Data);
encode_disp('Clearance', Data) -> enc_Clearance(Data);
encode_disp('RoleSyntax', Data) -> enc_RoleSyntax(Data);
encode_disp('SvceAuthInfo', Data) ->
    enc_SvceAuthInfo(Data);
encode_disp('IetfAttrSyntax', Data) ->
    enc_IetfAttrSyntax(Data);
encode_disp('TargetCert', Data) -> enc_TargetCert(Data);
encode_disp('Target', Data) -> enc_Target(Data);
encode_disp('Targets', Data) -> enc_Targets(Data);
encode_disp('AttCertValidityPeriod', Data) ->
    enc_AttCertValidityPeriod(Data);
encode_disp('IssuerSerial', Data) ->
    enc_IssuerSerial(Data);
encode_disp('V2Form', Data) -> enc_V2Form(Data);
encode_disp('AttCertIssuer', Data) ->
    enc_AttCertIssuer(Data);
encode_disp('ObjectDigestInfo', Data) ->
    enc_ObjectDigestInfo(Data);
encode_disp('Holder', Data) -> enc_Holder(Data);
encode_disp('AttCertVersion', Data) ->
    enc_AttCertVersion(Data);
encode_disp('AttributeCertificateInfo', Data) ->
    enc_AttributeCertificateInfo(Data);
encode_disp('AttributeCertificate', Data) ->
    enc_AttributeCertificate(Data);
encode_disp('InvalidityDate', Data) ->
    enc_InvalidityDate(Data);
encode_disp('HoldInstructionCode', Data) ->
    enc_HoldInstructionCode(Data);
encode_disp('CertificateIssuer', Data) ->
    enc_CertificateIssuer(Data);
encode_disp('CRLReason', Data) -> enc_CRLReason(Data);
encode_disp('BaseCRLNumber', Data) ->
    enc_BaseCRLNumber(Data);
encode_disp('IssuingDistributionPoint', Data) ->
    enc_IssuingDistributionPoint(Data);
encode_disp('CRLNumber', Data) -> enc_CRLNumber(Data);
encode_disp('SubjectInfoAccessSyntax', Data) ->
    enc_SubjectInfoAccessSyntax(Data);
encode_disp('AccessDescription', Data) ->
    enc_AccessDescription(Data);
encode_disp('AuthorityInfoAccessSyntax', Data) ->
    enc_AuthorityInfoAccessSyntax(Data);
encode_disp('FreshestCRL', Data) ->
    enc_FreshestCRL(Data);
encode_disp('InhibitAnyPolicy', Data) ->
    enc_InhibitAnyPolicy(Data);
encode_disp('KeyPurposeId', Data) ->
    enc_KeyPurposeId(Data);
encode_disp('ExtKeyUsageSyntax', Data) ->
    enc_ExtKeyUsageSyntax(Data);
encode_disp('ReasonFlags', Data) ->
    enc_ReasonFlags(Data);
encode_disp('DistributionPointName', Data) ->
    enc_DistributionPointName(Data);
encode_disp('DistributionPoint', Data) ->
    enc_DistributionPoint(Data);
encode_disp('CRLDistributionPoints', Data) ->
    enc_CRLDistributionPoints(Data);
encode_disp('SkipCerts', Data) -> enc_SkipCerts(Data);
encode_disp('PolicyConstraints', Data) ->
    enc_PolicyConstraints(Data);
encode_disp('BaseDistance', Data) ->
    enc_BaseDistance(Data);
encode_disp('GeneralSubtree', Data) ->
    enc_GeneralSubtree(Data);
encode_disp('GeneralSubtrees', Data) ->
    enc_GeneralSubtrees(Data);
encode_disp('NameConstraints', Data) ->
    enc_NameConstraints(Data);
encode_disp('BasicConstraints', Data) ->
    enc_BasicConstraints(Data);
encode_disp('SubjectDirectoryAttributes', Data) ->
    enc_SubjectDirectoryAttributes(Data);
encode_disp('IssuerAltName', Data) ->
    enc_IssuerAltName(Data);
encode_disp('EDIPartyName', Data) ->
    enc_EDIPartyName(Data);
encode_disp('AnotherName', Data) ->
    enc_AnotherName(Data);
encode_disp('GeneralName', Data) ->
    enc_GeneralName(Data);
encode_disp('GeneralNames', Data) ->
    enc_GeneralNames(Data);
encode_disp('SubjectAltName', Data) ->
    enc_SubjectAltName(Data);
encode_disp('PolicyMappings', Data) ->
    enc_PolicyMappings(Data);
encode_disp('DisplayText', Data) ->
    enc_DisplayText(Data);
encode_disp('NoticeReference', Data) ->
    enc_NoticeReference(Data);
encode_disp('UserNotice', Data) -> enc_UserNotice(Data);
encode_disp('CPSuri', Data) -> enc_CPSuri(Data);
encode_disp('PolicyQualifierId', Data) ->
    enc_PolicyQualifierId(Data);
encode_disp('PolicyQualifierInfo', Data) ->
    enc_PolicyQualifierInfo(Data);
encode_disp('CertPolicyId', Data) ->
    enc_CertPolicyId(Data);
encode_disp('PolicyInformation', Data) ->
    enc_PolicyInformation(Data);
encode_disp('CertificatePolicies', Data) ->
    enc_CertificatePolicies(Data);
encode_disp('PrivateKeyUsagePeriod', Data) ->
    enc_PrivateKeyUsagePeriod(Data);
encode_disp('KeyUsage', Data) -> enc_KeyUsage(Data);
encode_disp('SubjectKeyIdentifier', Data) ->
    enc_SubjectKeyIdentifier(Data);
encode_disp('KeyIdentifier', Data) ->
    enc_KeyIdentifier(Data);
encode_disp('AuthorityKeyIdentifier', Data) ->
    enc_AuthorityKeyIdentifier(Data);
encode_disp('DigestAlgorithmIdentifier', Data) ->
    enc_DigestAlgorithmIdentifier(Data);
encode_disp('DigestInfo', Data) -> enc_DigestInfo(Data);
encode_disp('OtherPrimeInfo', Data) ->
    enc_OtherPrimeInfo(Data);
encode_disp('OtherPrimeInfos', Data) ->
    enc_OtherPrimeInfos(Data);
encode_disp('VersionPKCS-1', Data) ->
    'enc_VersionPKCS-1'(Data);
encode_disp('RSAPrivateKey', Data) ->
    enc_RSAPrivateKey(Data);
encode_disp('TeletexDomainDefinedAttribute', Data) ->
    enc_TeletexDomainDefinedAttribute(Data);
encode_disp('TeletexDomainDefinedAttributes', Data) ->
    enc_TeletexDomainDefinedAttributes(Data);
encode_disp('TerminalType', Data) ->
    enc_TerminalType(Data);
encode_disp('PresentationAddress', Data) ->
    enc_PresentationAddress(Data);
encode_disp('ExtendedNetworkAddress', Data) ->
    enc_ExtendedNetworkAddress(Data);
encode_disp('PDSParameter', Data) ->
    enc_PDSParameter(Data);
encode_disp('LocalPostalAttributes', Data) ->
    enc_LocalPostalAttributes(Data);
encode_disp('UniquePostalName', Data) ->
    enc_UniquePostalName(Data);
encode_disp('PosteRestanteAddress', Data) ->
    enc_PosteRestanteAddress(Data);
encode_disp('PostOfficeBoxAddress', Data) ->
    enc_PostOfficeBoxAddress(Data);
encode_disp('StreetAddress', Data) ->
    enc_StreetAddress(Data);
encode_disp('UnformattedPostalAddress', Data) ->
    enc_UnformattedPostalAddress(Data);
encode_disp('ExtensionPhysicalDeliveryAddressComponents',
	    Data) ->
    enc_ExtensionPhysicalDeliveryAddressComponents(Data);
encode_disp('PhysicalDeliveryOrganizationName', Data) ->
    enc_PhysicalDeliveryOrganizationName(Data);
encode_disp('PhysicalDeliveryPersonalName', Data) ->
    enc_PhysicalDeliveryPersonalName(Data);
encode_disp('ExtensionORAddressComponents', Data) ->
    enc_ExtensionORAddressComponents(Data);
encode_disp('PhysicalDeliveryOfficeNumber', Data) ->
    enc_PhysicalDeliveryOfficeNumber(Data);
encode_disp('PhysicalDeliveryOfficeName', Data) ->
    enc_PhysicalDeliveryOfficeName(Data);
encode_disp('PostalCode', Data) -> enc_PostalCode(Data);
encode_disp('PhysicalDeliveryCountryName', Data) ->
    enc_PhysicalDeliveryCountryName(Data);
encode_disp('PDSName', Data) -> enc_PDSName(Data);
encode_disp('TeletexOrganizationalUnitName', Data) ->
    enc_TeletexOrganizationalUnitName(Data);
encode_disp('TeletexOrganizationalUnitNames', Data) ->
    enc_TeletexOrganizationalUnitNames(Data);
encode_disp('TeletexPersonalName', Data) ->
    enc_TeletexPersonalName(Data);
encode_disp('TeletexOrganizationName', Data) ->
    enc_TeletexOrganizationName(Data);
encode_disp('TeletexCommonName', Data) ->
    enc_TeletexCommonName(Data);
encode_disp('CommonName', Data) -> enc_CommonName(Data);
encode_disp('ExtensionAttribute', Data) ->
    enc_ExtensionAttribute(Data);
encode_disp('ExtensionAttributes', Data) ->
    enc_ExtensionAttributes(Data);
encode_disp('BuiltInDomainDefinedAttribute', Data) ->
    enc_BuiltInDomainDefinedAttribute(Data);
encode_disp('BuiltInDomainDefinedAttributes', Data) ->
    enc_BuiltInDomainDefinedAttributes(Data);
encode_disp('OrganizationalUnitName', Data) ->
    enc_OrganizationalUnitName(Data);
encode_disp('OrganizationalUnitNames', Data) ->
    enc_OrganizationalUnitNames(Data);
encode_disp('PersonalName', Data) ->
    enc_PersonalName(Data);
encode_disp('NumericUserIdentifier', Data) ->
    enc_NumericUserIdentifier(Data);
encode_disp('OrganizationName', Data) ->
    enc_OrganizationName(Data);
encode_disp('PrivateDomainName', Data) ->
    enc_PrivateDomainName(Data);
encode_disp('TerminalIdentifier', Data) ->
    enc_TerminalIdentifier(Data);
encode_disp('X121Address', Data) ->
    enc_X121Address(Data);
encode_disp('NetworkAddress', Data) ->
    enc_NetworkAddress(Data);
encode_disp('AdministrationDomainName', Data) ->
    enc_AdministrationDomainName(Data);
encode_disp('CountryName', Data) ->
    enc_CountryName(Data);
encode_disp('BuiltInStandardAttributes', Data) ->
    enc_BuiltInStandardAttributes(Data);
encode_disp('ORAddress', Data) -> enc_ORAddress(Data);
encode_disp('AlgorithmIdentifier', Data) ->
    enc_AlgorithmIdentifier(Data);
encode_disp('TBSCertList', Data) ->
    enc_TBSCertList(Data);
encode_disp('CertificateList', Data) ->
    enc_CertificateList(Data);
encode_disp('Extension', Data) -> enc_Extension(Data);
encode_disp('Extensions', Data) -> enc_Extensions(Data);
encode_disp('SubjectPublicKeyInfo', Data) ->
    enc_SubjectPublicKeyInfo(Data);
encode_disp('UniqueIdentifier', Data) ->
    enc_UniqueIdentifier(Data);
encode_disp('Time', Data) -> enc_Time(Data);
encode_disp('Validity', Data) -> enc_Validity(Data);
encode_disp('CertificateSerialNumber', Data) ->
    enc_CertificateSerialNumber(Data);
encode_disp('VersionPKIX1Explicit88', Data) ->
    enc_VersionPKIX1Explicit88(Data);
encode_disp('TBSCertificate', Data) ->
    enc_TBSCertificate(Data);
encode_disp('Certificate', Data) ->
    enc_Certificate(Data);
encode_disp('DirectoryString', Data) ->
    enc_DirectoryString(Data);
encode_disp('RelativeDistinguishedName', Data) ->
    enc_RelativeDistinguishedName(Data);
encode_disp('DistinguishedName', Data) ->
    enc_DistinguishedName(Data);
encode_disp('RDNSequence', Data) ->
    enc_RDNSequence(Data);
encode_disp('Name', Data) -> enc_Name(Data);
encode_disp('EmailAddress', Data) ->
    enc_EmailAddress(Data);
encode_disp('DomainComponent', Data) ->
    enc_DomainComponent(Data);
encode_disp('X520Pseudonym', Data) ->
    enc_X520Pseudonym(Data);
encode_disp('X520SerialNumber', Data) ->
    enc_X520SerialNumber(Data);
encode_disp('X520countryName', Data) ->
    enc_X520countryName(Data);
encode_disp('X520dnQualifier', Data) ->
    enc_X520dnQualifier(Data);
encode_disp('X520Title', Data) -> enc_X520Title(Data);
encode_disp('X520OrganizationalUnitName', Data) ->
    enc_X520OrganizationalUnitName(Data);
encode_disp('X520OrganizationName', Data) ->
    enc_X520OrganizationName(Data);
encode_disp('X520StateOrProvinceName', Data) ->
    enc_X520StateOrProvinceName(Data);
encode_disp('X520LocalityName', Data) ->
    enc_X520LocalityName(Data);
encode_disp('X520CommonName', Data) ->
    enc_X520CommonName(Data);
encode_disp('X520name', Data) -> enc_X520name(Data);
encode_disp('AttributeTypeAndValue', Data) ->
    enc_AttributeTypeAndValue(Data);
encode_disp('AttributeValue', Data) ->
    enc_AttributeValue(Data);
encode_disp('AttributeType', Data) ->
    enc_AttributeType(Data);
encode_disp('Attribute', Data) -> enc_Attribute(Data);
encode_disp('Extension-Any', Data) ->
    'enc_Extension-Any'(Data);
encode_disp('Any', Data) -> enc_Any(Data);
encode_disp('Boolean', Data) -> enc_Boolean(Data);
encode_disp('ObjId', Data) -> enc_ObjId(Data);
encode_disp('SSLExtension', Data) ->
    enc_SSLExtension(Data);
encode_disp('SSLExtensions', Data) ->
    enc_SSLExtensions(Data);
encode_disp('SSLExtensionAttribute', Data) ->
    enc_SSLExtensionAttribute(Data);
encode_disp('SSLExtensionAttributes', Data) ->
    enc_SSLExtensionAttributes(Data);
encode_disp('SSLCharacteristic-two', Data) ->
    'enc_SSLCharacteristic-two'(Data);
encode_disp('SSLFieldID', Data) -> enc_SSLFieldID(Data);
encode_disp('KEA-PublicKey', Data) ->
    'enc_KEA-PublicKey'(Data);
encode_disp('PublicKeyAlgorithm', Data) ->
    enc_PublicKeyAlgorithm(Data);
encode_disp('SignatureAlgorithm-Any', Data) ->
    'enc_SignatureAlgorithm-Any'(Data);
encode_disp('SignatureAlgorithm', Data) ->
    enc_SignatureAlgorithm(Data);
encode_disp('SSLSubjectPublicKeyInfo-Any', Data) ->
    'enc_SSLSubjectPublicKeyInfo-Any'(Data);
encode_disp('SSLSubjectPublicKeyInfo', Data) ->
    enc_SSLSubjectPublicKeyInfo(Data);
encode_disp('SSLAttributeTypeAndValue', Data) ->
    enc_SSLAttributeTypeAndValue(Data);
encode_disp('SSLTBSCertificate', Data) ->
    enc_SSLTBSCertificate(Data);
encode_disp('SSLCertificate', Data) ->
    enc_SSLCertificate(Data);
encode_disp(Type, _Data) ->
    exit({error, {asn1, {undefined_type, Type}}}).

decode_disp('Curve', Data) -> dec_Curve(Data);
decode_disp('ECPVer', Data) -> dec_ECPVer(Data);
decode_disp('ECParameters', Data) ->
    dec_ECParameters(Data);
decode_disp('EcpkParameters', Data) ->
    dec_EcpkParameters(Data);
decode_disp('ECPoint', Data) -> dec_ECPoint(Data);
decode_disp('FieldElement', Data) ->
    dec_FieldElement(Data);
decode_disp('Pentanomial', Data) ->
    dec_Pentanomial(Data);
decode_disp('Trinomial', Data) -> dec_Trinomial(Data);
decode_disp('Characteristic-two', Data) ->
    'dec_Characteristic-two'(Data);
decode_disp('Prime-p', Data) -> 'dec_Prime-p'(Data);
decode_disp('ECDSA-Sig-Value', Data) ->
    'dec_ECDSA-Sig-Value'(Data);
decode_disp('FieldID', Data) -> dec_FieldID(Data);
decode_disp('KEA-Parms-Id', Data) ->
    'dec_KEA-Parms-Id'(Data);
decode_disp('ValidationParms', Data) ->
    dec_ValidationParms(Data);
decode_disp('DomainParameters', Data) ->
    dec_DomainParameters(Data);
decode_disp('DHPublicKey', Data) ->
    dec_DHPublicKey(Data);
decode_disp('RSAPublicKey', Data) ->
    dec_RSAPublicKey(Data);
decode_disp('Dss-Sig-Value', Data) ->
    'dec_Dss-Sig-Value'(Data);
decode_disp('Dss-Parms', Data) -> 'dec_Dss-Parms'(Data);
decode_disp('DSAPublicKey', Data) ->
    dec_DSAPublicKey(Data);
decode_disp('ProxyInfo', Data) -> dec_ProxyInfo(Data);
decode_disp('ACClearAttrs', Data) ->
    dec_ACClearAttrs(Data);
decode_disp('AttrSpec', Data) -> dec_AttrSpec(Data);
decode_disp('AAControls', Data) -> dec_AAControls(Data);
decode_disp('SecurityCategory', Data) ->
    dec_SecurityCategory(Data);
decode_disp('ClassList', Data) -> dec_ClassList(Data);
decode_disp('Clearance', Data) -> dec_Clearance(Data);
decode_disp('RoleSyntax', Data) -> dec_RoleSyntax(Data);
decode_disp('SvceAuthInfo', Data) ->
    dec_SvceAuthInfo(Data);
decode_disp('IetfAttrSyntax', Data) ->
    dec_IetfAttrSyntax(Data);
decode_disp('TargetCert', Data) -> dec_TargetCert(Data);
decode_disp('Target', Data) -> dec_Target(Data);
decode_disp('Targets', Data) -> dec_Targets(Data);
decode_disp('AttCertValidityPeriod', Data) ->
    dec_AttCertValidityPeriod(Data);
decode_disp('IssuerSerial', Data) ->
    dec_IssuerSerial(Data);
decode_disp('V2Form', Data) -> dec_V2Form(Data);
decode_disp('AttCertIssuer', Data) ->
    dec_AttCertIssuer(Data);
decode_disp('ObjectDigestInfo', Data) ->
    dec_ObjectDigestInfo(Data);
decode_disp('Holder', Data) -> dec_Holder(Data);
decode_disp('AttCertVersion', Data) ->
    dec_AttCertVersion(Data);
decode_disp('AttributeCertificateInfo', Data) ->
    dec_AttributeCertificateInfo(Data);
decode_disp('AttributeCertificate', Data) ->
    dec_AttributeCertificate(Data);
decode_disp('InvalidityDate', Data) ->
    dec_InvalidityDate(Data);
decode_disp('HoldInstructionCode', Data) ->
    dec_HoldInstructionCode(Data);
decode_disp('CertificateIssuer', Data) ->
    dec_CertificateIssuer(Data);
decode_disp('CRLReason', Data) -> dec_CRLReason(Data);
decode_disp('BaseCRLNumber', Data) ->
    dec_BaseCRLNumber(Data);
decode_disp('IssuingDistributionPoint', Data) ->
    dec_IssuingDistributionPoint(Data);
decode_disp('CRLNumber', Data) -> dec_CRLNumber(Data);
decode_disp('SubjectInfoAccessSyntax', Data) ->
    dec_SubjectInfoAccessSyntax(Data);
decode_disp('AccessDescription', Data) ->
    dec_AccessDescription(Data);
decode_disp('AuthorityInfoAccessSyntax', Data) ->
    dec_AuthorityInfoAccessSyntax(Data);
decode_disp('FreshestCRL', Data) ->
    dec_FreshestCRL(Data);
decode_disp('InhibitAnyPolicy', Data) ->
    dec_InhibitAnyPolicy(Data);
decode_disp('KeyPurposeId', Data) ->
    dec_KeyPurposeId(Data);
decode_disp('ExtKeyUsageSyntax', Data) ->
    dec_ExtKeyUsageSyntax(Data);
decode_disp('ReasonFlags', Data) ->
    dec_ReasonFlags(Data);
decode_disp('DistributionPointName', Data) ->
    dec_DistributionPointName(Data);
decode_disp('DistributionPoint', Data) ->
    dec_DistributionPoint(Data);
decode_disp('CRLDistributionPoints', Data) ->
    dec_CRLDistributionPoints(Data);
decode_disp('SkipCerts', Data) -> dec_SkipCerts(Data);
decode_disp('PolicyConstraints', Data) ->
    dec_PolicyConstraints(Data);
decode_disp('BaseDistance', Data) ->
    dec_BaseDistance(Data);
decode_disp('GeneralSubtree', Data) ->
    dec_GeneralSubtree(Data);
decode_disp('GeneralSubtrees', Data) ->
    dec_GeneralSubtrees(Data);
decode_disp('NameConstraints', Data) ->
    dec_NameConstraints(Data);
decode_disp('BasicConstraints', Data) ->
    dec_BasicConstraints(Data);
decode_disp('SubjectDirectoryAttributes', Data) ->
    dec_SubjectDirectoryAttributes(Data);
decode_disp('IssuerAltName', Data) ->
    dec_IssuerAltName(Data);
decode_disp('EDIPartyName', Data) ->
    dec_EDIPartyName(Data);
decode_disp('AnotherName', Data) ->
    dec_AnotherName(Data);
decode_disp('GeneralName', Data) ->
    dec_GeneralName(Data);
decode_disp('GeneralNames', Data) ->
    dec_GeneralNames(Data);
decode_disp('SubjectAltName', Data) ->
    dec_SubjectAltName(Data);
decode_disp('PolicyMappings', Data) ->
    dec_PolicyMappings(Data);
decode_disp('DisplayText', Data) ->
    dec_DisplayText(Data);
decode_disp('NoticeReference', Data) ->
    dec_NoticeReference(Data);
decode_disp('UserNotice', Data) -> dec_UserNotice(Data);
decode_disp('CPSuri', Data) -> dec_CPSuri(Data);
decode_disp('PolicyQualifierId', Data) ->
    dec_PolicyQualifierId(Data);
decode_disp('PolicyQualifierInfo', Data) ->
    dec_PolicyQualifierInfo(Data);
decode_disp('CertPolicyId', Data) ->
    dec_CertPolicyId(Data);
decode_disp('PolicyInformation', Data) ->
    dec_PolicyInformation(Data);
decode_disp('CertificatePolicies', Data) ->
    dec_CertificatePolicies(Data);
decode_disp('PrivateKeyUsagePeriod', Data) ->
    dec_PrivateKeyUsagePeriod(Data);
decode_disp('KeyUsage', Data) -> dec_KeyUsage(Data);
decode_disp('SubjectKeyIdentifier', Data) ->
    dec_SubjectKeyIdentifier(Data);
decode_disp('KeyIdentifier', Data) ->
    dec_KeyIdentifier(Data);
decode_disp('AuthorityKeyIdentifier', Data) ->
    dec_AuthorityKeyIdentifier(Data);
decode_disp('DigestAlgorithmIdentifier', Data) ->
    dec_DigestAlgorithmIdentifier(Data);
decode_disp('DigestInfo', Data) -> dec_DigestInfo(Data);
decode_disp('OtherPrimeInfo', Data) ->
    dec_OtherPrimeInfo(Data);
decode_disp('OtherPrimeInfos', Data) ->
    dec_OtherPrimeInfos(Data);
decode_disp('VersionPKCS-1', Data) ->
    'dec_VersionPKCS-1'(Data);
decode_disp('RSAPrivateKey', Data) ->
    dec_RSAPrivateKey(Data);
decode_disp('TeletexDomainDefinedAttribute', Data) ->
    dec_TeletexDomainDefinedAttribute(Data);
decode_disp('TeletexDomainDefinedAttributes', Data) ->
    dec_TeletexDomainDefinedAttributes(Data);
decode_disp('TerminalType', Data) ->
    dec_TerminalType(Data);
decode_disp('PresentationAddress', Data) ->
    dec_PresentationAddress(Data);
decode_disp('ExtendedNetworkAddress', Data) ->
    dec_ExtendedNetworkAddress(Data);
decode_disp('PDSParameter', Data) ->
    dec_PDSParameter(Data);
decode_disp('LocalPostalAttributes', Data) ->
    dec_LocalPostalAttributes(Data);
decode_disp('UniquePostalName', Data) ->
    dec_UniquePostalName(Data);
decode_disp('PosteRestanteAddress', Data) ->
    dec_PosteRestanteAddress(Data);
decode_disp('PostOfficeBoxAddress', Data) ->
    dec_PostOfficeBoxAddress(Data);
decode_disp('StreetAddress', Data) ->
    dec_StreetAddress(Data);
decode_disp('UnformattedPostalAddress', Data) ->
    dec_UnformattedPostalAddress(Data);
decode_disp('ExtensionPhysicalDeliveryAddressComponents',
	    Data) ->
    dec_ExtensionPhysicalDeliveryAddressComponents(Data);
decode_disp('PhysicalDeliveryOrganizationName', Data) ->
    dec_PhysicalDeliveryOrganizationName(Data);
decode_disp('PhysicalDeliveryPersonalName', Data) ->
    dec_PhysicalDeliveryPersonalName(Data);
decode_disp('ExtensionORAddressComponents', Data) ->
    dec_ExtensionORAddressComponents(Data);
decode_disp('PhysicalDeliveryOfficeNumber', Data) ->
    dec_PhysicalDeliveryOfficeNumber(Data);
decode_disp('PhysicalDeliveryOfficeName', Data) ->
    dec_PhysicalDeliveryOfficeName(Data);
decode_disp('PostalCode', Data) -> dec_PostalCode(Data);
decode_disp('PhysicalDeliveryCountryName', Data) ->
    dec_PhysicalDeliveryCountryName(Data);
decode_disp('PDSName', Data) -> dec_PDSName(Data);
decode_disp('TeletexOrganizationalUnitName', Data) ->
    dec_TeletexOrganizationalUnitName(Data);
decode_disp('TeletexOrganizationalUnitNames', Data) ->
    dec_TeletexOrganizationalUnitNames(Data);
decode_disp('TeletexPersonalName', Data) ->
    dec_TeletexPersonalName(Data);
decode_disp('TeletexOrganizationName', Data) ->
    dec_TeletexOrganizationName(Data);
decode_disp('TeletexCommonName', Data) ->
    dec_TeletexCommonName(Data);
decode_disp('CommonName', Data) -> dec_CommonName(Data);
decode_disp('ExtensionAttribute', Data) ->
    dec_ExtensionAttribute(Data);
decode_disp('ExtensionAttributes', Data) ->
    dec_ExtensionAttributes(Data);
decode_disp('BuiltInDomainDefinedAttribute', Data) ->
    dec_BuiltInDomainDefinedAttribute(Data);
decode_disp('BuiltInDomainDefinedAttributes', Data) ->
    dec_BuiltInDomainDefinedAttributes(Data);
decode_disp('OrganizationalUnitName', Data) ->
    dec_OrganizationalUnitName(Data);
decode_disp('OrganizationalUnitNames', Data) ->
    dec_OrganizationalUnitNames(Data);
decode_disp('PersonalName', Data) ->
    dec_PersonalName(Data);
decode_disp('NumericUserIdentifier', Data) ->
    dec_NumericUserIdentifier(Data);
decode_disp('OrganizationName', Data) ->
    dec_OrganizationName(Data);
decode_disp('PrivateDomainName', Data) ->
    dec_PrivateDomainName(Data);
decode_disp('TerminalIdentifier', Data) ->
    dec_TerminalIdentifier(Data);
decode_disp('X121Address', Data) ->
    dec_X121Address(Data);
decode_disp('NetworkAddress', Data) ->
    dec_NetworkAddress(Data);
decode_disp('AdministrationDomainName', Data) ->
    dec_AdministrationDomainName(Data);
decode_disp('CountryName', Data) ->
    dec_CountryName(Data);
decode_disp('BuiltInStandardAttributes', Data) ->
    dec_BuiltInStandardAttributes(Data);
decode_disp('ORAddress', Data) -> dec_ORAddress(Data);
decode_disp('AlgorithmIdentifier', Data) ->
    dec_AlgorithmIdentifier(Data);
decode_disp('TBSCertList', Data) ->
    dec_TBSCertList(Data);
decode_disp('CertificateList', Data) ->
    dec_CertificateList(Data);
decode_disp('Extension', Data) -> dec_Extension(Data);
decode_disp('Extensions', Data) -> dec_Extensions(Data);
decode_disp('SubjectPublicKeyInfo', Data) ->
    dec_SubjectPublicKeyInfo(Data);
decode_disp('UniqueIdentifier', Data) ->
    dec_UniqueIdentifier(Data);
decode_disp('Time', Data) -> dec_Time(Data);
decode_disp('Validity', Data) -> dec_Validity(Data);
decode_disp('CertificateSerialNumber', Data) ->
    dec_CertificateSerialNumber(Data);
decode_disp('VersionPKIX1Explicit88', Data) ->
    dec_VersionPKIX1Explicit88(Data);
decode_disp('TBSCertificate', Data) ->
    dec_TBSCertificate(Data);
decode_disp('Certificate', Data) ->
    dec_Certificate(Data);
decode_disp('DirectoryString', Data) ->
    dec_DirectoryString(Data);
decode_disp('RelativeDistinguishedName', Data) ->
    dec_RelativeDistinguishedName(Data);
decode_disp('DistinguishedName', Data) ->
    dec_DistinguishedName(Data);
decode_disp('RDNSequence', Data) ->
    dec_RDNSequence(Data);
decode_disp('Name', Data) -> dec_Name(Data);
decode_disp('EmailAddress', Data) ->
    dec_EmailAddress(Data);
decode_disp('DomainComponent', Data) ->
    dec_DomainComponent(Data);
decode_disp('X520Pseudonym', Data) ->
    dec_X520Pseudonym(Data);
decode_disp('X520SerialNumber', Data) ->
    dec_X520SerialNumber(Data);
decode_disp('X520countryName', Data) ->
    dec_X520countryName(Data);
decode_disp('X520dnQualifier', Data) ->
    dec_X520dnQualifier(Data);
decode_disp('X520Title', Data) -> dec_X520Title(Data);
decode_disp('X520OrganizationalUnitName', Data) ->
    dec_X520OrganizationalUnitName(Data);
decode_disp('X520OrganizationName', Data) ->
    dec_X520OrganizationName(Data);
decode_disp('X520StateOrProvinceName', Data) ->
    dec_X520StateOrProvinceName(Data);
decode_disp('X520LocalityName', Data) ->
    dec_X520LocalityName(Data);
decode_disp('X520CommonName', Data) ->
    dec_X520CommonName(Data);
decode_disp('X520name', Data) -> dec_X520name(Data);
decode_disp('AttributeTypeAndValue', Data) ->
    dec_AttributeTypeAndValue(Data);
decode_disp('AttributeValue', Data) ->
    dec_AttributeValue(Data);
decode_disp('AttributeType', Data) ->
    dec_AttributeType(Data);
decode_disp('Attribute', Data) -> dec_Attribute(Data);
decode_disp('Extension-Any', Data) ->
    'dec_Extension-Any'(Data);
decode_disp('Any', Data) -> dec_Any(Data);
decode_disp('Boolean', Data) -> dec_Boolean(Data);
decode_disp('ObjId', Data) -> dec_ObjId(Data);
decode_disp('SSLExtension', Data) ->
    dec_SSLExtension(Data);
decode_disp('SSLExtensions', Data) ->
    dec_SSLExtensions(Data);
decode_disp('SSLExtensionAttribute', Data) ->
    dec_SSLExtensionAttribute(Data);
decode_disp('SSLExtensionAttributes', Data) ->
    dec_SSLExtensionAttributes(Data);
decode_disp('SSLCharacteristic-two', Data) ->
    'dec_SSLCharacteristic-two'(Data);
decode_disp('SSLFieldID', Data) -> dec_SSLFieldID(Data);
decode_disp('KEA-PublicKey', Data) ->
    'dec_KEA-PublicKey'(Data);
decode_disp('PublicKeyAlgorithm', Data) ->
    dec_PublicKeyAlgorithm(Data);
decode_disp('SignatureAlgorithm-Any', Data) ->
    'dec_SignatureAlgorithm-Any'(Data);
decode_disp('SignatureAlgorithm', Data) ->
    dec_SignatureAlgorithm(Data);
decode_disp('SSLSubjectPublicKeyInfo-Any', Data) ->
    'dec_SSLSubjectPublicKeyInfo-Any'(Data);
decode_disp('SSLSubjectPublicKeyInfo', Data) ->
    dec_SSLSubjectPublicKeyInfo(Data);
decode_disp('SSLAttributeTypeAndValue', Data) ->
    dec_SSLAttributeTypeAndValue(Data);
decode_disp('SSLTBSCertificate', Data) ->
    dec_SSLTBSCertificate(Data);
decode_disp('SSLCertificate', Data) ->
    dec_SSLCertificate(Data);
decode_disp(Type, _Data) ->
    exit({error, {asn1, {undefined_type, Type}}}).

decode_partial_inc_disp('Certificate', Data) ->
    'dec-inc-Certificate'(Data);
decode_partial_inc_disp(Type, _Data) ->
    exit({error, {asn1, {undefined_type, Type}}}).

info() ->
    case module_info() of
      MI when is_list(MI) ->
	  case lists:keysearch(attributes, 1, MI) of
	    {value, {_, Attributes}} when is_list(Attributes) ->
		case lists:keysearch(asn1_info, 1, Attributes) of
		  {value, {_, Info}} when is_list(Info) -> Info;
		  _ -> []
		end;
	    _ -> []
	  end
    end.

%%================================
%%  Curve
%%================================
enc_Curve(Val) -> enc_Curve(Val, [<<48>>]).

enc_Curve(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute a(1) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_octet_string([], Cindex1,
					       [<<4>>]),
    %%-------------------------------------------------
    %% attribute b(2) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_octet_string([], Cindex2,
					       [<<4>>]),
    %%-------------------------------------------------
    %% attribute seed(3) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex3, [], [<<3>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Curve(Tlv) -> dec_Curve(Tlv, [16]).

dec_Curve(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute a(1) with type OCTET STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_octet_string(V1, [], [4]),
    %%-------------------------------------------------
    %% attribute b(2) with type OCTET STRING
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_octet_string(V2, [], [4]),
    %%-------------------------------------------------
    %% attribute seed(3) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{3, V3} | TempTlv4] ->
			  {decode_compact_bit_string(V3, [], [], []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Curve', Term1, Term2, Term3}.

%%================================
%%  ECPVer
%%================================
enc_ECPVer(Val) -> enc_ECPVer(Val, [<<2>>]).

enc_ECPVer({'ECPVer', Val}, TagIn) ->
    enc_ECPVer(Val, TagIn);
enc_ECPVer(Val, TagIn) ->
    encode_integer([], Val, [{ecpVer1, 1}], TagIn).

dec_ECPVer(Tlv) -> dec_ECPVer(Tlv, [2]).

dec_ECPVer(Tlv, TagIn) ->
    decode_integer(Tlv, [], [{ecpVer1, 1}], TagIn).

%%================================
%%  ECParameters
%%================================
enc_ECParameters(Val) ->
    enc_ECParameters(Val, [<<48>>]).

enc_ECParameters(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [{ecpVer1, 1}], [<<2>>]),
    %%-------------------------------------------------
    %% attribute fieldID(2)   External OTP-PKIX:FieldID
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_FieldID(Cindex2, [<<48>>]),
    %%-------------------------------------------------
    %% attribute curve(3)   External OTP-PKIX:Curve
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_Curve(Cindex3, [<<48>>]),
    %%-------------------------------------------------
    %% attribute base(4) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = encode_octet_string([], Cindex4,
					       [<<4>>]),
    %%-------------------------------------------------
    %% attribute order(5) with type INTEGER
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = encode_integer([], Cindex5,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute cofactor(6) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = case Cindex6 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex6, [<<2>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ECParameters(Tlv) -> dec_ECParameters(Tlv, [16]).

dec_ECParameters(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [{ecpVer1, 1}], [2]),
    %%-------------------------------------------------
    %% attribute fieldID(2)   External OTP-PKIX:FieldID
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_FieldID(V2, [16]),
    %%-------------------------------------------------
    %% attribute curve(3)   External OTP-PKIX:Curve
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_Curve(V3, [16]),
    %%-------------------------------------------------
    %% attribute base(4) with type OCTET STRING
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = decode_octet_string(V4, [], [4]),
    %%-------------------------------------------------
    %% attribute order(5) with type INTEGER
    %%-------------------------------------------------
    [V5 | Tlv6] = Tlv5,
    Term5 = decode_integer(V5, [], [2]),
    %%-------------------------------------------------
    %% attribute cofactor(6) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term6, Tlv7} = case Tlv6 of
		      [{2, V6} | TempTlv7] ->
			  {decode_integer(V6, [], []), TempTlv7};
		      _ -> {asn1_NOVALUE, Tlv6}
		    end,
    case Tlv7 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv7}}}) % extra fields not allowed
    end,
    {'ECParameters', Term1, Term2, Term3, Term4, Term5,
     Term6}.

%%================================
%%  EcpkParameters
%%================================
enc_EcpkParameters(Val) -> enc_EcpkParameters(Val, []).

enc_EcpkParameters({'EcpkParameters', Val}, TagIn) ->
    enc_EcpkParameters(Val, TagIn);
enc_EcpkParameters(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   ecParameters ->
			       enc_ECParameters(element(2, Val), [<<48>>]);
			   namedCurve ->
			       encode_object_identifier(element(2, Val),
							[<<6>>]);
			   implicitlyCA ->
			       encode_null(element(2, Val), [<<5>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_EcpkParameters(Tlv) -> dec_EcpkParameters(Tlv, []).

dec_EcpkParameters(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'ecParameters'
      {16, V1} -> {ecParameters, dec_ECParameters(V1, [])};
      %% 'namedCurve'
      {6, V1} ->
	  {namedCurve, decode_object_identifier(V1, [])};
      %% 'implicitlyCA'
      {5, V1} -> {implicitlyCA, decode_null(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  ECPoint
%%================================
enc_ECPoint(Val) -> enc_ECPoint(Val, [<<4>>]).

enc_ECPoint({'ECPoint', Val}, TagIn) ->
    enc_ECPoint(Val, TagIn);
enc_ECPoint(Val, TagIn) ->
    encode_octet_string([], Val, TagIn).

dec_ECPoint(Tlv) -> dec_ECPoint(Tlv, [4]).

dec_ECPoint(Tlv, TagIn) ->
    decode_octet_string(Tlv, [], TagIn).

%%================================
%%  FieldElement
%%================================
enc_FieldElement(Val) -> enc_FieldElement(Val, [<<4>>]).

enc_FieldElement({'FieldElement', Val}, TagIn) ->
    enc_FieldElement(Val, TagIn);
enc_FieldElement(Val, TagIn) ->
    encode_octet_string([], Val, TagIn).

dec_FieldElement(Tlv) -> dec_FieldElement(Tlv, [4]).

dec_FieldElement(Tlv, TagIn) ->
    decode_octet_string(Tlv, [], TagIn).

%%================================
%%  Pentanomial
%%================================
enc_Pentanomial(Val) -> enc_Pentanomial(Val, [<<48>>]).

enc_Pentanomial(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute k1(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute k2(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute k3(3) with type INTEGER
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_integer([], Cindex3,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Pentanomial(Tlv) -> dec_Pentanomial(Tlv, [16]).

dec_Pentanomial(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute k1(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute k2(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute k3(3) with type INTEGER
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_integer(V3, [], [2]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Pentanomial', Term1, Term2, Term3}.

%%================================
%%  Trinomial
%%================================
enc_Trinomial(Val) -> enc_Trinomial(Val, [<<2>>]).

enc_Trinomial({'Trinomial', Val}, TagIn) ->
    enc_Trinomial(Val, TagIn);
enc_Trinomial(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_Trinomial(Tlv) -> dec_Trinomial(Tlv, [2]).

dec_Trinomial(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  Characteristic-two
%%================================
'enc_Characteristic-two'(Val) ->
    'enc_Characteristic-two'(Val, [<<48>>]).

'enc_Characteristic-two'(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute m(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute basis(2) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_object_identifier(Cindex2,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(3) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_open_type(Cindex3, []),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_Characteristic-two'(Tlv) ->
    'dec_Characteristic-two'(Tlv, [16]).

'dec_Characteristic-two'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute m(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute basis(2) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_object_identifier(V2, [6]),
    %%-------------------------------------------------
    %% attribute parameters(3) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_open_type_as_binary(V3, []),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Characteristic-two', Term1, Term2, Term3}.

%%================================
%%  Prime-p
%%================================
'enc_Prime-p'(Val) -> 'enc_Prime-p'(Val, [<<2>>]).

'enc_Prime-p'({'Prime-p', Val}, TagIn) ->
    'enc_Prime-p'(Val, TagIn);
'enc_Prime-p'(Val, TagIn) ->
    encode_integer([], Val, TagIn).

'dec_Prime-p'(Tlv) -> 'dec_Prime-p'(Tlv, [2]).

'dec_Prime-p'(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  ECDSA-Sig-Value
%%================================
'enc_ECDSA-Sig-Value'(Val) ->
    'enc_ECDSA-Sig-Value'(Val, [<<48>>]).

'enc_ECDSA-Sig-Value'(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute r(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute s(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_ECDSA-Sig-Value'(Tlv) ->
    'dec_ECDSA-Sig-Value'(Tlv, [16]).

'dec_ECDSA-Sig-Value'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute r(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute s(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'ECDSA-Sig-Value', Term1, Term2}.

%%================================
%%  FieldID
%%================================
enc_FieldID(Val) -> enc_FieldID(Val, [<<48>>]).

enc_FieldID(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute fieldType(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_FieldID(Tlv) -> dec_FieldID(Tlv, [16]).

dec_FieldID(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute fieldType(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'FieldID', Term1, Term2}.

%%================================
%%  KEA-Parms-Id
%%================================
'enc_KEA-Parms-Id'(Val) ->
    'enc_KEA-Parms-Id'(Val, [<<4>>]).

'enc_KEA-Parms-Id'({'KEA-Parms-Id', Val}, TagIn) ->
    'enc_KEA-Parms-Id'(Val, TagIn);
'enc_KEA-Parms-Id'(Val, TagIn) ->
    encode_octet_string([], Val, TagIn).

'dec_KEA-Parms-Id'(Tlv) -> 'dec_KEA-Parms-Id'(Tlv, [4]).

'dec_KEA-Parms-Id'(Tlv, TagIn) ->
    decode_octet_string(Tlv, [], TagIn).

%%================================
%%  ValidationParms
%%================================
enc_ValidationParms(Val) ->
    enc_ValidationParms(Val, [<<48>>]).

enc_ValidationParms(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute seed(1) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_bit_string([], Cindex1,
					     [], [<<3>>]),
    %%-------------------------------------------------
    %% attribute pgenCounter(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ValidationParms(Tlv) ->
    dec_ValidationParms(Tlv, [16]).

dec_ValidationParms(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute seed(1) with type BIT STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_compact_bit_string(V1, [], [], [3]),
    %%-------------------------------------------------
    %% attribute pgenCounter(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'ValidationParms', Term1, Term2}.

%%================================
%%  DomainParameters
%%================================
enc_DomainParameters(Val) ->
    enc_DomainParameters(Val, [<<48>>]).

enc_DomainParameters(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5} = Val,
    %%-------------------------------------------------
    %% attribute p(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute g(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute q(3) with type INTEGER
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_integer([], Cindex3,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute j(4) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case Cindex4 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex4, [<<2>>])
			   end,
    %%-------------------------------------------------
    %% attribute validationParms(5)   External OTP-PKIX:ValidationParms OPTIONAL
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = case Cindex5 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_ValidationParms(Cindex5, [<<48>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_DomainParameters(Tlv) ->
    dec_DomainParameters(Tlv, [16]).

dec_DomainParameters(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute p(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute g(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute q(3) with type INTEGER
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_integer(V3, [], [2]),
    %%-------------------------------------------------
    %% attribute j(4) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term4, Tlv5} = case Tlv4 of
		      [{2, V4} | TempTlv5] ->
			  {decode_integer(V4, [], []), TempTlv5};
		      _ -> {asn1_NOVALUE, Tlv4}
		    end,
    %%-------------------------------------------------
    %% attribute validationParms(5)   External OTP-PKIX:ValidationParms OPTIONAL
    %%-------------------------------------------------
    {Term5, Tlv6} = case Tlv5 of
		      [{16, V5} | TempTlv6] ->
			  {dec_ValidationParms(V5, []), TempTlv6};
		      _ -> {asn1_NOVALUE, Tlv5}
		    end,
    case Tlv6 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv6}}}) % extra fields not allowed
    end,
    {'DomainParameters', Term1, Term2, Term3, Term4, Term5}.

%%================================
%%  DHPublicKey
%%================================
enc_DHPublicKey(Val) -> enc_DHPublicKey(Val, [<<2>>]).

enc_DHPublicKey({'DHPublicKey', Val}, TagIn) ->
    enc_DHPublicKey(Val, TagIn);
enc_DHPublicKey(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_DHPublicKey(Tlv) -> dec_DHPublicKey(Tlv, [2]).

dec_DHPublicKey(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  RSAPublicKey
%%================================
enc_RSAPublicKey(Val) ->
    enc_RSAPublicKey(Val, [<<48>>]).

enc_RSAPublicKey(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute modulus(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute publicExponent(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_RSAPublicKey(Tlv) -> dec_RSAPublicKey(Tlv, [16]).

dec_RSAPublicKey(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute modulus(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute publicExponent(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'RSAPublicKey', Term1, Term2}.

%%================================
%%  Dss-Sig-Value
%%================================
'enc_Dss-Sig-Value'(Val) ->
    'enc_Dss-Sig-Value'(Val, [<<48>>]).

'enc_Dss-Sig-Value'(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute r(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute s(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_Dss-Sig-Value'(Tlv) ->
    'dec_Dss-Sig-Value'(Tlv, [16]).

'dec_Dss-Sig-Value'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute r(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute s(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'Dss-Sig-Value', Term1, Term2}.

%%================================
%%  Dss-Parms
%%================================
'enc_Dss-Parms'(Val) -> 'enc_Dss-Parms'(Val, [<<48>>]).

'enc_Dss-Parms'(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute p(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute q(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute g(3) with type INTEGER
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_integer([], Cindex3,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_Dss-Parms'(Tlv) -> 'dec_Dss-Parms'(Tlv, [16]).

'dec_Dss-Parms'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute p(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute q(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute g(3) with type INTEGER
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_integer(V3, [], [2]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Dss-Parms', Term1, Term2, Term3}.

%%================================
%%  DSAPublicKey
%%================================
enc_DSAPublicKey(Val) -> enc_DSAPublicKey(Val, [<<2>>]).

enc_DSAPublicKey({'DSAPublicKey', Val}, TagIn) ->
    enc_DSAPublicKey(Val, TagIn);
enc_DSAPublicKey(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_DSAPublicKey(Tlv) -> dec_DSAPublicKey(Tlv, [2]).

dec_DSAPublicKey(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  ProxyInfo
%%================================
enc_ProxyInfo(Val) -> enc_ProxyInfo(Val, [<<48>>]).

enc_ProxyInfo({'ProxyInfo', Val}, TagIn) ->
    enc_ProxyInfo(Val, TagIn);
enc_ProxyInfo(Val, TagIn) ->
    {EncBytes, EncLen} = enc_ProxyInfo_components(Val, [],
						  0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_ProxyInfo_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_ProxyInfo_components([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_Targets(H, [<<48>>]),
    enc_ProxyInfo_components(T, [EncBytes | AccBytes],
			     AccLen + EncLen).

dec_ProxyInfo(Tlv) -> dec_ProxyInfo(Tlv, [16]).

dec_ProxyInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Targets(V1, [16]) || V1 <- Tlv1].

%%================================
%%  ACClearAttrs
%%================================
enc_ACClearAttrs(Val) ->
    enc_ACClearAttrs(Val, [<<48>>]).

enc_ACClearAttrs(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute acIssuer(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_GeneralName(Cindex1, []),
    %%-------------------------------------------------
    %% attribute acSerial(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute attrs(3) with type SEQUENCE OF
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_ACClearAttrs_attrs(Cindex3,
						  [<<48>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  ACClearAttrs_attrs
%%================================

enc_ACClearAttrs_attrs({'ACClearAttrs_attrs', Val},
		       TagIn) ->
    enc_ACClearAttrs_attrs(Val, TagIn);
enc_ACClearAttrs_attrs(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_ACClearAttrs_attrs_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_ACClearAttrs_attrs_components([], AccBytes,
				  AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_ACClearAttrs_attrs_components([H | T], AccBytes,
				  AccLen) ->
    {EncBytes, EncLen} = enc_Attribute(H, [<<48>>]),
    enc_ACClearAttrs_attrs_components(T,
				      [EncBytes | AccBytes], AccLen + EncLen).

dec_ACClearAttrs_attrs(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Attribute(V1, [16]) || V1 <- Tlv1].

dec_ACClearAttrs(Tlv) -> dec_ACClearAttrs(Tlv, [16]).

dec_ACClearAttrs(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute acIssuer(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_GeneralName(V1, []),
    %%-------------------------------------------------
    %% attribute acSerial(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute attrs(3) with type SEQUENCE OF
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_ACClearAttrs_attrs(V3, [16]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'ACClearAttrs', Term1, Term2, Term3}.

%%================================
%%  AttrSpec
%%================================
enc_AttrSpec(Val) -> enc_AttrSpec(Val, [<<48>>]).

enc_AttrSpec({'AttrSpec', Val}, TagIn) ->
    enc_AttrSpec(Val, TagIn);
enc_AttrSpec(Val, TagIn) ->
    {EncBytes, EncLen} = enc_AttrSpec_components(Val, [],
						 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_AttrSpec_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_AttrSpec_components([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_object_identifier(H,
						  [<<6>>]),
    enc_AttrSpec_components(T, [EncBytes | AccBytes],
			    AccLen + EncLen).

dec_AttrSpec(Tlv) -> dec_AttrSpec(Tlv, [16]).

dec_AttrSpec(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_object_identifier(V1, [6]) || V1 <- Tlv1].

%%================================
%%  AAControls
%%================================
enc_AAControls(Val) -> enc_AAControls(Val, [<<48>>]).

enc_AAControls(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4} = Val,
    %%-------------------------------------------------
    %% attribute pathLenConstraint(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex1, [<<2>>])
			   end,
    %%-------------------------------------------------
    %% attribute permittedAttrs(2)   External OTP-PKIX:AttrSpec OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_AttrSpec(Cindex2, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute excludedAttrs(3)   External OTP-PKIX:AttrSpec OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_AttrSpec(Cindex3, [<<161>>])
			   end,
    %%-------------------------------------------------
    %% attribute permitUnSpecified(4) with type BOOLEAN DEFAULT = true
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case catch check_bool(true,
						 Cindex4)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex4, [<<1>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AAControls(Tlv) -> dec_AAControls(Tlv, [16]).

dec_AAControls(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute pathLenConstraint(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{2, V1} | TempTlv2] ->
			  {decode_integer(V1, {0, 'MAX'}, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute permittedAttrs(2)   External OTP-PKIX:AttrSpec OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131072, V2} | TempTlv3] ->
			  {dec_AttrSpec(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute excludedAttrs(3)   External OTP-PKIX:AttrSpec OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131073, V3} | TempTlv4] ->
			  {dec_AttrSpec(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute permitUnSpecified(4) with type BOOLEAN DEFAULT = true
    %%-------------------------------------------------
    {Term4, Tlv5} = case Tlv4 of
		      [{1, V4} | TempTlv5] ->
			  {decode_boolean(V4, []), TempTlv5};
		      _ -> {true, Tlv4}
		    end,
    case Tlv5 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv5}}}) % extra fields not allowed
    end,
    {'AAControls', Term1, Term2, Term3, Term4}.

%%================================
%%  SecurityCategory
%%================================
enc_SecurityCategory(Val) ->
    enc_SecurityCategory(Val, [<<48>>]).

enc_SecurityCategory(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<128>>]),
    %%-------------------------------------------------
    %% attribute value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2,
					    [<<161>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SecurityCategory(Tlv) ->
    dec_SecurityCategory(Tlv, [16]).

dec_SecurityCategory(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [131072]),
    %%-------------------------------------------------
    %% attribute value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, [131073]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SecurityCategory', Term1, Term2}.

%%================================
%%  ClassList
%%================================
enc_ClassList(Val) -> enc_ClassList(Val, [<<3>>]).

enc_ClassList({'ClassList', Val}, TagIn) ->
    enc_ClassList(Val, TagIn);
enc_ClassList(Val, TagIn) ->
    encode_bit_string([], Val,
		      [{unmarked, 0}, {unclassified, 1}, {restricted, 2},
		       {confidential, 3}, {secret, 4}, {topSecret, 5}],
		      TagIn).

dec_ClassList(Tlv) -> dec_ClassList(Tlv, [3]).

dec_ClassList(Tlv, TagIn) ->
    decode_compact_bit_string(Tlv, [],
			      [{unmarked, 0}, {unclassified, 1},
			       {restricted, 2}, {confidential, 3}, {secret, 4},
			       {topSecret, 5}],
			      TagIn).

%%================================
%%  Clearance
%%================================
enc_Clearance(Val) -> enc_Clearance(Val, [<<48>>]).

enc_Clearance(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute policyId(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<128>>]),
    %%-------------------------------------------------
    %% attribute classList(2) with type BIT STRING DEFAULT = [unclassified]
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch
				  check_bitstring([unclassified], Cindex2,
						  [{unmarked, 0},
						   {unclassified, 1},
						   {restricted, 2},
						   {confidential, 3},
						   {secret, 4}, {topSecret, 5}])
			       of
			     true -> {[], 0};
			     _ ->
				 encode_bit_string([], Cindex2,
						   [{unmarked, 0},
						    {unclassified, 1},
						    {restricted, 2},
						    {confidential, 3},
						    {secret, 4},
						    {topSecret, 5}],
						   [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute securityCategories(3) with type SET OF OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_Clearance_securityCategories(Cindex3,
								  [<<162>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  Clearance_securityCategories
%%================================

enc_Clearance_securityCategories({'Clearance_securityCategories',
				  Val},
				 TagIn) ->
    enc_Clearance_securityCategories(Val, TagIn);
enc_Clearance_securityCategories(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_Clearance_securityCategories_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_Clearance_securityCategories_components([],
					    AccBytes, AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_Clearance_securityCategories_components([H | T],
					    AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_SecurityCategory(H, [<<48>>]),
    enc_Clearance_securityCategories_components(T,
						[EncBytes | AccBytes],
						AccLen + EncLen).

dec_Clearance_securityCategories(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_SecurityCategory(V1, [16]) || V1 <- Tlv1].

dec_Clearance(Tlv) -> dec_Clearance(Tlv, [16]).

dec_Clearance(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute policyId(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [131072]),
    %%-------------------------------------------------
    %% attribute classList(2) with type BIT STRING DEFAULT = [unclassified]
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_compact_bit_string(V2, [],
						     [{unmarked, 0},
						      {unclassified, 1},
						      {restricted, 2},
						      {confidential, 3},
						      {secret, 4},
						      {topSecret, 5}],
						     []),
			   TempTlv3};
		      _ -> {[unclassified], Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute securityCategories(3) with type SET OF OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {dec_Clearance_securityCategories(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Clearance', Term1, Term2, Term3}.

%%================================
%%  RoleSyntax
%%================================
enc_RoleSyntax(Val) -> enc_RoleSyntax(Val, [<<48>>]).

enc_RoleSyntax(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute roleAuthority(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute roleName(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_GeneralName(Cindex2,
					   [<<161>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_RoleSyntax(Tlv) -> dec_RoleSyntax(Tlv, [16]).

dec_RoleSyntax(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute roleAuthority(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_GeneralNames(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute roleName(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_GeneralName(V2, [131073]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'RoleSyntax', Term1, Term2}.

%%================================
%%  SvceAuthInfo
%%================================
enc_SvceAuthInfo(Val) ->
    enc_SvceAuthInfo(Val, [<<48>>]).

enc_SvceAuthInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute service(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_GeneralName(Cindex1, []),
    %%-------------------------------------------------
    %% attribute ident(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_GeneralName(Cindex2, []),
    %%-------------------------------------------------
    %% attribute authInfo(3) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_octet_string([], Cindex3, [<<4>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SvceAuthInfo(Tlv) -> dec_SvceAuthInfo(Tlv, [16]).

dec_SvceAuthInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute service(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_GeneralName(V1, []),
    %%-------------------------------------------------
    %% attribute ident(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_GeneralName(V2, []),
    %%-------------------------------------------------
    %% attribute authInfo(3) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{4, V3} | TempTlv4] ->
			  {decode_octet_string(V3, [], []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'SvceAuthInfo', Term1, Term2, Term3}.

%%================================
%%  IetfAttrSyntax
%%================================
enc_IetfAttrSyntax(Val) ->
    enc_IetfAttrSyntax(Val, [<<48>>]).

enc_IetfAttrSyntax(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute policyAuthority(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute values(2) with type SEQUENCE OF
    %%-------------------------------------------------
    {EncBytes2, EncLen2} =
	enc_IetfAttrSyntax_values(Cindex2, [<<48>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  IetfAttrSyntax_values
%%================================

enc_IetfAttrSyntax_values({'IetfAttrSyntax_values',
			   Val},
			  TagIn) ->
    enc_IetfAttrSyntax_values(Val, TagIn);
enc_IetfAttrSyntax_values(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_IetfAttrSyntax_values_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_IetfAttrSyntax_values_components([], AccBytes,
				     AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_IetfAttrSyntax_values_components([H | T], AccBytes,
				     AccLen) ->
    {EncBytes, EncLen} = enc_IetfAttrSyntax_values_SEQOF(H,
							 []),
    enc_IetfAttrSyntax_values_components(T,
					 [EncBytes | AccBytes],
					 AccLen + EncLen).

%%================================
%%  IetfAttrSyntax_values_SEQOF
%%================================

enc_IetfAttrSyntax_values_SEQOF({'IetfAttrSyntax_values_SEQOF',
				 Val},
				TagIn) ->
    enc_IetfAttrSyntax_values_SEQOF(Val, TagIn);
enc_IetfAttrSyntax_values_SEQOF(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   octets ->
			       encode_octet_string([], element(2, Val),
						   [<<4>>]);
			   oid ->
			       encode_object_identifier(element(2, Val),
							[<<6>>]);
			   string ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_IetfAttrSyntax_values_SEQOF(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'octets'
      {4, V1} -> {octets, decode_octet_string(V1, [], [])};
      %% 'oid'
      {6, V1} -> {oid, decode_object_identifier(V1, [])};
      %% 'string'
      {12, V1} -> {string, decode_UTF8_string(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

dec_IetfAttrSyntax_values(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_IetfAttrSyntax_values_SEQOF(V1, []) || V1 <- Tlv1].

dec_IetfAttrSyntax(Tlv) ->
    dec_IetfAttrSyntax(Tlv, [16]).

dec_IetfAttrSyntax(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute policyAuthority(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_GeneralNames(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute values(2) with type SEQUENCE OF
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_IetfAttrSyntax_values(V2, [16]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'IetfAttrSyntax', Term1, Term2}.

%%================================
%%  TargetCert
%%================================
enc_TargetCert(Val) -> enc_TargetCert(Val, [<<48>>]).

enc_TargetCert(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute targetCertificate(1)   External OTP-PKIX:IssuerSerial
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_IssuerSerial(Cindex1,
					    [<<48>>]),
    %%-------------------------------------------------
    %% attribute targetName(2)   External OTP-PKIX:GeneralName OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralName(Cindex2, [])
			   end,
    %%-------------------------------------------------
    %% attribute certDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_ObjectDigestInfo(Cindex3, [<<48>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_TargetCert(Tlv) -> dec_TargetCert(Tlv, [16]).

dec_TargetCert(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute targetCertificate(1)   External OTP-PKIX:IssuerSerial
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_IssuerSerial(V1, [16]),
    %%-------------------------------------------------
    %% attribute targetName(2)   External OTP-PKIX:GeneralName OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [V2 = {131072, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131073, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131074, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131075, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131076, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131077, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131078, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131079, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      [V2 = {131080, _} | TempTlv3] ->
			  {dec_GeneralName(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute certDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{16, V3} | TempTlv4] ->
			  {dec_ObjectDigestInfo(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'TargetCert', Term1, Term2, Term3}.

%%================================
%%  Target
%%================================
enc_Target(Val) -> enc_Target(Val, []).

enc_Target({'Target', Val}, TagIn) ->
    enc_Target(Val, TagIn);
enc_Target(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   targetName ->
			       enc_GeneralName(element(2, Val), [<<160>>]);
			   targetGroup ->
			       enc_GeneralName(element(2, Val), [<<161>>]);
			   targetCert ->
			       enc_TargetCert(element(2, Val), [<<162>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_Target(Tlv) -> dec_Target(Tlv, []).

dec_Target(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'targetName'
      {131072, V1} -> {targetName, dec_GeneralName(V1, [])};
      %% 'targetGroup'
      {131073, V1} -> {targetGroup, dec_GeneralName(V1, [])};
      %% 'targetCert'
      {131074, V1} -> {targetCert, dec_TargetCert(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  Targets
%%================================
enc_Targets(Val) -> enc_Targets(Val, [<<48>>]).

enc_Targets({'Targets', Val}, TagIn) ->
    enc_Targets(Val, TagIn);
enc_Targets(Val, TagIn) ->
    {EncBytes, EncLen} = enc_Targets_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_Targets_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_Targets_components([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_Target(H, []),
    enc_Targets_components(T, [EncBytes | AccBytes],
			   AccLen + EncLen).

dec_Targets(Tlv) -> dec_Targets(Tlv, [16]).

dec_Targets(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Target(V1, []) || V1 <- Tlv1].

%%================================
%%  AttCertValidityPeriod
%%================================
enc_AttCertValidityPeriod(Val) ->
    enc_AttCertValidityPeriod(Val, [<<48>>]).

enc_AttCertValidityPeriod(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute notBeforeTime(1) with type GeneralizedTime
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_generalized_time([],
						   Cindex1, [<<24>>]),
    %%-------------------------------------------------
    %% attribute notAfterTime(2) with type GeneralizedTime
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_generalized_time([],
						   Cindex2, [<<24>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AttCertValidityPeriod(Tlv) ->
    dec_AttCertValidityPeriod(Tlv, [16]).

dec_AttCertValidityPeriod(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute notBeforeTime(1) with type GeneralizedTime
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_generalized_time(V1, [], [24]),
    %%-------------------------------------------------
    %% attribute notAfterTime(2) with type GeneralizedTime
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_generalized_time(V2, [], [24]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'AttCertValidityPeriod', Term1, Term2}.

%%================================
%%  IssuerSerial
%%================================
enc_IssuerSerial(Val) ->
    enc_IssuerSerial(Val, [<<48>>]).

enc_IssuerSerial(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute issuer(1)   External OTP-PKIX:GeneralNames
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_GeneralNames(Cindex1,
					    [<<48>>]),
    %%-------------------------------------------------
    %% attribute serial(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute issuerUID(3) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex3, [], [<<3>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_IssuerSerial(Tlv) -> dec_IssuerSerial(Tlv, [16]).

dec_IssuerSerial(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute issuer(1)   External OTP-PKIX:GeneralNames
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_GeneralNames(V1, [16]),
    %%-------------------------------------------------
    %% attribute serial(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute issuerUID(3) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{3, V3} | TempTlv4] ->
			  {decode_compact_bit_string(V3, [], [], []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'IssuerSerial', Term1, Term2, Term3}.

%%================================
%%  V2Form
%%================================
enc_V2Form(Val) -> enc_V2Form(Val, [<<48>>]).

enc_V2Form(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute issuerName(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex1, [<<48>>])
			   end,
    %%-------------------------------------------------
    %% attribute baseCertificateID(2)   External OTP-PKIX:IssuerSerial OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_IssuerSerial(Cindex2, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute objectDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_ObjectDigestInfo(Cindex3, [<<161>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_V2Form(Tlv) -> dec_V2Form(Tlv, [16]).

dec_V2Form(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute issuerName(1)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{16, V1} | TempTlv2] ->
			  {dec_GeneralNames(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute baseCertificateID(2)   External OTP-PKIX:IssuerSerial OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131072, V2} | TempTlv3] ->
			  {dec_IssuerSerial(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute objectDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131073, V3} | TempTlv4] ->
			  {dec_ObjectDigestInfo(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'V2Form', Term1, Term2, Term3}.

%%================================
%%  AttCertIssuer
%%================================
enc_AttCertIssuer(Val) -> enc_AttCertIssuer(Val, []).

enc_AttCertIssuer({'AttCertIssuer', Val}, TagIn) ->
    enc_AttCertIssuer(Val, TagIn);
enc_AttCertIssuer(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   v1Form ->
			       enc_GeneralNames(element(2, Val), [<<48>>]);
			   v2Form -> enc_V2Form(element(2, Val), [<<160>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_AttCertIssuer(Tlv) -> dec_AttCertIssuer(Tlv, []).

dec_AttCertIssuer(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'v1Form'
      {16, V1} -> {v1Form, dec_GeneralNames(V1, [])};
      %% 'v2Form'
      {131072, V1} -> {v2Form, dec_V2Form(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  ObjectDigestInfo
%%================================
enc_ObjectDigestInfo(Val) ->
    enc_ObjectDigestInfo(Val, [<<48>>]).

enc_ObjectDigestInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4} = Val,
    %%-------------------------------------------------
    %% attribute digestedObjectType(1) with type ENUMERATED
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     publicKey -> encode_enumerated(0, [<<10>>]);
			     publicKeyCert -> encode_enumerated(1, [<<10>>]);
			     otherObjectTypes -> encode_enumerated(2, [<<10>>]);
			     Enumval1 ->
				 exit({error,
				       {asn1,
					{enumerated_not_in_range, Enumval1}}})
			   end,
    %%-------------------------------------------------
    %% attribute otherObjectTypeID(2) with type OBJECT IDENTIFIER OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_object_identifier(Cindex2, [<<6>>])
			   end,
    %%-------------------------------------------------
    %% attribute digestAlgorithm(3)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_AlgorithmIdentifier(Cindex3,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute objectDigest(4) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = encode_bit_string([], Cindex4,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ObjectDigestInfo(Tlv) ->
    dec_ObjectDigestInfo(Tlv, [16]).

dec_ObjectDigestInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute digestedObjectType(1) with type ENUMERATED
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_enumerated(V1, [],
			      [{publicKey, 0}, {publicKeyCert, 1},
			       {otherObjectTypes, 2}],
			      [10]),
    %%-------------------------------------------------
    %% attribute otherObjectTypeID(2) with type OBJECT IDENTIFIER OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{6, V2} | TempTlv3] ->
			  {decode_object_identifier(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute digestAlgorithm(3)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_AlgorithmIdentifier(V3, [16]),
    %%-------------------------------------------------
    %% attribute objectDigest(4) with type BIT STRING
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = decode_compact_bit_string(V4, [], [], [3]),
    case Tlv5 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv5}}}) % extra fields not allowed
    end,
    {'ObjectDigestInfo', Term1, Term2, Term3, Term4}.

%%================================
%%  Holder
%%================================
enc_Holder(Val) -> enc_Holder(Val, [<<48>>]).

enc_Holder(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute baseCertificateID(1)   External OTP-PKIX:IssuerSerial OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_IssuerSerial(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute entityName(2)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex2, [<<161>>])
			   end,
    %%-------------------------------------------------
    %% attribute objectDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_ObjectDigestInfo(Cindex3, [<<162>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Holder(Tlv) -> dec_Holder(Tlv, [16]).

dec_Holder(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute baseCertificateID(1)   External OTP-PKIX:IssuerSerial OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_IssuerSerial(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute entityName(2)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {dec_GeneralNames(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute objectDigestInfo(3)   External OTP-PKIX:ObjectDigestInfo OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {dec_ObjectDigestInfo(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Holder', Term1, Term2, Term3}.

%%================================
%%  AttCertVersion
%%================================
enc_AttCertVersion(Val) ->
    enc_AttCertVersion(Val, [<<2>>]).

enc_AttCertVersion({'AttCertVersion', Val}, TagIn) ->
    enc_AttCertVersion(Val, TagIn);
enc_AttCertVersion(Val, TagIn) ->
    encode_integer([], Val, [{v2, 1}], TagIn).

dec_AttCertVersion(Tlv) -> dec_AttCertVersion(Tlv, [2]).

dec_AttCertVersion(Tlv, TagIn) ->
    decode_integer(Tlv, [], [{v2, 1}], TagIn).

%%================================
%%  AttributeCertificateInfo
%%================================
enc_AttributeCertificateInfo(Val) ->
    enc_AttributeCertificateInfo(Val, [<<48>>]).

enc_AttributeCertificateInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7, Cindex8, Cindex9} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [{v2, 1}], [<<2>>]),
    %%-------------------------------------------------
    %% attribute holder(2)   External OTP-PKIX:Holder
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_Holder(Cindex2, [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuer(3)   External OTP-PKIX:AttCertIssuer
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_AttCertIssuer(Cindex3, []),
    %%-------------------------------------------------
    %% attribute signature(4)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = enc_AlgorithmIdentifier(Cindex4,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute serialNumber(5) with type INTEGER
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = encode_integer([], Cindex5,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute attrCertValidityPeriod(6)   External OTP-PKIX:AttCertValidityPeriod
    %%-------------------------------------------------
    {EncBytes6, EncLen6} =
	enc_AttCertValidityPeriod(Cindex6, [<<48>>]),
    %%-------------------------------------------------
    %% attribute attributes(7) with type SEQUENCE OF
    %%-------------------------------------------------
    {EncBytes7, EncLen7} =
	enc_AttributeCertificateInfo_attributes(Cindex7,
						[<<48>>]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes8, EncLen8} = case Cindex8 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex8, [], [<<3>>])
			   end,
    %%-------------------------------------------------
    %% attribute extensions(9)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {EncBytes9, EncLen9} = case Cindex9 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_Extensions(Cindex9, [<<48>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7, EncBytes8,
		  EncBytes9],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7
		 + EncLen8
		 + EncLen9,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  AttributeCertificateInfo_attributes
%%================================

enc_AttributeCertificateInfo_attributes({'AttributeCertificateInfo_attributes',
					 Val},
					TagIn) ->
    enc_AttributeCertificateInfo_attributes(Val, TagIn);
enc_AttributeCertificateInfo_attributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_AttributeCertificateInfo_attributes_components(Val,
							   [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_AttributeCertificateInfo_attributes_components([],
						   AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_AttributeCertificateInfo_attributes_components([H
						    | T],
						   AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_Attribute(H, [<<48>>]),
    enc_AttributeCertificateInfo_attributes_components(T,
						       [EncBytes | AccBytes],
						       AccLen + EncLen).

dec_AttributeCertificateInfo_attributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Attribute(V1, [16]) || V1 <- Tlv1].

dec_AttributeCertificateInfo(Tlv) ->
    dec_AttributeCertificateInfo(Tlv, [16]).

dec_AttributeCertificateInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [{v2, 1}], [2]),
    %%-------------------------------------------------
    %% attribute holder(2)   External OTP-PKIX:Holder
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_Holder(V2, [16]),
    %%-------------------------------------------------
    %% attribute issuer(3)   External OTP-PKIX:AttCertIssuer
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_AttCertIssuer(V3, []),
    %%-------------------------------------------------
    %% attribute signature(4)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = dec_AlgorithmIdentifier(V4, [16]),
    %%-------------------------------------------------
    %% attribute serialNumber(5) with type INTEGER
    %%-------------------------------------------------
    [V5 | Tlv6] = Tlv5,
    Term5 = decode_integer(V5, [], [2]),
    %%-------------------------------------------------
    %% attribute attrCertValidityPeriod(6)   External OTP-PKIX:AttCertValidityPeriod
    %%-------------------------------------------------
    [V6 | Tlv7] = Tlv6,
    Term6 = dec_AttCertValidityPeriod(V6, [16]),
    %%-------------------------------------------------
    %% attribute attributes(7) with type SEQUENCE OF
    %%-------------------------------------------------
    [V7 | Tlv8] = Tlv7,
    Term7 = dec_AttributeCertificateInfo_attributes(V7,
						    [16]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term8, Tlv9} = case Tlv8 of
		      [{3, V8} | TempTlv9] ->
			  {decode_compact_bit_string(V8, [], [], []), TempTlv9};
		      _ -> {asn1_NOVALUE, Tlv8}
		    end,
    %%-------------------------------------------------
    %% attribute extensions(9)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {Term9, Tlv10} = case Tlv9 of
		       [{16, V9} | TempTlv10] ->
			   {dec_Extensions(V9, []), TempTlv10};
		       _ -> {asn1_NOVALUE, Tlv9}
		     end,
    case Tlv10 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv10}}}) % extra fields not allowed
    end,
    {'AttributeCertificateInfo', Term1, Term2, Term3, Term4,
     Term5, Term6, Term7, Term8, Term9}.

%%================================
%%  AttributeCertificate
%%================================
enc_AttributeCertificate(Val) ->
    enc_AttributeCertificate(Val, [<<48>>]).

enc_AttributeCertificate(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute acinfo(1)   External OTP-PKIX:AttributeCertificateInfo
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
	enc_AttributeCertificateInfo(Cindex1, [<<48>>]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_AlgorithmIdentifier(Cindex2,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute signatureValue(3) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_bit_string([], Cindex3,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AttributeCertificate(Tlv) ->
    dec_AttributeCertificate(Tlv, [16]).

dec_AttributeCertificate(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute acinfo(1)   External OTP-PKIX:AttributeCertificateInfo
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_AttributeCertificateInfo(V1, [16]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AlgorithmIdentifier(V2, [16]),
    %%-------------------------------------------------
    %% attribute signatureValue(3) with type BIT STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_compact_bit_string(V3, [], [], [3]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'AttributeCertificate', Term1, Term2, Term3}.

%%================================
%%  InvalidityDate
%%================================
enc_InvalidityDate(Val) ->
    enc_InvalidityDate(Val, [<<24>>]).

enc_InvalidityDate({'InvalidityDate', Val}, TagIn) ->
    enc_InvalidityDate(Val, TagIn);
enc_InvalidityDate(Val, TagIn) ->
    encode_generalized_time([], Val, TagIn).

dec_InvalidityDate(Tlv) ->
    dec_InvalidityDate(Tlv, [24]).

dec_InvalidityDate(Tlv, TagIn) ->
    decode_generalized_time(Tlv, [], TagIn).

%%================================
%%  HoldInstructionCode
%%================================
enc_HoldInstructionCode(Val) ->
    enc_HoldInstructionCode(Val, [<<6>>]).

enc_HoldInstructionCode({'HoldInstructionCode', Val},
			TagIn) ->
    enc_HoldInstructionCode(Val, TagIn);
enc_HoldInstructionCode(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_HoldInstructionCode(Tlv) ->
    dec_HoldInstructionCode(Tlv, [6]).

dec_HoldInstructionCode(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  CertificateIssuer
%%================================
enc_CertificateIssuer(Val) ->
    enc_CertificateIssuer(Val, [<<48>>]).

enc_CertificateIssuer({'CertificateIssuer', Val},
		      TagIn) ->
    enc_CertificateIssuer(Val, TagIn);
enc_CertificateIssuer(Val, TagIn) ->
    enc_GeneralNames(Val, TagIn).

dec_CertificateIssuer(Tlv) ->
    dec_CertificateIssuer(Tlv, [16]).

dec_CertificateIssuer(Tlv, TagIn) ->
    dec_GeneralNames(Tlv, TagIn).

%%================================
%%  CRLReason
%%================================
enc_CRLReason(Val) -> enc_CRLReason(Val, [<<10>>]).

enc_CRLReason({'CRLReason', Val}, TagIn) ->
    enc_CRLReason(Val, TagIn);
enc_CRLReason(Val, TagIn) ->
    case Val of
      unspecified -> encode_enumerated(0, TagIn);
      keyCompromise -> encode_enumerated(1, TagIn);
      cACompromise -> encode_enumerated(2, TagIn);
      affiliationChanged -> encode_enumerated(3, TagIn);
      superseded -> encode_enumerated(4, TagIn);
      cessationOfOperation -> encode_enumerated(5, TagIn);
      certificateHold -> encode_enumerated(6, TagIn);
      removeFromCRL -> encode_enumerated(8, TagIn);
      privilegeWithdrawn -> encode_enumerated(9, TagIn);
      aACompromise -> encode_enumerated(10, TagIn);
      Enumval1 ->
	  exit({error,
		{asn1, {enumerated_not_in_range, Enumval1}}})
    end.

dec_CRLReason(Tlv) -> dec_CRLReason(Tlv, [10]).

dec_CRLReason(Tlv, TagIn) ->
    decode_enumerated(Tlv, [],
		      [{unspecified, 0}, {keyCompromise, 1},
		       {cACompromise, 2}, {affiliationChanged, 3},
		       {superseded, 4}, {cessationOfOperation, 5},
		       {certificateHold, 6}, {removeFromCRL, 8},
		       {privilegeWithdrawn, 9}, {aACompromise, 10}],
		      TagIn).

%%================================
%%  BaseCRLNumber
%%================================
enc_BaseCRLNumber(Val) ->
    enc_BaseCRLNumber(Val, [<<2>>]).

enc_BaseCRLNumber({'BaseCRLNumber', Val}, TagIn) ->
    enc_BaseCRLNumber(Val, TagIn);
enc_BaseCRLNumber(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_BaseCRLNumber(Tlv) -> dec_BaseCRLNumber(Tlv, [2]).

dec_BaseCRLNumber(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 'MAX'}, TagIn).

%%================================
%%  IssuingDistributionPoint
%%================================
enc_IssuingDistributionPoint(Val) ->
    enc_IssuingDistributionPoint(Val, [<<48>>]).

enc_IssuingDistributionPoint(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6} =
	Val,
    %%-------------------------------------------------
    %% attribute distributionPoint(1)   External OTP-PKIX:DistributionPointName OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_DistributionPointName(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute onlyContainsUserCerts(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch check_bool(false,
						 Cindex2)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex2, [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute onlyContainsCACerts(3) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case catch check_bool(false,
						 Cindex3)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex3, [<<130>>])
			   end,
    %%-------------------------------------------------
    %% attribute onlySomeReasons(4) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case Cindex4 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_bit_string([], Cindex4,
						   [{unused, 0},
						    {keyCompromise, 1},
						    {cACompromise, 2},
						    {affiliationChanged, 3},
						    {superseded, 4},
						    {cessationOfOperation, 5},
						    {certificateHold, 6},
						    {privilegeWithdrawn, 7},
						    {aACompromise, 8}],
						   [<<131>>])
			   end,
    %%-------------------------------------------------
    %% attribute indirectCRL(5) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = case catch check_bool(false,
						 Cindex5)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex5, [<<132>>])
			   end,
    %%-------------------------------------------------
    %% attribute onlyContainsAttributeCerts(6) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = case catch check_bool(false,
						 Cindex6)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex6, [<<133>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_IssuingDistributionPoint(Tlv) ->
    dec_IssuingDistributionPoint(Tlv, [16]).

dec_IssuingDistributionPoint(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute distributionPoint(1)   External OTP-PKIX:DistributionPointName OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_DistributionPointName(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute onlyContainsUserCerts(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_boolean(V2, []), TempTlv3};
		      _ -> {false, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute onlyContainsCACerts(3) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {decode_boolean(V3, []), TempTlv4};
		      _ -> {false, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute onlySomeReasons(4) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term4, Tlv5} = case Tlv4 of
		      [{131075, V4} | TempTlv5] ->
			  {decode_compact_bit_string(V4, [],
						     [{unused, 0},
						      {keyCompromise, 1},
						      {cACompromise, 2},
						      {affiliationChanged, 3},
						      {superseded, 4},
						      {cessationOfOperation, 5},
						      {certificateHold, 6},
						      {privilegeWithdrawn, 7},
						      {aACompromise, 8}],
						     []),
			   TempTlv5};
		      _ -> {asn1_NOVALUE, Tlv4}
		    end,
    %%-------------------------------------------------
    %% attribute indirectCRL(5) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term5, Tlv6} = case Tlv5 of
		      [{131076, V5} | TempTlv6] ->
			  {decode_boolean(V5, []), TempTlv6};
		      _ -> {false, Tlv5}
		    end,
    %%-------------------------------------------------
    %% attribute onlyContainsAttributeCerts(6) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term6, Tlv7} = case Tlv6 of
		      [{131077, V6} | TempTlv7] ->
			  {decode_boolean(V6, []), TempTlv7};
		      _ -> {false, Tlv6}
		    end,
    case Tlv7 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv7}}}) % extra fields not allowed
    end,
    {'IssuingDistributionPoint', Term1, Term2, Term3, Term4,
     Term5, Term6}.

%%================================
%%  CRLNumber
%%================================
enc_CRLNumber(Val) -> enc_CRLNumber(Val, [<<2>>]).

enc_CRLNumber({'CRLNumber', Val}, TagIn) ->
    enc_CRLNumber(Val, TagIn);
enc_CRLNumber(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_CRLNumber(Tlv) -> dec_CRLNumber(Tlv, [2]).

dec_CRLNumber(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 'MAX'}, TagIn).

%%================================
%%  SubjectInfoAccessSyntax
%%================================
enc_SubjectInfoAccessSyntax(Val) ->
    enc_SubjectInfoAccessSyntax(Val, [<<48>>]).

enc_SubjectInfoAccessSyntax({'SubjectInfoAccessSyntax',
			     Val},
			    TagIn) ->
    enc_SubjectInfoAccessSyntax(Val, TagIn);
enc_SubjectInfoAccessSyntax(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_SubjectInfoAccessSyntax_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_SubjectInfoAccessSyntax_components([], AccBytes,
				       AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_SubjectInfoAccessSyntax_components([H | T],
				       AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_AccessDescription(H, [<<48>>]),
    enc_SubjectInfoAccessSyntax_components(T,
					   [EncBytes | AccBytes],
					   AccLen + EncLen).

dec_SubjectInfoAccessSyntax(Tlv) ->
    dec_SubjectInfoAccessSyntax(Tlv, [16]).

dec_SubjectInfoAccessSyntax(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_AccessDescription(V1, [16]) || V1 <- Tlv1].

%%================================
%%  AccessDescription
%%================================
enc_AccessDescription(Val) ->
    enc_AccessDescription(Val, [<<48>>]).

enc_AccessDescription(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute accessMethod(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute accessLocation(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_GeneralName(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AccessDescription(Tlv) ->
    dec_AccessDescription(Tlv, [16]).

dec_AccessDescription(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute accessMethod(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute accessLocation(2)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_GeneralName(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'AccessDescription', Term1, Term2}.

%%================================
%%  AuthorityInfoAccessSyntax
%%================================
enc_AuthorityInfoAccessSyntax(Val) ->
    enc_AuthorityInfoAccessSyntax(Val, [<<48>>]).

enc_AuthorityInfoAccessSyntax({'AuthorityInfoAccessSyntax',
			       Val},
			      TagIn) ->
    enc_AuthorityInfoAccessSyntax(Val, TagIn);
enc_AuthorityInfoAccessSyntax(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_AuthorityInfoAccessSyntax_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_AuthorityInfoAccessSyntax_components([], AccBytes,
					 AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_AuthorityInfoAccessSyntax_components([H | T],
					 AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_AccessDescription(H, [<<48>>]),
    enc_AuthorityInfoAccessSyntax_components(T,
					     [EncBytes | AccBytes],
					     AccLen + EncLen).

dec_AuthorityInfoAccessSyntax(Tlv) ->
    dec_AuthorityInfoAccessSyntax(Tlv, [16]).

dec_AuthorityInfoAccessSyntax(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_AccessDescription(V1, [16]) || V1 <- Tlv1].

%%================================
%%  FreshestCRL
%%================================
enc_FreshestCRL(Val) -> enc_FreshestCRL(Val, [<<48>>]).

enc_FreshestCRL({'FreshestCRL', Val}, TagIn) ->
    enc_FreshestCRL(Val, TagIn);
enc_FreshestCRL(Val, TagIn) ->
    enc_CRLDistributionPoints(Val, TagIn).

dec_FreshestCRL(Tlv) -> dec_FreshestCRL(Tlv, [16]).

dec_FreshestCRL(Tlv, TagIn) ->
    dec_CRLDistributionPoints(Tlv, TagIn).

%%================================
%%  InhibitAnyPolicy
%%================================
enc_InhibitAnyPolicy(Val) ->
    enc_InhibitAnyPolicy(Val, [<<2>>]).

enc_InhibitAnyPolicy({'InhibitAnyPolicy', Val},
		     TagIn) ->
    enc_InhibitAnyPolicy(Val, TagIn);
enc_InhibitAnyPolicy(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_InhibitAnyPolicy(Tlv) ->
    dec_InhibitAnyPolicy(Tlv, [2]).

dec_InhibitAnyPolicy(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 'MAX'}, TagIn).

%%================================
%%  KeyPurposeId
%%================================
enc_KeyPurposeId(Val) -> enc_KeyPurposeId(Val, [<<6>>]).

enc_KeyPurposeId({'KeyPurposeId', Val}, TagIn) ->
    enc_KeyPurposeId(Val, TagIn);
enc_KeyPurposeId(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_KeyPurposeId(Tlv) -> dec_KeyPurposeId(Tlv, [6]).

dec_KeyPurposeId(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  ExtKeyUsageSyntax
%%================================
enc_ExtKeyUsageSyntax(Val) ->
    enc_ExtKeyUsageSyntax(Val, [<<48>>]).

enc_ExtKeyUsageSyntax({'ExtKeyUsageSyntax', Val},
		      TagIn) ->
    enc_ExtKeyUsageSyntax(Val, TagIn);
enc_ExtKeyUsageSyntax(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_ExtKeyUsageSyntax_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_ExtKeyUsageSyntax_components([], AccBytes,
				 AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_ExtKeyUsageSyntax_components([H | T], AccBytes,
				 AccLen) ->
    {EncBytes, EncLen} = encode_object_identifier(H,
						  [<<6>>]),
    enc_ExtKeyUsageSyntax_components(T,
				     [EncBytes | AccBytes], AccLen + EncLen).

dec_ExtKeyUsageSyntax(Tlv) ->
    dec_ExtKeyUsageSyntax(Tlv, [16]).

dec_ExtKeyUsageSyntax(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_object_identifier(V1, [6]) || V1 <- Tlv1].

%%================================
%%  ReasonFlags
%%================================
enc_ReasonFlags(Val) -> enc_ReasonFlags(Val, [<<3>>]).

enc_ReasonFlags({'ReasonFlags', Val}, TagIn) ->
    enc_ReasonFlags(Val, TagIn);
enc_ReasonFlags(Val, TagIn) ->
    encode_bit_string([], Val,
		      [{unused, 0}, {keyCompromise, 1}, {cACompromise, 2},
		       {affiliationChanged, 3}, {superseded, 4},
		       {cessationOfOperation, 5}, {certificateHold, 6},
		       {privilegeWithdrawn, 7}, {aACompromise, 8}],
		      TagIn).

dec_ReasonFlags(Tlv) -> dec_ReasonFlags(Tlv, [3]).

dec_ReasonFlags(Tlv, TagIn) ->
    decode_compact_bit_string(Tlv, [],
			      [{unused, 0}, {keyCompromise, 1},
			       {cACompromise, 2}, {affiliationChanged, 3},
			       {superseded, 4}, {cessationOfOperation, 5},
			       {certificateHold, 6}, {privilegeWithdrawn, 7},
			       {aACompromise, 8}],
			      TagIn).

%%================================
%%  DistributionPointName
%%================================
enc_DistributionPointName(Val) ->
    enc_DistributionPointName(Val, []).

enc_DistributionPointName({'DistributionPointName',
			   Val},
			  TagIn) ->
    enc_DistributionPointName(Val, TagIn);
enc_DistributionPointName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   fullName ->
			       enc_GeneralNames(element(2, Val), [<<160>>]);
			   nameRelativeToCRLIssuer ->
			       enc_RelativeDistinguishedName(element(2, Val),
							     [<<161>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_DistributionPointName(Tlv) ->
    dec_DistributionPointName(Tlv, []).

dec_DistributionPointName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'fullName'
      {131072, V1} -> {fullName, dec_GeneralNames(V1, [])};
      %% 'nameRelativeToCRLIssuer'
      {131073, V1} ->
	  {nameRelativeToCRLIssuer,
	   dec_RelativeDistinguishedName(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  DistributionPoint
%%================================
enc_DistributionPoint(Val) ->
    enc_DistributionPoint(Val, [<<48>>]).

enc_DistributionPoint(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute distributionPoint(1)   External OTP-PKIX:DistributionPointName OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_DistributionPointName(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute reasons(2) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_bit_string([], Cindex2,
						   [{unused, 0},
						    {keyCompromise, 1},
						    {cACompromise, 2},
						    {affiliationChanged, 3},
						    {superseded, 4},
						    {cessationOfOperation, 5},
						    {certificateHold, 6},
						    {privilegeWithdrawn, 7},
						    {aACompromise, 8}],
						   [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute cRLIssuer(3)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex3, [<<162>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_DistributionPoint(Tlv) ->
    dec_DistributionPoint(Tlv, [16]).

dec_DistributionPoint(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute distributionPoint(1)   External OTP-PKIX:DistributionPointName OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_DistributionPointName(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute reasons(2) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_compact_bit_string(V2, [],
						     [{unused, 0},
						      {keyCompromise, 1},
						      {cACompromise, 2},
						      {affiliationChanged, 3},
						      {superseded, 4},
						      {cessationOfOperation, 5},
						      {certificateHold, 6},
						      {privilegeWithdrawn, 7},
						      {aACompromise, 8}],
						     []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute cRLIssuer(3)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {dec_GeneralNames(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'DistributionPoint', Term1, Term2, Term3}.

%%================================
%%  CRLDistributionPoints
%%================================
enc_CRLDistributionPoints(Val) ->
    enc_CRLDistributionPoints(Val, [<<48>>]).

enc_CRLDistributionPoints({'CRLDistributionPoints',
			   Val},
			  TagIn) ->
    enc_CRLDistributionPoints(Val, TagIn);
enc_CRLDistributionPoints(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_CRLDistributionPoints_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_CRLDistributionPoints_components([], AccBytes,
				     AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_CRLDistributionPoints_components([H | T], AccBytes,
				     AccLen) ->
    {EncBytes, EncLen} = enc_DistributionPoint(H, [<<48>>]),
    enc_CRLDistributionPoints_components(T,
					 [EncBytes | AccBytes],
					 AccLen + EncLen).

dec_CRLDistributionPoints(Tlv) ->
    dec_CRLDistributionPoints(Tlv, [16]).

dec_CRLDistributionPoints(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_DistributionPoint(V1, [16]) || V1 <- Tlv1].

%%================================
%%  SkipCerts
%%================================
enc_SkipCerts(Val) -> enc_SkipCerts(Val, [<<2>>]).

enc_SkipCerts({'SkipCerts', Val}, TagIn) ->
    enc_SkipCerts(Val, TagIn);
enc_SkipCerts(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_SkipCerts(Tlv) -> dec_SkipCerts(Tlv, [2]).

dec_SkipCerts(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 'MAX'}, TagIn).

%%================================
%%  PolicyConstraints
%%================================
enc_PolicyConstraints(Val) ->
    enc_PolicyConstraints(Val, [<<48>>]).

enc_PolicyConstraints(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute requireExplicitPolicy(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex1, [<<128>>])
			   end,
    %%-------------------------------------------------
    %% attribute inhibitPolicyMapping(2) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex2, [<<129>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PolicyConstraints(Tlv) ->
    dec_PolicyConstraints(Tlv, [16]).

dec_PolicyConstraints(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute requireExplicitPolicy(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_integer(V1, {0, 'MAX'}, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute inhibitPolicyMapping(2) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_integer(V2, {0, 'MAX'}, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PolicyConstraints', Term1, Term2}.

%%================================
%%  BaseDistance
%%================================
enc_BaseDistance(Val) -> enc_BaseDistance(Val, [<<2>>]).

enc_BaseDistance({'BaseDistance', Val}, TagIn) ->
    enc_BaseDistance(Val, TagIn);
enc_BaseDistance(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_BaseDistance(Tlv) -> dec_BaseDistance(Tlv, [2]).

dec_BaseDistance(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 'MAX'}, TagIn).

%%================================
%%  GeneralSubtree
%%================================
enc_GeneralSubtree(Val) ->
    enc_GeneralSubtree(Val, [<<48>>]).

enc_GeneralSubtree(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute base(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_GeneralName(Cindex1, []),
    %%-------------------------------------------------
    %% attribute minimum(2) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch check_int(0, Cindex2,
						[])
			       of
			     true -> {[], 0};
			     _ -> encode_integer([], Cindex2, [<<128>>])
			   end,
    %%-------------------------------------------------
    %% attribute maximum(3) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex3, [<<129>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_GeneralSubtree(Tlv) ->
    dec_GeneralSubtree(Tlv, [16]).

dec_GeneralSubtree(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute base(1)   External OTP-PKIX:GeneralName
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_GeneralName(V1, []),
    %%-------------------------------------------------
    %% attribute minimum(2) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131072, V2} | TempTlv3] ->
			  {decode_integer(V2, {0, 'MAX'}, []), TempTlv3};
		      _ -> {0, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute maximum(3) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131073, V3} | TempTlv4] ->
			  {decode_integer(V3, {0, 'MAX'}, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'GeneralSubtree', Term1, Term2, Term3}.

%%================================
%%  GeneralSubtrees
%%================================
enc_GeneralSubtrees(Val) ->
    enc_GeneralSubtrees(Val, [<<48>>]).

enc_GeneralSubtrees({'GeneralSubtrees', Val}, TagIn) ->
    enc_GeneralSubtrees(Val, TagIn);
enc_GeneralSubtrees(Val, TagIn) ->
    {EncBytes, EncLen} = enc_GeneralSubtrees_components(Val,
							[], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_GeneralSubtrees_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_GeneralSubtrees_components([H | T], AccBytes,
			       AccLen) ->
    {EncBytes, EncLen} = enc_GeneralSubtree(H, [<<48>>]),
    enc_GeneralSubtrees_components(T, [EncBytes | AccBytes],
				   AccLen + EncLen).

dec_GeneralSubtrees(Tlv) ->
    dec_GeneralSubtrees(Tlv, [16]).

dec_GeneralSubtrees(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_GeneralSubtree(V1, [16]) || V1 <- Tlv1].

%%================================
%%  NameConstraints
%%================================
enc_NameConstraints(Val) ->
    enc_NameConstraints(Val, [<<48>>]).

enc_NameConstraints(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute permittedSubtrees(1)   External OTP-PKIX:GeneralSubtrees OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralSubtrees(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute excludedSubtrees(2)   External OTP-PKIX:GeneralSubtrees OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralSubtrees(Cindex2, [<<161>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_NameConstraints(Tlv) ->
    dec_NameConstraints(Tlv, [16]).

dec_NameConstraints(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute permittedSubtrees(1)   External OTP-PKIX:GeneralSubtrees OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_GeneralSubtrees(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute excludedSubtrees(2)   External OTP-PKIX:GeneralSubtrees OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {dec_GeneralSubtrees(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'NameConstraints', Term1, Term2}.

%%================================
%%  BasicConstraints
%%================================
enc_BasicConstraints(Val) ->
    enc_BasicConstraints(Val, [<<48>>]).

enc_BasicConstraints(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute cA(1) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case catch check_bool(false,
						 Cindex1)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex1, [<<1>>])
			   end,
    %%-------------------------------------------------
    %% attribute pathLenConstraint(2) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex2, [<<2>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_BasicConstraints(Tlv) ->
    dec_BasicConstraints(Tlv, [16]).

dec_BasicConstraints(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute cA(1) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{1, V1} | TempTlv2] ->
			  {decode_boolean(V1, []), TempTlv2};
		      _ -> {false, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute pathLenConstraint(2) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{2, V2} | TempTlv3] ->
			  {decode_integer(V2, {0, 'MAX'}, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'BasicConstraints', Term1, Term2}.

%%================================
%%  SubjectDirectoryAttributes
%%================================
enc_SubjectDirectoryAttributes(Val) ->
    enc_SubjectDirectoryAttributes(Val, [<<48>>]).

enc_SubjectDirectoryAttributes({'SubjectDirectoryAttributes',
				Val},
			       TagIn) ->
    enc_SubjectDirectoryAttributes(Val, TagIn);
enc_SubjectDirectoryAttributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_SubjectDirectoryAttributes_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_SubjectDirectoryAttributes_components([], AccBytes,
					  AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_SubjectDirectoryAttributes_components([H | T],
					  AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_Attribute(H, [<<48>>]),
    enc_SubjectDirectoryAttributes_components(T,
					      [EncBytes | AccBytes],
					      AccLen + EncLen).

dec_SubjectDirectoryAttributes(Tlv) ->
    dec_SubjectDirectoryAttributes(Tlv, [16]).

dec_SubjectDirectoryAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Attribute(V1, [16]) || V1 <- Tlv1].

%%================================
%%  IssuerAltName
%%================================
enc_IssuerAltName(Val) ->
    enc_IssuerAltName(Val, [<<48>>]).

enc_IssuerAltName({'IssuerAltName', Val}, TagIn) ->
    enc_IssuerAltName(Val, TagIn);
enc_IssuerAltName(Val, TagIn) ->
    enc_GeneralNames(Val, TagIn).

dec_IssuerAltName(Tlv) -> dec_IssuerAltName(Tlv, [16]).

dec_IssuerAltName(Tlv, TagIn) ->
    dec_GeneralNames(Tlv, TagIn).

%%================================
%%  EDIPartyName
%%================================
enc_EDIPartyName(Val) ->
    enc_EDIPartyName(Val, [<<48>>]).

enc_EDIPartyName(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute nameAssigner(1)   External OTP-PKIX:DirectoryString OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_DirectoryString(Cindex1, [<<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute partyName(2)   External OTP-PKIX:DirectoryString
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_DirectoryString(Cindex2,
					       [<<161>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_EDIPartyName(Tlv) -> dec_EDIPartyName(Tlv, [16]).

dec_EDIPartyName(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute nameAssigner(1)   External OTP-PKIX:DirectoryString OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {dec_DirectoryString(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute partyName(2)   External OTP-PKIX:DirectoryString
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_DirectoryString(V2, [131073]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'EDIPartyName', Term1, Term2}.

%%================================
%%  AnotherName
%%================================
enc_AnotherName(Val) -> enc_AnotherName(Val, [<<48>>]).

enc_AnotherName(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type-id(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2,
					    [<<160>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AnotherName(Tlv) -> dec_AnotherName(Tlv, [16]).

dec_AnotherName(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type-id(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, [131072]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'AnotherName', Term1, Term2}.

%%================================
%%  GeneralName
%%================================
enc_GeneralName(Val) -> enc_GeneralName(Val, []).

enc_GeneralName({'GeneralName', Val}, TagIn) ->
    enc_GeneralName(Val, TagIn);
enc_GeneralName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   otherName ->
			       enc_AnotherName(element(2, Val), [<<160>>]);
			   rfc822Name ->
			       encode_restricted_string([], element(2, Val), 22,
							[<<129>>]);
			   dNSName ->
			       encode_restricted_string([], element(2, Val), 22,
							[<<130>>]);
			   x400Address ->
			       enc_ORAddress(element(2, Val), [<<163>>]);
			   directoryName ->
			       enc_Name(element(2, Val), [<<164>>]);
			   ediPartyName ->
			       enc_EDIPartyName(element(2, Val), [<<165>>]);
			   uniformResourceIdentifier ->
			       encode_restricted_string([], element(2, Val), 22,
							[<<134>>]);
			   iPAddress ->
			       encode_octet_string([], element(2, Val),
						   [<<135>>]);
			   registeredID ->
			       encode_object_identifier(element(2, Val),
							[<<136>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_GeneralName(Tlv) -> dec_GeneralName(Tlv, []).

dec_GeneralName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'otherName'
      {131072, V1} -> {otherName, dec_AnotherName(V1, [])};
      %% 'rfc822Name'
      {131073, V1} ->
	  {rfc822Name, decode_restricted_string(V1, [], 22, [])};
      %% 'dNSName'
      {131074, V1} ->
	  {dNSName, decode_restricted_string(V1, [], 22, [])};
      %% 'x400Address'
      {131075, V1} -> {x400Address, dec_ORAddress(V1, [])};
      %% 'directoryName'
      {131076, V1} -> {directoryName, dec_Name(V1, [])};
      %% 'ediPartyName'
      {131077, V1} ->
	  {ediPartyName, dec_EDIPartyName(V1, [])};
      %% 'uniformResourceIdentifier'
      {131078, V1} ->
	  {uniformResourceIdentifier,
	   decode_restricted_string(V1, [], 22, [])};
      %% 'iPAddress'
      {131079, V1} ->
	  {iPAddress, decode_octet_string(V1, [], [])};
      %% 'registeredID'
      {131080, V1} ->
	  {registeredID, decode_object_identifier(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  GeneralNames
%%================================
enc_GeneralNames(Val) ->
    enc_GeneralNames(Val, [<<48>>]).

enc_GeneralNames({'GeneralNames', Val}, TagIn) ->
    enc_GeneralNames(Val, TagIn);
enc_GeneralNames(Val, TagIn) ->
    {EncBytes, EncLen} = enc_GeneralNames_components(Val,
						     [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_GeneralNames_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_GeneralNames_components([H | T], AccBytes,
			    AccLen) ->
    {EncBytes, EncLen} = enc_GeneralName(H, []),
    enc_GeneralNames_components(T, [EncBytes | AccBytes],
				AccLen + EncLen).

dec_GeneralNames(Tlv) -> dec_GeneralNames(Tlv, [16]).

dec_GeneralNames(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_GeneralName(V1, []) || V1 <- Tlv1].

%%================================
%%  SubjectAltName
%%================================
enc_SubjectAltName(Val) ->
    enc_SubjectAltName(Val, [<<48>>]).

enc_SubjectAltName({'SubjectAltName', Val}, TagIn) ->
    enc_SubjectAltName(Val, TagIn);
enc_SubjectAltName(Val, TagIn) ->
    enc_GeneralNames(Val, TagIn).

dec_SubjectAltName(Tlv) ->
    dec_SubjectAltName(Tlv, [16]).

dec_SubjectAltName(Tlv, TagIn) ->
    dec_GeneralNames(Tlv, TagIn).

%%================================
%%  PolicyMappings
%%================================
enc_PolicyMappings(Val) ->
    enc_PolicyMappings(Val, [<<48>>]).

enc_PolicyMappings({'PolicyMappings', Val}, TagIn) ->
    enc_PolicyMappings(Val, TagIn);
enc_PolicyMappings(Val, TagIn) ->
    {EncBytes, EncLen} = enc_PolicyMappings_components(Val,
						       [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_PolicyMappings_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_PolicyMappings_components([H | T], AccBytes,
			      AccLen) ->
    {EncBytes, EncLen} = enc_PolicyMappings_SEQOF(H,
						  [<<48>>]),
    enc_PolicyMappings_components(T, [EncBytes | AccBytes],
				  AccLen + EncLen).

%%================================
%%  PolicyMappings_SEQOF
%%================================
enc_PolicyMappings_SEQOF(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute issuerDomainPolicy(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute subjectDomainPolicy(2) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_object_identifier(Cindex2,
						    [<<6>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PolicyMappings_SEQOF(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute issuerDomainPolicy(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute subjectDomainPolicy(2) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_object_identifier(V2, [6]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PolicyMappings_SEQOF', Term1, Term2}.

dec_PolicyMappings(Tlv) ->
    dec_PolicyMappings(Tlv, [16]).

dec_PolicyMappings(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_PolicyMappings_SEQOF(V1, [16]) || V1 <- Tlv1].

%%================================
%%  DisplayText
%%================================
enc_DisplayText(Val) -> enc_DisplayText(Val, []).

enc_DisplayText({'DisplayText', Val}, TagIn) ->
    enc_DisplayText(Val, TagIn);
enc_DisplayText(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   ia5String ->
			       encode_restricted_string([], element(2, Val), 22,
							[<<22>>]);
			   visibleString ->
			       encode_restricted_string([], element(2, Val), 26,
							[<<26>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_DisplayText(Tlv) -> dec_DisplayText(Tlv, []).

dec_DisplayText(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'ia5String'
      {22, V1} ->
	  {ia5String,
	   decode_restricted_string(V1, {1, 200}, 22, [])};
      %% 'visibleString'
      {26, V1} ->
	  {visibleString,
	   decode_restricted_string(V1, {1, 200}, 26, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 200}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  NoticeReference
%%================================
enc_NoticeReference(Val) ->
    enc_NoticeReference(Val, [<<48>>]).

enc_NoticeReference(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute organization(1)   External OTP-PKIX:DisplayText
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_DisplayText(Cindex1, []),
    %%-------------------------------------------------
    %% attribute noticeNumbers(2) with type SEQUENCE OF
    %%-------------------------------------------------
    {EncBytes2, EncLen2} =
	enc_NoticeReference_noticeNumbers(Cindex2, [<<48>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  NoticeReference_noticeNumbers
%%================================

enc_NoticeReference_noticeNumbers({'NoticeReference_noticeNumbers',
				   Val},
				  TagIn) ->
    enc_NoticeReference_noticeNumbers(Val, TagIn);
enc_NoticeReference_noticeNumbers(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_NoticeReference_noticeNumbers_components(Val, [],
						     0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_NoticeReference_noticeNumbers_components([],
					     AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_NoticeReference_noticeNumbers_components([H | T],
					     AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_integer([], H, [<<2>>]),
    enc_NoticeReference_noticeNumbers_components(T,
						 [EncBytes | AccBytes],
						 AccLen + EncLen).

dec_NoticeReference_noticeNumbers(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_integer(V1, [], [2]) || V1 <- Tlv1].

dec_NoticeReference(Tlv) ->
    dec_NoticeReference(Tlv, [16]).

dec_NoticeReference(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute organization(1)   External OTP-PKIX:DisplayText
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_DisplayText(V1, []),
    %%-------------------------------------------------
    %% attribute noticeNumbers(2) with type SEQUENCE OF
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_NoticeReference_noticeNumbers(V2, [16]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'NoticeReference', Term1, Term2}.

%%================================
%%  UserNotice
%%================================
enc_UserNotice(Val) -> enc_UserNotice(Val, [<<48>>]).

enc_UserNotice(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute noticeRef(1)   External OTP-PKIX:NoticeReference OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_NoticeReference(Cindex1, [<<48>>])
			   end,
    %%-------------------------------------------------
    %% attribute explicitText(2)   External OTP-PKIX:DisplayText OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_DisplayText(Cindex2, [])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_UserNotice(Tlv) -> dec_UserNotice(Tlv, [16]).

dec_UserNotice(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute noticeRef(1)   External OTP-PKIX:NoticeReference OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{16, V1} | TempTlv2] ->
			  {dec_NoticeReference(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute explicitText(2)   External OTP-PKIX:DisplayText OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [V2 = {22, _} | TempTlv3] ->
			  {dec_DisplayText(V2, []), TempTlv3};
		      [V2 = {26, _} | TempTlv3] ->
			  {dec_DisplayText(V2, []), TempTlv3};
		      [V2 = {30, _} | TempTlv3] ->
			  {dec_DisplayText(V2, []), TempTlv3};
		      [V2 = {12, _} | TempTlv3] ->
			  {dec_DisplayText(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'UserNotice', Term1, Term2}.

%%================================
%%  CPSuri
%%================================
enc_CPSuri(Val) -> enc_CPSuri(Val, [<<22>>]).

enc_CPSuri({'CPSuri', Val}, TagIn) ->
    enc_CPSuri(Val, TagIn);
enc_CPSuri(Val, TagIn) ->
    encode_restricted_string([], Val, 22, TagIn).

dec_CPSuri(Tlv) -> dec_CPSuri(Tlv, [22]).

dec_CPSuri(Tlv, TagIn) ->
    decode_restricted_string(Tlv, [], 22, TagIn).

%%================================
%%  PolicyQualifierId
%%================================
enc_PolicyQualifierId(Val) ->
    enc_PolicyQualifierId(Val, [<<6>>]).

enc_PolicyQualifierId({'PolicyQualifierId', Val},
		      TagIn) ->
    enc_PolicyQualifierId(Val, TagIn);
enc_PolicyQualifierId(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_PolicyQualifierId(Tlv) ->
    dec_PolicyQualifierId(Tlv, [6]).

dec_PolicyQualifierId(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  PolicyQualifierInfo
%%================================
enc_PolicyQualifierInfo(Val) ->
    enc_PolicyQualifierInfo(Val, [<<48>>]).

enc_PolicyQualifierInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute policyQualifierId(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute qualifier(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PolicyQualifierInfo(Tlv) ->
    dec_PolicyQualifierInfo(Tlv, [16]).

dec_PolicyQualifierInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute policyQualifierId(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute qualifier(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PolicyQualifierInfo', Term1, Term2}.

%%================================
%%  CertPolicyId
%%================================
enc_CertPolicyId(Val) -> enc_CertPolicyId(Val, [<<6>>]).

enc_CertPolicyId({'CertPolicyId', Val}, TagIn) ->
    enc_CertPolicyId(Val, TagIn);
enc_CertPolicyId(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_CertPolicyId(Tlv) -> dec_CertPolicyId(Tlv, [6]).

dec_CertPolicyId(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  PolicyInformation
%%================================
enc_PolicyInformation(Val) ->
    enc_PolicyInformation(Val, [<<48>>]).

enc_PolicyInformation(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute policyIdentifier(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute policyQualifiers(2) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_PolicyInformation_policyQualifiers(Cindex2,
									[<<48>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  PolicyInformation_policyQualifiers
%%================================

enc_PolicyInformation_policyQualifiers({'PolicyInformation_policyQualifiers',
					Val},
				       TagIn) ->
    enc_PolicyInformation_policyQualifiers(Val, TagIn);
enc_PolicyInformation_policyQualifiers(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_PolicyInformation_policyQualifiers_components(Val,
							  [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_PolicyInformation_policyQualifiers_components([],
						  AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_PolicyInformation_policyQualifiers_components([H
						   | T],
						  AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_PolicyQualifierInfo(H,
						 [<<48>>]),
    enc_PolicyInformation_policyQualifiers_components(T,
						      [EncBytes | AccBytes],
						      AccLen + EncLen).

dec_PolicyInformation_policyQualifiers(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_PolicyQualifierInfo(V1, [16]) || V1 <- Tlv1].

dec_PolicyInformation(Tlv) ->
    dec_PolicyInformation(Tlv, [16]).

dec_PolicyInformation(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute policyIdentifier(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute policyQualifiers(2) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{16, V2} | TempTlv3] ->
			  {dec_PolicyInformation_policyQualifiers(V2, []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PolicyInformation', Term1, Term2}.

%%================================
%%  CertificatePolicies
%%================================
enc_CertificatePolicies(Val) ->
    enc_CertificatePolicies(Val, [<<48>>]).

enc_CertificatePolicies({'CertificatePolicies', Val},
			TagIn) ->
    enc_CertificatePolicies(Val, TagIn);
enc_CertificatePolicies(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_CertificatePolicies_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_CertificatePolicies_components([], AccBytes,
				   AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_CertificatePolicies_components([H | T], AccBytes,
				   AccLen) ->
    {EncBytes, EncLen} = enc_PolicyInformation(H, [<<48>>]),
    enc_CertificatePolicies_components(T,
				       [EncBytes | AccBytes], AccLen + EncLen).

dec_CertificatePolicies(Tlv) ->
    dec_CertificatePolicies(Tlv, [16]).

dec_CertificatePolicies(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_PolicyInformation(V1, [16]) || V1 <- Tlv1].

%%================================
%%  PrivateKeyUsagePeriod
%%================================
enc_PrivateKeyUsagePeriod(Val) ->
    enc_PrivateKeyUsagePeriod(Val, [<<48>>]).

enc_PrivateKeyUsagePeriod(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute notBefore(1) with type GeneralizedTime OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_generalized_time([], Cindex1, [<<128>>])
			   end,
    %%-------------------------------------------------
    %% attribute notAfter(2) with type GeneralizedTime OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_generalized_time([], Cindex2, [<<129>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PrivateKeyUsagePeriod(Tlv) ->
    dec_PrivateKeyUsagePeriod(Tlv, [16]).

dec_PrivateKeyUsagePeriod(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute notBefore(1) with type GeneralizedTime OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_generalized_time(V1, [], []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute notAfter(2) with type GeneralizedTime OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_generalized_time(V2, [], []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PrivateKeyUsagePeriod', Term1, Term2}.

%%================================
%%  KeyUsage
%%================================
enc_KeyUsage(Val) -> enc_KeyUsage(Val, [<<3>>]).

enc_KeyUsage({'KeyUsage', Val}, TagIn) ->
    enc_KeyUsage(Val, TagIn);
enc_KeyUsage(Val, TagIn) ->
    encode_bit_string([], Val,
		      [{digitalSignature, 0}, {nonRepudiation, 1},
		       {keyEncipherment, 2}, {dataEncipherment, 3},
		       {keyAgreement, 4}, {keyCertSign, 5}, {cRLSign, 6},
		       {encipherOnly, 7}, {decipherOnly, 8}],
		      TagIn).

dec_KeyUsage(Tlv) -> dec_KeyUsage(Tlv, [3]).

dec_KeyUsage(Tlv, TagIn) ->
    decode_compact_bit_string(Tlv, [],
			      [{digitalSignature, 0}, {nonRepudiation, 1},
			       {keyEncipherment, 2}, {dataEncipherment, 3},
			       {keyAgreement, 4}, {keyCertSign, 5},
			       {cRLSign, 6}, {encipherOnly, 7},
			       {decipherOnly, 8}],
			      TagIn).

%%================================
%%  SubjectKeyIdentifier
%%================================
enc_SubjectKeyIdentifier(Val) ->
    enc_SubjectKeyIdentifier(Val, [<<4>>]).

enc_SubjectKeyIdentifier({'SubjectKeyIdentifier', Val},
			 TagIn) ->
    enc_SubjectKeyIdentifier(Val, TagIn);
enc_SubjectKeyIdentifier(Val, TagIn) ->
    encode_octet_string([], Val, TagIn).

dec_SubjectKeyIdentifier(Tlv) ->
    dec_SubjectKeyIdentifier(Tlv, [4]).

dec_SubjectKeyIdentifier(Tlv, TagIn) ->
    decode_octet_string(Tlv, [], TagIn).

%%================================
%%  KeyIdentifier
%%================================
enc_KeyIdentifier(Val) ->
    enc_KeyIdentifier(Val, [<<4>>]).

enc_KeyIdentifier({'KeyIdentifier', Val}, TagIn) ->
    enc_KeyIdentifier(Val, TagIn);
enc_KeyIdentifier(Val, TagIn) ->
    encode_octet_string([], Val, TagIn).

dec_KeyIdentifier(Tlv) -> dec_KeyIdentifier(Tlv, [4]).

dec_KeyIdentifier(Tlv, TagIn) ->
    decode_octet_string(Tlv, [], TagIn).

%%================================
%%  AuthorityKeyIdentifier
%%================================
enc_AuthorityKeyIdentifier(Val) ->
    enc_AuthorityKeyIdentifier(Val, [<<48>>]).

enc_AuthorityKeyIdentifier(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute keyIdentifier(1) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_octet_string([], Cindex1, [<<128>>])
			   end,
    %%-------------------------------------------------
    %% attribute authorityCertIssuer(2)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_GeneralNames(Cindex2, [<<161>>])
			   end,
    %%-------------------------------------------------
    %% attribute authorityCertSerialNumber(3) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_integer([], Cindex3, [<<130>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AuthorityKeyIdentifier(Tlv) ->
    dec_AuthorityKeyIdentifier(Tlv, [16]).

dec_AuthorityKeyIdentifier(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute keyIdentifier(1) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_octet_string(V1, [], []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute authorityCertIssuer(2)   External OTP-PKIX:GeneralNames OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {dec_GeneralNames(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute authorityCertSerialNumber(3) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {decode_integer(V3, [], []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'AuthorityKeyIdentifier', Term1, Term2, Term3}.

%%================================
%%  DigestAlgorithmIdentifier
%%================================
enc_DigestAlgorithmIdentifier(Val) ->
    enc_DigestAlgorithmIdentifier(Val, [<<48>>]).

enc_DigestAlgorithmIdentifier({'DigestAlgorithmIdentifier',
			       Val},
			      TagIn) ->
    enc_DigestAlgorithmIdentifier(Val, TagIn);
enc_DigestAlgorithmIdentifier(Val, TagIn) ->
    enc_AlgorithmIdentifier(Val, TagIn).

dec_DigestAlgorithmIdentifier(Tlv) ->
    dec_DigestAlgorithmIdentifier(Tlv, [16]).

dec_DigestAlgorithmIdentifier(Tlv, TagIn) ->
    dec_AlgorithmIdentifier(Tlv, TagIn).

%%================================
%%  DigestInfo
%%================================
enc_DigestInfo(Val) -> enc_DigestInfo(Val, [<<48>>]).

enc_DigestInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute digestAlgorithm(1)   External OTP-PKIX:DigestAlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
	enc_DigestAlgorithmIdentifier(Cindex1, [<<48>>]),
    %%-------------------------------------------------
    %% attribute digest(2) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_octet_string([], Cindex2,
					       [<<4>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_DigestInfo(Tlv) -> dec_DigestInfo(Tlv, [16]).

dec_DigestInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute digestAlgorithm(1)   External OTP-PKIX:DigestAlgorithmIdentifier
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_DigestAlgorithmIdentifier(V1, [16]),
    %%-------------------------------------------------
    %% attribute digest(2) with type OCTET STRING
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_octet_string(V2, [], [4]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'DigestInfo', Term1, Term2}.

%%================================
%%  OtherPrimeInfo
%%================================
enc_OtherPrimeInfo(Val) ->
    enc_OtherPrimeInfo(Val, [<<48>>]).

enc_OtherPrimeInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute prime(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute exponent(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute coefficient(3) with type INTEGER
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_integer([], Cindex3,
					  [<<2>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_OtherPrimeInfo(Tlv) ->
    dec_OtherPrimeInfo(Tlv, [16]).

dec_OtherPrimeInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute prime(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute exponent(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute coefficient(3) with type INTEGER
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_integer(V3, [], [2]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'OtherPrimeInfo', Term1, Term2, Term3}.

%%================================
%%  OtherPrimeInfos
%%================================
enc_OtherPrimeInfos(Val) ->
    enc_OtherPrimeInfos(Val, [<<48>>]).

enc_OtherPrimeInfos({'OtherPrimeInfos', Val}, TagIn) ->
    enc_OtherPrimeInfos(Val, TagIn);
enc_OtherPrimeInfos(Val, TagIn) ->
    {EncBytes, EncLen} = enc_OtherPrimeInfos_components(Val,
							[], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_OtherPrimeInfos_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_OtherPrimeInfos_components([H | T], AccBytes,
			       AccLen) ->
    {EncBytes, EncLen} = enc_OtherPrimeInfo(H, [<<48>>]),
    enc_OtherPrimeInfos_components(T, [EncBytes | AccBytes],
				   AccLen + EncLen).

dec_OtherPrimeInfos(Tlv) ->
    dec_OtherPrimeInfos(Tlv, [16]).

dec_OtherPrimeInfos(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_OtherPrimeInfo(V1, [16]) || V1 <- Tlv1].

%%================================
%%  VersionPKCS-1
%%================================
'enc_VersionPKCS-1'(Val) ->
    'enc_VersionPKCS-1'(Val, [<<2>>]).

'enc_VersionPKCS-1'({'VersionPKCS-1', Val}, TagIn) ->
    'enc_VersionPKCS-1'(Val, TagIn);
'enc_VersionPKCS-1'(Val, TagIn) ->
    encode_integer([], Val, [{'two-prime', 0}, {multi, 1}],
		   TagIn).

'dec_VersionPKCS-1'(Tlv) ->
    'dec_VersionPKCS-1'(Tlv, [2]).

'dec_VersionPKCS-1'(Tlv, TagIn) ->
    decode_integer(Tlv, [], [{'two-prime', 0}, {multi, 1}],
		   TagIn).

%%================================
%%  RSAPrivateKey
%%================================
enc_RSAPrivateKey(Val) ->
    enc_RSAPrivateKey(Val, [<<48>>]).

enc_RSAPrivateKey(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7, Cindex8, Cindex9, Cindex10} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [{'two-prime', 0}, {multi, 1}],
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute modulus(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute publicExponent(3) with type INTEGER
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_integer([], Cindex3,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute privateExponent(4) with type INTEGER
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = encode_integer([], Cindex4,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute prime1(5) with type INTEGER
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = encode_integer([], Cindex5,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute prime2(6) with type INTEGER
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = encode_integer([], Cindex6,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute exponent1(7) with type INTEGER
    %%-------------------------------------------------
    {EncBytes7, EncLen7} = encode_integer([], Cindex7,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute exponent2(8) with type INTEGER
    %%-------------------------------------------------
    {EncBytes8, EncLen8} = encode_integer([], Cindex8,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute coefficient(9) with type INTEGER
    %%-------------------------------------------------
    {EncBytes9, EncLen9} = encode_integer([], Cindex9,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute otherPrimeInfos(10)   External OTP-PKIX:OtherPrimeInfos OPTIONAL
    %%-------------------------------------------------
    {EncBytes10, EncLen10} = case Cindex10 of
			       asn1_NOVALUE -> {<<>>, 0};
			       _ -> enc_OtherPrimeInfos(Cindex10, [<<48>>])
			     end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7, EncBytes8,
		  EncBytes9, EncBytes10],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7
		 + EncLen8
		 + EncLen9
		 + EncLen10,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_RSAPrivateKey(Tlv) -> dec_RSAPrivateKey(Tlv, [16]).

dec_RSAPrivateKey(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [],
			   [{'two-prime', 0}, {multi, 1}], [2]),
    %%-------------------------------------------------
    %% attribute modulus(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute publicExponent(3) with type INTEGER
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_integer(V3, [], [2]),
    %%-------------------------------------------------
    %% attribute privateExponent(4) with type INTEGER
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = decode_integer(V4, [], [2]),
    %%-------------------------------------------------
    %% attribute prime1(5) with type INTEGER
    %%-------------------------------------------------
    [V5 | Tlv6] = Tlv5,
    Term5 = decode_integer(V5, [], [2]),
    %%-------------------------------------------------
    %% attribute prime2(6) with type INTEGER
    %%-------------------------------------------------
    [V6 | Tlv7] = Tlv6,
    Term6 = decode_integer(V6, [], [2]),
    %%-------------------------------------------------
    %% attribute exponent1(7) with type INTEGER
    %%-------------------------------------------------
    [V7 | Tlv8] = Tlv7,
    Term7 = decode_integer(V7, [], [2]),
    %%-------------------------------------------------
    %% attribute exponent2(8) with type INTEGER
    %%-------------------------------------------------
    [V8 | Tlv9] = Tlv8,
    Term8 = decode_integer(V8, [], [2]),
    %%-------------------------------------------------
    %% attribute coefficient(9) with type INTEGER
    %%-------------------------------------------------
    [V9 | Tlv10] = Tlv9,
    Term9 = decode_integer(V9, [], [2]),
    %%-------------------------------------------------
    %% attribute otherPrimeInfos(10)   External OTP-PKIX:OtherPrimeInfos OPTIONAL
    %%-------------------------------------------------
    {Term10, Tlv11} = case Tlv10 of
			[{16, V10} | TempTlv11] ->
			    {dec_OtherPrimeInfos(V10, []), TempTlv11};
			_ -> {asn1_NOVALUE, Tlv10}
		      end,
    case Tlv11 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv11}}}) % extra fields not allowed
    end,
    {'RSAPrivateKey', Term1, Term2, Term3, Term4, Term5,
     Term6, Term7, Term8, Term9, Term10}.

%%================================
%%  TeletexDomainDefinedAttribute
%%================================
enc_TeletexDomainDefinedAttribute(Val) ->
    enc_TeletexDomainDefinedAttribute(Val, [<<48>>]).

enc_TeletexDomainDefinedAttribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type(1) with type TeletexString
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string([],
						    Cindex1, 20, [<<20>>]),
    %%-------------------------------------------------
    %% attribute value(2) with type TeletexString
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_restricted_string([],
						    Cindex2, 20, [<<20>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_TeletexDomainDefinedAttribute(Tlv) ->
    dec_TeletexDomainDefinedAttribute(Tlv, [16]).

dec_TeletexDomainDefinedAttribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type TeletexString
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_restricted_string(V1, {1, 8}, 20, [20]),
    %%-------------------------------------------------
    %% attribute value(2) with type TeletexString
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_restricted_string(V2, {1, 128}, 20,
				     [20]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'TeletexDomainDefinedAttribute', Term1, Term2}.

%%================================
%%  TeletexDomainDefinedAttributes
%%================================
enc_TeletexDomainDefinedAttributes(Val) ->
    enc_TeletexDomainDefinedAttributes(Val, [<<48>>]).

enc_TeletexDomainDefinedAttributes({'TeletexDomainDefinedAttributes',
				    Val},
				   TagIn) ->
    enc_TeletexDomainDefinedAttributes(Val, TagIn);
enc_TeletexDomainDefinedAttributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_TeletexDomainDefinedAttributes_components(Val, [],
						      0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_TeletexDomainDefinedAttributes_components([],
					      AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_TeletexDomainDefinedAttributes_components([H | T],
					      AccBytes, AccLen) ->
    {EncBytes, EncLen} =
	enc_TeletexDomainDefinedAttribute(H, [<<48>>]),
    enc_TeletexDomainDefinedAttributes_components(T,
						  [EncBytes | AccBytes],
						  AccLen + EncLen).

dec_TeletexDomainDefinedAttributes(Tlv) ->
    dec_TeletexDomainDefinedAttributes(Tlv, [16]).

dec_TeletexDomainDefinedAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_TeletexDomainDefinedAttribute(V1, [16])
     || V1 <- Tlv1].

%%================================
%%  TerminalType
%%================================
enc_TerminalType(Val) -> enc_TerminalType(Val, [<<2>>]).

enc_TerminalType({'TerminalType', Val}, TagIn) ->
    enc_TerminalType(Val, TagIn);
enc_TerminalType(Val, TagIn) ->
    encode_integer([], Val,
		   [{telex, 3}, {teletex, 4}, {'g3-facsimile', 5},
		    {'g4-facsimile', 6}, {'ia5-terminal', 7},
		    {videotex, 8}],
		   TagIn).

dec_TerminalType(Tlv) -> dec_TerminalType(Tlv, [2]).

dec_TerminalType(Tlv, TagIn) ->
    decode_integer(Tlv, {0, 256},
		   [{telex, 3}, {teletex, 4}, {'g3-facsimile', 5},
		    {'g4-facsimile', 6}, {'ia5-terminal', 7},
		    {videotex, 8}],
		   TagIn).

%%================================
%%  PresentationAddress
%%================================
enc_PresentationAddress(Val) ->
    enc_PresentationAddress(Val, [<<48>>]).

enc_PresentationAddress(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4} = Val,
    %%-------------------------------------------------
    %% attribute pSelector(1) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_octet_string([], Cindex1,
						     [<<4>>, <<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute sSelector(2) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_octet_string([], Cindex2,
						     [<<4>>, <<161>>])
			   end,
    %%-------------------------------------------------
    %% attribute tSelector(3) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_octet_string([], Cindex3,
						     [<<4>>, <<162>>])
			   end,
    %%-------------------------------------------------
    %% attribute nAddresses(4) with type SET OF
    %%-------------------------------------------------
    {EncBytes4, EncLen4} =
	enc_PresentationAddress_nAddresses(Cindex4,
					   [<<49>>, <<163>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  PresentationAddress_nAddresses
%%================================

enc_PresentationAddress_nAddresses({'PresentationAddress_nAddresses',
				    Val},
				   TagIn) ->
    enc_PresentationAddress_nAddresses(Val, TagIn);
enc_PresentationAddress_nAddresses(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_PresentationAddress_nAddresses_components(Val, [],
						      0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_PresentationAddress_nAddresses_components([],
					      AccBytes, AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_PresentationAddress_nAddresses_components([H | T],
					      AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_octet_string([], H,
					     [<<4>>]),
    enc_PresentationAddress_nAddresses_components(T,
						  [EncBytes | AccBytes],
						  AccLen + EncLen).

dec_PresentationAddress_nAddresses(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_octet_string(V1, [], [4]) || V1 <- Tlv1].

dec_PresentationAddress(Tlv) ->
    dec_PresentationAddress(Tlv, [16]).

dec_PresentationAddress(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute pSelector(1) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_octet_string(V1, [], [4]), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute sSelector(2) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_octet_string(V2, [], [4]), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute tSelector(3) with type OCTET STRING OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131074, V3} | TempTlv4] ->
			  {decode_octet_string(V3, [], [4]), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute nAddresses(4) with type SET OF
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = dec_PresentationAddress_nAddresses(V4,
					       [131075, 17]),
    case Tlv5 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv5}}}) % extra fields not allowed
    end,
    {'PresentationAddress', Term1, Term2, Term3, Term4}.

%%================================
%%  ExtendedNetworkAddress
%%================================
enc_ExtendedNetworkAddress(Val) ->
    enc_ExtendedNetworkAddress(Val, []).

enc_ExtendedNetworkAddress({'ExtendedNetworkAddress',
			    Val},
			   TagIn) ->
    enc_ExtendedNetworkAddress(Val, TagIn);
enc_ExtendedNetworkAddress(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   'e163-4-address' ->
			       'enc_ExtendedNetworkAddress_e163-4-address'(element(2,
										   Val),
									   [<<48>>]);
			   'psap-address' ->
			       enc_PresentationAddress(element(2, Val),
						       [<<160>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

%%================================
%%  ExtendedNetworkAddress_e163-4-address
%%================================
'enc_ExtendedNetworkAddress_e163-4-address'(Val,
					    TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute number(1) with type NumericString
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string([],
						    Cindex1, 18, [<<128>>]),
    %%-------------------------------------------------
    %% attribute sub-address(2) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex2, 18,
							  [<<129>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_ExtendedNetworkAddress_e163-4-address'(Tlv,
					    TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute number(1) with type NumericString
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_restricted_string(V1, {1, 15}, 18,
				     [131072]),
    %%-------------------------------------------------
    %% attribute sub-address(2) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{131073, V2} | TempTlv3] ->
			  {decode_restricted_string(V2, {1, 40}, 18, []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'ExtendedNetworkAddress_e163-4-address', Term1, Term2}.

dec_ExtendedNetworkAddress(Tlv) ->
    dec_ExtendedNetworkAddress(Tlv, []).

dec_ExtendedNetworkAddress(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'e163-4-address'
      {16, V1} ->
	  {'e163-4-address',
	   'dec_ExtendedNetworkAddress_e163-4-address'(V1, [])};
      %% 'psap-address'
      {131072, V1} ->
	  {'psap-address', dec_PresentationAddress(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  PDSParameter
%%================================
enc_PDSParameter(Val) ->
    enc_PDSParameter(Val, [<<49>>]).

enc_PDSParameter(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute printable-string(1) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex1, 19,
							  [<<19>>])
			   end,
    %%-------------------------------------------------
    %% attribute teletex-string(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex2, 20,
							  [<<20>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PDSParameter(Tlv) -> dec_PDSParameter(Tlv, [17]).

dec_PDSParameter(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    SetFun = fun (FunTlv) ->
		     case FunTlv of
		       %printable-string
		       TTlv = {19, _} -> {1, TTlv};
		       %teletex-string
		       TTlv = {20, _} -> {2, TTlv};
		       Else -> {3, Else}
		     end
	     end,
    PositionList = [SetFun(TempTlv) || TempTlv <- Tlv1],
    Tlv2 = [Stlv || {_, Stlv} <- lists:sort(PositionList)],
    %%-------------------------------------------------
    %% attribute printable-string(1) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv3} = case Tlv2 of
		      [{19, V1} | TempTlv3] ->
			  {decode_restricted_string(V1, {1, 30}, 19, []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute teletex-string(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv4} = case Tlv3 of
		      [{20, V2} | TempTlv4] ->
			  {decode_restricted_string(V2, {1, 30}, 20, []),
			   TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'PDSParameter', Term1, Term2}.

%%================================
%%  LocalPostalAttributes
%%================================
enc_LocalPostalAttributes(Val) ->
    enc_LocalPostalAttributes(Val, [<<49>>]).

enc_LocalPostalAttributes({'LocalPostalAttributes',
			   Val},
			  TagIn) ->
    enc_LocalPostalAttributes(Val, TagIn);
enc_LocalPostalAttributes(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_LocalPostalAttributes(Tlv) ->
    dec_LocalPostalAttributes(Tlv, [17]).

dec_LocalPostalAttributes(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  UniquePostalName
%%================================
enc_UniquePostalName(Val) ->
    enc_UniquePostalName(Val, [<<49>>]).

enc_UniquePostalName({'UniquePostalName', Val},
		     TagIn) ->
    enc_UniquePostalName(Val, TagIn);
enc_UniquePostalName(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_UniquePostalName(Tlv) ->
    dec_UniquePostalName(Tlv, [17]).

dec_UniquePostalName(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PosteRestanteAddress
%%================================
enc_PosteRestanteAddress(Val) ->
    enc_PosteRestanteAddress(Val, [<<49>>]).

enc_PosteRestanteAddress({'PosteRestanteAddress', Val},
			 TagIn) ->
    enc_PosteRestanteAddress(Val, TagIn);
enc_PosteRestanteAddress(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PosteRestanteAddress(Tlv) ->
    dec_PosteRestanteAddress(Tlv, [17]).

dec_PosteRestanteAddress(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PostOfficeBoxAddress
%%================================
enc_PostOfficeBoxAddress(Val) ->
    enc_PostOfficeBoxAddress(Val, [<<49>>]).

enc_PostOfficeBoxAddress({'PostOfficeBoxAddress', Val},
			 TagIn) ->
    enc_PostOfficeBoxAddress(Val, TagIn);
enc_PostOfficeBoxAddress(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PostOfficeBoxAddress(Tlv) ->
    dec_PostOfficeBoxAddress(Tlv, [17]).

dec_PostOfficeBoxAddress(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  StreetAddress
%%================================
enc_StreetAddress(Val) ->
    enc_StreetAddress(Val, [<<49>>]).

enc_StreetAddress({'StreetAddress', Val}, TagIn) ->
    enc_StreetAddress(Val, TagIn);
enc_StreetAddress(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_StreetAddress(Tlv) -> dec_StreetAddress(Tlv, [17]).

dec_StreetAddress(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  UnformattedPostalAddress
%%================================
enc_UnformattedPostalAddress(Val) ->
    enc_UnformattedPostalAddress(Val, [<<49>>]).

enc_UnformattedPostalAddress(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute printable-address(1) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 'enc_UnformattedPostalAddress_printable-address'(Cindex1,
										  [<<48>>])
			   end,
    %%-------------------------------------------------
    %% attribute teletex-string(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex2, 20,
							  [<<20>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  UnformattedPostalAddress_printable-address
%%================================

'enc_UnformattedPostalAddress_printable-address'({'UnformattedPostalAddress_printable-address',
						  Val},
						 TagIn) ->
    'enc_UnformattedPostalAddress_printable-address'(Val,
						     TagIn);
'enc_UnformattedPostalAddress_printable-address'(Val,
						 TagIn) ->
    {EncBytes, EncLen} =
	'enc_UnformattedPostalAddress_printable-address_components'(Val,
								    [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

'enc_UnformattedPostalAddress_printable-address_components'([],
							    AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
'enc_UnformattedPostalAddress_printable-address_components'([H
							     | T],
							    AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_restricted_string([], H, 19,
						  [<<19>>]),
    'enc_UnformattedPostalAddress_printable-address_components'(T,
								[EncBytes
								 | AccBytes],
								AccLen +
								  EncLen).

'dec_UnformattedPostalAddress_printable-address'(Tlv,
						 TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_restricted_string(V1, {1, 30}, 19, [19])
     || V1 <- Tlv1].

dec_UnformattedPostalAddress(Tlv) ->
    dec_UnformattedPostalAddress(Tlv, [17]).

dec_UnformattedPostalAddress(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    SetFun = fun (FunTlv) ->
		     case FunTlv of
		       %printable-address
		       TTlv = {16, _} -> {1, TTlv};
		       %teletex-string
		       TTlv = {20, _} -> {2, TTlv};
		       Else -> {3, Else}
		     end
	     end,
    PositionList = [SetFun(TempTlv) || TempTlv <- Tlv1],
    Tlv2 = [Stlv || {_, Stlv} <- lists:sort(PositionList)],
    %%-------------------------------------------------
    %% attribute printable-address(1) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv3} = case Tlv2 of
		      [{16, V1} | TempTlv3] ->
			  {'dec_UnformattedPostalAddress_printable-address'(V1,
									    []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute teletex-string(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv4} = case Tlv3 of
		      [{20, V2} | TempTlv4] ->
			  {decode_restricted_string(V2, {1, 180}, 20, []),
			   TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'UnformattedPostalAddress', Term1, Term2}.

%%================================
%%  ExtensionPhysicalDeliveryAddressComponents
%%================================
enc_ExtensionPhysicalDeliveryAddressComponents(Val) ->
    enc_ExtensionPhysicalDeliveryAddressComponents(Val,
						   [<<49>>]).

enc_ExtensionPhysicalDeliveryAddressComponents({'ExtensionPhysicalDeliveryAddressComponents',
						Val},
					       TagIn) ->
    enc_ExtensionPhysicalDeliveryAddressComponents(Val,
						   TagIn);
enc_ExtensionPhysicalDeliveryAddressComponents(Val,
					       TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_ExtensionPhysicalDeliveryAddressComponents(Tlv) ->
    dec_ExtensionPhysicalDeliveryAddressComponents(Tlv,
						   [17]).

dec_ExtensionPhysicalDeliveryAddressComponents(Tlv,
					       TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PhysicalDeliveryOrganizationName
%%================================
enc_PhysicalDeliveryOrganizationName(Val) ->
    enc_PhysicalDeliveryOrganizationName(Val, [<<49>>]).

enc_PhysicalDeliveryOrganizationName({'PhysicalDeliveryOrganizationName',
				      Val},
				     TagIn) ->
    enc_PhysicalDeliveryOrganizationName(Val, TagIn);
enc_PhysicalDeliveryOrganizationName(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PhysicalDeliveryOrganizationName(Tlv) ->
    dec_PhysicalDeliveryOrganizationName(Tlv, [17]).

dec_PhysicalDeliveryOrganizationName(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PhysicalDeliveryPersonalName
%%================================
enc_PhysicalDeliveryPersonalName(Val) ->
    enc_PhysicalDeliveryPersonalName(Val, [<<49>>]).

enc_PhysicalDeliveryPersonalName({'PhysicalDeliveryPersonalName',
				  Val},
				 TagIn) ->
    enc_PhysicalDeliveryPersonalName(Val, TagIn);
enc_PhysicalDeliveryPersonalName(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PhysicalDeliveryPersonalName(Tlv) ->
    dec_PhysicalDeliveryPersonalName(Tlv, [17]).

dec_PhysicalDeliveryPersonalName(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  ExtensionORAddressComponents
%%================================
enc_ExtensionORAddressComponents(Val) ->
    enc_ExtensionORAddressComponents(Val, [<<49>>]).

enc_ExtensionORAddressComponents({'ExtensionORAddressComponents',
				  Val},
				 TagIn) ->
    enc_ExtensionORAddressComponents(Val, TagIn);
enc_ExtensionORAddressComponents(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_ExtensionORAddressComponents(Tlv) ->
    dec_ExtensionORAddressComponents(Tlv, [17]).

dec_ExtensionORAddressComponents(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PhysicalDeliveryOfficeNumber
%%================================
enc_PhysicalDeliveryOfficeNumber(Val) ->
    enc_PhysicalDeliveryOfficeNumber(Val, [<<49>>]).

enc_PhysicalDeliveryOfficeNumber({'PhysicalDeliveryOfficeNumber',
				  Val},
				 TagIn) ->
    enc_PhysicalDeliveryOfficeNumber(Val, TagIn);
enc_PhysicalDeliveryOfficeNumber(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PhysicalDeliveryOfficeNumber(Tlv) ->
    dec_PhysicalDeliveryOfficeNumber(Tlv, [17]).

dec_PhysicalDeliveryOfficeNumber(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PhysicalDeliveryOfficeName
%%================================
enc_PhysicalDeliveryOfficeName(Val) ->
    enc_PhysicalDeliveryOfficeName(Val, [<<49>>]).

enc_PhysicalDeliveryOfficeName({'PhysicalDeliveryOfficeName',
				Val},
			       TagIn) ->
    enc_PhysicalDeliveryOfficeName(Val, TagIn);
enc_PhysicalDeliveryOfficeName(Val, TagIn) ->
    enc_PDSParameter(Val, TagIn).

dec_PhysicalDeliveryOfficeName(Tlv) ->
    dec_PhysicalDeliveryOfficeName(Tlv, [17]).

dec_PhysicalDeliveryOfficeName(Tlv, TagIn) ->
    dec_PDSParameter(Tlv, TagIn).

%%================================
%%  PostalCode
%%================================
enc_PostalCode(Val) -> enc_PostalCode(Val, []).

enc_PostalCode({'PostalCode', Val}, TagIn) ->
    enc_PostalCode(Val, TagIn);
enc_PostalCode(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   'numeric-code' ->
			       encode_restricted_string([], element(2, Val), 18,
							[<<18>>]);
			   'printable-code' ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_PostalCode(Tlv) -> dec_PostalCode(Tlv, []).

dec_PostalCode(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'numeric-code'
      {18, V1} ->
	  {'numeric-code',
	   decode_restricted_string(V1, {1, 16}, 18, [])};
      %% 'printable-code'
      {19, V1} ->
	  {'printable-code',
	   decode_restricted_string(V1, {1, 16}, 19, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  PhysicalDeliveryCountryName
%%================================
enc_PhysicalDeliveryCountryName(Val) ->
    enc_PhysicalDeliveryCountryName(Val, []).

enc_PhysicalDeliveryCountryName({'PhysicalDeliveryCountryName',
				 Val},
				TagIn) ->
    enc_PhysicalDeliveryCountryName(Val, TagIn);
enc_PhysicalDeliveryCountryName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   'x121-dcc-code' ->
			       encode_restricted_string([], element(2, Val), 18,
							[<<18>>]);
			   'iso-3166-alpha2-code' ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_PhysicalDeliveryCountryName(Tlv) ->
    dec_PhysicalDeliveryCountryName(Tlv, []).

dec_PhysicalDeliveryCountryName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'x121-dcc-code'
      {18, V1} ->
	  {'x121-dcc-code',
	   decode_restricted_string(V1, 3, 18, [])};
      %% 'iso-3166-alpha2-code'
      {19, V1} ->
	  {'iso-3166-alpha2-code',
	   decode_restricted_string(V1, 2, 19, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  PDSName
%%================================
enc_PDSName(Val) -> enc_PDSName(Val, [<<19>>]).

enc_PDSName({'PDSName', Val}, TagIn) ->
    enc_PDSName(Val, TagIn);
enc_PDSName(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_PDSName(Tlv) -> dec_PDSName(Tlv, [19]).

dec_PDSName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 16}, 19, TagIn).

%%================================
%%  TeletexOrganizationalUnitName
%%================================
enc_TeletexOrganizationalUnitName(Val) ->
    enc_TeletexOrganizationalUnitName(Val, [<<20>>]).

enc_TeletexOrganizationalUnitName({'TeletexOrganizationalUnitName',
				   Val},
				  TagIn) ->
    enc_TeletexOrganizationalUnitName(Val, TagIn);
enc_TeletexOrganizationalUnitName(Val, TagIn) ->
    encode_restricted_string([], Val, 20, TagIn).

dec_TeletexOrganizationalUnitName(Tlv) ->
    dec_TeletexOrganizationalUnitName(Tlv, [20]).

dec_TeletexOrganizationalUnitName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 32}, 20, TagIn).

%%================================
%%  TeletexOrganizationalUnitNames
%%================================
enc_TeletexOrganizationalUnitNames(Val) ->
    enc_TeletexOrganizationalUnitNames(Val, [<<48>>]).

enc_TeletexOrganizationalUnitNames({'TeletexOrganizationalUnitNames',
				    Val},
				   TagIn) ->
    enc_TeletexOrganizationalUnitNames(Val, TagIn);
enc_TeletexOrganizationalUnitNames(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_TeletexOrganizationalUnitNames_components(Val, [],
						      0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_TeletexOrganizationalUnitNames_components([],
					      AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_TeletexOrganizationalUnitNames_components([H | T],
					      AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_restricted_string([], H, 20,
						  [<<20>>]),
    enc_TeletexOrganizationalUnitNames_components(T,
						  [EncBytes | AccBytes],
						  AccLen + EncLen).

dec_TeletexOrganizationalUnitNames(Tlv) ->
    dec_TeletexOrganizationalUnitNames(Tlv, [16]).

dec_TeletexOrganizationalUnitNames(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_restricted_string(V1, {1, 32}, 20, [20])
     || V1 <- Tlv1].

%%================================
%%  TeletexPersonalName
%%================================
enc_TeletexPersonalName(Val) ->
    enc_TeletexPersonalName(Val, [<<49>>]).

enc_TeletexPersonalName(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4} = Val,
    %%-------------------------------------------------
    %% attribute surname(1) with type TeletexString
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string([],
						    Cindex1, 20, [<<128>>]),
    %%-------------------------------------------------
    %% attribute given-name(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex2, 20,
							  [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute initials(3) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex3, 20,
							  [<<130>>])
			   end,
    %%-------------------------------------------------
    %% attribute generation-qualifier(4) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case Cindex4 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex4, 20,
							  [<<131>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_TeletexPersonalName(Tlv) ->
    dec_TeletexPersonalName(Tlv, [17]).

dec_TeletexPersonalName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    SetFun = fun (FunTlv) ->
		     case FunTlv of
		       %surname
		       TTlv = {131072, _} -> {1, TTlv};
		       %given-name
		       TTlv = {131073, _} -> {2, TTlv};
		       %initials
		       TTlv = {131074, _} -> {3, TTlv};
		       %generation-qualifier
		       TTlv = {131075, _} -> {4, TTlv};
		       Else -> {5, Else}
		     end
	     end,
    PositionList = [SetFun(TempTlv) || TempTlv <- Tlv1],
    Tlv2 = [Stlv || {_, Stlv} <- lists:sort(PositionList)],
    %%-------------------------------------------------
    %% attribute surname(1) with type TeletexString
    %%-------------------------------------------------
    [V1 | Tlv3] = Tlv2,
    Term1 = decode_restricted_string(V1, {1, 40}, 20,
				     [131072]),
    %%-------------------------------------------------
    %% attribute given-name(2) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv4} = case Tlv3 of
		      [{131073, V2} | TempTlv4] ->
			  {decode_restricted_string(V2, {1, 16}, 20, []),
			   TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute initials(3) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv5} = case Tlv4 of
		      [{131074, V3} | TempTlv5] ->
			  {decode_restricted_string(V3, {1, 5}, 20, []),
			   TempTlv5};
		      _ -> {asn1_NOVALUE, Tlv4}
		    end,
    %%-------------------------------------------------
    %% attribute generation-qualifier(4) with type TeletexString OPTIONAL
    %%-------------------------------------------------
    {Term4, Tlv6} = case Tlv5 of
		      [{131075, V4} | TempTlv6] ->
			  {decode_restricted_string(V4, {1, 3}, 20, []),
			   TempTlv6};
		      _ -> {asn1_NOVALUE, Tlv5}
		    end,
    case Tlv6 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv6}}}) % extra fields not allowed
    end,
    {'TeletexPersonalName', Term1, Term2, Term3, Term4}.

%%================================
%%  TeletexOrganizationName
%%================================
enc_TeletexOrganizationName(Val) ->
    enc_TeletexOrganizationName(Val, [<<20>>]).

enc_TeletexOrganizationName({'TeletexOrganizationName',
			     Val},
			    TagIn) ->
    enc_TeletexOrganizationName(Val, TagIn);
enc_TeletexOrganizationName(Val, TagIn) ->
    encode_restricted_string([], Val, 20, TagIn).

dec_TeletexOrganizationName(Tlv) ->
    dec_TeletexOrganizationName(Tlv, [20]).

dec_TeletexOrganizationName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 64}, 20, TagIn).

%%================================
%%  TeletexCommonName
%%================================
enc_TeletexCommonName(Val) ->
    enc_TeletexCommonName(Val, [<<20>>]).

enc_TeletexCommonName({'TeletexCommonName', Val},
		      TagIn) ->
    enc_TeletexCommonName(Val, TagIn);
enc_TeletexCommonName(Val, TagIn) ->
    encode_restricted_string([], Val, 20, TagIn).

dec_TeletexCommonName(Tlv) ->
    dec_TeletexCommonName(Tlv, [20]).

dec_TeletexCommonName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 64}, 20, TagIn).

%%================================
%%  CommonName
%%================================
enc_CommonName(Val) -> enc_CommonName(Val, [<<19>>]).

enc_CommonName({'CommonName', Val}, TagIn) ->
    enc_CommonName(Val, TagIn);
enc_CommonName(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_CommonName(Tlv) -> dec_CommonName(Tlv, [19]).

dec_CommonName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 64}, 19, TagIn).

%%================================
%%  ExtensionAttribute
%%================================
enc_ExtensionAttribute(Val) ->
    enc_ExtensionAttribute(Val, [<<48>>]).

enc_ExtensionAttribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute extension-attribute-type(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<128>>]),
    %%-------------------------------------------------
    %% attribute extension-attribute-value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2,
					    [<<161>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ExtensionAttribute(Tlv) ->
    dec_ExtensionAttribute(Tlv, [16]).

dec_ExtensionAttribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute extension-attribute-type(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, {0, 256}, [131072]),
    %%-------------------------------------------------
    %% attribute extension-attribute-value(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, [131073]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'ExtensionAttribute', Term1, Term2}.

%%================================
%%  ExtensionAttributes
%%================================
enc_ExtensionAttributes(Val) ->
    enc_ExtensionAttributes(Val, [<<49>>]).

enc_ExtensionAttributes({'ExtensionAttributes', Val},
			TagIn) ->
    enc_ExtensionAttributes(Val, TagIn);
enc_ExtensionAttributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_ExtensionAttributes_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_ExtensionAttributes_components([], AccBytes,
				   AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_ExtensionAttributes_components([H | T], AccBytes,
				   AccLen) ->
    {EncBytes, EncLen} = enc_ExtensionAttribute(H,
						[<<48>>]),
    enc_ExtensionAttributes_components(T,
				       [EncBytes | AccBytes], AccLen + EncLen).

dec_ExtensionAttributes(Tlv) ->
    dec_ExtensionAttributes(Tlv, [17]).

dec_ExtensionAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_ExtensionAttribute(V1, [16]) || V1 <- Tlv1].

%%================================
%%  BuiltInDomainDefinedAttribute
%%================================
enc_BuiltInDomainDefinedAttribute(Val) ->
    enc_BuiltInDomainDefinedAttribute(Val, [<<48>>]).

enc_BuiltInDomainDefinedAttribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type(1) with type PrintableString
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string([],
						    Cindex1, 19, [<<19>>]),
    %%-------------------------------------------------
    %% attribute value(2) with type PrintableString
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_restricted_string([],
						    Cindex2, 19, [<<19>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_BuiltInDomainDefinedAttribute(Tlv) ->
    dec_BuiltInDomainDefinedAttribute(Tlv, [16]).

dec_BuiltInDomainDefinedAttribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type PrintableString
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_restricted_string(V1, {1, 8}, 19, [19]),
    %%-------------------------------------------------
    %% attribute value(2) with type PrintableString
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_restricted_string(V2, {1, 128}, 19,
				     [19]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'BuiltInDomainDefinedAttribute', Term1, Term2}.

%%================================
%%  BuiltInDomainDefinedAttributes
%%================================
enc_BuiltInDomainDefinedAttributes(Val) ->
    enc_BuiltInDomainDefinedAttributes(Val, [<<48>>]).

enc_BuiltInDomainDefinedAttributes({'BuiltInDomainDefinedAttributes',
				    Val},
				   TagIn) ->
    enc_BuiltInDomainDefinedAttributes(Val, TagIn);
enc_BuiltInDomainDefinedAttributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_BuiltInDomainDefinedAttributes_components(Val, [],
						      0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_BuiltInDomainDefinedAttributes_components([],
					      AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_BuiltInDomainDefinedAttributes_components([H | T],
					      AccBytes, AccLen) ->
    {EncBytes, EncLen} =
	enc_BuiltInDomainDefinedAttribute(H, [<<48>>]),
    enc_BuiltInDomainDefinedAttributes_components(T,
						  [EncBytes | AccBytes],
						  AccLen + EncLen).

dec_BuiltInDomainDefinedAttributes(Tlv) ->
    dec_BuiltInDomainDefinedAttributes(Tlv, [16]).

dec_BuiltInDomainDefinedAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_BuiltInDomainDefinedAttribute(V1, [16])
     || V1 <- Tlv1].

%%================================
%%  OrganizationalUnitName
%%================================
enc_OrganizationalUnitName(Val) ->
    enc_OrganizationalUnitName(Val, [<<19>>]).

enc_OrganizationalUnitName({'OrganizationalUnitName',
			    Val},
			   TagIn) ->
    enc_OrganizationalUnitName(Val, TagIn);
enc_OrganizationalUnitName(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_OrganizationalUnitName(Tlv) ->
    dec_OrganizationalUnitName(Tlv, [19]).

dec_OrganizationalUnitName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 32}, 19, TagIn).

%%================================
%%  OrganizationalUnitNames
%%================================
enc_OrganizationalUnitNames(Val) ->
    enc_OrganizationalUnitNames(Val, [<<48>>]).

enc_OrganizationalUnitNames({'OrganizationalUnitNames',
			     Val},
			    TagIn) ->
    enc_OrganizationalUnitNames(Val, TagIn);
enc_OrganizationalUnitNames(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_OrganizationalUnitNames_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_OrganizationalUnitNames_components([], AccBytes,
				       AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_OrganizationalUnitNames_components([H | T],
				       AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_restricted_string([], H, 19,
						  [<<19>>]),
    enc_OrganizationalUnitNames_components(T,
					   [EncBytes | AccBytes],
					   AccLen + EncLen).

dec_OrganizationalUnitNames(Tlv) ->
    dec_OrganizationalUnitNames(Tlv, [16]).

dec_OrganizationalUnitNames(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_restricted_string(V1, {1, 32}, 19, [19])
     || V1 <- Tlv1].

%%================================
%%  PersonalName
%%================================
enc_PersonalName(Val) ->
    enc_PersonalName(Val, [<<49>>]).

enc_PersonalName(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4} = Val,
    %%-------------------------------------------------
    %% attribute surname(1) with type PrintableString
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string([],
						    Cindex1, 19, [<<128>>]),
    %%-------------------------------------------------
    %% attribute given-name(2) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex2, 19,
							  [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute initials(3) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex3, 19,
							  [<<130>>])
			   end,
    %%-------------------------------------------------
    %% attribute generation-qualifier(4) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case Cindex4 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex4, 19,
							  [<<131>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PersonalName(Tlv) -> dec_PersonalName(Tlv, [17]).

dec_PersonalName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    SetFun = fun (FunTlv) ->
		     case FunTlv of
		       %surname
		       TTlv = {131072, _} -> {1, TTlv};
		       %given-name
		       TTlv = {131073, _} -> {2, TTlv};
		       %initials
		       TTlv = {131074, _} -> {3, TTlv};
		       %generation-qualifier
		       TTlv = {131075, _} -> {4, TTlv};
		       Else -> {5, Else}
		     end
	     end,
    PositionList = [SetFun(TempTlv) || TempTlv <- Tlv1],
    Tlv2 = [Stlv || {_, Stlv} <- lists:sort(PositionList)],
    %%-------------------------------------------------
    %% attribute surname(1) with type PrintableString
    %%-------------------------------------------------
    [V1 | Tlv3] = Tlv2,
    Term1 = decode_restricted_string(V1, {1, 40}, 19,
				     [131072]),
    %%-------------------------------------------------
    %% attribute given-name(2) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv4} = case Tlv3 of
		      [{131073, V2} | TempTlv4] ->
			  {decode_restricted_string(V2, {1, 16}, 19, []),
			   TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute initials(3) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv5} = case Tlv4 of
		      [{131074, V3} | TempTlv5] ->
			  {decode_restricted_string(V3, {1, 5}, 19, []),
			   TempTlv5};
		      _ -> {asn1_NOVALUE, Tlv4}
		    end,
    %%-------------------------------------------------
    %% attribute generation-qualifier(4) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term4, Tlv6} = case Tlv5 of
		      [{131075, V4} | TempTlv6] ->
			  {decode_restricted_string(V4, {1, 3}, 19, []),
			   TempTlv6};
		      _ -> {asn1_NOVALUE, Tlv5}
		    end,
    case Tlv6 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv6}}}) % extra fields not allowed
    end,
    {'PersonalName', Term1, Term2, Term3, Term4}.

%%================================
%%  NumericUserIdentifier
%%================================
enc_NumericUserIdentifier(Val) ->
    enc_NumericUserIdentifier(Val, [<<18>>]).

enc_NumericUserIdentifier({'NumericUserIdentifier',
			   Val},
			  TagIn) ->
    enc_NumericUserIdentifier(Val, TagIn);
enc_NumericUserIdentifier(Val, TagIn) ->
    encode_restricted_string([], Val, 18, TagIn).

dec_NumericUserIdentifier(Tlv) ->
    dec_NumericUserIdentifier(Tlv, [18]).

dec_NumericUserIdentifier(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 32}, 18, TagIn).

%%================================
%%  OrganizationName
%%================================
enc_OrganizationName(Val) ->
    enc_OrganizationName(Val, [<<19>>]).

enc_OrganizationName({'OrganizationName', Val},
		     TagIn) ->
    enc_OrganizationName(Val, TagIn);
enc_OrganizationName(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_OrganizationName(Tlv) ->
    dec_OrganizationName(Tlv, [19]).

dec_OrganizationName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 64}, 19, TagIn).

%%================================
%%  PrivateDomainName
%%================================
enc_PrivateDomainName(Val) ->
    enc_PrivateDomainName(Val, []).

enc_PrivateDomainName({'PrivateDomainName', Val},
		      TagIn) ->
    enc_PrivateDomainName(Val, TagIn);
enc_PrivateDomainName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   numeric ->
			       encode_restricted_string([], element(2, Val), 18,
							[<<18>>]);
			   printable ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_PrivateDomainName(Tlv) ->
    dec_PrivateDomainName(Tlv, []).

dec_PrivateDomainName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'numeric'
      {18, V1} ->
	  {numeric,
	   decode_restricted_string(V1, {1, 16}, 18, [])};
      %% 'printable'
      {19, V1} ->
	  {printable,
	   decode_restricted_string(V1, {1, 16}, 19, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  TerminalIdentifier
%%================================
enc_TerminalIdentifier(Val) ->
    enc_TerminalIdentifier(Val, [<<19>>]).

enc_TerminalIdentifier({'TerminalIdentifier', Val},
		       TagIn) ->
    enc_TerminalIdentifier(Val, TagIn);
enc_TerminalIdentifier(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_TerminalIdentifier(Tlv) ->
    dec_TerminalIdentifier(Tlv, [19]).

dec_TerminalIdentifier(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 24}, 19, TagIn).

%%================================
%%  X121Address
%%================================
enc_X121Address(Val) -> enc_X121Address(Val, [<<18>>]).

enc_X121Address({'X121Address', Val}, TagIn) ->
    enc_X121Address(Val, TagIn);
enc_X121Address(Val, TagIn) ->
    encode_restricted_string([], Val, 18, TagIn).

dec_X121Address(Tlv) -> dec_X121Address(Tlv, [18]).

dec_X121Address(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 16}, 18, TagIn).

%%================================
%%  NetworkAddress
%%================================
enc_NetworkAddress(Val) ->
    enc_NetworkAddress(Val, [<<18>>]).

enc_NetworkAddress({'NetworkAddress', Val}, TagIn) ->
    enc_NetworkAddress(Val, TagIn);
enc_NetworkAddress(Val, TagIn) ->
    encode_restricted_string([], Val, 18, TagIn).

dec_NetworkAddress(Tlv) ->
    dec_NetworkAddress(Tlv, [18]).

dec_NetworkAddress(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 16}, 18, TagIn).

%%================================
%%  AdministrationDomainName
%%================================
enc_AdministrationDomainName(Val) ->
    enc_AdministrationDomainName(Val, [<<98>>]).

enc_AdministrationDomainName({'AdministrationDomainName',
			      Val},
			     TagIn) ->
    enc_AdministrationDomainName(Val, TagIn);
enc_AdministrationDomainName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   numeric ->
			       encode_restricted_string([], element(2, Val), 18,
							[<<18>>]);
			   printable ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_AdministrationDomainName(Tlv) ->
    dec_AdministrationDomainName(Tlv, [65538]).

dec_AdministrationDomainName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'numeric'
      {18, V1} ->
	  {numeric,
	   decode_restricted_string(V1, {0, 16}, 18, [])};
      %% 'printable'
      {19, V1} ->
	  {printable,
	   decode_restricted_string(V1, {0, 16}, 19, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  CountryName
%%================================
enc_CountryName(Val) -> enc_CountryName(Val, [<<97>>]).

enc_CountryName({'CountryName', Val}, TagIn) ->
    enc_CountryName(Val, TagIn);
enc_CountryName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   'x121-dcc-code' ->
			       encode_restricted_string([], element(2, Val), 18,
							[<<18>>]);
			   'iso-3166-alpha2-code' ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_CountryName(Tlv) -> dec_CountryName(Tlv, [65537]).

dec_CountryName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'x121-dcc-code'
      {18, V1} ->
	  {'x121-dcc-code',
	   decode_restricted_string(V1, 3, 18, [])};
      %% 'iso-3166-alpha2-code'
      {19, V1} ->
	  {'iso-3166-alpha2-code',
	   decode_restricted_string(V1, 2, 19, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  BuiltInStandardAttributes
%%================================
enc_BuiltInStandardAttributes(Val) ->
    enc_BuiltInStandardAttributes(Val, [<<48>>]).

enc_BuiltInStandardAttributes(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7, Cindex8, Cindex9} =
	Val,
    %%-------------------------------------------------
    %% attribute country-name(1)   External OTP-PKIX:CountryName OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_CountryName(Cindex1, [<<97>>])
			   end,
    %%-------------------------------------------------
    %% attribute administration-domain-name(2)   External OTP-PKIX:AdministrationDomainName OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_AdministrationDomainName(Cindex2, [<<98>>])
			   end,
    %%-------------------------------------------------
    %% attribute network-address(3) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex3, 18,
							  [<<128>>])
			   end,
    %%-------------------------------------------------
    %% attribute terminal-identifier(4) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = case Cindex4 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex4, 19,
							  [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute private-domain-name(5)   External OTP-PKIX:PrivateDomainName OPTIONAL
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = case Cindex5 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_PrivateDomainName(Cindex5, [<<162>>])
			   end,
    %%-------------------------------------------------
    %% attribute organization-name(6) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = case Cindex6 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex6, 19,
							  [<<131>>])
			   end,
    %%-------------------------------------------------
    %% attribute numeric-user-identifier(7) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {EncBytes7, EncLen7} = case Cindex7 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_restricted_string([], Cindex7, 18,
							  [<<132>>])
			   end,
    %%-------------------------------------------------
    %% attribute personal-name(8)   External OTP-PKIX:PersonalName OPTIONAL
    %%-------------------------------------------------
    {EncBytes8, EncLen8} = case Cindex8 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_PersonalName(Cindex8, [<<165>>])
			   end,
    %%-------------------------------------------------
    %% attribute organizational-unit-names(9)   External OTP-PKIX:OrganizationalUnitNames OPTIONAL
    %%-------------------------------------------------
    {EncBytes9, EncLen9} = case Cindex9 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_OrganizationalUnitNames(Cindex9, [<<166>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7, EncBytes8,
		  EncBytes9],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7
		 + EncLen8
		 + EncLen9,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_BuiltInStandardAttributes(Tlv) ->
    dec_BuiltInStandardAttributes(Tlv, [16]).

dec_BuiltInStandardAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute country-name(1)   External OTP-PKIX:CountryName OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{65537, V1} | TempTlv2] ->
			  {dec_CountryName(V1, []), TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute administration-domain-name(2)   External OTP-PKIX:AdministrationDomainName OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{65538, V2} | TempTlv3] ->
			  {dec_AdministrationDomainName(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute network-address(3) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{131072, V3} | TempTlv4] ->
			  {decode_restricted_string(V3, {1, 16}, 18, []),
			   TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    %%-------------------------------------------------
    %% attribute terminal-identifier(4) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term4, Tlv5} = case Tlv4 of
		      [{131073, V4} | TempTlv5] ->
			  {decode_restricted_string(V4, {1, 24}, 19, []),
			   TempTlv5};
		      _ -> {asn1_NOVALUE, Tlv4}
		    end,
    %%-------------------------------------------------
    %% attribute private-domain-name(5)   External OTP-PKIX:PrivateDomainName OPTIONAL
    %%-------------------------------------------------
    {Term5, Tlv6} = case Tlv5 of
		      [{131074, V5} | TempTlv6] ->
			  {dec_PrivateDomainName(V5, []), TempTlv6};
		      _ -> {asn1_NOVALUE, Tlv5}
		    end,
    %%-------------------------------------------------
    %% attribute organization-name(6) with type PrintableString OPTIONAL
    %%-------------------------------------------------
    {Term6, Tlv7} = case Tlv6 of
		      [{131075, V6} | TempTlv7] ->
			  {decode_restricted_string(V6, {1, 64}, 19, []),
			   TempTlv7};
		      _ -> {asn1_NOVALUE, Tlv6}
		    end,
    %%-------------------------------------------------
    %% attribute numeric-user-identifier(7) with type NumericString OPTIONAL
    %%-------------------------------------------------
    {Term7, Tlv8} = case Tlv7 of
		      [{131076, V7} | TempTlv8] ->
			  {decode_restricted_string(V7, {1, 32}, 18, []),
			   TempTlv8};
		      _ -> {asn1_NOVALUE, Tlv7}
		    end,
    %%-------------------------------------------------
    %% attribute personal-name(8)   External OTP-PKIX:PersonalName OPTIONAL
    %%-------------------------------------------------
    {Term8, Tlv9} = case Tlv8 of
		      [{131077, V8} | TempTlv9] ->
			  {dec_PersonalName(V8, []), TempTlv9};
		      _ -> {asn1_NOVALUE, Tlv8}
		    end,
    %%-------------------------------------------------
    %% attribute organizational-unit-names(9)   External OTP-PKIX:OrganizationalUnitNames OPTIONAL
    %%-------------------------------------------------
    {Term9, Tlv10} = case Tlv9 of
		       [{131078, V9} | TempTlv10] ->
			   {dec_OrganizationalUnitNames(V9, []), TempTlv10};
		       _ -> {asn1_NOVALUE, Tlv9}
		     end,
    case Tlv10 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv10}}}) % extra fields not allowed
    end,
    {'BuiltInStandardAttributes', Term1, Term2, Term3,
     Term4, Term5, Term6, Term7, Term8, Term9}.

%%================================
%%  ORAddress
%%================================
enc_ORAddress(Val) -> enc_ORAddress(Val, [<<48>>]).

enc_ORAddress(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute built-in-standard-attributes(1)   External OTP-PKIX:BuiltInStandardAttributes
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
	enc_BuiltInStandardAttributes(Cindex1, [<<48>>]),
    %%-------------------------------------------------
    %% attribute built-in-domain-defined-attributes(2)   External OTP-PKIX:BuiltInDomainDefinedAttributes OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_BuiltInDomainDefinedAttributes(Cindex2,
								    [<<48>>])
			   end,
    %%-------------------------------------------------
    %% attribute extension-attributes(3)   External OTP-PKIX:ExtensionAttributes OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_ExtensionAttributes(Cindex3, [<<49>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ORAddress(Tlv) -> dec_ORAddress(Tlv, [16]).

dec_ORAddress(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute built-in-standard-attributes(1)   External OTP-PKIX:BuiltInStandardAttributes
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_BuiltInStandardAttributes(V1, [16]),
    %%-------------------------------------------------
    %% attribute built-in-domain-defined-attributes(2)   External OTP-PKIX:BuiltInDomainDefinedAttributes OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{16, V2} | TempTlv3] ->
			  {dec_BuiltInDomainDefinedAttributes(V2, []),
			   TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute extension-attributes(3)   External OTP-PKIX:ExtensionAttributes OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{17, V3} | TempTlv4] ->
			  {dec_ExtensionAttributes(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'ORAddress', Term1, Term2, Term3}.

%%================================
%%  AlgorithmIdentifier
%%================================
enc_AlgorithmIdentifier(Val) ->
    enc_AlgorithmIdentifier(Val, [<<48>>]).

enc_AlgorithmIdentifier(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute algorithm(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_open_type(Cindex2, [])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AlgorithmIdentifier(Tlv) ->
    dec_AlgorithmIdentifier(Tlv, [16]).

dec_AlgorithmIdentifier(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [V2 | TempTlv3] ->
			  {decode_open_type_as_binary(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'AlgorithmIdentifier', Term1, Term2}.

%%================================
%%  TBSCertList
%%================================
enc_TBSCertList(Val) -> enc_TBSCertList(Val, [<<48>>]).

enc_TBSCertList(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case Cindex1 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 encode_integer([], Cindex1,
						[{v1, 0}, {v2, 1}, {v3, 2}],
						[<<2>>])
			   end,
    %%-------------------------------------------------
    %% attribute signature(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_AlgorithmIdentifier(Cindex2,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuer(3)   External OTP-PKIX:Name
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_Name(Cindex3, []),
    %%-------------------------------------------------
    %% attribute thisUpdate(4)   External OTP-PKIX:Time
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = enc_Time(Cindex4, []),
    %%-------------------------------------------------
    %% attribute nextUpdate(5)   External OTP-PKIX:Time OPTIONAL
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = case Cindex5 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_Time(Cindex5, [])
			   end,
    %%-------------------------------------------------
    %% attribute revokedCertificates(6) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = case Cindex6 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 enc_TBSCertList_revokedCertificates(Cindex6,
								     [<<48>>])
			   end,
    %%-------------------------------------------------
    %% attribute crlExtensions(7)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {EncBytes7, EncLen7} = case Cindex7 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_Extensions(Cindex7, [<<48>>, <<160>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  TBSCertList_revokedCertificates
%%================================

enc_TBSCertList_revokedCertificates({'TBSCertList_revokedCertificates',
				     Val},
				    TagIn) ->
    enc_TBSCertList_revokedCertificates(Val, TagIn);
enc_TBSCertList_revokedCertificates(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_TBSCertList_revokedCertificates_components(Val, [],
						       0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_TBSCertList_revokedCertificates_components([],
					       AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_TBSCertList_revokedCertificates_components([H | T],
					       AccBytes, AccLen) ->
    {EncBytes, EncLen} =
	enc_TBSCertList_revokedCertificates_SEQOF(H, [<<48>>]),
    enc_TBSCertList_revokedCertificates_components(T,
						   [EncBytes | AccBytes],
						   AccLen + EncLen).

%%================================
%%  TBSCertList_revokedCertificates_SEQOF
%%================================
enc_TBSCertList_revokedCertificates_SEQOF(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute userCertificate(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute revocationDate(2)   External OTP-PKIX:Time
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_Time(Cindex2, []),
    %%-------------------------------------------------
    %% attribute crlEntryExtensions(3)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = case Cindex3 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> enc_Extensions(Cindex3, [<<48>>])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_TBSCertList_revokedCertificates_SEQOF(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute userCertificate(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute revocationDate(2)   External OTP-PKIX:Time
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_Time(V2, []),
    %%-------------------------------------------------
    %% attribute crlEntryExtensions(3)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {Term3, Tlv4} = case Tlv3 of
		      [{16, V3} | TempTlv4] ->
			  {dec_Extensions(V3, []), TempTlv4};
		      _ -> {asn1_NOVALUE, Tlv3}
		    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'TBSCertList_revokedCertificates_SEQOF', Term1, Term2,
     Term3}.

dec_TBSCertList_revokedCertificates(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_TBSCertList_revokedCertificates_SEQOF(V1, [16])
     || V1 <- Tlv1].

dec_TBSCertList(Tlv) -> dec_TBSCertList(Tlv, [16]).

dec_TBSCertList(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER OPTIONAL
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{2, V1} | TempTlv2] ->
			  {decode_integer(V1, [], [{v1, 0}, {v2, 1}, {v3, 2}],
					  []),
			   TempTlv2};
		      _ -> {asn1_NOVALUE, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute signature(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AlgorithmIdentifier(V2, [16]),
    %%-------------------------------------------------
    %% attribute issuer(3)   External OTP-PKIX:Name
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_Name(V3, []),
    %%-------------------------------------------------
    %% attribute thisUpdate(4)   External OTP-PKIX:Time
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = dec_Time(V4, []),
    %%-------------------------------------------------
    %% attribute nextUpdate(5)   External OTP-PKIX:Time OPTIONAL
    %%-------------------------------------------------
    {Term5, Tlv6} = case Tlv5 of
		      [V5 = {23, _} | TempTlv6] ->
			  {dec_Time(V5, []), TempTlv6};
		      [V5 = {24, _} | TempTlv6] ->
			  {dec_Time(V5, []), TempTlv6};
		      _ -> {asn1_NOVALUE, Tlv5}
		    end,
    %%-------------------------------------------------
    %% attribute revokedCertificates(6) with type SEQUENCE OF OPTIONAL
    %%-------------------------------------------------
    {Term6, Tlv7} = case Tlv6 of
		      [{16, V6} | TempTlv7] ->
			  {dec_TBSCertList_revokedCertificates(V6, []),
			   TempTlv7};
		      _ -> {asn1_NOVALUE, Tlv6}
		    end,
    %%-------------------------------------------------
    %% attribute crlExtensions(7)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {Term7, Tlv8} = case Tlv7 of
		      [{131072, V7} | TempTlv8] ->
			  {dec_Extensions(V7, [16]), TempTlv8};
		      _ -> {asn1_NOVALUE, Tlv7}
		    end,
    case Tlv8 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv8}}}) % extra fields not allowed
    end,
    {'TBSCertList', Term1, Term2, Term3, Term4, Term5,
     Term6, Term7}.

%%================================
%%  CertificateList
%%================================
enc_CertificateList(Val) ->
    enc_CertificateList(Val, [<<48>>]).

enc_CertificateList(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute tbsCertList(1)   External OTP-PKIX:TBSCertList
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_TBSCertList(Cindex1,
					   [<<48>>]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_AlgorithmIdentifier(Cindex2,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_bit_string([], Cindex3,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_CertificateList(Tlv) ->
    dec_CertificateList(Tlv, [16]).

dec_CertificateList(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute tbsCertList(1)   External OTP-PKIX:TBSCertList
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_TBSCertList(V1, [16]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AlgorithmIdentifier(V2, [16]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_compact_bit_string(V3, [], [], [3]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'CertificateList', Term1, Term2, Term3}.

%%================================
%%  Extension
%%================================
enc_Extension(Val) -> enc_Extension(Val, [<<48>>]).

enc_Extension(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute extnID(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch check_bool(false,
						 Cindex2)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex2, [<<1>>])
			   end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_octet_string([], Cindex3,
					       [<<4>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Extension(Tlv) -> dec_Extension(Tlv, [16]).

dec_Extension(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute extnID(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{1, V2} | TempTlv3] ->
			  {decode_boolean(V2, []), TempTlv3};
		      _ -> {false, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type OCTET STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_octet_string(V3, [], [4]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Extension', Term1, Term2, Term3}.

%%================================
%%  Extensions
%%================================
enc_Extensions(Val) -> enc_Extensions(Val, [<<48>>]).

enc_Extensions({'Extensions', Val}, TagIn) ->
    enc_Extensions(Val, TagIn);
enc_Extensions(Val, TagIn) ->
    {EncBytes, EncLen} = enc_Extensions_components(Val, [],
						   0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_Extensions_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_Extensions_components([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_Extension(H, [<<48>>]),
    enc_Extensions_components(T, [EncBytes | AccBytes],
			      AccLen + EncLen).

dec_Extensions(Tlv) -> dec_Extensions(Tlv, [16]).

dec_Extensions(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Extension(V1, [16]) || V1 <- Tlv1].

%%================================
%%  SubjectPublicKeyInfo
%%================================
enc_SubjectPublicKeyInfo(Val) ->
    enc_SubjectPublicKeyInfo(Val, [<<48>>]).

enc_SubjectPublicKeyInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute algorithm(1)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_AlgorithmIdentifier(Cindex1,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_bit_string([], Cindex2,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SubjectPublicKeyInfo(Tlv) ->
    dec_SubjectPublicKeyInfo(Tlv, [16]).

dec_SubjectPublicKeyInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_AlgorithmIdentifier(V1, [16]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type BIT STRING
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_compact_bit_string(V2, [], [], [3]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SubjectPublicKeyInfo', Term1, Term2}.

%%================================
%%  UniqueIdentifier
%%================================
enc_UniqueIdentifier(Val) ->
    enc_UniqueIdentifier(Val, [<<3>>]).

enc_UniqueIdentifier({'UniqueIdentifier', Val},
		     TagIn) ->
    enc_UniqueIdentifier(Val, TagIn);
enc_UniqueIdentifier(Val, TagIn) ->
    encode_bit_string([], Val, [], TagIn).

dec_UniqueIdentifier(Tlv) ->
    dec_UniqueIdentifier(Tlv, [3]).

dec_UniqueIdentifier(Tlv, TagIn) ->
    decode_compact_bit_string(Tlv, [], [], TagIn).

%%================================
%%  Time
%%================================
enc_Time(Val) -> enc_Time(Val, []).

enc_Time({'Time', Val}, TagIn) -> enc_Time(Val, TagIn);
enc_Time(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   utcTime ->
			       encode_utc_time([], element(2, Val), [<<23>>]);
			   generalTime ->
			       encode_generalized_time([], element(2, Val),
						       [<<24>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_Time(Tlv) -> dec_Time(Tlv, []).

dec_Time(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'utcTime'
      {23, V1} -> {utcTime, decode_utc_time(V1, [], [])};
      %% 'generalTime'
      {24, V1} ->
	  {generalTime, decode_generalized_time(V1, [], [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  Validity
%%================================
enc_Validity(Val) -> enc_Validity(Val, [<<48>>]).

enc_Validity(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute notBefore(1)   External OTP-PKIX:Time
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_Time(Cindex1, []),
    %%-------------------------------------------------
    %% attribute notAfter(2)   External OTP-PKIX:Time
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_Time(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Validity(Tlv) -> dec_Validity(Tlv, [16]).

dec_Validity(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute notBefore(1)   External OTP-PKIX:Time
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_Time(V1, []),
    %%-------------------------------------------------
    %% attribute notAfter(2)   External OTP-PKIX:Time
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_Time(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'Validity', Term1, Term2}.

%%================================
%%  CertificateSerialNumber
%%================================
enc_CertificateSerialNumber(Val) ->
    enc_CertificateSerialNumber(Val, [<<2>>]).

enc_CertificateSerialNumber({'CertificateSerialNumber',
			     Val},
			    TagIn) ->
    enc_CertificateSerialNumber(Val, TagIn);
enc_CertificateSerialNumber(Val, TagIn) ->
    encode_integer([], Val, TagIn).

dec_CertificateSerialNumber(Tlv) ->
    dec_CertificateSerialNumber(Tlv, [2]).

dec_CertificateSerialNumber(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  VersionPKIX1Explicit88
%%================================
enc_VersionPKIX1Explicit88(Val) ->
    enc_VersionPKIX1Explicit88(Val, [<<2>>]).

enc_VersionPKIX1Explicit88({'VersionPKIX1Explicit88',
			    Val},
			   TagIn) ->
    enc_VersionPKIX1Explicit88(Val, TagIn);
enc_VersionPKIX1Explicit88(Val, TagIn) ->
    encode_integer([], Val, [{v1, 0}, {v2, 1}, {v3, 2}],
		   TagIn).

dec_VersionPKIX1Explicit88(Tlv) ->
    dec_VersionPKIX1Explicit88(Tlv, [2]).

dec_VersionPKIX1Explicit88(Tlv, TagIn) ->
    decode_integer(Tlv, [], [{v1, 0}, {v2, 1}, {v3, 2}],
		   TagIn).

%%================================
%%  TBSCertificate
%%================================
enc_TBSCertificate(Val) ->
    enc_TBSCertificate(Val, [<<48>>]).

enc_TBSCertificate(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7, Cindex8, Cindex9, Cindex10} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case catch check_int(0, Cindex1,
						[{v1, 0}, {v2, 1}, {v3, 2}])
			       of
			     true -> {[], 0};
			     _ ->
				 encode_integer([], Cindex1,
						[{v1, 0}, {v2, 1}, {v3, 2}],
						[<<2>>, <<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute serialNumber(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute signature(3)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_AlgorithmIdentifier(Cindex3,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuer(4)   External OTP-PKIX:Name
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = enc_Name(Cindex4, []),
    %%-------------------------------------------------
    %% attribute validity(5)   External OTP-PKIX:Validity
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = enc_Validity(Cindex5, [<<48>>]),
    %%-------------------------------------------------
    %% attribute subject(6)   External OTP-PKIX:Name
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = enc_Name(Cindex6, []),
    %%-------------------------------------------------
    %% attribute subjectPublicKeyInfo(7)   External OTP-PKIX:SubjectPublicKeyInfo
    %%-------------------------------------------------
    {EncBytes7, EncLen7} = enc_SubjectPublicKeyInfo(Cindex7,
						    [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes8, EncLen8} = case Cindex8 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex8, [], [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute subjectUniqueID(9) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes9, EncLen9} = case Cindex9 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex9, [], [<<130>>])
			   end,
    %%-------------------------------------------------
    %% attribute extensions(10)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {EncBytes10, EncLen10} = case Cindex10 of
			       asn1_NOVALUE -> {<<>>, 0};
			       _ -> enc_Extensions(Cindex10, [<<48>>, <<163>>])
			     end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7, EncBytes8,
		  EncBytes9, EncBytes10],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7
		 + EncLen8
		 + EncLen9
		 + EncLen10,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_TBSCertificate(Tlv) ->
    dec_TBSCertificate(Tlv, [16]).

dec_TBSCertificate(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_integer(V1, [], [{v1, 0}, {v2, 1}, {v3, 2}],
					  [2]),
			   TempTlv2};
		      _ -> {0, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute serialNumber(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute signature(3)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_AlgorithmIdentifier(V3, [16]),
    %%-------------------------------------------------
    %% attribute issuer(4)   External OTP-PKIX:Name
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = dec_Name(V4, []),
    %%-------------------------------------------------
    %% attribute validity(5)   External OTP-PKIX:Validity
    %%-------------------------------------------------
    [V5 | Tlv6] = Tlv5,
    Term5 = dec_Validity(V5, [16]),
    %%-------------------------------------------------
    %% attribute subject(6)   External OTP-PKIX:Name
    %%-------------------------------------------------
    [V6 | Tlv7] = Tlv6,
    Term6 = dec_Name(V6, []),
    %%-------------------------------------------------
    %% attribute subjectPublicKeyInfo(7)   External OTP-PKIX:SubjectPublicKeyInfo
    %%-------------------------------------------------
    [V7 | Tlv8] = Tlv7,
    Term7 = dec_SubjectPublicKeyInfo(V7, [16]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term8, Tlv9} = case Tlv8 of
		      [{131073, V8} | TempTlv9] ->
			  {decode_compact_bit_string(V8, [], [], []), TempTlv9};
		      _ -> {asn1_NOVALUE, Tlv8}
		    end,
    %%-------------------------------------------------
    %% attribute subjectUniqueID(9) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term9, Tlv10} = case Tlv9 of
		       [{131074, V9} | TempTlv10] ->
			   {decode_compact_bit_string(V9, [], [], []),
			    TempTlv10};
		       _ -> {asn1_NOVALUE, Tlv9}
		     end,
    %%-------------------------------------------------
    %% attribute extensions(10)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {Term10, Tlv11} = case Tlv10 of
			[{131075, V10} | TempTlv11] ->
			    {dec_Extensions(V10, [16]), TempTlv11};
			_ -> {asn1_NOVALUE, Tlv10}
		      end,
    case Tlv11 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv11}}}) % extra fields not allowed
    end,
    {'TBSCertificate', Term1, Term2, Term3, Term4, Term5,
     Term6, Term7, Term8, Term9, Term10}.

%%================================
%%  Certificate
%%================================
enc_Certificate(Val) -> enc_Certificate(Val, [<<48>>]).

enc_Certificate(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute tbsCertificate(1)   External OTP-PKIX:TBSCertificate
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_TBSCertificate(Cindex1,
					      [<<48>>]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_AlgorithmIdentifier(Cindex2,
						   [<<48>>]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_bit_string([], Cindex3,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_Certificate(Tlv) -> dec_Certificate(Tlv, [16]).

dec_Certificate(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute tbsCertificate(1)   External OTP-PKIX:TBSCertificate
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_TBSCertificate(V1, [16]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AlgorithmIdentifier(V2, [16]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_compact_bit_string(V3, [], [], [3]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Certificate', Term1, Term2, Term3}.

%%================================
%%  DirectoryString
%%================================
enc_DirectoryString(Val) ->
    enc_DirectoryString(Val, []).

enc_DirectoryString({'DirectoryString', Val}, TagIn) ->
    enc_DirectoryString(Val, TagIn);
enc_DirectoryString(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_DirectoryString(Tlv) ->
    dec_DirectoryString(Tlv, []).

dec_DirectoryString(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 'MAX'}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 'MAX'}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 'MAX'}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 'MAX'}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  RelativeDistinguishedName
%%================================
enc_RelativeDistinguishedName(Val) ->
    enc_RelativeDistinguishedName(Val, [<<49>>]).

enc_RelativeDistinguishedName({'RelativeDistinguishedName',
			       Val},
			      TagIn) ->
    enc_RelativeDistinguishedName(Val, TagIn);
enc_RelativeDistinguishedName(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_RelativeDistinguishedName_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_RelativeDistinguishedName_components([], AccBytes,
					 AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_RelativeDistinguishedName_components([H | T],
					 AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_AttributeTypeAndValue(H,
						   [<<48>>]),
    enc_RelativeDistinguishedName_components(T,
					     [EncBytes | AccBytes],
					     AccLen + EncLen).

dec_RelativeDistinguishedName(Tlv) ->
    dec_RelativeDistinguishedName(Tlv, [17]).

dec_RelativeDistinguishedName(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_AttributeTypeAndValue(V1, [16]) || V1 <- Tlv1].

%%================================
%%  DistinguishedName
%%================================
enc_DistinguishedName(Val) ->
    enc_DistinguishedName(Val, [<<48>>]).

enc_DistinguishedName({'DistinguishedName', Val},
		      TagIn) ->
    enc_DistinguishedName(Val, TagIn);
enc_DistinguishedName(Val, TagIn) ->
    enc_RDNSequence(Val, TagIn).

dec_DistinguishedName(Tlv) ->
    dec_DistinguishedName(Tlv, [16]).

dec_DistinguishedName(Tlv, TagIn) ->
    dec_RDNSequence(Tlv, TagIn).

%%================================
%%  RDNSequence
%%================================
enc_RDNSequence(Val) -> enc_RDNSequence(Val, [<<48>>]).

enc_RDNSequence({'RDNSequence', Val}, TagIn) ->
    enc_RDNSequence(Val, TagIn);
enc_RDNSequence(Val, TagIn) ->
    {EncBytes, EncLen} = enc_RDNSequence_components(Val, [],
						    0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_RDNSequence_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_RDNSequence_components([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = enc_RelativeDistinguishedName(H,
						       [<<49>>]),
    enc_RDNSequence_components(T, [EncBytes | AccBytes],
			       AccLen + EncLen).

dec_RDNSequence(Tlv) -> dec_RDNSequence(Tlv, [16]).

dec_RDNSequence(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_RelativeDistinguishedName(V1, [17]) || V1 <- Tlv1].

%%================================
%%  Name
%%================================
enc_Name(Val) -> enc_Name(Val, []).

enc_Name({'Name', Val}, TagIn) -> enc_Name(Val, TagIn);
enc_Name(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   rdnSequence ->
			       enc_RDNSequence(element(2, Val), [<<48>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_Name(Tlv) -> dec_Name(Tlv, []).

dec_Name(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'rdnSequence'
      {16, V1} -> {rdnSequence, dec_RDNSequence(V1, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  EmailAddress
%%================================
enc_EmailAddress(Val) ->
    enc_EmailAddress(Val, [<<22>>]).

enc_EmailAddress({'EmailAddress', Val}, TagIn) ->
    enc_EmailAddress(Val, TagIn);
enc_EmailAddress(Val, TagIn) ->
    encode_restricted_string([], Val, 22, TagIn).

dec_EmailAddress(Tlv) -> dec_EmailAddress(Tlv, [22]).

dec_EmailAddress(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 128}, 22, TagIn).

%%================================
%%  DomainComponent
%%================================
enc_DomainComponent(Val) ->
    enc_DomainComponent(Val, [<<22>>]).

enc_DomainComponent({'DomainComponent', Val}, TagIn) ->
    enc_DomainComponent(Val, TagIn);
enc_DomainComponent(Val, TagIn) ->
    encode_restricted_string([], Val, 22, TagIn).

dec_DomainComponent(Tlv) ->
    dec_DomainComponent(Tlv, [22]).

dec_DomainComponent(Tlv, TagIn) ->
    decode_restricted_string(Tlv, [], 22, TagIn).

%%================================
%%  X520Pseudonym
%%================================
enc_X520Pseudonym(Val) -> enc_X520Pseudonym(Val, []).

enc_X520Pseudonym({'X520Pseudonym', Val}, TagIn) ->
    enc_X520Pseudonym(Val, TagIn);
enc_X520Pseudonym(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520Pseudonym(Tlv) -> dec_X520Pseudonym(Tlv, []).

dec_X520Pseudonym(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 128}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 128}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 128}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 128}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520SerialNumber
%%================================
enc_X520SerialNumber(Val) ->
    enc_X520SerialNumber(Val, [<<19>>]).

enc_X520SerialNumber({'X520SerialNumber', Val},
		     TagIn) ->
    enc_X520SerialNumber(Val, TagIn);
enc_X520SerialNumber(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_X520SerialNumber(Tlv) ->
    dec_X520SerialNumber(Tlv, [19]).

dec_X520SerialNumber(Tlv, TagIn) ->
    decode_restricted_string(Tlv, {1, 64}, 19, TagIn).

%%================================
%%  X520countryName
%%================================
enc_X520countryName(Val) ->
    enc_X520countryName(Val, [<<19>>]).

enc_X520countryName({'X520countryName', Val}, TagIn) ->
    enc_X520countryName(Val, TagIn);
enc_X520countryName(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_X520countryName(Tlv) ->
    dec_X520countryName(Tlv, [19]).

dec_X520countryName(Tlv, TagIn) ->
    decode_restricted_string(Tlv, 2, 19, TagIn).

%%================================
%%  X520dnQualifier
%%================================
enc_X520dnQualifier(Val) ->
    enc_X520dnQualifier(Val, [<<19>>]).

enc_X520dnQualifier({'X520dnQualifier', Val}, TagIn) ->
    enc_X520dnQualifier(Val, TagIn);
enc_X520dnQualifier(Val, TagIn) ->
    encode_restricted_string([], Val, 19, TagIn).

dec_X520dnQualifier(Tlv) ->
    dec_X520dnQualifier(Tlv, [19]).

dec_X520dnQualifier(Tlv, TagIn) ->
    decode_restricted_string(Tlv, [], 19, TagIn).

%%================================
%%  X520Title
%%================================
enc_X520Title(Val) -> enc_X520Title(Val, []).

enc_X520Title({'X520Title', Val}, TagIn) ->
    enc_X520Title(Val, TagIn);
enc_X520Title(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520Title(Tlv) -> dec_X520Title(Tlv, []).

dec_X520Title(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 64}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 64}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 64}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 64}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520OrganizationalUnitName
%%================================
enc_X520OrganizationalUnitName(Val) ->
    enc_X520OrganizationalUnitName(Val, []).

enc_X520OrganizationalUnitName({'X520OrganizationalUnitName',
				Val},
			       TagIn) ->
    enc_X520OrganizationalUnitName(Val, TagIn);
enc_X520OrganizationalUnitName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520OrganizationalUnitName(Tlv) ->
    dec_X520OrganizationalUnitName(Tlv, []).

dec_X520OrganizationalUnitName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 64}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 64}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 64}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 64}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520OrganizationName
%%================================
enc_X520OrganizationName(Val) ->
    enc_X520OrganizationName(Val, []).

enc_X520OrganizationName({'X520OrganizationName', Val},
			 TagIn) ->
    enc_X520OrganizationName(Val, TagIn);
enc_X520OrganizationName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520OrganizationName(Tlv) ->
    dec_X520OrganizationName(Tlv, []).

dec_X520OrganizationName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 64}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 64}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 64}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 64}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520StateOrProvinceName
%%================================
enc_X520StateOrProvinceName(Val) ->
    enc_X520StateOrProvinceName(Val, []).

enc_X520StateOrProvinceName({'X520StateOrProvinceName',
			     Val},
			    TagIn) ->
    enc_X520StateOrProvinceName(Val, TagIn);
enc_X520StateOrProvinceName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520StateOrProvinceName(Tlv) ->
    dec_X520StateOrProvinceName(Tlv, []).

dec_X520StateOrProvinceName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 128}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 128}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 128}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 128}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520LocalityName
%%================================
enc_X520LocalityName(Val) ->
    enc_X520LocalityName(Val, []).

enc_X520LocalityName({'X520LocalityName', Val},
		     TagIn) ->
    enc_X520LocalityName(Val, TagIn);
enc_X520LocalityName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520LocalityName(Tlv) ->
    dec_X520LocalityName(Tlv, []).

dec_X520LocalityName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 128}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 128}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 128}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 128}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520CommonName
%%================================
enc_X520CommonName(Val) -> enc_X520CommonName(Val, []).

enc_X520CommonName({'X520CommonName', Val}, TagIn) ->
    enc_X520CommonName(Val, TagIn);
enc_X520CommonName(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520CommonName(Tlv) -> dec_X520CommonName(Tlv, []).

dec_X520CommonName(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 64}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 64}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 64}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 64}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  X520name
%%================================
enc_X520name(Val) -> enc_X520name(Val, []).

enc_X520name({'X520name', Val}, TagIn) ->
    enc_X520name(Val, TagIn);
enc_X520name(Val, TagIn) ->
    {EncBytes, EncLen} = case element(1, Val) of
			   teletexString ->
			       encode_restricted_string([], element(2, Val), 20,
							[<<20>>]);
			   printableString ->
			       encode_restricted_string([], element(2, Val), 19,
							[<<19>>]);
			   universalString ->
			       encode_universal_string([], element(2, Val),
						       [<<28>>]);
			   utf8String ->
			       encode_UTF8_string([], element(2, Val),
						  [<<12>>]);
			   bmpString ->
			       encode_BMP_string([], element(2, Val), [<<30>>]);
			   Else ->
			       exit({error,
				     {asn1, {invalid_choice_type, Else}}})
			 end,
    encode_tags(TagIn, EncBytes, EncLen).

dec_X520name(Tlv) -> dec_X520name(Tlv, []).

dec_X520name(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case case Tlv1 of
	   [CtempTlv1] -> CtempTlv1;
	   _ -> Tlv1
	 end
	of
      %% 'teletexString'
      {20, V1} ->
	  {teletexString,
	   decode_restricted_string(V1, {1, 32768}, 20, [])};
      %% 'printableString'
      {19, V1} ->
	  {printableString,
	   decode_restricted_string(V1, {1, 32768}, 19, [])};
      %% 'universalString'
      {28, V1} ->
	  {universalString,
	   decode_universal_string(V1, {1, 32768}, [])};
      %% 'utf8String'
      {12, V1} -> {utf8String, decode_UTF8_string(V1, [])};
      %% 'bmpString'
      {30, V1} ->
	  {bmpString, decode_BMP_string(V1, {1, 32768}, [])};
      Else ->
	  exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  AttributeTypeAndValue
%%================================
enc_AttributeTypeAndValue(Val) ->
    enc_AttributeTypeAndValue(Val, [<<48>>]).

enc_AttributeTypeAndValue(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute value(2)   External OTP-PKIX:AttributeValue
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_AttributeValue(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_AttributeTypeAndValue(Tlv) ->
    dec_AttributeTypeAndValue(Tlv, [16]).

dec_AttributeTypeAndValue(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute value(2)   External OTP-PKIX:AttributeValue
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AttributeValue(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'AttributeTypeAndValue', Term1, Term2}.

%%================================
%%  AttributeValue
%%================================
enc_AttributeValue(Val) -> enc_AttributeValue(Val, []).

enc_AttributeValue({'AttributeValue', Val}, TagIn) ->
    enc_AttributeValue(Val, TagIn);
enc_AttributeValue(Val, TagIn) ->
    %% OPEN TYPE
    encode_open_type(Val, TagIn).

dec_AttributeValue(Tlv) -> dec_AttributeValue(Tlv, []).

dec_AttributeValue(Tlv, TagIn) ->
    decode_open_type_as_binary(Tlv, TagIn).

%%================================
%%  AttributeType
%%================================
enc_AttributeType(Val) ->
    enc_AttributeType(Val, [<<6>>]).

enc_AttributeType({'AttributeType', Val}, TagIn) ->
    enc_AttributeType(Val, TagIn);
enc_AttributeType(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_AttributeType(Tlv) -> dec_AttributeType(Tlv, [6]).

dec_AttributeType(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  Attribute
%%================================
enc_Attribute(Val) -> enc_Attribute(Val, [<<48>>]).

enc_Attribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute values(2) with type SET OF
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_Attribute_values(Cindex2,
						[<<49>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  Attribute_values
%%================================

enc_Attribute_values({'Attribute_values', Val},
		     TagIn) ->
    enc_Attribute_values(Val, TagIn);
enc_Attribute_values(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_Attribute_values_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_Attribute_values_components([], AccBytes, AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_Attribute_values_components([H | T], AccBytes,
				AccLen) ->
    {EncBytes, EncLen} = enc_AttributeValue(H, []),
    enc_Attribute_values_components(T,
				    [EncBytes | AccBytes], AccLen + EncLen).

dec_Attribute_values(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_AttributeValue(V1, []) || V1 <- Tlv1].

dec_Attribute(Tlv) -> dec_Attribute(Tlv, [16]).

dec_Attribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute values(2) with type SET OF
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_Attribute_values(V2, [17]),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'Attribute', Term1, Term2}.

%%================================
%%  Extension-Any
%%================================
'enc_Extension-Any'(Val) ->
    'enc_Extension-Any'(Val, [<<48>>]).

'enc_Extension-Any'(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute extnID(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch check_bool(false,
						 Cindex2)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex2, [<<1>>])
			   end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_open_type(Cindex3, []),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_Extension-Any'(Tlv) ->
    'dec_Extension-Any'(Tlv, [16]).

'dec_Extension-Any'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute extnID(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{1, V2} | TempTlv3] ->
			  {decode_boolean(V2, []), TempTlv3};
		      _ -> {false, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_open_type_as_binary(V3, []),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Extension-Any', Term1, Term2, Term3}.

%%================================
%%  Any
%%================================
enc_Any(Val) -> enc_Any(Val, []).

enc_Any({'Any', Val}, TagIn) -> enc_Any(Val, TagIn);
enc_Any(Val, TagIn) ->
    %% OPEN TYPE
    encode_open_type(Val, TagIn).

dec_Any(Tlv) -> dec_Any(Tlv, []).

dec_Any(Tlv, TagIn) ->
    decode_open_type_as_binary(Tlv, TagIn).

%%================================
%%  Boolean
%%================================
enc_Boolean(Val) -> enc_Boolean(Val, [<<1>>]).

enc_Boolean({'Boolean', Val}, TagIn) ->
    enc_Boolean(Val, TagIn);
enc_Boolean(Val, TagIn) -> encode_boolean(Val, TagIn).

dec_Boolean(Tlv) -> dec_Boolean(Tlv, [1]).

dec_Boolean(Tlv, TagIn) -> decode_boolean(Tlv, TagIn).

%%================================
%%  ObjId
%%================================
enc_ObjId(Val) -> enc_ObjId(Val, [<<6>>]).

enc_ObjId({'ObjId', Val}, TagIn) ->
    enc_ObjId(Val, TagIn);
enc_ObjId(Val, TagIn) ->
    encode_object_identifier(Val, TagIn).

dec_ObjId(Tlv) -> dec_ObjId(Tlv, [6]).

dec_ObjId(Tlv, TagIn) ->
    decode_object_identifier(Tlv, TagIn).

%%================================
%%  SSLExtension
%%================================
enc_SSLExtension(Val) ->
    enc_SSLExtension(Val, [<<48>>]).

enc_SSLExtension(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    ObjextnID = getenc_SupportedExtensions(id, Cindex1),
    %%-------------------------------------------------
    %% attribute extnID(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case catch check_bool(false,
						 Cindex2)
			       of
			     true -> {[], 0};
			     _ -> encode_boolean(Cindex2, [<<1>>])
			   end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type typefieldType
    %%-------------------------------------------------
    {TmpBytes1, _} = ObjextnID('Type', Cindex3, []),
    {EncBytes3, EncLen3} = encode_open_type(TmpBytes1, []),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLExtension(Tlv) -> dec_SSLExtension(Tlv, [16]).

dec_SSLExtension(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute extnID(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute critical(2) with type BOOLEAN DEFAULT = false
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [{1, V2} | TempTlv3] ->
			  {decode_boolean(V2, []), TempTlv3};
		      _ -> {false, Tlv2}
		    end,
    %%-------------------------------------------------
    %% attribute extnValue(3) with type typefieldType
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Tmpterm1 = decode_open_type(V3, []),
    DecObjextnIDTerm1 = getdec_SupportedExtensions(id,
						   Term1),
    Term3 = case catch DecObjextnIDTerm1('Type', Tmpterm1,
					 [])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'SSLExtension', Term1, Term2, Term3}.

%%================================
%%  SSLExtensions
%%================================
enc_SSLExtensions(Val) ->
    enc_SSLExtensions(Val, [<<48>>]).

enc_SSLExtensions({'SSLExtensions', Val}, TagIn) ->
    enc_SSLExtensions(Val, TagIn);
enc_SSLExtensions(Val, TagIn) ->
    {EncBytes, EncLen} = enc_SSLExtensions_components(Val,
						      [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_SSLExtensions_components([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
enc_SSLExtensions_components([H | T], AccBytes,
			     AccLen) ->
    {EncBytes, EncLen} = enc_Extension(H, [<<48>>]),
    enc_SSLExtensions_components(T, [EncBytes | AccBytes],
				 AccLen + EncLen).

dec_SSLExtensions(Tlv) -> dec_SSLExtensions(Tlv, [16]).

dec_SSLExtensions(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_Extension(V1, [16]) || V1 <- Tlv1].

%%================================
%%  SSLExtensionAttribute
%%================================
enc_SSLExtensionAttribute(Val) ->
    enc_SSLExtensionAttribute(Val, [<<48>>]).

enc_SSLExtensionAttribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    ObjextensionAttributeType =
	getenc_SupportedExtensionAttributes(id, Cindex1),
    %%-------------------------------------------------
    %% attribute extensionAttributeType(1) with type fixedtypevaluefieldidtypetagUNIVERSAL2IMPLICIT0INTEGERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<128>>]),
    %%-------------------------------------------------
    %% attribute extensionAttributeValue(2) with type typefieldType
    %%-------------------------------------------------
    {TmpBytes1, _} = ObjextensionAttributeType('Type',
					       Cindex2, []),
    {EncBytes2, EncLen2} = encode_open_type(TmpBytes1,
					    [<<161>>]),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLExtensionAttribute(Tlv) ->
    dec_SSLExtensionAttribute(Tlv, [16]).

dec_SSLExtensionAttribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute extensionAttributeType(1) with type fixedtypevaluefieldidtypetagUNIVERSAL2IMPLICIT0INTEGERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [131072]),
    %%-------------------------------------------------
    %% attribute extensionAttributeValue(2) with type typefieldType
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Tmpterm1 = decode_open_type(V2, [131073]),
    DecObjextensionAttributeTypeTerm1 =
	getdec_SupportedExtensionAttributes(id, Term1),
    Term2 = case catch
		   DecObjextensionAttributeTypeTerm1('Type', Tmpterm1, [])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLExtensionAttribute', Term1, Term2}.

%%================================
%%  SSLExtensionAttributes
%%================================
enc_SSLExtensionAttributes(Val) ->
    enc_SSLExtensionAttributes(Val, [<<49>>]).

enc_SSLExtensionAttributes({'SSLExtensionAttributes',
			    Val},
			   TagIn) ->
    enc_SSLExtensionAttributes(Val, TagIn);
enc_SSLExtensionAttributes(Val, TagIn) ->
    {EncBytes, EncLen} =
	enc_SSLExtensionAttributes_components(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

enc_SSLExtensionAttributes_components([], AccBytes,
				      AccLen) ->
    {dynamicsort_SETOF(AccBytes), AccLen};
enc_SSLExtensionAttributes_components([H | T], AccBytes,
				      AccLen) ->
    {EncBytes, EncLen} = enc_ExtensionAttribute(H,
						[<<48>>]),
    enc_SSLExtensionAttributes_components(T,
					  [EncBytes | AccBytes],
					  AccLen + EncLen).

dec_SSLExtensionAttributes(Tlv) ->
    dec_SSLExtensionAttributes(Tlv, [17]).

dec_SSLExtensionAttributes(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [dec_ExtensionAttribute(V1, [16]) || V1 <- Tlv1].

%%================================
%%  SSLCharacteristic-two
%%================================
'enc_SSLCharacteristic-two'(Val) ->
    'enc_SSLCharacteristic-two'(Val, [<<48>>]).

'enc_SSLCharacteristic-two'(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    Objbasis = getenc_SupportedCharacteristicTwos(id,
						  Cindex2),
    %%-------------------------------------------------
    %% attribute m(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer([], Cindex1,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute basis(2) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_object_identifier(Cindex2,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(3) with type typefieldType
    %%-------------------------------------------------
    {TmpBytes1, _} = Objbasis('Type', Cindex3, []),
    {EncBytes3, EncLen3} = encode_open_type(TmpBytes1, []),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_SSLCharacteristic-two'(Tlv) ->
    'dec_SSLCharacteristic-two'(Tlv, [16]).

'dec_SSLCharacteristic-two'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute m(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [], [2]),
    %%-------------------------------------------------
    %% attribute basis(2) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_object_identifier(V2, [6]),
    %%-------------------------------------------------
    %% attribute parameters(3) with type typefieldType
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Tmpterm1 = decode_open_type(V3, []),
    DecObjbasisTerm2 =
	getdec_SupportedCharacteristicTwos(id, Term2),
    Term3 = case catch DecObjbasisTerm2('Type', Tmpterm1,
					[])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'SSLCharacteristic-two', Term1, Term2, Term3}.

%%================================
%%  SSLFieldID
%%================================
enc_SSLFieldID(Val) -> enc_SSLFieldID(Val, [<<48>>]).

enc_SSLFieldID(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    ObjfieldType = getenc_SupportedFieldIds(id, Cindex1),
    %%-------------------------------------------------
    %% attribute fieldType(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType
    %%-------------------------------------------------
    {TmpBytes1, _} = ObjfieldType('Type', Cindex2, []),
    {EncBytes2, EncLen2} = encode_open_type(TmpBytes1, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLFieldID(Tlv) -> dec_SSLFieldID(Tlv, [16]).

dec_SSLFieldID(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute fieldType(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Tmpterm1 = decode_open_type(V2, []),
    DecObjfieldTypeTerm1 = getdec_SupportedFieldIds(id,
						    Term1),
    Term2 = case catch DecObjfieldTypeTerm1('Type',
					    Tmpterm1, [])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLFieldID', Term1, Term2}.

%%================================
%%  KEA-PublicKey
%%================================
'enc_KEA-PublicKey'(Val) ->
    'enc_KEA-PublicKey'(Val, [<<2>>]).

'enc_KEA-PublicKey'({'KEA-PublicKey', Val}, TagIn) ->
    'enc_KEA-PublicKey'(Val, TagIn);
'enc_KEA-PublicKey'(Val, TagIn) ->
    encode_integer([], Val, TagIn).

'dec_KEA-PublicKey'(Tlv) ->
    'dec_KEA-PublicKey'(Tlv, [2]).

'dec_KEA-PublicKey'(Tlv, TagIn) ->
    decode_integer(Tlv, [], TagIn).

%%================================
%%  PublicKeyAlgorithm
%%================================
enc_PublicKeyAlgorithm(Val) ->
    enc_PublicKeyAlgorithm(Val, [<<48>>]).

enc_PublicKeyAlgorithm(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    Objalgorithm = getenc_SupportedPublicKeyAlgorithms(id,
						       Cindex1),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 {TmpBytes, _} = Objalgorithm('Type', Cindex2,
							      []),
				 {TmpBytes1, TmpLen} =
				     encode_open_type(TmpBytes, []),
				 {TmpBytes1, TmpLen}
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_PublicKeyAlgorithm(Tlv) ->
    dec_PublicKeyAlgorithm(Tlv, [16]).

dec_PublicKeyAlgorithm(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {Tmpterm1, Tlv3} = case Tlv2 of
			 [V2 | TempTlv3] ->
			     {decode_open_type(V2, []), TempTlv3};
			 _ -> {asn1_NOVALUE, Tlv2}
		       end,
    DecObjalgorithmTerm1 =
	getdec_SupportedPublicKeyAlgorithms(id, Term1),
    Term2 = case Tmpterm1 of
	      asn1_NOVALUE -> asn1_NOVALUE;
	      _ ->
		  case catch DecObjalgorithmTerm1('Type', Tmpterm1, []) of
		    {'EXIT', Reason1} ->
			exit({'Type not compatible with table constraint',
			      Reason1});
		    Tmpterm2 -> Tmpterm2
		  end
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'PublicKeyAlgorithm', Term1, Term2}.

%%================================
%%  SignatureAlgorithm-Any
%%================================
'enc_SignatureAlgorithm-Any'(Val) ->
    'enc_SignatureAlgorithm-Any'(Val, [<<48>>]).

'enc_SignatureAlgorithm-Any'(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute algorithm(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_open_type(Cindex2, [])
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_SignatureAlgorithm-Any'(Tlv) ->
    'dec_SignatureAlgorithm-Any'(Tlv, [16]).

'dec_SignatureAlgorithm-Any'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type OBJECT IDENTIFIER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type ASN1_OPEN_TYPE OPTIONAL
    %%-------------------------------------------------
    {Term2, Tlv3} = case Tlv2 of
		      [V2 | TempTlv3] ->
			  {decode_open_type_as_binary(V2, []), TempTlv3};
		      _ -> {asn1_NOVALUE, Tlv2}
		    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SignatureAlgorithm-Any', Term1, Term2}.

%%================================
%%  SignatureAlgorithm
%%================================
enc_SignatureAlgorithm(Val) ->
    enc_SignatureAlgorithm(Val, [<<48>>]).

enc_SignatureAlgorithm(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    Objalgorithm = getenc_SupportedSignatureAlgorithms(id,
						       Cindex1),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 {TmpBytes, _} = Objalgorithm('Type', Cindex2,
							      []),
				 {TmpBytes1, TmpLen} =
				     encode_open_type(TmpBytes, []),
				 {TmpBytes1, TmpLen}
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SignatureAlgorithm(Tlv) ->
    dec_SignatureAlgorithm(Tlv, [16]).

dec_SignatureAlgorithm(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {Tmpterm1, Tlv3} = case Tlv2 of
			 [V2 | TempTlv3] ->
			     {decode_open_type(V2, []), TempTlv3};
			 _ -> {asn1_NOVALUE, Tlv2}
		       end,
    DecObjalgorithmTerm1 =
	getdec_SupportedSignatureAlgorithms(id, Term1),
    Term2 = case Tmpterm1 of
	      asn1_NOVALUE -> asn1_NOVALUE;
	      _ ->
		  case catch DecObjalgorithmTerm1('Type', Tmpterm1, []) of
		    {'EXIT', Reason1} ->
			exit({'Type not compatible with table constraint',
			      Reason1});
		    Tmpterm2 -> Tmpterm2
		  end
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SignatureAlgorithm', Term1, Term2}.

%%================================
%%  SSLSubjectPublicKeyInfo-Any
%%================================
'enc_SSLSubjectPublicKeyInfo-Any'(Val) ->
    'enc_SSLSubjectPublicKeyInfo-Any'(Val, [<<48>>]).

'enc_SSLSubjectPublicKeyInfo-Any'(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    %%-------------------------------------------------
    %% attribute algorithm(1)   External OTP-PKIX:PublicKeyAlgorithm
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_PublicKeyAlgorithm(Cindex1,
						  [<<48>>]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_open_type(Cindex2, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

'dec_SSLSubjectPublicKeyInfo-Any'(Tlv) ->
    'dec_SSLSubjectPublicKeyInfo-Any'(Tlv, [16]).

'dec_SSLSubjectPublicKeyInfo-Any'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1)   External OTP-PKIX:PublicKeyAlgorithm
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_PublicKeyAlgorithm(V1, [16]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type ASN1_OPEN_TYPE
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_open_type_as_binary(V2, []),
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLSubjectPublicKeyInfo-Any', Term1, Term2}.

%%================================
%%  SSLSubjectPublicKeyInfo
%%================================
enc_SSLSubjectPublicKeyInfo(Val) ->
    enc_SSLSubjectPublicKeyInfo(Val, [<<48>>]).

enc_SSLSubjectPublicKeyInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    Objalgorithm = getenc_SupportedPublicKeyAlgorithms(id,
						       element(2, Cindex1)),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type SEQUENCE
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
	enc_SSLSubjectPublicKeyInfo_algorithm(Cindex1,
					      [<<48>>]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type typefieldPublicKeyType
    %%-------------------------------------------------
    {TmpBytes1, _} = Objalgorithm('PublicKeyType', Cindex2,
				  []),
    {EncBytes2, EncLen2} = encode_open_type(TmpBytes1, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

%%================================
%%  SSLSubjectPublicKeyInfo_algorithm
%%================================
enc_SSLSubjectPublicKeyInfo_algorithm(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    Objalgo = getenc_SupportedPublicKeyAlgorithms(id,
						  Cindex1),
    %%-------------------------------------------------
    %% attribute algo(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = case Cindex2 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ ->
				 {TmpBytes, _} = Objalgo('Type', Cindex2, []),
				 {TmpBytes1, TmpLen} =
				     encode_open_type(TmpBytes, []),
				 {TmpBytes1, TmpLen}
			   end,
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLSubjectPublicKeyInfo_algorithm(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algo(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute parameters(2) with type typefieldType OPTIONAL
    %%-------------------------------------------------
    {Tmpterm1, Tlv3} = case Tlv2 of
			 [V2 | TempTlv3] ->
			     {decode_open_type(V2, []), TempTlv3};
			 _ -> {asn1_NOVALUE, Tlv2}
		       end,
    DecObjalgoTerm1 =
	getdec_SupportedPublicKeyAlgorithms(id, Term1),
    Term2 = case Tmpterm1 of
	      asn1_NOVALUE -> asn1_NOVALUE;
	      _ ->
		  case catch DecObjalgoTerm1('Type', Tmpterm1, []) of
		    {'EXIT', Reason1} ->
			exit({'Type not compatible with table constraint',
			      Reason1});
		    Tmpterm2 -> Tmpterm2
		  end
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLSubjectPublicKeyInfo_algorithm', Term1, Term2}.

dec_SSLSubjectPublicKeyInfo(Tlv) ->
    dec_SSLSubjectPublicKeyInfo(Tlv, [16]).

dec_SSLSubjectPublicKeyInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute algorithm(1) with type SEQUENCE
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_SSLSubjectPublicKeyInfo_algorithm(V1, [16]),
    %%-------------------------------------------------
    %% attribute subjectPublicKey(2) with type typefieldPublicKeyType
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Tmpterm1 = decode_open_type(V2, []),
    DecObjalgorithmTerm1 =
	getdec_SupportedPublicKeyAlgorithms(id,
					    element(2, Term1)),
    Term2 = case catch DecObjalgorithmTerm1('PublicKeyType',
					    Tmpterm1, [])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLSubjectPublicKeyInfo', Term1, Term2}.

%%================================
%%  SSLAttributeTypeAndValue
%%================================
enc_SSLAttributeTypeAndValue(Val) ->
    enc_SSLAttributeTypeAndValue(Val, [<<48>>]).

enc_SSLAttributeTypeAndValue(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,
    Objtype = getenc_SupportedAttributeTypeAndValues(id,
						     Cindex1),
    %%-------------------------------------------------
    %% attribute type(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_object_identifier(Cindex1,
						    [<<6>>]),
    %%-------------------------------------------------
    %% attribute value(2) with type typefieldType
    %%-------------------------------------------------
    {TmpBytes1, _} = Objtype('Type', Cindex2, []),
    {EncBytes2, EncLen2} = encode_open_type(TmpBytes1, []),
    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLAttributeTypeAndValue(Tlv) ->
    dec_SSLAttributeTypeAndValue(Tlv, [16]).

dec_SSLAttributeTypeAndValue(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute type(1) with type fixedtypevaluefieldidtypetagUNIVERSAL6IMPLICIT0OBJECT IDENTIFIERno
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_object_identifier(V1, [6]),
    %%-------------------------------------------------
    %% attribute value(2) with type typefieldType
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Tmpterm1 = decode_open_type(V2, []),
    DecObjtypeTerm1 =
	getdec_SupportedAttributeTypeAndValues(id, Term1),
    Term2 = case catch DecObjtypeTerm1('Type', Tmpterm1, [])
		of
	      {'EXIT', Reason1} ->
		  exit({'Type not compatible with table constraint',
			Reason1});
	      Tmpterm2 -> Tmpterm2
	    end,
    case Tlv3 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv3}}}) % extra fields not allowed
    end,
    {'SSLAttributeTypeAndValue', Term1, Term2}.

%%================================
%%  SSLTBSCertificate
%%================================
enc_SSLTBSCertificate(Val) ->
    enc_SSLTBSCertificate(Val, [<<48>>]).

enc_SSLTBSCertificate(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3, Cindex4, Cindex5,
     Cindex6, Cindex7, Cindex8, Cindex9, Cindex10} =
	Val,
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = case catch check_int(0, Cindex1,
						[{v1, 0}, {v2, 1}, {v3, 2}])
			       of
			     true -> {[], 0};
			     _ ->
				 encode_integer([], Cindex1,
						[{v1, 0}, {v2, 1}, {v3, 2}],
						[<<2>>, <<160>>])
			   end,
    %%-------------------------------------------------
    %% attribute serialNumber(2) with type INTEGER
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_integer([], Cindex2,
					  [<<2>>]),
    %%-------------------------------------------------
    %% attribute signature(3)   External OTP-PKIX:SignatureAlgorithm
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = enc_SignatureAlgorithm(Cindex3,
						  [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuer(4)   External OTP-PKIX:Name
    %%-------------------------------------------------
    {EncBytes4, EncLen4} = enc_Name(Cindex4, []),
    %%-------------------------------------------------
    %% attribute validity(5)   External OTP-PKIX:Validity
    %%-------------------------------------------------
    {EncBytes5, EncLen5} = enc_Validity(Cindex5, [<<48>>]),
    %%-------------------------------------------------
    %% attribute subject(6)   External OTP-PKIX:Name
    %%-------------------------------------------------
    {EncBytes6, EncLen6} = enc_Name(Cindex6, []),
    %%-------------------------------------------------
    %% attribute subjectPublicKeyInfo(7)   External OTP-PKIX:SubjectPublicKeyInfo
    %%-------------------------------------------------
    {EncBytes7, EncLen7} = enc_SubjectPublicKeyInfo(Cindex7,
						    [<<48>>]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes8, EncLen8} = case Cindex8 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex8, [], [<<129>>])
			   end,
    %%-------------------------------------------------
    %% attribute subjectUniqueID(9) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {EncBytes9, EncLen9} = case Cindex9 of
			     asn1_NOVALUE -> {<<>>, 0};
			     _ -> encode_bit_string([], Cindex9, [], [<<130>>])
			   end,
    %%-------------------------------------------------
    %% attribute extensions(10)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {EncBytes10, EncLen10} = case Cindex10 of
			       asn1_NOVALUE -> {<<>>, 0};
			       _ -> enc_Extensions(Cindex10, [<<48>>, <<163>>])
			     end,
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3,
		  EncBytes4, EncBytes5, EncBytes6, EncBytes7, EncBytes8,
		  EncBytes9, EncBytes10],
    LenSoFar = EncLen1 + EncLen2 + EncLen3 + EncLen4 +
		 EncLen5
		 + EncLen6
		 + EncLen7
		 + EncLen8
		 + EncLen9
		 + EncLen10,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLTBSCertificate(Tlv) ->
    dec_SSLTBSCertificate(Tlv, [16]).

dec_SSLTBSCertificate(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute version(1) with type INTEGER DEFAULT = 0
    %%-------------------------------------------------
    {Term1, Tlv2} = case Tlv1 of
		      [{131072, V1} | TempTlv2] ->
			  {decode_integer(V1, [], [{v1, 0}, {v2, 1}, {v3, 2}],
					  [2]),
			   TempTlv2};
		      _ -> {0, Tlv1}
		    end,
    %%-------------------------------------------------
    %% attribute serialNumber(2) with type INTEGER
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_integer(V2, [], [2]),
    %%-------------------------------------------------
    %% attribute signature(3)   External OTP-PKIX:SignatureAlgorithm
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = dec_SignatureAlgorithm(V3, [16]),
    %%-------------------------------------------------
    %% attribute issuer(4)   External OTP-PKIX:Name
    %%-------------------------------------------------
    [V4 | Tlv5] = Tlv4,
    Term4 = dec_Name(V4, []),
    %%-------------------------------------------------
    %% attribute validity(5)   External OTP-PKIX:Validity
    %%-------------------------------------------------
    [V5 | Tlv6] = Tlv5,
    Term5 = dec_Validity(V5, [16]),
    %%-------------------------------------------------
    %% attribute subject(6)   External OTP-PKIX:Name
    %%-------------------------------------------------
    [V6 | Tlv7] = Tlv6,
    Term6 = dec_Name(V6, []),
    %%-------------------------------------------------
    %% attribute subjectPublicKeyInfo(7)   External OTP-PKIX:SubjectPublicKeyInfo
    %%-------------------------------------------------
    [V7 | Tlv8] = Tlv7,
    Term7 = dec_SubjectPublicKeyInfo(V7, [16]),
    %%-------------------------------------------------
    %% attribute issuerUniqueID(8) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term8, Tlv9} = case Tlv8 of
		      [{131073, V8} | TempTlv9] ->
			  {decode_compact_bit_string(V8, [], [], []), TempTlv9};
		      _ -> {asn1_NOVALUE, Tlv8}
		    end,
    %%-------------------------------------------------
    %% attribute subjectUniqueID(9) with type BIT STRING OPTIONAL
    %%-------------------------------------------------
    {Term9, Tlv10} = case Tlv9 of
		       [{131074, V9} | TempTlv10] ->
			   {decode_compact_bit_string(V9, [], [], []),
			    TempTlv10};
		       _ -> {asn1_NOVALUE, Tlv9}
		     end,
    %%-------------------------------------------------
    %% attribute extensions(10)   External OTP-PKIX:Extensions OPTIONAL
    %%-------------------------------------------------
    {Term10, Tlv11} = case Tlv10 of
			[{131075, V10} | TempTlv11] ->
			    {dec_Extensions(V10, [16]), TempTlv11};
			_ -> {asn1_NOVALUE, Tlv10}
		      end,
    case Tlv11 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv11}}}) % extra fields not allowed
    end,
    {'SSLTBSCertificate', Term1, Term2, Term3, Term4, Term5,
     Term6, Term7, Term8, Term9, Term10}.

%%================================
%%  SSLCertificate
%%================================
enc_SSLCertificate(Val) ->
    enc_SSLCertificate(Val, [<<48>>]).

enc_SSLCertificate(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,
    %%-------------------------------------------------
    %% attribute tbsCertificate(1)   External OTP-PKIX:TBSCertificate
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = enc_TBSCertificate(Cindex1,
					      [<<48>>]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:SignatureAlgorithm
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = enc_SignatureAlgorithm(Cindex2,
						  [<<48>>]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_bit_string([], Cindex3,
					     [], [<<3>>]),
    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SSLCertificate(Tlv) ->
    dec_SSLCertificate(Tlv, [16]).

dec_SSLCertificate(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute tbsCertificate(1)   External OTP-PKIX:TBSCertificate
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = dec_TBSCertificate(V1, [16]),
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:SignatureAlgorithm
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_SignatureAlgorithm(V2, [16]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_compact_bit_string(V3, [], [], [3]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'SSLCertificate', Term1, Term2, Term3}.

prime256v1() -> {1, 2, 840, 10045, 3, 1, 7}.

prime239v3() -> {1, 2, 840, 10045, 3, 1, 6}.

prime239v2() -> {1, 2, 840, 10045, 3, 1, 5}.

prime239v1() -> {1, 2, 840, 10045, 3, 1, 4}.

prime192v3() -> {1, 2, 840, 10045, 3, 1, 3}.

prime192v2() -> {1, 2, 840, 10045, 3, 1, 2}.

prime192v1() -> {1, 2, 840, 10045, 3, 1, 1}.

primeCurve() -> {1, 2, 840, 10045, 3, 1}.

c2tnb431r1() -> {1, 2, 840, 10045, 3, 0, 20}.

c2pnb368w1() -> {1, 2, 840, 10045, 3, 0, 19}.

c2tnb359v1() -> {1, 2, 840, 10045, 3, 0, 18}.

c2pnb304w1() -> {1, 2, 840, 10045, 3, 0, 17}.

c2pnb272w1() -> {1, 2, 840, 10045, 3, 0, 16}.

c2onb239v5() -> {1, 2, 840, 10045, 3, 0, 15}.

c2onb239v4() -> {1, 2, 840, 10045, 3, 0, 14}.

c2tnb239v3() -> {1, 2, 840, 10045, 3, 0, 13}.

c2tnb239v2() -> {1, 2, 840, 10045, 3, 0, 12}.

c2tnb239v1() -> {1, 2, 840, 10045, 3, 0, 11}.

c2pnb208w1() -> {1, 2, 840, 10045, 3, 0, 10}.

c2onb191v5() -> {1, 2, 840, 10045, 3, 0, 9}.

c2onb191v4() -> {1, 2, 840, 10045, 3, 0, 8}.

c2tnb191v3() -> {1, 2, 840, 10045, 3, 0, 7}.

c2tnb191v2() -> {1, 2, 840, 10045, 3, 0, 6}.

c2tnb191v1() -> {1, 2, 840, 10045, 3, 0, 5}.

c2pnb176w1() -> {1, 2, 840, 10045, 3, 0, 4}.

c2pnb163v3() -> {1, 2, 840, 10045, 3, 0, 3}.

c2pnb163v2() -> {1, 2, 840, 10045, 3, 0, 2}.

c2pnb163v1() -> {1, 2, 840, 10045, 3, 0, 1}.

'c-TwoCurve'() -> {1, 2, 840, 10045, 3, 0}.

ellipticCurve() -> {1, 2, 840, 10045, 3}.

'id-ecPublicKey'() -> {1, 2, 840, 10045, 2, 1}.

'id-publicKeyType'() -> {1, 2, 840, 10045, 2}.

ppBasis() -> {1, 2, 840, 10045, 1, 2, 3, 3}.

tpBasis() -> {1, 2, 840, 10045, 1, 2, 3, 2}.

gnBasis() -> {1, 2, 840, 10045, 1, 2, 3, 1}.

'id-characteristic-two-basis'() ->
    {1, 2, 840, 10045, 1, 2, 3}.

'characteristic-two-field'() ->
    {1, 2, 840, 10045, 1, 2}.

'prime-field'() -> {1, 2, 840, 10045, 1, 1}.

'id-fieldType'() -> {1, 2, 840, 10045, 1}.

'ecdsa-with-SHA1'() -> {1, 2, 840, 10045, 4, 1}.

'id-ecSigType'() -> {1, 2, 840, 10045, 4}.

'ansi-X9-62'() -> {1, 2, 840, 10045}.

'id-keyExchangeAlgorithm'() ->
    {2, 16, 840, 1, 101, 2, 1, 1, 22}.

dhpublicnumber() -> {1, 2, 840, 10046, 2, 1}.

sha1WithRSAEncryption() -> {1, 2, 840, 113549, 1, 1, 5}.

md5WithRSAEncryption() -> {1, 2, 840, 113549, 1, 1, 4}.

md2WithRSAEncryption() -> {1, 2, 840, 113549, 1, 1, 2}.

rsaEncryption() -> {1, 2, 840, 113549, 1, 1, 1}.

'id-dsa-with-sha1'() -> {1, 2, 840, 10040, 4, 3}.

'id-dsa'() -> {1, 2, 840, 10040, 4, 1}.

'id-sha1'() -> {1, 3, 14, 3, 2, 26}.

md5() -> {1, 2, 840, 113549, 2, 5}.

md2() -> {1, 2, 840, 113549, 2, 2}.

'id-at-clearance'() -> {2, 5, 1, 5, 55}.

'id-at-role'() -> {2, 5, 4, 72}.

'id-aca-encAttrs'() -> {1, 3, 6, 1, 5, 5, 7, 10, 6}.

'id-aca-group'() -> {1, 3, 6, 1, 5, 5, 7, 10, 4}.

'id-aca-chargingIdentity'() ->
    {1, 3, 6, 1, 5, 5, 7, 10, 3}.

'id-aca-accessIdentity'() ->
    {1, 3, 6, 1, 5, 5, 7, 10, 2}.

'id-aca-authenticationInfo'() ->
    {1, 3, 6, 1, 5, 5, 7, 10, 1}.

'id-aca'() -> {1, 3, 6, 1, 5, 5, 7, 10}.

'id-ce-targetInformation'() -> {2, 5, 29, 55}.

'id-pe-ac-proxying'() -> {1, 3, 6, 1, 5, 5, 7, 1, 10}.

'id-pe-aaControls'() -> {1, 3, 6, 1, 5, 5, 7, 1, 6}.

'id-pe-ac-auditIdentity'() ->
    {1, 3, 6, 1, 5, 5, 7, 1, 4}.

'id-ce-invalidityDate'() -> {2, 5, 29, 24}.

'id-holdinstruction-reject'() ->
    {2, 2, 840, 10040, 2, 3}.

'id-holdinstruction-callissuer'() ->
    {2, 2, 840, 10040, 2, 2}.

'id-holdinstruction-none'() -> {2, 2, 840, 10040, 2, 1}.

holdInstruction() -> {2, 2, 840, 10040, 2}.

'id-ce-holdInstructionCode'() -> {2, 5, 29, 23}.

'id-ce-certificateIssuer'() -> {2, 5, 29, 29}.

'id-ce-cRLReasons'() -> {2, 5, 29, 21}.

'id-ce-deltaCRLIndicator'() -> {2, 5, 29, 27}.

'id-ce-issuingDistributionPoint'() -> {2, 5, 29, 28}.

'id-ce-cRLNumber'() -> {2, 5, 29, 20}.

'id-pe-subjectInfoAccess'() ->
    {1, 3, 6, 1, 5, 5, 7, 1, 11}.

'id-pe-authorityInfoAccess'() ->
    {1, 3, 6, 1, 5, 5, 7, 1, 1}.

'id-ce-freshestCRL'() -> {2, 5, 29, 46}.

'id-ce-inhibitAnyPolicy'() -> {2, 5, 29, 54}.

'id-kp-OCSPSigning'() -> {1, 3, 6, 1, 5, 5, 7, 3, 9}.

'id-kp-timeStamping'() -> {1, 3, 6, 1, 5, 5, 7, 3, 8}.

'id-kp-emailProtection'() ->
    {1, 3, 6, 1, 5, 5, 7, 3, 4}.

'id-kp-codeSigning'() -> {1, 3, 6, 1, 5, 5, 7, 3, 3}.

'id-kp-clientAuth'() -> {1, 3, 6, 1, 5, 5, 7, 3, 2}.

'id-kp-serverAuth'() -> {1, 3, 6, 1, 5, 5, 7, 3, 1}.

anyExtendedKeyUsage() -> {2, 5, 29, 37, 0}.

'id-ce-extKeyUsage'() -> {2, 5, 29, 37}.

'id-ce-cRLDistributionPoints'() -> {2, 5, 29, 31}.

'id-ce-policyConstraints'() -> {2, 5, 29, 36}.

'id-ce-nameConstraints'() -> {2, 5, 29, 30}.

'id-ce-basicConstraints'() -> {2, 5, 29, 19}.

'id-ce-subjectDirectoryAttributes'() -> {2, 5, 29, 9}.

'id-ce-issuerAltName'() -> {2, 5, 29, 18}.

'id-ce-subjectAltName'() -> {2, 5, 29, 17}.

'id-ce-policyMappings'() -> {2, 5, 29, 33}.

anyPolicy() -> {2, 5, 29, 32, 0}.

'id-ce-certificatePolicies'() -> {2, 5, 29, 32}.

'id-ce-privateKeyUsagePeriod'() -> {2, 5, 29, 16}.

'id-ce-keyUsage'() -> {2, 5, 29, 15}.

'id-ce-subjectKeyIdentifier'() -> {2, 5, 29, 14}.

'id-ce-authorityKeyIdentifier'() -> {2, 5, 29, 35}.

'id-ce'() -> {2, 5, 29}.

'pkcs-1'() -> {1, 2, 840, 113549, 1, 1}.

'ub-x121-address-length'() -> 16.

'ub-unformatted-address-length'() -> 180.

'ub-terminal-id-length'() -> 24.

'ub-surname-length'() -> 40.

'ub-pseudonym'() -> 128.

'ub-postal-code-length'() -> 16.

'ub-pds-physical-address-lines'() -> 6.

'ub-pds-parameter-length'() -> 30.

'ub-pds-name-length'() -> 16.

'ub-organizational-units'() -> 4.

'ub-organizational-unit-name-length'() -> 32.

'ub-organization-name-length'() -> 64.

'ub-numeric-user-id-length'() -> 32.

'ub-integer-options'() -> 256.

'ub-initials-length'() -> 5.

'ub-given-name-length'() -> 16.

'ub-generation-qualifier-length'() -> 3.

'ub-e163-4-sub-address-length'() -> 40.

'ub-e163-4-number-length'() -> 15.

'ub-extension-attributes'() -> 256.

'ub-domain-name-length'() -> 16.

'ub-domain-defined-attribute-value-length'() -> 128.

'ub-domain-defined-attribute-type-length'() -> 8.

'ub-domain-defined-attributes'() -> 4.

'ub-country-name-numeric-length'() -> 3.

'ub-country-name-alpha-length'() -> 2.

'ub-common-name-length'() -> 64.

'ub-emailaddress-length'() -> 128.

'ub-match'() -> 128.

'ub-serial-number'() -> 64.

'ub-title'() -> 64.

'ub-organizational-unit-name'() -> 64.

'ub-organization-name'() -> 64.

'ub-state-name'() -> 128.

'ub-locality-name'() -> 128.

'ub-common-name'() -> 64.

'ub-name'() -> 32768.

'teletex-domain-defined-attributes'() -> 6.

'terminal-type'() -> 23.

'extended-network-address'() -> 22.

'local-postal-attributes'() -> 21.

'unique-postal-name'() -> 20.

'poste-restante-address'() -> 19.

'post-office-box-address'() -> 18.

'street-address'() -> 17.

'unformatted-postal-address'() -> 16.

'extension-physical-delivery-address-components'() ->
    15.

'physical-delivery-organization-name'() -> 14.

'physical-delivery-personal-name'() -> 13.

'extension-OR-address-components'() -> 12.

'physical-delivery-office-number'() -> 11.

'physical-delivery-office-name'() -> 10.

'postal-code'() -> 9.

'physical-delivery-country-name'() -> 8.

'pds-name'() -> 7.

'teletex-organizational-unit-names'() -> 5.

'teletex-personal-name'() -> 4.

'teletex-organization-name'() -> 3.

'teletex-common-name'() -> 2.

'common-name'() -> 1.

'id-emailAddress'() -> {1, 2, 840, 113549, 1, 9, 1}.

'pkcs-9'() -> {1, 2, 840, 113549, 1, 9}.

'id-domainComponent'() ->
    {0, 9, 2342, 19200300, 100, 1, 25}.

'id-at-pseudonym'() -> {2, 5, 4, 65}.

'id-at-serialNumber'() -> {2, 5, 4, 5}.

'id-at-countryName'() -> {2, 5, 4, 6}.

'id-at-dnQualifier'() -> {2, 5, 4, 46}.

'id-at-title'() -> {2, 5, 4, 12}.

'id-at-organizationalUnitName'() -> {2, 5, 4, 11}.

'id-at-organizationName'() -> {2, 5, 4, 10}.

'id-at-stateOrProvinceName'() -> {2, 5, 4, 8}.

'id-at-localityName'() -> {2, 5, 4, 7}.

'id-at-commonName'() -> {2, 5, 4, 3}.

'id-at-generationQualifier'() -> {2, 5, 4, 44}.

'id-at-initials'() -> {2, 5, 4, 43}.

'id-at-givenName'() -> {2, 5, 4, 42}.

'id-at-surname'() -> {2, 5, 4, 4}.

'id-at-name'() -> {2, 5, 4, 41}.

'id-at'() -> {2, 5, 4}.

'id-ad-caRepository'() -> {1, 3, 6, 1, 5, 5, 7, 48, 5}.

'id-ad-timeStamping'() -> {1, 3, 6, 1, 5, 5, 7, 48, 3}.

'id-ad-caIssuers'() -> {1, 3, 6, 1, 5, 5, 7, 48, 2}.

'id-ad-ocsp'() -> {1, 3, 6, 1, 5, 5, 7, 48, 1}.

'id-qt-unotice'() -> {1, 3, 6, 1, 5, 5, 7, 2, 2}.

'id-qt-cps'() -> {1, 3, 6, 1, 5, 5, 7, 2, 1}.

'id-ad'() -> {1, 3, 6, 1, 5, 5, 7, 48}.

'id-kp'() -> {1, 3, 6, 1, 5, 5, 7, 3}.

'id-qt'() -> {1, 3, 6, 1, 5, 5, 7, 2}.

'id-pe'() -> {1, 3, 6, 1, 5, 5, 7, 1}.

'id-pkix'() -> {1, 3, 6, 1, 5, 5, 7}.

%%================================
%%  invalidityDate
%%================================
enc_invalidityDate('Type', Val, _RestPrimFieldName) ->
    enc_InvalidityDate(Val, [<<24>>]).

dec_invalidityDate('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_InvalidityDate(Tlv, [24]).

tlv_format(Bytes) when is_binary(Bytes) ->
    {Tlv, _} = decode(Bytes), Tlv;
tlv_format(Bytes) -> Bytes.

%%================================
%%  holdInstructionCode
%%================================
enc_holdInstructionCode('Type', Val,
			_RestPrimFieldName) ->
    enc_HoldInstructionCode(Val, [<<6>>]).

dec_holdInstructionCode('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_HoldInstructionCode(Tlv, [6]).

%%================================
%%  certificateIssuer
%%================================
enc_certificateIssuer('Type', Val,
		      _RestPrimFieldName) ->
    enc_CertificateIssuer(Val, [<<48>>]).

dec_certificateIssuer('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_CertificateIssuer(Tlv, [16]).

%%================================
%%  cRLReasons
%%================================
enc_cRLReasons('Type', Val, _RestPrimFieldName) ->
    enc_CRLReason(Val, [<<10>>]).

dec_cRLReasons('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_CRLReason(Tlv, [10]).

%%================================
%%  deltaCRLIndicator
%%================================
enc_deltaCRLIndicator('Type', Val,
		      _RestPrimFieldName) ->
    enc_BaseCRLNumber(Val, [<<2>>]).

dec_deltaCRLIndicator('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_BaseCRLNumber(Tlv, [2]).

%%================================
%%  issuingDistributionPoint
%%================================
enc_issuingDistributionPoint('Type', Val,
			     _RestPrimFieldName) ->
    enc_IssuingDistributionPoint(Val, [<<48>>]).

dec_issuingDistributionPoint('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_IssuingDistributionPoint(Tlv, [16]).

%%================================
%%  cRLNumber
%%================================
enc_cRLNumber('Type', Val, _RestPrimFieldName) ->
    enc_CRLNumber(Val, [<<2>>]).

dec_cRLNumber('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_CRLNumber(Tlv, [2]).

%%================================
%%  subjectInfoAccess
%%================================
enc_subjectInfoAccess('Type', Val,
		      _RestPrimFieldName) ->
    enc_SubjectInfoAccessSyntax(Val, [<<48>>]).

dec_subjectInfoAccess('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_SubjectInfoAccessSyntax(Tlv, [16]).

%%================================
%%  authorityInfoAccess
%%================================
enc_authorityInfoAccess('Type', Val,
			_RestPrimFieldName) ->
    enc_AuthorityInfoAccessSyntax(Val, [<<48>>]).

dec_authorityInfoAccess('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_AuthorityInfoAccessSyntax(Tlv, [16]).

%%================================
%%  freshestCRL
%%================================
enc_freshestCRL('Type', Val, _RestPrimFieldName) ->
    enc_FreshestCRL(Val, [<<48>>]).

dec_freshestCRL('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_FreshestCRL(Tlv, [16]).

%%================================
%%  inhibitAnyPolicy
%%================================
enc_inhibitAnyPolicy('Type', Val, _RestPrimFieldName) ->
    enc_InhibitAnyPolicy(Val, [<<2>>]).

dec_inhibitAnyPolicy('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_InhibitAnyPolicy(Tlv, [2]).

%%================================
%%  extKeyUsage
%%================================
enc_extKeyUsage('Type', Val, _RestPrimFieldName) ->
    enc_ExtKeyUsageSyntax(Val, [<<48>>]).

dec_extKeyUsage('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_ExtKeyUsageSyntax(Tlv, [16]).

%%================================
%%  cRLDistributionPoints
%%================================
enc_cRLDistributionPoints('Type', Val,
			  _RestPrimFieldName) ->
    enc_CRLDistributionPoints(Val, [<<48>>]).

dec_cRLDistributionPoints('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_CRLDistributionPoints(Tlv, [16]).

%%================================
%%  policyConstraints
%%================================
enc_policyConstraints('Type', Val,
		      _RestPrimFieldName) ->
    enc_PolicyConstraints(Val, [<<48>>]).

dec_policyConstraints('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PolicyConstraints(Tlv, [16]).

%%================================
%%  nameConstraints
%%================================
enc_nameConstraints('Type', Val, _RestPrimFieldName) ->
    enc_NameConstraints(Val, [<<48>>]).

dec_nameConstraints('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_NameConstraints(Tlv, [16]).

%%================================
%%  basicConstraints
%%================================
enc_basicConstraints('Type', Val, _RestPrimFieldName) ->
    enc_BasicConstraints(Val, [<<48>>]).

dec_basicConstraints('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_BasicConstraints(Tlv, [16]).

%%================================
%%  subjectDirectoryAttributes
%%================================
enc_subjectDirectoryAttributes('Type', Val,
			       _RestPrimFieldName) ->
    enc_SubjectDirectoryAttributes(Val, [<<48>>]).

dec_subjectDirectoryAttributes('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_SubjectDirectoryAttributes(Tlv, [16]).

%%================================
%%  issuerAltName
%%================================
enc_issuerAltName('Type', Val, _RestPrimFieldName) ->
    enc_IssuerAltName(Val, [<<48>>]).

dec_issuerAltName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_IssuerAltName(Tlv, [16]).

%%================================
%%  subjectAltName
%%================================
enc_subjectAltName('Type', Val, _RestPrimFieldName) ->
    enc_SubjectAltName(Val, [<<48>>]).

dec_subjectAltName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_SubjectAltName(Tlv, [16]).

%%================================
%%  policyMappings
%%================================
enc_policyMappings('Type', Val, _RestPrimFieldName) ->
    enc_PolicyMappings(Val, [<<48>>]).

dec_policyMappings('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_PolicyMappings(Tlv, [16]).

%%================================
%%  certificatePolicies
%%================================
enc_certificatePolicies('Type', Val,
			_RestPrimFieldName) ->
    enc_CertificatePolicies(Val, [<<48>>]).

dec_certificatePolicies('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_CertificatePolicies(Tlv, [16]).

%%================================
%%  privateKeyUsagePeriod
%%================================
enc_privateKeyUsagePeriod('Type', Val,
			  _RestPrimFieldName) ->
    enc_PrivateKeyUsagePeriod(Val, [<<48>>]).

dec_privateKeyUsagePeriod('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PrivateKeyUsagePeriod(Tlv, [16]).

%%================================
%%  keyUsage
%%================================
enc_keyUsage('Type', Val, _RestPrimFieldName) ->
    enc_KeyUsage(Val, [<<3>>]).

dec_keyUsage('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_KeyUsage(Tlv, [3]).

%%================================
%%  subjectKeyIdentifier
%%================================
enc_subjectKeyIdentifier('Type', Val,
			 _RestPrimFieldName) ->
    enc_SubjectKeyIdentifier(Val, [<<4>>]).

dec_subjectKeyIdentifier('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_SubjectKeyIdentifier(Tlv, [4]).

%%================================
%%  authorityKeyIdentifier
%%================================
enc_authorityKeyIdentifier('Type', Val,
			   _RestPrimFieldName) ->
    enc_AuthorityKeyIdentifier(Val, [<<48>>]).

dec_authorityKeyIdentifier('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_AuthorityKeyIdentifier(Tlv, [16]).

%%================================
%%  x400-teletex-domain-defined-attributes
%%================================
'enc_x400-teletex-domain-defined-attributes'('Type',
					     Val, _RestPrimFieldName) ->
    enc_TeletexDomainDefinedAttributes(Val, [<<48>>]).

'dec_x400-teletex-domain-defined-attributes'('Type',
					     Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_TeletexDomainDefinedAttributes(Tlv, [16]).

%%================================
%%  x400-terminal-type
%%================================
'enc_x400-terminal-type'('Type', Val,
			 _RestPrimFieldName) ->
    enc_TerminalType(Val, [<<2>>]).

'dec_x400-terminal-type'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_TerminalType(Tlv, [2]).

%%================================
%%  x400-extended-network-address
%%================================
'enc_x400-extended-network-address'('Type', Val,
				    _RestPrimFieldName) ->
    enc_ExtendedNetworkAddress(Val, []).

'dec_x400-extended-network-address'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_ExtendedNetworkAddress(Tlv, []).

%%================================
%%  x400-local-postal-attributes
%%================================
'enc_x400-local-postal-attributes'('Type', Val,
				   _RestPrimFieldName) ->
    enc_LocalPostalAttributes(Val, [<<49>>]).

'dec_x400-local-postal-attributes'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_LocalPostalAttributes(Tlv, [17]).

%%================================
%%  x400-unique-postal-name
%%================================
'enc_x400-unique-postal-name'('Type', Val,
			      _RestPrimFieldName) ->
    enc_UniquePostalName(Val, [<<49>>]).

'dec_x400-unique-postal-name'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_UniquePostalName(Tlv, [17]).

%%================================
%%  x400-poste-restante-address
%%================================
'enc_x400-poste-restante-address'('Type', Val,
				  _RestPrimFieldName) ->
    enc_PosteRestanteAddress(Val, [<<49>>]).

'dec_x400-poste-restante-address'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PosteRestanteAddress(Tlv, [17]).

%%================================
%%  x400-post-office-box-address
%%================================
'enc_x400-post-office-box-address'('Type', Val,
				   _RestPrimFieldName) ->
    enc_PostOfficeBoxAddress(Val, [<<49>>]).

'dec_x400-post-office-box-address'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PostOfficeBoxAddress(Tlv, [17]).

%%================================
%%  x400-street-address
%%================================
'enc_x400-street-address'('Type', Val,
			  _RestPrimFieldName) ->
    enc_StreetAddress(Val, [<<49>>]).

'dec_x400-street-address'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_StreetAddress(Tlv, [17]).

%%================================
%%  x400-unformatted-postal-address
%%================================
'enc_x400-unformatted-postal-address'('Type', Val,
				      _RestPrimFieldName) ->
    enc_UnformattedPostalAddress(Val, [<<49>>]).

'dec_x400-unformatted-postal-address'('Type', Bytes,
				      _) ->
    Tlv = tlv_format(Bytes),
    dec_UnformattedPostalAddress(Tlv, [17]).

%%================================
%%  x400-extension-physical-delivery-address-components
%%================================
'enc_x400-extension-physical-delivery-address-components'('Type',
							  Val,
							  _RestPrimFieldName) ->
    enc_ExtensionPhysicalDeliveryAddressComponents(Val,
						   [<<49>>]).

'dec_x400-extension-physical-delivery-address-components'('Type',
							  Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_ExtensionPhysicalDeliveryAddressComponents(Tlv,
						   [17]).

%%================================
%%  x400-physical-delivery-organization-name
%%================================
'enc_x400-physical-delivery-organization-name'('Type',
					       Val, _RestPrimFieldName) ->
    enc_PhysicalDeliveryOrganizationName(Val, [<<49>>]).

'dec_x400-physical-delivery-organization-name'('Type',
					       Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PhysicalDeliveryOrganizationName(Tlv, [17]).

%%================================
%%  x400-physical-delivery-personal-name
%%================================
'enc_x400-physical-delivery-personal-name'('Type', Val,
					   _RestPrimFieldName) ->
    enc_PhysicalDeliveryPersonalName(Val, [<<49>>]).

'dec_x400-physical-delivery-personal-name'('Type',
					   Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PhysicalDeliveryPersonalName(Tlv, [17]).

%%================================
%%  x400-extension-OR-address-components
%%================================
'enc_x400-extension-OR-address-components'('Type', Val,
					   _RestPrimFieldName) ->
    enc_ExtensionORAddressComponents(Val, [<<49>>]).

'dec_x400-extension-OR-address-components'('Type',
					   Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_ExtensionORAddressComponents(Tlv, [17]).

%%================================
%%  x400-physical-delivery-office-number
%%================================
'enc_x400-physical-delivery-office-number'('Type', Val,
					   _RestPrimFieldName) ->
    enc_PhysicalDeliveryOfficeNumber(Val, [<<49>>]).

'dec_x400-physical-delivery-office-number'('Type',
					   Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_PhysicalDeliveryOfficeNumber(Tlv, [17]).

%%================================
%%  x400-physical-delivery-office-name
%%================================
'enc_x400-physical-delivery-office-name'('Type', Val,
					 _RestPrimFieldName) ->
    enc_PhysicalDeliveryOfficeName(Val, [<<49>>]).

'dec_x400-physical-delivery-office-name'('Type', Bytes,
					 _) ->
    Tlv = tlv_format(Bytes),
    dec_PhysicalDeliveryOfficeName(Tlv, [17]).

%%================================
%%  x400-postal-code
%%================================
'enc_x400-postal-code'('Type', Val,
		       _RestPrimFieldName) ->
    enc_PostalCode(Val, []).

'dec_x400-postal-code'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_PostalCode(Tlv, []).

%%================================
%%  x400-physical-delivery-country-name
%%================================
'enc_x400-physical-delivery-country-name'('Type', Val,
					  _RestPrimFieldName) ->
    enc_PhysicalDeliveryCountryName(Val, []).

'dec_x400-physical-delivery-country-name'('Type', Bytes,
					  _) ->
    Tlv = tlv_format(Bytes),
    dec_PhysicalDeliveryCountryName(Tlv, []).

%%================================
%%  x400-pds-name
%%================================
'enc_x400-pds-name'('Type', Val, _RestPrimFieldName) ->
    enc_PDSName(Val, [<<19>>]).

'dec_x400-pds-name'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_PDSName(Tlv, [19]).

%%================================
%%  x400-teletex-personal-name
%%================================
'enc_x400-teletex-personal-name'('Type', Val,
				 _RestPrimFieldName) ->
    enc_TeletexPersonalName(Val, [<<49>>]).

'dec_x400-teletex-personal-name'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_TeletexPersonalName(Tlv, [17]).

%%================================
%%  x400-teletex-common-name
%%================================
'enc_x400-teletex-common-name'('Type', Val,
			       _RestPrimFieldName) ->
    enc_TeletexCommonName(Val, [<<20>>]).

'dec_x400-teletex-common-name'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_TeletexCommonName(Tlv, [20]).

%%================================
%%  x400-common-name
%%================================
'enc_x400-common-name'('Type', Val,
		       _RestPrimFieldName) ->
    enc_CommonName(Val, [<<19>>]).

'dec_x400-common-name'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_CommonName(Tlv, [19]).

%%================================
%%  ec-public-key
%%================================
'enc_ec-public-key'('Type', Val, _RestPrimFieldName) ->
    enc_EcpkParameters(Val, []);
'enc_ec-public-key'('PublicKeyType', Val,
		    _RestPrimFieldName) ->
    enc_ECPoint(Val, [<<4>>]).

'dec_ec-public-key'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_EcpkParameters(Tlv, []);
'dec_ec-public-key'('PublicKeyType', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_ECPoint(Tlv, [4]).

%%================================
%%  pp-basis
%%================================
'enc_pp-basis'('Type', Val, _RestPrimFieldName) ->
    enc_Pentanomial(Val, [<<48>>]).

'dec_pp-basis'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_Pentanomial(Tlv, [16]).

%%================================
%%  tp-basis
%%================================
'enc_tp-basis'('Type', Val, _RestPrimFieldName) ->
    enc_Trinomial(Val, [<<2>>]).

'dec_tp-basis'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_Trinomial(Tlv, [2]).

%%================================
%%  gn-basis
%%================================
'enc_gn-basis'('Type', Val, _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_gn-basis'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  field-characteristic-two
%%================================
'enc_field-characteristic-two'('Type', Val,
			       _RestPrimFieldName) ->
    'enc_Characteristic-two'(Val, [<<48>>]).

'dec_field-characteristic-two'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    'dec_Characteristic-two'(Tlv, [16]).

%%================================
%%  field-prime-field
%%================================
'enc_field-prime-field'('Type', Val,
			_RestPrimFieldName) ->
    'enc_Prime-p'(Val, [<<2>>]).

'dec_field-prime-field'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), 'dec_Prime-p'(Tlv, [2]).

%%================================
%%  ecdsa-with-sha1
%%================================
'enc_ecdsa-with-sha1'('Type', Val,
		      _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_ecdsa-with-sha1'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  kea
%%================================
enc_kea('Type', Val, _RestPrimFieldName) ->
    'enc_KEA-Parms-Id'(Val, [<<4>>]);
enc_kea('PublicKeyType', Val, _RestPrimFieldName) ->
    'enc_KEA-PublicKey'(Val, [<<2>>]).

dec_kea('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), 'dec_KEA-Parms-Id'(Tlv, [4]);
dec_kea('PublicKeyType', Bytes, _) ->
    Tlv = tlv_format(Bytes), 'dec_KEA-PublicKey'(Tlv, [2]).

%%================================
%%  dh
%%================================
enc_dh('Type', Val, _RestPrimFieldName) ->
    enc_DomainParameters(Val, [<<48>>]);
enc_dh('PublicKeyType', Val, _RestPrimFieldName) ->
    enc_DHPublicKey(Val, [<<2>>]).

dec_dh('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_DomainParameters(Tlv, [16]);
dec_dh('PublicKeyType', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_DHPublicKey(Tlv, [2]).

%%================================
%%  rsa-encryption
%%================================
'enc_rsa-encryption'('Type', Val, _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]);
'enc_rsa-encryption'('PublicKeyType', Val,
		     _RestPrimFieldName) ->
    enc_RSAPublicKey(Val, [<<48>>]).

'dec_rsa-encryption'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]);
'dec_rsa-encryption'('PublicKeyType', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_RSAPublicKey(Tlv, [16]).

%%================================
%%  sha1-with-rsa-encryption
%%================================
'enc_sha1-with-rsa-encryption'('Type', Val,
			       _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_sha1-with-rsa-encryption'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  md5-with-rsa-encryption
%%================================
'enc_md5-with-rsa-encryption'('Type', Val,
			      _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_md5-with-rsa-encryption'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  md2-with-rsa-encryption
%%================================
'enc_md2-with-rsa-encryption'('Type', Val,
			      _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_md2-with-rsa-encryption'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  dsa-with-sha1
%%================================
'enc_dsa-with-sha1'('Type', Val, _RestPrimFieldName) ->
    encode_null(Val, [<<5>>]).

'dec_dsa-with-sha1'('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), decode_null(Tlv, [5]).

%%================================
%%  dsa
%%================================
enc_dsa('Type', Val, _RestPrimFieldName) ->
    'enc_Dss-Parms'(Val, [<<48>>]);
enc_dsa('PublicKeyType', Val, _RestPrimFieldName) ->
    enc_DSAPublicKey(Val, [<<2>>]).

dec_dsa('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), 'dec_Dss-Parms'(Tlv, [16]);
dec_dsa('PublicKeyType', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_DSAPublicKey(Tlv, [2]).

%%================================
%%  emailAddress
%%================================
enc_emailAddress('Type', Val, _RestPrimFieldName) ->
    enc_EmailAddress(Val, [<<22>>]).

dec_emailAddress('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_EmailAddress(Tlv, [22]).

%%================================
%%  domainComponent
%%================================
enc_domainComponent('Type', Val, _RestPrimFieldName) ->
    enc_DomainComponent(Val, [<<22>>]).

dec_domainComponent('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_DomainComponent(Tlv, [22]).

%%================================
%%  pseudonym
%%================================
enc_pseudonym('Type', Val, _RestPrimFieldName) ->
    enc_X520Pseudonym(Val, []).

dec_pseudonym('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520Pseudonym(Tlv, []).

%%================================
%%  serialNumber
%%================================
enc_serialNumber('Type', Val, _RestPrimFieldName) ->
    enc_X520SerialNumber(Val, [<<19>>]).

dec_serialNumber('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_X520SerialNumber(Tlv, [19]).

%%================================
%%  countryName
%%================================
enc_countryName('Type', Val, _RestPrimFieldName) ->
    enc_X520countryName(Val, [<<19>>]).

dec_countryName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520countryName(Tlv, [19]).

%%================================
%%  dnQualifier
%%================================
enc_dnQualifier('Type', Val, _RestPrimFieldName) ->
    enc_X520dnQualifier(Val, [<<19>>]).

dec_dnQualifier('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520dnQualifier(Tlv, [19]).

%%================================
%%  title
%%================================
enc_title('Type', Val, _RestPrimFieldName) ->
    enc_X520Title(Val, []).

dec_title('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520Title(Tlv, []).

%%================================
%%  organizationalUnitName
%%================================
enc_organizationalUnitName('Type', Val,
			   _RestPrimFieldName) ->
    enc_X520OrganizationalUnitName(Val, []).

dec_organizationalUnitName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_X520OrganizationalUnitName(Tlv, []).

%%================================
%%  organizationName
%%================================
enc_organizationName('Type', Val, _RestPrimFieldName) ->
    enc_X520OrganizationName(Val, []).

dec_organizationName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_X520OrganizationName(Tlv, []).

%%================================
%%  stateOrProvinceName
%%================================
enc_stateOrProvinceName('Type', Val,
			_RestPrimFieldName) ->
    enc_X520StateOrProvinceName(Val, []).

dec_stateOrProvinceName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes),
    dec_X520StateOrProvinceName(Tlv, []).

%%================================
%%  localityName
%%================================
enc_localityName('Type', Val, _RestPrimFieldName) ->
    enc_X520LocalityName(Val, []).

dec_localityName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520LocalityName(Tlv, []).

%%================================
%%  commonName
%%================================
enc_commonName('Type', Val, _RestPrimFieldName) ->
    enc_X520CommonName(Val, []).

dec_commonName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520CommonName(Tlv, []).

%%================================
%%  generationQualifier
%%================================
enc_generationQualifier('Type', Val,
			_RestPrimFieldName) ->
    enc_X520name(Val, []).

dec_generationQualifier('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520name(Tlv, []).

%%================================
%%  initials
%%================================
enc_initials('Type', Val, _RestPrimFieldName) ->
    enc_X520name(Val, []).

dec_initials('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520name(Tlv, []).

%%================================
%%  givenName
%%================================
enc_givenName('Type', Val, _RestPrimFieldName) ->
    enc_X520name(Val, []).

dec_givenName('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520name(Tlv, []).

%%================================
%%  surname
%%================================
enc_surname('Type', Val, _RestPrimFieldName) ->
    enc_X520name(Val, []).

dec_surname('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520name(Tlv, []).

%%================================
%%  name
%%================================
enc_name('Type', Val, _RestPrimFieldName) ->
    enc_X520name(Val, []).

dec_name('Type', Bytes, _) ->
    Tlv = tlv_format(Bytes), dec_X520name(Tlv, []).

%%================================
%%  SupportedExtensions
%%================================
getenc_SupportedExtensions(id,
			   {1, 3, 6, 1, 5, 5, 7, 1, 1}) ->
    fun enc_authorityInfoAccess/3;
getenc_SupportedExtensions(id, {2, 5, 29, 35}) ->
    fun enc_authorityKeyIdentifier/3;
getenc_SupportedExtensions(id, {2, 5, 29, 19}) ->
    fun enc_basicConstraints/3;
getenc_SupportedExtensions(id, {2, 5, 29, 31}) ->
    fun enc_cRLDistributionPoints/3;
getenc_SupportedExtensions(id, {2, 5, 29, 20}) ->
    fun enc_cRLNumber/3;
getenc_SupportedExtensions(id, {2, 5, 29, 21}) ->
    fun enc_cRLReasons/3;
getenc_SupportedExtensions(id, {2, 5, 29, 29}) ->
    fun enc_certificateIssuer/3;
getenc_SupportedExtensions(id, {2, 5, 29, 32}) ->
    fun enc_certificatePolicies/3;
getenc_SupportedExtensions(id, {2, 5, 29, 27}) ->
    fun enc_deltaCRLIndicator/3;
getenc_SupportedExtensions(id, {2, 5, 29, 37}) ->
    fun enc_extKeyUsage/3;
getenc_SupportedExtensions(id, {2, 5, 29, 46}) ->
    fun enc_freshestCRL/3;
getenc_SupportedExtensions(id, {2, 5, 29, 23}) ->
    fun enc_holdInstructionCode/3;
getenc_SupportedExtensions(id, {2, 5, 29, 54}) ->
    fun enc_inhibitAnyPolicy/3;
getenc_SupportedExtensions(id, {2, 5, 29, 24}) ->
    fun enc_invalidityDate/3;
getenc_SupportedExtensions(id, {2, 5, 29, 18}) ->
    fun enc_issuerAltName/3;
getenc_SupportedExtensions(id, {2, 5, 29, 28}) ->
    fun enc_issuingDistributionPoint/3;
getenc_SupportedExtensions(id, {2, 5, 29, 15}) ->
    fun enc_keyUsage/3;
getenc_SupportedExtensions(id, {2, 5, 29, 30}) ->
    fun enc_nameConstraints/3;
getenc_SupportedExtensions(id, {2, 5, 29, 36}) ->
    fun enc_policyConstraints/3;
getenc_SupportedExtensions(id, {2, 5, 29, 33}) ->
    fun enc_policyMappings/3;
getenc_SupportedExtensions(id, {2, 5, 29, 16}) ->
    fun enc_privateKeyUsagePeriod/3;
getenc_SupportedExtensions(id, {2, 5, 29, 17}) ->
    fun enc_subjectAltName/3;
getenc_SupportedExtensions(id, {2, 5, 29, 9}) ->
    fun enc_subjectDirectoryAttributes/3;
getenc_SupportedExtensions(id,
			   {1, 3, 6, 1, 5, 5, 7, 1, 11}) ->
    fun enc_subjectInfoAccess/3;
getenc_SupportedExtensions(id, {2, 5, 29, 14}) ->
    fun enc_subjectKeyIdentifier/3;
getenc_SupportedExtensions(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedExtensions(id,
			   {1, 3, 6, 1, 5, 5, 7, 1, 1}) ->
    fun dec_authorityInfoAccess/3;
getdec_SupportedExtensions(id, {2, 5, 29, 35}) ->
    fun dec_authorityKeyIdentifier/3;
getdec_SupportedExtensions(id, {2, 5, 29, 19}) ->
    fun dec_basicConstraints/3;
getdec_SupportedExtensions(id, {2, 5, 29, 31}) ->
    fun dec_cRLDistributionPoints/3;
getdec_SupportedExtensions(id, {2, 5, 29, 20}) ->
    fun dec_cRLNumber/3;
getdec_SupportedExtensions(id, {2, 5, 29, 21}) ->
    fun dec_cRLReasons/3;
getdec_SupportedExtensions(id, {2, 5, 29, 29}) ->
    fun dec_certificateIssuer/3;
getdec_SupportedExtensions(id, {2, 5, 29, 32}) ->
    fun dec_certificatePolicies/3;
getdec_SupportedExtensions(id, {2, 5, 29, 27}) ->
    fun dec_deltaCRLIndicator/3;
getdec_SupportedExtensions(id, {2, 5, 29, 37}) ->
    fun dec_extKeyUsage/3;
getdec_SupportedExtensions(id, {2, 5, 29, 46}) ->
    fun dec_freshestCRL/3;
getdec_SupportedExtensions(id, {2, 5, 29, 23}) ->
    fun dec_holdInstructionCode/3;
getdec_SupportedExtensions(id, {2, 5, 29, 54}) ->
    fun dec_inhibitAnyPolicy/3;
getdec_SupportedExtensions(id, {2, 5, 29, 24}) ->
    fun dec_invalidityDate/3;
getdec_SupportedExtensions(id, {2, 5, 29, 18}) ->
    fun dec_issuerAltName/3;
getdec_SupportedExtensions(id, {2, 5, 29, 28}) ->
    fun dec_issuingDistributionPoint/3;
getdec_SupportedExtensions(id, {2, 5, 29, 15}) ->
    fun dec_keyUsage/3;
getdec_SupportedExtensions(id, {2, 5, 29, 30}) ->
    fun dec_nameConstraints/3;
getdec_SupportedExtensions(id, {2, 5, 29, 36}) ->
    fun dec_policyConstraints/3;
getdec_SupportedExtensions(id, {2, 5, 29, 33}) ->
    fun dec_policyMappings/3;
getdec_SupportedExtensions(id, {2, 5, 29, 16}) ->
    fun dec_privateKeyUsagePeriod/3;
getdec_SupportedExtensions(id, {2, 5, 29, 17}) ->
    fun dec_subjectAltName/3;
getdec_SupportedExtensions(id, {2, 5, 29, 9}) ->
    fun dec_subjectDirectoryAttributes/3;
getdec_SupportedExtensions(id,
			   {1, 3, 6, 1, 5, 5, 7, 1, 11}) ->
    fun dec_subjectInfoAccess/3;
getdec_SupportedExtensions(id, {2, 5, 29, 14}) ->
    fun dec_subjectKeyIdentifier/3;
getdec_SupportedExtensions(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedExtensionAttributes
%%================================
getenc_SupportedExtensionAttributes(id, 1) ->
    fun 'enc_x400-common-name'/3;
getenc_SupportedExtensionAttributes(id, 22) ->
    fun 'enc_x400-extended-network-address'/3;
getenc_SupportedExtensionAttributes(id, 12) ->
    fun 'enc_x400-extension-OR-address-components'/3;
getenc_SupportedExtensionAttributes(id, 15) ->
    fun 'enc_x400-extension-physical-delivery-address-components'/3;
getenc_SupportedExtensionAttributes(id, 21) ->
    fun 'enc_x400-local-postal-attributes'/3;
getenc_SupportedExtensionAttributes(id, 7) ->
    fun 'enc_x400-pds-name'/3;
getenc_SupportedExtensionAttributes(id, 8) ->
    fun 'enc_x400-physical-delivery-country-name'/3;
getenc_SupportedExtensionAttributes(id, 10) ->
    fun 'enc_x400-physical-delivery-office-name'/3;
getenc_SupportedExtensionAttributes(id, 11) ->
    fun 'enc_x400-physical-delivery-office-number'/3;
getenc_SupportedExtensionAttributes(id, 14) ->
    fun 'enc_x400-physical-delivery-organization-name'/3;
getenc_SupportedExtensionAttributes(id, 13) ->
    fun 'enc_x400-physical-delivery-personal-name'/3;
getenc_SupportedExtensionAttributes(id, 18) ->
    fun 'enc_x400-post-office-box-address'/3;
getenc_SupportedExtensionAttributes(id, 9) ->
    fun 'enc_x400-postal-code'/3;
getenc_SupportedExtensionAttributes(id, 19) ->
    fun 'enc_x400-poste-restante-address'/3;
getenc_SupportedExtensionAttributes(id, 17) ->
    fun 'enc_x400-street-address'/3;
getenc_SupportedExtensionAttributes(id, 2) ->
    fun 'enc_x400-teletex-common-name'/3;
getenc_SupportedExtensionAttributes(id, 6) ->
    fun 'enc_x400-teletex-domain-defined-attributes'/3;
getenc_SupportedExtensionAttributes(id, 4) ->
    fun 'enc_x400-teletex-personal-name'/3;
getenc_SupportedExtensionAttributes(id, 23) ->
    fun 'enc_x400-terminal-type'/3;
getenc_SupportedExtensionAttributes(id, 16) ->
    fun 'enc_x400-unformatted-postal-address'/3;
getenc_SupportedExtensionAttributes(id, 20) ->
    fun 'enc_x400-unique-postal-name'/3;
getenc_SupportedExtensionAttributes(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedExtensionAttributes(id, 1) ->
    fun 'dec_x400-common-name'/3;
getdec_SupportedExtensionAttributes(id, 22) ->
    fun 'dec_x400-extended-network-address'/3;
getdec_SupportedExtensionAttributes(id, 12) ->
    fun 'dec_x400-extension-OR-address-components'/3;
getdec_SupportedExtensionAttributes(id, 15) ->
    fun 'dec_x400-extension-physical-delivery-address-components'/3;
getdec_SupportedExtensionAttributes(id, 21) ->
    fun 'dec_x400-local-postal-attributes'/3;
getdec_SupportedExtensionAttributes(id, 7) ->
    fun 'dec_x400-pds-name'/3;
getdec_SupportedExtensionAttributes(id, 8) ->
    fun 'dec_x400-physical-delivery-country-name'/3;
getdec_SupportedExtensionAttributes(id, 10) ->
    fun 'dec_x400-physical-delivery-office-name'/3;
getdec_SupportedExtensionAttributes(id, 11) ->
    fun 'dec_x400-physical-delivery-office-number'/3;
getdec_SupportedExtensionAttributes(id, 14) ->
    fun 'dec_x400-physical-delivery-organization-name'/3;
getdec_SupportedExtensionAttributes(id, 13) ->
    fun 'dec_x400-physical-delivery-personal-name'/3;
getdec_SupportedExtensionAttributes(id, 18) ->
    fun 'dec_x400-post-office-box-address'/3;
getdec_SupportedExtensionAttributes(id, 9) ->
    fun 'dec_x400-postal-code'/3;
getdec_SupportedExtensionAttributes(id, 19) ->
    fun 'dec_x400-poste-restante-address'/3;
getdec_SupportedExtensionAttributes(id, 17) ->
    fun 'dec_x400-street-address'/3;
getdec_SupportedExtensionAttributes(id, 2) ->
    fun 'dec_x400-teletex-common-name'/3;
getdec_SupportedExtensionAttributes(id, 6) ->
    fun 'dec_x400-teletex-domain-defined-attributes'/3;
getdec_SupportedExtensionAttributes(id, 4) ->
    fun 'dec_x400-teletex-personal-name'/3;
getdec_SupportedExtensionAttributes(id, 23) ->
    fun 'dec_x400-terminal-type'/3;
getdec_SupportedExtensionAttributes(id, 16) ->
    fun 'dec_x400-unformatted-postal-address'/3;
getdec_SupportedExtensionAttributes(id, 20) ->
    fun 'dec_x400-unique-postal-name'/3;
getdec_SupportedExtensionAttributes(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedCharacteristicTwos
%%================================
getenc_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 1}) ->
    fun 'enc_gn-basis'/3;
getenc_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 3}) ->
    fun 'enc_pp-basis'/3;
getenc_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 2}) ->
    fun 'enc_tp-basis'/3;
getenc_SupportedCharacteristicTwos(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 1}) ->
    fun 'dec_gn-basis'/3;
getdec_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 3}) ->
    fun 'dec_pp-basis'/3;
getdec_SupportedCharacteristicTwos(id,
				   {1, 2, 840, 10045, 1, 2, 3, 2}) ->
    fun 'dec_tp-basis'/3;
getdec_SupportedCharacteristicTwos(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedFieldIds
%%================================
getenc_SupportedFieldIds(id,
			 {1, 2, 840, 10045, 1, 2}) ->
    fun 'enc_field-characteristic-two'/3;
getenc_SupportedFieldIds(id,
			 {1, 2, 840, 10045, 1, 1}) ->
    fun 'enc_field-prime-field'/3;
getenc_SupportedFieldIds(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedFieldIds(id,
			 {1, 2, 840, 10045, 1, 2}) ->
    fun 'dec_field-characteristic-two'/3;
getdec_SupportedFieldIds(id,
			 {1, 2, 840, 10045, 1, 1}) ->
    fun 'dec_field-prime-field'/3;
getdec_SupportedFieldIds(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedPublicKeyAlgorithms
%%================================
getenc_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10046, 2, 1}) ->
    fun enc_dh/3;
getenc_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10040, 4, 1}) ->
    fun enc_dsa/3;
getenc_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10045, 2, 1}) ->
    fun 'enc_ec-public-key'/3;
getenc_SupportedPublicKeyAlgorithms(id,
				    {2, 16, 840, 1, 101, 2, 1, 1, 22}) ->
    fun enc_kea/3;
getenc_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 1}) ->
    fun 'enc_rsa-encryption'/3;
getenc_SupportedPublicKeyAlgorithms(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10046, 2, 1}) ->
    fun dec_dh/3;
getdec_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10040, 4, 1}) ->
    fun dec_dsa/3;
getdec_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 10045, 2, 1}) ->
    fun 'dec_ec-public-key'/3;
getdec_SupportedPublicKeyAlgorithms(id,
				    {2, 16, 840, 1, 101, 2, 1, 1, 22}) ->
    fun dec_kea/3;
getdec_SupportedPublicKeyAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 1}) ->
    fun 'dec_rsa-encryption'/3;
getdec_SupportedPublicKeyAlgorithms(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedSignatureAlgorithms
%%================================
getenc_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 10040, 4, 3}) ->
    fun 'enc_dsa-with-sha1'/3;
getenc_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 10045, 4, 1}) ->
    fun 'enc_ecdsa-with-sha1'/3;
getenc_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 2}) ->
    fun 'enc_md2-with-rsa-encryption'/3;
getenc_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 4}) ->
    fun 'enc_md5-with-rsa-encryption'/3;
getenc_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 5}) ->
    fun 'enc_sha1-with-rsa-encryption'/3;
getenc_SupportedSignatureAlgorithms(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 10040, 4, 3}) ->
    fun 'dec_dsa-with-sha1'/3;
getdec_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 10045, 4, 1}) ->
    fun 'dec_ecdsa-with-sha1'/3;
getdec_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 2}) ->
    fun 'dec_md2-with-rsa-encryption'/3;
getdec_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 4}) ->
    fun 'dec_md5-with-rsa-encryption'/3;
getdec_SupportedSignatureAlgorithms(id,
				    {1, 2, 840, 113549, 1, 1, 5}) ->
    fun 'dec_sha1-with-rsa-encryption'/3;
getdec_SupportedSignatureAlgorithms(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

%%================================
%%  SupportedAttributeTypeAndValues
%%================================
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 3}) ->
    fun enc_commonName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 6}) ->
    fun enc_countryName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 46}) ->
    fun enc_dnQualifier/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {0, 9, 2342, 19200300, 100, 1, 25}) ->
    fun enc_domainComponent/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {1, 2, 840, 113549, 1, 9, 1}) ->
    fun enc_emailAddress/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 44}) ->
    fun enc_generationQualifier/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 42}) ->
    fun enc_givenName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 43}) ->
    fun enc_initials/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 7}) ->
    fun enc_localityName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 41}) ->
    fun enc_name/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 10}) ->
    fun enc_organizationName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 11}) ->
    fun enc_organizationalUnitName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 65}) ->
    fun enc_pseudonym/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 5}) ->
    fun enc_serialNumber/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 8}) ->
    fun enc_stateOrProvinceName/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 4}) ->
    fun enc_surname/3;
getenc_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 12}) ->
    fun enc_title/3;
getenc_SupportedAttributeTypeAndValues(id, ErrV) ->
    fun (C, V, _) ->
	    exit({'Type not compatible with table constraint',
		  {component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 3}) ->
    fun dec_commonName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 6}) ->
    fun dec_countryName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 46}) ->
    fun dec_dnQualifier/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {0, 9, 2342, 19200300, 100, 1, 25}) ->
    fun dec_domainComponent/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {1, 2, 840, 113549, 1, 9, 1}) ->
    fun dec_emailAddress/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 44}) ->
    fun dec_generationQualifier/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 42}) ->
    fun dec_givenName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 43}) ->
    fun dec_initials/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 7}) ->
    fun dec_localityName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 41}) ->
    fun dec_name/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 10}) ->
    fun dec_organizationName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 11}) ->
    fun dec_organizationalUnitName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 65}) ->
    fun dec_pseudonym/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 5}) ->
    fun dec_serialNumber/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 8}) ->
    fun dec_stateOrProvinceName/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 4}) ->
    fun dec_surname/3;
getdec_SupportedAttributeTypeAndValues(id,
				       {2, 5, 4, 12}) ->
    fun dec_title/3;
getdec_SupportedAttributeTypeAndValues(id, ErrV) ->
    fun (C, V, _) ->
	    exit({{component, C}, {value, V},
		  {unique_name_and_value, id, ErrV}})
    end.

'dec-inc-Certificate'(Tlv) ->
    'dec-inc-Certificate'(Tlv, [16]).

'dec-inc-Certificate'(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    %%-------------------------------------------------
    %% attribute tbsCertificate(1)   External OTP-PKIX:TBSCertificate
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = {'Certificate_tbsCertificate', V1},
    %%-------------------------------------------------
    %% attribute signatureAlgorithm(2)   External OTP-PKIX:AlgorithmIdentifier
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = dec_AlgorithmIdentifier(V2, [16]),
    %%-------------------------------------------------
    %% attribute signature(3) with type BIT STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_compact_bit_string(V3, [], [], [3]),
    case Tlv4 of
      [] -> true;
      _ ->
	  exit({error,
		{asn1, {unexpected, Tlv4}}}) % extra fields not allowed
    end,
    {'Certificate', Term1, Term2, Term3}.

decode_TBSCert_exclusive(Bytes) ->
    decode_partial_incomplete('Certificate', Bytes,
			      [mandatory, [[undec, 16]]]).

decode_inc_disp('Certificate_tbsCertificate', Data) ->
    dec_TBSCertificate(Data, [16]).

%% =====================================================================
%% 
%% The following code stems from module `asn1rt_ber_bin_v2'.
%% 

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%% encoding / decoding of BER

-record(module,
	{pos, name, defid, tagdefault = 'EXPLICIT',
	 exports = {exports, []}, imports = {imports, []},
	 extensiondefault = empty, typeorval}).

-record('SEQUENCE',
	{pname = false, tablecinf = false, components = []}).

-record('SET',
	{pname = false, sorted = false, tablecinf = false,
	 components = []}).

-record('ComponentType',
	{pos, name, typespec, prop, tags, textual_order}).

-record('ObjectClassFieldType',
	{classname, class, fieldname, type}).

-record(typedef,
	{checked = false, pos, name, typespec}).

-record(classdef,
	{checked = false, pos, name, typespec}).

-record(valuedef,
	{checked = false, pos, name, type, value, module}).

-record(ptypedef,
	{checked = false, pos, name, args, typespec}).

-record(pvaluedef,
	{checked = false, pos, name, args, type, value}).

-record(pvaluesetdef,
	{checked = false, pos, name, args, type, valueset}).

-record(pobjectdef,
	{checked = false, pos, name, args, class, def}).

-record(pobjectsetdef,
	{checked = false, pos, name, args, class, def}).

-record(typereference, {pos, val}).

-record(identifier, {pos, val}).

-record(constraint, {c, e}).

-record('Constraint',
	{'SingleValue' = no, 'SizeConstraint' = no,
	 'ValueRange' = no, 'PermittedAlphabet' = no,
	 'ContainedSubtype' = no, 'TypeConstraint' = no,
	 'InnerSubtyping' = no, e = no, 'Other' = no}).

-record(simpletableattributes,
	{objectsetname, c_name, c_index, usedclassfield,
	 uniqueclassfield, valueindex}).

-record(type,
	{tag = [], def, constraint = [], tablecinf = [],
	 inlined = no}).

-record(objectclass, {fields = [], syntax}).

-record('Object', {classname, gen = true, def}).

-record('ObjectSet',
	{class, gen = true, uniquefname, set}).

-record(tag, {class, number, type, form = 32}).

-record(cmap,
	{single_value = no, contained_subtype = no,
	 value_range = no, size = no, permitted_alphabet = no,
	 type_constraint = no, inner_subtyping = no}).

-record('EXTENSIONMARK', {pos, val}).

-record('SymbolsFromModule', {symbols, module, objid}).

-record('Externaltypereference', {pos, module, type}).

-record('Externalvaluereference', {pos, module, value}).

-record(state,
	{module, mname, type, tname, value, vname, erule,
	 parameters = [], inputmodules, abscomppath = [],
	 recordtopname = [], options, sourcedir}).

-record(gen_state,
	{active = false, prefix, inc_tag_pattern, tag_pattern,
	 inc_type_pattern, type_pattern, func_name, namelist,
	 tobe_refed_funcs = [], gen_refed_funcs = [],
	 generated_functions = [], suffix_index = 1,
	 current_suffix_index}).

% the encoding of class of tag bits 8 and 7

%%% primitive or constructed encoding % bit 6

%%% The tag-number for universal types

% the complete tag-word of built-in types

                                                              % can be CONSTRUCTED
 % can be CONSTRUCTED

                                                               %can be constructed
 %can be constructed

                                                               %can be constructed
 %can be constructed

                                                               %can be constructed

                                                               %can be constructed
 %can be constructed

                                                               %can be constructed
 %can be constructed

                                                               %can be constructed

% encode(Tlv={_Tag={?PRIMITIVE,_},_VList}) ->
%     encode_primitive(Tlv);
% encode(Tlv) ->
%     encode_constructed(Tlv).

encode([Tlv]) -> encode(Tlv);
encode({TlvTag, TlvVal}) when is_list(TlvVal) ->
    %% constructed form of value
    encode_tlv(TlvTag, TlvVal, 32);
encode({TlvTag, TlvVal}) ->
    encode_tlv(TlvTag, TlvVal, 0);
encode(Bin) when is_binary(Bin) -> Bin.

encode_tlv(TlvTag, TlvVal, Form) ->
    Tag = encode_tlv_tag(TlvTag, Form),
    {Val, VLen} = encode_tlv_val(TlvVal),
    {Len, _LLen} = encode_length(VLen),
    BinLen = list_to_binary(Len),
    <<Tag/binary, BinLen/binary, Val/binary>>.

encode_tlv_tag(ClassTagNo, Form) ->
    Class = ClassTagNo bsr 16,
    encode_tag_val({Class bsl 6, Form,
		    ClassTagNo - (Class bsl 16)}).

encode_tlv_val(TlvL) when is_list(TlvL) ->
    encode_tlv_list(TlvL, []);
encode_tlv_val(Bin) -> {Bin, size(Bin)}.

encode_tlv_list([Tlv | Tlvs], Acc) ->
    EncTlv = encode(Tlv),
    encode_tlv_list(Tlvs, [EncTlv | Acc]);
encode_tlv_list([], Acc) ->
    Bin = list_to_binary(lists:reverse(Acc)),
    {Bin, size(Bin)}.

%% asn1-1.6.8.1
%% decode(B,driver) ->
%%     case catch port_control(asn1_driver_port,2,B) of
%% 	Bin when is_binary(Bin) ->
%% 	    binary_to_term(Bin);
%% 	List when is_list(List) -> handle_error(List,B);
%% 	{'EXIT',{badarg,Reason}} ->
%% 	    asn1rt_driver_handler:load_driver(),
%% 	    receive
%% 		driver_ready ->
%% 		    case catch port_control(asn1_driver_port,2,B) of
%% 			Bin2 when is_binary(Bin2) -> binary_to_term(Bin2);
%% 			List when is_list(List) -> handle_error(List,B);
%% 			Error -> exit(Error)
%% 		    end;
%% 		{error,Error} -> % error when loading driver
%% 		    %% the driver could not be loaded
%% 		    exit(Error);
%% 		Error={port_error,Reason} ->
%% 		    exit(Error)
%% 	    end;
%% 	{'EXIT',Reason} ->
%% 	    exit(Reason)
%%     end.

decode(Bin) when is_binary(Bin) ->
    decode_primitive(Bin);
decode(Tlv) -> % assume it is a tlv
    {Tlv, <<>>}.

decode_primitive(Bin) ->
    {Form, TagNo, V, Rest} = decode_tag_and_length(Bin),
    case Form of
      1 -> % constructed
	  {{TagNo, decode_constructed(V)}, Rest};
      0 -> % primitive
	  {{TagNo, V}, Rest};
      2 -> % constructed indefinite
	  {Vlist, Rest2} = decode_constructed_indefinite(V, []),
	  {{TagNo, Vlist}, Rest2}
    end.

decode_constructed(Bin) when byte_size(Bin) =:= 0 -> [];
decode_constructed(Bin) ->
    {Tlv, Rest} = decode_primitive(Bin),
    [Tlv | decode_constructed(Rest)].

decode_constructed_indefinite(<<0, 0, Rest/binary>>,
			      Acc) ->
    {lists:reverse(Acc), Rest};
decode_constructed_indefinite(Bin, Acc) ->
    {Tlv, Rest} = decode_primitive(Bin),
    decode_constructed_indefinite(Rest, [Tlv | Acc]).

%% decode_primitive_incomplete/2 decodes an encoded message incomplete
%% by help of the pattern attribute (first argument).
decode_primitive_incomplete([[default, TagNo]],
			    Bin) -> %default
    case decode_tag_and_length(Bin) of
      {Form, TagNo, V, Rest} ->
	  decode_incomplete2(Form, TagNo, V, [], Rest);
      _ ->
	  %{asn1_DEFAULT,Bin}
	  asn1_NOVALUE
    end;
decode_primitive_incomplete([[default, TagNo,
			      Directives]],
			    Bin) -> %default, constructed type, Directives points into this type
    case decode_tag_and_length(Bin) of
      {Form, TagNo, V, Rest} ->
	  decode_incomplete2(Form, TagNo, V, Directives, Rest);
      _ ->
	  %{asn1_DEFAULT,Bin}
	  asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt, TagNo]],
			    Bin) -> %optional
    case decode_tag_and_length(Bin) of
      {Form, TagNo, V, Rest} ->
	  decode_incomplete2(Form, TagNo, V, [], Rest);
      _ ->
	  %{{TagNo,asn1_NOVALUE},Bin}
	  asn1_NOVALUE
    end;
decode_primitive_incomplete([[opt, TagNo, Directives]],
			    Bin) -> %optional
    case decode_tag_and_length(Bin) of
      {Form, TagNo, V, Rest} ->
	  decode_incomplete2(Form, TagNo, V, Directives, Rest);
      _ ->
	  %{{TagNo,asn1_NOVALUE},Bin}
	  asn1_NOVALUE
    end;
%% An optional that shall be undecoded
decode_primitive_incomplete([[opt_undec, Tag]], Bin) ->
    case decode_tag_and_length(Bin) of
      {_, Tag, _, _} -> decode_incomplete_bin(Bin);
      _ -> asn1_NOVALUE
    end;
%% A choice alternative that shall be undecoded
decode_primitive_incomplete([[alt_undec, TagNo]
			     | RestAlts],
			    Bin) ->
    %    decode_incomplete_bin(Bin);
    %    case decode_tlv(Bin) of
    case decode_tag_and_length(Bin) of
      %	{{_Form,TagNo,_Len,_V},_R} ->
      {_, TagNo, _, _} -> decode_incomplete_bin(Bin);
      _ -> decode_primitive_incomplete(RestAlts, Bin)
    end;
decode_primitive_incomplete([[alt, TagNo] | RestAlts],
			    Bin) ->
    case decode_tag_and_length(Bin) of
      {_Form, TagNo, V, Rest} -> {{TagNo, V}, Rest};
      _ -> decode_primitive_incomplete(RestAlts, Bin)
    end;
decode_primitive_incomplete([[alt, TagNo, Directives]
			     | RestAlts],
			    Bin) ->
    case decode_tag_and_length(Bin) of
      {Form, TagNo, V, Rest} ->
	  decode_incomplete2(Form, TagNo, V, Directives, Rest);
      _ -> decode_primitive_incomplete(RestAlts, Bin)
    end;
decode_primitive_incomplete([[alt_parts, TagNo]],
			    Bin) ->
    case decode_tag_and_length(Bin) of
      {_Form, TagNo, V, Rest} -> {{TagNo, V}, Rest};
      _ -> asn1_NOVALUE
    end;
decode_primitive_incomplete([[alt_parts, TagNo]
			     | RestAlts],
			    Bin) ->
    case decode_tag_and_length(Bin) of
      {_Form, TagNo, V, Rest} ->
	  {{TagNo, decode_parts_incomplete(V)}, Rest};
      _ -> decode_primitive_incomplete(RestAlts, Bin)
    end;
decode_primitive_incomplete([[undec, _TagNo]
			     | _RestTag],
			    Bin) -> %incomlete decode
    decode_incomplete_bin(Bin);
decode_primitive_incomplete([[parts, TagNo] | _RestTag],
			    Bin) ->
    case decode_tag_and_length(Bin) of
      {_Form, TagNo, V, Rest} ->
	  {{TagNo, decode_parts_incomplete(V)}, Rest};
      Err -> {error, {asn1, "tag failure", TagNo, Err}}
    end;
decode_primitive_incomplete([mandatory | RestTag],
			    Bin) ->
    {Form, TagNo, V, Rest} = decode_tag_and_length(Bin),
    decode_incomplete2(Form, TagNo, V, RestTag, Rest);
%% A choice that is a toptype or a mandatory component of a
%% SEQUENCE or SET.
decode_primitive_incomplete([[mandatory | Directives]],
			    Bin) ->
    {Form, TagNo, V, Rest} = decode_tag_and_length(Bin),
    decode_incomplete2(Form, TagNo, V, Directives, Rest);
decode_primitive_incomplete([], Bin) ->
    decode_primitive(Bin).

%% decode_parts_incomplete/1 receives a number of values encoded in
%% sequence and returns the parts as unencoded binaries
decode_parts_incomplete(<<>>) -> [];
decode_parts_incomplete(Bin) ->
    {ok, Rest} = skip_tag(Bin),
    {ok, Rest2} = skip_length_and_value(Rest),
    LenPart = size(Bin) - size(Rest2),
    <<Part:LenPart/binary, RestBin/binary>> = Bin,
    [Part | decode_parts_incomplete(RestBin)].

%% decode_incomplete2 checks if V is a value of a constructed or
%% primitive type, and continues the decode propeerly.
decode_incomplete2(_Form = 2, TagNo, V, TagMatch, _) ->
    %% constructed indefinite length
    {Vlist, Rest2} =
	decode_constr_indef_incomplete(TagMatch, V, []),
    {{TagNo, Vlist}, Rest2};
decode_incomplete2(1, TagNo, V, [TagMatch], Rest)
    when is_list(TagMatch) ->
    {{TagNo, decode_constructed_incomplete(TagMatch, V)},
     Rest};
decode_incomplete2(1, TagNo, V, TagMatch, Rest) ->
    {{TagNo, decode_constructed_incomplete(TagMatch, V)},
     Rest};
decode_incomplete2(0, TagNo, V, _TagMatch, Rest) ->
    {{TagNo, V}, Rest}.

decode_constructed_incomplete([Tags = [Ts]], Bin)
    when is_list(Ts) ->
    decode_constructed_incomplete(Tags, Bin);
decode_constructed_incomplete(_TagMatch, <<>>) -> [];
decode_constructed_incomplete([mandatory | RestTag],
			      Bin) ->
    {Tlv, Rest} = decode_primitive(Bin),
    [Tlv | decode_constructed_incomplete(RestTag, Rest)];
decode_constructed_incomplete(Directives = [[Alt, _]
					    | _],
			      Bin)
    when Alt == alt_undec; Alt == alt; Alt == alt_parts ->
    {_Form, TagNo, V, Rest} = decode_tag_and_length(Bin),
    case incomplete_choice_alt(TagNo, Directives) of
      {alt_undec, _} ->
	  LenA = size(Bin) - size(Rest),
	  <<A:LenA/binary, Rest/binary>> = Bin,
	  A;
      {alt, InnerDirectives} ->
	  {Tlv, Rest} =
	      decode_primitive_incomplete(InnerDirectives, V),
	  {TagNo, Tlv};
      {alt_parts, _} -> [{TagNo, decode_parts_incomplete(V)}];
      no_match -> %% if a choice alternative was encoded that
	  %% was not specified in the config file,
	  %% thus decode component anonomous.
	  {Tlv, _} = decode_primitive(Bin),
	  Tlv
    end;
decode_constructed_incomplete([TagNo | RestTag], Bin) ->
    %%    {Tlv,Rest} = decode_primitive_incomplete([TagNo],Bin),
    case decode_primitive_incomplete([TagNo], Bin) of
      {Tlv, Rest} ->
	  [Tlv | decode_constructed_incomplete(RestTag, Rest)];
      asn1_NOVALUE ->
	  decode_constructed_incomplete(RestTag, Bin)
    end;
decode_constructed_incomplete([], Bin) ->
    {Tlv, Rest} = decode_primitive(Bin),
    [Tlv | decode_constructed_incomplete([], Rest)].

decode_constr_indef_incomplete(_TagMatch,
			       <<0, 0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_constr_indef_incomplete([Tag | RestTags], Bin,
			       Acc) ->
    %    {Tlv,Rest} = decode_primitive_incomplete([Tag],Bin),
    case decode_primitive_incomplete([Tag], Bin) of
      {Tlv, Rest} ->
	  decode_constr_indef_incomplete(RestTags, Rest,
					 [Tlv | Acc]);
      asn1_NOVALUE ->
	  decode_constr_indef_incomplete(RestTags, Bin, Acc)
    end.

decode_incomplete_bin(Bin) ->
    {ok, Rest} = skip_tag(Bin),
    {ok, Rest2} = skip_length_and_value(Rest),
    IncLen = size(Bin) - size(Rest2),
    <<IncBin:IncLen/binary, Ret/binary>> = Bin,
    {IncBin, Ret}.

incomplete_choice_alt(TagNo,
		      [[Alt, TagNo] | Directives]) ->
    {Alt, Directives};
incomplete_choice_alt(TagNo, [D]) when is_list(D) ->
    incomplete_choice_alt(TagNo, D);
incomplete_choice_alt(TagNo, [_H | Directives]) ->
    incomplete_choice_alt(TagNo, Directives);
incomplete_choice_alt(_, []) -> no_match.

%% skip_tag and skip_length_and_value are rutines used both by
%% decode_partial_incomplete and decode_selective (decode/2).

skip_tag(<<_:3, 31:5, Rest/binary>>) ->
    skip_long_tag(Rest);
skip_tag(<<_:3, _Tag:5, Rest/binary>>) -> {ok, Rest}.

skip_long_tag(<<1:1, _:7, Rest/binary>>) ->
    skip_long_tag(Rest);
skip_long_tag(<<0:1, _:7, Rest/binary>>) -> {ok, Rest}.

skip_length_and_value(Binary) ->
    case decode_length(Binary) of
      {indefinite, RestBinary} ->
	  skip_indefinite_value(RestBinary);
      {Length, RestBinary} ->
	  <<_:Length/unit:8, Rest/binary>> = RestBinary,
	  {ok, Rest}
    end.

skip_indefinite_value(<<0, 0, Rest/binary>>) ->
    {ok, Rest};
skip_indefinite_value(Binary) ->
    {ok, RestBinary} = skip_tag(Binary),
    {ok, RestBinary2} = skip_length_and_value(RestBinary),
    skip_indefinite_value(RestBinary2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% match_tags takes a Tlv (Tag, Length, Value) structure and matches
%% it with the tags in TagList. If the tags does not match the function
%% crashes otherwise it returns the remaining Tlv after that the tags have
%% been removed.
%%
%% match_tags(Tlv, TagList)
%%

match_tags({T, V}, [T]) -> V;
match_tags({T, V}, [T | Tt]) -> match_tags(V, Tt);
match_tags([{T, V}], [T | Tt]) -> match_tags(V, Tt);
match_tags(Vlist = [{T, _V} | _], [T]) -> Vlist;
match_tags(Tlv, []) -> Tlv;
match_tags({Tag, _V}, [T | _Tt]) ->
    {error, {asn1, {wrong_tag, {Tag, T}}}}.

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Optionals, preset not filled optionals with asn1_NOVALUE
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->
%%     8bit Int | binary
encode_tag_val({Class, Form, TagNo}) when TagNo =< 30 ->
    <<(Class bsr 6):2, (Form bsr 5):1, TagNo:5>>;
encode_tag_val({Class, Form, TagNo}) ->
    {Octets, _Len} = mk_object_val(TagNo),
    BinOct = list_to_binary(Octets),
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,
      BinOct/binary>>.

%%===============================================================================
%% Decode a tag
%%
%% decode_tag(OctetListBuffer) -> {{Form, (Class bsl 16)+ TagNo}, RestOfBuffer, RemovedBytes}
%%===============================================================================

decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 0:1,
			Length:7, V:Length/binary, RestBuffer/binary>>)
    when TagNo < 31 ->
    {Form, Class bsl 16 + TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, TagNo:5, 1:1, 0:7,
			T/binary>>)
    when TagNo < 31 ->
    {2, Class bsl 16 + TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, TagNo:5, 1:1,
			LL:7, Length:LL/unit:8, V:Length/binary, T/binary>>)
    when TagNo < 31 ->
    {Form, Class bsl 16 + TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1,
			TagNo:7, 0:1, Length:7, V:Length/binary,
			RestBuffer/binary>>) ->
    {Form, Class bsl 16 + TagNo, V, RestBuffer};
decode_tag_and_length(<<Class:2, 1:1, 31:5, 0:1,
			TagNo:7, 1:1, 0:7, T/binary>>) ->
    {2, Class bsl 16 + TagNo, T, <<>>};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 0:1,
			TagNo:7, 1:1, LL:7, Length:LL/unit:8, V:Length/binary,
			T/binary>>) ->
    {Form, Class bsl 16 + TagNo, V, T};
decode_tag_and_length(<<Class:2, Form:1, 31:5, 1:1,
			TagPart1:7, 0:1, TagPartLast, Buffer/binary>>) ->
    TagNo = TagPart1 bsl 7 bor TagPartLast,
    {Length, RestBuffer} = decode_length(Buffer),
    <<V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, Class bsl 16 + TagNo, V, RestBuffer2};
decode_tag_and_length(<<Class:2, Form:1, 31:5,
			Buffer/binary>>) ->
    {TagNo, Buffer1} = decode_tag(Buffer, 0),
    {Length, RestBuffer} = decode_length(Buffer1),
    <<V:Length/binary, RestBuffer2/binary>> = RestBuffer,
    {Form, Class bsl 16 + TagNo, V, RestBuffer2}.

%% last partial tag
decode_tag(<<0:1, PartialTag:7, Buffer/binary>>,
	   TagAck) ->
    TagNo = TagAck bsl 7 bor PartialTag,
    %%<<TagNo>> = <<TagAck:1, PartialTag:7>>,
    {TagNo, Buffer};
% more tags
decode_tag(<<_:1, PartialTag:7, Buffer/binary>>,
	   TagAck) ->
    TagAck1 = TagAck bsl 7 bor PartialTag,
    %%<<TagAck1:16>> = <<TagAck:1, PartialTag:7,0:8>>,
    decode_tag(Buffer, TagAck1).

%%=======================================================================
%%
%% Encode all tags in the list Tags and return a possibly deep list of
%% bytes with tag and length encoded
%% The taglist must be in reverse order (fixed by the asn1 compiler)
%% e.g [T1,T2] will result in
%% {[EncodedT2,EncodedT1|BytesSoFar],LenSoFar+LenT2+LenT1}
%%

encode_tags([Tag | Trest], BytesSoFar, LenSoFar) ->
    % remove    {Bytes1,L1} = encode_one_tag(Tag),
    {Bytes2, L2} = encode_length(LenSoFar),
    encode_tags(Trest, [Tag, Bytes2 | BytesSoFar],
		LenSoFar + size(Tag) + L2);
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar, LenSoFar}.

encode_tags(TagIn, {BytesSoFar, LenSoFar}) ->
    encode_tags(TagIn, BytesSoFar, LenSoFar).

% encode_one_tag(#tag{class=Class,number=No,type=Type, form = Form}) ->			
%     NewForm = case Type of
% 	       'EXPLICIT' ->
% 		   ?CONSTRUCTED;
% 	       _ ->
% 		   Form
% 	   end,
%     Bytes = encode_tag_val({Class,NewForm,No}),
%     {Bytes,size(Bytes)}.

%%===============================================================================
%%
%% This comment is valid for all the encode/decode functions
%%
%% C = Constraint -> typically {'ValueRange',LowerBound,UpperBound}
%%     used for PER-coding but not for BER-coding.
%%
%% Val = Value.  If Val is an atom then it is a symbolic integer value
%%       (i.e the atom must be one of the names in the NamedNumberList).
%%       The NamedNumberList is used to translate the atom to an integer value
%%       before encoding.
%%
%%===============================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Value) -> io_list (i.e nested list with integers, binaries)
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary

%%
encode_open_type(Val, T) when is_list(Val) ->
    encode_open_type(list_to_binary(Val), T);
encode_open_type(Val, []) -> {Val, size(Val)};
encode_open_type(Val, Tag) ->
    encode_tags(Tag, Val, size(Val)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Tlv, TagIn) -> Value
%% Tlv = {Tag,V} | V where V -> binary()
%% TagIn = [TagVal] where TagVal -> int()
%% Value = binary with decoded data (which must be decoded again as some type)
%%
decode_open_type(Tlv, TagIn) ->
    case match_tags(Tlv, TagIn) of
      Bin when is_binary(Bin) ->
	  {InnerTlv, _} = decode(Bin), InnerTlv;
      TlvBytes -> TlvBytes
    end.

decode_open_type_as_binary(Tlv, TagIn) ->
    case match_tags(Tlv, TagIn) of
      V when is_binary(V) -> V;
      [Tlv2] -> encode(Tlv2);
      Tlv2 -> encode(Tlv2)
    end.

%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Boolean, ITU_T X.690 Chapter 8.2
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode_boolean(Integer, ReversedTagList) -> {[Octet],Len}
%%===============================================================================

encode_boolean({Name, Val}, TagIn) when is_atom(Name) ->
    encode_boolean(Val, TagIn);
encode_boolean(true, TagIn) ->
    encode_tags(TagIn, [255], 1);
encode_boolean(false, TagIn) ->
    encode_tags(TagIn, [0], 1);
encode_boolean(X, _) ->
    exit({error, {asn1, {encode_boolean, X}}}).

%%===============================================================================
%% decode_boolean(BuffList, HasTag, TotalLen) -> {true, Remain, RemovedBytes} |
%%                                               {false, Remain, RemovedBytes}
%%===============================================================================
decode_boolean(Tlv, TagIn) ->
    Val = match_tags(Tlv, TagIn),
    case Val of
      <<0:8>> -> false;
      <<_:8>> -> true;
      _ -> exit({error, {asn1, {decode_boolean, Val}}})
    end.

%%===========================================================================
%% Integer, ITU_T X.690 Chapter 8.3

%% encode_integer(Constraint, Value, Tag) -> [octet list]
%% encode_integer(Constraint, Name, NamedNumberList, Tag) -> [octet list]
%%    Value = INTEGER | {Name,INTEGER}
%%    Tag = tag | notag
%%===========================================================================

encode_integer(C, Val, Tag) when is_integer(Val) ->
    encode_tags(Tag, encode_integer(C, Val));
encode_integer(C, {Name, Val}, Tag)
    when is_atom(Name) ->
    encode_integer(C, Val, Tag);
encode_integer(_C, Val, _Tag) ->
    exit({error, {asn1, {encode_integer, Val}}}).

encode_integer(C, Val, NamedNumberList, Tag)
    when is_atom(Val) ->
    case lists:keysearch(Val, 1, NamedNumberList) of
      {value, {_, NewVal}} ->
	  encode_tags(Tag, encode_integer(C, NewVal));
      _ ->
	  exit({error, {asn1, {encode_integer_namednumber, Val}}})
    end;
encode_integer(C, {_Name, Val}, NamedNumberList, Tag) ->
    encode_integer(C, Val, NamedNumberList, Tag);
encode_integer(C, Val, _NamedNumberList, Tag) ->
    encode_tags(Tag, encode_integer(C, Val)).

encode_integer(_, Val) ->
    Bytes = if Val >= 0 -> encode_integer_pos(Val, []);
	       true -> encode_integer_neg(Val, [])
	    end,
    {Bytes, length(Bytes)}.

encode_integer_pos(0, L = [B | _Acc]) when B < 128 -> L;
encode_integer_pos(N, Acc) ->
    encode_integer_pos(N bsr 8, [N band 255 | Acc]).

encode_integer_neg(-1, L = [B1 | _T]) when B1 > 127 ->
    L;
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 255 | Acc]).

%%===============================================================================
%% decode integer
%%    (Buffer, Range, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}
%%===============================================================================

decode_integer(Tlv, Range, NamedNumberList, TagIn) ->
    V = match_tags(Tlv, TagIn),
    Int = decode_integer(V),
    range_check_integer(Int, Range),
    number2name(Int, NamedNumberList).

decode_integer(Tlv, Range, TagIn) ->
    V = match_tags(Tlv, TagIn),
    Int = decode_integer(V),
    range_check_integer(Int, Range),
    Int.

%% decoding postitive integer values.
decode_integer(Bin = <<0:1, _:7, _/binary>>) ->
    Len = size(Bin),
    %    <<Int:Len/unit:8,Buffer2/binary>> = Bin,
    <<Int:Len/unit:8>> = Bin,
    Int;
%% decoding negative integer values.
decode_integer(Bin = <<1:1, B2:7, Bs/binary>>) ->
    Len = size(Bin),
    %    <<N:Len/unit:8,Buffer2/binary>> = <<B2,Bs/binary>>,
    <<N:Len/unit:8>> = <<B2, Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    Int.

range_check_integer(Int, Range) ->
    case Range of
      [] -> % No length constraint
	  Int;
      {Lb, Ub}
	  when Int >= Lb,
	       Ub >= Int -> % variable length constraint
	  Int;
      Int -> % fixed value constraint
	  Int;
      {_, _} ->
	  exit({error, {asn1, {integer_range, Range, Int}}});
      SingleValue when is_integer(SingleValue) ->
	  exit({error, {asn1, {integer_range, Range, Int}}});
      _ -> % some strange constraint that we don't support yet
	  Int
    end.

number2name(Int, []) -> Int;
number2name(Int, NamedNumberList) ->
    case lists:keysearch(Int, 2, NamedNumberList) of
      {value, {NamedVal, _}} -> NamedVal;
      _ -> Int
    end.

%%============================================================================
%% Enumerated value, ITU_T X.690 Chapter 8.4

%% encode enumerated value
%%============================================================================
encode_enumerated(Val, TagIn) when is_integer(Val) ->
    encode_tags(TagIn, encode_integer(false, Val));
encode_enumerated({Name, Val}, TagIn)
    when is_atom(Name) ->
    encode_enumerated(Val, TagIn).

%% The encode_enumerated functions below this line can be removed when the
%% new code generation is stable. (the functions might have to be kept here
%% a while longer for compatibility reasons)

%%============================================================================
%% decode enumerated value
%%   (Buffer, Range, NamedNumberList, HasTag, TotalLen) ->  Value
%%===========================================================================
decode_enumerated(Tlv, Range, NamedNumberList, Tags) ->
    Buffer = match_tags(Tlv, Tags),
    decode_enumerated_notag(Buffer, Range, NamedNumberList,
			    Tags).

decode_enumerated_notag(Buffer, _Range,
			{NamedNumberList, ExtList}, _Tags) ->
    IVal = decode_integer2(size(Buffer), Buffer),
    case decode_enumerated1(IVal, NamedNumberList) of
      {asn1_enum, IVal} -> decode_enumerated1(IVal, ExtList);
      EVal -> EVal
    end;
decode_enumerated_notag(Buffer, _Range, NNList,
			_Tags) ->
    IVal = decode_integer2(size(Buffer), Buffer),
    case decode_enumerated1(IVal, NNList) of
      {asn1_enum, _} ->
	  exit({error, {asn1, {illegal_enumerated, IVal}}});
      EVal -> EVal
    end.

decode_enumerated1(Val, NamedNumberList) ->
    %% it must be a named integer
    case lists:keysearch(Val, 2, NamedNumberList) of
      {value, {NamedVal, _}} -> NamedVal;
      _ -> {asn1_enum, Val}
    end.

%%============================================================================
%%
%% Real value, ITU_T X.690 Chapter 8.5
%%============================================================================
%%
%% encode real value
%%============================================================================

%%============================================================================
%% decode real value
%%
%% decode_real([OctetBufferList], tuple|value, tag|notag) ->
%%  {{Mantissa, Base, Exp} | realval | PLUS-INFINITY | MINUS-INFINITY | 0,
%%     RestBuff}
%%
%% only for base 2 and 10 decoding sofar!!
%%============================================================================

% decode_real2(Buffer, Form, Len) ->
%     <<First, Buffer2/binary>> = Buffer,
%     if
% 	First =:= 2#01000000 -> {'PLUS-INFINITY', Buffer2};
% 	First =:= 2#01000001 -> {'MINUS-INFINITY', Buffer2};
% 	First =:= 2#00000000 -> {0, Buffer2};
% 	true ->
% 	    %% have some check here to verify only supported bases (2)
% 	    <<B7:1,B6:1,B5_4:2,B3_2:2,B1_0:2>> = <<First>>,
% 		Sign = B6,
% 	    Base =
% 		case B5_4 of
% 		    0 -> 2;  % base 2, only one so far
% 		    _ -> exit({error,{asn1, {non_supported_base, First}}})
% 		end,
% 	    ScalingFactor =
% 		case B3_2 of
% 		    0 -> 0;  % no scaling so far
% 		    _ -> exit({error,{asn1, {non_supported_scaling, First}}})
% 		end,

% 	    {FirstLen,Exp,Buffer3} =
% 		case B1_0 of
% 		    0 ->
% 			<<_:1/unit:8,Buffer21/binary>> = Buffer2,
% 			{2, decode_integer2(1, Buffer2),Buffer21};
% 		    1 ->
% 			<<_:2/unit:8,Buffer21/binary>> = Buffer2,
% 			{3, decode_integer2(2, Buffer2)};
% 		    2 ->
% 			<<_:3/unit:8,Buffer21/binary>> = Buffer2,
% 			{4, decode_integer2(3, Buffer2)};
% 		    3 ->
% 			<<ExpLen1,RestBuffer/binary>> = Buffer2,
% 			<<_:ExpLen1/unit:8,RestBuffer2/binary>> = RestBuffer,
% 			{ ExpLen1 + 2,
% 			  decode_integer2(ExpLen1, RestBuffer, RemBytes1),
% 			  RestBuffer2}
% 		end,
% 	    Length = Len - FirstLen,
% 	    <<LongInt:Length/unit:8,RestBuff/binary>> = Buffer3,
% 	    {Mantissa, Buffer4} =
% 		if Sign =:= 0 ->

% 			{LongInt, RestBuff};%  sign plus,
% 		   true ->

% 			{-LongInt, RestBuff}%  sign minus
% 		end,
% 	    case Form of
% 		tuple ->
% 		    {Val,Buf,RemB} = Exp,
% 		    {{Mantissa, Base, {Val,Buf}}, Buffer4, RemBytes2+RemBytes3};
% 		_value ->
% 		    comming
% 	    end
%     end.

%%============================================================================
%% Bitstring value, ITU_T X.690 Chapter 8.6
%%
%% encode bitstring value
%%
%% bitstring NamedBitList
%% Val can be  of:
%% - [identifiers] where only named identifers are set to one,
%%   the Constraint must then have some information of the
%%   bitlength.
%% - [list of ones and zeroes] all bits
%% - integer value representing the bitlist
%% C is constrint Len, only valid when identifiers
%%============================================================================

encode_bit_string(C, Bin = {Unused, BinBits},
		  NamedBitList, TagIn)
    when is_integer(Unused), is_binary(BinBits) ->
    encode_bin_bit_string(C, Bin, NamedBitList, TagIn);
encode_bit_string(C, [FirstVal | RestVal], NamedBitList,
		  TagIn)
    when is_atom(FirstVal) ->
    encode_bit_string_named(C, [FirstVal | RestVal],
			    NamedBitList, TagIn);
encode_bit_string(C, [{bit, X} | RestVal], NamedBitList,
		  TagIn) ->
    encode_bit_string_named(C, [{bit, X} | RestVal],
			    NamedBitList, TagIn);
encode_bit_string(C, [FirstVal | RestVal], NamedBitList,
		  TagIn)
    when is_integer(FirstVal) ->
    encode_bit_string_bits(C, [FirstVal | RestVal],
			   NamedBitList, TagIn);
encode_bit_string(_C, 0, _NamedBitList, TagIn) ->
    encode_tags(TagIn, <<0>>, 1);
encode_bit_string(_C, [], _NamedBitList, TagIn) ->
    encode_tags(TagIn, <<0>>, 1);
encode_bit_string(C, IntegerVal, NamedBitList, TagIn)
    when is_integer(IntegerVal) ->
    BitListVal = int_to_bitlist(IntegerVal),
    encode_bit_string_bits(C, BitListVal, NamedBitList,
			   TagIn);
encode_bit_string(C, {Name, BitList}, NamedBitList,
		  TagIn)
    when is_atom(Name) ->
    encode_bit_string(C, BitList, NamedBitList, TagIn).

int_to_bitlist(0) -> [];
int_to_bitlist(Int) when is_integer(Int), Int >= 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)].

%%=================================================================
%% Encode BIT STRING of the form {Unused,BinBits}.
%% Unused is the number of unused bits in the last byte in BinBits
%% and BinBits is a binary representing the BIT STRING.
%%=================================================================
encode_bin_bit_string(C, {Unused, BinBits},
		      _NamedBitList, TagIn) ->
    case get_constraint(C, 'SizeConstraint') of
      no -> remove_unused_then_dotag(TagIn, Unused, BinBits);
      {_Min, Max} ->
	  BBLen = size(BinBits) * 8 - Unused,
	  if BBLen > Max ->
		 exit({error,
		       {asn1,
			{bitstring_length, {{was, BBLen}, {maximum, Max}}}}});
	     true -> remove_unused_then_dotag(TagIn, Unused, BinBits)
	  end;
      Size ->
	  case size(BinBits) * 8 - Unused of
	    BBSize when BBSize =< Size ->
		remove_unused_then_dotag(TagIn, Unused, BinBits);
	    BBSize ->
		exit({error,
		      {asn1,
		       {bitstring_length,
			{{was, BBSize}, {should_be, Size}}}}})
	  end
    end.

remove_unused_then_dotag(TagIn, Unused, BinBits) ->
    case Unused of
      0 when size(BinBits) == 0 ->
	  encode_tags(TagIn, <<0>>, 1);
      0 ->
	  Bin = <<Unused, BinBits/binary>>,
	  encode_tags(TagIn, Bin, size(Bin));
      Num ->
	  N = size(BinBits) - 1,
	  <<BBits:N/binary, LastByte>> = BinBits,
	  encode_tags(TagIn,
		      [Unused,
		       binary_to_list(BBits) ++ [LastByte bsr Num bsl Num]],
		      1 + size(BinBits))
    end.

%%=================================================================
%% Encode named bits
%%=================================================================

encode_bit_string_named(C, [FirstVal | RestVal],
			NamedBitList, TagIn) ->
    ToSetPos = get_all_bitposes([FirstVal | RestVal],
				NamedBitList, []),
    Size = case get_constraint(C, 'SizeConstraint') of
	     no -> lists:max(ToSetPos) + 1;
	     {_Min, Max} -> Max;
	     TSize -> TSize
	   end,
    BitList = make_and_set_list(Size, ToSetPos, 0),
    {Len, Unused, OctetList} = encode_bitstring(BitList),
    encode_tags(TagIn, [Unused | OctetList], Len + 1).

%%----------------------------------------
%% get_all_bitposes([list of named bits to set], named_bit_db, []) ->
%%   [sorted_list_of_bitpositions_to_set]
%%----------------------------------------

get_all_bitposes([{bit, ValPos} | Rest], NamedBitList,
		 Ack) ->
    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
get_all_bitposes([Val | Rest], NamedBitList, Ack)
    when is_atom(Val) ->
    case lists:keysearch(Val, 1, NamedBitList) of
      {value, {_ValName, ValPos}} ->
	  get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
      _ -> exit({error, {asn1, {bitstring_namedbit, Val}}})
    end;
get_all_bitposes([], _NamedBitList, Ack) ->
    lists:sort(Ack).

%%----------------------------------------
%% make_and_set_list(Len of list to return, [list of positions to set to 1])->
%% returns list of Len length, with all in SetPos set.
%% in positioning in list the first element is 0, the second 1 etc.., but
%% Len will make a list of length Len, not Len + 1.
%%    BitList = make_and_set_list(C, ToSetPos, 0),
%%----------------------------------------

make_and_set_list(0, [], _) -> [];
make_and_set_list(0, _, _) ->
    exit({error, {asn1, bitstring_sizeconstraint}});
make_and_set_list(Len, [XPos | SetPos], XPos) ->
    [1 | make_and_set_list(Len - 1, SetPos, XPos + 1)];
make_and_set_list(Len, [Pos | SetPos], XPos) ->
    [0 | make_and_set_list(Len - 1, [Pos | SetPos],
			   XPos + 1)];
make_and_set_list(Len, [], XPos) ->
    [0 | make_and_set_list(Len - 1, [], XPos + 1)].

%%=================================================================
%% Encode bit string for lists of ones and zeroes
%%=================================================================
encode_bit_string_bits(C, BitListVal, _NamedBitList,
		       TagIn)
    when is_list(BitListVal) ->
    case get_constraint(C, 'SizeConstraint') of
      no ->
	  {Len, Unused, OctetList} = encode_bitstring(BitListVal),
	  %%add unused byte to the Len
	  encode_tags(TagIn, [Unused | OctetList], Len + 1);
      Constr = {Min, _Max} when is_integer(Min) ->
	  %% Max may be an integer or 'MAX'
	  encode_constr_bit_str_bits(Constr, BitListVal, TagIn);
      {Constr = {_, _}, []} ->%Constr={Min,Max}
	  %% constraint with extension mark
	  encode_constr_bit_str_bits(Constr, BitListVal, TagIn);
      Constr = {{_, _}, {_, _}} ->%{{Min1,Max1},{Min2,Max2}}
	  %% constraint with extension mark
	  encode_constr_bit_str_bits(Constr, BitListVal, TagIn);
      Size ->
	  case length(BitListVal) of
	    BitSize when BitSize == Size ->
		{Len, Unused, OctetList} = encode_bitstring(BitListVal),
		%%add unused byte to the Len
		encode_tags(TagIn, [Unused | OctetList], Len + 1);
	    BitSize when BitSize < Size ->
		PaddedList = pad_bit_list(Size - BitSize, BitListVal),
		{Len, Unused, OctetList} = encode_bitstring(PaddedList),
		%%add unused byte to the Len
		encode_tags(TagIn, [Unused | OctetList], Len + 1);
	    BitSize ->
		exit({error,
		      {asn1,
		       {bitstring_length,
			{{was, BitSize}, {should_be, Size}}}}})
	  end
    end.

encode_constr_bit_str_bits({{_Min1, Max1},
			    {Min2, Max2}},
			   BitListVal, TagIn) ->
    BitLen = length(BitListVal),
    case BitLen of
      Len when Len > Max2 ->
	  exit({error,
		{asn1,
		 {bitstring_length, {{was, BitLen}, {maximum, Max2}}}}});
      Len when Len > Max1, Len < Min2 ->
	  exit({error,
		{asn1,
		 {bitstring_length,
		  {{was, BitLen}, {not_allowed_interval, Max1, Min2}}}}});
      _ ->
	  {Len, Unused, OctetList} = encode_bitstring(BitListVal),
	  %%add unused byte to the Len
	  encode_tags(TagIn, [Unused, OctetList], Len + 1)
    end;
encode_constr_bit_str_bits({Min, Max}, BitListVal,
			   TagIn) ->
    BitLen = length(BitListVal),
    if BitLen > Max ->
	   exit({error,
		 {asn1,
		  {bitstring_length, {{was, BitLen}, {maximum, Max}}}}});
       BitLen < Min ->
	   exit({error,
		 {asn1,
		  {bitstring_length, {{was, BitLen}, {minimum, Max}}}}});
       true ->
	   {Len, Unused, OctetList} = encode_bitstring(BitListVal),
	   %%add unused byte to the Len
	   encode_tags(TagIn, [Unused, OctetList], Len + 1)
    end.

%% returns a list of length Size + length(BitListVal), with BitListVal
%% as the most significant elements followed by padded zero elements
pad_bit_list(Size, BitListVal) ->
    Tail = lists:duplicate(Size, 0), BitListVal ++ Tail.

%%=================================================================
%% Do the actual encoding
%%     ([bitlist]) -> {ListLen, UnusedBits, OctetList}
%%=================================================================

encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1
		  | Rest]) ->
    Val = B8 bsl 7 bor (B7 bsl 6) bor (B6 bsl 5) bor
	    (B5 bsl 4)
	    bor (B4 bsl 3)
	    bor (B3 bsl 2)
	    bor (B2 bsl 1)
	    bor B1,
    encode_bitstring(Rest, [Val], 1);
encode_bitstring(Val) ->
    {Unused, Octet} = unused_bitlist(Val, 7, 0),
    {1, Unused, [Octet]}.

encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1
		  | Rest],
		 Ack, Len) ->
    Val = B8 bsl 7 bor (B7 bsl 6) bor (B6 bsl 5) bor
	    (B5 bsl 4)
	    bor (B4 bsl 3)
	    bor (B3 bsl 2)
	    bor (B2 bsl 1)
	    bor B1,
    encode_bitstring(Rest, [Ack, Val], Len + 1);
%%even multiple of 8 bits..
encode_bitstring([], Ack, Len) -> {Len, 0, Ack};
%% unused bits in last octet
encode_bitstring(Rest, Ack, Len) ->
    %    io:format("uneven ~w ~w ~w~n",[Rest, Ack, Len]),
    {Unused, Val} = unused_bitlist(Rest, 7, 0),
    {Len + 1, Unused, [Ack, Val]}.

%%%%%%%%%%%%%%%%%%
%% unused_bitlist([list of ones and zeros <= 7], 7, []) ->
%%  {Unused bits, Last octet with bits moved to right}
unused_bitlist([], Trail, Ack) -> {Trail + 1, Ack};
unused_bitlist([Bit | Rest], Trail, Ack) ->
    %%    io:format("trail Bit: ~w Rest: ~w Trail: ~w Ack:~w~n",[Bit, Rest, Trail, Ack]),
    unused_bitlist(Rest, Trail - 1, Bit bsl Trail bor Ack).

%%============================================================================
%% decode bitstring value
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}
%%============================================================================

decode_compact_bit_string(Buffer, Range,
			  NamedNumberList, Tags) ->
    %    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_BIT_STRING}),
    decode_restricted_string(Buffer, Range, 3, Tags,
			     NamedNumberList, bin).

decode_bit_string2(<<0>>, _NamedNumberList, BinOrOld) ->
    case BinOrOld of
      bin -> {0, <<>>};
      _ -> []
    end;
decode_bit_string2(<<Unused, Bits/binary>>,
		   NamedNumberList, BinOrOld) ->
    case NamedNumberList of
      [] ->
	  case BinOrOld of
	    bin -> {Unused, Bits};
	    _ -> decode_bitstring2(size(Bits), Unused, Bits)
	  end;
      _ ->
	  BitString = decode_bitstring2(size(Bits), Unused, Bits),
	  decode_bitstring_NNL(BitString, NamedNumberList)
    end.

%%----------------------------------------
%% Decode the in buffer to bits
%%----------------------------------------
decode_bitstring2(1, Unused,
		  <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1,
		    _/binary>>) ->
    lists:sublist([B7, B6, B5, B4, B3, B2, B1, B0],
		  8 - Unused);
decode_bitstring2(Len, Unused,
		  <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1,
		    Buffer/binary>>) ->
    [B7, B6, B5, B4, B3, B2, B1, B0 | decode_bitstring2(Len
							  - 1,
							Unused, Buffer)].

%%decode_bitstring2(1, Unused, Buffer) ->
%%    make_bits_of_int(hd(Buffer), 128, 8-Unused);
%%decode_bitstring2(Len, Unused, [BitVal | Buffer]) ->
%%    [B7, B6, B5, B4, B3, B2, B1, B0] = make_bits_of_int(BitVal, 128, 8),
%%    [B7, B6, B5, B4, B3, B2, B1, B0 |
%%     decode_bitstring2(Len - 1, Unused, Buffer)].

%%make_bits_of_int(_, _, 0) ->
%%    [];
%%make_bits_of_int(BitVal, MaskVal, Unused) when Unused > 0 ->
%%    X = case MaskVal band BitVal of
%%	    0 -> 0 ;
%%	    _ -> 1
%%	end,
%%    [X | make_bits_of_int(BitVal, MaskVal bsr 1, Unused - 1)].

%%----------------------------------------
%% Decode the bitlist to names
%%----------------------------------------

decode_bitstring_NNL(BitList, NamedNumberList) ->
    decode_bitstring_NNL(BitList, NamedNumberList, 0, []).

decode_bitstring_NNL([], _, _No, Result) ->
    lists:reverse(Result);
decode_bitstring_NNL([B | BitList],
		     [{Name, No} | NamedNumberList], No, Result) ->
    if B == 0 ->
	   decode_bitstring_NNL(BitList, NamedNumberList, No + 1,
				Result);
       true ->
	   decode_bitstring_NNL(BitList, NamedNumberList, No + 1,
				[Name | Result])
    end;
decode_bitstring_NNL([1 | BitList], NamedNumberList, No,
		     Result) ->
    decode_bitstring_NNL(BitList, NamedNumberList, No + 1,
			 [{bit, No} | Result]);
decode_bitstring_NNL([0 | BitList], NamedNumberList, No,
		     Result) ->
    decode_bitstring_NNL(BitList, NamedNumberList, No + 1,
			 Result).

%%============================================================================
%% Octet string, ITU_T X.690 Chapter 8.7
%%
%% encode octet string
%% The OctetList must be a flat list of integers in the range 0..255
%% the function does not check this because it takes to much time
%%============================================================================
encode_octet_string(_C, OctetList, TagIn)
    when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, size(OctetList));
encode_octet_string(_C, OctetList, TagIn)
    when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList));
encode_octet_string(C, {Name, OctetList}, TagIn)
    when is_atom(Name) ->
    encode_octet_string(C, OctetList, TagIn).

%%============================================================================
%% decode octet string
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%
%% Octet string is decoded as a restricted string
%%============================================================================
decode_octet_string(Buffer, Range, Tags) ->
    %    NewTags = new_tags(HasTag,#tag{class=?UNIVERSAL,number=?N_OCTET_STRING}),
    decode_restricted_string(Buffer, Range, 4, Tags, [],
			     old).

%%============================================================================
%% Null value, ITU_T X.690 Chapter 8.8
%%
%% encode NULL value
%%============================================================================

encode_null({Name, _Val}, TagIn) when is_atom(Name) ->
    encode_tags(TagIn, [], 0);
encode_null(_Val, TagIn) -> encode_tags(TagIn, [], 0).

%%============================================================================
%% decode NULL value
%%    (Buffer, HasTag, TotalLen) -> {NULL, Remain, RemovedBytes}
%%============================================================================

decode_null(Tlv, Tags) ->
    Val = match_tags(Tlv, Tags),
    case Val of
      <<>> -> 'NULL';
      _ -> exit({error, {asn1, {decode_null, Val}}})
    end.

%%============================================================================
%% Object identifier, ITU_T X.690 Chapter 8.19
%%
%% encode Object Identifier value
%%============================================================================

encode_object_identifier({Name, Val}, TagIn)
    when is_atom(Name) ->
    encode_object_identifier(Val, TagIn);
encode_object_identifier(Val, TagIn) ->
    encode_tags(TagIn, e_object_identifier(Val)).

e_object_identifier({'OBJECT IDENTIFIER', V}) ->
    e_object_identifier(V);
e_object_identifier({Cname, V})
    when is_atom(Cname), is_tuple(V) ->
    e_object_identifier(tuple_to_list(V));
e_object_identifier({Cname, V})
    when is_atom(Cname), is_list(V) ->
    e_object_identifier(V);
e_object_identifier(V) when is_tuple(V) ->
    e_object_identifier(tuple_to_list(V));
%%%%%%%%%%%%%%%
%% e_object_identifier([List of Obect Identifiers]) ->
%% {[Encoded Octetlist of ObjIds], IntLength}
%%
e_object_identifier([E1, E2 | Tail]) ->
    Head = 40 * E1 + E2,  % wow!
    {H, Lh} = mk_object_val(Head),
    {R, Lr} = enc_obj_id_tail(Tail, [], 0),
    {[H | R], Lh + Lr}.

enc_obj_id_tail([], Ack, Len) ->
    {lists:reverse(Ack), Len};
enc_obj_id_tail([H | T], Ack, Len) ->
    {B, L} = mk_object_val(H),
    enc_obj_id_tail(T, [B | Ack], Len + L).

%%%%%%%%%%%
%% mk_object_val(Value) -> {OctetList, Len}
%% returns a Val as a list of octets, the 8 bit is allways set to one except
%% for the last octet, where its 0
%%

mk_object_val(Val) when Val =< 127 ->
    {[255 band Val], 1};
mk_object_val(Val) ->
    mk_object_val(Val bsr 7, [Val band 127], 1).

mk_object_val(0, Ack, Len) -> {Ack, Len};
mk_object_val(Val, Ack, Len) ->
    mk_object_val(Val bsr 7, [Val band 127 bor 128 | Ack],
		  Len + 1).

%%============================================================================
%% decode Object Identifier value
%%    (Buffer, HasTag, TotalLen) -> {{ObjId}, Remain, RemovedBytes}
%%============================================================================

decode_object_identifier(Tlv, Tags) ->
    Val = match_tags(Tlv, Tags),
    [AddedObjVal | ObjVals] = dec_subidentifiers(Val, 0,
						 []),
    {Val1, Val2} = if AddedObjVal < 40 -> {0, AddedObjVal};
		      AddedObjVal < 80 -> {1, AddedObjVal - 40};
		      true -> {2, AddedObjVal - 80}
		   end,
    list_to_tuple([Val1, Val2 | ObjVals]).

dec_subidentifiers(<<>>, _Av, Al) -> lists:reverse(Al);
dec_subidentifiers(<<1:1, H:7, T/binary>>, Av, Al) ->
    dec_subidentifiers(T, Av bsl 7 + H, Al);
dec_subidentifiers(<<H, T/binary>>, Av, Al) ->
    dec_subidentifiers(T, 0, [Av bsl 7 + H | Al]).

%%============================================================================
%% Restricted character string types, ITU_T X.690 Chapter 8.20
%%
%% encode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings
%%============================================================================
%% The StringType arg is kept for future use but might be removed
encode_restricted_string(_C, OctetList, _StringType,
			 TagIn)
    when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, size(OctetList));
encode_restricted_string(_C, OctetList, _StringType,
			 TagIn)
    when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList));
encode_restricted_string(C, {Name, OctetL}, StringType,
			 TagIn)
    when is_atom(Name) ->
    encode_restricted_string(C, OctetL, StringType, TagIn).

%%============================================================================
%% decode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings
%%    (Buffer, Range, StringType, HasTag, TotalLen) ->
%%                                  {String, Remain, RemovedBytes}
%%============================================================================

decode_restricted_string(Buffer, Range, StringType,
			 Tags) ->
    decode_restricted_string(Buffer, Range, StringType,
			     Tags, [], old).

decode_restricted_string(Tlv, Range, StringType, TagsIn,
			 NamedNumberList, BinOrOld) ->
    Val = match_tags(Tlv, TagsIn),
    Val2 = case Val of
	     PartList = [_H | _T] -> % constructed val
		 Bin = collect_parts(PartList),
		 decode_restricted(Bin, StringType, NamedNumberList,
				   BinOrOld);
	     Bin ->
		 decode_restricted(Bin, StringType, NamedNumberList,
				   BinOrOld)
	   end,
    check_and_convert_restricted_string(Val2, StringType,
					Range, NamedNumberList, BinOrOld).

% 	case StringType of
% 	    ?N_BIT_STRING when BinOrOld == bin ->
% 		{concat_bit_binaries(AccVal, Val), AccRb+Rb};
% 	    _ when is_binary(Val),is_binary(AccVal) ->
% 		{<<AccVal/binary,Val/binary>>,AccRb+Rb};
% 	    _ when is_binary(Val), AccVal==[] ->
% 		{Val,AccRb+Rb};				
% 	    _ ->	
% 		{AccVal++Val, AccRb+Rb}
% 	end,

decode_restricted(Bin, StringType, NamedNumberList,
		  BinOrOld) ->
    case StringType of
      3 -> decode_bit_string2(Bin, NamedNumberList, BinOrOld);
      28 -> mk_universal_string(binary_to_list(Bin));
      30 -> mk_BMP_string(binary_to_list(Bin));
      _ -> Bin
    end.

check_and_convert_restricted_string(Val, StringType,
				    Range, NamedNumberList, _BinOrOld) ->
    {StrLen, NewVal} = case StringType of
			 3 when NamedNumberList /= [] -> {no_check, Val};
			 3 when is_list(Val) -> {length(Val), Val};
			 3 when is_tuple(Val) ->
			     {size(element(2, Val)) * 8 - element(1, Val), Val};
			 _ when is_binary(Val) ->
			     {size(Val), binary_to_list(Val)};
			 _ when is_list(Val) -> {length(Val), Val}
		       end,
    case Range of
      _ when StrLen == no_check -> NewVal;
      [] -> % No length constraint
	  NewVal;
      {Lb, Ub}
	  when StrLen >= Lb,
	       Ub >= StrLen -> % variable length constraint
	  NewVal;
      {{Lb, _Ub}, []} when StrLen >= Lb -> NewVal;
      {{Lb, _Ub}, _Ext = [Min | _]}
	  when StrLen >= Lb; StrLen >= Min ->
	  NewVal;
      {{Lb1, Ub1}, {Lb2, Ub2}}
	  when StrLen >= Lb1, StrLen =< Ub1;
	       StrLen =< Ub2, StrLen >= Lb2 ->
	  NewVal;
      StrLen -> % fixed length constraint
	  NewVal;
      {_, _} -> exit({error, {asn1, {length, Range, Val}}});
      _Len when is_integer(_Len) ->
	  exit({error, {asn1, {length, Range, Val}}});
      _ -> % some strange constraint that we don't support yet
	  NewVal
    end.

%%============================================================================
%% encode Universal string
%%============================================================================

encode_universal_string(C, {Name, Universal}, TagIn)
    when is_atom(Name) ->
    encode_universal_string(C, Universal, TagIn);
encode_universal_string(_C, Universal, TagIn) ->
    OctetList = mk_uni_list(Universal),
    encode_tags(TagIn, OctetList, length(OctetList)).

mk_uni_list(In) -> mk_uni_list(In, []).

mk_uni_list([], List) -> lists:reverse(List);
mk_uni_list([{A, B, C, D} | T], List) ->
    mk_uni_list(T, [D, C, B, A | List]);
mk_uni_list([H | T], List) ->
    mk_uni_list(T, [H, 0, 0, 0 | List]).

%%===========================================================================
%% decode Universal strings
%%    (Buffer, Range, StringType, HasTag, LenIn) ->
%%                           {String, Remain, RemovedBytes}
%%===========================================================================

decode_universal_string(Buffer, Range, Tags) ->
    decode_restricted_string(Buffer, Range, 28, Tags, [],
			     old).

mk_universal_string(In) -> mk_universal_string(In, []).

mk_universal_string([], Acc) -> lists:reverse(Acc);
mk_universal_string([0, 0, 0, D | T], Acc) ->
    mk_universal_string(T, [D | Acc]);
mk_universal_string([A, B, C, D | T], Acc) ->
    mk_universal_string(T, [{A, B, C, D} | Acc]).

%%============================================================================
%% encode UTF8 string
%%============================================================================

encode_UTF8_string(_C, UTF8String, TagIn)
    when is_binary(UTF8String) ->
    encode_tags(TagIn, UTF8String, size(UTF8String));
encode_UTF8_string(_C, UTF8String, TagIn) ->
    encode_tags(TagIn, UTF8String, length(UTF8String)).

%%============================================================================
%% decode UTF8 string
%%============================================================================

decode_UTF8_string(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
      PartList = [_H | _T] -> % constructed val
	  collect_parts(PartList);
      Bin -> Bin
    end.

%%============================================================================
%% encode BMP string
%%============================================================================

encode_BMP_string(C, {Name, BMPString}, TagIn)
    when is_atom(Name) ->
    encode_BMP_string(C, BMPString, TagIn);
encode_BMP_string(_C, BMPString, TagIn) ->
    OctetList = mk_BMP_list(BMPString),
    encode_tags(TagIn, OctetList, length(OctetList)).

mk_BMP_list(In) -> mk_BMP_list(In, []).

mk_BMP_list([], List) -> lists:reverse(List);
mk_BMP_list([{0, 0, C, D} | T], List) ->
    mk_BMP_list(T, [D, C | List]);
mk_BMP_list([H | T], List) ->
    mk_BMP_list(T, [H, 0 | List]).

%%============================================================================
%% decode (OctetList, Range(ignored), tag|notag) -> {ValList, RestList}
%%    (Buffer, Range, StringType, HasTag, TotalLen) ->
%%                               {String, Remain, RemovedBytes}
%%============================================================================
decode_BMP_string(Buffer, Range, Tags) ->
    decode_restricted_string(Buffer, Range, 30, Tags, [],
			     old).

mk_BMP_string(In) -> mk_BMP_string(In, []).

mk_BMP_string([], US) -> lists:reverse(US);
mk_BMP_string([0, B | T], US) ->
    mk_BMP_string(T, [B | US]);
mk_BMP_string([C, D | T], US) ->
    mk_BMP_string(T, [{0, 0, C, D} | US]).

%%============================================================================
%% Generalized time, ITU_T X.680 Chapter 39
%%
%% encode Generalized time
%%============================================================================

encode_generalized_time(C, {Name, OctetList}, TagIn)
    when is_atom(Name) ->
    encode_generalized_time(C, OctetList, TagIn);
encode_generalized_time(_C, OctetList, TagIn) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

%%============================================================================
%% decode Generalized time
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%============================================================================

decode_generalized_time(Tlv, _Range, Tags) ->
    Val = match_tags(Tlv, Tags),
    NewVal = case Val of
	       PartList = [_H | _T] -> % constructed
		   collect_parts(PartList);
	       Bin -> Bin
	     end,
    binary_to_list(NewVal).

%%============================================================================
%% Universal time, ITU_T X.680 Chapter 40
%%
%% encode UTC time
%%============================================================================

encode_utc_time(C, {Name, OctetList}, TagIn)
    when is_atom(Name) ->
    encode_utc_time(C, OctetList, TagIn);
encode_utc_time(_C, OctetList, TagIn) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

%%============================================================================
%% decode UTC time
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}
%%============================================================================

decode_utc_time(Tlv, _Range, Tags) ->
    Val = match_tags(Tlv, Tags),
    NewVal = case Val of
	       PartList = [_H | _T] -> % constructed
		   collect_parts(PartList);
	       Bin -> Bin
	     end,
    binary_to_list(NewVal).

%%============================================================================
%% Length handling
%%
%% Encode length
%%
%% encode_length(Int | indefinite) ->
%%          [<127]| [128 + Int (<127),OctetList] | [16#80]
%%============================================================================

encode_length(indefinite) ->
    {[128], 1}; % 128
encode_length(L) when L =< 127 -> {[L], 1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if Len =< 126 -> {[128 + Len | Oct], Len + 1};
       true -> exit({error, {asn1, to_long_length_oct, Len}})
    end.

%% Val must be >= 0
minimum_octets(Val) -> minimum_octets(Val, []).

minimum_octets(0, Acc) -> Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 255 | Acc]).

%%===========================================================================
%% Decode length
%%
%% decode_length(OctetList) -> {{indefinite, RestOctetsL}, NoRemovedBytes} |
%%                             {{Length, RestOctetsL}, NoRemovedBytes}
%%===========================================================================

decode_length(<<1:1, 0:7, T/binary>>) ->
    {indefinite, T};
decode_length(<<0:1, Length:7, T/binary>>) ->
    {Length, T};
decode_length(<<1:1, LL:7, T/binary>>) ->
    <<Length:LL/unit:8, Rest/binary>> = T, {Length, Rest}.

%%-------------------------------------------------------------------------
%% INTERNAL HELPER FUNCTIONS (not exported)
%%-------------------------------------------------------------------------

%% decoding postitive integer values.
decode_integer2(Len, Bin = <<0:1, _:7, _Bs/binary>>) ->
    <<Int:Len/unit:8>> = Bin, Int;
%% decoding negative integer values.
decode_integer2(Len, <<1:1, B2:7, Bs/binary>>) ->
    <<N:Len/unit:8>> = <<B2, Bs/binary>>,
    Int = N - (1 bsl (8 * Len - 1)),
    Int.

get_constraint(C, Key) ->
    case lists:keysearch(Key, 1, C) of
      false -> no;
      {value, {_, V}} -> V
    end.

collect_parts(TlvList) -> collect_parts(TlvList, []).

collect_parts([{_, L} | Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L) | Acc]);
collect_parts([{3, <<Unused, Bits/binary>>} | Rest],
	      _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T, V} | Rest], Acc) ->
    collect_parts(Rest, [V | Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{3, <<Unused, Bits/binary>>} | Rest],
		  Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits | Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc | lists:reverse(Acc)]).

%% =====================================================================
%% 
%% The following code stems from module `asn1rt_check'.
%% 

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%<<< -record(module,
%%<<<         {pos, name, defid, tagdefault = 'EXPLICIT',
%%<<<          exports = {exports, []}, imports = {imports, []},
%%<<<          extensiondefault = empty, typeorval}).

%%<<< -record('SEQUENCE',
%%<<<         {pname = false, tablecinf = false, components = []}).

%%<<< -record('SET',
%%<<<         {pname = false, sorted = false, tablecinf = false,
%%<<<          components = []}).

%%<<< -record('ComponentType',
%%<<<         {pos, name, typespec, prop, tags, textual_order}).

%%<<< -record('ObjectClassFieldType',
%%<<<         {classname, class, fieldname, type}).

%%<<< -record(typedef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(classdef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(valuedef,
%%<<<         {checked = false, pos, name, type, value, module}).

%%<<< -record(ptypedef,
%%<<<         {checked = false, pos, name, args, typespec}).

%%<<< -record(pvaluedef,
%%<<<         {checked = false, pos, name, args, type, value}).

%%<<< -record(pvaluesetdef,
%%<<<         {checked = false, pos, name, args, type, valueset}).

%%<<< -record(pobjectdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(pobjectsetdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(typereference, {pos, val}).

%%<<< -record(identifier, {pos, val}).

%%<<< -record(constraint, {c, e}).

%%<<< -record('Constraint',
%%<<<         {'SingleValue' = no, 'SizeConstraint' = no,
%%<<<          'ValueRange' = no, 'PermittedAlphabet' = no,
%%<<<          'ContainedSubtype' = no, 'TypeConstraint' = no,
%%<<<          'InnerSubtyping' = no, e = no, 'Other' = no}).

%%<<< -record(simpletableattributes,
%%<<<         {objectsetname, c_name, c_index, usedclassfield,
%%<<<          uniqueclassfield, valueindex}).

%%<<< -record(type,
%%<<<         {tag = [], def, constraint = [], tablecinf = [],
%%<<<          inlined = no}).

%%<<< -record(objectclass, {fields = [], syntax}).

%%<<< -record('Object', {classname, gen = true, def}).

%%<<< -record('ObjectSet',
%%<<<         {class, gen = true, uniquefname, set}).

%%<<< -record(tag, {class, number, type, form = 32}).

%%<<< -record(cmap,
%%<<<         {single_value = no, contained_subtype = no,
%%<<<          value_range = no, size = no, permitted_alphabet = no,
%%<<<          type_constraint = no, inner_subtyping = no}).

%%<<< -record('EXTENSIONMARK', {pos, val}).

%%<<< -record('SymbolsFromModule', {symbols, module, objid}).

%%<<< -record('Externaltypereference', {pos, module, type}).

%%<<< -record('Externalvaluereference', {pos, module, value}).

%%<<< -record(state,
%%<<<         {module, mname, type, tname, value, vname, erule,
%%<<<          parameters = [], inputmodules, abscomppath = [],
%%<<<          recordtopname = [], options, sourcedir}).

%%<<< -record(gen_state,
%%<<<         {active = false, prefix, inc_tag_pattern, tag_pattern,
%%<<<          inc_type_pattern, type_pattern, func_name, namelist,
%%<<<          tobe_refed_funcs = [], gen_refed_funcs = [],
%%<<<          generated_functions = [], suffix_index = 1,
%%<<<          current_suffix_index}).

check_bool(_Bool, asn1_DEFAULT) -> true;
check_bool(Bool, Bool)
    when Bool == true; Bool == false ->
    true;
check_bool(_Bool1, Bool2) -> throw({error, Bool2}).

check_int(_, asn1_DEFAULT, _) -> true;
check_int(Value, Value, _) when is_integer(Value) ->
    true;
check_int(DefValue, Value, NNL) when is_atom(Value) ->
    case lists:keysearch(Value, 1, NNL) of
      {value, {_, DefValue}} -> true;
      _ -> throw({error, DefValue})
    end;
check_int(DefaultValue, _Value, _) ->
    throw({error, DefaultValue}).

% check_bitstring([H|T],[H|T],_) when is_integer(H) ->
%     true;
% check_bitstring(V,V,_) when is_integer(V) ->
%     true;
%% Two equal lists or integers
check_bitstring(_, asn1_DEFAULT, _) -> true;
check_bitstring(V, V, _) -> true;
%% Default value as a list of 1 and 0 and user value as an integer
check_bitstring(L = [H | T], Int, _)
    when is_integer(Int), is_integer(H) ->
    case bit_list_to_int(L, length(T)) of
      Int -> true;
      _ -> throw({error, L, Int})
    end;
%% Default value as an integer, val as list
check_bitstring(Int, Val, NBL)
    when is_integer(Int), is_list(Val) ->
    BL = int_to_bit_list(Int, [], length(Val)),
    check_bitstring(BL, Val, NBL);
%% Default value and user value as lists of ones and zeros
check_bitstring(L1 = [H1 | _T1], L2 = [H2 | _T2],
		NBL = [_H | _T])
    when is_integer(H1), is_integer(H2) ->
    L2new = remove_trailing_zeros(L2),
    check_bitstring(L1, L2new, NBL);
%% Default value as a list of 1 and 0 and user value as a list of atoms
check_bitstring(L1 = [H1 | _T1], L2 = [H2 | _T2], NBL)
    when is_integer(H1), is_atom(H2) ->
    L3 = bit_list_to_nbl(L1, NBL, 0, []),
    check_bitstring(L3, L2, NBL);
%% Both default value and user value as a list of atoms
check_bitstring(L1 = [H1 | T1], L2 = [H2 | _T2], _)
    when is_atom(H1), is_atom(H2),
	 length(L1) == length(L2) ->
    case lists:member(H1, L2) of
      true -> check_bitstring1(T1, L2);
      false -> throw({error, L2})
    end;
%% Default value as a list of atoms and user value as a list of 1 and 0
check_bitstring(L1 = [H1 | _T1], L2 = [H2 | _T2], NBL)
    when is_atom(H1), is_integer(H2) ->
    L3 = bit_list_to_nbl(L2, NBL, 0, []),
    check_bitstring(L1, L3, NBL);
%% User value in compact format
check_bitstring(DefVal, CBS = {_, _}, NBL) ->
    NewVal = cbs_to_bit_list(CBS),
    check_bitstring(DefVal, NewVal, NBL);
check_bitstring(DV, V, _) -> throw({error, DV, V}).

bit_list_to_int([0 | Bs], ShL) ->
    bit_list_to_int(Bs, ShL - 1) + 0;
bit_list_to_int([1 | Bs], ShL) ->
    bit_list_to_int(Bs, ShL - 1) + (1 bsl ShL);
bit_list_to_int([], _) -> 0.

int_to_bit_list(0, Acc, 0) -> Acc;
int_to_bit_list(Int, Acc, Len) ->
    int_to_bit_list(Int bsr 1, [Int band 1 | Acc], Len - 1).

bit_list_to_nbl([0 | T], NBL, Pos, Acc) ->
    bit_list_to_nbl(T, NBL, Pos + 1, Acc);
bit_list_to_nbl([1 | T], NBL, Pos, Acc) ->
    case lists:keysearch(Pos, 2, NBL) of
      {value, {N, _}} ->
	  bit_list_to_nbl(T, NBL, Pos + 1, [N | Acc]);
      _ -> throw({error, {no, named, element, at, pos, Pos}})
    end;
bit_list_to_nbl([], _, _, Acc) -> Acc.

remove_trailing_zeros(L2) ->
    remove_trailing_zeros1(lists:reverse(L2)).

remove_trailing_zeros1(L) ->
    lists:reverse(lists:dropwhile(fun (0) -> true;
				      (_) -> false
				  end,
				  L)).

check_bitstring1([H | T], NBL) ->
    case lists:member(H, NBL) of
      true -> check_bitstring1(T, NBL);
      V -> throw({error, V})
    end;
check_bitstring1([], _) -> true.

cbs_to_bit_list({Unused,
		 <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1,
		   Rest/binary>>})
    when size(Rest) >= 1 ->
    [B7, B6, B5, B4, B3, B2, B1, B0
     | cbs_to_bit_list({Unused, Rest})];
cbs_to_bit_list({0,
		 <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>}) ->
    [B7, B6, B5, B4, B3, B2, B1, B0];
cbs_to_bit_list({Unused, Bin}) when size(Bin) == 1 ->
    Used = 8 - Unused,
    <<Int:Used, _:Unused>> = Bin,
    int_to_bit_list(Int, [], Used).

%% dynamicsort_SETOF(Arg) -> Res
%% Arg -> list()
%% Res -> list()
%% Sorts the elements in Arg in increasing size
dynamicsort_SETOF(ListOfEncVal) ->
    BinL = [dynamicsort_SETOF_1(V1) || V1 <- ListOfEncVal],
    lists:sort(BinL).

dynamicsort_SETOF_1(L) when is_list(L) ->
    list_to_binary(L);
dynamicsort_SETOF_1(B) -> B.

%% =====================================================================
%% 
%% The following code stems from module `asn1rt_driver_handler'.
%% 

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%<<< -record(module,
%%<<<         {pos, name, defid, tagdefault = 'EXPLICIT',
%%<<<          exports = {exports, []}, imports = {imports, []},
%%<<<          extensiondefault = empty, typeorval}).

%%<<< -record('SEQUENCE',
%%<<<         {pname = false, tablecinf = false, components = []}).

%%<<< -record('SET',
%%<<<         {pname = false, sorted = false, tablecinf = false,
%%<<<          components = []}).

%%<<< -record('ComponentType',
%%<<<         {pos, name, typespec, prop, tags, textual_order}).

%%<<< -record('ObjectClassFieldType',
%%<<<         {classname, class, fieldname, type}).

%%<<< -record(typedef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(classdef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(valuedef,
%%<<<         {checked = false, pos, name, type, value, module}).

%%<<< -record(ptypedef,
%%<<<         {checked = false, pos, name, args, typespec}).

%%<<< -record(pvaluedef,
%%<<<         {checked = false, pos, name, args, type, value}).

%%<<< -record(pvaluesetdef,
%%<<<         {checked = false, pos, name, args, type, valueset}).

%%<<< -record(pobjectdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(pobjectsetdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(typereference, {pos, val}).

%%<<< -record(identifier, {pos, val}).

%%<<< -record(constraint, {c, e}).

%%<<< -record('Constraint',
%%<<<         {'SingleValue' = no, 'SizeConstraint' = no,
%%<<<          'ValueRange' = no, 'PermittedAlphabet' = no,
%%<<<          'ContainedSubtype' = no, 'TypeConstraint' = no,
%%<<<          'InnerSubtyping' = no, e = no, 'Other' = no}).

%%<<< -record(simpletableattributes,
%%<<<         {objectsetname, c_name, c_index, usedclassfield,
%%<<<          uniqueclassfield, valueindex}).

%%<<< -record(type,
%%<<<         {tag = [], def, constraint = [], tablecinf = [],
%%<<<          inlined = no}).

%%<<< -record(objectclass, {fields = [], syntax}).

%%<<< -record('Object', {classname, gen = true, def}).

%%<<< -record('ObjectSet',
%%<<<         {class, gen = true, uniquefname, set}).

%%<<< -record(tag, {class, number, type, form = 32}).

%%<<< -record(cmap,
%%<<<         {single_value = no, contained_subtype = no,
%%<<<          value_range = no, size = no, permitted_alphabet = no,
%%<<<          type_constraint = no, inner_subtyping = no}).

%%<<< -record('EXTENSIONMARK', {pos, val}).

%%<<< -record('SymbolsFromModule', {symbols, module, objid}).

%%<<< -record('Externaltypereference', {pos, module, type}).

%%<<< -record('Externalvaluereference', {pos, module, value}).

%%<<< -record(state,
%%<<<         {module, mname, type, tname, value, vname, erule,
%%<<<          parameters = [], inputmodules, abscomppath = [],
%%<<<          recordtopname = [], options, sourcedir}).

%%<<< -record(gen_state,
%%<<<         {active = false, prefix, inc_tag_pattern, tag_pattern,
%%<<<          inc_type_pattern, type_pattern, func_name, namelist,
%%<<<          tobe_refed_funcs = [], gen_refed_funcs = [],
%%<<<          generated_functions = [], suffix_index = 1,
%%<<<          current_suffix_index}).

%% Internal exports
%%<<< -export([init/2]).

%% Macros

%% =====================================================================
%% 
%% The following code stems from module `asn1rt'.
%% 

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

%%<<< -record(module,
%%<<<         {pos, name, defid, tagdefault = 'EXPLICIT',
%%<<<          exports = {exports, []}, imports = {imports, []},
%%<<<          extensiondefault = empty, typeorval}).

%%<<< -record('SEQUENCE',
%%<<<         {pname = false, tablecinf = false, components = []}).

%%<<< -record('SET',
%%<<<         {pname = false, sorted = false, tablecinf = false,
%%<<<          components = []}).

%%<<< -record('ComponentType',
%%<<<         {pos, name, typespec, prop, tags, textual_order}).

%%<<< -record('ObjectClassFieldType',
%%<<<         {classname, class, fieldname, type}).

%%<<< -record(typedef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(classdef,
%%<<<         {checked = false, pos, name, typespec}).

%%<<< -record(valuedef,
%%<<<         {checked = false, pos, name, type, value, module}).

%%<<< -record(ptypedef,
%%<<<         {checked = false, pos, name, args, typespec}).

%%<<< -record(pvaluedef,
%%<<<         {checked = false, pos, name, args, type, value}).

%%<<< -record(pvaluesetdef,
%%<<<         {checked = false, pos, name, args, type, valueset}).

%%<<< -record(pobjectdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(pobjectsetdef,
%%<<<         {checked = false, pos, name, args, class, def}).

%%<<< -record(typereference, {pos, val}).

%%<<< -record(identifier, {pos, val}).

%%<<< -record(constraint, {c, e}).

%%<<< -record('Constraint',
%%<<<         {'SingleValue' = no, 'SizeConstraint' = no,
%%<<<          'ValueRange' = no, 'PermittedAlphabet' = no,
%%<<<          'ContainedSubtype' = no, 'TypeConstraint' = no,
%%<<<          'InnerSubtyping' = no, e = no, 'Other' = no}).

%%<<< -record(simpletableattributes,
%%<<<         {objectsetname, c_name, c_index, usedclassfield,
%%<<<          uniqueclassfield, valueindex}).

%%<<< -record(type,
%%<<<         {tag = [], def, constraint = [], tablecinf = [],
%%<<<          inlined = no}).

%%<<< -record(objectclass, {fields = [], syntax}).

%%<<< -record('Object', {classname, gen = true, def}).

%%<<< -record('ObjectSet',
%%<<<         {class, gen = true, uniquefname, set}).

%%<<< -record(tag, {class, number, type, form = 32}).

%%<<< -record(cmap,
%%<<<         {single_value = no, contained_subtype = no,
%%<<<          value_range = no, size = no, permitted_alphabet = no,
%%<<<          type_constraint = no, inner_subtyping = no}).

%%<<< -record('EXTENSIONMARK', {pos, val}).

%%<<< -record('SymbolsFromModule', {symbols, module, objid}).

%%<<< -record('Externaltypereference', {pos, module, type}).

%%<<< -record('Externalvaluereference', {pos, module, value}).

%%<<< -record(state,
%%<<<         {module, mname, type, tname, value, vname, erule,
%%<<<          parameters = [], inputmodules, abscomppath = [],
%%<<<          recordtopname = [], options, sourcedir}).

%%<<< -record(gen_state,
%%<<<         {active = false, prefix, inc_tag_pattern, tag_pattern,
%%<<<          inc_type_pattern, type_pattern, func_name, namelist,
%%<<<          tobe_refed_funcs = [], gen_refed_funcs = [],
%%<<<          generated_functions = [], suffix_index = 1,
%%<<<          current_suffix_index}).

%% Runtime functions for ASN.1 (i.e encode, decode)

%% asn1-1.6.8.1	
%% load_driver() ->
%%     asn1rt_driver_handler:load_driver(),
%%     receive
%% 	driver_ready ->
%% 	    ok;
%% 	Err={error,_Reason} ->
%% 	    Err;
%% 	Error ->
%% 	    {error,Error}
%%     end.

%% macros used for utf8 encoding

