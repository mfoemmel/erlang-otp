%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%% Purpose : API module for decoding of certificates.

-module(ssl_pkix).

-include("ssl_pkix.hrl").

-record('AlgorithmIdentifier',{
	  algorithm, 
	  parameters = asn1_NOVALUE}).


-export([decode_cert_file/1, decode_cert_file/2, 
	 decode_cert/1, decode_cert/2]).

decode_cert_file(File) ->
    decode_cert_file(File, []).

decode_cert_file(File, Opts) ->
    case lists:member(pem, Opts) of
	true -> {ok, [{cert, Bin}]} = ssl_pem:read_file(File),
	    decode_cert(Bin, lists:delete(pem, Opts));
	false  ->
	    {ok, Bin} = file:read_file(File),
	    decode_cert(Bin, Opts)
    end.

decode_cert(Bin) ->
    decode_cert(Bin, []).

decode_cert(Bin, []) when binary(Bin) ->
    {ok, Bin};
decode_cert(Bin, Opts) when binary(Bin) ->
    {ok, Cert} = 'PKIX1Explicit88':decode('Certificate', Bin),
    case lists:member(ssl, Opts) of
	true ->
	    NCert = transform(Cert),
	    case lists:member(subject, Opts) of
		true ->
		    {ok, get_subj(NCert)};
		false  ->
		    {ok, NCert}
	    end;
	false ->
	    case lists:member(pkix, Opts) of
		true ->
		    case lists:member(subject, Opts) of
			true ->
			    {ok, get_subj(Cert)};
			false  ->
			    {ok, Cert}
		    end;
		false ->
		    {error, eoptions}
	    end
    end.


%% Transfrom from PKIX1-Explicit88 to SSL-PKIX. 

transform(Cert) when record(Cert, 'Certificate') ->
    #'Certificate'{tbsCertificate = 
		   transform(Cert#'Certificate'.tbsCertificate),
		   signatureAlgorithm = 
		   transform(Cert#'Certificate'.signatureAlgorithm),
		   signature = 
		   transform(Cert#'Certificate'.signature)};

%% -record('TBSCertificate',{
%% version = asn1_DEFAULT, serialNumber, signature, issuer, validity, subject,
%% subjectPublicKeyInfo, issuerUniqueID = asn1_NOVALUE, 
%% subjectUniqueID = asn1_NOVALUE, extensions = asn1_NOVALUE}).

transform(TBSCert) when record(TBSCert, 'TBSCertificate') ->
    TBSCert#'TBSCertificate'{
      subject = transform(TBSCert#'TBSCertificate'.subject),
      issuer = transform(TBSCert#'TBSCertificate'.issuer),
      subjectPublicKeyInfo = 
      transform(TBSCert#'TBSCertificate'.subjectPublicKeyInfo),
      signature = transform(TBSCert#'TBSCertificate'.signature),
      extensions = transform_extensions(TBSCert#'TBSCertificate'.extensions)
     };

transform(SignAlg) when record(SignAlg, 'AlgorithmIdentifier') ->
    SignAlgAny = #'SignatureAlgorithm-Any'
      {algorithm = SignAlg#'AlgorithmIdentifier'.algorithm, 
       parameters = SignAlg#'AlgorithmIdentifier'.parameters},
    {ok, AnyEnc} = 'SSL-PKIX':encode('SignatureAlgorithm-Any', SignAlgAny),
    {ok, SignAlgCd} =  'SSL-PKIX':decode('SignatureAlgorithm', 
					 list_to_binary(AnyEnc)),
    NAlgo = ssl_pkix_oid:id2atom(SignAlgCd#'SignatureAlgorithm'.algorithm),
    SignAlgCd#'SignatureAlgorithm'{algorithm = NAlgo};

transform({rdnSequence, Lss}) when list(Lss) ->
    {rndSequence, 
     lists:map(fun(Ls) -> lists:map(fun(L) -> transform(L) end, Ls)
	       end, Lss)};
transform({rdnSequence, Lss}) ->
    {rdnSequence, Lss}; 

transform(ATAV) when record(ATAV, 'AttributeTypeAndValue') ->
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue', ATAV),
    {ok, ATAVDec} = 'SSL-PKIX':decode('AttributeTypeAndValue', 
				      list_to_binary(ATAVEnc)),
    Type = ATAVDec#'AttributeTypeAndValue'.type,
    ATAVDec#'AttributeTypeAndValue'{type =  ssl_pkix_oid:id2atom(Type)};

%% -record('SubjectPublicKeyInfo',{
%% algorithm, subjectPublicKey}).
%%
%% -record('SubjectPublicKeyInfo_algorithm',{
%% algo, parameters = asn1_NOVALUE}).
%%
%% -record('SubjectPublicKeyInfo-Any',{
%% algorithm, subjectPublicKey}).
%%
%% -record('PublicKeyAlgorithm',{
%% algorithm, parameters = asn1_NOVALUE}).

transform(SInfo) when record(SInfo, 'SubjectPublicKeyInfo') ->
    %% Transform from SubjectPublicKeyInfo (PKIX1Explicit88) 
    %% to SubjectPublicKeyInfo-Any (SSL-PKIX). 
    Algorithm = SInfo#'SubjectPublicKeyInfo'.algorithm,
    Algo = Algorithm#'AlgorithmIdentifier'.algorithm,
    Parameters = Algorithm#'AlgorithmIdentifier'.parameters,
    SubjectPublicKey = SInfo#'SubjectPublicKeyInfo'.subjectPublicKey,

    AlgorithmAny = #'PublicKeyAlgorithm'{algorithm = Algo, 
					 parameters = Parameters},
    {0, Bin} = SubjectPublicKey,
    SInfoAny = #'SubjectPublicKeyInfo-Any'{algorithm = AlgorithmAny,
					    subjectPublicKey = Bin},

    %% Encode according to SubjectPublicKeyInfo-Any, and decode according
    %% to SubjectPublicKeyInfo. 
    {ok, AnyEnc} = 'SSL-PKIX':encode('SubjectPublicKeyInfo-Any', SInfoAny),
    {ok, SInfoCd} =  'SSL-PKIX':decode('SubjectPublicKeyInfo', 
					list_to_binary(AnyEnc)),
    %% Replace object identifier by atom
    AlgorithmCd = SInfoCd#'SubjectPublicKeyInfo'.algorithm,
    AlgoCd = AlgorithmCd#'SubjectPublicKeyInfo_algorithm'.algo, 
    NAlgoCd = ssl_pkix_oid:id2atom(AlgoCd),
    NAlgorithmCd = AlgorithmCd#'SubjectPublicKeyInfo_algorithm'{
		   algo = NAlgoCd},
    SInfoCd#'SubjectPublicKeyInfo'{algorithm = NAlgorithmCd};

transform(Ext) when record(Ext, 'Extension') ->
    NExtID = ssl_pkix_oid:id2atom(Ext#'Extension'.extnID),
    ExtAny = setelement(1, Ext, 'Extension-Any'),
    {ok, AnyEnc} = 'SSL-PKIX':encode('Extension-Any', ExtAny),
    {ok, ExtCd} =  'SSL-PKIX':decode('Extension', 
				      list_to_binary(AnyEnc)),
    ExtCd#'Extension'{extnID = NExtID};

transform(X) ->
    X.

transform_extensions(Exts) when list(Exts) ->
    lists:map(fun(Ext) -> transform(Ext) end, Exts);
transform_extensions(Exts) ->
    Exts.



get_subj(Cert) ->
    (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject.
    
    


    
    
