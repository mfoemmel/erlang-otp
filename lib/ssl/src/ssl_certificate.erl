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
%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling certificat verification.
%% The path validation defined in ssl_handshake.erl that mainly
%% calls functions in this module is described in RFC 3280. 
%%----------------------------------------------------------------------

-module(ssl_certificate).

-include("ssl_handshake.hrl").
-include("ssl_pkix.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_debug.hrl").

-export([trusted_cert_and_path/2, init_validation_state/2,
 	 prepare_for_next_cert/2,
 	 validate_time/1, validate_signature/3,
 	 validate_issuer/2, validate_names/2,
	 is_not_revoked/1, validate_extensions/2,
	 normalize_general_name/1,
	 get_public_key_info/1,
	 certificate_chain/2, file_to_certificats/1]).
 
%%====================================================================
%% Internal application API
%%====================================================================
get_public_key_info(BinCert) ->
    {ok, ErlCert} = ssl_pkix:decode_cert(BinCert, [ssl]),
    TBSCert = ErlCert#'Certificate'.tbsCertificate, 
    PublicKeyInfo = TBSCert#'TBSCertificate'.subjectPublicKeyInfo,
    PublicKey = PublicKeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'SubjectPublicKeyInfo'.algorithm,
    PublicKeyParams = AlgInfo#'SubjectPublicKeyInfo_algorithm'.parameters,
    Algorithm = AlgInfo#'SubjectPublicKeyInfo_algorithm'.algorithm, 
    {Algorithm, PublicKey, PublicKeyParams}.

trusted_cert_and_path(CertChain, CertDbRef) ->
    [Cert = {_, ErlCert} | RestPath] = lists:reverse(CertChain),
    
    IssuerAnPath = 
	case is_self_signed(ErlCert) of
	    true ->
		{issuer_id(ErlCert, Cert, self), RestPath};
	    false  ->
		{issuer_id(ErlCert, Cert, other), [Cert | RestPath]}
	end,
    
    case IssuerAnPath of
	{{error, issuer_not_found}, _ } ->
	    throw(#alert{level = ?FATAL,
			 description = ?UNKNOWN_CA
			});
	{{SerialNr, Issuer}, Path} ->
	    trusted_cert_and_path(CertDbRef, SerialNr, Issuer, Path)
    end.

trusted_cert_and_path(CertDbRef, SerialNr, Issuer, Path) ->
    case ssl_certificate_db:lookup_trusted_cert(CertDbRef, SerialNr,
						Issuer) of
	{ok, BinCert} ->
	    {ok, NewErlCert} = ssl_pkix:decode_cert(BinCert, [ssl]),
	    {{BinCert, NewErlCert}, Path};
	_ ->
	    throw(#alert{level = ?FATAL,
			 description = ?UNKNOWN_CA
			})
    end.
    
init_validation_state(ErlCert, MaxLen) ->
    PolicyTree = #policy_tree_node{valid_policy = ?anyPolicy,
				   qualifier_set = [],
				   criticality_indicator = false,
				   expected_policy_set = [?anyPolicy]},
    prepare_for_next_cert(ErlCert, 
			  #path_validation_state{max_path_length = MaxLen,
						 valid_policy_tree =
						 PolicyTree,
						 explicit_policy  =
						 MaxLen + 1,
						 inhibit_any_policy =
						 MaxLen + 1,
						 policy_mapping =
						 MaxLen + 1,
						 cert_num = 0}).

prepare_for_next_cert(ErlCert, ValidationState) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate, 
    Issuer =  TBSCert#'TBSCertificate'.subject,
   
    {Algorithm, PublicKey, PublicKeyParams} =
	public_key_info(TBSCert#'TBSCertificate'.subjectPublicKeyInfo,
			ValidationState),
    
    ValidationState#path_validation_state{
      working_public_key_algorithm = Algorithm,
      working_public_key = PublicKey,
      working_public_key_parameters = PublicKeyParams,
      working_issuer_name = Issuer,
      cert_num = ValidationState#path_validation_state.cert_num + 1
     }.
   
validate_time(ErlCert) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate,
    {'Validity', {utcTime, NotBeforeStr}, {utcTime, NotAfterStr}} 
	= TBSCert#'TBSCertificate'.validity,
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    NotBefore = time_str_2_gregorian_sec(NotBeforeStr),
    NotAfter = time_str_2_gregorian_sec(NotAfterStr),

    case ((NotBefore =< Now) and (Now =< NotAfter)) of
	true ->
	    ok;
	false ->
	    throw(#alert{level = ?FATAL,
			 description = ?CERTIFICATE_EXPIRED})
    end.

validate_issuer(ErlCert, 
		#path_validation_state{working_issuer_name = Issuer}) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate,
    case is_issuer(Issuer, TBSCert#'TBSCertificate'.issuer) of
	true ->
	    ok;
	_ ->
	    throw(#alert{level = ?FATAL,
			 description = ?BAD_CERTIFICATE})
    end. 

validate_signature(ErlCert, Cert, #path_validation_state{
		     working_public_key_algorithm = Alg,
		     working_public_key = Key,
		     working_public_key_parameters = KeyParams}) ->
    
    %% Signature is an ASN1 compact bit string 
    {0, Signature} = ErlCert#'Certificate'.signature,
    SigAlgRec = ErlCert#'Certificate'.signatureAlgorithm,
    SigAlg = SigAlgRec#'SignatureAlgorithm'.algorithm,
    ?DBG_TERM(Signature),
    Digest = ssl_cipher:digest(SigAlg, Cert),

    case ssl_cipher:verify_signature(Alg, Digest, Signature,
				     Key, KeyParams) of
	true ->
	    ok;
	false ->
	    throw(#alert{level = ?FATAL, 
			 description = ?BAD_CERTIFICATE})
    end.

validate_names(ErlCert,
	       #path_validation_state{permitted_subtrees = Permit,
				      excluded_subtrees =  Exclude}) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate, 
    Subject = TBSCert#'TBSCertificate'.subject,
    AltSubject = select_extension('id-ce-subjectAltName', 
				  TBSCert#'TBSCertificate'.extensions),
    
    Name = case Subject of
	       [] ->
		   [];
	       _ ->
		   [{directoryName, Subject}]
	   end,
    
    AltNames = case AltSubject of
		   undefined ->
		       [];
		   _ ->
		       AltSubject#'Extension'.extnValue
	       end,
    
    case (is_permitted(Name, Permit) andalso 
	  is_permitted(AltNames, Permit) andalso
	  (not is_excluded(Name, Exclude)) andalso
	  (not is_excluded(AltNames, Exclude))) of
	true ->
	    ok;
	false ->
	    throw(#alert{level = ?FATAL, 
			 description = ?BAD_CERTIFICATE})
    end.

is_not_revoked(_ErlCert) ->
    %% true |
    %% throw(#alert{})
    true.

validate_extensions(ErlCert, ValidationState) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate,
    Extensions = TBSCert#'TBSCertificate'.extensions,
    validate_extensions(Extensions, ValidationState, no_basic_constraint).


normalize_general_name({rdnSequence, Issuer}) ->
    NormIssuer = normalize_general_name(Issuer),
    {rdnSequence, NormIssuer};

normalize_general_name(Issuer) ->
    Normalize = fun([{Description, Type, {printableString, Value}}]) ->
			NewValue = string:to_lower(strip_spaces(Value)),
			{Description, Type, {printableString, NewValue}};
		   (Atter)  ->
			Atter
		end,
    lists:sort(lists:map(Normalize, Issuer)).

certificate_chain(OwnCert, CertsDbRef) ->
    {ok, ErlCert} = ssl_pkix:decode_cert(OwnCert, [ssl]),
    certificate_chain(ErlCert, OwnCert, CertsDbRef, [OwnCert]).


file_to_certificats(File) ->
    {ok, List} = ssl_pem:read_file(File), 
    [Bin || {cert, Bin} <- List].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_chain(ErlCert, Cert, CertsDbRef, Chain) ->
    
    IssuerAndSelfSigned = 
	case is_self_signed(ErlCert) of
	    true ->
		{issuer_id(ErlCert, Cert, self), true};
	    false  ->
		{issuer_id(ErlCert, Cert, other), false}
	end,
    
    case IssuerAndSelfSigned of 
	{{error, issuer_not_found}, _} ->
	    {error, issuer_not_found};
	{{SerialNr, Issuer}, SelfSigned} -> 
	    certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, SelfSigned)
    end.
  
certificate_chain(CertsDbRef, Chain, SerialNr, Issuer, SelfSigned) ->
    case ssl_certificate_db:lookup_trusted_cert(CertsDbRef, SerialNr,
						Issuer) of
	{ok, IssuerCert} ->
	    {ok, ErlCert} = ssl_pkix:decode_cert(IssuerCert, [ssl]),
	    case SelfSigned of
		true ->
		    {ok, lists:reverse(Chain)};
		false ->
		    certificate_chain(ErlCert, IssuerCert, CertsDbRef,
				      [IssuerCert | Chain])
	    end;
	_ ->
	    {error, {issuer_not_found, {SerialNr, Issuer}}}		      
    end.


issuer_id(ErlCert, Cert, other) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate,
    Extensions = TBSCert#'TBSCertificate'.extensions,
    case select_extension('ce-authorityKeyIdentifier', Extensions) of
	undefined ->
	    find_issuer(ErlCert, Cert, no_candidate);
	AuthKeyExt ->
	    cert_auth_key_id(AuthKeyExt#'Extension'.extnValue)
    end;
	
issuer_id(ErlCert, _, self) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate, 
    Issuer = TBSCert#'TBSCertificate'.issuer,
    SerialNr = TBSCert#'TBSCertificate'.serialNumber,
    {SerialNr, normalize_general_name(Issuer)}.  

cert_auth_key_id(#'AuthorityKeyIdentifier'{authorityCertIssuer = 
					   AuthCertIssuer,
					   authorityCertSerialNumber = 
					   SerialNr}) ->
    Issuer = decode_general_name(AuthCertIssuer),
    {SerialNr, Issuer}.

is_self_signed(ErlCert) ->
    TBSCert = ErlCert#'Certificate'.tbsCertificate, 
    is_issuer(TBSCert#'TBSCertificate'.issuer, 
	      TBSCert#'TBSCertificate'.subject).

find_issuer(ErlCert, Cert, PrevCandidateKey) ->
    case ssl_certificate_db:issuer_candidate(PrevCandidateKey) of
	no_more_candidates ->
	    {error, issuer_not_found};
	{Key, BinCandidate} ->
	    {ok, Candidate} = ssl_pkix:decode_cert(BinCandidate, [ssl]),
%%% TODO: case match_subject_and_issuer(Candidate, Cert) ->
%%% så slipper man validera varenda cert för att hitta issuer...
	    ValidationState = init_validation_state(Candidate, 1),
	    try validate_signature(ErlCert, Cert, ValidationState) of
		ok ->
		   issuer_id(Candidate, BinCandidate, self) 	
	    catch
		#alert{} ->
		    find_issuer(ErlCert, Cert, Key)
	    end
    end.

public_key_info(PublicKeyInfo, 
		#path_validation_state{working_public_key_algorithm =
				       WorkingAlgorithm,
				       working_public_key_parameters =
				       WorkingParams}) ->
    PublicKey = PublicKeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
    AlgInfo = PublicKeyInfo#'SubjectPublicKeyInfo'.algorithm,
    
    PublicKeyParams = AlgInfo#'SubjectPublicKeyInfo_algorithm'.parameters,
    Algorithm = AlgInfo#'SubjectPublicKeyInfo_algorithm'.algorithm, 
    
    NewPublicKeyParams =
	case PublicKeyParams of
	    'NULL' when WorkingAlgorithm == Algorithm ->
		WorkingParams;
	    _ ->
		PublicKeyParams
	end,
    {Algorithm, PublicKey, NewPublicKeyParams}.

time_str_2_gregorian_sec([Y1, Y2, M1, M2, D1, D2, H1, H2, M3, M4, 
			  S1, S2, Z]) ->
    case list_to_integer([Y1, Y2]) of
	N when N >= 50 ->
	    time_str_2_gregorian_sec([$1, $9, Y1, Y2, M1, M2, D1, D2, 
				      H1, H2, M3, M4, S1, S2, Z]);
	_ ->
	    time_str_2_gregorian_sec([$2, $0, Y1, Y2, M1, M2, D1, D2, 
				      H1, H2, M3, M4, S1, S2, Z]) 
    end;

time_str_2_gregorian_sec([Y1, Y2, Y3, Y4, M1, M2, D1, D2, H1, H2, M3, M4, 
			  S1, S2, $Z])  ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    calendar:datetime_to_gregorian_seconds({{Year, Month, Day},
					    {Hour, Min, Sec}}).

is_issuer({rdnSequence, Issuer}, {rdnSequence, Candidate}) ->
    AttrFun = fun([{'AttributeTypeAndValue', Type, Value}]) ->
		      {Type, Value}
	      end,
    IssuerAttrs = lists:sort(lists:map(AttrFun, Issuer)),
    CandidateAttrs = lists:sort(lists:map(AttrFun, Candidate)),
    is_dir_name(IssuerAttrs, CandidateAttrs).

is_dir_name([], []) ->
    true;
is_dir_name([{Type, {printableString, Value1}} | Rest1], 
	  [{Type, {printableString, Value2}} | Rest2]) ->
    case  string:to_lower(strip_spaces(Value1)) == 
	string:to_lower(strip_spaces(Value2))  of
	true ->
	    is_dir_name(Rest1, Rest2);
	false ->
	    false
    end;
is_dir_name([{Type, Value} | Rest1], [{Type, Value} | Rest2]) ->
    is_dir_name(Rest1, Rest2);
is_dir_name(_, _) ->
    false.

%% Strip all leading and trailing spaces and make
%% sure there is no double spaces in between. 
strip_spaces(String) ->   
    NewString = 
	lists:foldl(fun(Char, Acc) -> Acc ++ Char ++ " " end, [], 
		    string:tokens(String, " ")),
    string:strip(NewString).

select_extension(_, []) ->
    undefined;
select_extension(Id, [#'Extension'{extnID = Id} = Extension | _]) ->
    Extension;
select_extension(Id, [_ | Extensions]) ->
    select_extension(Id, Extensions).

%% No extensions present
validate_extensions(asn1_NOVALUE, ValidationState, ExistBasicCon) ->
    validate_extensions([], ValidationState, ExistBasicCon);

validate_extensions([], ValidationState, basic_constraint) ->
    ValidationState;
validate_extensions([], ValidationState = 
		    #path_validation_state{max_path_length = Len}, 
		    no_basic_constraint) ->
    ValidationState#path_validation_state{max_path_length = Len - 1};

validate_extensions([#'Extension'{extnID = 'ce-basicConstraints',
				  extnValue = 
				  #'BasicConstraints'{cA = true,
						      pathLenConstraint = N}} |
		     Rest],
		    ValidationState = 
		    #path_validation_state{max_path_length = Len}, _) ->
    validate_extensions(Rest,
			ValidationState#path_validation_state{max_path_length 
							      = min(N, 
								    Len - 1)},
			basic_constraint);
%% The pathLenConstraint field is meaningful only if cA is set to
%% TRUE.
validate_extensions([#'Extension'{extnID = 'ce-basicConstraints',
				  extnValue = 
				  #'BasicConstraints'{cA = false}} |
		     Rest], ValidationState, ExistBasicCon) ->
    validate_extensions(Rest, ValidationState, ExistBasicCon);
  
%% 
validate_extensions([#'Extension'{extnID = 'ce-keyUsage',
				  extnValue = KeyUse,
				  critical = true} | Rest], 
		    #path_validation_state{} = ValidationState, 
		    ExistBasicCon) ->
    case is_valid_key_usage(KeyUse, keyCertSign) of
	true ->
	    validate_extensions(Rest, ValidationState, ExistBasicCon);
	false ->
	    throw(#alert{level = ?FATAL,
			 description = ?BAD_CERTIFICATE})
    end;

validate_extensions([#'Extension'{extnID = 'ce-extKeyUsage',
				  extnValue = KeyUse,
				  critical = true} | Rest], 
		    #path_validation_state{} = ValidationState,
		    ExistBasicCon) ->
    case is_valid_extkey_usage(KeyUse) of
	true ->
	    validate_extensions(Rest, ValidationState, ExistBasicCon);
	false ->
	    throw(#alert{level = ?FATAL,
			 description = ?BAD_CERTIFICATE})
    end;

validate_extensions([#'Extension'{extnID = 'ce-subjectAltName',
				  extnValue = Names} | Rest], 
		    ValidationState, ExistBasicCon)  ->    
    case validate_subject_alt_names(Names) of
	true when Names =/= [] ->
	    validate_extensions(Rest, ValidationState, ExistBasicCon);
	_ ->
	    throw(#alert{level = ?FATAL,
			 description = ?BAD_CERTIFICATE})	
    end;

%% This extension SHOULD NOT be marked critical. Its value
%% does note have to be further validated at this point.
validate_extensions([#'Extension'{extnID = 'ce-issuerAltName', 
				  extnValue = _} | Rest], 
		    ValidationState, ExistBasicCon) ->
    validate_extensions(Rest, ValidationState, ExistBasicCon);

%% This extension MUST NOT be marked critical.Its value
%% does note have to be further validated at this point.
validate_extensions([#'Extension'{extnID = Id,
				  extnValue = _,
				  critical = false} | Rest], 
		    ValidationState, 
		    ExistBasicCon) when Id == 'ce-subjectKeyIdentifier'; 
                                        Id == 'ce-authorityKeyIdentifier' ->
    validate_extensions(Rest, ValidationState, ExistBasicCon);

validate_extensions([#'Extension'{extnID = 'ce-nameConstraints',
				  extnValue = NameConst,
				  critical = false} | Rest], 
		    ValidationState, 
		    ExistBasicCon) ->
    Permitted = NameConst#'NameConstraints'.permittedSubtrees, 
    Excluded = NameConst#'NameConstraints'.excludedSubtrees,
    
    NewValidationState = add_name_constraints(Permitted, Excluded, 
					      ValidationState),
    
    validate_extensions(Rest, NewValidationState, ExistBasicCon);


validate_extensions([#'Extension'{extnID = 'ce-certificatePolicies',
				  critical = true} | _], _, _) ->
    %% TODO: Remove this clause when policy handling is
    %% fully implemented
    throw(#alert{level = ?FATAL,
		 description = ?UNSUPPORTED_CERTIFICATE});

validate_extensions([#'Extension'{extnID = 'ce-certificatePolicies',
				  extnValue = #'PolicyInformation'{
				    policyIdentifier = Id,
				    policyQualifiers = Qualifier}} 
		     | Rest], #path_validation_state{valid_policy_tree = Tree}
		    = ValidationState,
		    ExistBasicCon) ->

    %% TODO: Policy imp incompleat
    NewTree = process_policy_tree(Id, Qualifier, Tree),
    
    validate_extensions(Rest, 
			ValidationState#path_validation_state{
			  valid_policy_tree = NewTree}, 
			ExistBasicCon);

validate_extensions([#'Extension'{extnID = 'ce-PolicyConstraints',
				  critical = true} | _], _, _) ->
    %% TODO: Remove this clause when policy handling is
    %% fully implemented
    throw(#alert{level = ?FATAL,
		 description = ?UNSUPPORTED_CERTIFICATE});

validate_extensions([#'Extension'{extnID = 'ce-PolicyConstraints',
				  extnValue = #'PolicyConstraints'{
				    requireExplicitPolicy = ExpPolicy,
				    inhibitPolicyMapping = MapPolicy}} 
		     | Rest], ValidationState, ExistBasicCon) ->
    
    %% TODO: Policy imp incompleat
    NewValidationState = add_policy_constraints(ExpPolicy, MapPolicy, 
						ValidationState),

    validate_extensions(Rest, NewValidationState, ExistBasicCon);

%% Skip unknown non critical extensions
validate_extensions([#'Extension'{critical = false} | Rest], ValidationState,
		    ExistBasicCon) ->
    validate_extensions(Rest, ValidationState, ExistBasicCon);

%% Unsupported critical extension found
validate_extensions([#'Extension'{critical = true} | _], _, _) ->
    throw(#alert{level = ?FATAL,
		 description = ?UNSUPPORTED_CERTIFICATE}).

is_valid_key_usage(KeyUse, Use) ->
    lists:member(Use, KeyUse).

is_valid_extkey_usage('id-kp-clientAuth') ->
    true;
is_valid_extkey_usage('id-kp-serverAuth') ->
    true;
is_valid_extkey_usage(_) ->
    false.

validate_subject_alt_names([]) ->
    true;
validate_subject_alt_names([AltName | Rest]) ->
    case is_valid_subject_alt_name(AltName) of
	true ->
	    validate_subject_alt_names(Rest);
	false ->
	    false
    end.

is_valid_subject_alt_name({Name, Value}) when Name == rfc822Name;
					      Name == dNSName ->    
    case Value of
	"" ->
	    false;
	_  ->
	    true	   
    end;

is_valid_subject_alt_name({iPAdress, Addr}) ->
    case length(Addr) of
        4 ->  %ipv4
	    true;
	16 -> %ipv6
	    true;
	_ ->
	    false
    end;
is_valid_subject_alt_name({uniformResourceIdentifier, URI}) ->
    is_valid_uri(URI);

is_valid_subject_alt_name({_, [_|_]}) ->
    true;
is_valid_subject_alt_name({_, _}) ->
    false.

decode_general_name([{directoryName, Issuer}]) ->
    normalize_general_name(Issuer).

min(N, M) when N =< M ->
    N;
min(_, M) ->
    M.

is_ip_address(Address) ->
    case inet_parse:address(Address) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

is_fully_qualified_name(Name) ->
    lists:last(Name) == $..

is_valid_uri(AbsURI) -> 
    case split_uri(AbsURI) of
	incompleat ->
	    false;
	{StrScheme, _, Host, _, _} ->
	    case list_to_atom(string:to_lower(StrScheme)) of
		Scheme when Scheme == http; Scheme == ftp ->
		    is_valid_host(Host);
		_ ->
		    false
	    end
    end.

is_valid_host(Host) ->
    case is_ip_address(Host) of
	true ->
	    true;   
	false -> 
	    is_fully_qualified_name(Host)
    end.

%% Could have a more general split URI in stdlib? Maybe when
%% regexs are improved. Needed also in inets!
split_uri(Uri) ->
    case split_uri(Uri, ":", {error, no_scheme}, 1, 1) of
	{error, no_scheme} ->
	    incompleat;
	{StrScheme, "//" ++ URIPart} ->
	    {Authority, PathQuery} = 
		split_auth_path(URIPart),
	    {UserInfo, HostPort} = 
		split_uri(Authority, "@", {"", Authority}, 1, 1),
	    {Host, Port} = 
		split_uri(HostPort, ":", {HostPort, dummy_port}, 1, 1),
	    {StrScheme, UserInfo, Host, Port, PathQuery}
    end.

split_auth_path(URIPart) ->
    case split_uri(URIPart, "/", URIPart, 1, 0) of
	Split = {_, _} ->
	    Split;
	URIPart ->
	    case split_uri(URIPart, "\\?", URIPart, 1, 0) of
		Split = {_, _} ->
		    Split;
		URIPart ->
		    {URIPart,""}
	    end
    end.

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case regexp:first_match(UriPart, SplitChar) of
	{match, Match, _} ->
	    {string:substr(UriPart, 1, Match - SkipLeft),
	     string:substr(UriPart, Match + SkipRight, length(UriPart))}; 
	nomatch ->
	    NoMatchResult
    end.

is_permitted(_, no_constraints) ->
    true;
is_permitted(Names, Constraints) ->
    is_valid_name(Names, Constraints).

is_excluded(_, []) ->
    false;
is_excluded(Names, Constraints) ->
    is_valid_name(Names, Constraints).

is_valid_name([], _) ->
    true;
is_valid_name([{Type, Name} | Rest], Constraints) ->
    case type_subtree_names(Type, Constraints) of
	[_|_] = ConstraintNames ->
	    is_valid_name(Type, Name, ConstraintNames, Rest, Constraints);
	[] ->
	    is_valid_name(Rest, Constraints)
    end.

is_valid_name(Type, Name, ConstraintNames, Names, Constraints) ->
    case match_name(Type, Name, ConstraintNames) of
	true ->
	    is_valid_name(Names, Constraints);
	false ->
	    false
    end.

add_name_constraints(NewPermittedTrees, NewExcludedTrees, 
		     #path_validation_state{
				       permitted_subtrees = PermittedTrees,
					  excluded_subtrees = ExcludedTrees} =
		     ValidationState) ->
    NewPermitted = subtree_intersection(NewPermittedTrees, PermittedTrees, []),
    NewExcluded = subtree_union(NewExcludedTrees, ExcludedTrees),   
    ValidationState#path_validation_state{permitted_subtrees = NewPermitted,
					  excluded_subtrees = NewExcluded}.
subtree_union(asn1_NOVALUE, Trees) ->
    Trees;
subtree_union(Trees1, Trees2) ->
    Trees1 ++ Trees2.

subtree_intersection(asn1_NOVALUE, Trees, _) ->
    Trees;
subtree_intersection([], _, TreesInt) ->
    TreesInt;
subtree_intersection([Tree | Trees1], Trees2, Trees3) ->
    
    case is_in_intersection(Tree, Trees2) of
	true ->
	   subtree_intersection(Trees1, Trees2, [Tree | Trees3]);
	false ->
	    subtree_intersection(Trees1, Trees2, Trees3)
    end.

is_in_intersection(#'GeneralSubtree'{base = {directoryName, Name1}} 
		   = Name, 
		   [#'GeneralSubtree'{base = {directoryName, Name2}} 
		    | IntCandidates]) ->
    case is_dir_name(Name1, Name2) of
	true ->
	    true;
	false ->
	    is_in_intersection(Name, IntCandidates)
    end;
is_in_intersection(#'GeneralSubtree'{base = {ipAdress, Ip}}, 
		   [#'GeneralSubtree'{base = {ipAdress, Ip}} | _]) ->	     
    true;
is_in_intersection(#'GeneralSubtree'{base = {x400Address, OrAddr1}} = Addr, 
		   [#'GeneralSubtree'{base = {x400Address, OrAddr2}} 
		    | IntCandidates]) ->	
    case is_or_address(OrAddr1, OrAddr2) of
	true ->
	    true;
	false ->
	    is_in_intersection(Addr, IntCandidates)
    end;

is_in_intersection(#'GeneralSubtree'{base = {Type, Name1}} = Name, 
		   [#'GeneralSubtree'{base = {Type, Name2}} 
		    | IntCandidates]) ->	
    case case_insensitive_match(Name1, Name2) of
	true ->
	    true;
	false ->
	    is_in_intersection(Name, IntCandidates)
    end;
is_in_intersection(_, []) ->
    false;
is_in_intersection(Name, [_ | IntCandidates]) ->
    is_in_intersection(Name, IntCandidates).

type_subtree_names(Type, SubTrees) ->
    [Name || #'GeneralSubtree'{base = {TreeType, Name}} <- SubTrees,
	     TreeType == Type].

match_name(rfc822Name, Name, [PermittedName | Rest]) ->
    match_name(fun is_valid_host_or_domain/2, Name, PermittedName, Rest);
	
match_name(directoryName, DirName,  [PermittedName | Rest]) ->
    match_name(fun is_dir_name/2, DirName, PermittedName, Rest);

match_name(uniformResourceIdentifier, URI,  [PermittedName | Rest]) ->
    case split_uri(URI) of
	incompleat ->
	    false;
	{_, _, Host, _, _} ->
	    match_name(fun is_valid_host_or_domain/2, Host, 
		       PermittedName, Rest)
    end;

match_name(emailAddress, Name, [PermittedName | Rest]) ->
    Fun = fun(Email, PermittedEmail) -> 
		  is_valid_email_address(Email, PermittedEmail,
				   string:tokens(PermittedEmail,"@"))
	  end,
     match_name(Fun, Name, PermittedName, Rest);

match_name(dNSName, Name, [PermittedName | Rest]) ->
    Fun = fun(Name1, Name2) ->
		  lists:suffix(string:to_lower(Name2), 
			       string:to_lower(Name1))
	  end,
    match_name(Fun, Name, "." ++ PermittedName, Rest);	  
	     
match_name(x400Address, OrAddress, [PermittedAddr | Rest]) ->
    match_name(fun is_or_address/2, OrAddress, PermittedAddr, Rest);

match_name(ipAdress, IP, [PermittedIP | Rest]) ->
    Fun = fun([IP1, IP2, IP3, IP4],
	      [IP5, IP6, IP7, IP8, M1, M2, M3, M4]) ->
		  is_permitted_ip([IP1, IP2, IP3, IP4],
				  [IP5, IP6, IP7, IP8],
				  [M1, M2, M3, M4]);
	     ([IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,
	       IP9, IP10, IP11, IP12, IP13, IP14, IP15, IP16],
	      [IP17, IP18, IP19, IP20, IP21, IP22, IP23, IP24,
	       IP25, IP26, IP27, IP28, IP29, IP30, IP31, IP32,
	       M1, M2, M3, M4, M5, M6, M7, M8,
	       M9, M10, M11, M12, M13, M14, M15, M16]) ->
		  is_permitted_ip([IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,
				   IP9, IP10, IP11, IP12, IP13,
				   IP14, IP15, IP16],
				  [IP17, IP18, IP19, IP20, IP21, IP22, IP23, 
				   IP24,IP25, IP26, IP27, IP28, IP29, IP30, 
				   IP31, IP32],
				    [M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, 
				     M11, M12, M13, M14, M15, M16]);
	     (_,_) ->
		  false
	  end,
    match_name(Fun, IP, PermittedIP, Rest).
    
match_name(Fun, Name, PermittedName, []) ->
    Fun(Name, PermittedName);
match_name(Fun, Name, PermittedName, [Head | Tail]) ->
    case Fun(Name, PermittedName) of
	true ->
	    true;
	false ->
	    match_name(Fun, Name, Head, Tail)
    end.

is_permitted_ip([], [], []) ->
    true;
is_permitted_ip([CandidatIp | CandidatIpRest], 
		[PermittedIp | PermittedIpRest], [Mask | MaskRest] ) -> 
    case mask_cmp(CandidatIp, PermittedIp, Mask) of
	true ->
	    is_permitted_ip(CandidatIpRest, PermittedIpRest, MaskRest);
	false ->
	    false
    end.

mask_cmp(Canditate, Permitted, Mask) ->
    (Canditate band Mask) == Permitted.

is_valid_host_or_domain(Canditate, "$." ++ _ = Permitted) ->
    is_suffix(Canditate, Permitted);	     
is_valid_host_or_domain(Canditate, Permitted) ->
    case_insensitive_match(Canditate, Permitted).

is_valid_email_address(Canditate, "$." ++ Permitted, [_]) ->
    is_suffix(Canditate, Permitted);

is_valid_email_address(Canditate, PermittedHost, [_]) ->
    [_ | CanditateHost] = string:tokens(Canditate,"@"),
    case_insensitive_match(CanditateHost, PermittedHost);

is_valid_email_address(Canditate, Permitted, [_, _]) ->
    case_insensitive_match(Canditate, Permitted).

is_suffix(Str, Suffix) ->
    lists:suffix(string:to_lower(Str), 
		 string:to_lower(Suffix)).
case_insensitive_match(Str1, Str2) ->
    string:to_lower(Str1) == string:to_lower(Str2).

is_or_address(Address, Canditate) ->
    %% TODO: Is case_insensitive_match sufficient?
    %% study rfc2156 probably need more a complex check.
    is_double_quoted(Address) andalso 
	is_double_quoted(Canditate) andalso 
	case_insensitive_match(Address, Canditate).
    
is_double_quoted(["\"" | Tail]) ->
    is_double_quote(lists:last(Tail));
is_double_quoted("%22" ++ Tail) ->
    case lists:reverse(Tail) of
	[A, B, C | _] ->
	    is_double_quote([C, B, A]);
	_ ->
	    false
    end;

is_double_quoted(_) ->
    false.

is_double_quote("%22") ->
    true;
is_double_quote("\"") ->
    true;
is_double_quote(_) ->
    false.

add_policy_constraints(ExpPolicy, MapPolicy, 
		       #path_validation_state{cert_num = CertNum,
					      explicit_policy = CurExpPolicy,
					      policy_mapping = CurMapPolicy} = 
		       ValidationState) ->
    
    NewExpPolicy = policy_constraint(CurExpPolicy, ExpPolicy, CertNum),
    NewMapPolicy = policy_constraint(CurMapPolicy, MapPolicy, CertNum),

    ValidationState#path_validation_state{explicit_policy = NewExpPolicy,
					  policy_mapping = NewMapPolicy}.

policy_constraint(Current, asn1_NOVALUE, _) ->
    Current;
policy_constraint(Current, New, CertNum) ->
    min(Current, New + CertNum).

process_policy_tree(_,_, ?NULL) ->
    ?NULL;
process_policy_tree(_Id, _Qualifier, Tree) ->
    %% TODO real imp.
    Tree.

