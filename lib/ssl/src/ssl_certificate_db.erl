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
%% Purpose: Storage for trused certificats 
%%----------------------------------------------------------------------

-module(ssl_certificate_db).

-include("ssl_pkix.hrl").

-export([create/0, remove/1, add_trusted_certs/3, 
	 remove_trusted_certs/2, lookup_trusted_cert/3, issuer_candidate/1]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: create() -> Db
%% Db = term() - Reference to the crated database 
%% 
%% Description: Creates a new certificate db.
%% Note: lookup_trusted_cert/3 may be called from any process but only
%% the process that called create may call the other functions.
%%--------------------------------------------------------------------
create() ->
    [ets:new(certificate_db_name(), [named_table, set, protected]),
     ets:new(ssl_file_to_ref, [set, private]),
     ets:new(ssl_pid_to_file, [set, private])]. 

%%--------------------------------------------------------------------
%% Function: delete(Db) -> _
%% Db = Database refererence as returned by create/0
%%
%% Description: Removes database db  
%%--------------------------------------------------------------------
remove(Dbs) ->
    lists:foreach(fun(Db) -> true = ets:delete(Db) end, Dbs).

%%--------------------------------------------------------------------
%% Function: lookup_trusted_cert(Ref, SerialNumber, Issuer) -> BinCert
%% Ref = ref()
%% SerialNumber = integer()
%% Issuer = {rdnSequence, IssuerAttrs}
%% BinCert = binary()
%%
%% Description: Retrives the trusted certificate identified by 
%% <SerialNumber, Issuer>. Ref is used as it is specified  
%% for each connection which certificates are trusted.
%%--------------------------------------------------------------------
lookup_trusted_cert(Ref, SerialNumber, Issuer) ->
    case lookup({Ref, SerialNumber, Issuer}, certificate_db_name()) of
	undefined ->
	    undefined;
	BinCert ->
	    {ok, BinCert}
    end.

%%--------------------------------------------------------------------
%% Function: add_trusted_certs(Pid, File, Db) -> {ok, Ref}
%% Pid = pid() 
%% File = string()
%% Db = Database refererence as returned by create/0
%% Ref = ref()
%%
%% Description: Adds the trusted certificates from file <File> to the
%% runtime database. Returns Ref that should be handed to lookup_trusted_cert
%% together with the cert serialnumber and issuer.
%%--------------------------------------------------------------------
add_trusted_certs(Pid, File, [CertsDb, FileToRefDb, PidToFileDb]) ->
    {Ref, Counter} = case lookup(File, FileToRefDb) of
			undefined ->
			    NewRef = make_ref(),
			    add_certs_from_file(File, NewRef, CertsDb),
			    {NewRef, 0};
			{OldRef, OldCounter} ->
			    {OldRef, OldCounter}
		    end,
    insert(Pid, File, PidToFileDb),
    insert(File, {Ref, Counter + 1}, FileToRefDb),
    {ok, Ref}.

%%--------------------------------------------------------------------
%% Function: remove_trusted_certs(Pid, Db) -> _ 
%%
%% Description: Removes trusted certs originating from 
%% the file associated to Pid from the runtime database.  
%%--------------------------------------------------------------------
remove_trusted_certs(Pid, [CertsDb, FileToRef, PidToFileDb]) ->
    File = lookup(Pid, PidToFileDb),
    delete(Pid, PidToFileDb),
    case lookup(File, FileToRef) of
	{Ref, 1} ->
	    remove_certs(Ref, CertsDb),
	    delete(File, FileToRef);
	{Ref, Counter} ->
	    insert(File, {Ref, Counter - 1}, FileToRef)
    end.

%%--------------------------------------------------------------------
%% Function: issuer_candidate() -> {Key, Candidate} | no_more_candidates   
%%
%%     Candidate
%%     
%%     
%% Description: If a certificat does note define its issuer through
%%              the extension 'ce-authorityKeyIdentifier' we can
%%              try to find the issuer in the database over knows
%%              certificates.
%%--------------------------------------------------------------------
issuer_candidate(no_candidate) ->
    Db = certificate_db_name(),
    case ets:first(Db) of
 	'$end_of_table' ->
 	    no_more_candidates;
 	Key ->
 	    {Key, lookup(Key, Db)}
    end;

issuer_candidate(PrevCandidateKey) ->	    
    Db = certificate_db_name(),
    case ets:next(Db, PrevCandidateKey) of
 	'$end_of_table' ->
 	    no_more_candidates;
 	Key ->
 	    {Key, lookup(Key, Db)}
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
certificate_db_name() ->
    ssl_otp_certificate_db.

insert(Key, Data, Db) ->
    true = ets:insert(Db, {Key, Data}).

delete(Key, Db) ->
    true = ets:delete(Db, Key).

lookup(Key, Db) ->
   case ets:lookup(Db, Key) of
       [{Key, Data}] ->
	   Data;
       [] ->
	   undefined
   end.

remove_certs(Ref, CertsDb) ->
    ets:match_delete(CertsDb, {'_', Ref, '_'}).

add_certs_from_file(File, Ref, CertsDb) ->   
    Decode = fun(Cert) ->
		     {ok, ErlCert} = ssl_pkix:decode_cert(Cert, [ssl]),
		     {Cert, ErlCert}
	     end,

    Certs =  case ssl_pkix:decode_cert_file(File, [pem]) of
		 {ok, Bin} when is_binary(Bin) ->
		     [Decode(Bin)];
		 {ok, List} when is_list(List) ->
		     lists:map(Decode, List)
	     end,
    
    SaveCert = 
	fun({Cert, ErlCert}) ->
		TBSCertificate = ErlCert#'Certificate'.tbsCertificate,
		SerialNumber = TBSCertificate#'TBSCertificate'.serialNumber,
		Issuer = 
		    ssl_certificate:normalize_general_name(
		      TBSCertificate#'TBSCertificate'.issuer),
		insert({Ref, SerialNumber, Issuer}, Cert, CertsDb)
	end,
    
    lists:foreach(SaveCert, Certs).
