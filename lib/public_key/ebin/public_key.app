{application, public_key,
  [{description, "Public key infrastructure"},
   {vsn, "0.3"},
   {modules, [
	          public_key,
		  pubkey_pem, 
		  pubkey_crypto,
		  pubkey_cert,
		  pubkey_cert_records,
		  'OTP-PUB-KEY'
            ]},
   {applications, [crypto, kernel, stdlib]},
   {registered, []},
   {env, []}  
   ]
}.