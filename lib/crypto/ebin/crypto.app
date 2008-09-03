{application, crypto,
   [{description, "CRYPTO version 1"},
    {vsn, "1.5.2.1"},
    {modules, [crypto,
	       crypto_app,
	       crypto_sup,
	       crypto_server]},
    {registered, [crypto_sup, crypto_server]},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {crypto_app, []}}]}.


