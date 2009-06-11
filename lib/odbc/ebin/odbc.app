{application, odbc,
 [{description, "Erlang ODBC application"},
  {vsn, "2.10.5"},
  {modules, [
	     odbc,
	     odbc_app,
	     odbc_sup
            ]},
  {registered, [
		odbc_sup
	       ]},
  {applications, [kernel, stdlib]},
  {env,[]},
  {mod, {odbc_app, []}}]}.

