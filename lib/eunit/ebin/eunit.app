% This is an -*- erlang -*- file.

{application, eunit,
 [{description, "EUnit"},
  {vsn, "2.0"},
  {modules, [eunit,
	     eunit_autoexport,
	     eunit_striptests,
	     eunit_server,
	     eunit_proc,
	     eunit_serial,
	     eunit_test,
	     eunit_lib,
	     eunit_data,
	     eunit_tty]},
  {registered,[]},
  {applications, [stdlib]},
  {env, []}]}.
