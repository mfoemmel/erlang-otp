% This is an -*- erlang -*- file.

{application, typer,
 [{description, "TYPe annotator for ERlang programs, version 0.1.0.1"},
  {vsn, "0.1.0.1"},
  {modules, [typer,
	     typer_generator,
	     typer_info,
	     typer_map,
	     typer_options,
	     typer_parse,
	     typer_preprocess]},
  {registered, []},
  {applications, [compiler, dialyzer, hipe, kernel, stdlib]},
  {env, []}]}.
