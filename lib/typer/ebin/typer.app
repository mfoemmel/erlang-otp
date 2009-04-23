% This is an -*- erlang -*- file.

{application, typer,
 [{description, "TYPe annotator for ERlang programs, version 0.1.7"},
  {vsn, "0.1.7"},
  {modules, [typer,
	     typer_annotator,
	     typer_info,
	     typer_map,
	     typer_options,
	     typer_preprocess]},
  {registered, []},
  {applications, [compiler, dialyzer, hipe, kernel, stdlib]},
  {env, []}]}.
