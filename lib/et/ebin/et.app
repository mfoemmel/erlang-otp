{application, et,
 [{description, "Event Tracer"},
  {vsn, "1.3"},
  {modules,
   [	
	et,
	et_collector,
	et_contents_viewer,
	et_selector,
	et_viewer
       ]},
  {registered, [et_collector]},
  {applications, [stdlib, kernel]},
  {env, []},
  {mod, {et_sup, []}}
 ]}.


