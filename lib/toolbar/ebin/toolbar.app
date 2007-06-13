{application, toolbar,
 [{description, "Toolbar"},
  {vsn, "1.3.0.1"},
  {modules, [
	     toolbar,
	     toolbar_graphics,
	     toolbar_lib,
	     toolbar_toolconfig,
	     canvasbutton
	    ]},
  {registered,[toolbar]},
  {applications, [kernel, stdlib, gs]}]}.
