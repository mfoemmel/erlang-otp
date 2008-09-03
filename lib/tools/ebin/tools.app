{application, tools,
 [{description, "DEVTOOLS  CXC 138 16"},
  {vsn, "2.6.2"},
  {modules, [cover,
	     cover_web,
	     eprof,
	     fprof,
	     instrument,
	     make,
	     xref,
	     xref_base,
	     xref_compiler,
	     xref_parser,
	     xref_reader,
	     xref_scanner,
	     xref_utils
	    ]
  },
  {registered,[webcover_server]},
  {applications, [kernel, stdlib]},
  {env, [{file_util_search_methods,[{"", ""}, {"ebin", "esrc"}, {"ebin", "src"}]}
	]
  }
 ]
}. 
 
















