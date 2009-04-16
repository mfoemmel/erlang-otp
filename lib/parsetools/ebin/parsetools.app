{application, parsetools,
 [{description, "XLATETOOLS  CXC 138 xx"},
  {vsn, "1.4.7"},
  {modules, [yecc,
	     yeccparser,
	     yeccscan
	    ]
  },
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, [{file_util_search_methods,[{"", ""}, {"ebin", "esrc"}, {"ebin", "src"}]}
	]
  }
 ]
}. 
 
















