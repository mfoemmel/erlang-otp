{application,inviso,
   [{description,  "INVISO trace tool"},
    {vsn,          "0.6"},
    {modules,      [inviso_c,inviso,
		    inviso_lfm,inviso_lfm_tpfreader
		   ]},
    {registered,   [inviso_c]},
    {applications, [kernel, stdlib, runtime_tools]},
    {env,          []}
    ]}.



