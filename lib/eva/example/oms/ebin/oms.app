{application, oms,
   [{description, "OMS TEST"},
    {vsn, "1.0"},
    {modules, [oms]},
    {registered, [oms_sup]},
    {applications, [kernel, stdlib, sasl, mnesia]},
    {included_applications, [eva]},
    {env, []},
    {mod, {oms, []}}]}.

