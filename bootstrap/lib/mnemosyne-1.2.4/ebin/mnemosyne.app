{application, mnemosyne,
 [{description, "MNEMOSYNE  CXC 138 12"},
  {vsn, "1.2.4"},
  {modules, [
	     mnemosyne, 
	     mnemosyne_catalog, 
	     mnemosyne_compiler, 
	     mnemosyne_constraint, 
	     mnemosyne_cost, 
	     mnemosyne_exec, 
	     mnemosyne_lc, 
	     mnemosyne_lib, 
	     mnemosyne_op, 
	     mnemosyne_optimizer, 
	     mnemosyne_pp, 
	     mnemosyne_slg,
	     mnemosyne_sup,
	     mnemosyne_transform, 
	     mnemosyne_unify
            ]},
  {registered, [
		mnemosyne_catalog,
		mnemosyne_sup
	       ]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {mnemosyne_sup, []}}]}.


