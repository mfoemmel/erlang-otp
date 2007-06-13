{application, observer,
   [{description, "OBSERVER version 1"},
    {vsn, "0.9.7.4"},
    {modules, [crashdump_viewer,
	       crashdump_viewer_html,
	       crashdump_translate,
	       etop,
	       etop_gui,
	       etop_tr,
	       etop_txt,
	       ttb,
	       ttb_et]},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, []}]}.


