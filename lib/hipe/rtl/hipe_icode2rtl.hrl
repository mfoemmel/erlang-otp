
%%-------------------------------------------------------------------------
%% Check whether this pass should add gc-test or whether it is done.
%%
-define(AddGC(Opts),proplists:get_bool(rtl_add_gc,Opts)).
-define(DoAddGC(Opts), [rtl_add_gc,Opts]).

%% ----------------------------------------------------------------------
%%
%% DEBUG level >1 transforms exit values 
%%    from  exit_value
%%    to    {'mod:fun/arity', exit_value}
%%  This is a non-standard exception which shows from where the 
%%   exception originated. 
%%
%% DEBUG level >2 prints the translation
%-ifndef(DEBUG).
%-define(DEBUG,3).
%-endif.
%%-define(DO_ASSERT,true).
