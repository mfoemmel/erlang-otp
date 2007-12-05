%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-record(sdi_info,
	{lb   :: integer(),	% span lower bound for short form
	 ub   :: integer(),	% span upper bound for short form
	 incr :: byte()}).	% instruction size increase for long form
