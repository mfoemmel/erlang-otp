%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-record(sdi_info,
	{lb,		% span lower bound for short form
	 ub,		% span upper bound for short form
	 incr}).	% instruction size increase for long form
