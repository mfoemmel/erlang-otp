%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_sdi.hrl,v 1.1 2004/04/13 00:08:19 mikpe Exp $

-record(sdi_info,
	{lb,		% span lower bound for short form
	 ub,		% span upper bound for short form
	 incr}).	% instruction size increase for long form
