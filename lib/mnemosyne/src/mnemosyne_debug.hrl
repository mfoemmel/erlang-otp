%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-define(debugwrite(F,A), io:format("~s:~w: " F, [?FILE,?LINE|A])).

-ifdef(debug).
  -define(debugmsg(Level, F, A), 
      if  Level=<?debug, integer(?debug) -> ?debugwrite(F,A);
	  true -> ok
      end).
  -define(debugformat(Level, F, A), 
      if  Level=<?debug, integer(?debug)  -> io:format(F,A);
	  true -> ok
      end).
  -define(debug_do(Level, Do),
      if  Level=<?debug, integer(?debug)  -> Do;
	  true -> ok
      end).
  -define(debug_receive(Level),
		case
		    receive
			__MsG__ -> ?debugmsg(Level, "---> ~w\n", [__MsG__]),
				   __MsG__
		    end of	
		   ).	
-else.
  -define(debugmsg(Level, F, A), ok).
  -define(debugformat(Level, F, A), ok).
  -define(debug_do(Level, Do), ok).
  -define(debug_receive(Level), receive).
-endif.

-ifndef(no_not_yet).
   -define(not_yet(Fn), ?debugwrite("Not yet implemented: ~w\n", [Fn])).
-else.
   -define(not_yet(Fn), ok).
-endif.


