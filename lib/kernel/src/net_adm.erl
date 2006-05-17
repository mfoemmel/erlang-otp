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
-module(net_adm).
-export([host_file/0,
	 localhost/0,
	 names/0, names/1,
	 ping_list/1,
	 world/0,world/1,
	 world_list/1, world_list/2,
	 dns_hostname/1,
	 ping/1]).

%% Try to read .hosts.erlang file in 
%% 1. cwd , 2. $HOME 3. init:root_dir() 

host_file() ->
    Home = case init:get_argument(home) of
	       {ok, [[H]]} -> [H];
	       _ -> []
	   end,
    case file:path_consult(["."] ++ Home ++ [code:root_dir()], ".hosts.erlang") of
	{ok, Hosts, _} -> Hosts;
	Error -> Error
    end.
	
%%% Check whether a node is up or down
%%% side effect: set up a connection to Node if there 
%%  not yet is one.

ping(Node) when is_atom(Node) ->
    case catch gen:call({net_kernel, Node},
			'$gen_call',
			{is_auth, node()},
			infinity) of
	{ok, yes} -> pong;
	_ ->
	    erlang:disconnect_node(Node),
	    pang
    end.

localhost() ->
    {ok, Host} = inet:gethostname(),
    case inet_db:res_option(domain) of
	"" -> Host;
	Domain -> Host ++ "." ++ Domain
    end.


names() ->
    names(localhost()).

names(Hostname) ->
    case inet:gethostbyname(Hostname) of
	{ok,{hostent, _Name, _ , _Af, _Size, [Addr | _]}} ->
	    erl_epmd:names(Addr);
	Else ->
	    Else
    end.

dns_hostname(Hostname) ->
    case inet:gethostbyname(Hostname) of
	{ok,{hostent, Name, _ , _Af, _Size, _Addr}} ->
	    {ok, Name};
	_ ->
	    {error, Hostname}
    end.

%% a common situation in "life" is to have a configuration file with a list
%% of nodes, and then at startup, all nodes in the list are ping'ed
%% this can lead to no end of troubles if two disconnected nodes
%% simultaneously ping each other.
%% Use this function in order to do it safe,
%% It assumes a working global.erl which ensures a fully 
%% connected network.
%% Had the erlang runtime system been able to fully cope with 
%% the possibility of two simultaneous (unix) connects, this function would
%% merley  be lists:map({net_adm, ping}, [], Nodelist).
%% It is also assumed, that the same (identical) Nodelist is given to all
%% nodes which are to perform this call. (possibly simultaneously)
%% Even this code has a flaw, and that is the case where two
%% nodes simultaneously and without *any* other already 
%% running nodes execute this code. :-(


ping_list(Nodelist) ->
    net_kernel:monitor_nodes(true),
    Sofar = ping_first(Nodelist, nodes()),
    collect_new(Sofar, Nodelist).

ping_first([], _S) ->
	[];

ping_first([Node|Nodes], S) ->
    case lists:member(Node, S) of
	true -> [Node | ping_first(Nodes, S)];
	false ->
	    case ping(Node) of
		pong -> [Node];
		pang -> ping_first(Nodes, S)
	    end
    end.
    
collect_new(Sofar, Nodelist) ->
    receive
	{nodeup, Node} ->
	    case lists:member(Node, Nodelist) of
		true ->
		    collect_new(Sofar, Nodelist);
		false ->
		    collect_new([Node | Sofar], Nodelist)
	    end
    after 3000 ->
	    net_kernel:monitor_nodes(false),
	    Sofar
    end.

%% This function polls a set of hosts according to a file called
%% .hosts.erlang that need to reside either in the current directory
%% or at your home directory ( The current dir is tried first )
%% world() returns a list of all nodes on the network that can be 
%% found (including ourselves) obs. $HOME variable is inspected
%%
%% Added possibility to supply a list of hosts instead of reading
%% the .hosts.erlang file. 971016 patrik@erix.ericsson.se
%% e.g. 
%% net_adm:world_list(['elrond.du.etx.ericsson.se', 'thorin.du.etx.ericsson.se']). 
%%
world() ->
    world(silent).

world(Verbose) ->
    case net_adm:host_file() of
        {error,R} -> exit({error, R});
        Hosts -> expand_hosts(Hosts, Verbose)
    end.

world_list(Hosts) when list (Hosts) ->
    expand_hosts(Hosts, silent).

world_list(Hosts, Verbose) when list(Hosts) ->
    expand_hosts(Hosts, Verbose).

expand_hosts(Hosts, Verbose) ->
    lists:flatten(collect_nodes(Hosts, Verbose)).

collect_nodes([],_) -> [];
collect_nodes([Host|Tail],Verbose) ->
    case collect_host_nodes(Host,Verbose) of
        nil ->
            collect_nodes(Tail,Verbose);
        L ->
            [L|collect_nodes(Tail,Verbose)]
    end.

collect_host_nodes(Host,Verbose) ->
    case names(Host) of
	{ok, Namelist} ->
	    do_ping(Namelist, atom_to_list(Host), Verbose);
	_ ->
	    nil
    end.

do_ping([], _Host, _Verbose) ->
    [];
do_ping([{Name, _} | Rest], Host, Verbose) ->
    Node = list_to_atom(Name ++ "@" ++ longshort(Host)),
    verbose(Verbose, "Pinging ~w -> ", [Node]),
    Result = ping(Node),
    verbose(Verbose, "~p\n", [Result]),
    case Result of
	pong ->
	    [Node | do_ping(Rest, Host, Verbose)];
	pang ->
	    do_ping(Rest, Host, Verbose)
    end.

verbose(verbose, Format, Args) ->
    io:format(Format, Args);
verbose(_, _, _) ->
    ok.

longshort(Host) ->
    case net_kernel:longnames() of
	false ->
	    uptodot(Host);
	true ->
	    Host
    end.

uptodot([$.|_]) -> [];
uptodot([])-> [];
uptodot([H|T]) -> [H|uptodot(T)].
