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
-module(inet_config).

-include("inet_config.hrl").
-include_lib("kernel/include/inet.hrl").

-import(lists, [foreach/2, member/2, reverse/1]).

-export([init/0]).

%%
%% Must be called after inet_db:start
%%
%% Order in which to load inet_db data:
%% 1. Hostname  (possibly derive domain and search)
%% 2. OS default  /etc/resolv.conf,  Windows registry etc
%%    a) Hosts database
%%    b) Resolver options 
%% 3. Config (kernel app)
%% 4. Root   (otp root)
%% 5. Home   (user inetrc)
%%
%%
init() ->
    set_hostname(),
    case os:type() of
	{unix, Type} ->
	    %% The Etc variable enables us to run tests with other configuration files than
	    %% the normal ones 
	    Etc = case os:getenv("ERL_INET_ETC_DIR") of
		      false -> ?DEFAULT_ETC;
		      _EtcDir -> 
			  _EtcDir				 
		  end,
	    load_hosts(filename:join(Etc,?DEFAULT_HOSTS),unix),
	    load_resolv(filename:join(Etc,"resolv.conf"), resolv),
	    case Type of
		unix_sv ->			% UnixWare 2.1.2
		    case inet_db:add_rc_list([{lookup,["native"]}]) of
			ok ->
			    true;
			{error, Reason} ->
			    error("can't set lookup to native: ~p", [Reason])
		    end;
		freebsd -> %% we may have to check version (2.2.2)
		    load_resolv(filename:join(Etc,"host.conf"), host_conf_freebsd);
		'bsd/os' ->
		    load_resolv(filename:join(Etc,"irs.conf"), host_conf_bsdos);
		linux ->
		    load_resolv(filename:join(Etc,"host.conf"),host_conf_linux),

		    % It may be the case that the domain name was not set
		    % because the hostname was short. But we can now look it
		    % up and get the long name and the domain name from it.

		    % FIXME: The second call to set_hostname will insert
		    % a duplicate entry in the search list.

		    case inet_db:res_option(domain) of
			"" ->
			    case inet:gethostbyname(inet_db:gethostname()) of
				{ok,#hostent{h_name = []}} ->
				    true;
				{ok,#hostent{h_name = HostName}} ->
				    set_hostname({ok,HostName});
				_ ->
				    true
			    end;
			_ ->
			    true
		    end;
		sunos ->
		    case os:version() of
			{Major,_,_} when Major >= 5 ->
			    load_resolv(filename:join(Etc,"nsswitch.conf"), nsswitch_conf);
			_ -> 
			    ok
		    end;
		_ ->
		    ok
	    end,
	    add_dns_lookup(inet_db:res_option(lookup));

	{win32, Type} ->
	    win32_load_from_registry(Type),
	    inet_db:set_lookup([native]);

	vxworks ->
	    vxworks_load_hosts(),
	    inet_db:set_lookup([file, dns]),
	    case os:getenv("ERLRESCONF") of
		false ->
		    no_ERLRESCONF;
		Resolv ->
		    load_resolv(Resolv, resolv)
	    end;
	_ ->
	    false
    end,
    load_rc(),

    %% Now test if we can lookup our own hostname in the inet_hosts table.
    standalone_host().

%% This host seems to be standalone.  Add a shortcut to enable us to
%% lookup our own hostname.

standalone_host() ->
    Name = inet_db:gethostname(),
    case inet_hosts:gethostbyname(Name) of
	{ok, #hostent{}} ->
	    ok;
	_ -> 
	    case inet_db:res_option(domain) of
		"" ->
		    inet_db:add_host({127,0,0,1}, [Name]);
		Domain ->
		    FQName = lists:append([inet_db:gethostname(),
					    ".", Domain]),
		    case inet_hosts:gethostbyname(FQName) of
			{ok, #hostent{
			   h_name      = N,
			   h_addr_list = [IP|_],
			   h_aliases   = As}} ->
			    inet_db:add_host(IP, [N | As] ++ [Name]);
			_ ->
			    inet_db:add_host({127,0,0,1}, [Name])
		    end
	    end,
	    Lookup = inet_db:res_option(lookup),
	    case lists:member(file, Lookup) of
		true -> 
		    ok;
		false -> 
		    inet_db:set_lookup(Lookup++[file]),
		    ok
	    end
    end.

add_dns_lookup(L) ->
    case lists:member(dns,L) of
	true -> ok;
	_ ->
	    case application:get_env(kernel,inet_dns_when_nis) of
		{ok,true} -> 
		    add_dns_lookup(L,[]);
		_ ->
		    ok
	    end
    end.

add_dns_lookup([yp|T],Acc) ->
    add_dns_lookup(T,[yp,dns|Acc]);
add_dns_lookup([H|T],Acc) ->
    add_dns_lookup(T,[H|Acc]);
add_dns_lookup([],Acc) ->
    inet_db:set_lookup(lists:reverse(Acc)).

%%
%% Set the hostname (SHORT)
%% If hostname is long use the suffix as default domain
%% and initalize the search option with the parts of domain
%%
set_hostname() ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    Res = inet:gethostname(U),
	    inet_udp:close(U),
	    set_hostname(Res);
	_ ->
	    set_hostname({ok, []})
    end.

set_hostname({ok,Name}) when length(Name) > 0 ->
    {Host, Domain} = lists:splitwith(fun($.) -> false;
					(_)  -> true
				     end, Name),
    inet_db:set_hostname(Host),
    set_search_dom(Domain);
set_hostname({ok,[]}) ->
    inet_db:set_hostname("nohost"),
    set_search_dom("nodomain").

set_search_dom([$.|Domain]) ->
    %% leading . not removed by dropwhile above.
    inet_db:set_domain(Domain),
    inet_db:ins_search(Domain),
    Domain;
set_search_dom([]) ->
    [];
set_search_dom(Domain) ->
    inet_db:set_domain(Domain),
    inet_db:ins_search(Domain),
    Domain.

%%
%% Load resolver data
%% normally assume that we load from /etc/resolv.conf
%%
load_resolv(File, Func) ->
    case get_file(File) of
	{ok,Bin} ->
	    case apply(inet_parse, Func, [File,{chars,Bin}]) of
		{ok, Ls} -> inet_db:add_rc_list(Ls);
		{error, Reason} ->
		    error("parse error in file ~s: ~p", [File, Reason])
	    end;
	Error ->
	    warning("file not found ~s~n", [File])
    end.

%%
%% Load a UNIX hosts file
%%
load_hosts(File,Os) ->
    case get_file(File) of
	{ok,Bin} ->
	    case inet_parse:hosts(File,{chars,Bin}) of
		{ok, Ls} ->
		    foreach(
		      fun({IP, Name, Aliases}) -> 
			      inet_db:add_host(IP, [Name|Aliases]) end,
		      Ls);
		{error, Reason} ->
		    error("parse error in file ~s: ~p", [File, Reason])
	    end;
	Error ->
	    case Os of
		unix ->
		    error("file not found ~s~n", [File]);
		_ -> 
		    %% for windows or nt the hosts file is not always there
		    %% and we don't require it
		    ok
	    end
    end.

%%
%% Load resolver data from Windows registry
%%
win32_load_from_registry(Type) ->
    %% The TcpReg variable enables us to run tests with other registry configurations than
    %% the normal ones 
    TcpReg = case os:getenv("ERL_INET_ETC_DIR") of
		 false -> [];
		 _TReg -> _TReg
	     end,
    {ok, Reg} = win32reg:open([read]),
    {TcpIp,HFileKey} =
    case Type of
	nt ->
	    case TcpReg of
		[] -> 
		    {"\\hklm\\system\\CurrentControlSet\\Services\\TcpIp\\Parameters",
		     "DataBasePath" };
		Other ->
		    {Other,"DataBasePath"}
	    end;
	windows ->
	    case TcpReg of 
		[] ->
		    {"\\hklm\\system\\CurrentControlSet\\Services\\VxD\\MSTCP",
		     "LMHostFile" };
		Other ->
		    {Other,"LMHostFile"}
	    end
    end,
    Result = 
	case win32reg:change_key(Reg,TcpIp) of
	    ok ->
		win32_load1(Reg,Type,HFileKey);
	    {error, Reason} ->
		error("Failed to locate TCP/IP parameters (is TCP/IP installed)?",
		      [])
	end,
    win32reg:close(Reg),
    Result.

win32_load1(Reg,Type,HFileKey) ->
    Names = [HFileKey, "Domain", "DhcpDomain", 
	     "EnableDNS", "NameServer", "SearchList"],
    case win32_get_strings(Reg, Names) of
	[DBPath0, Domain, DhcpDomain, 
	 EnableDNS, NameServers0, Search] ->
	    inet_db:set_domain(
	      case Domain of "" -> DhcpDomain; _ -> Domain end),
	    NameServers = win32_split_line(NameServers0,Type),
	    AddNs = fun(Addr) ->
			    case inet_parse:address(Addr) of
				{ok, Address} ->
				    inet_db:add_ns(Address);
				{error, _} ->
				    error("Bad TCP/IP address in registry", [])
			    end
		    end,
	    foreach(AddNs, NameServers),
	    Searches0 = win32_split_line(Search,Type),
	    Searches = case member(Domain, Searches0) of
			   true  -> Searches0;
			   false -> [Domain|Searches0]
		       end,
	    foreach(fun(D) -> inet_db:add_search(D) end, Searches),
	    if Type == nt ->
		    DBPath = win32reg:expand(DBPath0),
		    load_hosts(filename:join(DBPath, "hosts"),nt);
		Type == windows ->
		    load_hosts(filename:join(DBPath0,""),windows)
	    end,
%% Maybe activate this later as an optimization
%% For now we use native always as the SAFE way
%%	    case NameServers of
%%		[] -> inet_db:set_lookup([native, file]);
%%		_  -> inet_db:set_lookup([dns, file, native])
%%	    end;
	    true;
	{error, Resaon} ->
	    error("Failed to read TCP/IP parameters from registry", [])
    end.

win32_split_line(Line,nt) -> inet_parse:split_line(Line);
win32_split_line(Line,windows) -> string:tokens(Line, ",").

win32_get_strings(Reg, Names) ->
    win32_get_strings(Reg, Names, []).

win32_get_strings(Reg, [Name|Rest], Result) ->
    case win32reg:value(Reg, Name) of
	{ok, Value} when list(Value) ->
	    win32_get_strings(Reg, Rest, [Value|Result]);
	{ok, NotString} ->
	    {error, not_string};
	{error, Reason} ->
	    win32_get_strings(Reg, Rest, [""|Result])
    end;
win32_get_strings(_, [], Result) ->
    lists:reverse(Result).

%%
%% Load host data from VxWorks hostShow command
%%

vxworks_load_hosts() ->
    HostShow = os:cmd("hostShow"),
    case check_hostShow(HostShow) of
	Hosts when list(Hosts) ->
	    case inet_parse:hosts_vxworks({chars, Hosts}) of
		{ok, Ls} ->
		    foreach(
		      fun({IP, Name, Aliases}) -> 
			      inet_db:add_host(IP, [Name|Aliases])
		      end,
		      Ls);
		{error,Reason} ->
		    error("parser error VxWorks hostShow ~s", [Reason])
	    end;
	Error ->
	    error("error in VxWorks hostShow~s~n", [HostShow])
    end.

%%
%% Check if hostShow yields at least two line; the first one
%% starting with "hostname", the second one starting with
%% "--------".
%% Returns: list of hosts in VxWorks notation
%% rows of 'Name          IP                [Aliases]  \n'
%% if hostShow yielded these two lines, false otherwise.
check_hostShow(HostShow) ->
    check_hostShow(["hostname", "--------"], HostShow).

check_hostShow([], HostShow) ->
    HostShow;
check_hostShow([String_match|Rest], HostShow) ->
    case lists:prefix(String_match, HostShow) of
	true ->
	    check_hostShow(Rest, next_line(HostShow));
	false ->
	    false
    end.

next_line([]) ->
    [];
next_line([$\n|Rest]) ->
    Rest;
next_line([First|Rest]) ->
    next_line(Rest).

%% 
%% Search .inetrc and let user provided inetrc files
%% add more (or reset) information.
%% If no .inetrc file is found this node may not set up any connections
%% to other nodes !!
%% 
load_rc() ->
    load_inetrc([config, root, home]).

load_inetrc([F|Fs]) ->
    case which_file(F) of
	{ok,File} -> get_rc(File);
	error -> ignore
    end,
    load_inetrc(Fs);
load_inetrc([]) ->
    ok.

%% Determine the filename
which_file(config) ->
    case application:get_env(inetrc_dir) of
	{ok, D} -> {ok, filename:join(D,".inetrc")};
	_ -> error
    end;
which_file(cwd) ->  {ok, filename:join(".",".inetrc")};
which_file(home) ->
    case init:get_argument(home) of
	{ok, [[Home]]} -> {ok,filename:join(Home, ".inetrc")};
	_ -> error
    end;
which_file(root) ->
    case init:get_argument(root) of
	{ok, [[Root]]} -> {ok, filename:join([Root, "bin", ".inetrc"])};
	_ -> error
    end;
which_file(File) when list(File) -> {ok,File};
which_file(_) -> error.


get_rc(File) ->
    case get_file(File) of
	{ok, Bin} ->
	    case parse_inetrc(Bin) of
		{ok, Ls} ->
		    case inet_db:add_rc_list(Ls) of
			ok -> ok;
			Error ->
			    error("syntax error in ~s~n", [File]),
			    Error
		    end;
		Error -> 
		    error("parse error in ~s~n", [File]),
		    Error
	    end;
	Error -> Error
    end.

%% XXX Check if we really need to prim load the stuff
get_file(File) ->
    case erl_prim_loader:get_file(File) of
	{ok, Bin, _} -> {ok, Bin};
	Error -> Error
    end.

error(Fmt, Args) ->
    error_logger:error_msg("inet_config:" ++ Fmt, Args).

warning(Fmt, Args) ->
    case application:get_env(kernel,inet_warnings) of
	%{ok,silent} -> ok;
	{ok,on} -> 
	    error_logger:info_msg("inet_config:" ++ Fmt, Args);
	_ ->
	    ok
    end.

%% 
%% Parse .inetrc, i.e. make a binary of a term list.
%% The extra newline is to let the user ignore the whitespace !!!
%% Ignore leading whitespace before a token (due to bug in erl_scan) !
%% 
parse_inetrc(Bin) ->
    Str = binary_to_list(Bin) ++ "\n", 
    parse_inetrc(Str, 1, []).

parse_inetrc_skip_line([], Line, Ack) ->
    {ok, reverse(Ack)};
parse_inetrc_skip_line([$\n|Str], Line, Ack) ->
    parse_inetrc(Str, Line+1, Ack);
parse_inetrc_skip_line([_|Str], Line, Ack) ->
    parse_inetrc_skip_line(Str, Line, Ack).

parse_inetrc([$%|Str], Line, Ack) ->
    parse_inetrc_skip_line(Str, Line, Ack);
parse_inetrc([$\s|Str], Line, Ack) ->
    parse_inetrc(Str, Line, Ack);
parse_inetrc([$\n |Str], Line, Ack) ->
    parse_inetrc(Str, Line+1, Ack);
parse_inetrc([$\t|Str], Line, Ack) ->
    parse_inetrc(Str, Line, Ack);
parse_inetrc([], _, Ack) ->
    {ok, reverse(Ack)};


%% The clauses above are here due to a bug in erl_scan (OTP-1449).

parse_inetrc(Str, Line, Ack) ->
    case erl_scan:tokens([], Str, Line) of
	{done, {ok, Tokens, EndLine}, MoreChars} ->
	    case erl_parse:parse_term(Tokens) of
		{ok, Term} ->
		    parse_inetrc(MoreChars, EndLine, [Term|Ack]);
		Error ->
		    {error, {'parse_.inetrc', Error}}
	    end;
	{done, {eof, _}, _} ->
	    {ok, reverse(Ack)};
	{done, Error, _} ->
	    {error, {'scan_.inetrc', Error}};
	{more_chars, _} ->
	    {error, {'scan_.inetrc', {eof, Line}}};
	{more, _} -> %% Bug in erl_scan !!
	    {error, {'scan_.inetrc', {eof, Line}}}
    end.
