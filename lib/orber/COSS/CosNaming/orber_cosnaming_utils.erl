%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: CosNaming_BindingIterator_impl.erl
%% Creation date: 970902
%% Modified:
%%
%%-----------------------------------------------------------------
-module(orber_cosnaming_utils).

-include("orber_cosnaming.hrl").
-include("CosNaming.hrl").
-include("CosNaming_NamingContext.hrl").
-include("CosNaming_NamingContextExt.hrl").
-include_lib("orber/include/corba.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([query_result/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([addresses/1, name/1, 
	 check_addresses/1, check_name/1, 
	 key/1, select_type/1, lookup/1,
	 escape_string/1, unescape_string/1,
	 name2string/1, string2name/1]).

%%-----------------------------------------------------------------
%% Records
%%-----------------------------------------------------------------
-record(address, {protocol, version, host, port}).

%%-----------------------------------------------------------------
%% Defines
%%-----------------------------------------------------------------
%% DEFAULT VALUES:
%%
%% IIOP:
%%  - port:         2089
%%  - iiop version: 1.0
-define(DEF_VERS,      {1,0}).
-define(DEF_PORT,      2089).
-define(DEF_KEY,       "NameService").
-define(HTTP_DEF_PORT, 80).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%% Check a read transaction
query_result(?query_check(Qres)) -> 
    case Qres of
	[Hres] ->
	    Hres#orber_CosNaming.nameindex;
	[Hres | Tres] ->
	    error;
	[] ->
	    error;
	Other ->
	    error
    end.


%%----------------------------------------------------------------------
%% Function   : check_addresses
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
check_addresses(Str) ->
    {_, Rest2} = addresses(Str),
    case key(Rest2) of
	{_, []} ->
	    ok;
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end.

%%----------------------------------------------------------------------
%% Function   : check_name
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
check_name(Str) ->
    name(Str).

%%----------------------------------------------------------------------
%% Function   : select_type
%% Arguments  : A corbaloc/corbaname-string.
%% Description: 
%% Returns    : A tuple which contain data about what connection we want to use |
%%              {'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}
%%----------------------------------------------------------------------
select_type([$c, $o, $r, $b, $a, $l, $o, $c, $:|Rest1]) ->
    {Addresses, Rest2} = addresses(Rest1),
    case key(Rest2) of
	{Key, []} ->
	    {corbaloc, Addresses, Key};
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
select_type([$c, $o, $r, $b, $a, $n, $a, $m, $e, $:|Rest1]) ->
    {Addresses, Rest2} = addresses(Rest1),
    {Key, Rest3} = key(Rest2),
    Name = name(Rest3),
    {corbaname, Addresses, Key, string2name(Name)};

select_type([$f, $i, $l, $e, $:|Rest]) ->
    file(Rest);
select_type([$f, $t, $p, $:, $/, $/|Rest]) ->
    ftp(Rest);
select_type([$h, $t, $t, $p, $:, $/, $/|Rest]) ->
    http(Rest);

select_type(_) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{}).


%%----------------------------------------------------------------------
%% Function   : addresses
%% Arguments  : A corbaloc string.
%% Description: 
%% Returns    : A list of addresses an the remaining part possibly containg
%%              a Key and a stringified Name
%%----------------------------------------------------------------------
addresses(Str) ->
    addresses(address(protocol, Str, [], []), []).

addresses({false, rir, Rest}, []) ->
    {rir, Rest};
addresses({false, Adr, Rest}, Addresses) ->
    {[Adr|Addresses], Rest};
addresses({true, Adr, Rest}, Addresses) ->
    addresses(address(protocol, Rest, [], []), [Adr|Addresses]).

%% Which protocol.
address(protocol, [$:|T], [], []) ->
    address(version, T, [], [iiop]);
address(protocol, [$i, $i, $o, $p, $:|T], [], []) ->
    address(version, T, [], [iiop]);
address(protocol, [$r, $i, $r, $:|T], [], []) ->
    {false, rir, T};
address(protocol, _, _, _) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});

%% Parsed one address, no version found or port found.
address(version, [$,|T], Acc, Previous) ->
    {true, lists:reverse([?DEF_PORT, lists:reverse(Acc), ?DEF_VERS|Previous]), T};
address(version, [$/|T], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc), ?DEF_VERS|Previous]), T};
%% Found iiop version.
address(version, [$@|T], Acc, Previous) ->
    case Acc of
	[Minor, $., Major] ->
	    address(address, T, [], [{Major-$0, Minor-$0}|Previous]);
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
%% Found no iiop version, switch to port
address(version, [$:|T], Acc, Previous) ->
    address(port, T, [], [lists:reverse(Acc), ?DEF_VERS|Previous]);

%% Parsed one address, port not found.
address(address, [$,|T], Acc, Previous) ->
    {true, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), T};
address(address, [$/|T], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), T};

%% Parsed one address.
address(port, [$/|T], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when integer(Port) ->
	    {false, lists:reverse([Port|Previous]), T};
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
address(port, [$,|T], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when integer(Port) ->
	    {true, lists:reverse([Port|Previous]), T};
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;

%% EOS, chech how far we have reached so far and add necessary default values.
address(port, [], [], Previous) ->
    {false, lists:reverse([?DEF_PORT|Previous]), []};
address(port, [], Acc, Previous) ->
    case catch list_to_integer(lists:reverse(Acc)) of
	Port when integer(Port) ->
	    {false, lists:reverse([Port|Previous]), []};
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end;
address(address, [], Acc, Previous) ->
    {false, lists:reverse([?DEF_PORT, lists:reverse(Acc)|Previous]), []};
    
%% Found port
address(address, [$:|T], Acc, Previous) ->
    address(port, T, [], [lists:reverse(Acc)|Previous]);
address(Type, [H|T], Acc, Previous) ->
    address(Type, T, [H|Acc], Previous).


%%----------------------------------------------------------------------
%% Function   : key
%% Arguments  : A string which contain a Key we want to use and, if defined,
%%              stringified NameComponent sequence.
%% Description: 
%% Returns    : The Key and the remaining part, i.e., a stringified 
%%              NameComponent sequence.
%%----------------------------------------------------------------------
key(Str) ->
    key(Str, []).
key([], []) ->
    {?DEF_KEY, []};
key([], Acc) ->
    {lists:reverse(Acc), []};
key([$#|T], []) ->
    {?DEF_KEY, T};
key([$#|T], Acc) ->
    {lists:reverse(Acc), T};
key([$/|T], []) ->
    key(T, []);
key([H|T], Acc) ->
    key(T, [H|Acc]).

%%----------------------------------------------------------------------
%% Function   : name
%% Arguments  : A string describing a NameComponent sequence.
%% Description: 
%% Returns    : The input string |
%%              {'EXCEPTION', #'CosNaming_NamingContext_InvalidName'{}}
%%----------------------------------------------------------------------
name(Str) ->
    name(Str, []).
name([], Acc) ->
    lists:reverse(Acc);
name([$., $/|T], _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
name([$/, $/|T], _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
name([$/|T], []) ->
    name(T, []);
name([H|T], Acc) ->
    name(T, [H|Acc]).


%%----------------------------------------------------------------------
%% Function   : file
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
file(File) ->
    %% Perhaps we should run some checks here?
    {file, File}.

%%----------------------------------------------------------------------
%% Function   : ftp
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
ftp(Address) ->
    %% Perhaps we should run some checks here?
    {ftp, Address}.

%%----------------------------------------------------------------------
%% Function   : http
%% Arguments  : A string describing connection parameters.
%% Description: 
%% Returns    : A tuple consisting of data extracted from the given string.
%%----------------------------------------------------------------------
http(Address) ->
    case catch address(address, Address, [], []) of
	{false, [Host, _], Rest} ->
	    case key(Rest) of
		{Key, []} ->
		    {http, Host, ?HTTP_DEF_PORT, Key};
		_->
		    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
	    end;
	_->
	    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{})
    end.

%%----------------------------------------------------------------------
%% Function   : lookup
%% Arguments  : A tuple which contain data about what connection we want to use.
%% Description: 
%% Returns    : Object |
%%              {'EXCEPTION', E}
%%----------------------------------------------------------------------
lookup({corbaname, rir, Key, []}) ->
    %% If no key supplied NameService is defined to be default.
    corba:resolve_initial_references("NameService");
lookup({corbaname, rir, Key, Name}) ->
    NS = corba:resolve_initial_references(Key),
    'CosNaming_NamingContext':resolve(NS, Name);

lookup({corbaloc, rir, Key}) ->
     corba:resolve_initial_references(Key);

lookup({corbaname, [], Key, Name}) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup({corbaname, [[iiop, Vers, Host, Port]|Addresses], Key, ""}) ->
    iop_ior:create(Vers, "", Host, Port, Key);
lookup({corbaname, [[iiop, Vers, Host, Port]|Addresses], Key, Name}) ->
    NS = iop_ior:create(Vers, "", Host, Port, Key),
    case catch 'CosNaming_NamingContext':resolve(NS, Name) of
	{'EXCEPTION', _} ->
	    lookup({corbaname, Addresses, Key, Name});
	Obj ->
	    Obj
    end;
lookup({corbaname, [_|Addresses], Key, Name}) ->
    lookup({corbaname, Addresses, Key, Name});

lookup({corbaloc, [], Key}) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup({corbaloc, [[iiop, Vers, Host, Port]|Addresses], Key}) ->
    ObjRef = iop_ior:create(Vers, "", Host, Port, Key),
    case catch corba_object:non_existent(ObjRef) of
	false ->
	    ObjRef;
	true ->
	    lookup({corbaloc, Addresses, Key});
	_ ->
	    %% May be located on a version using '_not_existent'
            %% see CORBA2.3.1 page 15-34 try again.
	    case catch corba_object:not_existent(ObjRef) of
		false ->
		    ObjRef;
		_ ->
		    lookup({corbaloc, Addresses, Key})
	    end
    end;
		
lookup({corbaloc, [_|Addresses], Key}) ->
    lookup({corbaloc, Addresses, Key});
    

lookup({file, File}) ->
    case file:read_file(File) of
	{ok, IOR} ->
	    binary_to_list(IOR);
	{error, Reason} ->
	    corba:raise(#'CosNaming_NamingContext_InvalidName'{})
    end;
lookup({http, Host, Port, Key}) ->
    {ok, Socket} = create_connection(Host,Port),
    Request = "GET " ++ Key ++ " HTTP/1.0\r\n\r\n",
    case gen_tcp:send(Socket, Request) of
	ok ->
	    receive 
		{tcp_closed, Socket} ->
		    corba:raise(#'COMM_FAILURE'{minor=99, 
						completion_status=?COMPLETED_NO});
		{tcp, Socket, Response} ->
		    close_connection(Socket),
		    Response;
		_->
		    close_connection(Socket),
		    corba:raise(#'COMM_FAILURE'{minor=99, 
						completion_status=?COMPLETED_NO})
	    after 100000 ->
		    %% FIX this timeout!!
		    close_connection(Socket),
		    corba:raise(#'COMM_FAILURE'{minor=99, 
						completion_status=?COMPLETED_NO})
	    end
    end;
lookup({ftp, Address}) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
lookup(_) ->
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{}).


create_connection(Host,Port) ->
    case gen_tcp:connect(Host,Port,[{packet,0},{reuseaddr,true}]) of
	{ok,Socket} ->
	    {ok,Socket};
	Error ->
	    corba:raise(#'COMM_FAILURE'{minor=99, completion_status=?COMPLETED_NO})
    end.
close_connection(Socket) ->
    gen_tcp:close(Socket).


%%----------------------------------------------------------------------
%% Function   : name2string
%% Arguments  : A sequence of NameComponents
%% Description: 
%% Returns    : A string describing the sequence.
%%----------------------------------------------------------------------
name2string(Name) ->
    name2string(lists:reverse(Name), []).
name2string([], Acc) ->
    lists:flatten(Acc);
name2string([#'CosNaming_NameComponent'{id="", kind=""}], Acc) ->
    name2string([], [$.|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=""}], Acc) ->
    name2string([], [convert_reserved(ID)|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=Kind}], Acc) ->
    name2string([], [convert_reserved(ID), $., convert_reserved(Kind)|Acc]);
name2string([#'CosNaming_NameComponent'{id="", kind=""}|T], Acc) ->
    name2string(T, [$/, $.|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=""}|T], Acc) ->
    name2string(T, [$/, convert_reserved(ID)|Acc]);
name2string([#'CosNaming_NameComponent'{id=ID, kind=Kind}|T], Acc) ->
    name2string(T, [$/, convert_reserved(ID), $., convert_reserved(Kind)|Acc]);
name2string(_, _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{}).

%% '/' and '.' are reserved as separators but can be overridden by using '\'.
convert_reserved([]) ->
    [];
convert_reserved([$/|T]) ->
    [$\\, $/|convert_reserved(T)];
convert_reserved([$.|T]) ->
    [$\\, $.|convert_reserved(T)];
convert_reserved([$\\, H|T]) ->
    [$\\, H|convert_reserved(T)];
convert_reserved([H|T]) ->
    [H|convert_reserved(T)].


%%----------------------------------------------------------------------
%% Function   : string2name
%% Arguments  : A string describing a sequence of NameComponents.
%% Description: 
%% Returns    : A sequence of NameComponents
%%----------------------------------------------------------------------
string2name([]) ->
    [];
string2name(Str) ->
    {NC, Rest} = get_NC(id, Str, [], []),
    [NC|string2name(Rest)].
    
get_NC(id, [], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=""}, []};
get_NC(kind, [], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=lists:reverse(Kind)}, []};
%% // is not allowed; must be /./
get_NC(id, [$/|T], [], _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
get_NC(id, [$., $/|T], [], _) ->
    {#'CosNaming_NameComponent'{id="", kind=""}, T};
%% End of this ID/Kind; in this case kind eq. "".
get_NC(id, [$/|T], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=""}, T};
get_NC(kind, [$/|T], ID, Kind) ->
    {#'CosNaming_NameComponent'{id=lists:reverse(ID), kind=lists:reverse(Kind)}, T};
%% ID exist but it's not allowed to write "id1./id2.kind2".
get_NC(id, [$., $/|T], _, _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{});
get_NC(id, [$\\, $., H|T], ID, Kind) ->
    get_NC(id, T, [H, $.|ID], Kind);
get_NC(id, [$\\, $/, H|T], ID, Kind) ->
    get_NC(id, T, [H, $/|ID], Kind);
get_NC(kind, [$\\, $., H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(kind, [$\\, $/, H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(id, [$.|T], ID, Kind) ->
    get_NC(kind, T, ID, Kind);
get_NC(id, [H|T], ID, Kind) ->
    get_NC(id, T, [H|ID], Kind);
get_NC(kind, [H|T], ID, Kind) ->
    get_NC(kind, T, ID, [H|Kind]);
get_NC(_, _, _, _) ->
    corba:raise(#'CosNaming_NamingContext_InvalidName'{}).


%% Converts \< to '%3c' 
escape_string(Str) ->
    escape_string(Str, []).
escape_string([], Acc) ->
    lists:reverse(Acc);
escape_string([$\\, Char |T], Acc) ->
    escape_string(T, [code_character(16#0f band Char), 
		      code_character(16#0f band (Char bsr 4)),$%|Acc]);
escape_string([Char|T], Acc) ->
    escape_string(T, [Char|Acc]).


code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $a + (N - 10).

%% Converts '%3c' to \<
unescape_string(Str) ->
    unescape_string(Str, []).
unescape_string([], Acc) ->    
    lists:reverse(Acc);
unescape_string([$%, H1, H2 |T], Acc) ->
    I1 = hex2int(H1),
    I2 = hex2int(H2),
    I = I1 * 16 + I2,
    unescape_string(T, [I, $\\|Acc]);
unescape_string([H|T], Acc) ->
    unescape_string(T, [H|Acc]).

hex2int(H) when H >= $a ->
    10 + H - $a;
hex2int(H) when H >= $A ->
    10 + H -$A;
hex2int(H) ->
    H - $0.

%%-------------------------- END OF MODULE -----------------------------
