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
%% The Initial Developer of the Original Code is Mobile Arts AB
%% Portions created by Mobile Arts are Copyright 2002, Mobile Arts AB
%% All Rights Reserved.''
%% 
%%
-module(http_cookie).

-include("http.hrl").

-export([header/5, store_cookie/2]).


%%% Johan Bloms assembled cookie code might need some work to make it work

%%% Add cookie header if cookie settings is true and matching domain, path and
%%% port found in db.
header(_Host,_Port,_Path,_Query,false) ->
    [];

header(Host, Port, Path, _Query, true) ->
    case lookup_cookies(Host, Port, Path) of
	{Vsn, CookieList} ->
	    CookieStr = print_cookies(CookieList,[]),
	    ["Cookie:","$Version=",integer_to_list(Vsn),CookieStr, ?CRLF];
	no_match ->
	    []
    end.

print_cookies([],Out) ->
    lists:reverse(Out);
print_cookies([#http_cookie{name=Name,value=Val,
			    key={{_,Domain},Path},port=Port}|Rest],Out) ->
    _Attrs =  %% FIXME This variable is not used! Probably should be! 
	["$Domain=",Domain,
	 ";$Path=",Path,
	 if 
	     Port==undefined ->
		 [];
	     true -> % FIXME!
		 ";$Port=",Port
	 end],
    Cookie=[";",Name,"=",Val],
    print_cookies(Rest,[Cookie|Out]).
    

store_cookie(#http_response_h{other=Other},Req) ->
    if
	(Req#request.settings)#http_options.cookie ->
	    store_setcookie(Other,Req,[]);
	true ->
	    ok
    end.
    
store_setcookie([],_Req,[]) ->
    ok;
store_setcookie([],_Req,CookieList) ->
    store_cookies(CookieList);
store_setcookie([{'Set-Cookie',SetCookie}|Rest],Req,Out) ->
    Cookie = parse_setcookie_netscape(SetCookie,Req),
    store_setcookie(Rest,Req,[Cookie|Out]);
store_setcookie([{'Set-Cookie2',SetCookie2}|Rest],Req,Out) ->
    CookieList = parse_setcookie2(SetCookie2,Req),
    store_setcookie(Rest,Req,CookieList++Out);
store_setcookie([_H|Rest],Req,Out) ->
    store_setcookie(Rest,Req,Out).

%%% Note:
%%% - 
%%%   Also the Netscape Set-Cookie only allow a single cookie in each header.
%%%   From http://wp.netscape.com/newsref/std/cookie_spec.html
%%%
%%%   The expires attribute specifies a date string that defines the valid life
%%%   time of that cookie. Once the expiration date has been reached, the
%%%   cookie will no longer be stored or given out.
%%%     The date string is formatted as:
%%%         Wdy, DD-Mon-YYYY HH:MM:SS GMT
%%%     This is based on RFC 822, RFC 850, RFC 1036, and RFC 1123, with the
%%%     variations that the only legal time zone is GMT and the separators
%%%     between the elements of the date must be dashes.
%%%     expires is an optional attribute. If not specified, the cookie will
%%%     expire when the user's session ends.
set_parameter("comment",Pval,rfc2965,Cookie) ->
    Cookie#http_cookie{comment=Pval};
set_parameter("commenturl",Pval,rfc2965,Cookie) -> % Only SetCookie2
    Cookie#http_cookie{comment_url=Pval};
set_parameter("discard",Pval,rfc2965,Cookie) -> % Only SetCookie2
    Cookie#http_cookie{discard=Pval};
set_parameter("domain",Pval,_Type,Cookie=#http_cookie{key={_,Path}}) ->
    DomainSpec={subdomain,string:tokens(Pval,".")},
    Cookie#http_cookie{key={DomainSpec,Path}};
set_parameter("max-age",Pval,rfc2965,Cookie) ->
    Cookie#http_cookie{maxage=Pval};
set_parameter("path",Pval,_Type,Cookie=#http_cookie{key={Domain,_}}) ->
    Cookie#http_cookie{key={Domain,Pval}};
set_parameter("port",Pval,rfc2965,Cookie) -> % Only SetCookie2
    Cookie#http_cookie{port=Pval};
set_parameter("secure",Pval,_Type,Cookie) ->
    Cookie#http_cookie{secure=Pval};
set_parameter("version",Pval,rfc2965,Cookie) ->
    Cookie#http_cookie{version=Pval};
set_parameter("expires",Pval,netscape,Cookie) ->
    %% FIXME: This code is note used, probably should be!
    %%{{Year,Month,Date},{Hour,Min,Sec}}=
    httpd_util:convert_netscapecookie_date(Pval),
    MaxAge=1,
    Cookie#http_cookie{maxage=MaxAge};
set_parameter(Pkey,Pval,_Type,Cookie) ->
    %% Ignore unknown attributes (Sectio
    io:format("WARNING Unknown cookie parameter ~p = ~p~n"
	      " Cookie=~p~n",[Pkey,Pval,Cookie]),
    Cookie.

%%% Parse old-style Netscape cookies. These differs from Set-Cookie in RFC2109 
%%% (obsoleted by RFC2965) in the following ways:
%%% - Includes:
%%%   + expires=DATE attribute
%%% - Excludes
%%%   + comment=value attribute
%%%   + max-age=value attribute
%%% - Only allow a for single cookie in each Set-Cookie header
parse_setcookie_netscape(SetCookie,Req) ->
    case string:tokens(SetCookie,";") of
	[NameVal|Parameters] ->	    
	    Cookie=parse_cookie(NameVal,Req),
	    parse_cookie_parameters(Parameters,Cookie,netscape);
	[] ->
	    {error,bad_cookie}
    end.

%%% Parse cookie name and value, and set default attributes from request. 
%%% FIXME! What if Host is IP number?
parse_cookie(NameVal,#request{address={Host,_Port},path=Path}) ->
    case scan_char(NameVal,$=,[]) of
	{Name,Value} ->
	    DomainSpec={exact,string:tokens(Host,".")},
	    #http_cookie{name=Name,value=Value,key={DomainSpec,Path}};
	_ ->
	    {error,bad_cookie}
    end.

scan_char([], _Char, _Out) ->
    {error,char_not_found};
scan_char([Char|Rest],Char,Out) ->
    {lists:reverse(Out),Rest};
scan_char([H|Rest],Char,Out) ->
    scan_char(Rest,Char,[H|Out]).

parse_cookie_parameters([],Cookie,_Type) ->
    Cookie;
parse_cookie_parameters([P|Rest],Cookie,Type) ->
    case string:tokens(P,"=") of
	[Pname,Pval] ->
	    Pname2=httpd_util:to_lower(string:strip(Pname)),
	    NewCookie=set_parameter(Pname2,Pval,Type,Cookie),
	    parse_cookie_parameters(Rest,NewCookie,Type);
	_ ->
	    {error,bad_cookie_par}
    end.
domain_match(Host, _Port, _Path, Db) ->
    ReqHost=string:tokens(Host,"."),
    SubMatchCond=gen_submatchcond(length(ReqHost),ReqHost,{'=:=',ReqHost,'$2'}),
%    io:format("domain_match~n SubMatchCond=~p~n",[SubMatchCond]),
    MatchSpec=
	[{{'_',{{exact,'$2'},'$3'},'_','_','_','_','_','_','_','_','_'},
	  [{'=:=',ReqHost,'$2'}], % MatchConditions,
	  ['$_']  % MatchBody
	 },
	 {{'_',{{subdomain,'$2'},'$3'},'_','_','_','_','_','_','_','_','_'},
	  [SubMatchCond], % MatchConditions,
	  ['$_']  % MatchBody
	 }],
    case ets:select(Db, MatchSpec) of
	[] ->
	    no_match;
	CookieList ->
	    Vsn=0, % FIXME! Check version numbers in cookies
	    {Vsn,CookieList}
    end.


%%% ReqHost=[di,se]
%%% Domain=[se]
gen_submatchcond(1,_ReqHost,Cond) when tuple(Cond) ->
    Cond;
gen_submatchcond(1,_ReqHost,Cond) ->
    list_to_tuple(['orelse'|lists:reverse(Cond)]);
gen_submatchcond(Num,ReqHost,Cond) ->
    NewReqHost=tl(ReqHost),    
    NewCond=if
		tuple(Cond) ->
		    [{'=:=',NewReqHost,'$2'},Cond];
		true ->
		    [{'=:=',NewReqHost,'$2'}|Cond]
	    end,
    gen_submatchcond(Num-1,NewReqHost,NewCond).

lookup_cookies(Host,Port,Path) ->
    domain_match(Host,Port,Path,cookie_db).

store_cookies(CookieList) ->
    store_cookies(CookieList,cookie_db).

%%% Store list of parsed cookies in db.
%%% Note:
%%% - If an old cookie with the same name exists, it should be removed first.
store_cookies([], _Db) ->
    ok;
store_cookies([Cookie=#http_cookie{name=Name}|Rest],Db) ->
    case catch ets:lookup_element(Db,Name,3) of
	{'EXIT', _Reason} ->
	    ok;
	[OldCookie] ->
	    ets:delete_object(Db,OldCookie)
    end,
    ets:insert(Db,Cookie),
    store_cookies(Rest,Db).

%%% FIXME! There can be multiple cookies defined in each setCookie header.

parse_setcookie2(SetCookie,Req) ->
    CookieList=string:tokens(SetCookie,","),
    parse_setcookie2(CookieList,Req,[]).

parse_setcookie2([], _Req,Out) ->
    Out;
parse_setcookie2([SetCookie|Rest],Req,Out) ->
    case string:tokens(SetCookie,";") of
	[NameVal|Parameters] ->
	    Cookie=parse_cookie(NameVal,Req),
	    Cookie2=parse_cookie_parameters(Parameters,Cookie,rfc2965),
	    parse_setcookie2(Rest,Req,[Cookie2|Out]);
	[] ->
	    {error,bad_cookie}
    end.


