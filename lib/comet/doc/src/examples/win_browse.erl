-module(win_browse).

-include("erl_com.hrl").

-export([open/1, example/0]).
open(Url) ->
    {ok, _Pid}= erl_com:get_program(win_browse),
    Obj= erl_com:create_dispatch(win_browse, "InternetExplorer.Application",
				 ?CLSCTX_LOCAL_SERVER),
    erl_com:invoke(Obj, "Navigate", [Url]),
    erl_com:property_put(Obj, "Visible", true),
    Obj.

example() ->
    open("www.erlang.org").

