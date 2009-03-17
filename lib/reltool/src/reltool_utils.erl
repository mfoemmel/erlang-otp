%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-module(reltool_utils).

%% Public
-compile([export_all]).

-include_lib("wx/include/wx.hrl").
-include("reltool.hrl").

root_dir() ->
    code:root_dir().

erl_libs() ->
    case os:getenv("ERL_LIBS") of
	false -> 
	    [];
	LibStr ->
	    string:tokens(LibStr, ":;")
    end.    

lib_dirs(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Files} ->
	    [F || F <- Files,
		  filelib:is_dir(filename:join([Dir, F]),
				 erl_prim_loader)];
	error -> 
	    []
    end.

%% "asn1-1.6.2" -> {"asn1", "1.6.2"}; "asn1" -> {"asn1", ""}
split_app_name(Name) ->
    Pred =
	fun(Elem) ->
		if
		    Elem =:= $\. -> true;
                    Elem >= $0, Elem =< $9 -> true;
                    true -> false
                end
        end, 
    case lists:splitwith(Pred, lists:reverse(Name)) of
	{Vsn, [$- | App]} ->
	    {list_to_atom(lists:reverse(App)), lists:reverse(Vsn)};
	_ ->
	    {list_to_atom(Name), ""}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_consult(Bin) when is_binary(Bin) ->
    case erl_scan:string(binary_to_list(Bin)) of
	{ok, Tokens, _EndLine} ->
	    prim_parse(Tokens, []);
	{error, {_ErrorLine, Module, Reason}, _EndLine} ->
	    {error, Module:format_error(Reason)}
    end;
prim_consult(FullName) when is_list(FullName) ->
    case erl_prim_loader:get_file(FullName) of
        {ok, Bin, _} ->
	    prim_consult(Bin);
        error ->
            {error, file:format_error(enoent)}
    end.

prim_parse(Tokens, Acc) ->
    case lists:splitwith(fun(T) -> element(1,T) =/= dot end, Tokens) of
        {[], []} ->
            {ok, lists:reverse(Acc)};
        {Tokens2, [{dot,_} = Dot | Rest]} ->
            case erl_parse:parse_term(Tokens2 ++ [Dot]) of
                {ok, Term} ->
                    prim_parse(Rest, [Term | Acc]);
		{error, {_ErrorLine, Module, Reason}} ->
		    {error, Module:format_error(Reason)}
            end;
        {Tokens2, []} ->
            case erl_parse:parse_term(Tokens2) of
                {ok, Term} ->
                    {ok, lists:reverse([Term | Acc])};
		{error, {_ErrorLine, Module, Reason}} ->
		    {error, Module:format_error(Reason)}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_rels() ->
    Kernel = #rel_app{name = kernel, incl_apps = []},
    Stdlib = #rel_app{name = stdlib, incl_apps = []},
    Sasl   = #rel_app{name = sasl,   incl_apps = []},
    [
     #rel{name = ?DEFAULT_REL_NAME,
	  vsn = "1.0",
	  rel_apps = [Kernel, Stdlib]},
     #rel{name = "start_sasl",
	  vsn = "1.0",
	  rel_apps = [Kernel, Sasl, Stdlib]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign_image_list(ListCtrl) ->
    Art = wxImageList:new(16,16),
    [wxImageList:add(Art, wxArtProvider:getBitmap(Image, [{size, {16,16}}])) 
     || Image <- ["wxART_ERROR",
		  "wxART_WARNING",
                  "wxART_QUESTION",
                  "wxART_TICK_MARK",
		  "wxART_CROSS_MARK",
		  "wxART_GO_HOME"]],
    wxListCtrl:assignImageList(ListCtrl, Art, ?wxIMAGE_LIST_SMALL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_latest_resize(#wx{obj = ObjRef, event = #wxSize{}} = Wx) ->
    receive
	#wx{obj = ObjRef, event = #wxSize{}} = Wx2 ->
	    get_latest_resize(Wx2)
    after 10 ->
	    Wx
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mod_conds() ->
    ["all (ebin + app file)", "ebin + derived", "app file + derived", "derived", "none"].

list_to_mod_cond(List) ->
    case List of
	"all" ++ _   -> all;
 	"ebin" ++ _  -> ebin;
	"app" ++ _   -> app;
	"derived"    -> derived;
	"none"       -> none
    end.

mod_cond_to_index(ModCond) ->
    case ModCond of
	all       -> 0;
	ebin      -> 1;	
	app       -> 2;
	derived   -> 3;
	undefined -> 3;
	none      -> 4
    end.

incl_conds() ->
    ["include", "exclude", "derived"].

list_to_incl_cond(List) ->
    case List of
	"include" -> include;
 	"exclude" -> exclude;
	"derived" -> derived
    end.

incl_cond_to_index(ModCond) ->
    case ModCond of
	include -> 0;
	exclude -> 1;	
	derived -> 2
    end.

elem_to_index(Elem, List) ->
    elem_to_index(Elem, List, 1).

elem_to_index(Elem, [H | T], Index) ->
    case Elem =:= H of
	true -> Index;
	false -> elem_to_index(Elem, T, Index + 1)
    end;
elem_to_index(Elem, [], _) ->
    erlang:error({not_found, Elem}).

app_dir_test(Dir1, Dir2) ->
    {Name1, Vsn1, Parent1} = split_app_dir(Dir1),
    {Name2, Vsn2, Parent2} = split_app_dir(Dir2),
    if
	Name1 < Name2 -> true;
	Name1 > Name2 -> false;
	Vsn1 < Vsn2 -> false;
	Vsn1 > Vsn2 -> true;
	Parent1 < Parent2 -> true;
	true -> false
    end.

split_app_dir(Dir) ->
    ParentDir = filename:dirname(Dir),
    Base = filename:basename(Dir),
    {Name, Vsn} = split_app_name(Base),
    Vsn2 = 
	try
	    [list_to_integer(N) || N <- string:tokens(Vsn, ".")]
	catch
	    _:_ ->
		Vsn
	end,
    {Name, Vsn2, ParentDir}.

get_item(ListCtrl) ->
    case wxListCtrl:getItemCount(ListCtrl) of
	0 ->
	    undefined;
	_ ->
	    case wxListCtrl:getNextItem(ListCtrl,
					-1,
					[{geometry, ?wxLIST_NEXT_ALL},
					 {state, ?wxLIST_STATE_SELECTED}]) of
		-1 ->
		    ItemNo = wxListCtrl:getTopItem(ListCtrl),
		    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
			"" ->
			    undefined;
			Text ->
			    {ItemNo, Text}
		    end;
		ItemNo ->
		    Text = wxListCtrl:getItemText(ListCtrl, ItemNo),
		    {ItemNo, Text}
	    end
    end.

get_items(ListCtrl) ->
    case wxListCtrl:getItemCount(ListCtrl) of
	0 ->
	    [];
	Count ->
	    case get_selected_items(ListCtrl, -1, []) of
		[] ->
		    ItemNo = wxListCtrl:getTopItem(ListCtrl),
		    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
			"" ->
			    [];
			Text when Text =/= ?MISSING_APP_TEXT ->
			    [{ItemNo, Text}];
			_MissingText when Count > 1 ->
			    case wxListCtrl:getItemText(ListCtrl, ItemNo + 1) of
				"" ->
				    [];
				Text ->
				    [{ItemNo, Text}]
			    end;
			_MissingText ->
			    []
		    end;
		Items ->
		    Items
	    end
    end.

get_selected_items(ListCtrl, PrevItem, Acc) ->
    case wxListCtrl:getNextItem(ListCtrl,
                                PrevItem,
                                [{geometry, ?wxLIST_NEXT_ALL},
                                 {state, ?wxLIST_STATE_SELECTED}]) of
        -1 ->
	    Acc;
        ItemNo ->
	    case wxListCtrl:getItemText(ListCtrl, ItemNo) of
		Text when Text =/= ?MISSING_APP_TEXT ->
		    get_selected_items(ListCtrl, ItemNo, [{ItemNo, Text} | Acc]);
		_Text ->
		    get_selected_items(ListCtrl, ItemNo, Acc)
	    end
    end.

select_items(_ListCtrl, _OldItems, []) ->
    %% No new items. Nothing to select.
    false;
select_items(ListCtrl, [], Items) ->
    %% No old selection. Select first.
    select_item(ListCtrl, Items);
select_items(ListCtrl, _OldItems, [Item]) ->
    %% Only one new item. Select it.
    select_item(ListCtrl, [Item]);
select_items(ListCtrl, OldItems, NewItems) ->
    %% Try to propagate old selection to new items.
    Filter =
	fun({_OldItemNo, Text}) ->
		case lists:keysearch(Text, 2, NewItems) of
		    {value, Item} -> {true, Item};
		    false -> false
		end
	end,
    case lists:zf(Filter, OldItems) of
	[] ->
	    %% None of the old selections are valid. Select the first.
	    select_item(ListCtrl, NewItems);
	ValidItems ->
	    %% Some old selections are still valid. Select them again.
	    lists:foreach(fun(Item) -> select_item(ListCtrl, [Item]) end, ValidItems)
    end.

select_item(ListCtrl, [{ItemNo, Text} | Items]) ->
    case Text =:= ?MISSING_APP_TEXT of
	true ->
	    select_item(ListCtrl, Items);
	false ->
	    StateMask = ?wxLIST_STATE_SELECTED,
	    State = wxListCtrl:getItemState(ListCtrl, ItemNo, StateMask),
	    State2 = State bor ?wxLIST_STATE_SELECTED,
	    wxListCtrl:setItemState(ListCtrl, ItemNo, State2, StateMask),
	    wxListCtrl:refreshItem(ListCtrl, ItemNo)
    end;
select_item(_ListCtrl, []) ->
    ok.

safe_keysearch(Key, Pos, List, Mod, Line) ->
    case lists:keysearch(Key, Pos, List) of
        false ->
            io:format("~p(~p): lists:keysearch(~p, ~p, ~p) -> false\n",
                      [Mod, Line, Key, Pos, List]),
            erlang:error({Mod, Line, lists, keysearch, [Key, Pos, List]});
        {value, Val} ->
            Val
    end.

print(X, X, Format, Args) ->
    io:format(Format, Args);
print(_, _, _, _) ->
    ok.

%% -define(SAFE(M,F,A), safe(M, F, A, ?MODULE, ?LINE)).
%% 
%% safe(M, F, A, Mod, Line) ->
%%     case catch apply(M, F, A) of
%%      {'EXIT', Reason} ->
%%          io:format("~p(~p): ~p:~p~p -> ~p\n", [Mod, Line, M, F, A, Reason]),
%%          timer:sleep(infinity);
%%      Res ->
%%          Res
%%     end.

return_first_error(Status, NewError) when is_list(NewError) ->
    case Status of
	{ok, _Warnings} ->
	    {error, NewError};
	{error, OldError} ->
	    {error, OldError}
    end.
    
add_warning(Status, Warning) ->
    case Status of
	{ok, Warnings} ->
	    {ok, [Warning | Warnings]};
	{error, Error} ->
	    {error, Error}
    end.
