%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxartprovider.html">wxArtProvider</a>.
%% @type wxArtProvider().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxArtProvider).
-include("wxe.hrl").
-export([getBitmap/1,getBitmap/2,getIcon/1,getIcon/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Id::string()) -> wxBitmap:wxBitmap()
%% @equiv getBitmap(Id, [])
getBitmap(Id)
 when is_list(Id) ->
  getBitmap(Id, []).

%% @spec (Id::string(), [Option]) -> wxBitmap:wxBitmap()
%% Option = {client, string()} | {size, {W::integer(),H::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxartprovider.html#wxartprovidergetbitmap">external documentation</a>.
getBitmap(Id, Options)
 when is_list(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary([Id,0]),
  MOpts = fun({client, Client}, Acc) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C,0]),[<<1:32/?UI,(byte_size(Client_UC)):32/?UI,(Client_UC)/binary, 0:(((8- ((0+byte_size(Client_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc]  end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxArtProvider_GetBitmap,
  <<(byte_size(Id_UC)):32/?UI,(Id_UC)/binary, 0:(((8- ((4+byte_size(Id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (Id::string()) -> wxIcon:wxIcon()
%% @equiv getIcon(Id, [])
getIcon(Id)
 when is_list(Id) ->
  getIcon(Id, []).

%% @spec (Id::string(), [Option]) -> wxIcon:wxIcon()
%% Option = {client, string()} | {size, {W::integer(),H::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxartprovider.html#wxartprovidergeticon">external documentation</a>.
getIcon(Id, Options)
 when is_list(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary([Id,0]),
  MOpts = fun({client, Client}, Acc) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C,0]),[<<1:32/?UI,(byte_size(Client_UC)):32/?UI,(Client_UC)/binary, 0:(((8- ((0+byte_size(Client_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc]  end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxArtProvider_GetIcon,
  <<(byte_size(Id_UC)):32/?UI,(Id_UC)/binary, 0:(((8- ((4+byte_size(Id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

