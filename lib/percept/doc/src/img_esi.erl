-module(img_esi).

-export([image/3]).

image(SessionID, _Env, _Input) ->
    mod_esi:deliver(SessionID, header()),
    Binary = my_image(),
    mod_esi:deliver(SessionID, binary_to_list(Binary)).

my_image() ->
    Im = egd:create(300,20),
    White = egd:color(Im, white),
    Black = egd:color(Im, black),
    Red = egd:color(Im, red),
    egd:fill(Im, {20,10}, White),
    egd:filledRectangle(Im, {30,14}, {270,19}, Red),
    egd:rectangle(Im, {30,14}, {270,19}, Black),
    egd:text(Im, medium, {30, 0}, "egd with esi callback", Black),
    Bin = egd:image(Im, png),
    egd:destroy(Im),
    Bin.

header() ->
    "Content-Type: image/png\r\n\r\n".
