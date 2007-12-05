-module(img).

-export([do/0]).

do() ->
    Im = egd:create(200,200),
    White = egd:color(Im, white),
    Green = egd:color(Im, green),
    Blue = egd:color(Im, blue),
    Red = egd:color(Im, red),
    Black = egd:color(Im, black),
    Yellow = egd:color(Im, {255,255,0}),

    egd:fill(Im, {10,10}, White),

    % Line and fillRectangle

    egd:filledRectangle(Im, {20,20}, {180,180}, Red),
    egd:line(Im, {0,0}, {200,200}, Black),    

    egd:save(egd:image(Im, gif), "/home/egil/test1.gif"),

    % Fill border

    egd:fill(Im, {15,5}, Blue),
    egd:fill(Im, {115,100}, Green),
    
    egd:save(egd:image(Im, gif), "/home/egil/test2.gif"),

    % Pacman filledArc
    egd:filledArc(Im, {100,100}, 100,100, 28,332, Yellow, [arc]), 
    egd:filledArc(Im, {100,100}, 100,100, 28,332, Black, [arc, no_fill, edged]), 
    
    egd:save(egd:image(Im, gif), "/home/egil/test3.gif"),
    
    % Text
    {W,H} = egd:fontSize(Im, giant),
    String = "egd says hello!",
    Length = length(String),
    egd:text(Im, giant, {round(100 - W*Length/2), 200 - H - 5}, String, Black), 
    egd:save(egd:image(Im, gif), "/home/egil/test4.gif"),

    egd:destroy(Im).

