%%%----------------------------------------------------------------------
%%% File    : ybot.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Start Ybot.
%%%----------------------------------------------------------------------
-module(ybot).

-export([start/0]).

-spec start() -> ok.
start() ->
    application:start(ybot).
