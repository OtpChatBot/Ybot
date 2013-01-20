%%%----------------------------------------------------------------------
%%% File    : ybot.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Start Ybot.
%%%----------------------------------------------------------------------
-module(ybot).

-export([start/0, stop/0]).

-spec start() -> ok.
start() ->
    [application:start(A) || A <- deps() ++ [ybot]],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- lists:reverse(deps()) ++ [ybot]],
    ok.

%% Internal functions
deps() ->
    [compiler, syntax_tools, lager, ibrowse, reloader].
