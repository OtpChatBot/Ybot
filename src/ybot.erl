-module(ybot).

-export([start/0]).

-spec start() -> ok.
start() ->
    application:start(ybot).
