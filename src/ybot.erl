%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot entry point.
%%% @end
%%%-----------------------------------------------------------------------------
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
    [compiler, syntax_tools, asn1, lager, inets, crypto, public_key, ssl,
     mnesia, ranch, cowlib, cowboy, jiffy, ibrowse, reloader].
