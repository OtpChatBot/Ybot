%%%----------------------------------------------------------------------
%%% File    : ybot_app.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot application.
%%%----------------------------------------------------------------------
-module(ybot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ybot_sup:start_link().

stop(_State) ->
    ok.
