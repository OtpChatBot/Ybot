%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Ybot memory plugin using OTP application
%%% @end
%%% Created : 20 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(ybot_plugin_memory_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%=============================================================================
%% Application callbacks
%%=============================================================================
start(_StartType, _StartArgs) ->
    ybot_plugin_memory_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
