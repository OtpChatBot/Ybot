%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Ybot memory plugin using OTP supervisor
%%% @end
%%% Created : 20 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(ybot_plugin_memory_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Params),
    {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%%=============================================================================
%% API functions
%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%=============================================================================
%% Supervisor callbacks
%%=============================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
