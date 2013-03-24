%%%-----------------------------------------------------------------------------
%%% @author tgrk <tajgur@gmail.com>
%%% @doc
%%% Ybot brain HTTP API supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(ybot_brain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%======================================================================
%% API functions
%%======================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%======================================================================
%% Supervisor callback
%%======================================================================

init([]) ->

    % Get brain backend storage
    {ok, BrainStorage} = application:get_env(ybot, brain_storage),

    % brain API configuration
    {ok, ApiHost} = application:get_env(ybot, brain_api_host),
    {ok, ApiPort} = application:get_env(ybot, brain_api_port),

    ChildSpec = [
        {ybot_brain,
            {ybot_brain, start_link, [BrainStorage]},
             permanent, 2000, worker, []
        },

        {ybot_brain_api_server,
            {ybot_brain_api_server, start_link, [ApiHost, ApiPort]},
             permanent, 2000, worker, []
        }
    ],

    {ok,{{one_for_one, 10, 60}, ChildSpec}}.
