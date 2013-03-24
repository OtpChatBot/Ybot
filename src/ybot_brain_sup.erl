%%%----------------------------------------------------------------------
%%% File    : ybot_brain_sup.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain HTTP API supervisor.
%%%----------------------------------------------------------------------
-module(ybot_brain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ======================================================================
%% API functions
%% ======================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
