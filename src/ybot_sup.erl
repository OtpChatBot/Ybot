%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot root supervisor.
%%% @end
%%%-----------------------------------------------------------------------------

-module(ybot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    % Get plugins directory
    {ok, PluginsDirectory} = application:get_env(ybot, plugins_path),

    % Get transports
    {ok, Transports} = application:get_env(ybot, transports),

    % Root supervisor childrens
    Childrens = [

        % start http supervisor
        {http_sup,
            {http_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        {ybot_web_admin_sup,
            {ybot_web_admin_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % start brain http api supervisor
        {ybot_brain_sup,
            {ybot_brain_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % start flowdock supervisor
        {flowdock_sup,
            {flowdock_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % run irc root supervisor
        {irc_lib_sup,
            {irc_lib_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % run xmpp root supervisor
        {xmpp_sup,
            {xmpp_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % run campfire root supervisor
        {campfire_sup,
            {campfire_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % run talkerapp root supervisor
        {talker_app_sup,
            {talker_app_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },

        % start manager with transports list
        {ybot_manager,
            {ybot_manager, start_link, [PluginsDirectory, Transports]},
             permanent, brutal_kill, worker, []
        },

        % start ybot api process
        {ybot,
            {ybot, start_link, []},
            permanent, brutal_kill, worker, []
        }
    ],
    
    % init
    {ok, { {one_for_one, 1, 60}, Childrens} }.
