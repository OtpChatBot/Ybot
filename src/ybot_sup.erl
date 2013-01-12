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
    	
        % run irc root supervisor
        {irc_lib_sup,
            {irc_lib_sup, start_link, []},
            permanent, brutal_kill, supervisor, []
        },
        
    	% start manager with transports list
    	{ybot_manager, 
    		{ybot_manager, start_link, [PluginsDirectory, Transports]},
    		 permanent, brutal_kill, worker, []
    	}
    ],

    % start
    {ok, { {one_for_one, 5, 10}, Childrens} }.