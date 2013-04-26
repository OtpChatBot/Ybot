%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot notification process supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_notification_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_notification_proc/2]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start new notification process
%% @end
-spec start_notification_proc({PluginName :: string(), NotificationTimeout :: integer()}, NotificationDir :: string()) -> 
                              {ok, Pid :: pid()} | {error, Reason :: term()}.
start_notification_proc(Notification, NotificationDir) ->
    supervisor:start_child(?MODULE, [Notification, NotificationDir]).

init([]) ->
    ChildSpec = [

        {ybot_notification_handler, 
            {ybot_notification_handler, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.