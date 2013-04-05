%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Talkerapp transport root supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(talker_app_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_talkerapp_client/5]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start new talkerapp client
%% @end
-spec start_talkerapp_client(Callback :: pid(), BotNick :: binary(), Room :: binary(), Token :: binary(), RecTimeout :: integer()) 
                             -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_talkerapp_client(Callback, BotNick, Room, Token, RecTimeout) ->
    % Child
    Child = {talker_app_client, 
                {talker_app_client, start_link, [Callback, BotNick, Room, Token, RecTimeout]},
                 temporary, 2000, worker, []
             },
    % run new talkerapp client
    supervisor:start_child(?MODULE, Child).

init([]) ->
    % init
    {ok,{{one_for_one, 2, 60}, []}}.
