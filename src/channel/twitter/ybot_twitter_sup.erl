%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot twitter client process supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_twitter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_twitter_client/4]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start new twitter client
%% @end
-spec start_twitter_client(Consumer :: binary(), ConsumerSecret :: binary(), AccessToken :: binary(), AccessTokenSecret :: binary()) -> 
                              {ok, Pid :: pid()} | {error, Reason :: term()}.
start_twitter_client(Consumer, ConsumerSecret, AccessToken, AccessTokenSecret) ->
    supervisor:start_child(?MODULE, [Consumer, ConsumerSecret, AccessToken, AccessTokenSecret]).

init([]) ->
    ChildSpec = [

        {ybot_twitter_client, 
            {ybot_twitter_client, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.