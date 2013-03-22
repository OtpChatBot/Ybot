%%%----------------------------------------------------------------------
%%% File    : ../transport/http/http_sup.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot http root supervisor.
%%%----------------------------------------------------------------------
-module(http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_http/2]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start http server
%% @end
-spec start_http(Host :: binary(), 
                 Port :: integer()) 
                -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_http(Host, Port) ->
    % run http server
    supervisor:start_child(?MODULE, [Host, Port]).

init([]) ->
    % start ranch application
    application:start(ranch),
    % start cowboy application
    application:start(cowboy),

    % http server child process
    ChildSpec = [

        {http_server,
            {http_server, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.