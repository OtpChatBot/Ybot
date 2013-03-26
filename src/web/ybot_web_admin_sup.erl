%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot web admin root supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_web_admin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_web_admin/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start http server
%% @end
-spec start_web_admin() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_web_admin() ->
    % run http server
    supervisor:start_child(?MODULE, []).

init([]) ->
    % Check ranch and cowboy
    case whereis(ranch) of
        undefined ->
            % start ranch application
            application:start(ranch),
            % start cowboy application
            application:start(cowboy);
        _ ->
            ok
    end,

    % http server child process
    ChildSpec = [

        {web_admin,
            {web_admin, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.