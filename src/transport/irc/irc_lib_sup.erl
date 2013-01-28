%%%----------------------------------------------------------------------
%%% File    : transport/irc/irc_lib_sup.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Irc transport root supervisor.
%%%----------------------------------------------------------------------
-module(irc_lib_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_irc_client/6]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start new irc client
%% @end
-spec start_irc_client(CallbackModule :: atom() | pid(), 
                       Host :: binary(),
                       Port :: integer(),
                       Channel :: {binary(), binary()}, 
                       Nick :: binary(),
                       UseSsl :: boolean()) 
                       -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_irc_client(CallbackModule, Host, Port, Channel, Nick, UseSsl) ->
    % Check use ssl or not
    SocketMod = case UseSsl of
      true -> ssl;
      false -> gen_tcp
    end,
    Child = {irc_lib_client, 
                {irc_lib_client, start_link, [CallbackModule, Host, Port, SocketMod, Channel, Nick]},
                 temporary, 2000, worker, []
             },
    % run new irc client
    supervisor:start_child(?MODULE, Child).

init([]) ->
    % init
    {ok,{{one_for_one, 2, 60}, []}}.
