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
                       Channel :: binary(), 
                       Nick :: binary(),
                       UseSsl :: boolean()) 
                       -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_irc_client(CallbackModule, Host, Port, Channel, Nick, UseSsl) ->
    % Check use ssl or not
    case UseSsl of
      false ->
            % Irc client without ssl
            Child = {irc_lib_client, 
                        {irc_lib_client, start_link, [CallbackModule, Host, Port, Channel, Nick]},
                         temporary, 2000, worker, []
                     },
            % run new irc client
            supervisor:start_child(?MODULE, Child);
      true ->
          % ssl child spec
          Child = {irc_ssl_client, 
                      {irc_ssl_client, start_link, [CallbackModule, Host, Port, Channel, Nick]},
                       temporary, 2000, worker, []
                  },
          % Start irc client with ssl
          supervisor:start_child(?MODULE, Child)
    end.

init([]) ->
    % init
    {ok,{{one_for_one, 2, 60}, []}}.