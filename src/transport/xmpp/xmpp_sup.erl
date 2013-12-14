%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Xmpp root supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(xmpp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_xmpp_client/10]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start new xmpp muc client
-spec start_xmpp_client(CallbackModule :: atom() | pid(), 
                        Login :: binary(),
                        Password :: binary(),
                        Server :: binary(),
                        Port :: integer(),
                        Room :: binary(),
                        Nick :: binary(),
                        Resource :: binary(),
                        UseSsl :: boolean(),
                        ReconnectTimeout :: integer()) 
                       -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_xmpp_client(CallbackModule, Login, Password, Server, Port, Room, Nick, Resource, UseSsl, ReconnectTimeout ) ->
    % Match socket mode
    SocketMode = case UseSsl of
                    true -> 
                        ssl;
                    false ->
                        gen_tcp
                 end,
    % Xmpp child
    Child = {xmpp_client, 
                {xmpp_client, start_link, [CallbackModule, Login, Password, Server, Port, Room, Nick, Resource, SocketMode, ReconnectTimeout]},
                temporary, 2000, worker, []
            },

    % run new xmpp client
    supervisor:start_child(?MODULE, Child).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    % init and start
    {ok,{{one_for_one, 2, 60}, []}}.
