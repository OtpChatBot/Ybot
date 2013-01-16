%%%----------------------------------------------------------------------
%%% File    : transport/xmpp/xmpp_sup.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Xmpp root supervisor
%%%----------------------------------------------------------------------
-module(xmpp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_xmpp_client/6]).

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
                        Room :: binary(), 
                        Resource :: binary()) 
                       -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_xmpp_client(CallbackModule, Login, Password, Server, Room, Resource) ->
    % run new irc client
    supervisor:start_child(?MODULE, [CallbackModule, Login, Password, Server, Room, Resource]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Supervisor childs
    Childs = [
        % start xmpp client 
        {xmpp_client, 
            {xmpp_client, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init and start
    {ok, { {simple_one_for_one, 5, 10}, Childs} }.
