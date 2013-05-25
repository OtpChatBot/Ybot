%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot smtp client process supervisor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_mail_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_smtp_client/4]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
%% @doc Start new smtp client
%% @end
-spec start_smtp_client(From :: binary(), FromPassword :: binary(), To :: [binary()], Options :: [{atom(), any()}]) 
                        -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_smtp_client(From, FromPassword, To, Options) ->
    supervisor:start_child(?MODULE, [From, FromPassword, To, Options]).

init([]) ->
    ChildSpec = [

        {ybot_mail_client, 
            {ybot_mail_client, start_link, []},
             temporary, 2000, worker, []
        }
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.