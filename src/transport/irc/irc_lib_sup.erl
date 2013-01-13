%%%----------------------------------------------------------------------
%%% File    : transport/irc/irc_lib_sup.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Irc transport root supervisor.
%%%----------------------------------------------------------------------
-module(irc_lib_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_irc_client/4]).

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
	                   Channel :: binary(), 
	                   Nick :: binary()) 
                       -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_irc_client(CallbackModule, Host, Channel, Nick) ->
    % run new irc client
    supervisor:start_child(?MODULE, [CallbackModule, Host, Channel, Nick]).

init([]) ->
	% irc client
    ChildSpec = [
        % start irc client
    	{irc_lib_client, 
   			{irc_lib_client, start_link, []},
    	 	 temporary, 2000, worker, []
    	}
    ],

    % init
    {ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.