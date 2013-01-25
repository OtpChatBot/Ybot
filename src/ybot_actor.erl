%%%----------------------------------------------------------------------
%%% File    : ybot_actor.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot main plugin executor.
%%%----------------------------------------------------------------------
-module(ybot_actor).

-behaviour(gen_server).

-export([start_link/3, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-type cmd() :: string().
-type cmd_args() :: [string()].
-type transport() :: pid().

-record(state, {}).

start_link(TransportPid, Command, Args) ->
    gen_server:start_link(?MODULE, [TransportPid, Command, Args], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([TransportPid, Command, Args]) ->
    % execute plugin
    gen_server:cast(self(), {execute, TransportPid, Command, Args}),
    % init
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Try to execute command
handle_cast({execute, TransportPid, Command, Args}, State) ->
    % Log
    lager:info("Command: ~s, ~p", [Command, Args]),
    % Handle received command
    handle_command(Command, Args, TransportPid),
    % stop actor
    stop(),
    % return
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions

%% @doc Try to find plugin and execute it
-spec handle_command(cmd(), cmd_args(), transport()) -> ok | pass.
handle_command(Command, Args, TransportPid) ->
    % Get plugin metadata
    TryToFindPlugin = gen_server:call(ybot_manager, {get_plugin, Command}),
    % Check plugin
    case TryToFindPlugin of
        wrong_plugin ->
            % plugin not found
            pass;
        {plugin, Lang, _PluginName, PluginPath} ->
            % execute plugin
            Result = os:cmd(Lang ++ " " ++ PluginPath ++ " \'" ++ Args ++ "\'"),
            % Save command to history
            ok = gen_server:cast(ybot_history, {update_history, TransportPid, "Ybot " ++ Command ++ " " ++ Args ++ "\n"}),
            % send result to chat
            gen_server:cast(TransportPid, {send_message, Result})
    end.
