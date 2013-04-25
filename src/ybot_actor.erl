%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot main plugin executor.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_actor).

-behaviour(gen_server).

-export([start_link/4, stop/0, handle_command/4]).

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

%%=============================================================================
%% API functions
%%=============================================================================

start_link(TransportPid, From, Command, Args) ->
    gen_server:start_link(?MODULE, [TransportPid, From, Command, Args], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%=============================================================================
%% ybot_actor callbacks
%%=============================================================================

init([TransportPid, From, Command, Args]) ->
    % execute plugin
    gen_server:cast(self(), {execute, TransportPid, From, Command, Args}),
    % init
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Try to execute command
handle_cast({execute, TransportPid, From, Command, Args}, State) ->
    try
        % Log
        lager:info("Command: ~s, ~p", [Command, Args]),
        % Handle received command
        handle_command(From, Command, Args, TransportPid),
        % stop actor
        stop()
    catch
        _:Reason ->
            lager:error("Unable to execute plugin! Error=~p, Stack=~p",
                        [Reason, erlang:get_stacktrace()])
    end,
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

%%=============================================================================
%% Internal functions
%%=============================================================================

%% @doc Try to find plugin and execute it
-spec handle_command(string(), cmd(), cmd_args(), transport()) -> ok | pass.
handle_command(From, Command, Args, TransportPid) ->
    % Get plugin metadata
    TryToFindPlugin = gen_server:call(ybot_manager, {get_plugin, Command}),
    % Check plugin
    case TryToFindPlugin of
        wrong_plugin ->
            % plugin not found
            gen_server:cast(TransportPid, {send_message, From, "Sorry, but i don't know about this :("});
        {plugin, "erlang", PluginName, AppModule} ->
            % execute plugin
            Result = AppModule:execute(Args),
            % Save command to history
            ok = gen_server:cast(ybot_history, {update_history, TransportPid, "Ybot " ++ PluginName ++ " " ++ Args ++ "\n"}),
            % send result to chat
            gen_server:cast(TransportPid, {send_message, From, Result});
        {plugin, Lang, _PluginName, PluginPath} ->
            % execute plugin
            Cmd = Lang ++ " " ++ PluginPath ++ os_escape(Args),
            %%lager:info("Exec: ~p", [Cmd]),
            Result = os:cmd(Cmd),
            % Save command to history
            ok = gen_server:cast(ybot_history, {update_history, TransportPid, "Ybot " ++ Command ++ " " ++ Args ++ "\n"}),
            % send result to chat
            gen_server:cast(TransportPid, {send_message, From, Result})
    end.


os_escape([]) ->
    [];
os_escape(Args) ->
    os_escape(Args, [$",$ ]).

os_escape([], Acc) ->
    lists:reverse([$"|Acc]);
os_escape([$"|Args], Acc) ->
    os_escape(Args, [$",$\\|Acc]);
os_escape([C|Args], Acc) ->
    os_escape(Args, [C|Acc]).
