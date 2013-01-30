%%%----------------------------------------------------------------------
%%% File    : transport/irc/irc_handler.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Handler of irc messages.
%%%----------------------------------------------------------------------
-module(irc_handler).

-behavior(gen_server).

-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
% state
-record(state, {
        % irc bot nick
        nick = <<>> :: binary(),
        % irc client pid
        irc_client_pid :: pid()
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({irc_client, ClientPid, BotNick}, State) ->
    % save irc client pid
    {noreply, State#state{irc_client_pid = ClientPid, nick = BotNick}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Receive incoming message from irc chat
handle_info({incoming_message, IncomingMessage, From}, State) ->
    Nick = binary_to_list(State#state.nick),
    % Check this is message for Ybot or not
    case string:tokens(IncomingMessage, " \r\n") of
        [Nick] ->
            gen_server:cast(State#state.irc_client_pid, {send_message, From, "What?"});
        [Nick, "hi"] ->
            gen_server:cast(State#state.irc_client_pid, {send_message, From, "Hello :)"});
        [Nick, "bye"] ->
            gen_server:cast(State#state.irc_client_pid, {send_message, From, "Good bue"});
        [Nick, "history"] ->
            % Get history
            History = gen_server:call(ybot_history, {get_history, State#state.irc_client_pid}),
            % Send history
            gen_server:cast(State#state.irc_client_pid, {send_message, From, History});
        [Nick, "plugins?"] ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            % Format plugins
            PluginNames = lists:map(fun({_, _, Pl, _}) -> Pl end, Plugins),
            % Send plugins
            gen_server:cast(State#state.irc_client_pid, {send_message, From, "Plugins: " ++ string:join(PluginNames, ", ")}),
            % That's all :)
            gen_server:cast(State#state.irc_client_pid, {send_message, From, "That's all :)"});
        [Nick, Command | _] ->
                % Get command arguments
                Args = string:tokens(ybot_utils:split_at_end(IncomingMessage, Command), "\r\n"),
                % Start process with supervisor which will be execute plugin and send to pid
                ybot_actor:start_link(State#state.irc_client_pid, From, Command, Args);
        _ ->
            % this is not our command
            pass
    end,
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
