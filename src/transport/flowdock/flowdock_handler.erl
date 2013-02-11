%%%----------------------------------------------------------------------
%%% File    : ../transport/flowdock/flowdock_handler.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot flowdock incoming message handler.
%%%----------------------------------------------------------------------
-module(flowdock_handler).

-behaviour(gen_server).
 
-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        % bot nick in handler room
        flowdock_nick = <<>> :: binary(),
        % flowdock client process pid
        flowdock_client_pid :: pid()
    }).
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({flowdock_client, ClientPid, Login}, State) ->
    {noreply, State#state{flowdock_client_pid = ClientPid, flowdock_nick = Login}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, IncomingMessage}, State) ->
    % Get Ybot Nick from current chat
    Nick = binary_to_list(State#state.flowdock_nick),
    % Get message body
    Message = string:tokens(binary_to_list(IncomingMessage), " \r\n"),
    % Check this is message for Ybot or not
    case Message of
        [Nick] ->
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "What?"});
        [Nick, "hi"] ->
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "Hello :)"});
        [Nick, "bye"] ->    
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "Good bue"});
        [Nick, "history"] ->
            % Get history
            History = gen_server:call(ybot_history, {get_history, State#state.flowdock_client_pid}),
            % Send history
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", History});
        [Nick, "plugins?"] ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            % Send plugins label
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "Plugins:"}),
            % Make plugins list
            lists:foreach(fun(Plugin) ->
                              {_, _, Pl, _} = Plugin,
                              gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "Plugin: " ++ Pl ++ "\r\n"})
                          end, 
                          Plugins),
            gen_server:cast(State#state.flowdock_client_pid, {send_message, "", "That's all :)"});
        [Nick, Command | Arg] ->
            Args = case Arg of
                    [] ->
                        % return empty args
                        "";
                    _ ->
                        % Get command arguments
                        ybot_utils:split_at_end(binary_to_list(IncomingMessage), Command)
                    end,
            % Start process with supervisor which will be execute plugin and send to pid
            ybot_actor:start_link(State#state.flowdock_client_pid, "", Command, Args);
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