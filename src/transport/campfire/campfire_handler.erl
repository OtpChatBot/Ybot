-module(campfire_handler).

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
    campfire_nick,
    campfire_client_pid
    }).
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({campfire_client, ClientPid, Login}, State) ->
    {noreply, State#state{campfire_client_pid = ClientPid, campfire_nick = Login}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, IncomingMessage}, State) ->
    Nick = binary_to_list(State#state.campfire_nick),
    % Decode json message
    {struct, DataList} = mochijson2:decode(IncomingMessage),
    {_, Body} = lists:keyfind(<<"body">>, 1, DataList),
    % Get message body
    Message = string:tokens(binary_to_list(Body), " \r\n"),
    % Check this is message for Ybot or not
    case Message of
        [Nick] ->
            gen_server:cast(State#state.campfire_client_pid, {send_message, "What?"});
        [Nick, "hi"] ->
            gen_server:cast(State#state.campfire_client_pid, {send_message, "Hello :)"});
        [Nick, "bye"] ->    
            gen_server:cast(State#state.campfire_client_pid, {send_message, "Good bue"});
        [Nick, "plugins?"] ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_all_plugins),
            % Send plugins label
            gen_server:cast(State#state.campfire_client_pid, {send_message, "Plugins:"}),
            % Make plugins list
            lists:foreach(fun(Plugin) ->
                              {_, _, Pl, _} = Plugin,
                              gen_server:cast(State#state.campfire_client_pid, {send_message, "Plugin:" ++ Pl ++ "\r\n"})
                          end, 
                          Plugins),
            gen_server:cast(State#state.campfire_client_pid, {send_message, "That's all :)"});
        [Nick, Command | Args] ->
            case Args of
                [] ->
                    % Start process with supervisor which will be execute plugin and send to pid
                    ybot_actor:start_link(State#state.campfire_client_pid, Command, Args);
                _ ->
                    % Check that args not consist wrong symbols
                    case re:run(Args, "^[-+*/%()A_Za-z0-9\s]+$") of
                        nomatch ->
                            gen_server:cast(State#state.campfire_client_pid, {send_message, "Hey, there is wrong symbols in your request"});
                        _ ->    
                            % Start process with supervisor which will be execute plugin and send to pid
                            ybot_actor:start_link(State#state.campfire_client_pid, Command, Args)
                    end
            end;
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