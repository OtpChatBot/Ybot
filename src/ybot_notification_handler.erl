%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot notification handler.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_notification_handler).
 
-behaviour(gen_server).
 
-export([start_link/2]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {
        % transports for notifications
        transports = [],
        % plugin language
        lang = "",
        % plugin file path
        plugin_path = "",
        % notification timeout
        timeout = 0
    }).
 
start_link(Notification, NotificationDir) ->
    gen_server:start_link(?MODULE, [Notification, NotificationDir], []).
 
init([Notification, NotificationDir]) ->
    % some logs
    lager:info("Start notification for ~p", [Notification]),
    % init handler
    gen_server:cast(self(), {init_handler, Notification, NotificationDir}),
    % return
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({init_handler, {PluginName, TranportsList, Timeout}, NotificationDir}, State) ->
    % Get runned transports
    RunnedTransports = lists:flatten(lists:map(fun(Transp) ->
                                                   % Check is notification for this transport or not 
                                                   IsTransport = lists:member(element(1, Transp), TranportsList),
                                                   case IsTransport of
                                                       true ->
                                                           element(2, Transp);
                                                       false ->
                                                           []
                                                   end
                                               end,
                                               gen_server:call(ybot_manager, get_transports))),

    % Get necessary channels
    Channels = lists:flatten(lists:map(fun(Channel) ->
                                           % Check is notification for this channel or not 
                                           IsChannel = lists:member(element(1, Channel), TranportsList),
                                           case IsChannel of
                                               true ->
                                                   element(2, Channel);
                                               false ->
                                                   []
                                            end
                                       end,
                                       gen_server:call(ybot_manager, get_channels))),
    % Make transports list
    Transports = lists:append(RunnedTransports, Channels),
    % Get plugin file with extension
    [PluginWithExt | _] = filelib:wildcard(atom_to_list(PluginName) ++ "*", NotificationDir),
    % Get lang
    {plugin, Lang, Plugin, PluginWithExt} = ybot_manager:load_plugin(PluginWithExt),
    % Get full plugin path
    PluginPath = NotificationDir ++ PluginWithExt,
    % start notifications
    erlang:send_after(Timeout * 1000, self(), execute),
    % return
    {noreply, State#state{transports = Transports, lang = Lang, timeout = Timeout * 1000, plugin_path = PluginPath}};
 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(execute, State) ->
    % execute plugin
    Result = os:cmd(State#state.lang ++ " " ++ State#state.plugin_path),
    % send notification to the chats
    lists:foreach(fun(Transport) ->
                      % send message
                      gen_server:cast(Transport, {send_message, "", Result})
                  end, 
                  State#state.transports),
    % start new notification
    erlang:send_after(State#state.timeout, self(), execute),
    % return
    {noreply, State};
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions