-module(ybot_manager).

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
       % ybot transports list (irc, xmpp and etc..)
       % Example : [{irc, ClientPid, HandlerPid, Nick, Channel, Host}]
       transports = [],
       % Ybot active plugins list
       plugins = [] :: [{plugin, Source :: string(), PluginName :: string(), Path :: string()}]
    }).
 
start_link(PluginsDirectory, Transports) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Transports], []).
 
init([PluginsDirectory, Transports]) ->
    % init plugins
    ok = gen_server:cast(?MODULE, {init_plugins, PluginsDirectory}),
    % Start transports
    ok = gen_server:cast(?MODULE, {start_transports, Transports}),
    % init
    {ok, #state{}}.

handle_call({get_plugin, PluginName}, _From, State) ->
    case lists:keyfind(PluginName, 3, State#state.plugins) of
        false ->
            % there is no plugin with `PluginName`
            {reply, wrong_plugin, State};
        Plugin ->
            % return plugin with metadata
            {reply, Plugin, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Init active plugins
handle_cast({init_plugins, PluginsDirectory}, State) ->
    % Get all plugins
    Plugins = ybot_utils:get_all_files(PluginsDirectory),
    % Parse plugins
    PluginsList = lists:flatten(
                    lists:map(fun(Plugin) -> 
                                % Get plugin extension
                                Ext = filename:extension(Plugin),
                                % Match extension
                                case Ext of
                                    ".py" ->
                                        % python plugin
                                        {plugin, "python", filename:basename(Plugin, ".py"), Plugin};
                                    ".rb" ->
                                        % ruby plugin
                                        {plugin, "ruby", filename:basename(Plugin, ".rb"), Plugin};
                                    ".sh" ->
                                        % shell plugin
                                        {plugin, "shell", filename:basename(Plugin, ".sh"), Plugin};
                                    _ ->
                                        % this is wrong plugin
                                        []
                                end
                            end, 
                            Plugins)),
    % init plugins
    {noreply, State#state{plugins = PluginsList}};

%% @doc Run transports from `Transports` list
handle_cast({start_transports, Transports}, State) ->
    % Review supported mode of transportation
    TransportList 
        = lists:flatten(
            lists:map(fun(Trans) -> 
                          case element(1, Trans) of
                              irc ->
                                  % Get irc params
                                  {irc, Nick, Channel, Host} = Trans,
                                  % Start handler
                                  {ok, HandlerPid} = irc_handler:start_link(),
                                  % Run new irc client
                                  {ok, ClientPid} = irc_lib_sup:start_irc_client(HandlerPid, Host, Channel, Nick),
                                  % send client pid to handler
                                  ok = gen_server:cast(HandlerPid, {irc_client, ClientPid}),
                                  % return correct transport
                                  {irc, ClientPid, HandlerPid, Nick, Channel, Host};
                              _ ->
                                  []
                          end  
                      end, 
                      Transports)),
    % return
    {noreply, State#state{transports = TransportList}};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions