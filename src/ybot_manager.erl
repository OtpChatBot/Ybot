%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot main manager. Run transport, load plugins.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_manager).

-behaviour(gen_server).

-export([start_link/2, load_plugin/1, run_transport/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% Internal state
-record(state, {
       % ybot transports list (irc, xmpp and etc..)
       % Example : [{irc, ClientPid, HandlerPid, Nick, Channel, Host}]
       transports = [],
       % Ybot active plugins list
       plugins = [] :: [{plugin, Source :: string(), PluginName :: string(), Path :: string()}],
       % Runned transports pid list
       runned_transports = [] :: [pid()],
       % plugins paths
       plugins_paths = []
    }).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(PluginsDirectory, Transports) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Transports], []).

%%%=============================================================================
%%% ybot_manager callbacks
%%%=============================================================================

init([PluginsDirectory, Transports]) ->
    % init plugins
    ok = gen_server:cast(?MODULE, {init_plugins, PluginsDirectory}),
    % Start transports
    ok = gen_server:cast(?MODULE, {start_transports, Transports}),
    % init command history process
    ok = gen_server:cast(?MODULE, init_history),
    % start webadmin interface
    ybot_web_admin_sup:start_web_admin(),
    % init
    {ok, #state{}}.

%% @doc Get plugin metadata by plugin name
handle_call({get_plugin, PluginName}, _From, State) ->
    case lists:keyfind(PluginName, 3, State#state.plugins) of
        false ->
            % there is no plugin with `PluginName`
            {reply, wrong_plugin, State};
        Plugin ->
            % return plugin with metadata
            {reply, Plugin, State}
    end;

%% @doc get transports info
handle_call(get_transports, _From, State) ->
    {reply, State#state.transports, State};

%% @doc get all runned transports pid
handle_call(get_runnned_transports, _From, State) ->
    % Return all runned transports
    {reply, State#state.runned_transports, State};

%% @doc Return all plugins
handle_call(get_plugins, _From, State) ->
    % Return all plugins
    {reply, State#state.plugins, State};

handle_call(get_plugins_paths, _From, State) ->
    {reply, State#state.plugins_paths, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Init command history process
handle_cast(init_history, State) ->
    % Get 
    NeedCommandHistory = case application:get_env(ybot, commands_history) of
                            {ok, true} ->
                                true;
                            {ok, false} ->
                                false;
                            _ ->
                                false
                         end,
    % Check need command history or not
    case NeedCommandHistory of
        true ->
            % Get history limit
            {ok, HistoryLimit} = application:get_env(ybot, history_command_limit_count),
            % start history process
            ybot_history:start_link(HistoryLimit);
        _ ->
            % do nothing
            ok
    end,
    % return
    {noreply, State};

%% @doc update plugins
handle_cast({update_plugins, NewPlugins}, State) ->
    % save new plugins
    {noreply, State#state{plugins = lists:flatten([NewPlugins | State#state.plugins])}};

%% @doc Init active plugins
handle_cast({init_plugins, PluginsDirectory}, State) ->
    case filelib:is_dir(PluginsDirectory) of
        true ->
            % Get all plugins
            PluginsPaths = lists:append(
                             ybot_utils:get_all_files(PluginsDirectory),
                             ybot_utils:get_all_directories(PluginsDirectory)
                            ),
            % Parse plugins and load to state
            Plugins = lists:flatten(lists:map(fun load_plugin/1, PluginsPaths)),
            % observe new plugins after start
            ok = ybot_plugins_observer:observe_new_plugins(PluginsDirectory, PluginsPaths),
            % return plugins
            {noreply, State#state{plugins = Plugins, plugins_paths =  PluginsPaths}};
        false ->
            % some log
            lager:error("Unable to load plugins. Invalid directory ~s", [PluginsDirectory]),
            % return empty plugins list
            {noreply, State#state{plugins = []}}
    end;

%% @doc Run transports from `Transports` list
handle_cast({start_transports, Transports}, State) ->
    % Review supported mode of transportation
    TransportList = lists:flatten(lists:map(fun load_transport/1, Transports)),
    % Get runned transports pid list
    RunnedTransport = lists:flatten(lists:map(fun(Transport) -> 
                                                  if erlang:element(1, Transport) == http ->
                                                      % we no need in http transport
                                                      [];
                                                  true ->
                                                      erlang:element(2, Transport) 
                                                  end 
                                              end, TransportList)),
    % Init transports
    {noreply, State#state{transports = TransportList, runned_transports = RunnedTransport}};

%% @doc add new runned transport
handle_cast({update_transport, NewTransport, NewTransportPid}, State) ->
    % update transports
    {noreply, State#state{transports = lists:flatten([NewTransport | State#state.transports]), 
                          runned_transports = lists:flatten([NewTransportPid | State#state.runned_transports])}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @doc Start irc client
load_transport({irc, Nick, Channel, Host, Options}) ->
    % Validate transport options
    case ybot_validators:validate_transport_opts(Options) of
        ok ->
            % Get irc server port
            {port, Port} = lists:keyfind(port, 1, Options),
            % SSL?
            {use_ssl, UseSsl} = lists:keyfind(use_ssl, 1, Options),
            % Get reconnect timeout
            {reconnect_timeout, ReconnectTimeout} = lists:keyfind(reconnect_timeout, 1, Options),
            % Start irc handler
            {ok, HandlerPid} = irc_handler:start_link(),
            % Run new irc client
            {ok, ClientPid} = irc_lib_sup:start_irc_client(HandlerPid, Host, Port, Channel, Nick, UseSsl, ReconnectTimeout),
            % Start parser process
            {ok, ParserPid} = ybot_parser:start_link(),
            % Log
            lager:info("Starting IRC transport: ~p, ~p, with nick: ~s", [Host, Channel, Nick]),
            % send client pid to handler
            ok = gen_server:cast(HandlerPid, {irc_client, ClientPid, ParserPid, Nick}),
            % return correct transport
            {irc, ClientPid, HandlerPid, Nick, Channel, Host, Port};
        % wrong transport
        _ ->
            []
    end;

%% @doc start xmpp client
load_transport({xmpp, Login, Password, Room, Host, Resource, Options}) ->
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),
    % Validate transport options
    case ybot_validators:validate_transport_opts(Options) of
        ok ->
            % Get irc server port
            {port, Port} = lists:keyfind(port, 1, Options),
            % SSL?
            {use_ssl, UseSsl} = lists:keyfind(use_ssl, 1, Options),
            % Get reconnect timeout
            {reconnect_timeout, ReconnectTimeout} = lists:keyfind(reconnect_timeout, 1, Options),
            % Start xmpp handler
            {ok, HandlerPid} = xmpp_handler:start_link(),
            % Make room
            XmppRoom = list_to_binary(binary_to_list(Room) ++ "/" ++ binary_to_list(Login)),
            % Log
            lager:info("Starting XMPP transport: ~s, ~s, ~s", [Host, Room, Resource]),
            % Start new xmpp transport
            {ok, ClientPid} = xmpp_sup:start_xmpp_client(HandlerPid, Login, Password, Host, Port, XmppRoom, Resource, UseSsl, ReconnectTimeout),
            % Send client pid to handler
            ok = gen_server:cast(HandlerPid, {xmpp_client, ClientPid, ParserPid, Login}),
            % return correct transport
            {xmpp, ClientPid, HandlerPid, Login, Password, Host, Room, Resource};
        % wrong options
        _ ->
            []
    end;

%% @doc start hipchat client
load_transport({hipchat, Login, Password, Room, Host, Resource, HipChatNick, Options}) ->
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),
    % HipChat port
    Port = 5223,
    % Use ssl for hipchat
    UseSsl = true,
    % Get reconnect timeout
    {reconnect_timeout, ReconnectTimeout} = lists:keyfind(reconnect_timeout, 1, Options),
    % Start xmpp handler
    {ok, HandlerPid} = xmpp_handler:start_link(),
    % Make room
    XmppRoom = list_to_binary(binary_to_list(Room) ++ "/" ++ binary_to_list(HipChatNick)), 
    % Run new xmpp client
    {ok, ClientPid} = xmpp_sup:start_xmpp_client(HandlerPid, Login, Password, Host, Port, XmppRoom, Resource, UseSsl, ReconnectTimeout),
    % Log
    lager:info("Starting HipChat transport: ~s, ~s, ~s", [Host, Room, Resource]),
    % Send client pid to handler
    ok = gen_server:cast(HandlerPid, {xmpp_client, ClientPid, ParserPid, list_to_binary("@" ++ lists:concat(string:tokens(binary_to_list(HipChatNick), " ")))}),
    % return correct transport
    {hipchat, ClientPid, HandlerPid, Login, Password, Host, Room, Resource};

%% @doc start campfire client
load_transport({campfire, Login, Token, RoomId, CampfireSubDomain, Options}) ->
    % Get reconnect timeout
    {reconnect_timeout, ReconnectTimeout} = lists:keyfind(reconnect_timeout, 1, Options),
    % Start campfire handler
    {ok, HandlerPid} = campfire_handler:start_link(),
    % Run new campfire client
    {ok, ClientPid} = campfire_sup:start_campfire_client(HandlerPid, RoomId, Token, CampfireSubDomain, ReconnectTimeout),
    % Log
    lager:info("Starting Campfire transport: ~p, ~s", [RoomId, CampfireSubDomain]),
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),
    % Send client pid to handler
    ok = gen_server:cast(HandlerPid, {campfire_client, ClientPid, ParserPid, Login}),
    % return correct transport
    {campfire, ClientPid, HandlerPid};

%% @doc Ybot http interface
load_transport({http, Host, Port, BotNick}) ->
    % Start http server
    {ok, HttpPid} = http_sup:start_http(Host, Port),
    % Log
    lager:info("Starting http transport ~p:~p", [Host, Port]),
    % Send bot nick to http server
    ok = gen_server:cast(HttpPid, {bot_nick, BotNick}),
    % return correct transport
    {http, HttpPid};

%% @doc start flowdock client
load_transport({flowdock, NickInChat, Login, Password, FlowdockOrg, Flow}) ->
    % Start flowdock handler
    {ok, HandlerPid} = flowdock_handler:start_link(),
    % Start flowdock client
    {ok, ClientPid} = flowdock_sup:start_flowdock_client(HandlerPid, FlowdockOrg, Flow, Login, Password),
    % Log
    lager:info("Starting flowdock transport ~p:~p", [FlowdockOrg, Flow]),
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),
    % Send client pid to handler
    ok = gen_server:cast(HandlerPid, {flowdock_client, ClientPid, ParserPid, NickInChat}),
    % return correct transport
    {flowdock, ClientPid, HandlerPid};

%% @doc Use skype or not
load_transport({skype, UseSkype, Host, Port}) ->
    % Check use skype or not
    case UseSkype of
        true ->
            % Get skype script from priv dir
            Skype = ybot_utils:get_priv_dir() ++ "skype.py",
            % Skype command
            Command = "python " ++ Skype ++ " " ++ binary_to_list(Host) ++ " " ++ integer_to_list(Port), 
            % Start skype
            skype:start_link(Command),
            % Log
            lager:info("Starting skype ..."),
            % return correct transport
            {skype, UseSkype, Host, Port};
        _ ->
            % do nothing
            []
    end;

%% @doc start talkerapp client
load_transport({talkerapp, Nick, Room, Token}) ->
    % Start handler
    {ok, HandlerPid} = talkerapp_handler:start_link(),
    % Start talker app client
    {ok, ClientPid} = talker_app_sup:start_talkerapp_client(HandlerPid, Nick, Room, Token),
    % Start parser process
    {ok, ParserPid} = ybot_parser:start_link(),
    % Send client pid to handler
    ok = gen_server:cast(HandlerPid, {talkerapp_client, ClientPid, ParserPid, Nick}),
    % Log
    lager:info("Starting talkerapp transport ~p:~p", [Room, Nick]),
    % return correct transport
    {talkerapp, ClientPid, HandlerPid};

load_transport(_) ->
    [].

load_plugin(Plugin) ->
    % Get plugin extension
    Ext = filename:extension(Plugin),
    Name = filename:basename(Plugin, Ext),
    % Match extension
    case Ext of
        ".py" ->
            % python plugin
            lager:info("Loading plugin(Python): ~s", [Name]),
            {plugin, "python", Name, Plugin};
        ".rb" ->
            % ruby plugin
            lager:info("Loading plugin(Ruby): ~s", [Name]),
            {plugin, "ruby", Name, Plugin};
        ".sh" ->
            % shell plugin
            lager:info("Loading plugin(Shell): ~s", [Name]),
            {plugin, "sh", Name, Plugin};
        ".pl" ->
            % perl plugin
            lager:info("Loading plugin(Perl) ~s", [Name]),
            {plugin, "perl", Name, Plugin};
        ".ex" ->
            % elixir plugin
            lager:info("Loading plugin(Elixir) ~s", [Name]),
            {plugin, "elixir", Name, Plugin};
        ".scala" ->
            % scala plugin
            lager:info("Loading plugin(Scala) ~s", [Name]),
            {plugin, "scala", Name, Plugin};
        [] ->
            % Check Erlang/OTP plugin
            case filelib:wildcard(Plugin ++ "/ebin/*.app") of
                [AppFile] ->
                    AppName = list_to_atom(filename:basename(AppFile, ".app")),
                    [_, Name] = string:tokens(Plugin, "/"),
                    lager:info("Loading plugin(Erlang) ~s", [Name]),
                    application:start(AppName),
                    {plugin, "erlang", Name, atom_to_list(AppName)};
                _ ->
                    []
            end;
        _ ->
            % this is wrong plugin
            lager:info("Unsupported plugin type: ~s", [Ext]),
            []
    end.

%% @doc run new transport manualy
run_transport(Transport) ->
    case load_transport(Transport) of
        [] ->
            wrong_transport;
        NewTransport ->
            % update transport
            gen_server:cast(ybot_manager, {update_transport, NewTransport, element(2, NewTransport)})
    end.