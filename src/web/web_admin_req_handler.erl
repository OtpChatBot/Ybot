%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot web admin requests handler.
%%% @end
%%%-----------------------------------------------------------------------------
-module(web_admin_req_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%=============================================================================
%% Cowboy handler callback
%%=============================================================================

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    % Get method
    {Method, Req2} = cowboy_req:method(Req),
    % Get query string
    {QS, Req3} = cowboy_req:qs(Req),
    % Check method
    case Method of
        % Get requests
        <<"GET">> ->
            % Parse query string
            case string:tokens(binary_to_list(QS), "=") of
                %
                % Get data for front page
                %
                ["req", "main_web_interface_req"] ->
                    % Get runned transports
                    Transports = [atom_to_list(element(1, Transport))  ++ " " ++
                                  pid_to_list(element(2, Transport))   ++ " " ++ 
                                  binary_to_list(element(4, Transport)) ++ "\n" || Transport <- ybot:get_runned_transports(), element(1, Transport) /= http],
                    % Get plugins
                    PluginsDirectory = {plugins_directory, list_to_binary(ybot:get_plugins_directory())},
                    % Get plugins
                    Plugins = {plugins, format_plugins_helper(ybot:get_plugins())},

                    % Is history using
                    IsHistory = case whereis(ybot_history) of
                                    undefined ->
                                        {is_history, false};
                                    _ ->
                                        {is_history, true}
                                end,
                    % History limit
                    HistoryLimit = case whereis(ybot_history) of
                                    undefined ->
                                        {history_limit, 0};
                                    _ ->
                                        {history_limit, gen_server:call(ybot_history, get_history_limit)}
                                   end,
                    % Get observer
                    IsObserver = case whereis(ybot_plugins_observer) of
                                    undefined ->
                                        {is_observer, false};
                                    _ ->
                                        {is_observer, true}
                                 end,
                    % Get observer timeout
                    ObserverTimeout = case whereis(ybot_plugins_observer) of
                                          undefined ->
                                              {observer_timeout, 0};
                                          _ ->
                                              {observer_timeout, gen_server:call(ybot_plugins_observer, get_observer_timeout)}
                                      end,
                    % Get storage
                    Storage = {storage_type, ybot:get_brain_storage()},
                    % prepare data to json
                    Data = {[
                                {transport, list_to_binary(Transports)},
                                IsHistory,
                                HistoryLimit,
                                PluginsDirectory,
                                Plugins,
                                IsObserver,
                                ObserverTimeout,
                                Storage
                            ]},
                    % Convert to json
                    Json = jiffy:encode(Data),
                    % Send info to Ybot webadmin
                    cowboy_req:reply(200, [], Json, Req3);
                %
                % Get observer and history settings
                %
                ["req", "ybot_plugins_settings"] ->
                    % Using observer or not
                    IsObserver = case whereis(ybot_plugins_observer) of
                                    undefined ->
                                        {is_observer, false};
                                    _ ->
                                        {is_observer, true}
                                 end,
                    % Observer timeout limit
                    ObserverTimeout = case whereis(ybot_plugins_observer) of
                                          undefined ->
                                              {observer_timeout, 0};
                                          _ ->
                                              {observer_timeout, gen_server:call(ybot_plugins_observer, get_observer_timeout)}
                                      end,
                    % Is history using
                    IsHistory = case whereis(ybot_history) of
                                    undefined ->
                                        {is_history, false};
                                    _ ->
                                        {is_history, true}
                                end,
                    % History limit
                    HistoryLimit = case whereis(ybot_history) of
                                    undefined ->
                                        {history_limit, 0};
                                    _ ->
                                        {history_limit, gen_server:call(ybot_history, get_history_limit)}
                                   end,
                    % Prepara data for converting into json
                    Data = {[IsObserver, ObserverTimeout, IsHistory, HistoryLimit]},
                    % Convert into json
                    Json = jiffy:encode(Data),
                    % Send to webadmin
                    cowboy_req:reply(200, [], Json, Req3);
                % Wrong requests
                _ ->
                    cowboy_req:reply(200, [], <<"Wrong request">>, Req3)
            end;
        % Post requests
        <<"POST">> ->
            % Parse query string
            case string:tokens(binary_to_list(QS), "=") of
                %
                % Update observer settings
                %
                ["req", "update_observer_settings"] ->
                    % Get body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle request
                    handle_request(jiffy:decode(Body)),
                    % Send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);    
                %
                % Update history settings
                %
                ["req", "update_history_settings"] ->
                    % Get request body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle request
                    handle_request(jiffy:decode(Body)),
                    % Send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);
                %
                % Upload new plugin
                %
                ["req", "upload_web_plugin"] ->
                    % Get request body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle request from web interface
                    handle_request(jiffy:decode(Body)),
                    % send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);
                %
                % Start irc transport
                %
                ["req", "start_irc"] ->
                    % Get request body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle_request from web interface
                    handle_request(jiffy:decode(Body)),
                    % send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);
                %
                % Start xmpp transport
                %
                ["req", "start_xmpp"] ->
                    % Get request body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle_request from web interface
                    handle_request(jiffy:decode(Body)),
                    % send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);
                %
                % Start campfire
                %
                ["req", "start_campfire"] ->
                    % Get request body
                    {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                    % handle_request from web interface
                    handle_request(jiffy:decode(Body)),
                    % send response
                    cowboy_req:reply(200, [], <<"Ok">>, Req3);
                _ ->
                    % do nothing
                    pass
            end;
        _ ->
            cowboy_req:reply(200, [], <<"Wrong request">>, Req)
    end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%=============================================================================
%% Internal functions
%%=============================================================================

%% @doc Format plugins
format_plugins_helper(Plugins) ->
    list_to_binary([Lang ++ " " ++ Name ++ " " ++ Path ++ "\n" || {plugin, Lang, Name, Path} <- Plugins]).

%% @doc request handling helper
handle_request({[{<<"is_history">>, IsHistory}, {<<"limit">>, History}]}) ->
    HistoryLimit = ybot_utils:to_int(History), 
    % Check use history or not
    case IsHistory of
        true ->
            case whereis(ybot_history) of
                undefined ->
                    % start ybot_history process
                    ybot_history:start_link(HistoryLimit);
                _ ->
                    % update history limit
                    gen_server:cast(ybot_history, {update_history_limit, HistoryLimit})
            end;
        false ->
            case whereis(ybot_history) of
                undefined ->
                    ok;
                _ ->
                    ybot_history:stop()
            end;
        _ ->
            wrong_command
    end;

handle_request({[{<<"upload_plugin_path">>, PluginUrl}]}) ->
    % Get plugin extension
    Ext = filename:extension(binary_to_list(PluginUrl)),
    % Check extenstion
    case lists:member(Ext, [".py", ".rb", ".sh", ".pl", ".ex", ".scala"]) of
        % good plugin
        true ->
            %
            % download plugin and save to plugins directory
            %
            PluginName = filename:basename(binary_to_list(PluginUrl)),
            {ok, PluginsDirectory} = application:get_env(ybot, plugins_path),
            {_, _, _, PluginData} = ibrowse:send_req(binary_to_list(PluginUrl), [], get),
            {ok, IODevice} = file:open(PluginsDirectory ++ PluginName, [write]), 
            file:write(IODevice, PluginData), file:close(IODevice);
        _ ->
            wrong_plugin
    end;

handle_request({[{<<"is_observer">>, UseObserver}, {<<"timeout">>, Timeout}]}) ->
    Time = ybot_utils:to_int(Timeout),
    % Check use observer or not
    case UseObserver of
        true ->
            case whereis(ybot_plugins_observer) of
                undefined ->
                    % get plugins directory
                    {ok, PluginsDirectory} = application:get_env(ybot, plugins_path),
                    % get all plugins paths
                    PluginsPaths = gen_server:call(ybot_manager, get_plugins_paths),
                    % start observer
                    ybot_plugins_observer:start_link(PluginsDirectory, PluginsPaths, Time);
                _ ->
                    % update plugins observer timeout
                    gen_server:cast(ybot_plugins_observer, {update_timeout, Time})
            end;
        false ->
            case whereis(ybot_plugins_observer) of
                undefined ->
                    ok;
                _ ->
                    ybot_plugins_observer:stop()
            end;
        _ ->
            wrong_command
    end;

handle_request({[{<<"transport">>, <<"irc">>}, _, _, _, _, _, _, _, _]} = Irc) ->
    {[{<<"transport">>, <<"irc">>}, {<<"irc_login">>, IrcLogin}, {<<"irc_password">>, Password},
      {<<"irc_channel">>, IrcChannel}, {<<"irc_channel_key">>, Key}, {<<"irc_server_host">>, Host},
      {<<"irc_server_port">>, Port}, {<<"irc_use_ssl">>, Ssl}, {<<"irc_reconnect_timeout">>, RecTimeout}
    ]} = Irc,
    Options = [{port, ybot_utils:to_int(Port)}, {use_ssl, Ssl}, {reconnect_timeout, ybot_utils:to_int(RecTimeout)}],
    % start irc
    ybot_manager:run_transport({irc, IrcLogin, {IrcChannel, Key}, {Host, Password}, Options}),
    % return 
    done;

handle_request({[{<<"transport">>, <<"xmpp">>}, _, _, _, _, _, _, _, _]} = Xmpp) ->
    {[{<<"transport">>, <<"xmpp">>}, {<<"xmpp_login">>, Login}, {<<"xmpp_password">>, Password},
      {<<"xmpp_room">>, Room}, {<<"xmpp_server">>, Host}, {<<"xmpp_resource">>, Resource},
      {<<"xmpp_port">>, Port}, {<<"xmpp_ssl">>, Ssl}, {<<"xmpp_reconnect_timeout">>, RecTimeout}
    ]} = Xmpp,
    % Options
    Options = [{port, ybot_utils:to_int(Port)}, {use_ssl, Ssl}, {reconnect_timeout, ybot_utils:to_int(RecTimeout)}],
    % Start xmpp
    ybot_manager:run_transport({xmpp, Login, Password, Room, Host, Resource, Options}),
    % return
    done;

handle_request({[{<<"transport">>, <<"campfire">>}, _, _, _, _, _]} = Campfire) ->
    {[{<<"transport">>, <<"campfire">>}, {<<"login">>, Login}, {<<"token">>, Token},
      {<<"room">>, Room}, {<<"subdomain">>, SubDomain}, {<<"reconnect_timeout">>, RecTimeout}
    ]} = Campfire,
    % Start campfire
    ybot_manager:run_transport({campfire, Login, Token, ybot_utils:to_int(Room), SubDomain, [{reconnect_timeout, ybot_utils:to_int(RecTimeout)}]}),
    done;

handle_request(_) ->
    error.