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
                    % Get history
                    IsHistory = {is_history, ybot:is_command_history()},
                    % Get history limit
                    HistoryLimit = {history_limit, ybot:get_command_history_limit()},
                    % Get plugins
                    PluginsDirectory = {plugins_directory, list_to_binary(ybot:get_plugins_directory())},
                    % Get plugins
                    Plugins = {plugins, format_plugins_helper(ybot:get_plugins())},

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
                                              ObservTimeout = gen_server:call(ybot_plugins_observer, get_observer_timeout),
                                              {observer_timeout, ObservTimeout}
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
                                              ObservTimeout = gen_server:call(ybot_plugins_observer, get_observer_timeout),
                                              {observer_timeout, ObservTimeout}
                                      end,
                    % Prepara data for converting into json
                    Data = {[IsObserver, ObserverTimeout]},
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
                    case cowboy_req:has_body(Req3) of
                        true ->
                            % Get body
                            {ok, [{Body, _}], _} = cowboy_req:body_qs(Req3),
                            % handle request
                            handle_request(jiffy:decode(Body)),
                            % Send response
                            cowboy_req:reply(200, [], <<"Ok">>, Req3);
                        _ ->
                            pass
                    end;
                %
                % @TODO Update history settings
                %
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
handle_request({[{<<"is_observer">>, UseObserver}, {<<"timeout">>, Timeout}]}) ->
    % Check time
    Time = ybot_utils:to_int(Timeout),
    % Check observer
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
    
handle_request(_) ->
    error.