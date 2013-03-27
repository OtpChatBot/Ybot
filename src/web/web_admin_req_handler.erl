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
    case Method of
        <<"GET">> ->
            % Get query string
            {QS, Req3} = cowboy_req:qs(Req2),
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
                    IsObserver = {is_observer, ybot:is_new_plugins_observing()},
                    % Get observer timeout
                    ObserverTimeout = {observer_timeout, ybot:plugin_observer_timeout()},
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
                % Get current plugins settings
                %
                ["req", "ybot_plugins_settings"] ->
                    % Using observer or not
                    IsObserver = {is_observer, ybot:is_new_plugins_observing()},
                    % Observer timeout limit
                    ObserverTimeout = {observer_timeout, ybot:plugin_observer_timeout()},
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