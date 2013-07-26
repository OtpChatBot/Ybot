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
    % check body
    case cowboy_req:has_body(Req) of
        true ->
            % get body
            case cowboy_req:body(Req) of
                {ok, Body, Req2} ->
                    handle_request(Body, Req2, State);
                _ ->
                    {ok, Req, State}
            end;
        false ->
            {ok, Req, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%=============================================================================
%% Internal functions
%%=============================================================================
handle_request(Body, Req, State) ->
    % get method and params
    {[{<<"method">>, Method},{<<"params">>, Params}]} = jiffy:decode(Body),
    case Method of
        <<"get_start_page">> ->
            % get runned transports
            Transports = [atom_to_list(element(1, Transport))  ++ " " ++
                          pid_to_list(element(2, Transport))   ++ " " ++ "\n" || Transport <- gen_server:call(ybot_manager, get_transports), 
                          (element(1, Transport) /= http) or (element(1, Transport) /= skype)],

            % Get plugins
            Plugins = {plugins, format_plugins_helper(gen_server:call(ybot_manager, get_plugins))},

            % Is history using
            IsHistory = ybot_utils:get_val(ybot_history, {is_history, false}),
            % History limit
            HistoryLimit = ybot_utils:get_val(ybot_history, {history_limit, 0}),
            % Get observer
            IsObserver = ybot_utils:get_val(ybot_plugins_observer, {is_observer, false}),
            % Get observer timeout
            ObserverTimeout = ybot_utils:get_val(ybot_plugins_observer, {observer_timeout, 0}),
            % Get storage
            Storage = {storage_type, ybot_utils:get_config_val(brain_storage, mnesia)},
            % prepare data to json
            Data = {[{transport, list_to_binary(Transports)}, IsHistory, HistoryLimit, Plugins, IsObserver, ObserverTimeout,
                      Storage]},
            % Convert to json
            Json = jiffy:encode(Data),
            % Send info to Ybot webadmin
            cowboy_req:reply(200, [], Json, Req);
        <<"update_observer">> ->
            % Get params
            {[{<<"timeout">>, Timeout},{<<"is_observer">>, UseObserver}]} = Params,
            % convert time to integer
            Time = ybot_utils:to_int(Timeout),
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
                    end
            end;
        <<"update_history">> ->
            % Get params
            {[{<<"timeout">>, Timeout},{<<"is_history">>, IsHistory}]} = Params,
            % Convert history parama to integer
            HistoryLimit = ybot_utils:to_int(Timeout),
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
                    end
            end;
        _ ->
            ok
    end,
    {ok, Req, State}.

%% @doc Format plugins
format_plugins_helper(Plugins) ->
    list_to_binary([Lang ++ " " ++ Name ++ " " ++ Path ++ "\n" || {plugin, Lang, Name, Path} <- Plugins]).