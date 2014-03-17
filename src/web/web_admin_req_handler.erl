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
    {ok, WebAdmin} = application:get_env(ybot, web_admin),

    %% check access credentials if auth is enabled
    case config_option(webadmin_auth, WebAdmin) of
        true ->
            %% get configured credentials
            AuthUser = list_to_binary(
                         config_option(webadmin_auth_user, WebAdmin)),
            AuthPasswd = list_to_binary(
                           config_option(webadmin_auth_passwd, WebAdmin)),
            case is_authorized(Req, AuthUser, AuthPasswd) of
                {true, Req1}  -> authorized(Req1, State);
                {false, Req1} -> unauthorized(Req1, State)
            end;
        _ ->
            authorized(Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%=============================================================================
%% Internal functions
%%=============================================================================
authorized(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    case Path of
        <<"/">> ->
            {ok, Bin} = file:read_file(docroot("index.html")),
            {ok, Req2} = cowboy_req:reply(200,
                                          [
                                           {<<"content-type">>, <<"text/html">>}
                                          ], Bin, Req1),
            {ok, Req2, State};
        <<"/admin">> ->
            %% check body
            case cowboy_req:has_body(Req1) of
                true ->
                    %% get body
                    case cowboy_req:body(Req1) of
                        {ok, Body, Req2} ->
                            handle_request(Body, Req2, State);
                        _ ->
                            {ok, Req1, State}
                    end;
                false ->
                    {ok, Req1, State}
            end
    end.

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
        <<"upload_plugin">> ->
            % Get params
            {[{<<"upload_plugin_path">>, PluginPath}]} = Params,
            PluginName = filename:basename(binary_to_list(PluginPath)),
            {ok, PluginsDirectory} = application:get_env(ybot, plugins_path),
            {_, _, _, PluginData} = ibrowse:send_req(binary_to_list(PluginPath), [], get),
            {ok, IODevice} = file:open(PluginsDirectory ++ PluginName, [write]), 
            file:write(IODevice, PluginData), file:close(IODevice),
            file:close(IODevice);
        <<"get_runned_transports">> ->
            Transports = [atom_to_list(element(1, Transport))  ++ " " ++
                          pid_to_list(element(2, Transport))   ++ " " ++ "\n" || Transport <- gen_server:call(ybot_manager, get_transports), 
                          (element(1, Transport) /= http) or (element(1, Transport) /= skype)],
            Data = jiffy:encode({[{transport, list_to_binary(Transports)}]}),
            cowboy_req:reply(200, [], Data, Req);
        <<"start_irc">> ->
            {[{<<"irc_login">>, IrcLogin}, {<<"irc_password">>, Password},
              {<<"irc_channel">>, IrcChannel}, {<<"irc_channel_key">>, Key}, {<<"irc_server_host">>, Host},
              {<<"irc_server_port">>, Port}, {<<"irc_use_ssl">>, Ssl}, {<<"irc_reconnect_timeout">>, RecTimeout}]} = Params,
            Options = [{port, ybot_utils:to_int(Port)}, {use_ssl, Ssl}, {reconnect_timeout, ybot_utils:to_int(RecTimeout)}],
            ybot_manager:run_transport({irc, IrcLogin, {IrcChannel, Key}, {Host, Password}, Options}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_xmpp">> ->
            {[{<<"xmpp_login">>, Login}, {<<"xmpp_password">>, Password},
              {<<"xmpp_room">>, Room}, {<<"xmpp_nick">>, Nick}, {<<"xmpp_server">>, Host}, {<<"xmpp_resource">>, Resource},
              {<<"xmpp_port">>, Port}, {<<"xmpp_ssl">>, Ssl}, {<<"xmpp_reconnect_timeout">>, RecTimeout}]} = Params,
            Options = [{port, ybot_utils:to_int(Port)}, {use_ssl, Ssl}, {reconnect_timeout, ybot_utils:to_int(RecTimeout)}],
            ybot_manager:run_transport({xmpp, Login, Password, Room, Nick, Host, Resource, Options}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_campfire">> ->
            {[{<<"login">>, Login}, {<<"token">>, Token},
              {<<"room">>, Room}, {<<"subdomain">>, SubDomain}, {<<"reconnect_timeout">>, RecTimeout}]} = Params,
            ybot_manager:run_transport({campfire, Login, Token, ybot_utils:to_int(Room), SubDomain, [{reconnect_timeout, ybot_utils:to_int(RecTimeout)}]}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_hipchat">> ->
            {[{<<"hipchat_jid">>, Jid}, {<<"hipchat_password">>, Password},
              {<<"hipchat_room">>, Room}, {<<"hipchat_nick">>, Nick}, {<<"hipchat_reconnect_timeout">>, RecTimeout}]} = Params,
            ybot_manager:run_transport({hipchat, Jid, Password, Room, <<"chat.hipchat.com">>, <<"bot">>, Nick, [{reconnect_timeout, ybot_utils:to_int(RecTimeout)}]}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_skype">> ->
            {[{<<"skype_http_host">>, Host}, {<<"skype_http_port">>, Port}]} = Params,
            ybot_manager:run_transport({skype, true, Host, ybot_utils:to_int(Port)}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_http">> ->
            {[{<<"http_host">>, Host}, {<<"http_port">>, Port}, {<<"http_bot_nick">>, Nick}]} = Params,
            ybot_manager:run_transport({http, Host, ybot_utils:to_int(Port), Nick}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_flowdock">> ->
            {[{<<"flowdock_nick">>, Nick}, {<<"flowdock_login">>, Login},
              {<<"flowdock_password">>, Password}, {<<"flowdock_org">>, Org}, {<<"flowdock_flow">>, Flow}]} = Params,
            ybot_manager:run_transport({flowdock, Nick, Login, Password, Org, Flow}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"start_talkerapp">> ->
            {[{<<"talkerapp_nick">>, Nick}, {<<"talkerapp_room">>, Room}, {<<"talkerapp_token">>, Token}]} = Params,
            ybot_manager:run_transport({talkerapp, Nick, Room, Token}),
            cowboy_req:reply(200, [], <<"ok">>, Req);
        <<"get_storage_info">> ->
            StorageHost = ybot_utils:get_val(brain_api_host, {storage_host, false}),    
            StoragePort = ybot_utils:get_val(brain_api_port, {storage_port, 0}),
            Data = jiffy:encode({[StorageHost, StoragePort]}),
            cowboy_req:reply(200, [], Data, Req);
        _ ->
            ok
    end,
    {ok, Req, State}.

%% @doc Format plugins
format_plugins_helper(Plugins) ->
    list_to_binary([Lang ++ " " ++ Name ++ " " ++ Path ++ "\n" || {plugin, Lang, Name, Path} <- Plugins]).

%% Authorization helpers
is_authorized(Req, User, Passwd) ->
    {ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {<<"basic">>, {User, Passwd}} ->
            {true, Req1};
        _ ->
            {false, Req1}
    end.

unauthorized(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"Www-Authenticate">>,
                                      <<"Basic realm=\"Secure Area\"">>, Req),
    Req2 = cowboy_req:set_resp_body(unauthorized_body(), Req1),
    {ok, Req3} = cowboy_req:reply(401, Req2),
    {ok, Req3, State}.

unauthorized_body() ->
    <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dt\">
    <html>
      <head>
        <title>Error</title>
        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
      </head>
      <body><h1>401 Unauthorized.</h1></body>
    </html>
    ">>.

docroot(Append) ->
    priv_dir() ++ "webadmin/" ++ Append.

priv_dir() ->
    case code:priv_dir(ybot) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.

config_option(Key, Options) ->
    case lists:keyfind(Key, 1, Options) of
        {Key, Value} -> Value;
        false -> false
    end.
