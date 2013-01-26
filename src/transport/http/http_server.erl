%%%----------------------------------------------------------------------
%%% File    : ../transport/http/http_server.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot http server.
%%%----------------------------------------------------------------------
-module(http_server).
 
-behaviour(gen_server).
 
-export([start_link/2, do/1]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("inets/include/httpd.hrl").

 %% @doc internal state
-record(state, {
    }).
 
start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).
 
init([Host, Port]) ->
    % start server
    ok = gen_server:cast(self(), {start_serve, Host, Port}),
    % init state
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc start serve
handle_cast({start_serve, Host, Port}, State) ->
    % Get priv directory
    PrivDirectory = ybot_utils:get_priv_dir(),
    % document root
    WWW = PrivDirectory ++ "www",
    % server root
    Log = PrivDirectory ++ "log",
    % Start httpd
    inets:start(httpd, [{modules, [http_server]}, {port, Port}, {server_name,"Ybot"}, 
                        {server_root, Log}, {document_root, WWW}, {ipfamily, inet}, 
                        {bind_address, binary_to_list(Host)}, {error_log, "error.log"},
                        {security_log, "security.log"}, {transfer_log, "transfer.log"}]),
 
    % return
    {noreply, State};
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions

%% @doc httpd callback
do(#mod{entity_body = Data} = _ModData) ->
    % Match incoming message
    case string:tokens(Data, " \r\n") of
        ["Ybot"] ->
            % Send response
            {proceed, [{response, {200, "What?"}}]};
        ["Ybot", "hi"] ->
            % Send response
            {proceed, [{response, {200, "Hello :)"}}]};
        ["Ybot", "bye"] ->
            % Send response
            {proceed, [{response, {200, "Good bue"}}]};
        ["Ybot", "history"] ->
            % Get history
            History = gen_server:call(ybot_history, {get_history, self()}),
            % Send response
            {proceed, [{response, {200, History}}]};
        ["Ybot", "plugins?"] ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            PluginNames = lists:map(fun({_, _, Pl, _}) -> Pl end, Plugins),
            % Send plugins label
            {proceed, [{response, {200, "Plugins: " ++ string:join(PluginNames, ", ") ++ "\n" ++ "That's all :)"}}]};
        ["Ybot", Command | _] ->
                % Get command arguments
                Args = string:tokens(ybot_utils:split_at_end(Data, Command), "\r\n"),
                % Try to execute plugin and resend result
                Result = handle_command(Command, Args, self()),
                % send response
                {proceed, [{response, {200, Result}}]};
        % this is not our command
        _ ->
            {proceed, [{response, {200, "Wrong request"}}]}
    end.

%% @doc Try to find plugin and execute it
handle_command(Command, Args, TransportPid) ->
    % Get plugin metadata
    TryToFindPlugin = gen_server:call(ybot_manager, {get_plugin, Command}),
    % Check plugin
    case TryToFindPlugin of
        wrong_plugin ->
            % plugin not found
            error;
        {plugin, Lang, _PluginName, PluginPath} ->
            % execute plugin
            Result = os:cmd(Lang ++ " " ++ PluginPath ++ " \'" ++ Args ++ "\'"),
            % Save command to history
            ok = gen_server:cast(ybot_history, {update_history, TransportPid, "Ybot " ++ Command ++ " " ++ Args ++ "\n"}),
            % return result
            Result
    end.