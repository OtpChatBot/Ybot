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
        % bot nick
        nick = "" :: string()
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

%% @doc Set bot nick
handle_cast({bot_nick, BotNick}, State) ->
    {noreply, State#state{nick = binary_to_list(BotNick)}};

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
do(#mod{entity_body = Data, parsed_header = Headers} = _ModData) ->
    % Try to find content-type header
    Incomming = case lists:keyfind("content-type", 1, Headers) of
        false ->
            % do nothing
            Data;
        {_, ContentType} ->
            % Check content-type
            case ContentType of
                % Got json
                "application/json" ->
                    % Try to decode
                    try
                        % decode json
                        {struct, DecodeJson} = mochijson2:decode(Data),
                        % get nick
                        {_, Nick} = lists:keyfind(<<"nick">>, 1, DecodeJson),
                        % get command
                        {_, Comm} = lists:keyfind(<<"command">>, 1, DecodeJson),
                        % get arguments
                        {_, TempArgs} = lists:keyfind(<<"args">>, 1, DecodeJson),
                        % Make string
                        binary_to_list(Nick) ++ " " ++ binary_to_list(Comm) ++ " " ++ lists:flatten([binary_to_list(Arg) ++ " " || Arg <- TempArgs])
                    catch _ : _ ->
                        ""
                    end;
                % Other content-type. Do nothing
                _ ->
                    Data
            end
    end,

    % Match incoming message
    case string:tokens(Incomming, " \r\n") of
        [BotNick] ->
            % Send response
            {proceed, [{response, {200, "What?"}}]};
        [BotNick, "hi"] ->
            % Send response
            {proceed, [{response, {200, "Hello :)"}}]};
        [BotNick, "bye"] ->
            % Send response
            {proceed, [{response, {200, "Good bue"}}]};
        [BotNick, "history"] ->
            % Get history
            History = gen_server:call(ybot_history, {get_history, self()}),
            % Send response
            {proceed, [{response, {200, History}}]};
        [BotNick, "plugins?"] ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            PluginNames = lists:map(fun({_, _, Pl, _}) -> Pl end, Plugins),
            % Send plugins label
            {proceed, [{response, {200, "Plugins: " ++ string:join(PluginNames, ", ") ++ "\n" ++ "That's all :)"}}]};
        [BotNick, Command | _] ->
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