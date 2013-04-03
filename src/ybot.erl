%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot entry point.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot).

-export([start/0, stop/0]).

-behaviour(gen_server).
 
-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% API
-export([plugins/0, 
         act/1,
         get_plugins_directory/0,
         get_runned_transports/0,
         get_plugins/0,
         is_new_plugins_observing/0,
         plugin_observer_timeout/0,
         is_command_history/0,
         get_command_history_limit/0,
         get_brain_storage/0,
         get_brain_storage_host/0,
         get_brain_storage_port/0]).

%% Internal state 
-record(state, {
        % ybot command parser
        parser_pid :: pid()
    }).

-spec start() -> ok.
start() ->
    [application:start(A) || A <- deps() ++ [ybot]],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- lists:reverse(deps()) ++ [ybot]],
    ok.

%% Internal functions
deps() ->
    [compiler, syntax_tools, lager, inets, crypto, public_key, ssl,
     mnesia, ranch, cowboy, jiffy, ibrowse, reloader].
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    % start parser
    {ok, Pid} = ybot_parser:start_link(),
    % init state
    {ok, #state{parser_pid = Pid}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc send command to Ybot
handle_cast({command, Command}, State) ->
    % Send command to parser
    gen_server:cast(State#state.parser_pid, {incoming_message, self(), "Ybot", "", Command}),
    % return
    {noreply, State};

%% @doc print result
handle_cast({send_message, _From, Result}, State) ->
    % print result
    lager:info("~p", [Result]),
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
 
%%=============================================================================
%% Internal functions
%%=============================================================================

%% @doc Print all plugins
-spec plugins() -> done.
plugins() ->
    case whereis(ybot_manager) of
        undefined ->
            lager:info("ybot_manager not runned");
        _ ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            % Log
            lager:info("Plugins:"),
            % print plugins
            [lager:info("~p ~p", [Source, PluginName]) || {plugin, Source, PluginName, _} <- Plugins]
    end,
    % return
    done.

%% @doc Ybot execute command
%% Example:
%%     ybot:act("Ybot math 1 + 1").
%% @end
-spec act(Command :: string()) -> done.
act(Command) ->
    % Send command to ybot
    gen_server:cast(ybot, {command, Command}),
    % return
    done.

%% @doc get plugins directory
-spec get_plugins_directory() -> string().
get_plugins_directory() ->
    {ok, PluginsPath} = application:get_env(ybot, plugins_path),
    PluginsPath.

%% @doc get runned transports
get_runned_transports() ->
    gen_server:call(ybot_manager, get_transports).

%% @doc get plugins
get_plugins() ->
    gen_server:call(ybot_manager, get_plugins).

%% @doc plugin observer works or not
is_new_plugins_observing() ->
    ybot_utils:get_config_val(checking_new_plugins, false).

%% @doc plugin observer timeout
plugin_observer_timeout() ->
    ybot_utils:get_config_val(checking_new_plugins_timeout, 0).

%% @doc is command history on
is_command_history() ->
    ybot_utils:get_config_val(commands_history, false).
    
%% @doc command history limit
get_command_history_limit() ->
    ybot_utils:get_config_val(history_command_limit_count, 0).

%% @doc get storage backend
get_brain_storage() ->
    ybot_utils:get_config_val(brain_storage, mnesia).

%% @doc get storage host
get_brain_storage_host() ->
    ybot_utils:get_config_val(brain_api_host, <<"localhost">>).

%% @doc get storage por
get_brain_storage_port() ->
    ybot_utils:get_config_val(brain_api_port, 8090).
