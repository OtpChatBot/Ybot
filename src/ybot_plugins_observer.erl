%%%----------------------------------------------------------------------
%%% File    : ybot_plugins_observer.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot new plugins observer
%%%----------------------------------------------------------------------

-module(ybot_plugins_observer).
 
-behaviour(gen_server).
 
-export([start_link/3]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {
    % current loaded plugins
    current_plugins = [],
    % plugins observer timeot
    timeout = 0,
    % plugins directory
    plugins_directory = ""
    }).
 
start_link(PluginsDirectory, Plugins, Timeout) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Plugins, Timeout], []).
 
init([PluginsDirectory, Plugins, Timeout]) ->
    % start observer
    erlang:send_after(Timeout, self(), {check_new_plugins, PluginsDirectory}),
    % init observer parameters
    {ok, #state{current_plugins = Plugins, timeout = Timeout, plugins_directory = PluginsDirectory}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc check new plugins
handle_info({check_new_plugins, PluginsDirectory}, State) ->
    % Get all plugins
    AllPlugins = ybot_utils:get_all_files(PluginsDirectory),
    % Get all current plugins
    CurrentPlugins = [Plugin || {_, _, _, Plugin} <- gen_server:call(ybot_manager, get_plugins)],
    % Check difference between plugins
    case AllPlugins -- CurrentPlugins of        
        % no new plugins
        [] ->
            % do nothing
            ok;
        NewPlugs ->
            % Make new plugins
            NewPlugins = lists:flatten(lists:map(fun ybot_manager:load_plugin/1, NewPlugs)),
            % update manager's plugins list
            gen_server:cast(ybot_manager, {update_plugins, NewPlugins})
    end,
    % Start new timer
    erlang:send_after(State#state.timeout, self(), {check_new_plugins, State#state.plugins_directory}),
    % return
    {noreply, State};
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions