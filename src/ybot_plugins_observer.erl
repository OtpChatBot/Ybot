%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot new plugins observer.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_plugins_observer).
 
-behaviour(gen_server).
 
-export([start_link/3, stop/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Public api
-export([observe_new_plugins/2]).

%% Internal state 
-record(state, {
    % current loaded plugins
    current_plugins = [],
    % plugins observer timeot
    timeout = 0,
    % plugins directory
    plugins_directory = ""
    }).

%%=============================================================================
%% API functions
%%=============================================================================

%%
%% @doc Plugins directory observer
%% PluginsDirectory - Directory with plugins
%% Plugins - current plugins
%% Timeout - observe timeout
%%
start_link(PluginsDirectory, Plugins, Timeout) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Plugins, Timeout], []).

%% @doc stop observer process
stop() ->
    gen_server:cast(ybot_plugins_observer, stop).

%%%============================================================================
%%% Observer callbacks
%%%============================================================================
 
init([PluginsDirectory, Plugins, Timeout]) ->
    % start observer
    erlang:send_after(Timeout, self(), {check_new_plugins, PluginsDirectory, Plugins}),
    % init observer parameters
    {ok, #state{current_plugins = Plugins, timeout = Timeout, plugins_directory = PluginsDirectory}}.

handle_call(get_observer_timeout, _From, State) ->
    {reply, State#state.timeout, State};
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc update observing timeout
handle_cast({update_timeout, Time}, State) ->
    {noreply, State#state{timeout = Time}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc check new plugins
handle_info({check_new_plugins, PluginsDirectory, CurrentPlugins}, State) ->
    % Get all plugins
    Plugins = lists:append(
                ybot_utils:get_all_files(PluginsDirectory),
                ybot_utils:get_all_directories(PluginsDirectory)
               ),
    % Try to get difference between plugins
    case Plugins -- CurrentPlugins of
        % no new plugins
        [] ->
            erlang:send_after(State#state.timeout, self(), {check_new_plugins, State#state.plugins_directory, Plugins});
        NewPluginsPaths ->
            % Make new plugins
            NewPlugins = lists:flatten(lists:map(fun ybot_manager:load_plugin/1, NewPluginsPaths)),
            % update manager's plugins list
            gen_server:cast(ybot_manager, {update_plugins, NewPlugins}),
            % Start new timer
            erlang:send_after(State#state.timeout, self(), {check_new_plugins, State#state.plugins_directory, Plugins})
    end,
    % return
    {noreply, State#state{current_plugins = CurrentPlugins}};
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%%============================================================================
%%% Internal functions
%%%============================================================================

%% @doc check observe new plugins after start or not.
observe_new_plugins(PluginsDirectory, PluginsPaths) ->
     % Try to get checking_new_plugins parameter from config
    UseNewPlugins = case application:get_env(ybot, checking_new_plugins) of
                        {ok, true} ->
                            true;
                        {ok, false} ->
                            false;
                        _ ->
                            false
                    end,
            
    % Check checking_new_plugins
    case UseNewPlugins of
        true ->
            % Get new plugins checking timeout
            {ok, NewPluginsCheckingTimeout} = application:get_env(ybot, checking_new_plugins_timeout),
            % Start new plugins observer
            ybot_plugins_observer:start_link(PluginsDirectory, PluginsPaths, NewPluginsCheckingTimeout);
        _ ->
            % don't use new plugins
            pass
    end,
    % return
    ok.