%%%----------------------------------------------------------------------
%%% File    : ybot_actor.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot actor. Process started when Ybot receive message.
%%%           Main command executer.
%%%----------------------------------------------------------------------
-module(ybot_actor).

-behaviour(gen_server).
 
-export([start_link/3, stop/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {}).
 
start_link(TransportPid, Command, Args) ->
    gen_server:start_link(?MODULE, [TransportPid, Command, Args], []).
 
init([TransportPid, Command, Args]) ->
    % execute plugin
    gen_server:cast(self(), {execute, TransportPid, Command, Args}),
    % init
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Execute plugin
handle_cast({execute, TransportPid, Command, Args}, State) ->
    % Get plugin metadata
    TryToFindPlugin = gen_server:call(ybot_manager, {get_plugin, Command}),
    % Check plugin
    case TryToFindPlugin of
        % plugin not found
        wrong_plugin ->
            % Send message to transport
            gen_server:cast(TransportPid, {send_message, "Sorry, i don't know anything about " ++ Command});
        {plugin, Lang, _PluginName, PluginPath} ->
            % execute plugin
            Result = os:cmd(Lang ++ " " ++ PluginPath ++ " " ++ Args),
            % send result to chat
            gen_server:cast(TransportPid, {send_message, Result})
    end,
    % stop actor
    stop(),
    % return
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions
stop() ->
    gen_server:cast(?MODULE, stop).