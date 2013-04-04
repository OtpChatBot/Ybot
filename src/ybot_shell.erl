%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot shell.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_shell).

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
         act/1]).

%% Internal state 
-record(state, {
        % ybot command parser
        parser_pid :: pid()
    }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
	lager:info("Ybot shell start ~n"),
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