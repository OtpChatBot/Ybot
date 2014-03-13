%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot incoming message parser.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_parser).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Internal state
-record(state, {}).


%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%%=============================================================================
%%% ybot_parser callbacks
%%%=============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%%-----------------------------------------------------------------------------
%% @doc parse incoming message and execute plugin if command is successful
%% @param incoming_message :: atom()
%% @param TransportPid :: pid()
%% @param Nick :: string()
%% @param From :: string()
%% @param IncomingMessage :: string()
%%-----------------------------------------------------------------------------
handle_cast({incoming_message, TransportPid, BotNick, From, Input}, State) ->
    [FirstWord | Rest] = string:tokens(Input, " \r\n"),
    handle_message(TransportPid, From, BotNick, [parse_nick(FirstWord) | Rest]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

handle_message(TransportPid, From, BotNick, [BotNick]) ->
    send_message(TransportPid, From, "What?");
handle_message(TransportPid, From, BotNick, [BotNick, "name?"]) ->
    send_message(TransportPid, From, "My name is: " ++ BotNick);
handle_message(_TransportPid, _From, _BotNick, [_BotNick, "announce" | _]
               = Input) ->
    ybot_utils:broadcast(
      string:tokens(ybot_utils:split_at_end(Input, "announce"), "\r\n"));
handle_message(TransportPid, From, _BotNick, [_Nick, "hi"]) ->
    send_message(TransportPid, From, "Hello");
handle_message(TransportPid, From, _BotNick, [_Nick, "hello"]) ->
    send_message(TransportPid, From, "Hi!");
handle_message(TransportPid, From, _BotNick, [_Nick, "bye"]) ->
    send_message(TransportPid, From, "Good bye");
handle_message(TransportPid, From, _BotNick, [_Nick, "thanks"]) ->
    send_message(TransportPid, From, "by all means");
handle_message(TransportPid, From, _BotNick, [_Nick, "history"]) ->
    %% Check ybot_history process
    case whereis(ybot_history) of
        undefined ->
            send_message(TransportPid, From, "History is disabled");
        _ ->
            %% Get history and send its content
            case gen_server:call(ybot_history, {get_history, TransportPid}) of
                [] -> send_message(TransportPid, From, "The history is empty");
                History -> send_message(TransportPid, From, History)
            end
    end;
handle_message(TransportPid, From, _BotNick, [_Nick, "plugins?"]) ->
    %% Get plugins and format output
    Plugins = gen_server:call(ybot_manager, get_plugins),
    PluginNames = string:join(
                    lists:map(fun({_, _, Pl, _}) -> Pl end, Plugins), ", "),

    %% Send plugins
    send_message(TransportPid, From, "Plugins: " ++ PluginNames),
    send_message(TransportPid, From, "That's all :)");
handle_message(TransportPid, From, _BotNick, [_Nick, Command | _] = Input) ->
    %% Get command arguments
    Args =string:strip(
            lists:flatten(
              string:tokens(ybot_utils:split_at_end(Input, Command),"\r\n"))),
    %% Start plugin process and send command
    ybot_actor:start_link(TransportPid, From, Command, Args);
handle_message(_TransportPid, _From, _BotNick, _Message) ->
    pass.

send_message(TransportPid, From, Message) ->
    gen_server:cast(TransportPid, {send_message, From, Message}).

%% @doc Strip last permissible symbol from tail of nick
parse_nick(FirstWord) ->
    [LastLetter | Rest] = lists:reverse(FirstWord),
    case lists:member(LastLetter, [$,, $:, 32]) of
        true  -> lists:reverse(Rest);
        false -> FirstWord
    end.
