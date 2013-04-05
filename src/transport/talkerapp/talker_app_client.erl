%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Talkerapp transport client.
%%% @end
%%%-----------------------------------------------------------------------------
-module(talker_app_client).

-behaviour(gen_server).
 
-export([start_link/5]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Default port for Talkerapp. 
-define(PORT, 8500).

%% @doc Internal state.
-record(state, {
        % callback module with handler
        callback :: pid(),
        % connected socket
        socket = null,
        % user access token
        token = <<>> :: binary(),
        % Bot nick
        bot_nick = <<>> :: binary(),
        % talkerapp room
        room = <<>> :: binary(),
        % talkerapp reconnect timeout
        reconnect_timeout = 0 :: integer()
    }).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Callback, BotNick, Room, Token, RecTimeout) ->
    gen_server:start_link(?MODULE, [Callback, BotNick, Room, Token, RecTimeout], []).

%%%=============================================================================
%%% talker_app client callback
%%%=============================================================================
 
init([Callback, BotNick, Room, Token, RecTimeout]) ->
    % start connection
    gen_server:cast(self(), {connect, Token, Room}),
    % init state and return
    {ok, #state{bot_nick = BotNick, callback = Callback, token = Token, room = Room, reconnect_timeout = RecTimeout}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc send message
handle_cast({send_message, _From, Message}, State) ->
    % Make json message
    JsonMessage = "{\"type\":\"message\",\"content\" :\"" ++ Message ++ "\"}",
    % Send message
    ssl:send(State#state.socket, JsonMessage),
    % return state
    {noreply, State};

%% @doc connect to talkerapp
handle_cast({connect, Token, Room}, State) ->
    % Connect to talkerapp.com
    case ssl:connect("talkerapp.com", ?PORT, [{verify, 0}]) of
        {ok, Socket} ->
            AuthMessage = "{\"type\":\"connect\",\"room\":\"" ++ binary_to_list(Room) ++ "\",\"token\":\"" ++ binary_to_list(Token) ++ "\"}",
            % Try to auth
            ssl:send(Socket, AuthMessage),
            % Send ping in loop
            erlang:send_after(25000, self(), ping),
            % save socket and return
            {noreply, State#state{socket = Socket}};
        {error, Reason} ->
            % Some log
            lager:error("Unable to connect to talkerapp server with reason ~s", [Reason]),
            % return state
            {noreply, State}
    end;
 
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Incoming message
handle_info({ssl, _Socket, Data}, State) ->
    % Decode json data
    {DecodeJson} = jiffy:decode(Data),
    case lists:member({<<"type">>,<<"message">>}, DecodeJson) of
        true ->
            % Get user data
            UserName = get_user_name(DecodeJson),
            if UserName == State#state.bot_nick ->
                % do nothing
                pass;
            true ->
                % Send incoming message to handler
                Message = get_message(DecodeJson),
                State#state.callback ! {incoming_message, Message}
            end;
        false ->
            % do nothing
            pass
    end,
    % return
    {noreply, State};

handle_info({ssl_closed, Reason}, State) ->
    % Some log
    lager:info("ssl_closed with reason: ~p~n", [Reason]),
    % try reconnect
    try_reconnect(State);

handle_info({ssl_error, _Socket, Reason}, State) ->
    % Some log
    lager:error("tcp_error: ~p~n", [Reason]),
    % try reconnect
    try_reconnect(State);

%% @doc send ping
handle_info(ping, State) ->
    % Send ping
    ssl:send(State#state.socket, "{\"type\":\"ping\"}"),
    % Send ping in loop
    erlang:send_after(25000, self(), ping),
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%%=============================================================================
%%% Internal functions
%%%=============================================================================
get_user_name([H | Json]) ->
    case H of
        {<<"user">>, UserData} ->
            {<<"user">>, {Data}} = H,
            [{<<"name">>, UserName}] = lists:flatten(lists:filter(fun(D) -> {Label, _} = D,
                                                                      case Label of
                                                                          <<"name">> -> true;
                                                                          _ -> false
                                                                      end
                                                                  end, Data)),
            UserName;
        _ ->
            get_user_name(Json)
    end.

get_message([H | Json]) ->
    case H of
        {<<"content">>, Message} ->
            Message;
        _ ->
            get_message(Json)
    end.

%% @doc try reconnect
-spec try_reconnect(State :: #state{}) -> {normal, stop, State} | {noreply, State}.
try_reconnect(#state{reconnect_timeout = Timeout, token = Token, room = Room} = State) ->
    case Timeout > 0 of
        true ->
            % no need in reconnect
            {normal, stop, State};
        false ->
            % sleep
            timer:sleep(Timeout),
            % Try reconnect
            gen_server:cast(self(), {connect, Token, Room}),
            % return
            {noreply, State}
    end.