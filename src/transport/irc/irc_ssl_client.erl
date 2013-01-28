%%%----------------------------------------------------------------------
%%% File    : transport/irc/irc_ssl_client.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Irc client with ssl supporting.
%%%----------------------------------------------------------------------
-module(irc_ssl_client).

-behaviour(gen_server).

-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% irc client state
-record(state, {
    % irc nick
    login = <<>> :: binary(),
    % irc server host
    host = <<>> :: binary(),
    % irc channel
    irc_channel = <<>> :: binary(),
    % channel key
    irc_channel_key = <<>> :: binary(),
    % irc connection socket
    socket = null,
    % auth or not
    is_auth = false :: boolean(),
    % calback module
    callback = null
    }).

start_link(CallbackModule, Host, Port, Channel, Nick) ->
    gen_server:start_link(?MODULE, [CallbackModule, Host, Port, Channel, Nick], []).

init([CallbackModule, Host, Port, Channel, Nick]) ->
    % try to connect
    gen_server:cast(self(), {connect, Host, Port}),
    % Get channel and key
    {Chan, Key} = Channel,
    % init process internal state
    {ok, #state{login = Nick, host = Host, irc_channel = Chan, irc_channel_key = Key, callback = CallbackModule}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Try to connect to irc server and join to channel
handle_cast({connect, Host, Port}, State) ->
    % Try to connect to irc server
    case ssl:connect(binary_to_list(Host), Port, [{delay_send, false}, {verify, 0}, {nodelay, true}]) of
        {ok, Socket} ->
            ssl:send(Socket, "NICK " ++ binary_to_list(State#state.login) ++ "\r\n"),
            % Send user data
            ssl:send(Socket, "USER " ++ binary_to_list(State#state.login) ++ " some fake info\r\n"),
            % Join to channel
            erlang:send_after(15000, self(), {join, Socket, "JOIN " ++ binary_to_list(State#state.irc_channel) 
                                                             ++ " " ++ binary_to_list(State#state.irc_channel_key) ++ "\r\n"}),
            % return
            {noreply, State#state{socket = Socket, is_auth = false}};
        {error, Reason} ->
            % Some log
            lager:error("Unable to connect to irc server with reason ~s", [Reason]),
            % return
            {noreply, State}
        end;

%% Send message to irc
handle_cast({send_message, Message}, State) ->
    % Split messages by \r\n
    MessagesList = string:tokens(Message, "\r\n"),
    % Send messages
    lists:foreach(fun(Mes) ->
                      timer:sleep(200),
                      % Send message to irc
                      ssl:send(State#state.socket, "PRIVMSG " ++ binary_to_list(State#state.irc_channel) ++ " :" ++ Mes ++ "\r\n")
                  end, 
                  MessagesList),
    % return
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Join to channel
handle_info({join, Socket, Message}, State) ->
    ssl:send(Socket, Message),
    {noreply, State};

%% @doc Incoming message
handle_info({ssl, Socket, Data}, State) ->
    % Parse incoming data
    case string:tokens(Data, " ") of
        ["PING" | PongHost] ->
            % Send pong
            ssl:send(Socket, "PONG " ++ PongHost ++ "\r\n");    
        [_User, "PRIVMSG", _Channel | Message] ->
            % Get incoming message
            [_ | IncomingMessage] = string:join(Message, " "),
            % Send incomming message to callback
            State#state.callback ! {incoming_message, IncomingMessage};
        _ ->
            pass
    end,
    % return
    {noreply, State#state{socket = Socket}};


handle_info({ssl_closed, Reason}, State) ->
    % Some log
    io:format("ssl_closed with reason: ~p~n", [Reason]),
    % stop and return state
    {stop, normal, State};

handle_info({ssl_error, _Socket, Reason}, State) ->
    % Some log
    io:format("tcp_error: ~p~n", [Reason]),
    % stop and return state
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, State) ->
    % Check active socket
    case State#state.socket of
        null ->
            ok;
        _ ->
            case State#state.is_auth of
                false ->
                    ok;
                _ ->
                    ssl:send(State#state.socket, "QUIT :Session off \r\n")
            end
    end,
    % terminate
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions