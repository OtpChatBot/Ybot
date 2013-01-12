-module(irc_lib_client).

-behaviour(gen_server).
 
-export([start_link/4, send_message/2]).
 
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
	% irc connection socket
	socket = null,
    % auth or not
    is_auth = false :: boolean(),
    % calback module
    callback = null
	}).

-define(PORT, 6667).

start_link(CallbackModule, Host, Channel, Nick) ->
    gen_server:start_link(?MODULE, [CallbackModule, Host, Channel, Nick], []).

%% @doc IRC client with `Pid` send message to chat
send_message(Pid, Message) ->
	gen_server:cast(Pid, {send_message, Message}).
 
init([CallbackModule, Host, Channel, Nick]) ->
	% try to connect
	gen_server:cast(self(), {connect, Host}),
    % init process internal state
    {ok, #state{login = Nick, host = Host, irc_channel = Channel, callback = CallbackModule}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Try to connect to irc server and join to channel
handle_cast({connect, Host}, State) ->
	% Try to connect to irc server
	case gen_tcp:connect(binary_to_list(Host), ?PORT, []) of
		{ok, Socket} ->
			gen_tcp:send(Socket, "NICK " ++ binary_to_list(State#state.login) ++ "\r\n"),
			% Send user data
            gen_tcp:send(Socket, "USER " ++ binary_to_list(State#state.login) ++ " some fake info\r\n"),
    		% Join to channel
            gen_tcp:send(Socket, "JOIN " ++ binary_to_list(State#state.irc_channel) ++ "\r\n"),
			% return
			{noreply, State#state{socket = Socket, is_auth = true}};
		{error, Reason} ->
			% Some log
			io:format("ERROR: ~p~n", [Reason]),
			{noreply, State}
		end;

%% Send message to irc
handle_cast({send_message, Message}, State) ->
	% Send message to irc
	gen_tcp:send(State#state.socket, "PRIVMSG " ++ binary_to_list(State#state.irc_channel) ++ " :" ++ Message ++ "\r\n"),
	% return
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Incoming message
handle_info({tcp, Socket, Data}, State) ->
	% Parse incoming data
	case string:tokens(Data, " ") of
		["PING" | _] ->
			% Send pong
			gen_tcp:send(Socket, "PONG " ++ State#state.host);
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

handle_info({tcp_closed, _}, State) ->
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
    				gen_tcp:send(State#state.socket, "QUIT :Session off \r\n")
    		end
    end,
    % terminate
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions