%%%----------------------------------------------------------------------
%%% File    : transport/xmpp/xmpp_client.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Xmpp client
%%%----------------------------------------------------------------------
-module(xmpp_client).

-behaviour(gen_server).

-include("xmpp.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([start_link/6]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Default port
-define(PORT, 5222).

%% @doc Xmpp client internal state
-record (state, {
        % Xmpp client socket
        socket = null,
        % Is auth state or not
        is_auth = false,
        % client callback module or pid
        callback,
        % Xmpp client login
        login,
        % Xmpp client password
        password,
        % jabber server host
        host,
        % Jabber room
        room,
        % Client resource
        resource
    }).

start_link(CallbackModule, Login, Password, Server, Room, Resource) ->
    gen_server:start_link(?MODULE, [CallbackModule, Login, Password, Server, Room, Resource], []).

init([CallbackModule, Login, Password, Server, Room, Resource]) ->
    % try to connect
    gen_server:cast(self(), {connect, Server}),
    % init process internal state
    {ok, #state{callback = CallbackModule,
                login = Login,
                password = Password,
                host = Server,
                room = list_to_binary(binary_to_list(Room) ++ "/" ++ binary_to_list(Login)),
                resource = Resource
               }
    }.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc send message to jabber
handle_cast({send_message, Message}, State) ->
    % Make from
    From = binary_to_list(State#state.login) ++ "@" ++ binary_to_list(State#state.host) ++ "/" ++ binary_to_list(State#state.resource),
    % Make room
    [Room | _] = string:tokens(binary_to_list(State#state.room), "/"),
    % send message to jabber
    gen_tcp:send(State#state.socket, xmpp_xml:message(From, Room, Message)),
    % return
    {noreply, State};

%% @doc connect to jabber server
handle_cast({connect, Host}, State) ->
    % connect
    case gen_tcp:connect(binary_to_list(Host), ?PORT, []) of
        {ok, Socket} ->
            % handshake with jabber server
            gen_tcp:send(Socket, ?STREAM(binary_to_list(Host))),
            % Format login/password
            Auth = binary_to_list(base64:encode("\0" ++ binary_to_list(State#state.login) ++ "\0" ++ binary_to_list(State#state.password))),
            % Send authorization (PLAIN method)
            gen_tcp:send(Socket, xmpp_xml:auth_plain(Auth)),
            % init
            {noreply, State#state{socket = Socket}};
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Incoming message
handle_info({tcp, _Socket, Data}, State) ->
    try
        % Parse incoming xml
        {Xml, _} = xmerl_scan:string(Data),
        % Check auth state
        case State#state.is_auth of
            % Authorized
            true ->
                % Catch incoming jabber message
                case xmerl_xpath:string("/message", Xml) of
                    [] ->
                        % Not find
                        {noreply, State};
                    _ ->
                        % Get message body
                        case xmerl_xpath:string("/message/body/text()", Xml) of
                            [{xmlText, _, _, _, IncomingMessage, text}] ->
                                % Send message to callback
                                State#state.callback ! {incoming_message, IncomingMessage}
                        end,
                        {noreply, State}
                end;
            % Not authorized
            false ->
                % Got success authorization
                case xmerl_xpath:string("/success", Xml) of
                    [] ->
                        {noreply, State};
                    _ ->
                        % create new stream
                        gen_tcp:send(State#state.socket, ?STREAM(binary_to_list(State#state.host))),
                        % bind resource
                        gen_tcp:send(State#state.socket, xmpp_xml:bind(binary_to_list(State#state.resource))),
                        % create session
                        gen_tcp:send(State#state.socket, xmpp_xml:create_session()),
                        % Join to muc
                        gen_tcp:send(State#state.socket, xmpp_xml:muc(binary_to_list(State#state.room))),
                        % set is_auth = true and return
                        {noreply, State#state{is_auth = true}}
                end
        end
    catch _ : _ ->
        {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.