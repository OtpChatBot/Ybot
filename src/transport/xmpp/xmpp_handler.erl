%%%----------------------------------------------------------------------
%%% File    : transport/xmpp/xmpp_handler.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Xmpp messages handler
%%%----------------------------------------------------------------------
-module(xmpp_handler).

-behavior(gen_server).

-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
% state
-record(state, {
        % xmpp bot nick
        nick = "",
        % xmpp client pid
        xmpp_client_pid :: pid(),
        % parser process pid
        parser_pid :: pid()
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({xmpp_client, ClientPid, ParserPid, BotNick}, State) ->
    % Get first symbol of bot nick name
    [NickSymb | _] = binary_to_list(BotNick),
    % Check this symbol. If it's '@' this is Hipchat.
    Nick = case NickSymb of
        $@ ->
            % return nick
            binary_to_list(BotNick);
        _ ->
            % get nick part
            [TempNick | _] = string:tokens(binary_to_list(BotNick), "@"),
            % return nick
            TempNick
    end,
    % save xmpp client pid
    {noreply, State#state{xmpp_client_pid = ClientPid, parser_pid = ParserPid, nick = Nick}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, From, IncomingMessage}, #state{nick = Nick} = State) ->
    % Send message to parser
    gen_server:cast(State#state.parser_pid, {incoming_message, State#state.xmpp_client_pid, Nick, From, IncomingMessage}),
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions