%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Handler of irc messages.
%%% @end
%%%-----------------------------------------------------------------------------
-module(irc_handler).

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
        % irc bot nick
        nick = <<>> :: binary(),
        % irc client pid
        irc_client_pid :: pid(),
        % command parser process
        parser_pid :: pid()
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({irc_client, ClientPid, ParserPid, BotNick}, State) ->
    % save irc client pid
    {noreply, State#state{irc_client_pid = ClientPid, parser_pid = ParserPid, nick = BotNick}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Receive incoming message from irc chat
handle_info({incoming_message, IncomingMessage, From}, State) ->
    % Get bot chat nick
    Nick = binary_to_list(State#state.nick),
    % Send message to parser
    gen_server:cast(State#state.parser_pid, {incoming_message, State#state.irc_client_pid, Nick, From, IncomingMessage}),
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
