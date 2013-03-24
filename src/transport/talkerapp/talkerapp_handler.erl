%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot talkerapp incoming message handler.
%%% @end
%%%-----------------------------------------------------------------------------
-module(talkerapp_handler).

-behaviour(gen_server).
 
-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        % bot nick in handler room
        bot_nick = <<>> :: binary(),
        % talker_app client process pid
        client_pid :: pid(),
        % parser process pid
        parser_pid :: pid()
    }).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%=============================================================================
%%% talker_app handler callbacks
%%%=============================================================================

init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({talkerapp_client, ClientPid, ParserPid, Login}, State) ->
    {noreply, State#state{client_pid = ClientPid, parser_pid = ParserPid, bot_nick = Login}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, IncomingMessage}, State) ->
    % Get Ybot Nick from current chat
    Nick = binary_to_list(State#state.bot_nick),
    % Send message to parser
    gen_server:cast(State#state.parser_pid, {incoming_message, State#state.client_pid, Nick, "", binary_to_list(IncomingMessage)}),
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