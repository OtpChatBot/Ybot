%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot flowdock incoming message handler.
%%% @end
%%%-----------------------------------------------------------------------------

-module(flowdock_handler).

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
        flowdock_nick = <<>> :: binary(),
        % flowdock client process pid
        flowdock_client_pid :: pid(),
        % process parser pid
        parser_pid :: pid()
    }).
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({flowdock_client, ClientPid, ParserPid, Login}, State) ->
    {noreply, State#state{flowdock_client_pid = ClientPid, parser_pid = ParserPid, flowdock_nick = Login}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc handle incoming message
handle_info({incoming_message, IncomingMessage}, State) ->
    % Get Ybot Nick from current chat
    Nick = binary_to_list(State#state.flowdock_nick),
    % Send message to parser
    gen_server:cast(State#state.parser_pid, {incoming_message, State#state.flowdock_client_pid, Nick, "", binary_to_list(IncomingMessage)}),
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions