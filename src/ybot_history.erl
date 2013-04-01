%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Provides Ybot history storage.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_history).
 
-behaviour(gen_server).
 
-export([start_link/1, stop/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

 %% @doc internal state
-record(state, {
    % history command limit
    limit = 0,
    % command history list
    history = []
    }).

%%%=============================================================================
%%% API
%%%============================================================================= 

start_link(HistoryLimit) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [HistoryLimit], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%=============================================================================
%%% ybot_history callbacks
%%%=============================================================================

init([HistoryLimit]) ->
    % init state
    {ok, #state{limit = HistoryLimit}}.

%% @doc return command history
handle_call({get_history, Transport}, _From, State) ->
    % Get history for Transport
    History = lists:flatten([Command || {Tr, Command}<- State#state.history, Tr == Transport]),
    % return history
    {reply, History, State};
 
%% @doc return history limit
handle_call(get_history_limit, _From, State) -> 
    {reply, State#state.limit, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% update history limit
handle_cast({update_history_limit, HistoryLimit}, State) ->
    {noreply, State#state{limit = HistoryLimit}};

%% @doc stop history process
handle_cast(stop, State) ->
    {stop, normal, State};

%% @doc update history
handle_cast({update_history, From, Command}, State) ->
    % Check limit
    case length([{Transport, CommandHistory} || {Transport, CommandHistory} <- State#state.history, Transport == From]) >= State#state.limit of
        true ->
            % Save history
            {noreply, State#state{history = [{From, Command}]}};
        false ->
            {noreply, State#state{history = lists:append(State#state.history, [{From, Command}])}}
    end;

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