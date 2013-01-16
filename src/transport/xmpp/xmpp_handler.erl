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
        xmpp_client_pid :: pid()
    }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({xmpp_client, ClientPid, BotNick}, State) ->
    % save xmpp client pid
    {noreply, State#state{xmpp_client_pid = ClientPid, nick = BotNick}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, IncomingMessage}, State) ->
    Nick = binary_to_list(State#state.nick),
    % Check this is message for Ybot or not
    case string:tokens(IncomingMessage, " \r\n") of
        [Nick, Command | Args] ->
            % Start process with supervisor which will be execute plugin and send to pid
            ybot_actor:start_link(State#state.xmpp_client_pid, Command, Args);
        _ ->
            % this is not our command
            pass
    end,
    % return
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions