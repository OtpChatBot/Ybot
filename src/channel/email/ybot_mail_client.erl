%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot mail client.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_mail_client).
 
-behaviour(gen_server).
 
%% api 
-export([start_link/4, send_message/2]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        from = <<>>,
        fromPassword = <<>>,
        to = [],
        connectionOptions = []
    }).

start_link(From, FromPassword, To, Options) ->
    gen_server:start_link(?MODULE, [From, FromPassword, To, Options], []).

%% @doc send mail message
-spec send_message(SmtpClientPid :: pid(), Messsage :: string()) -> ok.
send_message(SmtpClientPid, Messsage) ->
    % send message
    gen_server:cast(SmtpClientPid, {send_message, "", Messsage}).

init([From, FromPassword, To, Options]) ->
    % init interanal state
    {ok, #state{from = From, fromPassword = FromPassword, to =To, connectionOptions = Options}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({send_message, _From, Messsage}, State) ->
    % send mail message
    lists:foreach(fun(To) -> 
                      gen_smtpc:send({binary_to_list(State#state.from), binary_to_list(State#state.fromPassword)}, 
                                      To, "Message from Ybot chat robot", Messsage, State#state.connectionOptions)
                  end, 
                  State#state.to),
    % return
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions