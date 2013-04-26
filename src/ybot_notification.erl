%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot notification manager
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_notification).
 
-behaviour(gen_server).
 
-export([start_link/2]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {}).
 
start_link(Notification, NotificationsDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Notification, NotificationsDir], []).
 
init([Notification, NotificationsDir]) ->
    % init notification manager
    gen_server:cast(?MODULE, {init, Notification, NotificationsDir}),
    % return
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({init, Notification, NotificationsDir}, State) ->
    % traverse all notifications and start notification handler
    lists:foreach(fun(Not) ->
                      % start notification handler
                      ybot_notification_sup:start_notification_proc(Not, NotificationsDir)
                  end, 
                  Notification),
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