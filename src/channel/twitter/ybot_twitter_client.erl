%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot twitter client.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_twitter_client).
 
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
        % consumer key
        consumer = <<>>,
        % consumer secret
        consumer_secret = <<>>,
        % access token
        access_token = <<>>,
        % access token secret
        access_token_secret = <<>>
    }).
 
start_link(Consumer, ConsumerSecret, AccessToken, AccessTokenSecret) ->
    gen_server:start_link(?MODULE, [Consumer, ConsumerSecret, AccessToken, AccessTokenSecret], []).

%% @doc update twitter status
-spec send_message(TwitterClient :: pid(), Messsage :: string()) -> ok.
send_message(TwitterClient, Messsage) ->
    % send message
    gen_server:cast(TwitterClient, {send_message, "", Messsage}).

init([Consumer, ConsumerSecret, AccessToken, AccessTokenSecret]) ->
    % init interanal state
    {ok, #state{consumer = Consumer, consumer_secret = ConsumerSecret, 
                access_token = AccessToken, access_token_secret = AccessTokenSecret}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({send_message, _From, Messsage}, State) ->
    Consumer = {binary_to_list(State#state.consumer, binary_to_list(State#state.consumer_secret), hmac_sha1)},
    % update twitter status
    oauth:post("https://api.twitter.com/1/statuses/update.json", [{"status", Messsage}], Consumer, 
               binary_to_list(State#state.access_token), binary_to_list(State#state.access_token_secret)),
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