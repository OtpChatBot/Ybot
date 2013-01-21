%%%----------------------------------------------------------------------
%%% File    : ../transport/campfire/capfire_client.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot campfire client.
%%%----------------------------------------------------------------------
-module(campfire_client).

-behaviour(gen_server).
 
-export([start_link/4]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
        % handler module
        callback = null,
        % campfire room id
        room = 0,
        % campfire api token
        token = <<>>,
        % campfire sub_domain
        domain = <<>>,
        % request id
        req_id = null
    }).

start_link(CallbackModule, Room, Token, Domain) ->
    gen_server:start_link(?MODULE, [CallbackModule, Room, Token, Domain], []).
 
init([CallbackModule, Room, Token, Domain]) ->
    % Start http stream
    gen_server:cast(self(), stream),
    % init state
    {ok, #state{callback = CallbackModule, room = Room, token = Token, domain = Domain}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({send_message, Message}, State) ->
    % Make url
    Url = "https://" ++ binary_to_list(State#state.domain) ++ ".campfirenow.com/room/" ++ integer_to_list(State#state.room) ++ "/speak.xml",
    % Make message
    Mes = "<message><body>" ++ Message ++ "</body></message>",
    % Send message
    ibrowse:send_req(Url, [{"Content-Type", "application/xml"}, {basic_auth, {binary_to_list(State#state.token), "x"}}], post, Mes),
    % return
    {noreply, State};

%% @doc Start http campfire stream
handle_cast(stream, State) ->
    % First of all leave from campfire room
    ok = leave_room(State#state.domain, State#state.room, State#state.token),
    % Now try to join to the room
    ok = join_room(State#state.domain, State#state.room, State#state.token),
    % Make url for stream
    Url = "https://streaming.campfirenow.com/room/" ++ integer_to_list(State#state.room) ++ "/live.json",
    % Start campfire stream
    {_, ReqId} = ibrowse:send_req(Url, [{"Content-Type", "application/json"}], get, 
                                  [], [{basic_auth, {binary_to_list(State#state.token), "x"}}, {stream_to, {self(), once}}], infinity),
    % Save request id
    {noreply, State#state{req_id = ReqId}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ibrowse_async_headers, ReqId, _Status, _Headers}, State) ->
    % Next stream
    ibrowse:stream_next(ReqId),
    % return
    {noreply, State};

%% @doc
handle_info({ibrowse_async_headers, ReqId, _Body}, State) ->
    ibrowse:stream_next(ReqId),
    {noreply, State};

%% @doc Timeout request error
handle_info({ibrowse_async_response, _OldReqId, {error, req_timedout}}, State) ->
    % Make url for new Stream
    Url = "https://streaming.campfirenow.com/room/" ++ integer_to_list(State#state.room) ++ "/live.json",
    % Start new stream
    {_, ReqId} = ibrowse:send_req(Url, [{"Content-Type", "application/json"}], get, 
                                  [], [{basic_auth, {binary_to_list(State#state.token), "x"}}, {stream_to, {self(), once}}]),
    % Save new request id
    {noreply, State#state{req_id = ReqId}};

%% @doc
handle_info({ibrowse_async_response, ReqId, Data}, State) ->
    ok = ibrowse:stream_next(ReqId),
    % Parse response
    case Data of
      " " ->
          {noreply, State};
      _ ->
          % Send message to handler
          State#state.callback ! {incoming_message, Data},
          % Return state
          {noreply, State}
    end;

handle_info({ibrowse_async_response_end, _ReqId}, State) ->
    {noreply, State};

%% @doc connection timeout
handle_info({error, {conn_failed, {error, _}}}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions
leave_room(Domain, Room, Token) ->
    % Make url
    Url = "https://" ++ binary_to_list(Domain) ++ ".campfirenow.com/room/" ++ integer_to_list(Room) ++ "/leave.json",
    % Make content type
    ContentType = [{"Content-Type", "application/json"}],
    % send leave request
    ibrowse:send_req(Url, ContentType, post, [], [{basic_auth, {binary_to_list(Token), "x"}}]),
    % return
    ok.

%% @doc Join to room
join_room(Domain, Room, Token) ->
    % Make url
    Url = "https://" ++ binary_to_list(Domain) ++ ".campfirenow.com/room/" ++ integer_to_list(Room) ++ "/join.json",
    % Make content type
    ContentType = [{"Content-Type", "application/json"}],
    % Send join request
    ibrowse:send_req(Url, ContentType, post, [], [{basic_auth, {binary_to_list(Token), "x"}}]),
    % return
    ok.
   
