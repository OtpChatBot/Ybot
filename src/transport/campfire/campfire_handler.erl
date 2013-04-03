%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot campfire incoming message handler.
%%% @end
%%%-----------------------------------------------------------------------------
-module(campfire_handler).

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
        % bot nick in campfire room
        campfire_nick = <<>> :: binary(),
        % campfire client process pid
        campfire_client_pid :: pid(),
        % parser process pid
        parser_pid :: pid()
    }).
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({campfire_client, ClientPid, ParserPid, Login}, State) ->
    {noreply, State#state{campfire_client_pid = ClientPid, parser_pid = ParserPid, campfire_nick = Login}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_message, IncomingMessage}, State) ->
    % Get Ybot Nick from current chat
    Nick = binary_to_list(State#state.campfire_nick),
    % Decode json message
    Message = lists:flatten(string:tokens(IncomingMessage, "\r\n")),
    {Json} = try 
                jiffy:decode(Message)
             catch _ : _ ->
                {""}
             end,
    % Get body
    case get_body(Json) of
        [] ->
            % do nothing
            pass;
        Body ->
            % Send message to parser
            gen_server:cast(State#state.parser_pid, {incoming_message, State#state.campfire_client_pid, Nick, "", binary_to_list(Body)})
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
get_body([]) ->
    [];
get_body([H | Json]) ->
    case H of
        {<<"body">>, Body} ->
            Body;
        _ ->
            get_body(Json)
    end.