%%%----------------------------------------------------------------------
%%% File    : ../transport/http/http_server.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot http server.
%%%----------------------------------------------------------------------
-module(http_server).
 
-behaviour(gen_server).
 
-export([start_link/2]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("inets/include/httpd.hrl").

 %% @doc internal state
-record(state, {
        % bot nick
        nick = "" :: string()
    }).
 
start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).
 
init([Host, Port]) ->
    % start server
    ok = gen_server:cast(self(), {start_serve, Host, Port}),
    % init state
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc start serve
handle_cast({start_serve, Host, Port}, State) ->    
    % cowboy dispatches
    Dispatch = cowboy_router:compile([
        {binary_to_list(Host), [{'_', http_handler, []}]}
    ]),

    {ok, _} = cowboy:start_http(my_http_listener, 3, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),

    % return
    {noreply, State};

%% @doc Set bot nick
handle_cast({bot_nick, BotNick}, State) ->
    {noreply, State#state{nick = binary_to_list(BotNick)}};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions
