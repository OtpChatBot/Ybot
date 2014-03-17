%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot web admin main process.
%%% @end
%%%-----------------------------------------------------------------------------
-module(web_admin).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% web admin process callbacks
%% ===================================================================

init([]) ->
    % start server
    ok = gen_server:cast(self(), start_serve),
    % init internal state
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(start_serve, State) ->
    % Get web admin config
    {ok, WebAdmin} = application:get_env(ybot, web_admin),

    % Get Host
    {webadmin_host, Host} = lists:keyfind(webadmin_host, 1, WebAdmin),

    % Get Port
    {webadmin_port, Port} = lists:keyfind(webadmin_port, 1, WebAdmin),

    % Cowboy dispatch
    Dispatch = cowboy_router:compile([
        {binary_to_list(Host),
         [
          {"/css/[...]", cowboy_static,
           {dir, docroot("css"), [{mimetypes, cow_mimetypes, all}]}},
          {"/js/[...]", cowboy_static,
           {dir, docroot("js"), [{mimetypes, cow_mimetypes, all}]}},
          {"/views/[...]", cowboy_static,
           {dir, docroot("views"), [{mimetypes, cow_mimetypes, all}]}},
          {"/", web_admin_req_handler, []}
        ]}
    ]),
    % start serving
    {ok, _} = cowboy:start_http(web_http_listener, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
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

%% ===================================================================
%% Internal functions
%% ===================================================================
docroot(Append) ->
    priv_dir() ++ "webadmin/" ++ Append.

priv_dir() ->
    case code:priv_dir(ybot) of
        {error, bad_name} ->
            {ok, Cwd} = file:get_cwd(),
            Cwd ++ "/" ++ "priv/";
        Priv ->
            Priv ++ "/"
    end.
