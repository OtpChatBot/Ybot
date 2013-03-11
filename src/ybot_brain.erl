%%%----------------------------------------------------------------------
%%% File    : ybot_brain.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------
-module(ybot_brain).

-behaviour(gen_server).

-include("ybot.hrl").

-export([post/3,
         put/4,
         delete/2,
         get_by_uuid/1,
         get/0,
         get/1,
         get/2,

         start_link/1,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {storage=undefined}).


%% API
start_link(Type) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Type], []).

stop() ->
    gen_server:cast({local,?MODULE}, stop).

post(Plugin, Key, Value) ->
    gen_server:call(?MODULE, {post, [Plugin, Key, Value]}).

put(Id, Plugin, Key, Value) ->
    gen_server:call(?MODULE, {put, [Id, Plugin, Key, Value]}).

get_by_uuid(Id) ->
    gen_server:call(?MODULE, {get_by_uuid, [Id]}).

get() ->
    gen_server:call(?MODULE, get).

get(Plugin) ->
    gen_server:call(?MODULE, {get, [Plugin]}).

get(Plugin, Key) ->
    gen_server:call(?MODULE, {get, [Plugin, Key]}).

delete(Plugin, Key) ->
    gen_server:call(?MODULE, {delete, [Plugin, Key]}).

%% gen_server callbacks
init([Type]) ->
    Db = start_storage(Type),
    {ok, #state{storage=Db}}.

handle_call({post, [Plugin, Key, Value]}, _From, #state{storage=Db}=State) ->
    {reply, Db:post(Plugin, Key, Value), State};
handle_call({put, [Id, Plugin, Key, Value]}, _From, #state{storage=Db}=State) ->
    {reply, Db:put(Id, Plugin, Key, Value), State};
handle_call({get_by_uuid, [Id]}, _From, #state{storage=Db}=State) ->
    {reply, Db:get_by_uuid(Id), State};
handle_call(get, _From, #state{storage=Db}=State) ->
    {reply, Db:get(), State};
handle_call({get, [Plugin]}, _From, #state{storage=Db}=State) ->
    {reply, Db:get(Plugin), State};
handle_call({get, [Plugin, Key]}, _From, #state{storage=Db}=State) ->
    {reply, Db:get(Plugin, Key), State};
handle_call({delete, [Plugin, Key]}, _From, #state{storage=Db}=State) ->
    {reply, Db:delete(Plugin, Key), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, #state{storage=Db}=State) ->
    Db:stop(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{storage=Db}) ->
    Db:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
start_storage(Type) ->
    % select and start brain storage backend
    Db = case Type of
             mnesia ->
                 ybot_brain_mnesia;
             _ ->
                 %TODO - use memory only?
                 lager:error("Unknown brain storage module - ~s", [Type]),
                 undefined
         end,
    Db:start(),
    lager:info("Brain ~s storage started", [Type]),
    Db.
