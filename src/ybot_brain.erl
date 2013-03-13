%%%----------------------------------------------------------------------
%%% File    : ybot_brain.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------
-module(ybot_brain).

-behaviour(gen_server).

-include("ybot.hrl").

-export([post/4,
         put/4,
         delete/1,
         get_by_id/1,
         get_by_key/1,
         get_by_value/1,
         get_by_plugin/1,
         get_all/0,
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

post(Id, Plugin, Key, Value) ->
    gen_server:call(?MODULE, {post, [Id, Plugin, Key, Value]}).

put(Id, Plugin, Key, Value) ->
    gen_server:call(?MODULE, {put, [Id, Plugin, Key, Value]}).

get_by_id(Id) ->
    gen_server:call(?MODULE, {get_by_id, [Id]}).

get_all() ->
    gen_server:call(?MODULE, get_all).

get(Plugin, Key) ->
    gen_server:call(?MODULE, {get, [Plugin, Key]}).

get_by_plugin(Plugin) ->
    gen_server:call(?MODULE, {get_by_plugin, [Plugin]}).

get_by_key(Key) ->
    gen_server:call(?MODULE, {get_by_key, [Key]}).

get_by_value(Value) ->
    gen_server:call(?MODULE, {get_by_value, [Value]}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, [Id]}).

%% gen_server callbacks
init([Type]) ->
    Db = start_storage(Type),
    {ok, #state{storage=Db}}.

handle_call({post, [Id, Plugin, Key, Value]}, _From,
            #state{storage = Db} = State) ->
    {reply, Db:post(Id, Plugin, Key, Value), State};
handle_call({put, [Id, Plugin, Key, Value]}, _From,
            #state{storage = Db} = State) ->
    {reply, Db:put(Id, Plugin, Key, Value), State};
handle_call({get_by_id, [Id]}, _From, #state{storage = Db} = State) ->
    {reply, Db:get_by_id(Id), State};
handle_call({get_by_key, [Key]}, _From, #state{storage = Db} = State) ->
    {reply, Db:get_by_key(Key), State};
handle_call(get_all, _From, #state{storage = Db} = State) ->
    {reply, Db:get_all(), State};
handle_call({get_by_plugin, [Plugin]}, _From, #state{storage = Db} = State) ->
    {reply, Db:get_by_plugin(Plugin), State};
handle_call({get_by_value, [Value]}, _From, #state{storage = Db} = State) ->
    {reply, Db:get_by_value(Value), State};
handle_call({get, [Plugin, Key]}, _From, #state{storage = Db} = State) ->
    {reply, Db:get(Plugin, Key), State};
handle_call({delete, [Id]}, _From, #state{storage = Db} = State) ->
    {reply, Db:delete(Id), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, #state{storage = Db} = State) ->
    Db:stop(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{storage = Db}) ->
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
