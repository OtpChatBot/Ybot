%%%----------------------------------------------------------------------
%%% File    : ybot_manager.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot main manager. Run transport, load plugins.
%%%----------------------------------------------------------------------
-module(ybot_manager).

-behaviour(gen_server).
 
-export([start_link/2]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {
       % ybot transports list (irc, xmpp and etc..)
       % Example : [{irc, ClientPid, HandlerPid, Nick, Channel, Host}]
       transports = [],
       % Ybot active plugins list
       plugins = [] :: [{plugin, Source :: string(), PluginName :: string(), Path :: string()}]
    }).
 
start_link(PluginsDirectory, Transports) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PluginsDirectory, Transports], []).
 
init([PluginsDirectory, Transports]) ->
    % init plugins
    ok = gen_server:cast(?MODULE, {init_plugins, PluginsDirectory}),
    % Start transports
    ok = gen_server:cast(?MODULE, {start_transports, Transports}),
    % init
    {ok, #state{}}.

%% @doc Get plugin metadata by plugin name
handle_call({get_plugin, PluginName}, _From, State) ->
    case lists:keyfind(PluginName, 3, State#state.plugins) of
        false ->
            % there is no plugin with `PluginName`
            {reply, wrong_plugin, State};
        Plugin ->
            % return plugin with metadata
            {reply, Plugin, State}
    end;

%% @doc Return all plugins
handle_call(get_plugins, _From, State) ->
    {reply, State#state.plugins, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Init active plugins
handle_cast({init_plugins, PluginsDirectory}, State) ->
    % Get all plugins
    Plugins = ybot_utils:get_all_files(PluginsDirectory),
    % Parse plugins and load to state
    PluginsList = lists:flatten(lists:map(fun load_plugin/1, Plugins)),
    % init plugins
    {noreply, State#state{plugins = PluginsList}};

%% @doc Run transports from `Transports` list
handle_cast({start_transports, Transports}, State) ->
    % Review supported mode of transportation
    TransportList 
        = lists:flatten(
            lists:map(fun(Trans) ->
                          case element(1, Trans) of
                              irc ->
                                  % Get irc params
                                  {irc, Nick, Channel, Host} = Trans,
                                  % Start irc handler
                                  {ok, HandlerPid} = irc_handler:start_link(),
                                  % Run new irc client
                                  {ok, ClientPid} = irc_lib_sup:start_irc_client(HandlerPid, Host, Channel, Nick),
                                  % Log
                                  lager:info("Starting IRC transport: ~s, ~p, ~s", [Host, Channel, Nick]),
                                  % send client pid to handler
                                  ok = gen_server:cast(HandlerPid, {irc_client, ClientPid, Nick}),
                                  % return correct transport
                                  {irc, ClientPid, HandlerPid, Nick, Channel, Host};
                              xmpp ->
                                  % Get xmpp params
                                  {xmpp, Login, Password, Room, Host, Resource} = Trans,
                                  % Start xmpp handler
                                  {ok, HandlerPid} = xmpp_handler:start_link(),
                                  % Run new xmpp client
                                  {ok, ClientPid} = xmpp_sup:start_xmpp_client(HandlerPid, Login, Password, Host, Room, Resource),
                                  % Send client pid to handler
                                  ok = gen_server:cast(HandlerPid, {xmpp_client, ClientPid, Login}),
                                  % return correct transport
                                  {xmpp, ClientPid, HandlerPid, Login, Password, Host, Room, Resource};
                              campfire ->
                                  % Get campfire params
                                  {campfire, Login, Token, RoomId, CampfireSubDomain} = Trans,
                                  % Start campfire handler
                                  {ok, HandlerPid} = campfire_handler:start_link(),
                                  % Run new campfire client
                                  {ok, ClientPid} = campfire_sup:start_campfire_client(HandlerPid, RoomId, Token, CampfireSubDomain),
                                  % Send client pid to handler
                                  ok = gen_server:cast(HandlerPid, {campfire_client, ClientPid, Login}),
                                  % return correct transport
                                  {campfire, ClientPid, HandlerPid};
                              _ ->
                                  []
                          end
                      end,
                      Transports)),
    % return
    {noreply, State#state{transports = TransportList}};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
load_plugin(Plugin) ->
    % Get plugin extension
    Ext = filename:extension(Plugin),
    Name = filename:basename(Plugin, Ext),
    % Match extension
    case Ext of
        ".py" ->
            % python plugin
            lager:info("Loading plugin(Python): ~s", [Name]),
            {plugin, "python", Name, Plugin};
        ".rb" ->
            % ruby plugin
            lager:info("Loading plugin(Ruby): ~s", [Name]),
            {plugin, "ruby", Name, Plugin};
        ".sh" ->
            % shell plugin
            lager:info("Loading plugin(Shell): ~s", [Name]),
            {plugin, "sh", Name, Plugin};
        ".pl" ->
            % perl plugin
            lager:info("Loading plugin(Perl) ~s", [Name]),
            {plugins, "perl", Name, Plugin};
        ".ex" ->
            % elixir plugin
            lager:info("Loading plugin(Elixir) ~s", [Name]),
            {plugins, "elixir", Name, Plugin};
        _ ->
            % this is wrong plugin
            lager:info("Unsupported plugin type: ~s", [Ext]),
            []
    end.
