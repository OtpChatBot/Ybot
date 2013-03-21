%%%----------------------------------------------------------------------
%%% File    : ybot_parser.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot incoming message parser.
%%%----------------------------------------------------------------------
-module(ybot_parser).
 
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
 
start_link() ->
    gen_server:start_link(?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%-----------------------------------------------------------------------------
%% @doc parse incoming message and execute plugin if command is successful
%% @param incoming_message :: atom()
%% @param TransportPid :: pid()
%% @param Nick :: string()
%% @param From :: string()
%% @param IncomingMessage :: string()
%%-----------------------------------------------------------------------------
handle_cast({incoming_message, TrasportPid, Nick, From, IncomingMessage}, State) ->
    % Check this is message for Ybot or not
    case string:tokens(IncomingMessage, " \r\n") of
        [Nick] ->    
            gen_server:cast(TrasportPid, {send_message, From, "What?"});
        [Nick, "announce" | _] ->
            % Get announce content
            Announce = string:tokens(ybot_utils:split_at_end(IncomingMessage, "announce"), "\r\n"),
            % send announce to all chat
            ybot_utils:broadcast(Announce);
        [Nick1, "hi"] -> maybe_respond({Nick1, Nick}, fun() ->
            gen_server:cast(TrasportPid, {send_message, From, "Hello"})
        end);
        [Nick1, "hello"] -> maybe_respond({Nick1, Nick}, fun() ->
            gen_server:cast(TrasportPid, {send_message, From, "Hi!"})
        end);
        [Nick1, "bye"] -> maybe_respond({Nick1, Nick}, fun() ->
            gen_server:cast(TrasportPid, {send_message, From, "Good bue"}) 
        end);
        [Nick1, "history"] -> maybe_respond({Nick1, Nick}, fun() ->
            % Check ybot_history process
            case whereis(ybot_history) of
                undefined ->
                    gen_server:cast(TrasportPid, {send_message, From, "History is disabled"});
                _ ->
                    % Get history
                    History = gen_server:call(ybot_history, {get_history, TrasportPid}),
                    % Check history
                    case History of
                        [] ->
                            gen_server:cast(TrasportPid, {send_message, From, "The history is empty"});
                        _ ->
                            % Send history
                            gen_server:cast(TrasportPid, {send_message, From, History})
                    end
            end
        end);
        [Nick1, "thanks"] -> maybe_respond({Nick1, Nick}, fun() ->
            gen_server:cast(TrasportPid, {send_message, From, "by all means"})
        end);
        [Nick1, "plugins?"] -> maybe_respond({Nick1, Nick}, fun() ->
            % Get plugins
            Plugins = gen_server:call(ybot_manager, get_plugins),
            % Format plugins
            PluginNames = lists:map(fun({_, _, Pl, _}) -> Pl end, Plugins),
            % Send plugins
            gen_server:cast(TrasportPid, {send_message, From, "Plugins: " ++ string:join(PluginNames, ", ")}),
            % That's all :)
            gen_server:cast(TrasportPid, {send_message, From, "That's all :)"}) 
        end);
        [Nick1, Command | _] -> maybe_respond({Nick1, Nick}, fun() ->
                % Get command arguments
                Args = string:tokens(ybot_utils:split_at_end(IncomingMessage, Command), "\r\n"),
                % Start process with supervisor which will be execute plugin and send to pid
                ybot_actor:start_link(TrasportPid, From, Command, Args) 
        end);
        _ ->
            % this is not our command
            pass
    end,
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
maybe_respond({FirstWord, YNick}, F) when is_list(YNick) and is_list(FirstWord) and is_function(F) ->
    if
        YNick == FirstWord ->
            F();
        true ->
            [LastLetter | DrowTsrif] = lists:reverse(FirstWord),
            % Check last permissible symbol
            CheckLastLEtter = lists:member(LastLetter, [$,, $:, 32]),
            % Validate
            case ((DrowTsrif == lists:reverse(YNick)) or (YNick == FirstWord)) and (CheckLastLEtter == true) of
                true -> 
                    F();
                _ -> 
                    pass
            end
    end.