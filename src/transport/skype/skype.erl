%%%----------------------------------------------------------------------
%%% File    : ../transport/skype/skype.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot skype handler.
%%%----------------------------------------------------------------------
-module(skype).

-behaviour(gen_server).
 
-export([start_link/1]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
 %% @doc internal state
-record(state, {
        % skype pid
        skype_pid = 0
    }).
 
start_link(Command) ->
    process_flag(trap_exit, true),

    gen_server:start_link(?MODULE, [Command], []).
 
init([Command]) ->
    % start python script
    gen_server:cast(self(), {command, Command}),
    % init
    {ok, #state{}}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc start skype script
handle_cast({command, Command}, State) ->
    % Run
    Port = open_port({spawn, Command}, [exit_status]),
    % get pid
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    % Save pid and return
    {noreply, State#state{skype_pid = OsPid}};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(Info, State) ->
    io:format("Info ~p~n", [Info]),
    {noreply, State}.
 
terminate(_Reason, State) ->
    % Kill skype pid
    os:cmd(io_lib:format("kill -9 ~p", [State#state.skype_pid])),
    % return
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions