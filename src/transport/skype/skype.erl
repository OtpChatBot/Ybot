%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot skype handler.
%%% @end
%%%-----------------------------------------------------------------------------
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
    open_port({spawn, Command}, [exit_status]),
    % return
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    % return
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions