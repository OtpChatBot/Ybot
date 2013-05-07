%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Ybot memory plugin using OTP
%%% @end
%%% Created : 20 Mar 2013 by tgrk <martin@wiso.cz>
%%%-----------------------------------------------------------------------------
-module(ybot_plugin_memory).

%% API
-export([start/0,
         stop/0,
         execute/1
        ]).

%%%=============================================================================
%%% API
%%%=============================================================================
start() ->
    application:start(ybot_plugin_memory).

stop() ->
    application:stop(ybot_plugin_memory).

%% @doc execute plugin action
execute([]) ->
    handle_command(list, [], []);

execute(Input) ->
    io:format("Start to execute ~n"),
    case re:split(Input, " ", [{return, list}]) of
        [] ->
            "No input";
        [[], "help"] ->
            handle_command(help, [], []);
        [[], "list"] ->
            io:format("Is list ~n"),
            handle_command(list, [], []);
        [[], Key] ->
            handle_command(list, Key, []);
        [[], Cmd, Key] ->
            handle_command(list_to_atom(Cmd), Key, []);
        [[], Cmd, Key | Value] ->
            handle_command(list_to_atom(Cmd), Key, string:join(Value, " "));
        [[], Key | Value] ->
            handle_command(add, Key, string:join(Value, " "))
    end.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================

%% @doc handle plugin command
handle_command(help, [], []) ->
    "Usage: memory add key value\n"
        "       memory list\n"
        "       memory delete key\n";

handle_command(delete, Key, []) ->
    case ybot_brain:get_by_key(to_bin(Key)) of
        [] ->
            "Key is not found";
        [{memory, Id, _, _, _, _} | _Rest] ->
            ybot_brain:delete(Id),
            "Memory deleted."
    end;

handle_command(add, _Key, []) ->
    "Not complete memory";

handle_command(add, Key, Value) ->
    ybot_brain:post(<<"memory">>, to_bin(Key), to_bin(Value)),
    "New memory added.";

handle_command(list, [], []) ->
    case memories_to_list(ybot_brain:get_by_plugin(<<"memory">>)) of
        [] ->
            % memory empty
            "My memory is empty.";
        Mem ->
            Mem
    end;

handle_command(list, Key, []) ->
    case memories_to_list(ybot_brain:get_by_key(to_bin(Key))) of
        [] ->
            % memory empty
            "There is nothing about " ++ Key ++ " in memory";
        Mem ->
            Mem
    end.

memories_to_list(Memories) ->
     lists:flatten(lists:map(fun memory_to_list/1, Memories)).

memory_to_list({memory, _Id, _Plugin, Key, Value, _Created}) ->
     to_list(Key) ++ " = " ++ to_list(Value) ++ "\n".

%% @doc utils functions
to_list(List) when is_list(List) ->
    List;
to_list(Bin) ->
    binary_to_list(Bin).

to_bin(List) when is_binary(List) ->
    List;
to_bin(List) ->
    list_to_binary(List).