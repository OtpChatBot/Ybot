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

execute([]) ->
    handle_command(list, [], []);
execute(Input) ->
    case re:split(Input, " ", [{return, list}]) of
        [] ->
            "No input";
        [[], Cmd] ->
            handle_command(list_to_atom(Cmd), [], []);
        [[], Cmd, Key] ->
            handle_command(list_to_atom(Cmd), Key, []);
        [[], Cmd, Key | Value] ->
            Value1 = string:join(Value, " "),
            handle_command(list_to_atom(Cmd), Key, Value1);
        [[], Key | Value] ->
            Value1 = string:join(Value, " "),
            handle_command(add, Key, Value1)
    end.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
handle_command(help, [], []) ->
    "Usage: memory add key value\n"
        "       memory list\n"
        "       memory delete key\n";
handle_command(delete, Key, []) ->
    case ybot_brain:get_by_key(to_bin(Key)) of
        [] ->
            "Not found";
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
    Memories = ybot_brain:get_by_plugin(<<"memory">>),
    lists:flatten(
      lists:map(fun({memory, _Id, _Plugin, Key, Value, _Created}) ->
                      to_list(Key) ++ " = " ++ to_list(Value) ++ "\n"
              end, Memories)
     ).

to_list(Bin) ->
    binary_to_list(Bin).

to_bin(List) when is_binary(List) ->
    List;
to_bin(List) ->
    list_to_binary(List).
