%%%----------------------------------------------------------------------
%%% File    : ybot_sup.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot Mnesia brain storage
%%%----------------------------------------------------------------------
-module(ybot_brain_mnesia).

-include("ybot.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([post/3,
         put/3,
         delete/2,
         get_by_uuid/1,
         get/0,
         get/1,
         get/2,

         start/0,
         stop/0
         ]).

%% API
start() ->
    case mnesia:create_schema([node()]) of
        {error, {_,{already_exists, _}}} ->
            start_db();
        {error, Error} ->
            exit(Error);
        ok ->
            start_db(),
            create_schema(),
            ok
    end.

stop() ->
    mnesia:stop().

post(Plugin, Key, Value) ->
    run(fun() ->
                mnesia:write(
                  #memory{uuid = ybot_utils:get_uuid(),
                          plugin = ybot_utils:to_atom(Plugin),
                          key = ybot_utils:to_binary(Key),
                          value = Value,
                          created = erlang:localtime()
                         }
                 )
        end).

put(Plugin, Key, Value) ->
    case get(Plugin, Key) of
        [] -> unknown_item;
        Item ->
            run(fun() ->
                        mnesia:write(
                          Item#memory{value=Value,
                                      created=erlang:localtime()
                                     }
                         )
                end)
    end.

delete(Plugin, Key) ->
    case get(Plugin, Key) of
        [] -> unknown_item;
        Item ->
            run(fun() -> mnesia:delete({memory, Item#memory.uuid}) end)
    end.

get_by_uuid(Id) ->
    run(fun() -> mnesia:wread({memory, Id}) end).

get() ->
    run(fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(memory)])) end).

get(Plugin) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.plugin =:= ybot_utils:to_atom(Plugin)])
                 )
        end).

get(Plugin, Key) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.plugin =:= ybot_utils:to_atom(Plugin),
                              X#memory.key =:= ybot_utils:to_binary(Key)
                        ])
                 )
         end).

%% Internal functions
start_db() ->
    case mnesia:start() of
        {error, Error1}  ->
            exit(Error1);
        ok -> ok
    end.

create_schema() ->
    mnesia:create_table(memory,
                        [{attributes, record_info(fields, memory)},
                         {index, [plugin, key]},
                         {disc_copies, [node()]}
                        ]).

run(ExecFun) ->
    try
        {atomic, State} = mnesia:transaction(ExecFun),
        State
    catch
        _:Reason ->
            {error, Reason}
    end.
