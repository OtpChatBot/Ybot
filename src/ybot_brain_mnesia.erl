%%%----------------------------------------------------------------------
%%% File    : ybot_brain_mnesia.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot Mnesia brain storage
%%%----------------------------------------------------------------------
-module(ybot_brain_mnesia).

-include("ybot.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([post/3,
         post/4,
         put/4,
         delete/1,
         get_by_id/1,
         get_by_key/1,
         get_by_value/1,
         get_by_plugin/1,
         get_all/0,
         get/2,

         start/0,
         stop/0
         ]).

%% API
start() ->
    case mnesia:create_schema([node()]) of
        {error, {_,{already_exists, _}}} ->
            % if there is already ram_copies schema
            case mnesia:system_info(local_tables) of
                [schema] ->
                    mnesia:change_table_copy_type(schema, node(), disc_copies),
                    create_schema();
                _        ->
                    already_created
            end,
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
                  #memory{
                     uuid = ybot_brain_api:get_uuid(),
                     plugin = Plugin,
                     key = Key,
                     value = Value,
                     created = erlang:localtime()
                    }
                 )
        end).

post(Id, Plugin, Key, Value) ->
    run(fun() ->
                mnesia:write(
                  #memory{
                     uuid = Id,
                     plugin = Plugin,
                     key = Key,
                     value = Value,
                     created = erlang:localtime()
                    }
                 )
        end).

put(Id, Plugin, Key, Value) ->
    run(fun() ->
                [R] = mnesia:wread({memory, Id}),
                mnesia:write(
                  R#memory{
                    plugin = Plugin,
                    key = Key,
                    value = Value,
                    created = erlang:localtime()
                   }
                 )
        end).

delete(Id) ->
    run(fun() -> mnesia:delete({memory, Id}) end).

get_by_id(Id) ->
    run(fun() -> mnesia:wread({memory, Id}) end).

get_all() ->
    run(fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(memory)])) end).

get_by_plugin(Plugin) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.plugin =:= Plugin])
                 )
        end).

get_by_key(Key) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.key =:= Key])
                 )
        end).

get_by_value(Value) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.value =:= Value])
                 )
        end).

get(Plugin, Key) ->
    run(fun() ->
                qlc:eval(
                  qlc:q([X || X <- mnesia:table(memory),
                              X#memory.plugin =:= Plugin,
                              X#memory.key =:= Key
                        ])
                 )
         end).

%% Internal functions
start_db() ->
    case mnesia:start() of
        {error, Error1} ->
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
            lager:error("Unable to run brain query! Error ~p", [Reason]),
            {error, Reason}
    end.
