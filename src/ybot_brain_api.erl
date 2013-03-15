%%%----------------------------------------------------------------------------
%%% File    : ybot_brain_api.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------------
%%% TODO
%%%----------------------------------------------------------------------------
%%% * operations on collections
%%% * write docs into README
%%% * handle errors
%%%----------------------------------------------------------------------------
-module(ybot_brain_api).

-include("ybot.hrl").

%% REST Callbacks
-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         post_is_create/2,
         create_path/2,
         create_json/2,
         get_json/2,
         delete_resource/2
        ]).

%% API
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, create_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, get_json}
     ], Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(memory_id, Req) of
        {undefined, Req2} ->
            {true, Req2, all};
        {Id, Req2} ->
            {true, Req2, ybot_utils:hex_to_bin(Id)}
    end.

post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    {ybot_utils:get_uuid(), Req, State}.

create_json(Req, Id) ->
    try
        {Method, Req1} = cowboy_req:method(Req),
        {ok, Body, Req2} = cowboy_req:body(Req1),
        Json = from_json(Body),
        lager:info("debug:create_json: method=~p, id=~p, json=~p",
                   [Method, Id, Json]),
        Items = deserialize(Json),
        [store(Method, Id, Item) || Item <- Items],
        {true, Req2, Id}
    catch
        _:Reason ->
            lager:error("Unable to create memoery. Invalid JSON!"
                        "Error=~p", [Reason]),
            {false, Req, Id}
    end.

get_json(Req, all) ->
    {Params, Req1} = cowboy_req:qs_vals(Req),
    lager:info("debug:get_json: params=~p", [Params]),
    {get_by_params(Params), Req1, all};
get_json(Req, Id) ->
    {get_by_id(Id), Req, Id}.

delete_resource(Req, Id) ->
    lager:info("api:delete id=~p", [Id]),
    ybot_brain:delete(Id),
    {true, Req, Id}.


%% Internal functions
store(<<"POST">>, undefind, List) ->
    ybot_brain:post(get_value(<<"plugin">>, List),
                    get_value(<<"key">>, List),
                    get_value(<<"value">>, List)
                   );
store(<<"POST">>, Id, List) ->
    ybot_brain:post(Id,
                    get_value(<<"plugin">>, List),
                    get_value(<<"key">>, List),
                    get_value(<<"value">>, List)
                   );
store(<<"PUT">>, Id, List) ->
    ybot_brain:put(Id,
                   get_value(<<"plugin">>, List),
                   get_value(<<"key">>, List),
                   get_value(<<"value">>, List)
                  ).

get_by_id(Id) ->
    case ybot_brain:get_by_id(Id) of
        [] -> false;
        [Item] -> to_json(serialize(Item))
    end.

get_by_params([]) ->
    to_json(
      serialize(ybot_brain:get_all())
     );
get_by_params([{<<"plugin">>, Plugin}]) ->
    to_json(
      serialize(ybot_brain:get_by_plugin(Plugin))
     );
get_by_params([{<<"key">>, Key}]) ->
    to_json(
      serialize(ybot_brain:get_by_key(Key))
     );
get_by_params([{<<"value">>, Value}]) ->
    to_json(
      serialize(ybot_brain:get_by_value(Value))
     );
get_by_params([{<<"plugin">>, Plugin}, {<<"key">>, Key}]) ->
    to_json(
      serialize(ybot_brain:get(Plugin, Key))
     ).

% Helpers
from_json(Input) ->
    mochijson2:decode(ybot_utils:to_list(Input)).

%TODO - check how mochijson handles arrays
deserialize(Collection) when is_list(Collection) ->
    [deserialize(I) || I <- Collection];
deserialize(Item) ->
    case validate_json(Item) of
        true  -> {struct, List} = Item,
                 List;
        false ->
            throw({invalid_json, Item})
    end.

serialize(Records) when is_list(Records) ->
    [serialize(R) || R <- Records];
serialize(Record) ->
    {struct,[
             {id,      ybot_utils:bin_to_hex(Record#memory.uuid)},
             {plugin,  Record#memory.plugin},
             {key,     Record#memory.key},
             {value,   Record#memory.plugin},
             {created, format_datetime(Record#memory.created)}
            ]}.

validate_json({struct, [{<<"plugin">>, _}, {<<"key">>, _},
                        {<<"value">>, _}]}) ->
    true;
validate_json({struct, [{<<"id">>, _}, {<<"plugin">>, _}, {<<"key">>, _},
                        {<<"value">>, _}]}) ->
    true;
validate_json(_Other) ->
    false.

to_json(Input) ->
    ybot_utils:to_binary(mochijson2:encode(Input)).

get_value(Key, List) ->
    proplists:get_value(Key, List).

format_datetime({{Y,M,D},{H,Mi,S}}) ->
    list_to_binary(
      lists:flatten(
        io_lib:format("~4B-~2..0B-~2..0B ~2B:~2..0B:~2..0B",
                      [Y, M, D, H, Mi, S])
       )
     ).
