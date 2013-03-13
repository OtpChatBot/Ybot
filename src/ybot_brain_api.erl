%%%----------------------------------------------------------------------------
%%% File    : ybot_brain_api.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------------
-module(ybot_brain_api).

-include("ybot.hrl").

%% REST Callbacks
-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
%         resource_exists/2,
         post_is_create/2,
         create_path/2,
         create_json/2,
         get_json/2,
         delete_resource/2,
         delete_completed/2
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

post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    {ybot_utils:get_uuid(), Req, State}.

create_json(Req, State) ->
    {Id, Req1} = cowboy_req:meta(put_path, Req),
    {Method, Req2} = cowboy_req:method(Req1),
    {ok, Body, Req3} = cowboy_req:body(Req2),
    Json = from_json(Body),
    lager:info("debug:create_json: method=~p, id=~p, json=~p", [Method, Id, Json]),
    case validate_json(Json) of
        true ->
            {struct, List} = Json,
            case Method of
                <<"POST">> ->
                    % handle POST
                    ybot_brain:post(
                      Id,
                      get_value(<<"plugin">>, List),
                      get_value(<<"key">>, List),
                      get_value(<<"value">>, List)
                     );
               <<"PUT">> ->
                    % handle PUT
                    ybot_brain:put(
                      Id,
                      get_value(<<"plugin">>, List),
                      get_value(<<"key">>, List),
                      get_value(<<"value">>, List)
                     )
                end,
            {true, Req3, State};
        false ->
            lager:error("Unable to create memoery. Invalid JSON=~p", [Json]),
            {false, Req3, State}
    end.

get_json(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    {Params, Req2} = cowboy_req:qs_vals(Req1),
    Reply = case Path of
                <<"/">> ->
                   get_by_params(Params);
                <<"/favicon.ico">> ->
                   [];
                Id ->
                   get_by_id(Id)
            end,
    {to_json(Reply), Req2, State}.

delete_resource(Req, State) ->
    lager:info("api:delete", []),
    {true, Req, State}.

delete_completed(Req, State) ->
    lager:info("api:delete_completed", []),
    {true, Req, State}.

%% Internal functions
validate_json(Json) ->
    try
      {struct,
       [{<<"plugin">>, _},
        {<<"key">>, _},
        {<<"value">>, _}
       ]
      } = Json,
      true
    catch
        _:_Reason ->
            false
    end.

get_by_id(<<$/,Id/binary>>) ->
    to_json(
      serialize(
        ybot_brain:get_by_id(ybot_utils:hex_to_bin(Id))
       )
     ).

get_by_params([]) ->
   to_json(
     serialize(ybot_brain:get_all())
    );
get_by_params([{<<"plugin">>, Plugin}]) ->
   to_json(
     serialize(
       ybot_brain:get_by_plugin(Plugin)
      )
    );
get_by_params([{<<"key">>, Key}]) ->
   to_json(
     serialize(
       ybot_brain:get_by_key(Key)
      )
    );
get_by_params([{<<"value">>, Value}]) ->
   to_json(
     serialize(
       ybot_brain:get_by_value(Value)
      )
    );
get_by_params([{<<"plugin">>, Plugin}, {<<"key">>, Key}]) ->
   to_json(
     serialize(
       ybot_brain:get(Plugin, Key)
      )
    ).

% Helpers
from_json(Input) ->
    mochijson2:decode(ybot_utils:to_list(Input)).

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

format_datetime({{Y,M,D},{H,Mi,S}}) ->
    list_to_binary(
      lists:flatten(
        io_lib:format("~4B-~2..0B-~2..0B ~2B:~2..0B:~2..0B",
                      [Y, M, D, H, Mi, S])
     )
     ).

to_json(Input) ->
    ybot_utils:to_binary(mochijson2:encode(Input)).

get_value(Key, List) ->
    proplists:get_value(Key, List).
