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
         created_path/2,
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

created_path(Req, State) ->
    lager:info("api:created_path", []),
    {ybot_utils:get_uuid(), Req, State}.

create_json(Req, State) ->
    {Id, Req1} = cowboy_req:meta(put_path, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    Json = from_json(Body),
    case validate_json(Json) of
        true ->
            {struct, List} = Json,
            case Id of
                undefined ->
                    % handle POST
                    ybot_brain:post(
                      get_value(<<"plugin">>, List),
                      get_value(<<"key">>, List),
                      get_value(<<"value">>, List)
                     );
                _ ->
                    % handle PUT
                    ybot_brain:put(
                      Id,
                      get_value(<<"plugin">>, List),
                      get_value(<<"key">>, List),
                      get_value(<<"value">>, List)
                     )
                end,
            {true, Req2, State};
        false ->
            lager:error("Unable to create memoery. Invalid JSON=~p", [Json]),
            {false, Req2, State}
    end.

get_json(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    lager:info("api:get - path=~p", [Path]),
    {Plugin, Req2} = get_param(<<"plugin">>, Req1),
    lager:info("api:get - plugin=~p", [Plugin]),
    Reply = case Path of
                <<"/">> -> [];
                <<"/favicon.ico">> -> [];
                Id -> get_resource(ybot_utils:to_list(Id))
            end,
    {to_json(Reply), Req2, State}.

delete_resource(Req, State) ->
    lager:info("api:delete", []),
    {true, Req, State}.

delete_completed(Req, State) ->
    lager:info("api:delete_completed", []),
    {true, Req, State}.

%% Internal functions
get_resource(Id) ->
    % get all keys for a plugin
    "".

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

% Helpers
from_json(Input) ->
    mochijson2:decode(ybot_utils:to_list(Input)).

to_json(Input) ->
    ybot_utils:to_binary(mochijson2:encode(Input)).

get_param(Key, Req) ->
    cowboy_req:qs_val(Key, Req).

get_value(Key, List) ->
    proplists:get_value(Key, List).
