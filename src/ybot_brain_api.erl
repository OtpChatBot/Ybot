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
         process_post/2,
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

%% resource_exists(Req, State)->
%%     lager:info("api:resource_exists", []),
%%     {true, Req, index}.

post_is_create(Req, State) ->
    {true, Req, State}.

created_path(Req, State) ->
    lager:info("api:created_path", []),
    {ybot_utils:get_uuid(), Req, State}.

create_json(Req, State) ->
    {Id, Req1} = cowboy_req:meta(put_path, Req),
    {ok, Body, Req2} = cowboy_req:body(Req1),
    lager:info("api:put id=~p, json=~p", [Id, from_json(Body)]),
    {true, Req2, State}.

process_post(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    lager:info("api:post json=~p", [from_json(Body)]),
    {true, Req1, State}.

get_json(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    lager:info("api:get - path=~p", [Path]),
    Reply = case Path of
                <<"/">> -> [];
                <<"/favicon.ico">> -> [];
                _ ->
                    get_resource(string:tokens(Path, "/"))
            end,
    {to_json(Reply), Req1, State}.

delete_resource(Req, State) ->
    lager:info("api:delete", []),
    {true, Req, State}.

delete_completed(Req, State) ->
    lager:info("api:delete_completed", []),
    {true, Req, State}.

%% Internal functions
get_resource([Plugin]) ->
    % get all keys for a plugin
    "";
get_resource([Plugin, Key]) ->
    % get a specific memory
    "".

% Helpers
from_json(Input) ->
    mochijson2:decode(binary_to_list(Input)).

to_json(Input) ->
    list_to_binary(mochijson2:encode(Input)).
