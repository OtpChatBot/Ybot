%%%----------------------------------------------------------------------
%%% File    : ybot_brain_api.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------
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
         post_json/2,
         put_json/2,
         get_json/2,
         delete_json/2
        ]).

%% API
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, put_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, get_json}
     ], Req, State}.

%% resource_exists(Req, _State) ->
%%     case cowboy_req:binding(paste_id, Req) of
%%         {undefined, Req2} ->
%%             {true, Req2, index};
%%         {PasteID, Req2} ->
%%             case valid_path(PasteID) and file_exists(PasteID) of
%%                 true -> {true, Req2, PasteID};
%%                 false -> {false, Req2, PasteID}
%%             end
%%     end.
resource_exists(Req, State)->
    lager:info("api:resource_exists", []),
    {true, Req, index}.

post_is_create(Req, State) ->
    lager:info("api:post_is_create", []),
    {true, Req, State}.

create_path(Req, State) ->
    lager:info("api:create_path", []),
    {ybot_utils:get_uuid(), Req, State}.

put_json(Req, State) ->
    {Id, Req1} = cowboy_req:meta(put_path, Req),
    {ok, [{Json, true}], Req2} = cowboy_req:body_qs(Req1),
    lager:info("api:put id=~p, json=~p", [Id, from_json(Json)]),
    {true, Req2, State}.

post_json(Req, State) ->
    {ok, [Json], Req1} = cowboy_req:body_qs(Req),
    lager:info("api:post json=~p", [from_json(Json)]),
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

delete_json(Req, State) ->
    lager:info("api:delete", []),
    {true, Req, State}.

%% process_post(Req, State) ->
%%     {true, Req, State}.

%% delete_resource(Req, State) ->
%%     {true, Req, State}.

%% delete_completed(Req, State) ->
%%     {true, Req, State}.

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
