%%%----------------------------------------------------------------------
%%% File    : ybot_brain_api.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------
-module(ybot_brain_api).

%% REST Callbacks
-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         post_is_create/2,
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
    {true, Req, State}.

post_is_create(Req, State) ->
    {true, Req, State}.

put_json(Req, State) ->
    lager:info("api:put", []),
    {true, Req, State}.

post_json(Req, State) ->
    lager:info("api:post", []),
    {true, Req, State}.

get_json(Req, State) ->
    lager:info("api:get", []),
    {true, Req, State}.

delete_json(Req, State) ->
    lager:info("api:delete", []),
    {true, Req, State}.

%% process_post(Req, State) ->
%%     {true, Req, State}.

%% delete_resource(Req, State) ->
%%     {true, Req, State}.

%% delete_completed(Req, State) ->
%%     {true, Req, State}.
