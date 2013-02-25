%%%----------------------------------------------------------------------
%%% File    : ybot_brain_api.erl
%%% Author  : tgrk <tajgur@gmail.com>
%%% Purpose : Ybot brain REST API
%%%----------------------------------------------------------------------
-module(ybot_brain_api).

%% REST Callbacks
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).


init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, paste_text}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_paste}],
     Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(paste_id, Req) of
        {undefined, Req2} ->
            {true, Req2, index};
        {PasteID, Req2} ->
            case valid_path(PasteID) and file_exists(PasteID) of
                true -> {true, Req2, PasteID};
                false -> {false, Req2, PasteID}
            end
    end.
