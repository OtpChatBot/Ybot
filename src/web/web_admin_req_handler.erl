%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot web admin requests handler.
%%% @end
%%%-----------------------------------------------------------------------------
-module(web_admin_req_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
	% Get method
	{Method, Req2} = cowboy_req:method(Req),
	case Method of
		<<"GET">> ->
			% Get query string
			{QS, Req3} = cowboy_req:qs(Req2),
			% Parse query string
			case string:tokens(binary_to_list(QS), "=") of
				% Get data for front page
				["req", "main_web_interface_req"] ->
					% Get runned transports
					Transports = [{element(1, Transport), 
					               element(2, Transport), 
					               element(4, Transport)} || Transport <- ybot:get_runned_transports(), element(1, Transport) /= http],
					% Get history
					% Get observer
					% Get storage
					% @TODO get Ybot info and send to webadmin
					cowboy_req:reply(200, [], <<"Hi">>, Req3);
				% Wrong requests
				_ ->
					cowboy_req:reply(200, [], <<"Wrong request">>, Req3)
			end;
		_ ->
			cowboy_req:reply(200, [], <<"Wrong request">>, Req)
	end,
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.