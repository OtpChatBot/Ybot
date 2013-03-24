%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot transports options validator.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_validators).

%% Public API
-export([validate_transport_opts/1]).

-type opt() :: integer() | binary() | string() | [].

%% @doc Validate transport options
-spec validate_transport_opts(Options :: [{atom(), opt()}]) -> ok | wrong_options.
validate_transport_opts([]) ->
    wrong_options;
validate_transport_opts(Options) when not is_list(Options) ->
    wrong_options;
validate_transport_opts(Options) ->
    % Get irc opts
    PortOpts = lists:keyfind(port, 1, Options),
    % Get ssl opts
    SslOpts = lists:keyfind(use_ssl, 1, Options),
    % Check options
    if (PortOpts == false) or (SslOpts == false) ->
        wrong_options;
    true ->
        % get port      
        {port, Port} = PortOpts,
        % get ssl
        {use_ssl, Ssl} = SslOpts,
        % Check this parameters
        if (not is_integer(Port)) == false or (not is_boolean(Ssl) == false) ->
            wrong_options;
        true ->
            ok
        end
    end.