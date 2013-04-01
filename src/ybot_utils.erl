%%%-----------------------------------------------------------------------------
%%% @author 0xAX <anotherworldofworld@gmail.com>
%%% @doc
%%% Ybot utils functions.
%%% @end
%%%-----------------------------------------------------------------------------
-module(ybot_utils).

%% Public API
-export([get_all_files/1,
         get_all_directories/1,
         get_priv_dir/0,
         split_at_end/2,
         to_binary/1,
         to_atom/1,
         to_list/1,
         to_int/1,
         broadcast/1,
         get_config_val/1,
         get_config_val/2
        ]).

%% @doc get all files from directory
-spec get_all_files(Dir :: string()) -> [string()].
get_all_files(Dir) ->
    FindFiles = fun(F, Acc) -> [F | Acc] end,
    filelib:fold_files(Dir, ".*", false, FindFiles, []).

%% @doc get all sub directories from directory
-spec get_all_directories(Pathx :: string()) -> [string()].
get_all_directories(Path) ->
    lists:filter(fun(X) -> filelib:is_dir(X) end, filelib:wildcard(Path ++ "/*")).

%% @doc Take 2 string. Find SplitSnippet in String
%%      and return all string content which after SplitSnippet in string.
%% example:
%%
%% > estring:split_at_end("Hello, my name is 0xAX", "name").
%%   >> " is 0xAX"
%%
-spec split_at_end(String :: string(), SplitSnippet :: string()) -> string().
split_at_end(String, SplitSnippet) ->
    StartPosition = string:str(String, SplitSnippet),
    SplitSnippetLength = length(SplitSnippet),
    string:substr(String, StartPosition + SplitSnippetLength).

%% @doc Get priv directory path
-spec get_priv_dir() -> string().
get_priv_dir() ->
    % get current dir
    {ok, Cwd} = file:get_cwd(),
    % Return priv dir
    Cwd ++ "/priv/".

%% @doc Ensures that is binary
-spec to_binary(any()) -> binary().
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_binary(X) -> X.

%% @doc Ensures that is atom
-spec to_atom(any()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_binary(X) -> list_to_atom(binary_to_list(X)).

%% @doc Ensures that is list
-spec to_list(any()) -> list().
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> mochinum:digits(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% @doc Ensures that this is integer
-spec to_int(any()) -> integer().
to_int(X) when is_binary(X) -> list_to_integer(binary_to_list(X));
to_int(X) when is_integer(X) -> X;
to_int(X) when is_float(X) -> list_to_integer(float_to_list(X));
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(_) -> 0.

%% @doc Send Body to all chats
-spec broadcast(any()) -> ok.
broadcast(Body) ->
    % Get all runned transports pid list
    Transports = gen_server:call(ybot_manager, get_runnned_transports),
    % Send to messages
    lists:foreach(fun(TransportPid) ->
                    % Send message
                    gen_server:cast(TransportPid,
                                    {send_message, "", binary_to_list(Body)}
                                   )
                  end,
                  Transports).

%% get parameter value from config
get_config_val(Param) ->
   get_config_val(Param, []).

get_config_val(Param, DefaultValue) ->
    case application:get_env(ybot, Param) of
        {_, HistoryLimit} ->
            HistoryLimit;
        _ ->
            DefaultValue
    end.
