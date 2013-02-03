%%%----------------------------------------------------------------------
%%% File    : ybot_utils.erl
%%% Author  : 0xAX <anotherworldofworld@gmail.com>
%%% Purpose : Ybot utils functions
%%%----------------------------------------------------------------------
-module(ybot_utils).

-export([get_all_files/1,
         get_priv_dir/0,
         split_at_end/2,
         get_uuid/0,

         to_binary/1,
         to_atom/1,
         to_list/1
        ]).

%% @doc get all files from directory
-spec get_all_files(Dir :: string()) -> [string()].
get_all_files(Dir) ->
    FindFiles = fun(F, Acc) -> [F | Acc] end,
    filelib:fold_files(Dir, ".*", true, FindFiles, []).

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

%% @doc Get unique id
-spec get_uuid() -> binary().
get_uuid() ->
    <<(crypto:rand_bytes(8))/bytes,
      (erlang:term_to_binary(erlang:now()))/bytes>>.

%% @doc Ensures that is binary
-spec to_binary(any()) -> binary().
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X)).

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
