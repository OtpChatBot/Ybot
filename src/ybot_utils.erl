-module(ybot_utils).

-export([get_all_files/1,
		 splitAtEnd/2]).

%% @doc get all files from directory
-spec get_all_files(Dir :: string()) -> [string()].
get_all_files(Dir) ->
    FindFiles = fun(F, Acc) -> [F | Acc] end,
    filelib:fold_files(Dir, ".*", true, FindFiles, []).

%% @doc Take 2 string. Find `SplitSnippet` in `String` 
%%      and return all string content which after `SplitSnippet` in string.
%% @example:
%%
%% > estring:splitAtEnd("Hello, my name is 0xAX", "name").
%%   >> " is 0xAX"
%%
-spec splitAtEnd(String :: string(), SplitSnippet :: string()) -> string().
splitAtEnd(String, SplitSnippet) ->
	StartPosition = string:str(String, SplitSnippet),
    SplitSnippetLength = length(SplitSnippet),
    string:substr(String, StartPosition + SplitSnippetLength).