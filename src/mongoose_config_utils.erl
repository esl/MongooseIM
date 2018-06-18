%% @doc Short functions useful for config file manipulations.
%% This stiff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([is_file_readable/1]).

-include_lib("kernel/include/file.hrl").

-spec is_file_readable(Path :: string()) -> boolean().
is_file_readable(Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            case {FileInfo#file_info.type, FileInfo#file_info.access} of
                {regular, read} -> true;
                {regular, read_write} -> true;
                _ -> false
            end;
        {error, _Reason} ->
            false
    end.

