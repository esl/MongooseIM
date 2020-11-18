%% @doc Short functions useful for config file manipulations.
%% This stuff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([exit_or_halt/1]).
-export([is_file_readable/1]).

-include_lib("kernel/include/file.hrl").

%% @doc If MongooseIM isn't yet running in this node, then halt the node
-spec exit_or_halt(ExitText :: string()) -> none().
exit_or_halt(ExitText) ->
    case [Vsn || {mongooseim, _Desc, Vsn} <- application:which_applications()] of
        [] ->
            timer:sleep(1000),
            halt(string:substr(ExitText, 1, 199));
        [_] ->
            exit(ExitText)
    end.

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
