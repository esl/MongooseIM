%% @doc Short functions useful for config file manipulations.
%% This stuff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([exit_or_halt/1]).
-export([is_file_readable/1]).
-export([get_absolute_path/1]).
-export([get_config_lines/4]).

-type config_line() :: [[any()] | non_neg_integer(), ...]. % spec me better

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

%% @doc Convert configuration filename to absolute path.
%% Input is an absolute or relative path to an ejabberd configuration file.
%% And returns an absolute path to the configuration file.
-spec get_absolute_path(string()) -> string().
get_absolute_path(File) ->
    case filename:pathtype(File) of
        absolute ->
            File;
        relative ->
            {ok, Cwd} = file:get_cwd(),
            filename:absname_join(Cwd, File)
    end.

-spec get_config_lines(Filename :: string(),
                       TargetNumber :: integer(),
                       PreContext :: 10,
                       PostContext :: 3) -> [config_line()].
get_config_lines(Filename, TargetNumber, PreContext, PostContext) ->
    {ok, Fd} = file:open(Filename, [read]),
    LNumbers = lists:seq(TargetNumber - PreContext, TargetNumber + PostContext),
    NextL = io:get_line(Fd, no_prompt),
    R = get_config_lines2(Fd, NextL, 1, LNumbers, []),
    file:close(Fd),
    R.

get_config_lines2(_Fd, eof, _CurrLine, _LNumbers, R) ->
    lists:reverse(R);
get_config_lines2(_Fd, _NewLine, _CurrLine, [], R) ->
    lists:reverse(R);
get_config_lines2(Fd, Data, CurrLine, [NextWanted | LNumbers], R) when is_list(Data) ->
    NextL = io:get_line(Fd, no_prompt),
    case CurrLine >= NextWanted of
        true ->
            Line2 = [integer_to_list(CurrLine), ": " | Data],
            get_config_lines2(Fd, NextL, CurrLine + 1, LNumbers, [Line2 | R]);
        false ->
            get_config_lines2(Fd, NextL, CurrLine + 1, [NextWanted | LNumbers], R)
    end.
