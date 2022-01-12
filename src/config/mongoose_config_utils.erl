%% @doc Short functions useful for config file manipulations.
%% This stuff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([exit_or_halt/1]).

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
