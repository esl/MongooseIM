%% @doc Short functions useful for config file manipulations.
%% This stuff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([exit_or_halt/1]).
-export([section_to_defaults/1]).
-export([keys_are_binaries/1]).
-ignore_xref([section_to_defaults/1]).

-include("mongoose_config_spec.hrl").

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

section_to_defaults(#section{defaults = Defaults}) ->
    Defaults.

keys_are_binaries(KV) ->
    maps:from_list([{atom_to_binary(K, latin1), V} || {K, V} <- maps:to_list(KV)]).
