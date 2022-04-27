%% @doc Short functions useful for config file manipulations.
%% This stuff can be pure, but most likely not.
%% It's for generic functions.
-module(mongoose_config_utils).
-export([exit_or_halt/1, section_to_defaults/1, merge_sections/2]).

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

-spec merge_sections(mongoose_config_spec:config_section(),
                     mongoose_config_spec:config_section()) ->
          mongoose_config_spec:config_section().
merge_sections(BasicSection, ExtraSection) ->
    #section{items = Items1, required = Required1, defaults = Defaults1,
             process = Process1} = BasicSection,
    #section{items = Items2, required = Required2, defaults = Defaults2,
             process = Process2} = ExtraSection,
    BasicSection#section{items = maps:merge(Items1, Items2),
                         required = Required1 ++ Required2,
                         defaults = maps:merge(Defaults1, Defaults2),
                         process = merge_process_functions(Process1, Process2)}.

merge_process_functions(Process1, Process2) ->
    fun(Path, V) ->
            V1 = mongoose_config_parser_toml:process(Path, V, Process1),
            mongoose_config_parser_toml:process(Path, V1, Process2)
    end.
