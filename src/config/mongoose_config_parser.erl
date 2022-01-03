%% @doc Parsing and processing of MongooseIM config files
%%   - parser backends: only 'toml'
%%   - config state management
-module(mongoose_config_parser).

%% parser API
-export([parse_file/1]).

%% state API
-export([new_state/0,
         set_opts/2,
         set_hosts/2,
         set_host_types/2,
         get_opts/1,
         state_to_opts/1,
         state_to_hosts/1,
         state_to_host_types/1,
         state_to_global_opt/3]).

%% config post-processing
-export([dedup_state_opts/1,
         post_process_modules/1]).

-ignore_xref([behaviour_info/1, get_opts/1,
              state_to_global_opt/3, state_to_host_types/1, state_to_hosts/1]).

-callback parse_file(FileName :: string()) -> state().

-include("mongoose.hrl").

-export_type([state/0]).

-record(state, {opts = [] :: list(),
                hosts = [] :: [domain_name()],
                host_types = [] :: [host_type()]}).

-type domain_name() :: jid:server().
-type host_type() :: mongooseim:host_type().
-type state() :: #state{}.

%% Parser API

-spec parse_file(FileName :: string()) -> state().
parse_file(FileName) ->
    ParserModule = parser_module(filename:extension(FileName)),
    try
        ParserModule:parse_file(FileName)
    catch
        error:{config_error, ExitMsg, Errors} ->
            halt_with_msg(ExitMsg, Errors)
    end.

%% Only the TOML format is supported
parser_module(".toml") -> mongoose_config_parser_toml.

%% State API

-spec new_state() -> state().
new_state() ->
    #state{}.

-spec set_opts(Opts :: list(), state()) -> state().
set_opts(Opts, State) ->
    State#state{opts = Opts}.

-spec set_hosts([domain_name()], state()) -> state().
set_hosts(Hosts, State) ->
    State#state{hosts = Hosts}.

-spec set_host_types([host_type()], state()) -> state().
set_host_types(HostTypes, State) ->
    State#state{host_types = HostTypes}.

-spec get_opts(state()) -> list().
get_opts(State) ->
    State#state.opts.

%% @doc Final getter - reverses the accumulated options.
-spec state_to_opts(state()) -> list().
state_to_opts(#state{opts = Opts}) ->
    lists:reverse(Opts).

-spec state_to_hosts(state()) -> [domain_name()].
state_to_hosts(#state{hosts = Hosts}) ->
    Hosts.

-spec state_to_host_types(state()) -> [host_type()].
state_to_host_types(#state{host_types = HostTypes}) ->
    HostTypes.

-spec state_to_global_opt(OptName :: atom(), state(), Default :: any()) -> any().
state_to_global_opt(OptName, State, Default) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, OptName, Default).

opts_to_global_opt([{config, OptName, OptValue}|_], OptName, _Default) ->
    OptValue;
opts_to_global_opt([_|Opts], OptName, Default) ->
    opts_to_global_opt(Opts, OptName, Default);
opts_to_global_opt([], _OptName, Default) ->
    Default.

%% Config post-processing

-spec dedup_state_opts(state()) -> state().
dedup_state_opts(State = #state{opts = RevOpts}) ->
    {RevOpts2, _Removed} = dedup_state_opts_list(RevOpts, [], [], sets:new()),
    State#state{opts = RevOpts2}.

dedup_state_opts_list([{K, _V} = H|List], Removed, Keep, Set) ->
    case sets:is_element(K, Set) of
        true ->
            dedup_state_opts_list(List, [H|Removed], Keep, Set);
        false ->
            Set1 = sets:add_element(K, Set),
            dedup_state_opts_list(List, Removed, [H|Keep], Set1)
    end;
dedup_state_opts_list([], Removed, Keep, _Set) ->
    {Keep, Removed}.

-spec post_process_modules(state()) -> state().
post_process_modules(State = #state{opts = Opts}) ->
    Opts2 = lists:map(fun post_process_modules_opt/1, Opts),
    State#state{opts = Opts2}.

post_process_modules_opt({{modules, HostType}, Modules}) ->
    ModulesWithDeps = gen_mod_deps:resolve_deps(HostType, Modules),
    {{modules, HostType}, unfold_opts(ModulesWithDeps)};
post_process_modules_opt(Other) ->
    Other.

unfold_opts(Modules) ->
    maps:map(fun(_Mod, Opts) -> proplists:unfold(Opts) end, Modules).

%% local functions

-spec halt_with_msg(string(), [any()]) -> no_return().
-ifdef(TEST).
halt_with_msg(ExitMsg, Errors) ->
    error({config_error, ExitMsg, Errors}).
-else.
halt_with_msg(ExitMsg, Errors) ->
    [?LOG_ERROR(Error) || Error <- Errors],
    mongoose_config_utils:exit_or_halt(ExitMsg).
-endif.
