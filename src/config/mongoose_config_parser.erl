%% @doc Parsing and processing of MongooseIM config files
%%   - parser backends: only 'toml'
%%   - config state management
-module(mongoose_config_parser).

%% parser API
-export([parse_file/1]).

%% state API
-export([build_state/3]).

%% only for tests
-export([get_opts/1]).

-ignore_xref([get_opts/1]).

-callback parse_file(FileName :: string()) -> state().

-include("mongoose.hrl").

-export_type([state/0]).

-record(state, {opts = [] :: opts(),
                hosts = [] :: [domain_name()],
                host_types = [] :: [mongooseim:host_type()]}).

-type opts() :: [{mongoose_config:key(), mongoose_config:value()}].
-type domain_name() :: jid:server().
-type state() :: #state{}.

%% Parser API

-spec parse_file(FileName :: string()) -> opts().
parse_file(FileName) ->
    ParserModule = parser_module(filename:extension(FileName)),
    try ParserModule:parse_file(FileName) of
        State ->
            get_opts(State)
    catch
        error:{config_error, ExitMsg, Errors} ->
            halt_with_msg(ExitMsg, Errors)
    end.

%% Only the TOML format is supported
parser_module(".toml") -> mongoose_config_parser_toml.

%% State API

-spec build_state([jid:server()], [jid:server()], opts()) -> state().
build_state(Hosts, HostTypes, Opts) ->
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                new_state(),
                [fun(S) -> set_hosts(Hosts, S) end,
                 fun(S) -> set_host_types(HostTypes, S) end,
                 fun(S) -> set_opts(Opts, S) end,
                 fun unfold_globals/1,
                 fun post_process_services/1,
                 fun post_process_modules/1]).

-spec new_state() -> state().
new_state() ->
    #state{}.

-spec set_opts(opts(), state()) -> state().
set_opts(Opts, State) ->
    State#state{opts = Opts}.

-spec set_hosts([domain_name()], state()) -> state().
set_hosts(Hosts, State) ->
    State#state{hosts = Hosts}.

-spec set_host_types([mongooseim:host_type()], state()) -> state().
set_host_types(HostTypes, State) ->
    State#state{host_types = HostTypes}.

-spec get_opts(state()) -> opts().
get_opts(#state{opts = Opts}) ->
    Opts.

%% Config post-processing

%% @doc Repeat global options for each host type for easier lookup
-spec unfold_globals(state()) -> state().
unfold_globals(Config = #state{opts = Opts, hosts = Hosts, host_types = HostTypes}) ->
    {HTOpts, SimpleOpts} = lists:partition(fun({K, _}) -> is_tuple(K) end, Opts),
    GroupedOpts = maps:to_list(group_opts(HTOpts)),
    AllHostTypes = Hosts ++ HostTypes,
    NewHTOpts = lists:flatmap(fun({K, M}) -> merge_opts(K, M, AllHostTypes) end, GroupedOpts),
    Config#state{opts = SimpleOpts ++ NewHTOpts}.

%% @doc For each host type, merge the global value with the host-type value (if it exists)
-spec merge_opts(atom(), #{mongooseim:host_type_or_global() => mongoose_config:value()},
                 [mongooseim:host_type()]) ->
          [{mongoose_config:host_type_key(), mongoose_config:value()}].
merge_opts(Key, Opts = #{global := GlobalValue}, AllHostTypes) ->
    Global = case keep_global_value(Key) of
                 true -> [{{Key, global}, GlobalValue}];
                 false -> []
             end,
    Global ++ [{{Key, HT}, merge_values(Key, GlobalValue, maps:get(HT, Opts, GlobalValue))}
               || HT <- AllHostTypes];
merge_opts(Key, Opts, _AllHostTypes) ->
    [{{Key, HT}, Val} || {HT, Val} <- maps:to_list(Opts)].

%% @doc Group host-type options by keys for easier processing (key by key)
-spec group_opts([{mongoose_config:host_type_key(), mongoose_config:value()}]) ->
          #{atom() => #{mongooseim:host_type_or_global() => mongoose_config:value()}}.
group_opts(HTOpts) ->
    lists:foldl(fun({{Key, HT}, Val}, Acc) ->
                        maps:update_with(Key, fun(Opts) -> Opts#{HT => Val} end, #{HT => Val}, Acc)
                end, #{}, HTOpts).

%% @doc Merge global options with host-type ones
-spec merge_values(atom(), mongoose_config:value(), mongoose_config:value()) ->
          mongoose_config:value().
merge_values(acl, GlobalValue, HTValue) ->
    merge_with(fun(V1, V2) -> V1 ++ V2 end, GlobalValue, HTValue);
merge_values(access, GlobalValue, HTValue) ->
    merge_with(fun acl:merge_access_rules/2, GlobalValue, HTValue);
merge_values(_Key, _GlobalValue, HTValue) ->
    HTValue.

%% Use maps:merge_with/3 when dropping OTP 23
merge_with(F, GlobalMap, HTMap) ->
    maps:fold(fun(Key, HTVal, M) ->
                      maps:update_with(Key, fun(GVal) when GVal =:= HTVal -> GVal;
                                               (GVal) -> F(GVal, HTVal)
                                            end, HTVal, M)
              end, GlobalMap, HTMap).

%% @doc Global value is retained for access rules and acl as they can be matched on the global level
-spec keep_global_value(atom()) -> boolean().
keep_global_value(acl) -> true;
keep_global_value(access) -> true;
keep_global_value(_) -> false.

-spec post_process_services(state()) -> state().
post_process_services(State = #state{opts = Opts}) ->
    Opts1 = lists:map(fun post_process_services_opt/1, Opts),
    State#state{opts = Opts1}.

post_process_services_opt({services, Services}) ->
    ServicesWithDeps = mongoose_service_deps:resolve_deps(Services),
    {services, ServicesWithDeps};
post_process_services_opt(Other) ->
    Other.

-spec post_process_modules(state()) -> state().
post_process_modules(State = #state{opts = Opts}) ->
    Opts1 = lists:map(fun post_process_modules_opt/1, Opts),
    State#state{opts = Opts1}.

post_process_modules_opt({{modules, HostType}, Modules}) ->
    ModulesWithDeps = gen_mod_deps:resolve_deps(HostType, Modules),
    {{modules, HostType}, ModulesWithDeps};
post_process_modules_opt(Other) ->
    Other.

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
