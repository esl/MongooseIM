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
         add_dep_modules/1]).

-ignore_xref([behaviour_info/1, get_opts/1,
              state_to_global_opt/3, state_to_host_types/1, state_to_hosts/1]).

-callback parse_file(FileName :: string()) -> state().

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type key() :: atom()
             | {key(), jid:server() | atom() | list()}
             | {atom(), atom(), atom()}
             | binary(). % TODO: binary is questionable here

-type value() :: atom()
               | binary()
               | integer()
               | string()
               | [value()]
               | tuple().

-export_type([state/0, key/0, value/0]).

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
        State = ParserModule:parse_file(FileName),
        check_dynamic_domains_support(State),
        State
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

dedup_state_opts_list([{Type, K, _V} = H|List], Removed, Keep, Set)
  when Type =:= config; Type =:= local_config ->
    Element = {Type, K},
    case sets:is_element(Element, Set) of
        true ->
            dedup_state_opts_list(List, [H|Removed], Keep, Set);
        false ->
            Set1 = sets:add_element(Element, Set),
            dedup_state_opts_list(List, Removed, [H|Keep], Set1)
    end;
dedup_state_opts_list([H|List], Removed, Keep, Set) ->
    dedup_state_opts_list(List, Removed, [H|Keep], Set);
dedup_state_opts_list([], Removed, Keep, _Set) ->
    {Keep, Removed}.

-spec add_dep_modules(state()) -> state().
add_dep_modules(State = #state{opts = Opts}) ->
    Opts2 = add_dep_modules_opts(Opts),
    State#state{opts = Opts2}.

add_dep_modules_opts(Opts) ->
    lists:map(fun add_dep_modules_opt/1, Opts).

add_dep_modules_opt({local_config, {modules, Host}, Modules}) ->
    Modules2 = gen_mod_deps:add_deps(Host, Modules),
    {local_config, {modules, Host}, Modules2};
add_dep_modules_opt(Other) ->
    Other.

%% local functions

-spec halt_with_msg(string(), [any()]) -> no_return().
-ifdef(TEST).
halt_with_msg(ExitMsg, Errors) ->
    config_error(ExitMsg, Errors).
-else.
halt_with_msg(ExitMsg, Errors) ->
    [?LOG_ERROR(Error) || Error <- Errors],
    mongoose_config_utils:exit_or_halt(ExitMsg).
-endif.

config_error(ErrorMsg, Errors) ->
    error({config_error, ErrorMsg, Errors}).

-spec check_dynamic_domains_support(mongoose_config_parser:state()) -> any().
check_dynamic_domains_support(State) ->
    %% we must check that all the the modules configured for
    %% the pure host types support dynamic domains feature
    Config = get_opts(State),
    HostTypes = state_to_host_types(State),
    FoldFN = fun(ConfigEl, ErrorsAcc) ->
                 ErrorsAcc ++
                     maybe_check_modules_for_host_type(ConfigEl, HostTypes) ++
                     maybe_check_auth_methods_for_host_types(ConfigEl, HostTypes)
             end,
    case lists:foldl(FoldFN, [], Config) of
        [] -> ok;
        Errors ->
            config_error("Invalid host type configuration", Errors)
    end.

maybe_check_modules_for_host_type(#local_config{key = {modules, HostOrHostType},
                                                value = ModulesWithOpts},
                                  HostTypes) ->
    case lists:member(HostOrHostType, HostTypes) of
        false -> [];
        true ->
            BadModules = check_modules_for_host_type(ModulesWithOpts),
            invalid_modules_for_host_type(HostOrHostType, BadModules)
    end;
maybe_check_modules_for_host_type(_, _) -> [].

check_modules_for_host_type(ModulesWithOpts) ->
    FilterMapFN = fun({Module, _}) ->
                      case gen_mod:does_module_support(Module, dynamic_domains) of
                          true -> false;
                          false -> {true, Module}
                      end
                  end,
    lists:filtermap(FilterMapFN, ModulesWithOpts).

invalid_modules_for_host_type(HostType, Modules) ->
    MapFN = fun(Module) ->
                #{class => error,
                  module => Module,
                  host_type => HostType,
                  reason => not_supported_module,
                  text => "this module doesn't support dynamic domains",
                  what => toml_processing_failed}
            end,
    lists:map(MapFN, Modules).

maybe_check_auth_methods_for_host_types(#local_config{key = {auth_method, HostOrHostType},
                                                      value = ListOfMethods},
                                        HostTypes) ->
    case lists:member(HostOrHostType, HostTypes) of
        false -> [];
        true ->
            BadModules = check_auth_methods_for_host_type(ListOfMethods),
            invalid_auth_methods_for_host_type(HostOrHostType, BadModules)
    end;
maybe_check_auth_methods_for_host_types(_, _) -> [].

check_auth_methods_for_host_type(ListOfMethods) ->
    FilterMapFN = fun(Method) ->
                      case ejabberd_auth:does_method_support(Method, dynamic_domains) of
                          true -> false;
                          false -> {true, Method}
                      end
                  end,
    lists:filtermap(FilterMapFN, ListOfMethods).

invalid_auth_methods_for_host_type(HostType, Methods) ->
    MapFN = fun(Method) ->
                #{class => error,
                  auth_method => Method,
                  host_type => HostType,
                  reason => not_supported_auth_method,
                  text => "this auth method doesn't support dynamic domains",
                  what => toml_processing_failed}
            end,
    lists:map(MapFN, Methods).
