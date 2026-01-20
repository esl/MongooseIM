-module(mongoose_graphql_server_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").

execute(_Ctx, server, <<"status">>, _) ->
    {ok, {Status, Message, Version, CommitHash}} = mongoose_server_api:status(),
    {ok, #{<<"statusCode">> => status_code(Status), <<"message">> => Message,
           <<"version">> => Version, <<"commitHash">> => CommitHash}};
execute(_Ctx, server, <<"hostTypes">>, _) ->
    HostTypes = host_types(),
    {ok, [{ok, host_type_info(HostType)} || HostType <- HostTypes]};
execute(_Ctx, server, <<"globalInfo">>, _) ->
    Services = get_loaded_services(),
    InternalDatabases = get_internal_databases(),
    {ok, #{<<"services">> => [{ok, #{<<"name">> => atom_to_binary(S, utf8)}} || S <- Services],
           <<"internalDatabases">> => [{ok, atom_to_binary(DB, utf8)} || DB <- InternalDatabases]}};
execute(_Ctx, server, <<"getLoglevel">>, _) ->
    mongoose_server_api:get_loglevel();
execute(_Ctx, server, <<"getCookie">>, _) ->
    mongoose_server_api:get_cookie().

status_code(true) -> <<"RUNNING">>;
status_code(false) -> <<"NOT_RUNNING">>.

host_types() ->
    lists:usort(?ALL_HOST_TYPES).

host_type_info(HostType) ->
    ModulesWithOpts = gen_mod:loaded_modules_with_opts(HostType),
    Modules = lists:keysort(1, maps:to_list(ModulesWithOpts)),
    AuthMethods = get_auth_methods(HostType),
    #{<<"name">> => HostType,
      <<"modules">> => [{ok, module_info(HostType, Module, Opts)} || {Module, Opts} <- Modules],
      <<"authMethods">> => [{ok, M} || M <- AuthMethods]}.

get_auth_methods(HostType) ->
    try mongoose_config:get_opt([{auth, HostType}, methods]) of
        Methods when is_list(Methods) ->
            [atom_to_binary(M, utf8) || M <- Methods]
    catch
        _:_ -> []
    end.

get_loaded_services() ->
    ServicesWithOpts = mongoose_service:loaded_services_with_opts(),
    lists:sort(maps:keys(ServicesWithOpts)).

get_internal_databases() ->
    InternalDatabasesWithOpts = mongoose_config:get_opt(internal_databases),
    case is_map(InternalDatabasesWithOpts) of
        true -> lists:sort(maps:keys(InternalDatabasesWithOpts));
        false -> []
    end.

module_info(HostType, Module, Opts) ->
    Options = module_options(HostType, Module, Opts),
    #{<<"name">> => atom_to_binary(Module, utf8),
      <<"options">> => [{ok, OptionMap} || OptionMap <- Options]}.

module_options(HostType, Module, Opts) ->
    RawOptions = module_options_to_report(HostType, Module, Opts),
    Formatted = [format_module_option(Entry) || Entry <- RawOptions],
    lists:sort(fun compare_module_options/2, Formatted).

%% Modules can optionally export reported_module_options/2 to control the reported
%% configuration keys.
module_options_to_report(HostType, Module, Opts) ->
    case erlang:function_exported(Module, reported_module_options, 2) of
        true ->
            try Module:reported_module_options(HostType, Opts) of
                Options when is_list(Options) -> Options;
                _ -> default_options_to_report(Opts)
            catch
                _:_ -> default_options_to_report(Opts)
            end;
        false ->
            default_options_to_report(Opts)
    end.

default_options_to_report(Opts) ->
    [{Key, Value} || {Key, Value} <- maps:to_list(Opts), is_backend_option(Key)].

is_backend_option(Key) when is_atom(Key) ->
    lists:suffix("backend", atom_to_list(Key));
is_backend_option(_) ->
    false.

format_module_option({Key, Value}) ->
    #{<<"key">> => format_option_key(Key),
      <<"value">> => format_option_value(Value)};
format_module_option(Other) ->
    format_module_option({Other, undefined}).

compare_module_options(#{<<"key">> := Key1}, #{<<"key">> := Key2}) ->
    Key1 =< Key2.

format_option_key(Key) ->
    format_term(Key).

format_option_value(undefined) -> null;
format_option_value(null) -> null;
format_option_value(Value) ->
    format_term(Value).

format_term(Term) when is_binary(Term) -> Term;
format_term(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
format_term(Term) when is_integer(Term) -> integer_to_binary(Term);
format_term(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> unicode:characters_to_binary(Term);
        false -> unicode:characters_to_binary(io_lib:format("~tp", [Term]))
    end.

