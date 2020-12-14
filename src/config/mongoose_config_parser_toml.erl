%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-ifdef(TEST).
-export([parse/1,
         extract_errors/1]).
-endif.

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("ejabberd_config.hrl").

%% Used to create per-host config when the list of hosts is not known yet
-define(HOST_F(Expr), [fun(Host) -> Expr end]).

%% Input: TOML parsed by tomerl
-type toml_key() :: binary().
-type toml_value() :: tomerl:value().
-type toml_section() :: tomerl:section().

%% Output: list of config records, containing key-value pairs
-type option() :: term(). % a part of a config value OR a list of them, may contain config errors
-type top_level_option() :: #config{} | #local_config{} | acl:acl().
-type config_error() :: #{class := error, what := atom(), text := string(), any() => any()}.
-type override() :: {override, atom()}.
-type config() :: top_level_option() | config_error() | override().
-type config_list() :: [config() | fun((ejabberd:server()) -> [config()])]. % see HOST_F

%% Path from the currently processed config node to the root
%%   - toml_key(): key in a toml_section()
%%   - item: item in a list
%%   - tuple(): item in a list, tagged with data from the item, e.g. host name
-type path() :: [toml_key() | item | tuple()].

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    case tomerl:read_file(FileName) of
        {ok, Content} ->
            process(Content);
        {error, Error} ->
            Text = tomerl:format_error(Error),
            error(config_error([#{what => toml_parsing_failed, text => Text}]))
    end.

-spec process(toml_section()) -> mongoose_config_parser:state().
process(Content) ->
    Config = parse(Content),
    Hosts = get_hosts(Config),
    {FOpts, Config1} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    {Overrides, Opts} = lists:partition(fun({override, _}) -> true;
                                           (_) -> false
                                        end, Config1),
    HOpts = lists:flatmap(fun(F) -> lists:flatmap(F, Hosts) end, FOpts),
    AllOpts = Opts ++ HOpts,
    case extract_errors(AllOpts) of
        [] ->
            build_state(Hosts, AllOpts, Overrides);
        Errors ->
            error(config_error(Errors))
    end.

config_error(Errors) ->
    {config_error, "Could not read the TOML configuration file", Errors}.

%% Config processing functions are annotated with TOML paths
%% Path syntax: dotted, like TOML keys with the following additions:
%%   - '[]' denotes an element in a list
%%   - '( ... )' encloses an optional prefix
%%   - '*' is a wildcard for names - usually that name is passed as an argument
%% If the path is the same as for the previous function, it is not repeated.
%%
%% Example: (host_config[].)access.*
%% Meaning: either a key in the 'access' section, e.g.
%%            [access]
%%              local = ...
%%          or the same, but prefixed for a specific host, e.g.
%%            [[host_config]]
%%              host = "myhost"
%%              host_config.access
%%                local = ...

%% root path
-spec parse(toml_section()) -> config_list().
parse(Content) ->
    handle([], Content).

-spec parse_root(path(), toml_section()) -> config_list().
parse_root(Path, Content) ->
    ensure_keys([<<"general">>], Content),
    parse_section(Path, Content).

%% path: (host_config[].)modules.*
-spec process_module(path(), toml_section()) -> [option()].
process_module([Mod|_] = Path, Opts) ->
    %% Sort option keys to ensure options could be matched in tests
    post_process_module(b2a(Mod), parse_section(Path, Opts)).

post_process_module(mod_mam_meta, Opts) ->
    %% Disable the archiving by default
    [{mod_mam_meta, lists:sort(defined_or_false(muc, defined_or_false(pm, Opts)))}];
post_process_module(Mod, Opts) ->
    [{Mod, lists:sort(Opts)}].

%% path: (host_config[].)modules.*.*
-spec module_opt(path(), toml_value()) -> [option()].
module_opt([<<"service">>, <<"mod_extdisco">>|_] = Path, V) ->
    parse_list(Path, V);
module_opt([<<"host">>, <<"mod_http_upload">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"backend">>, <<"mod_http_upload">>|_], V) ->
    [{backend, b2a(V)}];
module_opt([<<"expiration_time">>, <<"mod_http_upload">>|_], V) ->
    [{expiration_time, V}];
module_opt([<<"token_bytes">>, <<"mod_http_upload">>|_], V) ->
    [{token_bytes, V}];
module_opt([<<"max_file_size">>, <<"mod_http_upload">>|_], V) ->
    [{max_file_size, V}];
module_opt([<<"s3">>, <<"mod_http_upload">>|_] = Path, V) ->
    S3Opts = parse_section(Path, V),
    [{s3, S3Opts}];
module_opt([<<"reset_markers">>, <<"mod_inbox">>|_] = Path, V) ->
    Markers = parse_list(Path, V),
    [{reset_markers, Markers}];
module_opt([<<"groupchat">>, <<"mod_inbox">>|_] = Path, V) ->
    GChats = parse_list(Path, V),
    [{groupchat, GChats}];
module_opt([<<"aff_changes">>, <<"mod_inbox">>|_], V) ->
    [{aff_changes, V}];
module_opt([<<"remove_on_kicked">>, <<"mod_inbox">>|_], V) ->
    [{remove_on_kicked, V}];
module_opt([<<"global_host">>, <<"mod_global_distrib">>|_], V) ->
    [{global_host, b2l(V)}];
module_opt([<<"local_host">>, <<"mod_global_distrib">>|_], V) ->
    [{local_host, b2l(V)}];
module_opt([<<"message_ttl">>, <<"mod_global_distrib">>|_], V) ->
    [{message_ttl, V}];
module_opt([<<"connections">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Conns = parse_section(Path, V),
    [{connections, Conns}];
module_opt([<<"cache">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Cache = parse_section(Path, V),
    [{cache, Cache}];
module_opt([<<"bounce">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Bounce = parse_section(Path, V, fun format_global_distrib_bounce/1),
    [{bounce, Bounce}];
module_opt([<<"redis">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Redis = parse_section(Path, V),
    [{redis, Redis}];
module_opt([<<"hosts_refresh_interval">>, <<"mod_global_distrib">>|_], V) ->
    [{hosts_refresh_interval, V}];
module_opt([<<"proxy_host">>, <<"mod_jingle_sip">>|_], V) ->
    [{proxy_host, b2l(V)}];
module_opt([<<"proxy_port">>, <<"mod_jingle_sip">>|_], V) ->
    [{proxy_port, V}];
module_opt([<<"listen_port">>, <<"mod_jingle_sip">>|_], V) ->
    [{listen_port, V}];
module_opt([<<"local_host">>, <<"mod_jingle_sip">>|_], V) ->
    [{local_host, b2l(V)}];
module_opt([<<"sdp_origin">>, <<"mod_jingle_sip">>|_], V) ->
    [{sdp_origin, b2l(V)}];
module_opt([<<"ram_key_size">>, <<"mod_keystore">>|_], V) ->
    [{ram_key_size, V}];
module_opt([<<"keys">>, <<"mod_keystore">>|_] = Path, V) ->
    Keys = parse_list(Path, V),
    [{keys, Keys}];
module_opt([<<"pm">>, <<"mod_mam_meta">>|_] = Path, V) ->
    PM = parse_section(Path, V),
    [{pm, PM}];
module_opt([<<"muc">>, <<"mod_mam_meta">>|_] = Path, V) ->
    Muc = parse_section(Path, V),
    [{muc, Muc}];
module_opt([_, <<"mod_mam_meta">>|_] = Path, V) ->
    mod_mam_opts(Path, V);
module_opt([<<"host">>, <<"mod_pubsub">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"access_createnode">>, <<"mod_pubsub">>|_], V) ->
    [{access_createnode, b2a(V)}];
module_opt([<<"max_items_node">>, <<"mod_pubsub">>|_], V) ->
    [{max_items_node, V}];
module_opt([<<"max_subscriptions_node">>, <<"mod_pubsub">>|_], <<"infinity">>) ->
    [];
module_opt([<<"max_subscriptions_node">>, <<"mod_pubsub">>|_], V) ->
    [{max_subscriptions_node, V}];
module_opt([<<"nodetree">>, <<"mod_pubsub">>|_], V) ->
    [{nodetree, V}];
module_opt([<<"ignore_pep_from_offline">>, <<"mod_pubsub">>|_], V) ->
    [{ignore_pep_from_offline, V}];
module_opt([<<"last_item_cache">>, <<"mod_pubsub">>|_], false) ->
    [{last_item_cache, false}];
module_opt([<<"last_item_cache">>, <<"mod_pubsub">>|_], V) ->
    [{last_item_cache, b2a(V)}];
module_opt([<<"plugins">>, <<"mod_pubsub">>|_] = Path, V) ->
    Plugs = parse_list(Path, V),
    [{plugins, Plugs}];
module_opt([<<"pep_mapping">>, <<"mod_pubsub">>|_] = Path, V) ->
    Mappings = parse_list(Path, V),
    [{pep_mapping, Mappings}];
module_opt([<<"default_node_config">>, <<"mod_pubsub">>|_] = Path, V) ->
    Config = parse_section(Path, V),
    [{default_node_config, Config}];
module_opt([<<"item_publisher">>, <<"mod_pubsub">>|_], V) ->
    [{item_publisher, V}];
module_opt([<<"sync_broadcast">>, <<"mod_pubsub">>|_], V) ->
    [{sync_broadcast, V}];
module_opt([<<"pool_name">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{pool_name, b2a(V)}];
module_opt([<<"api_version">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{api_version, b2l(V)}];
module_opt([<<"max_http_connections">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{max_http_connections, V}];
module_opt([<<"access">>, <<"mod_register">>|_], V) ->
    [{access, b2a(V)}];
module_opt([<<"registration_watchers">>, <<"mod_register">>|_] = Path, V) ->
    [{registration_watchers, parse_list(Path, V)}];
module_opt([<<"password_strength">>, <<"mod_register">>|_], V) ->
    [{password_strength, V}];
module_opt([<<"ip_access">>, <<"mod_register">>|_] = Path, V) ->
    Rules = parse_list(Path, V),
    [{ip_access, Rules}];
module_opt([<<"welcome_message">>, <<"mod_register">>|_] = Path, V) ->
    parse_section(Path, V, fun process_welcome_message/1);
module_opt([<<"routes">>, <<"mod_revproxy">>|_] = Path, V) ->
    Routes = parse_list(Path, V),
    [{routes, Routes}];
module_opt([<<"versioning">>, <<"mod_roster">>|_], V) ->
    [{versioning, V}];
module_opt([<<"store_current_id">>, <<"mod_roster">>|_], V) ->
    [{store_current_id, V}];
module_opt([<<"ldap_useruid">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_useruid, b2l(V)}];
module_opt([<<"ldap_groupattr">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_groupattr, b2l(V)}];
module_opt([<<"ldap_groupdesc">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_groupdesc, b2l(V)}];
module_opt([<<"ldap_userdesc">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_userdesc, b2l(V)}];
module_opt([<<"ldap_userid">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_userid, b2l(V)}];
module_opt([<<"ldap_memberattr">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr, b2l(V)}];
module_opt([<<"ldap_memberattr_format">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr_format, b2l(V)}];
module_opt([<<"ldap_memberattr_format_re">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr_format_re, b2l(V)}];
module_opt([<<"ldap_auth_check">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_auth_check, V}];
module_opt([<<"ldap_user_cache_validity">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_user_cache_validity, V}];
module_opt([<<"ldap_group_cache_validity">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_group_cache_validity, V}];
module_opt([<<"ldap_user_cache_size">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_user_cache_size, V}];
module_opt([<<"ldap_group_cache_size">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_group_cache_size, V}];
module_opt([<<"ldap_rfilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_rfilter, b2l(V)}];
module_opt([<<"ldap_gfilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_gfilter, b2l(V)}];
module_opt([<<"ldap_ufilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_ufilter, b2l(V)}];
module_opt([<<"buffer_max">>, <<"mod_stream_management">>|_], <<"no_buffer">>) ->
    [{buffer_max, no_buffer}];
module_opt([<<"buffer_max">>, <<"mod_stream_management">>|_], V) ->
    [{buffer_max, int_or_infinity(V)}];
module_opt([<<"ack_freq">>, <<"mod_stream_management">>|_], <<"never">>) ->
    [{ack_freq, never}];
module_opt([<<"ack_freq">>, <<"mod_stream_management">>|_], V) ->
    [{ack_freq, V}];
module_opt([<<"resume_timeout">>, <<"mod_stream_management">>|_], V) ->
    [{resume_timeout, V}];
module_opt([<<"stale_h">>, <<"mod_stream_management">>|_] = Path, V) ->
    Stale = parse_section(Path, V),
    [{stale_h, Stale}];
module_opt([<<"host">>, <<"mod_vcard">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"search">>, <<"mod_vcard">>|_], V) ->
    [{search, V}];
module_opt([<<"matches">>, <<"mod_vcard">>|_], V) ->
    [{matches, int_or_infinity(V)}];
module_opt([<<"ldap_vcard_map">>, <<"mod_vcard">>|_] = Path, V) ->
    Maps = parse_list(Path, V),
    [{ldap_vcard_map, Maps}];
module_opt([<<"ldap_uids">>, <<"mod_vcard">>|_] = Path, V) ->
    List = parse_list(Path, V),
    [{ldap_uids, List}];
module_opt([<<"ldap_search_fields">>, <<"mod_vcard">>|_] = Path, V) ->
    Fields = parse_list(Path, V),
    [{ldap_search_fields, Fields}];
module_opt([<<"ldap_search_reported">>, <<"mod_vcard">>|_] = Path, V) ->
    Reported = parse_list(Path, V),
    [{ldap_search_reported, Reported}];
module_opt([<<"ldap_search_operator">>, <<"mod_vcard">>|_], V) ->
    [{ldap_search_operator, b2a(V)}];
module_opt([<<"ldap_binary_search_fields">>, <<"mod_vcard">>|_] = Path, V) ->
    List = parse_list(Path, V),
    [{ldap_binary_search_fields, List}];
module_opt([<<"os_info">>, <<"mod_version">>|_], V) ->
    [{os_info, V}];
% General options
module_opt([<<"iqdisc">>|_], V) ->
    {Type, Opts} = maps:take(<<"type">>, V),
    [{iqdisc, iqdisc_value(b2a(Type), Opts)}];
module_opt([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
%% LDAP-specific options
module_opt([<<"ldap_pool_tag">>|_], V) ->
    [{ldap_pool_tag, b2a(V)}];
module_opt([<<"ldap_base">>|_], V) ->
    [{ldap_base, b2l(V)}];
module_opt([<<"ldap_filter">>|_], V) ->
    [{ldap_filter, b2l(V)}];
module_opt([<<"ldap_deref">>|_], V) ->
    [{ldap_deref, b2a(V)}];
%% Backend-specific options
module_opt([<<"riak">>|_] = Path, V) ->
    parse_section(Path, V).

process_welcome_message(Props) ->
    Subject = proplists:get_value(subject, Props, ""),
    Body = proplists:get_value(body, Props, ""),
    [{welcome_message, {Subject, Body}}].

%% path: (host_config[].)modules.*.riak.*
-spec riak_opts(path(), toml_section()) -> [option()].
riak_opts([<<"defaults_bucket_type">>|_], V) ->
    [{defaults_bucket_type, V}];
riak_opts([<<"names_bucket_type">>|_], V) ->
    [{names_bucket_type, V}];
riak_opts([<<"version_bucket_type">>|_], V) ->
    [{version_bucket_type, V}];
riak_opts([<<"bucket_type">>|_], V) ->
    [{bucket_type, V}];
riak_opts([<<"search_index">>|_], V) ->
    [{search_index, V}].

-spec mod_register_ip_access_rule(path(), toml_section()) -> [option()].
mod_register_ip_access_rule(_, #{<<"address">> := Addr, <<"policy">> := Policy}) ->
    [{b2a(Policy), b2l(Addr)}].

-spec mod_extdisco_service(path(), toml_value()) -> [option()].
mod_extdisco_service([_, <<"service">>|_] = Path, V) ->
    [parse_section(Path, V)];
mod_extdisco_service([<<"type">>|_], V) ->
    [{type, b2a(V)}];
mod_extdisco_service([<<"host">>|_], V) ->
    [{host, b2l(V)}];
mod_extdisco_service([<<"port">>|_], V) ->
    [{port, V}];
mod_extdisco_service([<<"transport">>|_], V) ->
    [{transport, b2l(V)}];
mod_extdisco_service([<<"username">>|_], V) ->
    [{username, b2l(V)}];
mod_extdisco_service([<<"password">>|_], V) ->
    [{password, b2l(V)}].

-spec mod_http_upload_s3(path(), toml_value()) -> [option()].
mod_http_upload_s3([<<"bucket_url">>|_], V) ->
    [{bucket_url, b2l(V)}];
mod_http_upload_s3([<<"add_acl">>|_], V) ->
    [{add_acl, V}];
mod_http_upload_s3([<<"region">>|_], V) ->
    [{region, b2l(V)}];
mod_http_upload_s3([<<"access_key_id">>|_], V) ->
    [{access_key_id, b2l(V)}];
mod_http_upload_s3([<<"secret_access_key">>|_], V) ->
    [{secret_access_key, b2l(V)}].

-spec mod_global_distrib_connections(path(), toml_value()) -> [option()].
mod_global_distrib_connections([<<"endpoints">>|_] = Path, V) ->
    Endpoints = parse_list(Path, V),
    [{endpoints, Endpoints}];
mod_global_distrib_connections([<<"advertised_endpoints">>|_], false) ->
    [{advertised_endpoints, false}];
mod_global_distrib_connections([<<"advertised_endpoints">>|_] = Path, V) ->
    Endpoints = parse_list(Path, V),
    [{advertised_endpoints, Endpoints}];
mod_global_distrib_connections([<<"connections_per_endpoint">>|_], V) ->
    [{connections_per_endpoint, V}];
mod_global_distrib_connections([<<"endpoint_refresh_interval">>|_], V) ->
    [{endpoint_refresh_interval, V}];
mod_global_distrib_connections([<<"endpoint_refresh_interval_when_empty">>|_], V) ->
    [{endpoint_refresh_interval_when_empty, V}];
mod_global_distrib_connections([<<"disabled_gc_interval">>|_], V) ->
    [{disabled_gc_interval, V}];
mod_global_distrib_connections([<<"tls">>|_] = Path, V) ->
    TLSOpts = parse_section(Path, V, fun format_global_distrib_tls/1),
    [{tls_opts, TLSOpts}].

-spec format_global_distrib_tls([option()]) -> option().
format_global_distrib_tls(Opts) ->
    case proplists:lookup(enabled, Opts) of
        {enabled, true} -> proplists:delete(enabled, Opts);
        _ -> false
    end.

-spec mod_global_distrib_cache(path(), toml_value()) -> [option()].
mod_global_distrib_cache([<<"cache_missed">>|_], V) ->
    [{cache_missed, V}];
mod_global_distrib_cache([<<"domain_lifetime_seconds">>|_], V) ->
    [{domain_lifetime_seconds, V}];
mod_global_distrib_cache([<<"jid_lifetime_seconds">>|_], V) ->
    [{jid_lifetime_seconds, V}];
mod_global_distrib_cache([<<"max_jids">>|_], V) ->
    [{max_jids, V}].

-spec mod_global_distrib_redis(path(), toml_value()) -> [option()].
mod_global_distrib_redis([<<"pool">>|_], V) ->
    [{pool, b2a(V)}];
mod_global_distrib_redis([<<"expire_after">>|_], V) ->
    [{expire_after, V}];
mod_global_distrib_redis([<<"refresh_after">>|_], V) ->
    [{refresh_after, V}].

-spec mod_global_distrib_bounce(path(), toml_value()) -> [option()].
mod_global_distrib_bounce([<<"resend_after_ms">>|_], V) ->
    [{resend_after_ms, V}];
mod_global_distrib_bounce([<<"max_retries">>|_], V) ->
    [{max_retries, V}];
mod_global_distrib_bounce([<<"enabled">>|_], V) ->
    [{enabled, V}].

-spec format_global_distrib_bounce([option()]) -> option().
format_global_distrib_bounce(Opts) ->
    case proplists:lookup(enabled, Opts) of
        {enabled, false} -> false;
        _ -> proplists:delete(enabled, Opts)
    end.

-spec mod_global_distrib_connections_endpoints(path(), toml_section()) -> [option()].
mod_global_distrib_connections_endpoints(_, #{<<"host">> := Host, <<"port">> := Port}) ->
    [{b2l(Host), Port}].

-spec mod_global_distrib_connections_advertised_endpoints(path(), toml_section()) -> [option()].
mod_global_distrib_connections_advertised_endpoints(_, #{<<"host">> := Host, <<"port">> := Port}) ->
    [{b2l(Host), Port}].

-spec mod_keystore_keys(path(), toml_section()) -> [option()].
mod_keystore_keys(_, #{<<"name">> := Name, <<"type">> := <<"ram">>}) ->
    [{b2a(Name), ram}];
mod_keystore_keys(_, #{<<"name">> := Name, <<"type">> := <<"file">>, <<"path">> := Path}) ->
    [{b2a(Name), {file, b2l(Path)}}].

-spec mod_mam_opts(path(), toml_value()) -> [option()].
mod_mam_opts([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
mod_mam_opts([<<"no_stanzaid_element">>|_], V) ->
    [{no_stanzaid_element, V}];
mod_mam_opts([<<"is_archivable_message">>|_], V) ->
    [{is_archivable_message, b2a(V)}];
mod_mam_opts([<<"message_retraction">>|_], V) ->
    [{message_retraction, V}];
mod_mam_opts([<<"user_prefs_store">>|_], false) ->
    [{user_prefs_store, false}];
mod_mam_opts([<<"user_prefs_store">>|_], V) ->
    [{user_prefs_store, b2a(V)}];
mod_mam_opts([<<"full_text_search">>|_], V) ->
    [{full_text_search, V}];
mod_mam_opts([<<"cache_users">>|_], V) ->
    [{cache_users, V}];
mod_mam_opts([<<"rdbms_message_format">>|_], V) ->
    [{rdbms_message_format, b2a(V)}];
mod_mam_opts([<<"async_writer">>|_], V) ->
    [{async_writer, V}];
mod_mam_opts([<<"flush_interval">>|_], V) ->
    [{flush_interval, V}];
mod_mam_opts([<<"max_batch_size">>|_], V) ->
    [{max_batch_size, V}];
mod_mam_opts([<<"default_result_limit">>|_], V) ->
    [{default_result_limit, V}];
mod_mam_opts([<<"max_result_limit">>|_], V) ->
    [{max_result_limit, V}];
mod_mam_opts([<<"archive_chat_markers">>|_], V) ->
    [{archive_chat_markers, V}];
mod_mam_opts([<<"archive_groupchats">>|_], V) ->
    [{archive_groupchats, V}];
mod_mam_opts([<<"async_writer_rdbms_pool">>|_], V) ->
    [{async_writer_rdbms_pool, b2a(V)}];
mod_mam_opts([<<"db_jid_format">>|_], V) ->
    [{db_jid_format, b2a(V)}];
mod_mam_opts([<<"db_message_format">>|_], V) ->
    [{db_message_format, b2a(V)}];
mod_mam_opts([<<"simple">>|_], V) ->
    [{simple, V}];
mod_mam_opts([<<"host">>|_], V) ->
    [{host, b2l(V)}];
mod_mam_opts([<<"extra_lookup_params">>|_], V) ->
    [{extra_lookup_params, b2a(V)}];
mod_mam_opts([<<"riak">>|_] = Path, V) ->
    parse_section(Path, V).

-spec mod_pubsub_pep_mapping(path(), toml_section()) -> [option()].
mod_pubsub_pep_mapping(_, #{<<"namespace">> := Name, <<"node">> := Node}) ->
    [{b2l(Name), b2l(Node)}].

-spec mod_pubsub_default_node_config(path(), toml_section()) -> [option()].
mod_pubsub_default_node_config([<<"access_model">>|_], Value) ->
    [{access_model, b2a(Value)}];
mod_pubsub_default_node_config([<<"deliver_notifications">>|_], Value) ->
    [{deliver_notifications, Value}];
mod_pubsub_default_node_config([<<"deliver_payloads">>|_], Value) ->
    [{deliver_payloads, Value}];
mod_pubsub_default_node_config([<<"max_items">>|_], Value) ->
    [{max_items, Value}];
mod_pubsub_default_node_config([<<"max_payload_size">>|_], Value) ->
    [{max_payload_size, Value}];
mod_pubsub_default_node_config([<<"node_type">>|_], Value) ->
    [{node_type, b2a(Value)}];
mod_pubsub_default_node_config([<<"notification_type">>|_], Value) ->
    [{notification_type, b2a(Value)}];
mod_pubsub_default_node_config([<<"notify_config">>|_], Value) ->
    [{notify_config, Value}];
mod_pubsub_default_node_config([<<"notify_delete">>|_], Value) ->
    [{notify_delete, Value}];
mod_pubsub_default_node_config([<<"notify_retract">>|_], Value) ->
    [{notify_retract, Value}];
mod_pubsub_default_node_config([<<"persist_items">>|_], Value) ->
    [{persist_items, Value}];
mod_pubsub_default_node_config([<<"presence_based_delivery">>|_], Value) ->
    [{presence_based_delivery, Value}];
mod_pubsub_default_node_config([<<"publish_model">>|_], Value) ->
    [{publish_model, b2a(Value)}];
mod_pubsub_default_node_config([<<"purge_offline">>|_], Value) ->
    [{purge_offline, Value}];
mod_pubsub_default_node_config([<<"roster_groups_allowed">>|_] = Path, Value) ->
    Groups = parse_list(Path, Value),
    [{roster_groups_allowed, Groups}];
mod_pubsub_default_node_config([<<"send_last_published_item">>|_], Value) ->
    [{send_last_published_item, b2a(Value)}];
mod_pubsub_default_node_config([<<"subscribe">>|_], Value) ->
    [{subscribe, Value}].

mod_pubsub_roster_groups_allowed(_, Value) ->
    [Value].

-spec mod_revproxy_routes(path(), toml_section()) -> [option()].
mod_revproxy_routes(_, #{<<"host">> := Host, <<"path">> := Path, <<"method">> := Method,
    <<"upstream">> := Upstream}) ->
        [{b2l(Host), b2l(Path), b2l(Method), b2l(Upstream)}];
mod_revproxy_routes(_, #{<<"host">> := Host, <<"path">> := Path, <<"upstream">> := Upstream}) ->
        [{b2l(Host), b2l(Path), b2l(Upstream)}].

-spec mod_stream_management_stale_h(path(), toml_value()) -> [option()].
mod_stream_management_stale_h([<<"enabled">>|_], V) ->
    [{enabled, V}];
mod_stream_management_stale_h([<<"repeat_after">>|_], V) ->
    [{stale_h_repeat_after, V}];
mod_stream_management_stale_h([<<"geriatric">>|_], V) ->
    [{stale_h_geriatric, V}].

-spec mod_vcard_ldap_uids(path(), toml_section()) -> [option()].
mod_vcard_ldap_uids(_, #{<<"attr">> := Attr, <<"format">> := Format}) ->
    [{b2l(Attr), b2l(Format)}];
mod_vcard_ldap_uids(_, #{<<"attr">> := Attr}) ->
    [b2l(Attr)].


-spec mod_vcard_ldap_vcard_map(path(), toml_section()) -> [option()].
mod_vcard_ldap_vcard_map(_, #{<<"vcard_field">> := VF, <<"ldap_pattern">> := LP,
    <<"ldap_field">> := LF}) ->
    [{VF, LP, [LF]}].

-spec mod_vcard_ldap_search_fields(path(), toml_section()) -> [option()].
mod_vcard_ldap_search_fields(_, #{<<"search_field">> := SF, <<"ldap_field">> := LF}) ->
    [{SF, LF}].

-spec mod_vcard_ldap_search_reported(path(), toml_section()) -> [option()].
mod_vcard_ldap_search_reported(_, #{<<"search_field">> := SF, <<"vcard_field">> := VF}) ->
    [{SF, VF}].

-spec mod_vcard_ldap_binary_search_fields(path(), toml_section()) -> [option()].
mod_vcard_ldap_binary_search_fields(_, V) ->
    [V].

-spec iqdisc_value(atom(), toml_section()) -> option().
iqdisc_value(queues, #{<<"workers">> := Workers} = V) ->
    limit_keys([<<"workers">>], V),
    {queues, Workers};
iqdisc_value(Type, V) ->
    limit_keys([], V),
    Type.

welcome_message([<<"subject">>|_], Value) ->
    [{subject, b2l(Value)}];
welcome_message([<<"body">>|_], Value) ->
    [{body, b2l(Value)}].

%% path: host_config[]
-spec process_host_item(path(), toml_section()) -> config_list().
process_host_item(Path, M) ->
    {_Host, Sections} = maps:take(<<"host">>, M),
    parse_section(Path, Sections).

%% path: (host_config[].)modules.mod_global_distrib.connections.tls.*
-spec fast_tls_option(path(), toml_value()) -> [option()].
fast_tls_option([<<"certfile">>|_], V) -> [{certfile, b2l(V)}];
fast_tls_option([<<"cacertfile">>|_], V) -> [{cafile, b2l(V)}];
fast_tls_option([<<"dhfile">>|_], V) -> [{dhfile, b2l(V)}];
fast_tls_option([<<"ciphers">>|_], V) -> [{ciphers, b2l(V)}].

mod_global_distrib_tls_option([<<"enabled">>|_], V) ->
    [{enabled, V}];
mod_global_distrib_tls_option(P, V) ->
    fast_tls_option(P, V).

set_overrides(Overrides, State) ->
    lists:foldl(fun({override, Scope}, CurrentState) ->
                        mongoose_config_parser:override(Scope, CurrentState)
                end, State, Overrides).

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

b2l(B) -> binary_to_list(B).

int_or_infinity(I) when is_integer(I) -> I;
int_or_infinity(<<"infinity">>) -> infinity.

-spec limit_keys([toml_key()], toml_section()) -> any().
limit_keys(Keys, Section) ->
    case maps:keys(maps:without(Keys, Section)) of
        [] -> ok;
        ExtraKeys -> error(#{what => unexpected_keys, unexpected_keys => ExtraKeys})
    end.

-spec ensure_keys([toml_key()], toml_section()) -> any().
ensure_keys(Keys, Section) ->
    case lists:filter(fun(Key) -> not maps:is_key(Key, Section) end, Keys) of
        [] -> ok;
        MissingKeys -> error(#{what => missing_mandatory_keys, missing_keys => MissingKeys})
    end.

%% Parse with post-processing, this needs to be eliminated by fixing the internal config structure
-spec parse_section(path(), toml_section(), fun(([option()]) -> option())) -> option().
parse_section(Path, V, PostProcessF) ->
    L = parse_section(Path, V),
    case extract_errors(L) of
        [] -> PostProcessF(L);
        Errors -> Errors
    end.

-spec parse_section(path(), toml_section()) -> [option()].
parse_section(Path, M) ->
    lists:flatmap(fun({K, V}) ->
                          handle([K|Path], V)
                  end, lists:sort(maps:to_list(M))).

-spec parse_list(path(), [toml_value()]) -> [option()].
parse_list(Path, L) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          handle([Key|Path], Elem)
                  end, L).

-spec handle(path(), toml_value()) -> option().
handle(Path, Value) ->
    lists:foldl(fun(_, [#{what := _, class := error}] = Error) ->
                        Error;
                   (StepName, AccIn) ->
                        try_call(handle_step(StepName, AccIn), StepName, Path, Value)
                end, Path, [handle, parse, validate, process, format]).

handle_step(handle, _) ->
    fun(Path, _Value) -> handler(Path) end;
handle_step(parse, Spec) when is_tuple(Spec) ->
    fun(Path, Value) ->
            ParsedValue = case Spec of
                              #section{} = Spec when is_map(Value) ->
                                  check_required_keys(Spec, Value),
                                  validate_keys(Spec, Value),
                                  parse_section(Path, Value);
                              #list{} when is_list(Value) ->
                                  parse_list(Path, Value);
                              #option{type = Type} when not is_list(Value), not is_map(Value) ->
                                  convert(Value, Type)
                          end,
            case extract_errors(ParsedValue) of
                [] -> {ParsedValue, Spec};
                Errors -> Errors
            end
    end;
handle_step(parse, Handler) ->
    Handler;
handle_step(validate, {ParsedValue, Spec}) ->
    fun(_Path, _Value) ->
            validate(ParsedValue, Spec),
            {ParsedValue, Spec}
    end;
handle_step(validate, ParsedValue) ->
    fun(Path, _Value) ->
            mongoose_config_validator_toml:validate(Path, ParsedValue),
            ParsedValue
    end;
handle_step(process, {ParsedValue, Spec}) ->
    fun(Path, _Value) ->
            ProcessedValue = process(Path, ParsedValue, process_spec(Spec)),
            {ProcessedValue, Spec}
    end;
handle_step(process, V) ->
    fun(_, _) -> V end;
handle_step(format, {ParsedValue, Spec}) ->
    fun(Path, _Value) ->
            format(Path, ParsedValue, format_spec(Spec))
    end;
handle_step(format, V) ->
    fun(_, _) -> V end.

check_required_keys(#section{required = all, items = Items}, Section) ->
    ensure_keys(maps:keys(Items), Section);
check_required_keys(#section{required = Required}, Section) ->
    ensure_keys(Required, Section).

validate_keys(#section{validate_keys = undefined}, _Section) -> ok;
validate_keys(#section{validate_keys = Validator}, Section) ->
    lists:foreach(fun(Key) ->
                          mongoose_config_validator_toml:validate(b2a(Key), atom, Validator)
                  end, maps:keys(Section)).

validate(Value, #section{validate = Validator}) ->
    mongoose_config_validator_toml:validate_section(Value, Validator);
validate(Value, #list{validate = Validator}) ->
    mongoose_config_validator_toml:validate_list(Value, Validator);
validate(Value, #option{type = Type, validate = Validator}) ->
    mongoose_config_validator_toml:validate(Value, Type, Validator).

process_spec(#section{process = Process}) -> Process;
process_spec(#list{process = Process}) -> Process;
process_spec(#option{process = Process}) -> Process.

process(_Path, V, undefined) -> V;
process(_Path, V, F) when is_function(F, 1) -> F(V);
process(Path, V, F) when is_function(F, 2) -> F(Path, V).

convert(V, boolean) when is_boolean(V) -> V;
convert(V, binary) when is_binary(V) -> V;
convert(V, string) -> binary_to_list(V);
convert(V, atom) -> b2a(V);
convert(<<"infinity">>, int_or_infinity) -> infinity; %% TODO maybe use TOML '+inf'
convert(V, int_or_infinity) when is_integer(V) -> V;
convert(V, int_or_atom) when is_integer(V) -> V;
convert(V, int_or_atom) -> b2a(V);
convert(V, integer) when is_integer(V) -> V;
convert(V, float) when is_float(V) -> V.

format_spec(#section{format = Format}) -> Format;
format_spec(#list{format = Format}) -> Format;
format_spec(#option{format = Format}) -> Format.

format(Path, KVs, {foreach, Format}) when is_atom(Format) ->
    Keys = lists:map(fun({K, _}) -> K end, KVs),
    mongoose_config_validator_toml:validate_list(Keys, unique),
    lists:flatmap(fun({K, V}) -> format(Path, V, {Format, K}) end, KVs);
format([Key|_] = Path, V, host_local_config) ->
    format(Path, V, {host_local_config, b2a(Key)});
format([Key|_] = Path, V, local_config) ->
    format(Path, V, {local_config, b2a(Key)});
format([Key|_] = Path, V, config) ->
    format(Path, V, {config, b2a(Key)});
format(Path, V, {host_local_config, Key}) ->
    case get_host(Path) of
        global -> ?HOST_F([#local_config{key = {Key, Host}, value = V}]);
        Host -> [#local_config{key = {Key, Host}, value = V}]
    end;
format(Path, V, {local_config, Key}) ->
    global = get_host(Path),
    [#local_config{key = Key, value = V}];
format([Key|_] = Path, V, {host_or_global_config, Tag}) ->
    [#config{key = {Tag, b2a(Key), get_host(Path)}, value = V}];
format([item, Key|_] = Path, V, host_or_global_acl) ->
    [acl:to_record(get_host(Path), b2a(Key), V)];
format(Path, V, {config, Key}) ->
    global = get_host(Path),
    [#config{key = Key, value = V}];
format(Path, V, override) ->
    global = get_host(Path),
    [{override, V}];
format([item|_] = Path, V, default) ->
    format(Path, V, item);
format([Key|_] = Path, V, default) ->
    format(Path, V, {kv, b2a(Key)});
format(_Path, V, {kv, Key}) ->
    [{Key, V}];
format(_Path, V, item) ->
    [V];
format([Key|_], V, prepend_key) ->
    L = [b2a(Key) | tuple_to_list(V)],
    [list_to_tuple(L)];
format(_Path, V, none) ->
    V.

get_host(Path) ->
    case lists:reverse(Path) of
        [<<"host_config">>, {host, Host} | _] -> Host;
        _ -> global
    end.

-spec try_call(fun((path(), any()) -> option()), atom(), path(), toml_value()) -> option().
try_call(F, StepName, Path, Value) ->
    try
        F(Path, Value)
    catch error:Reason:Stacktrace ->
            BasicFields = #{what => toml_processing_failed,
                            class => error,
                            stacktrace => Stacktrace,
                            text => error_text(StepName),
                            toml_path => path_to_string(Path),
                            toml_value => Value},
            ErrorFields = error_fields(Reason),
            [maps:merge(BasicFields, ErrorFields)]
    end.

-spec error_text(atom()) -> string().
error_text(handle) -> "Unexpected option in the TOML configuration file";
error_text(parse) -> "Malformed option in the TOML configuration file";
error_text(validate) -> "Incorrect option value in the TOML configuration file";
error_text(process) -> "Unable to process a value the TOML configuration file";
error_text(format) -> "Unable to format an option in the TOML configuration file".

-spec error_fields(any()) -> map().
error_fields(#{what := Reason} = M) -> maps:remove(what, M#{reason => Reason});
error_fields(Reason) -> #{reason => Reason}.

-spec path_to_string(path()) -> string().
path_to_string(Path) ->
    Items = lists:flatmap(fun node_to_string/1, lists:reverse(Path)),
    string:join(Items, ".").

node_to_string(item) -> [];
node_to_string({host, _}) -> [];
node_to_string({tls, TLSAtom}) -> [atom_to_list(TLSAtom)];
node_to_string(Node) -> [binary_to_list(Node)].

-define(HAS_NO_SPEC(Mod),
        Mod =/= <<"mod_adhoc">>,
        Mod =/= <<"mod_auth_token">>,
        Mod =/= <<"mod_bosh">>,
        Mod =/= <<"mod_caps">>,
        Mod =/= <<"mod_carboncopy">>,
        Mod =/= <<"mod_csi">>,
        Mod =/= <<"mod_disco">>,
        Mod =/= <<"mod_event_pusher">>,
        Mod =/= <<"mod_muc">>,
        Mod =/= <<"mod_muc_light">>,
        Mod =/= <<"mod_muc_log">>,
        Mod =/= <<"mod_offline">>,
        Mod =/= <<"mod_ping">>). % TODO temporary, remove with 'handler/1'

-spec handler(path()) ->
          fun((path(), toml_value()) -> option()) | mongoose_config_spec:config_node().
handler([]) -> fun parse_root/2;
handler([<<"host_config">>]) -> fun parse_list/2;

%% modules
handler([Mod, <<"modules">>]) when ?HAS_NO_SPEC(Mod) -> fun process_module/2;
handler([_, Mod, <<"modules">>]) when ?HAS_NO_SPEC(Mod) -> fun module_opt/2;
handler([_, <<"riak">>, Mod, <<"modules">>]) when ?HAS_NO_SPEC(Mod) ->
    fun riak_opts/2;
handler([_, <<"ip_access">>, <<"mod_register">>, <<"modules">>]) ->
    fun mod_register_ip_access_rule/2;
handler([_, <<"registration_watchers">>, <<"mod_register">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"welcome_message">>, <<"mod_register">>, <<"modules">>]) ->
    fun welcome_message/2;
handler([_, <<"service">>, <<"mod_extdisco">>, <<"modules">>]) ->
    fun mod_extdisco_service/2;
handler([_, _, <<"service">>, <<"mod_extdisco">>, <<"modules">>]) ->
    fun mod_extdisco_service/2;
handler([_, <<"s3">>, <<"mod_http_upload">>, <<"modules">>]) ->
    fun mod_http_upload_s3/2;
handler([_, <<"reset_markers">>, <<"mod_inbox">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([_, <<"groupchat">>, <<"mod_inbox">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([_, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_connections/2;
handler([_, <<"cache">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_cache/2;
handler([_, <<"bounce">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_bounce/2;
handler([_, <<"redis">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_redis/2;
handler([_,<<"endpoints">>, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_connections_endpoints/2;
handler([_,<<"advertised_endpoints">>, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_connections_advertised_endpoints/2;
handler([_,<<"tls">>, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_tls_option/2;
handler([_, <<"keys">>, <<"mod_keystore">>, <<"modules">>]) ->
    fun mod_keystore_keys/2;
handler([_, _, <<"mod_mam_meta">>, <<"modules">>]) ->
    fun mod_mam_opts/2;
handler([_, <<"plugins">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"pep_mapping">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun mod_pubsub_pep_mapping/2;
handler([_, <<"default_node_config">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun mod_pubsub_default_node_config/2;
handler([_, <<"roster_groups_allowed">>, <<"default_node_config">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun mod_pubsub_roster_groups_allowed/2;
handler([_, <<"routes">>, <<"mod_revproxy">>, <<"modules">>]) ->
    fun mod_revproxy_routes/2;
handler([_, <<"stale_h">>, <<"mod_stream_management">>, <<"modules">>]) ->
    fun mod_stream_management_stale_h/2;
handler([_, <<"ldap_uids">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_uids/2;
handler([_, <<"ldap_vcard_map">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_vcard_map/2;
handler([_, <<"ldap_search_fields">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_search_fields/2;
handler([_, <<"ldap_search_reported">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_search_reported/2;
handler([_, <<"ldap_binary_search_fields">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_binary_search_fields/2;

%% host_config
handler([_, <<"host_config">>]) -> fun process_host_item/2;
handler([<<"auth">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([<<"modules">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([<<"general">>, _, <<"host_config">>]) -> fun parse_section/2;
handler([_, <<"general">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([_, <<"s2s">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler(Path) ->
    reverse_handler(lists:reverse(Path)).

reverse_handler([<<"host_config">>, {host, _} | Subtree]) ->
    handler(lists:reverse(Subtree));
reverse_handler(Path) ->
    mongoose_config_spec:handler(Path).

%% 1. Strip host_config, choose the handler for the remaining path
%% 2. Wrap the handler in a fun that calls the resulting function F for the current host
-spec handler_for_host(path()) ->
          fun((path(), toml_value()) -> option()) | mongoose_config_spec:config_node().
handler_for_host(Path) ->
    [<<"host_config">>, {host, Host} | Rest] = lists:reverse(Path),
    case handler(lists:reverse(Rest)) of
        Handler when is_function(Handler) ->
            fun(PathArg, ValueArg) ->
                    ConfigFunctions = Handler(PathArg, ValueArg),
                    lists:flatmap(fun(F) -> F(Host) end, ConfigFunctions)
            end;
        Spec ->
            Spec
    end.

-spec item_key(path(), toml_value()) -> tuple() | item.
item_key([<<"host_config">>], #{<<"host">> := Host}) -> {host, Host};
item_key(_, _) -> item.

defined_or_false(Key, Opts) ->
    case proplists:is_defined(Key, Opts) of
        true ->
            [];
        false ->
            [{Key, false}]
    end ++ Opts.

%% Processing of the parsed options

-spec get_hosts(config_list()) -> [ejabberd:server()].
get_hosts(Config) ->
    case lists:filter(fun(#config{key = hosts}) -> true;
                         (_) -> false
                      end, Config) of
        [] -> [];
        [#config{value = Hosts}] -> Hosts
    end.

-spec build_state([ejabberd:server()], [top_level_option()], [override()]) ->
          mongoose_config_parser:state().
build_state(Hosts, Opts, Overrides) ->
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                mongoose_config_parser:new_state(),
                [fun(S) -> mongoose_config_parser:set_hosts(Hosts, S) end,
                 fun(S) -> mongoose_config_parser:set_opts(Opts, S) end,
                 fun mongoose_config_parser:dedup_state_opts/1,
                 fun mongoose_config_parser:add_dep_modules/1,
                 fun(S) -> set_overrides(Overrides, S) end]).

%% Any nested option() may be a config_error() - this function extracts them all recursively
-spec extract_errors([config()]) -> [config_error()].
extract_errors(Config) ->
    extract(fun(#{what := _, class := error}) -> true;
               (_) -> false
            end, Config).

-spec extract(fun((option()) -> boolean()), option()) -> [option()].
extract(Pred, Data) ->
    case Pred(Data) of
        true -> [Data];
        false -> extract_items(Pred, Data)
    end.

-spec extract_items(fun((option()) -> boolean()), option()) -> [option()].
extract_items(Pred, L) when is_list(L) -> lists:flatmap(fun(El) -> extract(Pred, El) end, L);
extract_items(Pred, T) when is_tuple(T) -> extract_items(Pred, tuple_to_list(T));
extract_items(Pred, M) when is_map(M) -> extract_items(Pred, maps:to_list(M));
extract_items(_, _) -> [].
