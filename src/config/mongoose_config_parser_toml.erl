%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-ifdef(TEST).
-export([parse/1,
         extract_errors/1]).
-endif.

-include("mongoose.hrl").
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

%% path: *
-spec process_section(path(), toml_section() | [toml_section()]) -> config_list().
process_section([<<"services">>] = Path, Content) ->
    Services = parse_section(Path, Content),
    [#local_config{key = services, value = Services}];
process_section([<<"modules">>|_] = Path, Content) ->
    Mods = parse_section(Path, Content),
    ?HOST_F([#local_config{key = {modules, Host}, value = Mods}]);
process_section([<<"host_config">>] = Path, Content) ->
    parse_list(Path, Content);
process_section(Path, Content) ->
    parse_section(Path, Content).

%% path: (host_config[].)modules.mod_event_pusher.backend.push.wpool.*
-spec pool_option(path(), toml_value()) -> [option()].
pool_option([<<"workers">>|_], V) -> [{workers, V}];
pool_option([<<"strategy">>|_], V) -> [{strategy, b2a(V)}];
pool_option([<<"call_timeout">>|_], V) -> [{call_timeout, V}].

%% path: services.*
-spec process_service(path(), toml_section()) -> [option()].
process_service([S|_] = Path, Opts) ->
    [{b2a(S), parse_section(Path, Opts)}].

%% path: services.*.*
-spec service_opt(path(), toml_value()) -> [option()].
service_opt([<<"submods">>, <<"service_admin_extra">>|_] = Path, V) ->
    List = parse_list(Path, V),
    [{submods, List}];
service_opt([<<"initial_report">>, <<"service_mongoose_system_metrics">>|_], V) ->
    [{initial_report, V}];
service_opt([<<"periodic_report">>, <<"service_mongoose_system_metrics">>|_], V) ->
    [{periodic_report, V}];
service_opt([<<"report">>, <<"service_mongoose_system_metrics">>|_], true) ->
    [report];
service_opt([<<"report">>, <<"service_mongoose_system_metrics">>|_], false) ->
    [no_report];
service_opt([<<"tracking_id">>, <<"service_mongoose_system_metrics">>|_],  V) ->
    [{tracking_id, b2l(V)}].

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
module_opt([<<"report_commands_node">>, <<"mod_adhoc">>|_], V) ->
    [{report_commands_node, V}];
module_opt([<<"validity_period">>, <<"mod_auth_token">>|_] = Path, V) ->
    parse_list(Path, V);
module_opt([<<"inactivity">>, <<"mod_bosh">>|_], V) ->
    [{inactivity, int_or_infinity(V)}];
module_opt([<<"max_wait">>, <<"mod_bosh">>|_], V) ->
    [{max_wait, int_or_infinity(V)}];
module_opt([<<"server_acks">>, <<"mod_bosh">>|_], V) ->
    [{server_acks, V}];
module_opt([<<"maxpause">>, <<"mod_bosh">>|_], V) ->
    [{maxpause, V}];
module_opt([<<"cache_size">>, <<"mod_caps">>|_], V) ->
    [{cache_size, V}];
module_opt([<<"cache_life_time">>, <<"mod_caps">>|_], V) ->
    [{cache_life_time, V}];
module_opt([<<"buffer_max">>, <<"mod_csi">>|_], V) ->
    [{buffer_max, int_or_infinity(V)}];
module_opt([<<"extra_domains">>, <<"mod_disco">>|_] = Path, V) ->
    Domains = parse_list(Path, V),
    [{extra_domains, Domains}];
module_opt([<<"server_info">>, <<"mod_disco">>|_] = Path, V) ->
    Info = parse_list(Path, V),
    [{server_info, Info}];
module_opt([<<"users_can_see_hidden_services">>, <<"mod_disco">>|_], V) ->
    [{users_can_see_hidden_services, V}];
module_opt([<<"backend">>, <<"mod_event_pusher">>|_] = Path, V) ->
    Backends = parse_section(Path, V),
    [{backends, Backends}];
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
module_opt([<<"host">>, <<"mod_muc">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"access">>, <<"mod_muc">>|_], V) ->
    [{access, b2a(V)}];
module_opt([<<"access_create">>, <<"mod_muc">>|_], V) ->
    [{access_create, b2a(V)}];
module_opt([<<"access_admin">>, <<"mod_muc">>|_], V) ->
    [{access_admin, b2a(V)}];
module_opt([<<"access_persistent">>, <<"mod_muc">>|_], V) ->
    [{access_persistent, b2a(V)}];
module_opt([<<"history_size">>, <<"mod_muc">>|_], V) ->
    [{history_size, V}];
module_opt([<<"room_shaper">>, <<"mod_muc">>|_], V) ->
    [{room_shaper, b2a(V)}];
module_opt([<<"max_room_id">>, <<"mod_muc">>|_], V) ->
    [{max_room_id, int_or_infinity(V)}];
module_opt([<<"max_room_name">>, <<"mod_muc">>|_], V) ->
    [{max_room_name, int_or_infinity(V)}];
module_opt([<<"max_room_desc">>, <<"mod_muc">>|_], V) ->
    [{max_room_desc, int_or_infinity(V)}];
module_opt([<<"min_message_interval">>, <<"mod_muc">>|_], V) ->
    [{min_message_interval, V}];
module_opt([<<"min_presence_interval">>, <<"mod_muc">>|_], V) ->
    [{min_presence_interval, V}];
module_opt([<<"max_users">>, <<"mod_muc">>|_], V) ->
    [{max_users, V}];
module_opt([<<"max_users_admin_threshold">>, <<"mod_muc">>|_], V) ->
    [{max_users_admin_threshold, V}];
module_opt([<<"user_message_shaper">>, <<"mod_muc">>|_], V) ->
    [{user_message_shaper, b2a(V)}];
module_opt([<<"user_presence_shaper">>, <<"mod_muc">>|_], V) ->
    [{user_presence_shaper, b2a(V)}];
module_opt([<<"max_user_conferences">>, <<"mod_muc">>|_], V) ->
    [{max_user_conferences, V}];
module_opt([<<"http_auth_pool">>, <<"mod_muc">>|_], V) ->
    [{http_auth_pool, b2a(V)}];
module_opt([<<"load_permanent_rooms_at_startup">>, <<"mod_muc">>|_], V) ->
    [{load_permanent_rooms_at_startup, V}];
module_opt([<<"hibernate_timeout">>, <<"mod_muc">>|_], V) ->
    [{hibernate_timeout, V}];
module_opt([<<"hibernated_room_check_interval">>, <<"mod_muc">>|_], V) ->
    [{hibernated_room_check_interval, int_or_infinity(V)}];
module_opt([<<"hibernated_room_timeout">>, <<"mod_muc">>|_], V) ->
    [{hibernated_room_timeout, int_or_infinity(V)}];
module_opt([<<"default_room">>, <<"mod_muc">>|_] = Path, V) ->
    Defaults = parse_section(Path, V),
    [{default_room_options, Defaults}];
module_opt([<<"outdir">>, <<"mod_muc_log">>|_], V) ->
    [{outdir, b2l(V)}];
module_opt([<<"access_log">>, <<"mod_muc_log">>|_], V) ->
    [{access_log, b2a(V)}];
module_opt([<<"dirtype">>, <<"mod_muc_log">>|_], V) ->
    [{dirtype, b2a(V)}];
module_opt([<<"dirname">>, <<"mod_muc_log">>|_], V) ->
    [{dirname, b2a(V)}];
module_opt([<<"file_format">>, <<"mod_muc_log">>|_], V) ->
    [{file_format, b2a(V)}];
module_opt([<<"css_file">>, <<"mod_muc_log">>|_], <<"false">>) ->
    [{cssfile, false}];
module_opt([<<"css_file">>, <<"mod_muc_log">>|_], V) ->
    [{cssfile, V}];
module_opt([<<"timezone">>, <<"mod_muc_log">>|_], V) ->
    [{timezone, b2a(V)}];
module_opt([<<"top_link">>, <<"mod_muc_log">>|_] = Path, V) ->
    Link = list_to_tuple(parse_section(Path, V)),
    [{top_link, Link}];
module_opt([<<"spam_prevention">>, <<"mod_muc_log">>|_], V) ->
    [{spam_prevention, V}];
module_opt([<<"host">>, <<"mod_muc_light">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"equal_occupants">>, <<"mod_muc_light">>|_], V) ->
    [{equal_occupants, V}];
module_opt([<<"legacy_mode">>, <<"mod_muc_light">>|_], V) ->
    [{legacy_mode, V}];
module_opt([<<"rooms_per_user">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_per_user, int_or_infinity(V)}];
module_opt([<<"blocking">>, <<"mod_muc_light">>|_], V) ->
    [{blocking, V}];
module_opt([<<"all_can_configure">>, <<"mod_muc_light">>|_], V) ->
    [{all_can_configure, V}];
module_opt([<<"all_can_invite">>, <<"mod_muc_light">>|_], V) ->
    [{all_can_invite, V}];
module_opt([<<"max_occupants">>, <<"mod_muc_light">>|_], V) ->
    [{max_occupants, int_or_infinity(V)}];
module_opt([<<"rooms_per_page">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_per_page, int_or_infinity(V)}];
module_opt([<<"rooms_in_rosters">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_in_rosters, V}];
module_opt([<<"config_schema">>, <<"mod_muc_light">>|_] = Path, V) ->
    Configs = parse_list(Path, V),
    [{config_schema, Configs}];
module_opt([<<"access_max_user_messages">>, <<"mod_offline">>|_], V) ->
    [{access_max_user_messages, b2a(V)}];
module_opt([<<"send_pings">>, <<"mod_ping">>|_], V) ->
    [{send_pings, V}];
module_opt([<<"ping_interval">>, <<"mod_ping">>|_], V) ->
    [{ping_interval, V}];
module_opt([<<"timeout_action">>, <<"mod_ping">>|_], V) ->
    [{timeout_action, b2a(V)}];
module_opt([<<"ping_req_timeout">>, <<"mod_ping">>|_], V) ->
    [{ping_req_timeout, V}];
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

-spec mod_auth_token_validity_periods(path(), toml_section()) -> [option()].
mod_auth_token_validity_periods(_,
    #{<<"token">> := Token, <<"value">> := Value, <<"unit">> := Unit}) ->
        [{{validity_period, b2a(Token)}, {Value, b2a(Unit)}}].

-spec mod_disco_server_info(path(), toml_section()) -> [option()].
mod_disco_server_info(Path, #{<<"module">> := <<"all">>, <<"name">> := Name, <<"urls">> := Urls}) ->
    URLList = parse_list([<<"urls">> | Path], Urls),
    [{all, b2l(Name), URLList}];
mod_disco_server_info(Path, #{<<"module">> := Modules, <<"name">> := Name, <<"urls">> := Urls}) ->
    Mods = parse_list([<<"module">> | Path], Modules),
    URLList = parse_list([<<"urls">> | Path], Urls),
    [{Mods, b2l(Name), URLList}].

-spec mod_event_pusher_backend_sns(path(), toml_section()) -> [option()].
mod_event_pusher_backend_sns(Path, Opts) ->
    SnsOpts = parse_section(Path, Opts),
    [{sns, SnsOpts}].

-spec mod_event_pusher_backend_push(path(), toml_section()) -> [option()].
mod_event_pusher_backend_push(Path, Opts) ->
    PushOpts = parse_section(Path, Opts),
    [{push, PushOpts}].

-spec mod_event_pusher_backend_http(path(), toml_section()) -> [option()].
mod_event_pusher_backend_http(Path, Opts) ->
    HttpOpts = parse_section(Path, Opts),
    [{http, HttpOpts}].

-spec mod_event_pusher_backend_rabbit(path(), toml_section()) -> [option()].
mod_event_pusher_backend_rabbit(Path, Opts) ->
    ROpts = parse_section(Path, Opts),
    [{rabbit, ROpts}].

-spec mod_event_pusher_backend_sns_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_sns_opts([<<"presence_updates_topic">>|_], V) ->
    [{presence_updates_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"pm_messages_topic">>|_], V) ->
    [{pm_messages_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"muc_messages_topic">>|_], V) ->
    [{muc_messages_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"plugin_module">>|_], V) ->
    [{plugin_module, b2a(V)}];
mod_event_pusher_backend_sns_opts([<<"muc_host">>|_], V) ->
    [{muc_host, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"sns_host">>|_], V) ->
    [{sns_host, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"region">>|_], V) ->
    [{region, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"access_key_id">>|_], V) ->
    [{access_key_id, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"secret_access_key">>|_], V) ->
    [{secret_access_key, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"account_id">>|_], V) ->
    [{account_id, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"pool_size">>|_], V) ->
    [{pool_size, V}];
mod_event_pusher_backend_sns_opts([<<"publish_retry_count">>|_], V) ->
    [{publish_retry_count, V}];
mod_event_pusher_backend_sns_opts([<<"publish_retry_time_ms">>|_], V) ->
    [{publish_retry_time_ms, V}].

-spec mod_event_pusher_backend_push_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_push_opts([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
mod_event_pusher_backend_push_opts([<<"wpool">>|_] = Path, V) ->
    WpoolOpts = parse_section(Path, V),
    [{wpool, WpoolOpts}];
mod_event_pusher_backend_push_opts([<<"plugin_module">>|_], V) ->
    [{plugin_module, b2a(V)}];
mod_event_pusher_backend_push_opts([<<"virtual_pubsub_hosts">> |_] = Path, V) ->
    VPH = parse_list(Path, V),
    [{virtual_pubsub_hosts, VPH}].

-spec mod_event_pusher_backend_http_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_http_opts([<<"pool_name">>|_], V) ->
    [{pool_name, b2a(V)}];
mod_event_pusher_backend_http_opts([<<"path">>|_], V) ->
    [{path, b2l(V)}];
mod_event_pusher_backend_http_opts([<<"callback_module">>|_], V) ->
    [{callback_module, b2a(V)}].

-spec mod_event_pusher_backend_rabbit_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_rabbit_opts([<<"presence_exchange">>|_] = Path, V) ->
    [{presence_exchange, parse_section(Path, V)}];
mod_event_pusher_backend_rabbit_opts([<<"chat_msg_exchange">>|_] = Path, V) ->
    [{chat_msg_exchange, parse_section(Path, V)}];
mod_event_pusher_backend_rabbit_opts([<<"groupchat_msg_exchange">>|_] = Path, V) ->
    [{groupchat_msg_exchange, parse_section(Path, V)}].

-spec mod_event_pusher_rabbit_presence_ex(path(), toml_value()) -> [option()].
mod_event_pusher_rabbit_presence_ex([<<"name">>|_], V) ->
    [{name, V}];
mod_event_pusher_rabbit_presence_ex([<<"type">>|_], V) ->
    [{type, V}].

-spec mod_event_pusher_rabbit_msg_ex(path(), toml_value()) -> [option()].
mod_event_pusher_rabbit_msg_ex([<<"name">>|_], V) ->
    [{name, V}];
mod_event_pusher_rabbit_msg_ex([<<"type">>|_], V) ->
    [{type, V}];
mod_event_pusher_rabbit_msg_ex([<<"sent_topic">>|_], V) ->
    [{sent_topic, V}];
mod_event_pusher_rabbit_msg_ex([<<"recv_topic">>|_], V) ->
    [{recv_topic, V}].

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

-spec mod_muc_default_room(path(), toml_value()) -> [option()].
mod_muc_default_room([<<"title">>|_], V) ->
    [{title, V}];
mod_muc_default_room([<<"description">>|_], V) ->
    [{description, V}];
mod_muc_default_room([<<"allow_change_subj">>|_], V) ->
    [{allow_change_subj, V}];
mod_muc_default_room([<<"allow_query_users">>|_], V) ->
    [{allow_query_users, V}];
mod_muc_default_room([<<"allow_private_messages">>|_], V) ->
    [{allow_private_messages, V}];
mod_muc_default_room([<<"allow_visitor_status">>|_], V) ->
    [{allow_visitor_status, V}];
mod_muc_default_room([<<"allow_visitor_nickchange">>|_], V) ->
    [{allow_visitor_nickchange, V}];
mod_muc_default_room([<<"public">>|_], V) ->
    [{public, V}];
mod_muc_default_room([<<"public_list">>|_], V) ->
    [{public_list, V}];
mod_muc_default_room([<<"persistent">>|_], V) ->
    [{persistent, V}];
mod_muc_default_room([<<"moderated">>|_], V) ->
    [{moderated, V}];
mod_muc_default_room([<<"members_by_default">>|_], V) ->
    [{members_by_default, V}];
mod_muc_default_room([<<"members_only">>|_], V) ->
    [{members_only, V}];
mod_muc_default_room([<<"allow_user_invites">>|_], V) ->
    [{allow_user_invites, V}];
mod_muc_default_room([<<"allow_multiple_sessions">>|_], V) ->
    [{allow_multiple_sessions, V}];
mod_muc_default_room([<<"password_protected">>|_], V) ->
    [{password_protected, V}];
mod_muc_default_room([<<"password">>|_], V) ->
    [{password, V}];
mod_muc_default_room([<<"anonymous">>|_], V) ->
    [{anonymous, V}];
mod_muc_default_room([<<"max_users">>|_], V) ->
    [{max_users, V}];
mod_muc_default_room([<<"logging">>|_], V) ->
    [{logging, V}];
mod_muc_default_room([<<"maygetmemberlist">>|_] = Path, V) ->
    List = parse_list(Path, V),
    [{maygetmemberlist, List}];
mod_muc_default_room([<<"affiliations">>|_] = Path, V) ->
    Affs = parse_list(Path, V),
    [{affiliations, Affs}];
mod_muc_default_room([<<"subject">>|_], V) ->
    [{subject, V}];
mod_muc_default_room([<<"subject_author">>|_], V) ->
    [{subject_author, V}].

-spec mod_muc_default_room_affiliations(path(), toml_section()) -> [option()].
mod_muc_default_room_affiliations(_, #{<<"user">> := User, <<"server">> := Server,
    <<"resource">> := Resource, <<"affiliation">> := Aff}) ->
    [{{User, Server, Resource}, b2a(Aff)}].

-spec mod_muc_log_top_link(path(), toml_value()) -> [option()].
mod_muc_log_top_link([<<"target">>|_], V) ->
    [b2l(V)];
mod_muc_log_top_link([<<"text">>|_], V) ->
    [b2l(V)].

-spec mod_muc_light_config_schema(path(), toml_section()) -> [option()].
mod_muc_light_config_schema(_, #{<<"field">> := Field, <<"value">> := Val,
                                 <<"internal_key">> := Key, <<"type">> := Type}) ->
    [{b2l(Field), Val, b2a(Key), b2a(Type)}];
mod_muc_light_config_schema(_, #{<<"field">> := Field, <<"value">> := Val}) ->
    [{b2l(Field), b2l(Val)}].

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

-spec service_admin_extra_submods(path(), toml_value()) -> [option()].
service_admin_extra_submods(_, V) ->
    [b2a(V)].

welcome_message([<<"subject">>|_], Value) ->
    [{subject, b2l(Value)}];
welcome_message([<<"body">>|_], Value) ->
    [{body, b2l(Value)}].

%% path: (host_config[].)shaper.*
-spec process_shaper(path(), toml_section()) -> [config()].
process_shaper([Name, _|Path], #{<<"max_rate">> := MaxRate}) ->
    [#config{key = {shaper, b2a(Name), host(Path)}, value = {maxrate, MaxRate}}].

%% path: (host_config[].)acl.*
-spec process_acl(path(), toml_value()) -> [config()].
process_acl([item, ACLName, _|Path], Content) ->
    [acl:to_record(host(Path), b2a(ACLName), acl_data(Content))].

-spec acl_data(toml_value()) -> option().
acl_data(#{<<"match">> := <<"all">>}) -> all;
acl_data(#{<<"match">> := <<"none">>}) -> none;
acl_data(M) ->
    {AclName, AclKeys} = find_acl(M, lists:sort(maps:keys(M)), acl_keys()),
    list_to_tuple([AclName | lists:map(fun(K) -> maps:get(K, M) end, AclKeys)]).

find_acl(M, SortedMapKeys, [{AclName, AclKeys}|Rest]) ->
    case lists:sort(AclKeys) of
        SortedMapKeys -> {AclName, AclKeys};
        _ -> find_acl(M, SortedMapKeys, Rest)
    end.

acl_keys() ->
    [{user, [<<"user">>, <<"server">>]},
     {user, [<<"user">>]},
     {server, [<<"server">>]},
     {resource, [<<"resource">>]},
     {user_regexp, [<<"user_regexp">>, <<"server">>]},
     {node_regexp, [<<"user_regexp">>, <<"server_regexp">>]},
     {user_regexp, [<<"user_regexp">>]},
     {server_regexp, [<<"server_regexp">>]},
     {resource_regexp, [<<"resource_regexp">>]},
     {user_glob, [<<"user_glob">>, <<"server">>]},
     {node_glob, [<<"user_glob">>, <<"server_glob">>]},
     {user_glob, [<<"user_glob">>]},
     {server_glob, [<<"server_glob">>]},
     {resource_glob, [<<"resource_glob">>]}
    ].

%% path: (host_config[].)access.*
-spec process_access_rule(path(), toml_value()) -> [config()].
process_access_rule([Name, _|HostPath] = Path, Contents) ->
    Rules = parse_list(Path, Contents),
    [#config{key = {access, b2a(Name), host(HostPath)}, value = Rules}].

%% path: (host_config[].)access.*[]
-spec process_access_rule_item(path(), toml_section()) -> [option()].
process_access_rule_item(_, #{<<"acl">> := ACL, <<"value">> := Value}) ->
    [{access_rule_value(Value), b2a(ACL)}].

host([]) -> global;
host([{host, Host}, _]) -> Host.

-spec access_rule_value(toml_value()) -> option().
access_rule_value(B) when is_binary(B) -> b2a(B);
access_rule_value(V) -> V.

%% path: (host_config[].)s2s.*
-spec process_s2s_option(path(), toml_value()) -> config_list().
process_s2s_option([<<"dns">>|_] = Path, V) ->
    [#local_config{key = s2s_dns_options, value = parse_section(Path, V)}];
process_s2s_option([<<"outgoing">>|_] = Path, V) ->
    parse_section(Path, V);
process_s2s_option([<<"use_starttls">>|_], V) ->
    [#local_config{key = s2s_use_starttls, value = b2a(V)}];
process_s2s_option([<<"certfile">>|_], V) ->
    [#local_config{key = s2s_certfile, value = b2l(V)}];
process_s2s_option([<<"default_policy">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_default_policy, Host}, value = b2a(V)}]);
process_s2s_option([<<"host_policy">>|_] = Path, V) ->
    parse_list(Path, V);
process_s2s_option([<<"address">>|_] = Path, V) ->
    parse_list(Path, V);
process_s2s_option([<<"ciphers">>|_], V) ->
    [#local_config{key = s2s_ciphers, value = b2l(V)}];
process_s2s_option([<<"domain_certfile">>|_] = Path, V) ->
    parse_list(Path, V);
process_s2s_option([<<"shared">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_shared, Host}, value = V}]);
process_s2s_option([<<"max_retry_delay">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_max_retry_delay, Host}, value = V}]).

%% path: s2s.dns.*
-spec s2s_dns_opt(path(), toml_value()) -> [option()].
s2s_dns_opt([<<"timeout">>|_], Value) -> [{timeout, Value}];
s2s_dns_opt([<<"retries">>|_], Value) -> [{retries, Value}].

%% path: s2s.outgoing.*
-spec outgoing_s2s_opt(path(), toml_value()) -> [config()].
outgoing_s2s_opt([<<"port">>|_], Value) ->
    [#local_config{key = outgoing_s2s_port, value = Value}];
outgoing_s2s_opt([<<"ip_versions">>|_] = Path, Value) ->
    [#local_config{key = outgoing_s2s_families, value = parse_list(Path, Value)}];
outgoing_s2s_opt([<<"connection_timeout">>|_], Value) ->
    [#local_config{key = outgoing_s2s_timeout, value = int_or_infinity(Value)}].

%% path: s2s.outgoing.ip_versions[]
-spec s2s_address_family(path(), toml_value()) -> [option()].
s2s_address_family(_, 4) -> [ipv4];
s2s_address_family(_, 6) -> [ipv6].

%% path: s2s.host_policy[]
-spec s2s_host_policy(path(), toml_section()) -> config_list().
s2s_host_policy(Path, M) ->
    parse_section(Path, M, fun process_host_policy/1).

process_host_policy(Opts) ->
    {_, S2SHost} = proplists:lookup(host, Opts),
    {_, Policy} = proplists:lookup(policy, Opts),
    ?HOST_F([#local_config{key = {{s2s_host, S2SHost}, Host}, value = Policy}]).

%% path: s2s.host_policy[].*
-spec s2s_host_policy_opt(path(), toml_value()) -> [option()].
s2s_host_policy_opt([<<"host">>|_], V) -> [{host, V}];
s2s_host_policy_opt([<<"policy">>|_], V) -> [{policy, b2a(V)}].

%% path: s2s.address[]
-spec s2s_address(path(), toml_section()) -> [config()].
s2s_address(Path, M) ->
    parse_section(Path, M, fun process_s2s_address/1).

process_s2s_address(Opts) ->
    {_, Host} = proplists:lookup(host, Opts),
    {_, IPAddress} = proplists:lookup(ip_address, Opts),
    Addr = case proplists:lookup(port, Opts) of
               {_, Port} -> {IPAddress, Port};
               none -> IPAddress
           end,
    [#local_config{key = {s2s_addr, Host}, value = Addr}].

%% path: s2s.address[].*
-spec s2s_addr_opt(path(), toml_value()) -> [option()].
s2s_addr_opt([<<"host">>|_], V) -> [{host, V}];
s2s_addr_opt([<<"ip_address">>|_], V) -> [{ip_address, b2l(V)}];
s2s_addr_opt([<<"port">>|_], V) -> [{port, V}].

%% path: s2s.domain_certfile[]
-spec s2s_domain_cert(path(), toml_section()) -> [config()].
s2s_domain_cert(_, #{<<"domain">> := Dom, <<"certfile">> := Cert}) ->
    [#local_config{key = {domain_certfile, b2l(Dom)}, value = b2l(Cert)}].

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

convert(V, boolean) -> V;
convert(V, binary) -> V;
convert(V, string) -> binary_to_list(V);
convert(V, atom) -> b2a(V);
convert(<<"infinity">>, int_or_infinity) -> infinity; %% TODO maybe use TOML '+inf'
convert(V, int_or_infinity) -> V;
convert(V, integer) -> V.

format_spec(#section{format = Format}) -> Format;
format_spec(#list{format = Format}) -> Format;
format_spec(#option{format = Format}) -> Format.

format(Path, L, {foreach, Format}) when is_atom(Format) ->
    lists:flatmap(fun({K, V}) -> format(Path, V, {Format, K}) end, L);
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

-spec handler(path()) ->
          fun((path(), toml_value()) -> option()) | mongoose_config_spec:config_node().
handler([]) -> fun parse_root/2;
handler([Section]) when Section =/= <<"general">>,
                        Section =/= <<"listen">>,
                        Section =/= <<"auth">>,
                        Section =/= <<"outgoing_pools">> -> fun process_section/2;

%% services
handler([_, <<"services">>]) -> fun process_service/2;
handler([_, _, <<"services">>]) -> fun service_opt/2;

%% modules
handler([_, <<"modules">>]) -> fun process_module/2;
handler([_, _, <<"modules">>]) -> fun module_opt/2;
handler([_, <<"riak">>, _, <<"modules">>]) ->
    fun riak_opts/2;
handler([_, <<"ip_access">>, <<"mod_register">>, <<"modules">>]) ->
    fun mod_register_ip_access_rule/2;
handler([_, <<"registration_watchers">>, <<"mod_register">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"welcome_message">>, <<"mod_register">>, <<"modules">>]) ->
    fun welcome_message/2;
handler([_, <<"validity_period">>, <<"mod_auth_token">>, <<"modules">>]) ->
    fun mod_auth_token_validity_periods/2;
handler([_, <<"extra_domains">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun mod_disco_server_info/2;
handler([_, <<"urls">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [b2l(V)] end;
handler([_, <<"module">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([<<"sns">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_sns/2;
handler([<<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_push/2;
handler([<<"http">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_http/2;
handler([<<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_rabbit/2;
handler([_, <<"sns">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_sns_opts/2;
handler([_, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_push_opts/2;
handler([_, <<"http">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_http_opts/2;
handler([_, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_rabbit_opts/2;
handler([_,<<"wpool">>, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun pool_option/2;
handler([_,<<"virtual_pubsub_hosts">>, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun (_, V) -> [b2l(V)] end;
handler([_,<<"presence_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_presence_ex/2;
handler([_,<<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_msg_ex/2;
handler([_,<<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_msg_ex/2;
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
handler([_, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun mod_muc_default_room/2;
handler([_, <<"maygetmemberlist">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun (_, V) -> [b2a(V)] end;
handler([_, <<"affiliations">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun mod_muc_default_room_affiliations/2;
handler([_, <<"top_link">>, <<"mod_muc_log">>, <<"modules">>]) ->
    fun mod_muc_log_top_link/2;
handler([_, <<"config_schema">>, <<"mod_muc_light">>, <<"modules">>]) ->
    fun mod_muc_light_config_schema/2;
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
handler([_, <<"submods">>, <<"service_admin_extra">>, <<"services">>]) ->
    fun service_admin_extra_submods/2;


%% shaper, acl, access
handler([_, <<"shaper">>]) -> fun process_shaper/2;
handler([_, <<"acl">>]) -> fun parse_list/2;
handler([_, _, <<"acl">>]) -> fun process_acl/2;
handler([_, <<"access">>]) -> fun process_access_rule/2;
handler([_, _, <<"access">>]) -> fun process_access_rule_item/2;

%% s2s
handler([_, <<"s2s">>]) -> fun process_s2s_option/2;
handler([_, <<"dns">>, <<"s2s">>]) -> fun s2s_dns_opt/2;
handler([_, <<"outgoing">>, <<"s2s">>]) -> fun outgoing_s2s_opt/2;
handler([_, <<"ip_versions">>, <<"outgoing">>, <<"s2s">>]) -> fun s2s_address_family/2;
handler([_, <<"host_policy">>, <<"s2s">>]) -> fun s2s_host_policy/2;
handler([_, _, <<"host_policy">>, <<"s2s">>]) -> fun s2s_host_policy_opt/2;
handler([_, <<"address">>, <<"s2s">>]) -> fun s2s_address/2;
handler([_, _, <<"address">>, <<"s2s">>]) -> fun s2s_addr_opt/2;
handler([_, <<"domain_certfile">>, <<"s2s">>]) -> fun s2s_domain_cert/2;

%% host_config
handler([_, <<"host_config">>]) -> fun process_host_item/2;
handler([<<"auth">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([<<"modules">>, _, <<"host_config">>] = P) -> handler_for_host(P);
handler([_, _, <<"host_config">>]) -> fun process_section/2;
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
