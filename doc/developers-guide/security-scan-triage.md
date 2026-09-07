# Security scan triage

This page records the triage of findings reported by [SAFE][safe], the static analyser we run over
the compiled BEAM files. It exists so that a new scan can be compared against a known baseline:
anything here has been reviewed and accepted, so anything *not* here deserves a look.

Update this page whenever a finding is triaged, and reference it from the pull request that does so.

[safe]: https://safe-docs.erlang-solutions.com/

## Baseline

| Field | Value |
| --- | --- |
| Scan date | 2026-09-03 |
| Commit | `cb51cb16b` |
| SAFE version | 1.5.1 (report format 1.1) |
| Modules analysed | 558 |
| Functions checked | 9679 |
| Finding locations | 71 (35 high, 36 low) |

Of those 71 locations, two are real defects and neither is fixed; the remaining 69 are accepted.
SAFE matches call sites of risky BIFs syntactically, without tracking where the argument comes from,
so a high proportion of false positives is expected rather than alarming.

Both defects are on the `mod_global_distrib` receive path, which is the one place in this list where
a risky BIF is reached from the network before authentication.

## Outstanding

### Unsafe deserialisation in global distribution

`mod_global_distrib_worker:do_work/1` calls `binary_to_term/1` on bytes taken straight off a network
socket, reachable from `mod_global_distrib_receiver:handle_data/2` with no validation in between.
Because TLS on the global distribution transport is optional, anyone able to reach the endpoint can
create atoms (which are never garbage collected) and fabricate pids, refs and funs.

Nothing has been done about it yet. Remediating it means stripping the accumulator of node-local
resources before serialisation, so that decoding can use the `safe` option at all, and having the
receiver drop undecodable payloads rather than crash the worker. Both are wire-format changes and
need their own discussion.

### Atom exhaustion on the worker-name path

`mod_global_distrib_worker_sup:get_worker/1` registers its worker under an atom built by
`mod_global_distrib_utils:any_binary_to_atom/1`, which is `binary_to_atom(base64:encode(Binary))` —
a new atom for every distinct input. On the receiving side that input is taken straight off the
wire, with its length also read from the packet:

```erlang
%% mod_global_distrib_receiver:handle_data/2
<<ClockTime:64, BinFromSize:16, _/binary>> = Data,
<<_:80, BinFrom:BinFromSize/binary, BinTerm/binary>> = Data,
Worker = mod_global_distrib_worker_sup:get_worker(BinFrom),
```

Nothing validates `BinFrom` first. Atoms are never garbage collected, so a peer sending a distinct
prefix per frame grows the atom table until the node dies. Remediating it means reusing an existing
atom where there is one and minting new ones only up to a fixed cap, with callers dropping and
logging rather than crashing once the cap is hit.

The same function is reached from `mod_global_distrib_bounce:resend_messages/1`, where the key comes
from `recipient_to_worker_key/2`. That returns the full JID — including the client-chosen resource —
when the sender is on the global host, so it is one atom per distinct JID rather than per peer.
Reaching it needs an authenticated user or a federated peer rather than an anonymous one, so it is
less severe, but it is the same unbounded shape and the same cap would cover it.

### Mitigation for both

Until these are fixed this is a deployment concern: treat the endpoints configured in
`connections.endpoints` as reachable only from the other datacenters, by network segmentation or a
firewall, and configure TLS on the transport, which is off by default. See
[`mod_global_distrib`](../modules/mod_global_distrib.md) for the options involved.

## Accepted: atom exhaustion (`SAFE-ISSUE-ATOM`)

61 of the 71 locations are calls to `list_to_atom/1`, `binary_to_atom/1,2` or `binary_to_term/1`
flagged as possible atom exhaustion. Two of those are the real defects above. The remaining 59 fall
into the four groups below, plus the datastore deserialisation group, which has its own section
because it rests on a different argument.

Each group's claim is about where the argument comes from, which is precisely what SAFE does not
track and why it flagged the call in the first place. The claims below were checked by reading the
call paths, not assumed.

### Config-derived

The argument originates in `mongooseim.toml`. An operator who can edit the configuration file does
not need atom exhaustion to affect the node, so these are not a privilege boundary.

`ejabberd_auth:auth_method_to_module/1`, `mongoose_config_parser_toml:b2a/1`,
`mongoose_config_validator:validate/3`, `ejabberd_cowboy:ref/1`, `gen_mod:get_module_proc/2`,
`mongoose_wpool:make_pool_name/3`, `mongoose_wpool:make_callback_module_name/1`,
`mongoose_wpool_mgr:name/1`, `mongoose_wpool_type_sup:name/1`, `mod_muc_online_cets:table_name/1`,
`mod_keystore_cets:table_name/1`, `ejabberd_auth_anonymous_cets:table_name/1`,
`mod_caps_backend_cets:store_table_names/1`, `mongoose_config_spec:process_sasl_mechanism/1`,
`mongoose_config_spec:b2a/1`, `ejabberd_auth_jwt:check_password/4`,
`mongoose_instrument_exometer:process_graphite_reporter/2`, `mongoose_http_handler:process_config/2`,
`mod_global_distrib_server_sup:endpoint_to_atom/1`.

Most of these take a host type, which `mongoose_domain_core:get_host_type/1` resolves by ETS lookup
and which therefore cannot be anything but a configured value.

One caveat on `gen_mod:get_module_proc/2`: the reasoning above covers almost every caller, but
`mod_global_distrib_utils:server_to_mgr_name/1` and `server_to_sup_name/1` pass a server taken from
`mod_global_distrib_mapping:hosts()`, which reads the Redis-backed host mapping. Those two are
bounded by the datastore trust assumption below rather than by the configuration file.

### Literal-derived

The argument is a compile-time constant, so the set of atoms is fixed.

`mod_muc:keys_as_atoms/1` only ever runs over the hardcoded `default_room_opts()` map.
`mongoose_graphql_directive_protected:protected_dir_args_to_map/1` and
`mongoose_graphql_directive_use:error_key/1` take their keys from the compiled GraphQL schema.
`mod_muc_light:process_config_schema_item/1`, `mod_pubsub_old:plugin/1`,
`mongoose_backend:backend_module/2`, `mongoose_instrument_hooks:event_name/1`,
`mod_mam_muc:action_to_muc_action/1`, `mod_mam_utils:action_to_shaper_name/1` and
`mod_mam_utils:action_to_global_shaper_name/1` likewise build names from a closed set.

### Query-name builders

Statement names assembled from a bounded set of filter combinations, then cached:
`rdbms_queries:mysql_derived_name/2`, `mod_inbox_rdbms:lookup_query_name/1`,
`mod_inbox_rdbms:update_query_name/1`, `mam_lookup_sql:filters_to_statement_name/6`,
`mod_vcard_rdbms:filters_to_statement_name/2`, `mod_mam_rdbms_arch_async:multi_name/2`,
`mongoose_async_pools:sup_name/2`, `mongoose_async_pools:gen_pool_name/2`.

### Admin-API-derived

Reachable only after administrator authentication. These are accepted on the privilege boundary
alone: an administrator who can reach them can already restart the node or remove it from the
cluster, so atom exhaustion adds nothing.

Note the bound really is just the privilege. `mongoose_graphql_enum:input/2` and
`mongoose_graphql_scalar:node_from_binary/1` are constrained by their schema types, but
`mongoose_graphql_metric_admin_query:get_name/1` and `get_nodes/1` convert the elements of a
`[String]` argument, and schema validation only checks that they are strings, not which ones. The
same goes for the node names taken by `mnesia_api`, `ejabberd_admin` and `mongoose_server_api`.

`mnesia_api:mnesia_info/1`, `mnesia_api:dump_table/2`, `mongoose_graphql_scalar:node_from_binary/1`,
`mongoose_graphql_enum:input/2`, `mongoose_graphql_metric_admin_query:prepare_key/1`,
`mongoose_graphql_metric_admin_query:get_name/1`,
`mongoose_graphql_metric_admin_query:get_nodes/1`, `ejabberd_admin:remove_from_cluster/1`,
`ejabberd_ctl:start/0`, `mongoose_server_api:join_cluster/1`,
`mongoose_server_api:remove_from_cluster/1`, `mongoose_server_api:remove_node/1`.

## Accepted: datastore deserialisation

These call `binary_to_term/1` on data that MongooseIM itself wrote, and are safe **as long as the
datastore is trusted**:

- `ejabberd_sm_redis:get_sessions/0,1,2,3` and `ejabberd_sm_redis:parse_session_key/1`
- `mod_offline_rdbms:extract_permanent_fields/1`
- `mam_message_eterm:decode/1`

This is a deliberate trust assumption, not an oversight. An unauthenticated Redis instance on a
shared network turns the session store into a term-injection path into every node that reads it, so
the session and archive backends must be treated as trusted infrastructure and access-controlled
accordingly. Note that the `safe` option is not a drop-in here either: stored session records
legitimately contain pids and refs.

## Rejected as incorrect

### Man in the middle (5 locations)

SAFE reports `ssl:connect/3,4` and `ssl:handshake/3` reached from `just_tls:tcp_to_tls/3`,
`mongoose_xmpp_socket:tcp_to_tls/3` and `mongoose_xmpp_socket:connect/4` as missing certificate
verification. It cannot see through `just_tls:format_opts/2`, which builds the
options list. `just_tls:verify_mode_opt/2` emits `{verify, verify_peer}` unless the operator
explicitly configures `verify_mode = none`, and `mongoose_config_spec` defaults `verify_mode` to
`peer`. Verification is on by default.

### Injection (2 locations)

`gdpr_api:run/3` calls `open_port({spawn_executable, Cmd}, [..., {args, Args}])`. `spawn_executable`
passes the argument vector directly to `execve` without a shell, so user-controlled values in `Args`
cannot inject a command. `extauth:init/2` spawns the program named by `auth.external.program`, which
is the documented purpose of the module.

### Race conditions (3 locations)

`ejabberd_sm_cets:unique_count/0` with its helper `ejabberd_sm_cets:compute_unique/2`, and
`mod_global_distrib_bounce:resend_messages/1`, traverse ETS
tables with `ets:first/1` and `ets:next/2` while other processes write. CETS tables are created as
`ordered_set`, where traversal remains well defined if the current key is deleted concurrently. The
worst outcome is a slightly stale session count on a metrics probe, and
`mod_global_distrib_bounce:resend_messages/1` already handles the lost race explicitly by matching
`ets:take/2` returning `_ -> ok`.

## Suppressing findings in the scanner

SAFE supports inline suppression comments: a comment on the line immediately above the flagged call,
naming the function and arity, with the `erlang:` prefix omitted.

```erlang
% safe-ignore list_to_atom/1
list_to_atom("ejabberd_auth_" ++ atom_to_list(Method)).

% safe-ignore ets:first/1
compute_unique(ets:first(?TABLE), 0).
```

60 of the 71 findings carry one. The bar for suppressing a finding is that no unprivileged party can
reach it: the config-derived, literal-derived, query-name and admin-API groups all require either an
operator who edits `mongooseim.toml` or an authenticated administrator, and the man-in-the-middle,
injection and race-condition findings are wrong about the code.

The other eleven keep reporting:

- The **two real defects** above. Suppressing an open defect would hide it.
- The **eight datastore deserialisation** findings. That group is an accepted *trust assumption*,
  not a false positive: it holds only while the datastore is trusted. Silencing it would mean a
  deployment that breaks the assumption — an exposed Redis, say — produces no signal at all.
- **`gen_mod:get_module_proc/2`**. Config-bounded for almost every caller, but the two global
  distribution ones take the server from the Redis-backed mapping, so it inherits the same trust
  assumption rather than being purely config-derived.

Because those eleven remain, the scan still exits non-zero and the CI job keeps its `|| true`.

A suppression comment is tied to the line below it, so a refactor that moves a flagged call away
from its comment loses the suppression and the finding returns on the next scan. That is the
intended failure mode: it resurfaces for re-triage rather than staying silent.
