# Plan: Admin GraphQL / CLI inventory (host types, enabled modules, backend types)

This document describes a minimal, practical plan to fetch a **server inventory** from a running MongooseIM node:

- host types configured on the server
- enabled (configured) modules per host type
- backend type(s) used by those modules

The plan includes both:

1. **GraphQL admin API** approach (HTTP endpoint)
2. **CLI approach** (shell commands that query the admin endpoint)

> Scope note: this is a plan document. If a field/query is not currently exposed in the admin schema, the plan proposes adding a small, dedicated GraphQL query for it.

## 0. Status / progress tracking

- Last updated: 2025-12-31
- Current status: implemented (pending review/merge)
- Implementation checklist: see section 4.6

---

## 1. Current state analysis

### Existing relevant GraphQL queries

The admin schema (`priv/graphql/schemas/admin/admin_schema.gql`) already exposes:

| Namespace | Query | What it returns |
|-----------|-------|-----------------|
| `domain` | `domainsByHostType(hostType: String!)` | List of domain names for a given host type |
| `domain` | `allDomains` | All domains with details (domain, host_type, status) |
| `server` | `status` | Server status, version, commit hash |

**Missing**: There is no query to list host types themselves, nor modules/backends per host type.

### Existing Erlang functions (data sources)

| What | Function | Returns |
|------|----------|---------|
| All host types | `mongoose_config:get_opt(hosts) ++ mongoose_config:get_opt(host_types)` or `?ALL_HOST_TYPES` macro | `[binary()]` |
| Modules per host type | `gen_mod:loaded_modules(HostType)` | `[module()]` |
| Modules with opts | `gen_mod:loaded_modules_with_opts(HostType)` | `#{module() => module_opts()}` |
| All modules across all host types | `gen_mod:loaded_modules_with_opts()` | `#{host_type() => #{module() => opts}}` |
| Domains by host type | `mongoose_domain_api:get_domains_by_host_type(HostType)` | `[binary()]` |
| Backend name (runtime) | `mongoose_backend:get_backend_name(HostType, MainModule)` | `atom()` (may crash if not initialized) |

---

## 2. Definitions

- **Host type**: an entry from `?ALL_HOST_TYPES` (see `include/mongoose.hrl:26`). It is derived from configuration:
  - `mongoose_config:get_opt(hosts)` — static hosts
  - `mongoose_config:get_opt(host_types)` — host types for dynamic domains
- **Enabled module**: a module present in `mongoose_config` for a given host type.
  - Checked via `gen_mod:is_loaded(HostType, Module)` (config presence).
- **Backend type**: the configured backend (e.g. `mnesia`, `rdbms`, `cets`, …) used by a module that follows the `mongoose_backend` pattern.

---

## 3. Recommended output shape

Return a compact JSON-friendly structure:

```json
{
  "hostTypes": [
    {
      "name": "localhost",
      "domains": ["localhost", "example.com"],
      "modules": [
        {
          "name": "mod_roster",
          "backend": {
            "configured": "rdbms",
            "runtime": "rdbms"
          }
        },
        {
          "name": "mod_mam",
          "backend": {
            "configured": "rdbms",
            "runtime": "rdbms"
          }
        },
        {
          "name": "mod_ping",
          "backend": null
        }
      ]
    }
  ]
}
```

**Fields**:
- `hostTypes[].name` — the host type binary
- `hostTypes[].domains[]` — domains mapped to this host type (optional, can be expensive for dynamic domains)
- `hostTypes[].modules[].name` — module atom as string
- `hostTypes[].modules[].backend.configured` — value of `backend` key in module opts (if present)
- `hostTypes[].modules[].backend.runtime` — value from `mongoose_backend:get_backend_name/2` (if initialized)

---

## 4. Implementation plan (Option A — preferred)

### 4.1 GraphQL API surface (align with existing admin schema)

The admin root query is `AdminQuery` (see `priv/graphql/schemas/admin/admin_schema.gql`) and already exposes `server: ServerAdminQuery`.

Add one new field under `ServerAdminQuery`:

- `server.hostTypes: [HostTypeInfo!]!` (global-admin only)

Minimal query example:

```graphql
query HostTypes {
  server {
    hostTypes {
      name
      domains
      modules {
        name
        backend {
          configured
          runtime
        }
      }
    }
  }
}
```

### 4.2 Schema changes

**File to edit**: `priv/graphql/schemas/admin/server.gql`

Add types (names are bikeshed-able; the important part is the shape and nullability):

```graphql
"""Backend information for a module (best-effort)."""
type BackendInfo {
  "Backend configured in module opts (may be null if not specified)"
  configured: String
  "Backend resolved at runtime by mongoose_backend (may be null if not initialized)"
  runtime: String
}

"""Information about a loaded module for a given host type."""
type ModuleInfo {
  "Module name (atom as string, e.g. 'mod_roster')"
  name: String!
  "Backend information when available"
  backend: BackendInfo
}

"""Information about a host type."""
type HostTypeInfo {
  "Host type name"
  name: String!
  "Domains mapped to this host type (may be empty)"
  domains: [String!]!
  "Loaded modules for this host type"
  modules: [ModuleInfo!]!
}

"""(Optional wrapper) If later you need more server-level fields, introduce a wrapper type."""
type ServerInventory {
  hostTypes: [HostTypeInfo!]!
}
```

Then extend `ServerAdminQuery`:

```graphql
extend type ServerAdminQuery {
  "List configured host types with their modules and backends"
  hostTypes: [HostTypeInfo!]!
    @protected(type: GLOBAL)
}
```

### 4.3 Resolver implementation

**File to edit**: `src/graphql/admin/mongoose_graphql_server_admin_query.erl`

This module already resolves fields under the `server` namespace (e.g. `server.status`). Add resolver clauses to return a nested object tree.

Key idea: use small tagged tuples as internal “object handles” and map them to GraphQL types.

Suggested execution clauses (illustrative; adjust to local style):

```erlang
%% NOTE: to use ?ALL_HOST_TYPES add:
%%   -include("mongoose.hrl").

%% server.hostTypes
execute(_Ctx, server, <<"hostTypes">>, _) ->
    HostTypes = lists:sort(?ALL_HOST_TYPES),
    {ok, [{host_type_info, HT} || HT <- HostTypes]};

%% hostTypeInfo.name
execute(_Ctx, {host_type_info, HostType}, <<"name">>, _) ->
    {ok, HostType};

%% hostTypeInfo.domains
execute(_Ctx, {host_type_info, HostType}, <<"domains">>, _) ->
    %% returns [binary()], which GraphQL String can represent
    {ok, mongoose_domain_api:get_domains_by_host_type(HostType)};

%% hostTypeInfo.modules
execute(_Ctx, {host_type_info, HostType}, <<"modules">>, _) ->
    ModulesWithOpts = gen_mod:loaded_modules_with_opts(HostType),
  Modules = lists:sort(maps:to_list(ModulesWithOpts)),
  {ok, [{module_info, HostType, Mod, Opts} || {Mod, Opts} <- Modules]};

%% moduleInfo.name
execute(_Ctx, {module_info, _HostType, Module, _Opts}, <<"name">>, _) ->
    {ok, atom_to_binary(Module, utf8)};

%% moduleInfo.backend
execute(_Ctx, {module_info, HostType, Module, Opts}, <<"backend">>, _) ->
    {ok, {backend_info, HostType, Module, Opts}};

%% backendInfo.configured
execute(_Ctx, {backend_info, _HostType, _Module, Opts}, <<"configured">>, _) ->
    case maps:get(backend, Opts, undefined) of
        undefined -> {ok, null};
        Backend -> {ok, atom_to_binary(Backend, utf8)}
    end;

%% backendInfo.runtime
execute(_Ctx, {backend_info, HostType, Module, _Opts}, <<"runtime">>, _) ->
    %% mongoose_backend stores backend name in persistent_term during init/4.
    %% If not initialized, persistent_term:get/1 throws badarg.
    try mongoose_backend:get_backend_name(HostType, Module) of
        Backend -> {ok, atom_to_binary(Backend, utf8)}
    catch
        error:badarg -> {ok, null}
    end.
```

Notes:

- `?ALL_HOST_TYPES` is the simplest way to list host types and is already used in the codebase.
- `gen_mod:loaded_modules_with_opts/1` gives both module names and module options.
- Runtime backend only exists if the module participates in `mongoose_backend:init/4`.

### 4.4 Data sources and behavior (more explicit)

| Data | Source | Output type | Failure mode | Handling |
|------|--------|-------------|--------------|----------|
| Host types | `?ALL_HOST_TYPES` | `[binary()]` | none | return as-is |
| Domains per host type | `mongoose_domain_api:get_domains_by_host_type/1` | `[binary()]` | none / empty list | return empty list |
| Modules with opts | `gen_mod:loaded_modules_with_opts/1` | `#{module() => map()}` | none | `maps:to_list/1` |
| Configured backend | `maps:get(backend, Opts, undefined)` | `atom()` or `undefined` | missing key | return `null` |
| Runtime backend | `mongoose_backend:get_backend_name/2` | `atom()` | `badarg` if not in `persistent_term` | return `null` |

### 4.5 Permissions

Use `@protected(type: GLOBAL)` on the `hostTypes` field so only global admins can access it.

If later you want domain-admin support, consider a separate field under the `domain` namespace (e.g. `domain.hostTypes` scoped to allowed domains/host types) to keep authorization simple and explicit.

### 4.6 Progress plan (milestones / checklist)

This is intended to be small and reviewable as a single PR, but can be split if preferred.

**Estimated scope**: small (schema + one resolver module + tests). Main risk is that runtime backend info is not available for every module (expected; should return `null`).

**Suggested PR strategy**

- **Single PR (preferred)**: schema + resolver + tests + docs example.
- **Two PRs (if you want smaller reviews)**:
  1) schema + resolver + docs example
  2) tests

**Rough time estimate** (for someone familiar with the GraphQL plumbing):

- Schema + resolver: ~1–2h
- Tests + local validation: ~1–2h

**Milestone 1 — Schema (types + field)**

- [ ] Update `priv/graphql/schemas/admin/server.gql`:
  - [ ] Add `BackendInfo`, `ModuleInfo`, `HostTypeInfo`
  - [ ] Add `hostTypes: [HostTypeInfo!]!` to `ServerAdminQuery` with `@protected(type: GLOBAL)`

**Milestone 2 — Resolver wiring**

- [ ] Update `src/graphql/admin/mongoose_graphql_server_admin_query.erl`:
  - [ ] Add `execute/4` clause for `server.hostTypes`
  - [ ] Add field resolvers for `HostTypeInfo` / `ModuleInfo` / `BackendInfo`
  - [ ] Ensure stable ordering (`lists:sort/1`) for deterministic output
  - [ ] Return `null` for missing backend info and handle `badarg` from `mongoose_backend:get_backend_name/2`

**Milestone 3 — Documentation example**

- [ ] Add/update an example query in `doc/graphql-api/Admin-GraphQL.md` (or keep this plan doc as the reference if preferred)

**Milestone 4 — Tests**

- [ ] Extend `test/mongoose_graphql_SUITE.erl`:
  - [ ] Schema presence test for `ServerAdminQuery.hostTypes`
  - [ ] Execution smoke test that asserts response shape only

**Milestone 5 — Manual validation**

- [ ] Run the curl command from section 5 against a dev node
- [ ] Sanity-check modules/backends for at least one host type

**Definition of done**

- [ ] All new fields are documented and protected (global admin)
- [ ] Tests cover schema + basic execution
- [ ] Output shape matches section 3

## Option B: Use existing queries first (no code changes)

If you’re unsure what is already exposed, you can discover available queries via GraphQL introspection.

Example: list top-level query field names:

```graphql
query AvailableQueries {
  __schema {
    queryType {
      fields {
        name
      }
    }
  }
}
```

Then check whether something like `server`, `metric`, `stat`, `domain`, etc. is already available.

In practice, for this specific data, the relevant existing entry points are `server` and `domain`.

In MongooseIM’s admin schema, the most relevant entry points for this feature are:

- `server` (already exists; recommended place to add `hostTypes`)
- `domain` (already exists; can be used as an alternative source for domains/host types)

## 5. CLI: Run the admin query from shell

MongooseIM’s admin GraphQL endpoint supports HTTP (see `doc/graphql-api/Admin-GraphQL.md`). Use `curl` as a simple “CLI”.

### 1) Set env vars

```sh
export MIM_GQL_URL="http://localhost:5551/api/graphql"   # adjust to your listener
export MIM_GQL_USER="alice"                             # global admin login from config
export MIM_GQL_PASS="secret"                            # global admin password from config
```

### 2) Execute the host types query

```sh
curl -sS \
  -u "$MIM_GQL_USER:$MIM_GQL_PASS" \
  -H 'Content-Type: application/json' \
  --data-binary '{"query":"query HostTypes { server { hostTypes { name domains modules { name backend { configured runtime } } } } }"}' \
  "$MIM_GQL_URL"
```

### 3) Pretty-print / extract lists (optional)

With `jq`:

```sh
curl -sS -u "$MIM_GQL_USER:$MIM_GQL_PASS" -H 'Content-Type: application/json' \
  --data-binary '{"query":"query HostTypes { server { hostTypes { name modules { name backend { configured runtime } } } } }"}' \
  "$MIM_GQL_URL" \
| jq '.data.server.hostTypes[] | {hostType: .name, modules: [.modules[].name]}'
```

## 6. Acceptance criteria

- Returns all configured host types (`?ALL_HOST_TYPES`) from the running node.
- For each host type, returns at least the list of configured modules.
- For each module, returns a backend type if available:
  - `configured` backend from module options when present
  - `runtime` backend from `mongoose_backend` when initialized

## 7. Testing and validation

### 7.1 Automated tests

The codebase already has GraphQL tests in `test/mongoose_graphql_SUITE.erl`. Recommended additions:

1. **Schema presence** (fast): verify the `ServerAdminQuery` type exposes `hostTypes` on the admin endpoint.
   - Create/init admin endpoint (existing helper patterns in the suite)
   - Fetch `graphql_schema:get(AdminEp, <<"ServerAdminQuery">>)`
  - Assert it contains a field `hostTypes` with list type `[HostTypeInfo!]!`

2. **Execution smoke test** (best-effort): execute a minimal query and assert the shape.
   - Input document:

     ```graphql
     query HostTypes { server { hostTypes { name modules { name } } } }
     ```

   - Assert response contains `data.server.hostTypes` as a list.
   - Do not assert exact host types/modules in a generic test environment; only assert types and required keys.

If the test environment does not have a fully configured domain map, keep assertions tolerant (e.g. `domains` can be empty).

### 7.2 Manual validation

1. Start a node with a known config (at least 1 host type, at least 1 module with `backend` configured).
2. Run the curl query from section 5.
3. Confirm:
   - `hostTypes[].name` lists expected host types
   - `modules[]` contains expected modules for each host type
   - `backend.configured` matches module opts when present
   - `backend.runtime` is either equal to configured backend or `null` (when not initialized)

## 8. Operational considerations

- **Do not trigger initialization**: this query should only read current state (config + runtime), not start modules/backends.
- **Runtime backend is optional**: returning `null` is expected when a module doesn’t use `mongoose_backend` or hasn’t initialized it yet.
- **Stability**: consider sorting host types and module names to keep output stable for tooling (`lists:sort/1` on binaries, atoms).

## 9. Follow-ups (optional)

- Add filter args to reduce payload on large installations:
  - `hostTypes(hostType: String): [HostTypeInfo!]!` (server-side filter)
  - or `hostTypesByName(hostType: String!): HostTypeInfo` (single host type)
- Add a flag to skip domain resolution if it’s expensive in some deployments:
  - `hostTypes(includeDomains: Boolean = true): [HostTypeInfo!]!`
- Add a separate domain-admin entry point later if needed (explicitly authorized and scoped).

