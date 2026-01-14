# Message Broadcast (mod_broadcast) — Implementation Plan & Progress

Status: in progress (branch: `feature/mod_broadcast`)
Last updated: 2026-01-14

This document tracks the phased implementation of the Message Broadcast feature.

## Goals (MVP)
- Admins (global + domain) can create and manage broadcast jobs via GraphQL.
- Broadcast delivers `chat` messages to bare JIDs with `<subject>` + `<body>`.
- Broadcast progress is durable (offset updated after every routed message).
- Jobs resume automatically after node restart (best-effort: resumes jobs owned by this node).
- Multiple broadcasts can run concurrently.
- `getBroadcasts` supports pagination.

## Phase 0 — Design Alignment
- [x] Confirm GraphQL pagination style for `getBroadcasts` (offset-based `limit`/`index`)
- [ ] Decide recipient file upload flow (GraphQL multipart upload vs REST vs server-side file)
- [ ] Decide rate limiting scope (per-job vs per-node vs cluster-wide)

## Phase 1 — Data Model + RDBMS Layer
- [x] Add SQL tables for broadcasts + recipients (PostgreSQL + MySQL)
- [x] Implement `mod_broadcast_rdbms` prepared statements
- [x] Implement minimal `mod_broadcast_api` (create/get/list/abort/delete)

## Phase 2 — Worker / Job Execution
- [x] Implement job manager + per-job runner (`mod_broadcast_manager`, `mod_broadcast_runner`)
- [x] Implement message stanza building and routing
- [x] Implement strict progress tracking (persist after each recipient)
- [x] Implement resume/claim logic on startup (best-effort: resumes jobs owned by this node)

## Phase 3 — GraphQL Admin API
- [x] Add GraphQL schema fragments (admin + global)
- [x] Add GraphQL mappings and root category wiring
- [x] Implement resolvers for queries + mutations
- [x] Implement paginated `getBroadcasts` (offset-based `limit`/`index`)

## Phase 4 — Tests & Verification
- [ ] Add unit-level tests for RDBMS layer (where reasonable)
- [x] Add big_tests GraphQL suite for broadcast flow
- [x] Verify `./rebar3 compile`

## Draft GraphQL API

### Query
- `getBroadcasts(domain: DomainName, limit: PosInt, index: NonNegInt): BroadcastsPayload`
- `getBroadcast(id: PosInt!): Broadcast`

### Mutations
- `startBroadcast(domain: DomainName!, name: NonEmptyString!, senderJid: BareJID!, subject: String, body: String!, ratePerSecond: PosInt!, recipients: BroadcastRecipientsInput!): Broadcast`
- `abortBroadcast(id: PosInt!): Broadcast`
- `deleteBroadcasts(ids: [PosInt!]): DeleteBroadcastsPayload`

## Notes / Risks
- Strict “persist after each routed message” can be expensive; MVP keeps the strict guarantee.
- Current resume logic is “same-node ownership” only.
