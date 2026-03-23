-module(mod_blocklist_rdbms).

-behaviour(mod_blocklist_backend).

%% mod_blocklist_backend callbacks
-export([init/2,
         get_block/3,
         upsert_block/4,
         remove_block/3,
         remove_domain/2,
         count_blocked_users/2,
         list_blocked_users/3,
         clear_all/1]).

%% mod_blocklist_backend

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    mongoose_rdbms:prepare(blocklist_select, blocklist, [luser, lserver],
                          <<"SELECT reason FROM blocklist WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_remove, blocklist, [luser, lserver],
                          <<"DELETE FROM blocklist WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_remove_domain, blocklist, [lserver],
                          <<"DELETE FROM blocklist WHERE lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_count_users, blocklist, [lserver],
                          <<"SELECT COUNT(*) FROM blocklist WHERE lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_list_users, blocklist, [lserver],
                          <<"SELECT luser, reason FROM blocklist WHERE lserver = ? ORDER BY luser">>),
    LimitOffset = rdbms_queries:limit_offset(),
    mongoose_rdbms:prepare(blocklist_list_users_range, blocklist, [lserver, limit, offset],
                          <<"SELECT luser, reason FROM blocklist WHERE lserver = ? ORDER BY luser ",
                            LimitOffset/binary>>),
    rdbms_queries:prepare_upsert(HostType, blocklist_upsert, blocklist,
                                 [<<"luser">>, <<"lserver">>, <<"reason">>],
                                 [<<"reason">>],
                                 [<<"luser">>, <<"lserver">>]),
    mongoose_rdbms:prepare(blocklist_clear_all, blocklist, [],
                          <<"DELETE FROM blocklist">>),
    ok.

-spec get_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> {ok, mod_blocklist:reason()} | not_found.
get_block(HostType, LUser, LServer) ->
    case mongoose_rdbms:execute_successfully(HostType, blocklist_select, [LUser, LServer]) of
        {selected, []} -> not_found;
        {selected, [{Reason}]} -> {ok, Reason}
    end.

-spec upsert_block(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_blocklist:reason()) -> ok.
upsert_block(HostType, LUser, LServer, Reason) ->
    Reason1 = encode_reason(Reason),
    {updated, _} = rdbms_queries:execute_upsert(HostType, blocklist_upsert,
                                                [LUser, LServer, Reason1],
                                                [Reason1]),
    ok.

-spec remove_block(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
remove_block(HostType, LUser, LServer) ->
    {updated, Result} = mongoose_rdbms:execute_successfully(HostType, blocklist_remove, [LUser, LServer]),
    Result > 0.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, Domain) ->
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, blocklist_remove_domain, [Domain]),
    ok.

-spec count_blocked_users(mongooseim:host_type(), jid:lserver()) -> non_neg_integer().
count_blocked_users(HostType, Domain) ->
    mongoose_rdbms:selected_to_integer(mongoose_rdbms:execute_successfully(HostType, blocklist_count_users, [Domain])).

-spec list_blocked_users(mongooseim:host_type(), jid:lserver(), mod_blocklist_backend:list_opts()) ->
    [{jid:luser(), mod_blocklist:reason()}].
list_blocked_users(HostType, Domain, Opts) ->
    {selected, Rows} = select_list_blocked_users(HostType, Domain, Opts),
    [{LUser, decode_reason(Reason)} || {LUser, Reason} <- Rows].

select_list_blocked_users(HostType, Domain, #{limit := Limit, offset := Offset}) ->
    mongoose_rdbms:execute_successfully(HostType, blocklist_list_users_range, [Domain, Limit, Offset]);
select_list_blocked_users(HostType, Domain, #{limit := Limit}) ->
    mongoose_rdbms:execute_successfully(HostType, blocklist_list_users_range, [Domain, Limit, 0]);
select_list_blocked_users(HostType, Domain, _Opts) ->
    mongoose_rdbms:execute_successfully(HostType, blocklist_list_users, [Domain]).

-spec clear_all(mongooseim:host_type()) -> ok.
clear_all(HostType) ->
    {updated, _} = mongoose_rdbms:execute_successfully(HostType, blocklist_clear_all, []),
    ok.

%% Helpers

encode_reason(undefined) -> null;
encode_reason(Reason) -> Reason.

decode_reason(null) -> undefined;
decode_reason(Reason) -> Reason.
