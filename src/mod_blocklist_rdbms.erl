-module(mod_blocklist_rdbms).

-behaviour(mod_blocklist_backend).

%% mod_blocklist_backend callbacks
-export([init/2, get_block/3, upsert_block/4, remove_block/3, remove_domain/2]).

%% mod_blocklist_backend

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    mongoose_rdbms:prepare(blocklist_select, blocklist, [luser, lserver],
                          <<"SELECT reason FROM blocklist WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_remove, blocklist, [luser, lserver],
                          <<"DELETE FROM blocklist WHERE luser = ? AND lserver = ?">>),
    mongoose_rdbms:prepare(blocklist_remove_domain, blocklist, [lserver],
                          <<"DELETE FROM blocklist WHERE lserver = ?">>),
    rdbms_queries:prepare_upsert(HostType, blocklist_upsert, blocklist,
                                 [<<"luser">>, <<"lserver">>, <<"reason">>],
                                 [<<"reason">>],
                                 [<<"luser">>, <<"lserver">>]),
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

%% Helpers

encode_reason(undefined) -> null;
encode_reason(Reason) -> Reason.
