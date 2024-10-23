-module(mod_fast_rdbms).
-behaviour(mod_fast_backend).
-include("mongoose_logger.hrl").

-export([init/2,
         store_new_token/7,
         read_tokens/4,
         remove_user/3,
         remove_domain/2]).

-import(mongoose_rdbms, [prepare/4, execute/3, execute_successfully/3]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    Key = [<<"server">>, <<"username">>, <<"user_agent_id">>],
    Upd = [<<"new_token">>, <<"new_expire">>, <<"new_count">>, <<"new_mech_id">>],
    Ins = Key ++ Upd,
    rdbms_queries:prepare_upsert(HostType, fast_upsert, fast_tokens, Ins, Upd, Key),
    prepare(fast_select, fast_tokens,
            [current_token, current_expire, current_count, current_mech_id,
             new_token, new_expire, new_count, new_mech_id],
            <<"SELECT "
                  "current_token, current_expire, current_count, current_mech_id, "
                  "new_token, new_expire, new_count, new_mech_id "
              "FROM fast_tokens "
              "WHERE server = ? AND username = ? AND user_agent_id = ?">>),
    prepare(fast_remove_user, fast_tokens,
            [server, username],
            <<"DELETE FROM fast_tokens "
              "WHERE server = ? AND username = ?">>),
    prepare(fast_remove_domain, fast_tokens,
            [server],
            <<"DELETE FROM fast_tokens WHERE server = ?">>),
    ok.

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_token:agent_id(),
        ExpireTS :: mod_token:seconds(),
        Token :: mod_token:token(),
        Mech :: mod_token:mechanism().
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) ->
    Key = [LServer, LUser, AgentId],
    Upd = [Token, ExpireTS, 0, mech_id(Mech)],
    Ins = Key ++ Upd,
    rdbms_queries:execute_upsert(HostType, fast_upsert, Ins, Upd, Key),
    ok.

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
       {ok, mod_fast:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast:agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    case execute(HostType, fast_select, [LServer, LUser, AgentId]) of
        {selected, []} ->
            {error, not_found};
        {selected, [{CurrentToken, CurrentExpire, CurrentCount, CurrentMechId,
                     NewToken, NewExpire, NewCount, NewMechId}]} ->
            Data = #{
                now_timestamp => mod_fast:utc_now_as_seconds(),
                current_token => null_as_undefined(CurrentToken),
                current_expire => maybe_to_integer(CurrentExpire),
                current_count => maybe_to_integer(CurrentCount),
                current_mech => maybe_to_mech(CurrentMechId),
                new_token => null_as_undefined(NewToken),
                new_expire => maybe_to_integer(NewExpire),
                new_count => maybe_to_integer(NewCount),
                new_mech => maybe_to_mech(NewMechId)
            },
            {ok, Data};
        Other ->
            ?LOG_ERROR(#{what => fast_token_read_failed,
                         username => LUser, server => LServer, result => Other}),
            {error, not_found}
    end.

maybe_to_integer(null) ->
    undefined;
maybe_to_integer(Result) ->
    mongoose_rdbms:result_to_integer(Result).

maybe_to_mech(null) ->
    undefined;
maybe_to_mech(Result) ->
    Int = mongoose_rdbms:result_to_integer(Result),
    mech_name(Int).

null_as_undefined(null) ->
    undefined;
null_as_undefined(Result) ->
    Result.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    execute_successfully(HostType, fast_remove_user, [LServer, LUser]),
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    execute_successfully(HostType, fast_remove_domain, [LServer]),
    ok.

mech_id(<<"HT-SHA-256-NONE">>) -> 1.

mech_name(1) -> <<"HT-SHA-256-NONE">>;
mech_name(_) -> <<"UNKNOWN-MECH">>. %% Just in case DB has an unknown mech_id
