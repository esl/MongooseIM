-module(mod_fast_auth_token_rdbms).
-behaviour(mod_fast_auth_token_backend).
-include("mongoose_logger.hrl").

-export([init/2,
         store_new_token/8,
         read_tokens/4,
         invalidate_token/4,
         set_current/6,
         set_count/6,
         remove_user/3,
         remove_domain/2]).

-import(mongoose_rdbms, [prepare/4, execute/3, execute_successfully/3]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    prepare_upsert(HostType),
    prepare_upsert_and_set_current(HostType),
    prepare(fast_select, fast_auth_token,
            [server, username, user_agent_id],
            <<"SELECT "
                  "current_token, current_expire, current_count, current_mech_id, "
                  "new_token, new_expire, new_count, new_mech_id "
              "FROM fast_auth_token "
              "WHERE server = ? AND username = ? AND user_agent_id = ?">>),
    prepare(fast_invalidate_token, fast_auth_token,
            [server, username, user_agent_id],
            <<"DELETE FROM fast_auth_token "
              "WHERE server = ? AND username = ? AND user_agent_id = ?">>),
    prepare(fast_remove_user, fast_auth_token,
            [server, username],
            <<"DELETE FROM fast_auth_token "
              "WHERE server = ? AND username = ?">>),
    prepare(fast_remove_domain, fast_auth_token,
            [server],
            <<"DELETE FROM fast_auth_token WHERE server = ?">>),
    prepare(fast_set_count, fast_auth_token,
            [current_count, server, username, user_agent_id, current_token],
            <<"UPDATE fast_auth_token SET current_count = ? "
              "WHERE server = ? AND username = ? AND user_agent_id = ? AND current_token = ?">>),
    ok.

prepare_upsert(HostType) ->
    Key = [<<"server">>, <<"username">>, <<"user_agent_id">>],
    Upd = [<<"new_token">>, <<"new_expire">>, <<"new_count">>, <<"new_mech_id">>],
    Ins = Key ++ Upd,
    rdbms_queries:prepare_upsert(HostType, fast_upsert, fast_auth_token, Ins, Upd, Key).

prepare_upsert_and_set_current(HostType) ->
    Key = [<<"server">>, <<"username">>, <<"user_agent_id">>],
    Upd = [<<"new_token">>, <<"new_expire">>, <<"new_count">>, <<"new_mech_id">>,
           <<"current_token">>, <<"current_expire">>, <<"current_count">>, <<"current_mech_id">>],
    Ins = Key ++ Upd,
    rdbms_queries:prepare_upsert(HostType, fast_upsert_and_set_current, fast_auth_token, Ins, Upd, Key).

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS,
                      Token, Mech, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        ExpireTS :: mod_fast_auth_token:seconds() | null,
        Token :: mod_fast_auth_token:token() | null,
        Mech :: mod_fast_auth_token:mechanism() | null,
        SetCurrent :: mod_fast_auth_token:set_current() | false.
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, false) ->
    Key = [LServer, LUser, AgentId],
    Upd = [Token, ExpireTS, 0, mech_id(Mech)],
    Ins = Key ++ Upd,
    rdbms_queries:execute_upsert(HostType, fast_upsert, Ins, Upd),
    ok;
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent) ->
    %% Move new_token into the current_token slot
    %% We pass data directly instead of checking from the database to avoid
    %% race conditions
    #{current_token := CurrentToken,
      current_expire := CurrentExpire,
      current_count := CurrentCount,
      current_mech := CurrentMechId} = SetCurrent,
    Key = [LServer, LUser, AgentId],
    Upd = [Token, ExpireTS, 0, mech_id(Mech),
           CurrentToken, CurrentExpire, CurrentCount, mech_id(CurrentMechId)],
    Ins = Key ++ Upd,
    rdbms_queries:execute_upsert(HostType, fast_upsert_and_set_current, Ins, Upd),
    ok.

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
       {ok, mod_fast_auth_token:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    case execute(HostType, fast_select, [LServer, LUser, AgentId]) of
        {selected, []} ->
            {error, not_found};
        {selected, [{CurrentToken, CurrentExpire, CurrentCount, CurrentMechId,
                     NewToken, NewExpire, NewCount, NewMechId}]} ->
            Data = #{
                now_timestamp => mod_fast_auth_token:utc_now_as_seconds(),
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

-spec invalidate_token(HostType, LServer, LUser, AgentId) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().
invalidate_token(HostType, LServer, LUser, AgentId) ->
    execute_successfully(HostType, fast_invalidate_token,
                         [LServer, LUser, AgentId]),
    ok.

-spec set_count(HostType, LServer, LUser, AgentId, NewCurrentCount, CurrentToken) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter(),
        CurrentToken :: mod_fast_auth_token:token().
set_count(HostType, LServer, LUser, AgentId, NewCurrentCount, CurrentToken) ->
    execute_successfully(HostType, fast_set_count,
                         [NewCurrentCount, LServer, LUser, AgentId, CurrentToken]),
    ok.

-spec set_current(HostType, LServer, LUser, AgentId, NewCurrentCount, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter() | undefined,
        SetCurrent :: mod_fast_auth_token:set_current().
set_current(HostType, LServer, LUser, AgentId, NewCurrentCount, SetCurrent) ->
    ExpireTS = null,
    Token = null,
    Mech = null,
    SetCurrent2 = maybe_set_count(NewCurrentCount, SetCurrent),
    store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent2),
    ok.

maybe_set_count(undefined, SetCurrent) ->
    SetCurrent;
maybe_set_count(Count, SetCurrent) when is_integer(Count) ->
    SetCurrent#{current_count := Count}.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    execute_successfully(HostType, fast_remove_domain, [LServer]),
    ok.

-spec mech_id(null | mod_fast_auth_token:mechanism()) -> null | non_neg_integer().
mech_id(null) ->
    null;
mech_id(Mech) ->
    mod_fast_auth_token_generic_mech:mech_id(Mech).

-spec mech_name(non_neg_integer()) -> mod_fast_auth_token:mechanism().
mech_name(MechId) ->
    mod_fast_auth_token_generic_mech:mech_name(MechId).
