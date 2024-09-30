-module(mod_fast_rdbms).
-behaviour(mod_fast_backend).
-include("mongoose_logger.hrl").

-export([init/2,
         store_new_token/6,
         read_tokens/4,
         remove_user/3,
         remove_domain/2]).

-import(mongoose_rdbms, [prepare/4, execute/3, execute_successfully/3]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, _Opts) ->
    Key = [<<"server">>, <<"username">>, <<"user_agent_id">>],
    Upd = [<<"new_token">>, <<"new_expire">>, <<"new_count">>],
    Ins = Key ++ Upd,
    rdbms_queries:prepare_upsert(HostType, fast_upsert, fast_tokens, Ins, Upd, Key),
    prepare(fast_select, fast_tokens,
            [current_token, current_expire, current_count,
             new_token, new_expire, new_count],
            <<"SELECT "
                  "current_token, current_expire, current_count, "
                  "new_token, new_expire, new_count "
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

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_token:agent_id(),
        ExpireTS :: mod_token:seconds(),
        Token :: mod_token:token().
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token) ->
    Key = [LServer, LUser, AgentId],
    Upd = [Token, ExpireTS, 0],
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
        {selected, [{CurrentToken, CurrentExpire, CurrentCount,
                     NewToken, NewExpire, NewCount}]} ->
            Data = #{
                now_timestamp => mod_fast:utc_now_as_seconds(),
                current_token => CurrentToken,
                current_expire => maybe_to_integer(CurrentExpire),
                current_count => maybe_to_integer(CurrentCount),
                new_token => null_as_undefined(NewToken),
                new_expire => null_as_undefined(NewExpire),
                new_count => null_as_undefined(NewCount)
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
