-module(mod_fast_auth_token_backend).

-export([init/2,
         store_new_token/8,
         read_tokens/4,
         invalidate_token/4,
         set_count/6,
         set_current/6,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_fast_auth_token).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback store_new_token(HostType, LServer, LUser, AgentId, ExpireTS,
                          Token, Mech, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        ExpireTS :: mod_fast_auth_token:seconds(),
        Token :: mod_fast_auth_token:token(),
        Mech :: mod_fast_auth_token:mechanism(),
        SetCurrent :: mod_fast_auth_token:set_current() | false.

-callback read_tokens(HostType, LServer, LUser, AgentId) ->
      {ok, mod_fast_auth_token:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().

-callback invalidate_token(HostType, LServer, LUser, AgentId) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().

-callback set_count(HostType, LServer, LUser, AgentId,
                    NewCurrentCount, CurrentToken) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter(),
        CurrentToken :: mod_fast_auth_token:token().

-callback set_current(HostType, LServer, LUser, AgentId,
                      NewCurrentCount, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter() | undefined,
        SetCurrent :: mod_fast_auth_token:set_current().

-callback remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.

-optional_callbacks([remove_domain/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    Tracked = [store_new_token, read_tokens, invalidate_token,
               set_count, set_current],
    mongoose_backend:init(HostType, ?MAIN_MODULE, Tracked, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS,
                      Token, Mech, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        ExpireTS :: mod_fast_auth_token:seconds(),
        Token :: mod_fast_auth_token:token(),
        Mech :: mod_fast_auth_token:mechanism(),
        SetCurrent :: mod_fast_auth_token:set_current() | false.
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent) ->
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech, SetCurrent],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
      {ok, mod_fast_auth_token:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    Args = [HostType, LServer, LUser, AgentId],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec invalidate_token(HostType, LServer, LUser, AgentId) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id().
invalidate_token(HostType, LServer, LUser, AgentId) ->
    Args = [HostType, LServer, LUser, AgentId],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_count(HostType, LServer, LUser, AgentId,
                    NewCurrentCount, CurrentToken) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter(),
        CurrentToken :: mod_fast_auth_token:token().
set_count(HostType, LServer, LUser, AgentId, NewCurrentCount, CurrentToken) ->
    Args = [HostType, LServer, LUser, AgentId, NewCurrentCount, CurrentToken],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec set_current(HostType, LServer, LUser, AgentId,
                      NewCurrentCount, SetCurrent) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast_auth_token:agent_id(),
        NewCurrentCount :: mod_fast_auth_token:counter() | undefined,
        SetCurrent :: mod_fast_auth_token:set_current().
set_current(HostType, LServer, LUser, AgentId, NewCurrentCount, SetCurrent) ->
    Args = [HostType, LServer, LUser, AgentId, NewCurrentCount, SetCurrent],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user(HostType, LUser, LServer) ->
    Args = [HostType, LUser, LServer],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(HostType, LServer) ->
    Args = [HostType, LServer],
    case mongoose_backend:is_exported(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, 2) of
        true ->
            mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args);
        false -> 
            ok
    end.
