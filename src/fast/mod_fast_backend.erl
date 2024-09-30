-module(mod_fast_backend).

-export([init/2,
         store_new_token/7,
         read_tokens/4,
         remove_user/3,
         remove_domain/2]).

-define(MAIN_MODULE, mod_fast).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_token:agent_id(),
        ExpireTS :: mod_token:seconds(),
        Token :: mod_token:token(),
        Mech :: mod_token:mechanism().

-callback read_tokens(HostType, LServer, LUser, AgentId) ->
      {ok, mod_fast:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast:agent_id().

-callback remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.

-callback remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.

-optional_callbacks([remove_domain/2]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    Tracked = [store_new_token, read_tokens],
    mongoose_backend:init(HostType, ?MAIN_MODULE, Tracked, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) -> ok
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_token:agent_id(),
        ExpireTS :: mod_token:seconds(),
        Token :: mod_token:token(),
        Mech :: mod_token:mechanism().
store_new_token(HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech) ->
    Args = [HostType, LServer, LUser, AgentId, ExpireTS, Token, Mech],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec read_tokens(HostType, LServer, LUser, AgentId) ->
      {ok, mod_fast:tokens_data()} | {error, not_found}
   when HostType :: mongooseim:host_type(),
        LServer :: jid:lserver(),
        LUser :: jid:luser(),
        AgentId :: mod_fast:agent_id().
read_tokens(HostType, LServer, LUser, AgentId) ->
    Args = [HostType, LServer, LUser, AgentId],
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
