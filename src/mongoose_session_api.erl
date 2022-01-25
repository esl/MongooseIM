%% @doc Provide an interface for frontends (like graphql or ctl) to manage sessions.
-module(mongoose_session_api).

-export([prepare_reason/1,
         kick_session/2,
         kick_session/4,
         kick_sessions/2]).

-spec kick_sessions(jid:jid(), binary()) -> [ok].
kick_sessions(JID, Reason) ->
    lists:map(
        fun(Resource) ->
                service_admin_extra_sessions:kick_session(
                  jid:replace_resource(JID, Resource), Reason)
        end,
        ejabberd_sm:get_user_resources(JID)).

-spec kick_session(jid:user(), jid:server(), jid:resource(), list() | binary()) -> ok.
kick_session(User, Server, Resource, ReasonText) ->
    kick_session(jid:make(User, Server, Resource), prepare_reason(ReasonText)).

-spec kick_session(jid:jid(), binary() | list()) -> ok.
kick_session(JID, ReasonText) ->
    ejabberd_c2s:terminate_session(JID, prepare_reason(ReasonText)),
    ok.

-spec prepare_reason(binary() | string()) -> binary().
prepare_reason(<<>>) ->
    <<"Kicked by administrator">>;
prepare_reason([Reason]) ->
    prepare_reason(Reason);
prepare_reason(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
prepare_reason(Reason) when is_binary(Reason) ->
    Reason.


