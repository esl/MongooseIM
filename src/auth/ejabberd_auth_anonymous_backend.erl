-module(ejabberd_auth_anonymous_backend).

-export([init/1, stop/1, does_anonymous_user_exist/2, remove_connection/3, add_connection/3]).

-define(MAIN_MODULE, ejabberd_auth_anonymous).

-callback init(mongooseim:host_type()) -> ok.

-callback stop(mongooseim:host_type()) -> ok.

-callback does_anonymous_user_exist(mongooseim:host_type(), jid:simple_bare_jid()) -> boolean().

-callback remove_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.

-callback add_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    TrackedFuns = [does_anonymous_user_exist],
    Backend = mongoose_config:get_opt([{auth, HostType}, anonymous, backend]),
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, #{backend => Backend}),
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, [HostType]).

-spec does_anonymous_user_exist(mongooseim:host_type(), jid:simple_bare_jid()) -> boolean().
does_anonymous_user_exist(HostType, US) ->
    Args = [HostType, US],
    mongoose_backend:call_tracked(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec add_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
add_connection(HostType, SID, US) ->
    Args = [HostType, SID, US],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
remove_connection(HostType, SID, US) ->
    Args = [HostType, SID, US],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
