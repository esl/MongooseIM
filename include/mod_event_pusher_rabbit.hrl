%%%===================================================================
%%% Metrics
%%%===================================================================

-define(BACKEND, mod_event_pusher_rabbit).
-define(RABBIT_CONNECTIONS_METRIC,
        [backends, ?BACKEND, rabbit_connections]).
-define(MESSAGES_PUBLISHED_METRIC, [backends, ?BACKEND, messages_published]).
-define(MESSAGES_FAILED_METRIC, [backends, ?BACKEND, messages_failed]).
-define(MESSAGES_TIMEOUT_METRIC, [backends, ?BACKEND, messages_timeout]).
-define(MESSAGE_PUBLISH_TIME_METRIC,
        [backends, ?BACKEND, message_publish_time]).
-define(MESSAGE_PAYLOAD_SIZE_METRIC,
        [backends, ?BACKEND, message_payload_size]).
