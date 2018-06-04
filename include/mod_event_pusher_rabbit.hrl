%%%===================================================================
%%% Metrics
%%%===================================================================

-define(METRIC_MODULE, mod_event_pusher_rabbit).
-define(RABBIT_CONNECTIONS_METRIC,
        [backends, ?METRIC_MODULE, rabbit_connections]).
-define(MESSAGES_PUBLISH_METRIC, [backends, ?METRIC_MODULE, messages_publish]).
-define(MESSAGES_FAILED_METRIC, [backends, ?METRIC_MODULE, messages_failed]).
-define(MESSAGES_TIMEOUT_METRIC, [backends, ?METRIC_MODULE, messages_timeout]).
-define(MESSAGE_PUBLISH_TIME_METRIC,
        [backends, ?METRIC_MODULE, message_publish_time]).
-define(MESSAGE_PAYLOAD_SIZE_METRIC,
        [backends, ?METRIC_MODULE, message_payload_size]).
