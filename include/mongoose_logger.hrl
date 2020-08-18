-ifndef(MONGOOSEIM_LOGGER_HRL).
-define(MONGOOSEIM_LOGGER_HRL, true).

-include_lib("kernel/include/logger.hrl").

-define(LOG_IF(Level, Condition, Msg),
    (Condition) == true andalso ?LOG(Level, Msg)).

-define(LOG_IF(Level, Condition, Format, Args),
    (Condition) == true andalso ?LOG(Level, Format, Args)).

-define(DEBUG(Format, Args),
    ?LOG_DEBUG(Format, Args)).

-define(DEBUG_IF(Condition, Format, Args),
    ?LOG_IF(debug, Condition, Format, Args)).

-define(INFO_MSG(Format, Args),
    ?LOG_INFO(Format, Args)).

-define(INFO_MSG_IF(Condition, Format, Args),
    ?LOG_IF(info, Condition, Format, Args)).

-define(WARNING_MSG(Format, Args),
    ?LOG_WARNING(Format, Args)).

-define(WARNING_MSG_IF(Condition, Format, Args),
    ?LOG_IF(warning, Condition, Format, Args)).

-define(ERROR_MSG(Format, Args),
    ?LOG_ERROR(Format, Args)).

-define(ERROR_MSG_IF(Condition, Format, Args),
    ?LOG_IF(error, Condition, Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    ?LOG_CRITICAL(Format, Args)).

-define(CRITICAL_MSG_IF(Condition, Format, Args),
    ?LOG_IF(critical, Condition, Format, Args)).


-define(LOG_DEBUG_IF(Condition, Map),
        ((Condition) == true andalso ?LOG_DEBUG(Map))).

-define(LOG_WARNING_IF(Condition, Map),
        ((Condition) == true andalso ?LOG_WARNING(Map))).

-define(LOG_ERROR_IF(Condition, Map),
        ((Condition) == true andalso ?LOG_ERROR(Map))).

-define(LOG_INFO_IF(Condition, Map),
        ((Condition) == true andalso ?LOG_INFO(Map))).

-endif.
