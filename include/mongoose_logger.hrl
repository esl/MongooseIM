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

-define(UNEXPECTED_INFO(Msg),
        ?LOG_WARNING(#{what => unexpected_info, msg => Msg})).
-define(UNEXPECTED_CAST(Msg),
        ?LOG_WARNING(#{what => unexpected_cast, msg => Msg})).
-define(UNEXPECTED_CALL(Msg, From),
        ?LOG_WARNING(#{what => unexpected_call, msg => Msg, call_from => From})).

-endif.
