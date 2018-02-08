-ifndef(MONGOOSEIM_LOGGER_HRL).
-define(MONGOOSEIM_LOGGER_HRL, true).

-define(LOG_IF(Level, Condition, Format, Args),
    (Condition) == true andalso lager:Level(Format, Args)).

-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(DEBUG_IF(Condition, Format, Args),
    ?LOG_IF(debug, Condition, Format, Args)).

-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(INFO_MSG_IF(Condition, Format, Args),
    ?LOG_IF(info, Condition, Format, Args)).

-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(WARNING_MSG_IF(Condition, Format, Args),
    ?LOG_IF(warning, Condition, Format, Args)).

-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(ERROR_MSG_IF(Condition, Format, Args),
    ?LOG_IF(error, Condition, Format, Args)).

-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

-define(CRITICAL_MSG_IF(Condition, Format, Args),
    ?LOG_IF(critical, Condition, Format, Args)).

-endif.
