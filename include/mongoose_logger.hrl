-ifndef(MONGOOSEIM_LOGGER_HRL).
-define(MONGOOSEIM_LOGGER_HRL, true).

-include_lib("kernel/include/logger.hrl").

-define(LOG_IF(Level, Condition, Msg),
    (Condition) == true andalso ?LOG(Level, Msg)).

-define(LOG_IF(Level, Condition, Format, Args),
    (Condition) == true andalso ?LOG(Level, Format, Args)).

-define(UNEXPECTED_INFO(Msg),
        ?LOG_WARNING(#{what => unexpected_info, msg => Msg})).
-define(UNEXPECTED_CAST(Msg),
        ?LOG_WARNING(#{what => unexpected_cast, msg => Msg})).
-define(UNEXPECTED_CALL(Msg, From),
        ?LOG_WARNING(#{what => unexpected_call, msg => Msg, call_from => From})).

-endif.
