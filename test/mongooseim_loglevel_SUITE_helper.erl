-module(mongooseim_loglevel_SUITE_helper).
-export([log/3]).

-include_lib("kernel/include/logger.hrl").

log(critical, Fmt, Args) -> ?LOG_CRITICAL(Fmt, Args);
log(error, Fmt, Args)    -> ?LOG_ERROR(Fmt, Args);
log(warning, Fmt, Args)  -> ?LOG_WARNING(Fmt, Args);
log(info, Fmt, Args)     -> ?LOG_INFO(Fmt, Args);
log(debug, Fmt, Args)    -> ?LOG_DEBUG(Fmt, Args).
