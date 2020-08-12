-module(mongooseim_loglevel_SUITE_helper).
-export([log/2]).

-include_lib("kernel/include/logger.hrl").

log(Level, Message) -> ?LOG(Level, Message).
