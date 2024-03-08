-module(mongoose_instrument_log).

-behaviour(mongoose_instrument).

-export([set_up/3, handle_event/4]).

-include("mongoose.hrl").

-spec set_up(mongoose_instrument:event_name(), mongoose_instrument:labels(),
             mongoose_instrument:config()) -> boolean().
set_up(_EventName, _Labels, _Config) ->
    true.

-spec handle_event(mongoose_instrument:event_name(), mongoose_instrument:labels(),
                   mongoose_instrument:config(), mongoose_instrument:measurements()) -> ok.
handle_event(EventName, Labels, Config, Measurements) ->
    ?LOG(get_loglevel(Config, Measurements),
         #{what => EventName,
           labels => Labels,
           measurements => maps:remove(loglevel, Measurements)}).

-spec get_loglevel(mongoose_instrument:config(), mongoose_instrument:measurements()) ->
          logger:level().
get_loglevel(#{}, #{loglevel := LogLevel}) -> LogLevel;
get_loglevel(#{loglevel := LogLevel}, #{}) -> LogLevel;
get_loglevel(#{}, #{}) -> mongoose_config:get_opt([instrumentation, log, level], debug).
