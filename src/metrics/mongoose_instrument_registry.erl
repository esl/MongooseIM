-module(mongoose_instrument_registry).

-export([start/0, attach/3, detach/2, lookup/2]).

-spec start() -> ok.
start() ->
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    ok.

-spec attach(mongoose_instrument:event_name(), mongoose_instrument:labels(),
             mongoose_instrument:handlers()) -> ok.
attach(Event, Labels, Val) ->
    ets:insert_new(?MODULE, {{Event, Labels}, Val}),
    ok.

-spec detach(mongoose_instrument:event_name(), mongoose_instrument:labels()) -> ok.
detach(Event, Labels) ->
    ets:delete(?MODULE, {Event, Labels}),
    ok.

-spec lookup(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
          {ok, mongoose_instrument:handlers()} | {error, not_found}.
lookup(Event, Labels) ->
    case ets:lookup(?MODULE, {Event, Labels}) of
        [] -> {error, not_found};
        [{_, Val}] -> {ok, Val}
    end.
