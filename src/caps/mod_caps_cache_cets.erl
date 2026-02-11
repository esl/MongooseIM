-module(mod_caps_cache_cets).
-moduledoc "CETS backend for caching entity capabilities".

-behaviour(mod_caps_cache).

-export([init/1, stop/1, read/2, write/3, delete/2, handle_conflict/2]).

table_name(HostType) ->
    binary_to_atom(<<"cets_caps_cache_", HostType/binary>>).

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    Tab = table_name(HostType),
    cets:start(Tab, #{handle_conflict => fun ?MODULE:handle_conflict/2}),
    cets_discovery:add_table(mongoose_cets_discovery, Tab).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Tab = table_name(HostType),
    cets_discovery:delete_table(mongoose_cets_discovery, Tab),
    cets:stop(Tab).

-spec read(mongooseim:host_type(), mod_caps:hash()) ->
          {ok, [mod_caps:feature()]} | {error, not_found}.
read(HostType, Key) ->
    case ets:lookup(table_name(HostType), Key) of
        [] -> {error, not_found};
        [{_, Features}] -> {ok, Features}
    end.

-spec write(mongooseim:host_type(), mod_caps:hash(), [mod_caps:feature()]) -> ok.
write(HostType, Key, Features) ->
    cets:insert(table_name(HostType), {Key, Features}).

-spec delete(mongooseim:host_type(), mod_caps:hash()) -> ok.
delete(HostType, Key) ->
    cets:delete(table_name(HostType), Key).

%% TODO verify this is acceptable
-spec handle_conflict(Tuple, Tuple) -> Tuple
              when Tuple :: {mod_caps:hash(), [mod_caps:feature()]}.
handle_conflict({_, Features1}, {_, Features2}) ->
    max(Features1, Features2).
