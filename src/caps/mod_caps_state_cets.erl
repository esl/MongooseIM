-module(mod_caps_state_cets).
-moduledoc "CETS backend for caching entity capabilities".

-behaviour(mod_caps_state).

-export([init/1, stop/1, get/2, get_resources/2, set/3, delete/2, handle_conflict/2]).

table_name(HostType) ->
    binary_to_atom(<<"cets_caps_state_", HostType/binary>>).

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

-spec get(mongooseim:host_type(), jid:ljid()) -> [mod_caps:feature()].
get(HostType, Key) ->
    case ets:lookup(table_name(HostType), Key) of
        [] -> [];
        [{_, Features}] -> Features
    end.

-spec get_resources(mongooseim:host_type(), jid:simple_bare_jid()) ->
          [{jid:lresource(), [mod_caps:feature()]}].
get_resources(HostType, {LUser, LServer}) ->
    ets:select(table_name(HostType), [{{{LUser, LServer, '$1'}, '$2'}, [], [{{'$1', '$2'}}]}]).

-spec set(mongooseim:host_type(), jid:ljid(), [mod_caps:feature()]) -> ok.
set(HostType, Key, Features) ->
    cets:insert(table_name(HostType), {Key, Features}).

-spec delete(mongooseim:host_type(), jid:ljid()) -> ok.
delete(HostType, Key) ->
    cets:delete(table_name(HostType), Key).

%% TODO verify this is acceptable
-spec handle_conflict(Tuple, Tuple) -> Tuple
              when Tuple :: {jid:ljid(), [mod_caps:feature()]}.
handle_conflict({_, Features1}, {_, Features2}) ->
    max(Features1, Features2).
