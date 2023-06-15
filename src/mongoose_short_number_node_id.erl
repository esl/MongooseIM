%% Returns an id from 0 to 255 for the current node.
%% Used to generate MAM IDs.
-module(mongoose_short_number_node_id).
-export([set_node_id/1]).
-export([node_id/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-type node_id() :: 0..255.
-define(KEY, ?MODULE).
-export_type([node_id/0]).

%% @doc Return an integer node ID.
-spec node_id() -> node_id().
node_id() ->
    %% We just return 0 if service is not running.
    persistent_term:get(?KEY, 0).

-spec set_node_id(node_id()) -> ignore | updated | same.
set_node_id(Id) ->
    case node_id() =:= Id of
        true ->
            same;
        false ->
            persistent_term:put(?KEY, Id),
            updated
    end.
