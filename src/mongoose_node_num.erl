%% Returns a numeric id from 0 to 255 for the current node.
%% Used to generate MAM IDs.
-module(mongoose_node_num).
-export([set_node_num/1]).
-export([node_num/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-type node_num() :: 0..255.
-define(KEY, ?MODULE).
-export_type([node_num/0]).

%% @doc Return an integer node ID.
-spec node_num() -> node_num().
node_num() ->
    %% We just return 0 if service is not running.
    persistent_term:get(?KEY, 0).

-spec set_node_num(node_num()) -> ignore | updated | same.
set_node_num(Num) ->
    case node_num() =:= Num of
        true ->
            same;
        false ->
            persistent_term:put(?KEY, Num),
            updated
    end.
