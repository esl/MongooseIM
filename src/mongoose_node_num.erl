%% Returns a numeric id from 0 to 255 for the current node.
%% Used to generate MAM IDs.
-module(mongoose_node_num).
-export([node_num/0, set_node_num/1]).

-type node_num() :: 0..255.
-export_type([node_num/0]).

%% @doc Return an integer node ID.
-spec node_num() -> node_num().
node_num() ->
    %% We just return 0 if set_node_num/1 is not called.
    persistent_term:get(?MODULE, 0).

-spec set_node_num(node_num()) -> ignore | updated | same.
set_node_num(Num) ->
    case node_num() =:= Num of
        true ->
            same;
        false ->
            persistent_term:put(?MODULE, Num),
            updated
    end.
