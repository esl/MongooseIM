%% @doc Message id based on node_id and unique timestamp
%%
%% Message ids unique unless nodes get restarted
%% We use HEX encoding for external ids
%% Ids are not transarent for the clients (they can't get timestamp from id)
-module(mam_uid_nodetime).
-behaviour(mam_uid).

%% UID
-export([generate_message_id/0,
         encode_compact_uuid/2,
         decode_compact_uuid/1,
         mess_id_to_external_binary/1,
         external_binary_to_mess_id/1]).

%% -----------------------------------------------------------------------
%% UID

-spec generate_message_id() -> integer().
generate_message_id() ->
    {ok, NodeId} = ejabberd_node_id:node_id(),
    CandidateStamp = p1_time_compat:os_system_time(micro_seconds),
    UniqueStamp = mongoose_mam_id:next_unique(CandidateStamp),
    encode_compact_uuid(UniqueStamp, NodeId).


%% @doc Create a message ID (UID).
%%
%% It removes a leading 0 from 64-bit binary representation.
%% It puts node id as a last byte.
%% The maximum date, that can be encoded is `{{4253,5,31},{22,20,37}}'.
-spec encode_compact_uuid(integer(), integer()) -> integer().
encode_compact_uuid(Microseconds, NodeId)
    when is_integer(Microseconds), is_integer(NodeId) ->
    (Microseconds bsl 8) + NodeId.

%% @doc Extract date and node id from a message id.
-spec decode_compact_uuid(integer()) -> {integer(),byte()}.
decode_compact_uuid(Id) ->
    Microseconds = Id bsr 8,
    NodeId = Id band 255,
    {Microseconds, NodeId}.


%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(integer()) -> binary().
mess_id_to_external_binary(MessID) when is_integer(MessID) ->
    list_to_binary(integer_to_list(MessID, 32)).


%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> integer().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    binary_to_integer(BExtMessID, 32).
