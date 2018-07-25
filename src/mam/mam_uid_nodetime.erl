%% @doc Message id based on node_id and unique timestamp
%%
%% Message ids unique unless nodes get restarted
%% We use HEX encoding for external ids
%% Ids are not transarent for the clients (they can't get timestamp from id)
-module(mam_uid_nodetime).
-behaviour(mam_uid).

%% UID
-export([generate_message_id/0,
         mess_id_to_external_binary/1,
         external_binary_to_mess_id/1,
         message_id_to_timestamp/1,
         timestamp_to_message_id/2]).

%% -----------------------------------------------------------------------
%% UID

-spec generate_message_id() -> integer().
generate_message_id() ->
    {ok, NodeId} = ejabberd_node_id:node_id(),
    CandidateStamp = p1_time_compat:os_system_time(micro_seconds),
    Microseconds = mongoose_mam_id:next_unique(CandidateStamp),
    (Microseconds bsl 8) + NodeId.


%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(integer()) -> binary().
mess_id_to_external_binary(MessID) when is_integer(MessID) ->
    list_to_binary(integer_to_list(MessID, 32)).


%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> integer().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) ->
    binary_to_integer(BExtMessID, 32).


%% @doc Extract the date from a message ID.
-spec message_id_to_timestamp(integer()) -> mod_mam:unix_timestamp().
message_id_to_timestamp(MessID) ->
    MessID bsr 8.


%% @doc Transform a timestamp to a message ID
-spec timestamp_to_message_id(mod_mam:posix_timestamp(), atom()) -> integer().
timestamp_to_message_id(Microseconds, min) ->
    Microseconds bsl 8;
timestamp_to_message_id(Microseconds, max) ->
    (Microseconds bsl 8) + 255.
