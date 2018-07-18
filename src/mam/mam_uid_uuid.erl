%% @doc Time based UUIDs message IDs
%%
%% See https://www.famkruithof.net/guid-uuid-timebased.html for details
-module(mam_uid_uuid).
-behaviour(mam_uid).

%% UID
-export([generate_message_id/0,
         encode_compact_uuid/2,
         decode_compact_uuid/1,
         mess_id_to_external_binary/1,
         external_binary_to_mess_id/1]).

%% -----------------------------------------------------------------------
%% UID

-spec generate_message_id() -> binary().
generate_message_id() ->
    quickrand:seed(),
    UState = uuid:new(self()),
    {UUID, _NewState} = uuid:get_v1(UState),
    list_to_binary(uuid:uuid_to_string(UUID)).

%% @doc Create a message ID (UID).
%%
%% It removes a leading 0 from 64-bit binary representation.
%% It puts node id as a last byte.
%% The maximum date, that can be encoded is `{{4253,5,31},{22,20,37}}'.
-spec encode_compact_uuid(integer(), byte()) -> integer().
encode_compact_uuid(Microseconds, NodeId)
  when is_integer(Microseconds), is_integer(NodeId) ->
    (Microseconds bsl 8) + NodeId.

%% @doc Extract date and node id from a message id.
-spec decode_compact_uuid(binary()) -> {integer(), byte()}.
decode_compact_uuid(UUID) ->
    <<TimeLow:32, TimeMid:16, 1:4, TimeHi:12, _ClockID:16, NodeId:48>> = uuid:string_to_uuid(
                                                                           binary_to_list(UUID)),
    UUIDTimestamp = binary:decode_unsigned(<<TimeHi:16, TimeMid:16, TimeLow:32>>),
    %% uuid offset and nanosecends step
    UnixTimestamp = (UUIDTimestamp - 122192928000000000) div 10,
    {UnixTimestamp , NodeId band 255}.

%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(binary()) -> binary().
mess_id_to_external_binary(MessID) when is_binary(MessID) -> MessID.

%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> binary().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) -> BExtMessID.
