%% @doc Time based UUIDs message IDs
%%
%% See https://www.famkruithof.net/guid-uuid-timebased.html for details
-module(mam_uid_uuid).
-behaviour(mam_uid).

%% UID
-export([generate_message_id/0,
         mess_id_to_external_binary/1,
         external_binary_to_mess_id/1,
         message_id_to_timestamp/1,
         timestamp_to_message_id/1]).

%% -----------------------------------------------------------------------
%% UID

-spec generate_message_id() -> binary().
generate_message_id() ->
    quickrand:seed(),
    UState = uuid:new(self()),
    {UUID, _NewState} = uuid:get_v1(UState),
    list_to_binary(uuid:uuid_to_string(UUID)).


%% @doc Encode a message ID to pass it to the user.
-spec mess_id_to_external_binary(binary()) -> binary().
mess_id_to_external_binary(MessID) when is_binary(MessID) -> MessID.


%% @doc Decode a message ID received from the user.
-spec external_binary_to_mess_id(binary()) -> binary().
external_binary_to_mess_id(BExtMessID) when is_binary(BExtMessID) -> BExtMessID.


%% @doc Extract the date from a message ID.
-spec message_id_to_timestamp(binary()) -> mod_mam:unix_timestamp().
message_id_to_timestamp(Id) ->
    uuid:get_v1_time(Id).


%% @doc Transform a timestamp to a message ID
-spec timestamp_to_message_id(mod_mam:posix_timestamp()) -> binary().
timestamp_to_message_id(Microseconds) ->
    BinNanoSecs = integer_to_binary((Microseconds * 10) + 16#01b21dd213814000),
    Time = <<0:(60-bit_size(BinNanoSecs)), BinNanoSecs/binary>>,
    <<TimeHigh:12, TimeMid:16, TimeLow:32>> = Time,
    %% generate a usual message ID and replace the timestamp part of it
    <<_:32, _:16, VersionBits:4, _:12, Suffix:64>> = generate_message_id(),
    <<TimeLow:32, TimeMid:16, VersionBits:4, TimeHigh:12, Suffix:64>>.
