%% @doc MAM message unique ID
%%
%% How to generate or represent message ids
%% This module is global, so it works for both mod_mam and mod_mam_muc
-module(mam_uid).

-callback generate_message_id() -> any().
-callback encode_compact_uuid(integer(), byte()) -> any().
-callback decode_compact_uuid(any()) -> {integer(), byte()}.
-callback mess_id_to_external_binary(any()) -> binary().
-callback external_binary_to_mess_id(binary()) -> any().
