%% @doc MAM message unique ID
%%
%% How to generate or represent message ids
%% This module is global, so it works for both mod_mam and mod_mam_muc
-module(mam_uid).

-callback generate_message_id() -> integer().
-callback encode_compact_uuid(integer(), integer()) -> integer().
-callback decode_compact_uuid(integer()) -> {integer(),byte()}.
-callback mess_id_to_external_binary(integer()) -> binary().
-callback external_binary_to_mess_id(binary()) -> integer().
