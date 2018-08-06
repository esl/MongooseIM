%% @doc MAM message unique ID
%%
%% This behaviour defines the way message IDs are generated and transformed into timestamps or vice
%% versa. It is assumed that generated message IDs always hold the timestamp of their time of
%% generation so that they can be compared with timestamps by mod_mam and database backends.
%%
%% This module is global, so it works for both mod_mam and mod_mam_muc.
-module(mam_uid).

-callback generate_message_id() -> any().
-callback mess_id_to_external_binary(any()) -> binary().
-callback external_binary_to_mess_id(binary()) -> any().
-callback message_id_to_timestamp(any()) -> integer().
-callback timestamp_to_message_id(integer(), atom()) -> any().
