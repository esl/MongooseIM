-module(mongoose_stanza_api).
-export([lookup_recent_messages/4]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").

%% Before is in microseconds
-spec lookup_recent_messages(
        ArcJID :: jid:jid(),
        With :: jid:jid() | undefined,
        Before :: mod_mam:unix_timestamp() | undefined,
        Limit :: non_neg_integer()) ->
    [mod_mam:message_row()].
lookup_recent_messages(_, _, _, Limit) when Limit > 500 ->
    throw({error, message_limit_too_high});
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(ArcJID) ->
    lookup_recent_messages(jid:from_binary(ArcJID), With, Before, Limit);
lookup_recent_messages(ArcJID, With, Before, Limit) when is_binary(With) ->
    lookup_recent_messages(ArcJID, jid:from_binary(With), Before, Limit);
lookup_recent_messages(ArcJID, WithJID, Before, Limit) ->
    #jid{luser = LUser, lserver = LServer} = ArcJID,
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    EndTS = case Before of
                0 -> undefined;
                _ -> Before
            end,
    Params = #{archive_id => mod_mam_pm:archive_id(LServer, LUser),
               owner_jid => ArcJID,
               borders => undefined,
               rsm => #rsm_in{direction = before, id = undefined}, % last msgs
               start_ts => undefined,
               end_ts => EndTS,
               now => os:system_time(microsecond),
               with_jid => WithJID,
               search_text => undefined,
               page_size => Limit,
               limit_passed => false,
               max_result_limit => 1,
               is_simple => true},
    R = mod_mam_pm:lookup_messages(HostType, Params),
    {ok, {_, _, L}} = R,
    L.
