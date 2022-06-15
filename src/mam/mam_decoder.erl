-module(mam_decoder).
-export([decode_row/2]).
-export([decode_muc_row/2]).
-export([decode_muc_gdpr_row/2]).
-export([decode_retraction_info/3]).

-type ext_mess_id() :: non_neg_integer() | binary().
-type env_vars() :: mod_mam_rdbms_arch:env_vars().
-type db_row() :: {ext_mess_id(), ExtSrcJID :: binary(), ExtData :: binary()}.
-type db_muc_row() :: {ext_mess_id(), Nick :: binary(), ExtData :: binary()}.
-type db_muc_gdpr_row() :: {ext_mess_id(), ExtData :: binary()}.
-type decoded_muc_gdpr_row() :: {ext_mess_id(), exml:element()}.

-spec decode_row(db_row(), env_vars()) -> mod_mam:message_row().
decode_row({ExtMessID, ExtSrcJID, ExtData}, Env) ->
    MessID = mongoose_rdbms:result_to_integer(ExtMessID),
    SrcJID = decode_jid(ExtSrcJID, Env),
    Packet = decode_packet(ExtData, Env),
    #{id => MessID, jid => SrcJID, packet => Packet}.

-spec decode_muc_row(db_muc_row(), env_vars()) -> mod_mam:message_row().
decode_muc_row({ExtMessID, Nick, ExtData}, Env = #{archive_jid := RoomJID}) ->
    MessID = mongoose_rdbms:result_to_integer(ExtMessID),
    SrcJID = jid:replace_resource(RoomJID, Nick),
    Packet = decode_packet(ExtData, Env),
    #{id => MessID, jid => SrcJID, packet => Packet}.

-spec decode_muc_gdpr_row(db_muc_gdpr_row(), env_vars()) -> decoded_muc_gdpr_row().
decode_muc_gdpr_row({ExtMessID, ExtData}, Env) ->
    Packet = decode_packet(ExtData, Env),
    {ExtMessID, Packet}.

-spec decode_retraction_info(env_vars(),
                             [{binary(), mod_mam:message_id(), binary()}],
                             mod_mam_utils:retraction_id()) ->
    skip | mod_mam_utils:retraction_info().
decode_retraction_info(_Env, [], _) -> skip;
decode_retraction_info(Env, [{ResMessID, Data}], {origin_id, OriginID}) ->
    Packet = decode_packet(Data, Env),
    MessID = mongoose_rdbms:result_to_integer(ResMessID),
    #{retract_on => origin_id, packet => Packet, message_id => MessID, origin_id => OriginID};
decode_retraction_info(Env, [{OriginID, Data}], {stanza_id, StanzaID}) ->
    Packet = decode_packet(Data, Env),
    MessID = mod_mam_utils:external_binary_to_mess_id(StanzaID),
    #{retract_on => stanza_id, packet => Packet, message_id => MessID, origin_id => OriginID}.

-spec decode_jid(binary(), env_vars()) -> jid:jid().
decode_jid(ExtJID, #{db_jid_codec := Codec, archive_jid := ArcJID}) ->
    mam_jid:decode(Codec, ArcJID, ExtJID).

-spec decode_packet(binary(), env_vars()) -> exml:element().
decode_packet(ExtBin, Env = #{db_message_codec := Codec}) ->
    Bin = unescape_binary(ExtBin, Env),
    mam_message:decode(Codec, Bin).

-spec unescape_binary(binary(), env_vars()) -> binary().
unescape_binary(Bin, #{host_type := HostType}) ->
    mongoose_rdbms:unescape_binary(HostType, Bin).
