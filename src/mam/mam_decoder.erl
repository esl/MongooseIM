-module(mam_decoder).
-export([decode_row/2]).
-export([decode_muc_row/2]).
-export([decode_muc_gdpr_row/2]).
-export([decode_retraction_info/2]).

-type env_vars() :: map().

decode_row({ExtMessID, ExtSrcJID, ExtData}, Env) ->
    MessID = mongoose_rdbms:result_to_integer(ExtMessID),
    SrcJID = decode_jid(ExtSrcJID, Env),
    Packet = decode_packet(ExtData, Env),
    {MessID, SrcJID, Packet}.

decode_muc_row({ExtMessID, Nick, ExtData}, Env = #{archive_jid := RoomJID}) ->
    MessID = mongoose_rdbms:result_to_integer(ExtMessID),
    SrcJID = jid:replace_resource(RoomJID, Nick),
    Packet = decode_packet(ExtData, Env),
    {MessID, SrcJID, Packet}.

decode_muc_gdpr_row({ExtMessID, ExtData}, Env) ->
    Packet = decode_packet(ExtData, Env),
    {ExtMessID, Packet}.

decode_retraction_info(_Env, []) -> skip;
decode_retraction_info(Env, [{ResMessID, Data}]) ->
    Packet = decode_packet(Data, Env),
    MessID = mongoose_rdbms:result_to_integer(ResMessID),
    #{packet => Packet, message_id => MessID}.

-spec decode_jid(binary(), env_vars()) -> jid:jid().
decode_jid(ExtJID, #{db_jid_codec := Codec, archive_jid := ArcJID}) ->
    mam_jid:decode(Codec, ArcJID, ExtJID).

-spec decode_packet(binary(), env_vars()) -> exml:element().
decode_packet(ExtBin, Env = #{db_message_codec := Codec}) ->
    Bin = unescape_binary(ExtBin, Env),
    mam_message:decode(Codec, Bin).

-spec unescape_binary(binary(), env_vars()) -> binary().
unescape_binary(Bin, #{host := Host}) ->
    %% Funny, rdbms ignores this Host variable
    mongoose_rdbms:unescape_binary(Host, Bin).
