-module(mam_encoder).
-export([encode_message/3]).
-export([encode_jid/2]).
-export([encode_direction/1]).
-export([encode_packet/2]).
-export([extend_lookup_params/2]).

-include("jlib.hrl").
-include("mongoose_mam.hrl").

-type value_type() :: int | maybe_string | direction | bare_jid | jid | jid_resource | xml | search.
-type env_vars() :: mod_mam_rdbms_arch:env_vars().
-type db_mapping() :: #db_mapping{}.
-type encoded_field_value() :: term().

-spec extend_lookup_params(mam_iq:lookup_params(), env_vars()) -> mam_iq:lookup_params().
extend_lookup_params(#{start_ts := Start, end_ts := End, with_jid := WithJID,
                       borders := Borders, search_text := SearchText} = Params, Env) ->
    Params#{norm_search_text => mod_mam_utils:normalize_search_text(SearchText),
            start_id => make_start_id(Start, Borders),
            end_id => make_end_id(End, Borders),
            remote_bare_jid => maybe_encode_bare_jid(WithJID, Env),
            remote_resource => jid_to_non_empty_resource(WithJID)}.

-spec encode_message(mod_mam:archive_message_params(), env_vars(), list(db_mapping())) ->
    [encoded_field_value()].
encode_message(Params, Env, Mappings) ->
    [encode_value_using_mapping(Params, Env, Mapping) || Mapping <- Mappings].

encode_value_using_mapping(Params, Env, #db_mapping{param = Param, format = Format}) ->
    Value = maps:get(Param, Params),
    encode_value(Format, Value, Env).

-spec encode_value(value_type(), term(), env_vars()) -> encoded_field_value().
encode_value(int, Value, _Env) when is_integer(Value) ->
    Value;
encode_value(maybe_string, none, _Env) ->
    null;
encode_value(maybe_string, Value, _Env) when is_binary(Value) ->
    Value;
encode_value(direction, Value, _Env) ->
    encode_direction(Value);
encode_value(bare_jid, Value, Env) ->
    encode_jid(jid:to_bare(Value), Env);
encode_value(jid, Value, Env) ->
    encode_jid(Value, Env);
encode_value(jid_resource, #jid{lresource = Res}, _Env) ->
    Res;
encode_value(xml, Value, Env) ->
    encode_packet(Value, Env);
encode_value(search, Value, Env) ->
    encode_search_body(Value, Env);
encode_value(bool, Value, _Env) ->
    encode_boolean(Value).

encode_direction(incoming) -> <<"I">>;
encode_direction(outgoing) -> <<"O">>.

make_start_id(Start, Borders) ->
    StartID = maybe_encode_compact_uuid(Start, 0),
    mod_mam_utils:apply_start_border(Borders, StartID).

make_end_id(End, Borders) ->
    EndID = maybe_encode_compact_uuid(End, 255),
    mod_mam_utils:apply_end_border(Borders, EndID).

maybe_encode_compact_uuid(undefined, _) ->
    undefined;
maybe_encode_compact_uuid(Microseconds, NodeID) ->
    mod_mam_utils:encode_compact_uuid(Microseconds, NodeID).

jid_to_non_empty_resource(undefined) -> undefined;
jid_to_non_empty_resource(#jid{lresource = <<>>}) -> undefined;
jid_to_non_empty_resource(#jid{lresource = Res}) -> Res.

-spec encode_jid(jid:jid(), env_vars()) -> binary().
encode_jid(JID, #{db_jid_codec := Codec, archive_jid := ArcJID}) ->
    mam_jid:encode(Codec, ArcJID, JID).

maybe_encode_bare_jid(undefined, _Env) -> undefined;
maybe_encode_bare_jid(JID, Env) -> encode_jid(jid:to_bare(JID), Env).

-spec encode_packet(exml:element(), env_vars()) -> binary().
encode_packet(Packet, #{db_message_codec := Codec}) ->
    mam_message:encode(Codec, Packet).

-spec encode_search_body(exml:element(), env_vars()) -> binary().
encode_search_body(Packet, #{has_full_text_search := SearchEnabled}) ->
    mod_mam_utils:packet_to_search_body(SearchEnabled, Packet).

encode_boolean(true) -> 1;
encode_boolean(false) -> 0.
