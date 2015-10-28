%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_codec.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC Light codec behaviour
%%% Created : 29 Sep 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_codec).
-author('piotr.nosek@erlang-solutions.com').

-include("mod_muc_light.hrl").
-include("jlib.hrl").

%% API
-export([encode_error/5]).

-type encoded_packet_handler() ::
    fun((From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> any()).

-type decode_result() :: {ok, muc_light_packet() | muc_light_disco() | ejabberd:iq()}
                       | {error, bad_request} | ignore.

-export_type([encoded_packet_handler/0, decode_result/0]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback decode(From :: ejabberd:jid(), To :: ejabberd:jid(), Stanza :: jlib:xmlel()) ->
    decode_result().

-callback encode(Request :: muc_light_encode_request(), OriginalSender :: ejabberd:jid(),
                 RoomUS :: ejabberd:simple_bare_jid(), % may be just service domain
                 HandleFun :: mod_muc_light_codec:encoded_packet_handler()) -> any().

%%====================================================================
%% API
%%====================================================================

-spec encode_error(ErrMsg :: tuple(), OrigFrom :: ejabberd:jid(), OrigTo :: ejabberd:jid(),
                   OrigPacket :: jlib:xmlel(), HandleFun :: encoded_packet_handler()) ->
    any().
encode_error(ErrMsg, OrigFrom, OrigTo, OrigPacket, HandleFun) ->
    ErrorElem = make_error_elem(ErrMsg),
    ErrorPacket = jlib:make_error_reply(OrigPacket#xmlel{ children = [] }, ErrorElem),
    HandleFun(OrigTo, OrigFrom, ErrorPacket).

-spec make_error_elem(tuple()) -> jlib:xmlel().
make_error_elem({error, not_allowed}) ->
    ?ERR_NOT_ALLOWED;
make_error_elem({error, bad_request}) ->
    ?ERR_BAD_REQUEST;
make_error_elem({error, item_not_found}) ->
    ?ERR_ITEM_NOT_FOUND;
make_error_elem({error, conflict}) ->
    ?ERR_CONFLICT;
make_error_elem({error, bad_request, Text}) ->
    ?ERRT_BAD_REQUEST(<<"en">>, iolist_to_binary(Text));
make_error_elem({error, feature_not_implemented}) ->
    ?ERR_FEATURE_NOT_IMPLEMENTED;
make_error_elem({error, internal_server_error}) ->
    ?ERR_INTERNAL_SERVER_ERROR.

