%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_codec_backend.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : MUC Light codec behaviour
%%% Created : 29 Sep 2015 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_muc_light_codec_backend).
-author('piotr.nosek@erlang-solutions.com').

-define(MAIN_MODULE, mod_muc_light_codec).

-include("mod_muc_light.hrl").
-include("jlib.hrl").

%% API
-export([start/2]).
-export([stop/1]).
-export([decode/4]).
-export([encode/5]).
-export([encode_error/5]).

-export([encode_error/6]).

-type encoded_packet_handler() ::
    fun((From :: jid:jid(), To :: jid:jid(), Packet :: exml:element()) -> mongoose_acc:t()).

-type decode_result() :: {ok, muc_light_packet() | muc_light_disco() | jlib:iq()}
                       | {error, bad_request}
                       | ignore.

-export_type([encoded_packet_handler/0, decode_result/0]).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-callback decode(From :: jid:jid(),
                 To :: jid:jid(),
                 Stanza :: jlib:iq() | exml:element(),
                 Acc :: mongoose_acc:t()) ->
    decode_result().

-callback encode(Request :: muc_light_encode_request(),
                 OriginalSender :: jid:jid(),
                 RoomJid :: jid:jid(), % may be just service domain
                 HandleFun :: encoded_packet_handler(),
                 Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().

-callback encode_error(ErrMsg :: tuple(),
                       OrigFrom :: jid:jid(),
                       OrigTo :: jid:jid(),
                       OrigPacket :: exml:element(),
                       Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().

%%====================================================================
%% API
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [], Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec decode(From :: jid:jid(),
             To :: jid:jid(),
             Stanza :: jlib:iq() | exml:element(),
             Acc :: mongoose_acc:t()) ->
    decode_result().
decode(From, To, Stanza, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    Args = [From, To, Stanza, Acc],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec encode(Request :: muc_light_encode_request(),
             OriginalSender :: jid:jid(),
             RoomJID :: jid:jid(),
             HandleFun :: encoded_packet_handler(),
             Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
encode(Request, Sender, RoomBareJid, HandleFun, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    Args = [Request, Sender, RoomBareJid, HandleFun, Acc],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec encode_error(
        ErrMsg :: tuple(), OrigFrom :: jid:jid(), OrigTo :: jid:jid(),
        OrigPacket :: exml:element(), Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
encode_error(ErrMsg, OrigFrom, OrigTo, OrigPacket, Acc) ->
    HostType = mongoose_acc:host_type(Acc),
    Args = [ErrMsg, OrigFrom, OrigTo, OrigPacket, Acc],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec encode_error(ErrMsg :: tuple(),
                   ExtraChildren :: [jlib:xmlch()],
                   OrigFrom :: jid:jid(),
                   OrigTo :: jid:jid(),
                   OrigPacket :: exml:element(),
                   Acc :: mongoose_acc:t()) ->
    mongoose_acc:t().
encode_error(ErrMsg, ExtraChildren, OrigFrom, OrigTo, OrigPacket, Acc) ->
    ErrorElem = make_error_elem(ErrMsg),
    ErrorPacket = jlib:make_error_reply(OrigPacket#xmlel{ children = ExtraChildren }, ErrorElem),
    ejabberd_router:route(OrigTo, OrigFrom, Acc, ErrorPacket).

-spec make_error_elem(tuple()) -> exml:element().
make_error_elem({error, not_allowed}) ->
    mongoose_xmpp_errors:not_allowed();
make_error_elem({error, bad_request}) ->
    mongoose_xmpp_errors:bad_request(<<"en">>, <<"Uncategorized bad request">>);
make_error_elem({error, item_not_found}) ->
    mongoose_xmpp_errors:item_not_found();
make_error_elem({error, {conflict, Text}}) ->
    mongoose_xmpp_errors:conflict(<<"en">>, Text);
make_error_elem({error, {bad_request, Text}}) ->
    make_error_elem({error, bad_request, Text});
make_error_elem({error, bad_request, Text}) ->
    mongoose_xmpp_errors:bad_request(<<"en">>, iolist_to_binary(Text));
make_error_elem({error, feature_not_implemented}) ->
    mongoose_xmpp_errors:feature_not_implemented();
make_error_elem({error, {feature_not_implemented, Text}}) ->
    mongoose_xmpp_errors:feature_not_implemented(<<"en">>, iolist_to_binary(Text));
make_error_elem({error, internal_server_error}) ->
    mongoose_xmpp_errors:internal_server_error();
make_error_elem({error, registration_required}) ->
    mongoose_xmpp_errors:registration_required();
make_error_elem({error, _}) ->
    mongoose_xmpp_errors:bad_request().
