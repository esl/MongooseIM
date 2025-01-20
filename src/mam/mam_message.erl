%%% @doc Encoder and decoder for MAM messages
%%%
%%% Default implementations are:
%%% - mam_message_xml
%%% - mam_message_eterm
%%% - mam_message_compressed_eterm
-module(mam_message).

-ignore_xref([behaviour_info/1]).

-callback encode(exml:element()) -> binary().
-callback decode(binary()) -> exml:element().

-export([encode/2, decode/2]).

-include_lib("kernel/include/logger.hrl").
-include_lib("exml/include/exml.hrl").

-spec encode(module(), exml:element()) -> binary().
encode(Mod, Packet) -> Mod:encode(Packet).

-spec decode(module(), binary()) -> exml:element().
decode(Mod, Bin) ->
    try
        Mod:decode(Bin)
    catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{what => mam_failed_to_decode_message,
                     encoded_message => Bin,
                     class => Class, reason => Reason, stacktrace => Stacktrace}),
        error_stanza()
    end.

error_stanza() ->
    Text = <<"Failed to decode message in database">>,
    Err = mongoose_xmpp_errors:internal_server_error(<<"en">>, Text),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Text}]},
    #xmlel{name = <<"message">>,
           attrs = #{<<"type">> => <<"error">>},
           children = [Err, Body]}.
