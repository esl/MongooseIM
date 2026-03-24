-module(mod_pubsub).

-export([hooks/1]).

-export([user_send_iq/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(item, {node_key :: node_key(),
               id :: binary(),
               publisher_jid :: jid:jid(),
               payload = [] :: [exml:child()]}).
-type item() :: #item{}.
-type node_key() :: {jid:jid(), node_id()}.
-type node_id() :: binary().

hooks(HostType) ->
    [{user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 50}].

user_send_iq(Acc, _Args = #{c2s_data := C2SData}, _Extra) ->
    {From, To, Stanza} = mongoose_acc:packet(Acc),
    IQ = jlib:iq_query_info(Stanza),
    maybe
        #xmlel{name = ~"pubsub", children = Children} ?= IQ#iq.sub_el,
        #{} ?= Action = pubsub_action(IQ#iq.xmlns, jlib:remove_cdata(Children)),
        ?LOG_DEBUG(#{what => pubsub_iq_action, action => Action, acc => Acc, c2s_data => C2SData}),
        maybe_reply(perform_action(Action#{service_jid => To, c2s_data => C2SData})),
        {stop, Acc}
    else
        _ -> {ok, Acc}
    end.

perform_action(#{from_jid := PublisherJid, service_jid := ServiceJid, action := publish, node_id := NodeId,
                 item_id := ItemId, payload := Payload}) ->
    Item = #item{node_key = {ServiceJid, NodeId}, id = ItemId,
                 publisher_jid = PublisherJid, payload = Payload},
    mod_pubsub_db:write_item(Item),
    mod_pubsub_broadcast:broadcast_item(Item),
    {reply, [#xmlel{name = ~"item", attrs = #{id => ItemId}}]}.

maybe_reply({reply, _Reply}) ->
    %% TODO
    ok.

pubsub_action(?NS_PUBSUB, [El = #xmlel{name = ~"publish", attrs = #{~"node" := NodeId}}]) ->
    case exml_query:subelements(El, ~"item") of
        [#xmlel{attrs = #{~"id" := ItemId}, children = Payload}] ->
            #{action => publish, node_id => NodeId, item_id => ItemId, payload => Payload};
        [] ->
            #{action => error, reason => {bad_request, ~"item-required"}};
        [_] ->
            #{action => error, reason => {bad_request, ~"invalid-payload"}}
    end;
pubsub_action(_, _) ->
    no_action.
