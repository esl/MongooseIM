-module(push_helper).
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-include("push_helper.hrl").

-export([enable_stanza/2, enable_stanza/3, enable_stanza/4,
         disable_stanza/1, disable_stanza/2]).

-export([become_unavailable/1, become_available/2]).

-export([ns_push/0, ns_pubsub_pub_options/0, push_form_type/0, make_form/1]).

-import(distributed_helper, [mim/0,
                             rpc/4]).

ns_push() -> <<"urn:xmpp:push:0">>.
ns_pubsub_pub_options() -> <<"http://jabber.org/protocol/pubsub#publish-options">>.
push_form_type()-> <<"urn:xmpp:push:summary">>.

disable_stanza(JID, undefined) ->
    disable_stanza([
                    {<<"xmlns">>, <<"urn:xmpp:push:0">>},
                    {<<"jid">>, JID}
                   ]);
disable_stanza(JID, Node) ->
    disable_stanza([
                    {<<"xmlns">>, <<"urn:xmpp:push:0">>},
                    {<<"jid">>, JID},
                    {<<"node">>, Node}
                   ]).

disable_stanza(JID) when is_binary(JID) ->
    disable_stanza(JID, undefined);
disable_stanza(Attrs) when is_list(Attrs) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>, attrs = Attrs}]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).
enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ns_pubsub_pub_options()).
enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = [
        {<<"xmlns">>, <<"urn:xmpp:push:0">>},
        {<<"jid">>, JID},
        {<<"node">>, Node}
    ], children = maybe_form(FormFields, FormType)}]).

maybe_form(undefined, _FormType) ->
    [];
maybe_form(FormFields, FormType) ->
    [make_form([{<<"FORM_TYPE">>, FormType} | FormFields])].

make_form(Fields) ->
    #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"submit">>}],
           children = [make_form_field(Name, Value) || {Name, Value} <- Fields]}.

make_form_field(Name, Value) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, Name}],
           children = [#xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}]}.

become_unavailable(Client) ->
    escalus:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    {ok, _} = wait_for_user_offline(Client).

become_available(Client, NumberOfUnreadMessages) ->
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Preds = [ is_presence | lists:duplicate(NumberOfUnreadMessages, is_message) ],
    Stanzas = escalus:wait_for_stanzas(Client, NumberOfUnreadMessages + 1),
    escalus:assert_many(Preds, Stanzas),
    {ok, true} = wait_for_user_online(Client).

is_online(LUser, LServer, LRes) ->
    PResources =  rpc(mim(), ejabberd_sm, get_user_present_resources, [LUser, LServer]),
    case lists:keyfind(LRes, 2, PResources) of
        {_, LRes} ->
            true;
        Other ->
            false
    end.

wait_for_user_online(Client) ->
    mongoose_helper:wait_until(fun() ->
                                       is_online(escalus_utils:jid_to_lower(escalus_client:username(Client)),
                                                  escalus_utils:jid_to_lower(escalus_client:server(Client)),
                                                  escalus_utils:jid_to_lower(escalus_client:resource(Client)))
                               end,
                               true,
                               #{sleep_time => 500, time_left => timer:seconds(20), name => is_online}).

is_offline(LUser, LServer, LRes) ->
    PResources =  rpc(mim(), ejabberd_sm, get_user_present_resources, [LUser, LServer]),
    case lists:keyfind(LRes, 2, PResources) of
        false ->
            true;
        _ ->
            false
    end.

wait_for_user_offline(Client) ->
    mongoose_helper:wait_until(fun() ->
                                       is_offline(escalus_utils:jid_to_lower(escalus_client:username(Client)),
                                                  escalus_utils:jid_to_lower(escalus_client:server(Client)),
                                                  escalus_utils:jid_to_lower(escalus_client:resource(Client)))
                               end,
                               true,
                               #{time_left => timer:seconds(20), name => is_offline}).
