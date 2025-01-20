-module(push_helper).

-include_lib("exml/include/exml.hrl").
-include("push_helper.hrl").

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).

ns_push() -> <<"urn:xmpp:push:0">>.
ns_pubsub_pub_options() -> <<"http://jabber.org/protocol/pubsub#publish-options">>.
push_form_type()-> <<"urn:xmpp:push:summary">>.

disable_stanza(JID, undefined) ->
    disable_stanza(#{<<"xmlns">> => <<"urn:xmpp:push:0">>,
                     <<"jid">> => JID});
disable_stanza(JID, Node) ->
    disable_stanza(#{<<"xmlns">> => <<"urn:xmpp:push:0">>,
                     <<"jid">> => JID,
                     <<"node">> => Node}).

disable_stanza(JID) when is_binary(JID) ->
    disable_stanza(JID, undefined);
disable_stanza(Attrs) when is_map(Attrs) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"disable">>, attrs = Attrs}]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).
enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ns_pubsub_pub_options()).
enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = #{
        <<"xmlns">> => <<"urn:xmpp:push:0">>,
        <<"jid">> => JID,
        <<"node">> => Node
    }, children = maybe_form(FormFields, FormType)}]).

maybe_form(undefined, _FormType) ->
    [];
maybe_form(FormFields, FormType) ->
    FieldSpecs = lists:map(fun field_spec/1, FormFields),
    [form_helper:form(#{fields => FieldSpecs, ns => FormType})].

field_spec({Var, undefined}) -> #{var => Var};
field_spec({Var, Value}) -> #{var => Var, values => [Value]}.

become_unavailable(Client) ->
    escalus:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    {ok, _} = wait_for_user_offline(Client).

become_available(Client, NumberOfUnreadMessages) ->
    become_available(Client, NumberOfUnreadMessages, 5000).

become_available(Client, NumberOfUnreadMessages, Timeout) ->
    mongoose_helper:wait_for_n_offline_messages(Client, NumberOfUnreadMessages),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Preds = [ is_presence | lists:duplicate(NumberOfUnreadMessages, is_message) ],
    Stanzas = escalus:wait_for_stanzas(Client, NumberOfUnreadMessages + 1, Timeout),
    escalus:assert_many(Preds, Stanzas),
    {ok, true} = wait_for_user_online(Client).

is_online(LUser, LServer, LRes) ->
    JID = mongoose_helper:make_jid_noprep(LUser, LServer, LRes),
    PResources =  rpc(mim(), ejabberd_sm, get_user_present_resources, [JID]),
    case lists:keyfind(LRes, 2, PResources) of
        {_, LRes} ->
            true;
        _Other ->
            false
    end.

wait_for_user_online(Client) ->
    wait_helper:wait_until(fun() ->
                                   is_online(escalus_utils:jid_to_lower(escalus_client:username(Client)),
                                             escalus_utils:jid_to_lower(escalus_client:server(Client)),
                                             escalus_utils:jid_to_lower(escalus_client:resource(Client)))
                           end,
                           true,
                           #{sleep_time => 500, time_left => timer:seconds(20), name => is_online}).

is_offline(LUser, LServer, LRes) ->
    JID = mongoose_helper:make_jid_noprep(LUser, LServer, LRes),
    PResources = rpc(mim(), ejabberd_sm, get_user_present_resources, [JID]),
    case lists:keyfind(LRes, 2, PResources) of
        false ->
            true;
        _ ->
            false
    end.

get_raw_sessions(LUser, LServer) ->
    JID = mongoose_helper:make_jid_noprep(LUser, LServer, <<>>),
    rpc(mim(), ejabberd_sm, get_raw_sessions, [JID]).

wait_for_user_offline(Client) ->
    U = escalus_utils:jid_to_lower(escalus_client:username(Client)),
    S = escalus_utils:jid_to_lower(escalus_client:server(Client)),
    R = escalus_utils:jid_to_lower(escalus_client:resource(Client)),
    wait_helper:wait_until(fun() -> is_offline(U, S, R) end,
                           true,
                           #{time_left => timer:seconds(20), name => wait_for_user_offline,
                             on_error => fun() -> get_raw_sessions(U, S) end}).


http_notifications_port() ->
    ct:get_config({hosts, mim, http_notifications_port}).

http_notifications_host() ->
    "http://localhost:" ++ integer_to_list(http_notifications_port()).
