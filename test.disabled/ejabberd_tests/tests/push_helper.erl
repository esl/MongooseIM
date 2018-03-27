-module(push_helper).
-include_lib("exml/include/exml.hrl").

-include("push_helper.hrl").

-export([enable_stanza/2, enable_stanza/3, enable_stanza/4,
         disable_stanza/1, disable_stanza/2,
         make_form/1, maybe_form/2]).

enable_stanza(JID, Node) ->
    enable_stanza(JID, Node, undefined).

enable_stanza(JID, Node, FormFields) ->
    enable_stanza(JID, Node, FormFields, ?NS_PUBSUB_PUB_OPTIONS).

enable_stanza(JID, Node, FormFields, FormType) ->
    escalus_stanza:iq(<<"set">>, [#xmlel{name = <<"enable">>, attrs = [
                                                                       {<<"xmlns">>, <<"urn:xmpp:push:0">>},
                                                                       {<<"jid">>, JID},
                                                                       {<<"node">>, Node}
                                                                      ], children = maybe_form(FormFields, FormType)}]).


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
