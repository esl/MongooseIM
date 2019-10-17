-ifndef(MONGOOSEIM_NS_HRL).
-define(MONGOOSEIM_NS_HRL, true).

-define(NS_CLIENT,              <<"jabber:client">>).
-define(NS_CONFERENCE,          <<"jabber:x:conference">>).
-define(NS_DISCO_ITEMS,         <<"http://jabber.org/protocol/disco#items">>).
-define(NS_DISCO_INFO,          <<"http://jabber.org/protocol/disco#info">>).
-define(NS_VCARD,               <<"vcard-temp">>).
-define(NS_VCARD_UPDATE,        <<"vcard-temp:x:update">>).
-define(NS_OAUTH_0,             <<"urn:xmpp:oauth:0">>).% Defined by XEP-0235: Authorization Tokens.
-define(NS_OAUTH_ERRORS_0,      <<"urn:xmpp:oauth:0:errors">>).%% Deferred : XEP-0235: Authorization Tokens.
-define(NS_AUTH_TOKEN,          <<"urn:xmpp:tmp:auth-token">>).
-define(NS_REGISTER,            <<"jabber:iq:register">>).
-define(NS_SEARCH,              <<"jabber:iq:search">>).
-define(NS_ROSTER,              <<"jabber:iq:roster">>).
-define(NS_ROSTER_VER,          <<"urn:xmpp:features:rosterver">>).
-define(NS_PRIVACY,             <<"jabber:iq:privacy">>).
-define(NS_BLOCKING,            <<"urn:xmpp:blocking">>).
-define(NS_BLOCKING_ERRORS,     <<"urn:xmpp:blocking:errors">>).
-define(NS_FORWARD,             <<"urn:xmpp:forward:0">>).
-define(NS_PRIVATE,             <<"jabber:iq:private">>).
-define(NS_VERSION,             <<"jabber:iq:version">>).
-define(NS_TIME90,              <<"jabber:iq:time">>). % TODO: Remove once XEP-0090 is Obsolete
-define(NS_TIME,                <<"urn:xmpp:time">>).
-define(NS_LAST,                <<"jabber:iq:last">>).
-define(NS_XDATA,               <<"jabber:x:data">>).
-define(NS_IQDATA,              <<"jabber:iq:data">>).
-define(NS_DELAY91,             <<"jabber:x:delay">>). % TODO: Remove once XEP-0091 is Obsolete
-define(NS_DELAY,               <<"urn:xmpp:delay">>).
-define(NS_EXPIRE,              <<"jabber:x:expire">>).
-define(NS_EVENT,               <<"jabber:x:event">>).
-define(NS_CHATSTATES,          <<"http://jabber.org/protocol/chatstates">>).
-define(NS_STATS,               <<"http://jabber.org/protocol/stats">>).
-define(NS_MUC,                 <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_S,               "http://jabber.org/protocol/muc").
-define(NS_MUC_USER,            <<"http://jabber.org/protocol/muc#user">>).
-define(NS_MUC_ADMIN,           <<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER,           <<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE,          <<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_REQUEST,         <<"http://jabber.org/protocol/muc#request">>).
-define(NS_MUC_CONFIG,          <<"http://jabber.org/protocol/muc#roomconfig">>).
-define(NS_PING,                <<"urn:xmpp:ping">>).
-define(NS_PUBSUB,              <<"http://jabber.org/protocol/pubsub">>).
-define(NS_PUBSUB_EVENT,        <<"http://jabber.org/protocol/pubsub#event">>).
-define(NS_PUBSUB_OWNER,        <<"http://jabber.org/protocol/pubsub#owner">>).
-define(NS_PUBSUB_NMI,          <<"http://jabber.org/protocol/pubsub#node-meta-info">>).
-define(NS_PUBSUB_ERRORS,       <<"http://jabber.org/protocol/pubsub#errors">>).
-define(NS_PUBSUB_NODE_CONFIG,  <<"http://jabber.org/protocol/pubsub#node_config">>).
-define(NS_PUBSUB_SUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_PUB_OPTIONS,  <<"http://jabber.org/protocol/pubsub#publish-options">>).
-define(NS_PUBSUB_SUB_AUTH,     <<"http://jabber.org/protocol/pubsub#subscribe_authorization">>).
-define(NS_PUBSUB_GET_PENDING,  <<"http://jabber.org/protocol/pubsub#get-pending">>).
-define(NS_COMMANDS,            <<"http://jabber.org/protocol/commands">>).
-define(NS_BYTESTREAMS,         <<"http://jabber.org/protocol/bytestreams">>).
-define(NS_ADMIN,               <<"http://jabber.org/protocol/admin">>).
-define(NS_SERVERINFO,          <<"http://jabber.org/network/serverinfo">>).
-define(NS_MAM_04,              <<"urn:xmpp:mam:1">>). % MAM 0.4.1 or 0.5
-define(NS_MAM_06,              <<"urn:xmpp:mam:2">>).  % MAM 0.6
-define(NS_HTTP_UPLOAD_025,     <<"urn:xmpp:http:upload">>).
-define(NS_HTTP_UPLOAD_030,     <<"urn:xmpp:http:upload:0">>).
-define(NS_PUSH,                <<"urn:xmpp:push:0">>). % Push Notifications v0.2.1
-define(NS_STANZAID,            <<"urn:xmpp:sid:0">>).

-define(NS_RSM,                 <<"http://jabber.org/protocol/rsm">>).
-define(NS_EJABBERD_CONFIG,     <<"ejabberd:config">>).

-define(NS_STREAM,              <<"http://etherx.jabber.org/streams">>).

-define(NS_STANZAS,             <<"urn:ietf:params:xml:ns:xmpp-stanzas">>).
-define(NS_STREAMS,             <<"urn:ietf:params:xml:ns:xmpp-streams">>).

-define(NS_TLS,                 <<"urn:ietf:params:xml:ns:xmpp-tls">>).
-define(NS_SASL,                <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_SESSION,             <<"urn:ietf:params:xml:ns:xmpp-session">>).
-define(NS_BIND,                <<"urn:ietf:params:xml:ns:xmpp-bind">>).

-define(NS_FEATURE_IQAUTH,      <<"http://jabber.org/features/iq-auth">>).
-define(NS_FEATURE_IQREGISTER,  <<"http://jabber.org/features/iq-register">>).
-define(NS_FEATURE_COMPRESS,    <<"http://jabber.org/features/compress">>).
-define(NS_FEATURE_MSGOFFLINE,  <<"msgoffline">>).

-define(NS_COMPRESS,            <<"http://jabber.org/protocol/compress">>).

-define(NS_CAPS,                <<"http://jabber.org/protocol/caps">>).
-define(NS_SHIM,                <<"http://jabber.org/protocol/shim">>).
-define(NS_ADDRESS,             <<"http://jabber.org/protocol/address">>).

-define(NS_HTTPBIND,            <<"http://jabber.org/protocol/httpbind">>).

-define(NS_STREAM_MGNT_3,       <<"urn:xmpp:sm:3">>).

-define(NS_CSI,                 <<"urn:xmpp:csi:0">>).

-define(NS_CHAT_MARKERS,        <<"urn:xmpp:chat-markers:0">>).

-define(JINGLE_NS, <<"urn:xmpp:jingle:1">>).

%% Erlang Solutions custom extension - token based authentication
-define(NS_ESL_TOKEN_AUTH,      <<"erlang-solutions.com:xmpp:token-auth:0">>).

%% Erlang Solutions custom extension - inbox feature
-define(NS_ESL_INBOX,      <<"erlang-solutions.com:xmpp:inbox:0">>).
-define(NS_ESL_INBOX_CONVERSATION, <<"erlang-solutions.com:xmpp:inbox:0#conversation">>).

-endif.
