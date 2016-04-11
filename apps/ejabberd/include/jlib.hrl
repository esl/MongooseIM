%%%----------------------------------------------------------------------
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
-ifndef(MONGOOSEIM_JLIB_HRL).
-define(MONGOOSEIM_JLIB_HRL, true).

%% Load record definitions.
-include_lib("exml/include/exml.hrl").

-define(NS_CLIENT,       <<"jabber:client">>).
-define(NS_DISCO_ITEMS,  <<"http://jabber.org/protocol/disco#items">>).
-define(NS_DISCO_INFO,   <<"http://jabber.org/protocol/disco#info">>).
-define(NS_VCARD,        <<"vcard-temp">>).
-define(NS_VCARD_UPDATE, <<"vcard-temp:x:update">>).
-define(NS_AUTH,         <<"jabber:iq:auth">>).
-define(NS_AUTH_ERROR,   <<"jabber:iq:auth:error">>).
-define(NS_OAUTH_0, <<"urn:xmpp:oauth:0">>).% Defined by XEP-0235: Authorization Tokens.
-define(NS_OAUTH_ERRORS_0, <<"urn:xmpp:oauth:0:errors">>).%% Deferred : XEP-0235: Authorization Tokens.
-define(NS_AUTH_TOKEN,   <<"urn:xmpp:tmp:auth-token">>).
-define(NS_REGISTER,     <<"jabber:iq:register">>).
-define(NS_SEARCH,       <<"jabber:iq:search">>).
-define(NS_ROSTER,       <<"jabber:iq:roster">>).
-define(NS_ROSTER_VER,   <<"urn:xmpp:features:rosterver">>).
-define(NS_PRIVACY,      <<"jabber:iq:privacy">>).
-define(NS_BLOCKING,     <<"urn:xmpp:blocking">>).
-define(NS_PRIVATE,      <<"jabber:iq:private">>).
-define(NS_VERSION,      <<"jabber:iq:version">>).
-define(NS_TIME90,       <<"jabber:iq:time">>). % TODO: Remove once XEP-0090 is Obsolete
-define(NS_TIME,         <<"urn:xmpp:time">>).
-define(NS_LAST,         <<"jabber:iq:last">>).
-define(NS_XDATA,        <<"jabber:x:data">>).
-define(NS_IQDATA,       <<"jabber:iq:data">>).
-define(NS_DELAY91,      <<"jabber:x:delay">>). % TODO: Remove once XEP-0091 is Obsolete
-define(NS_DELAY,        <<"urn:xmpp:delay">>).
-define(NS_EXPIRE,       <<"jabber:x:expire">>).
-define(NS_EVENT,       <<"jabber:x:event">>).
-define(NS_CHATSTATES,  <<"http://jabber.org/protocol/chatstates">>).
-define(NS_STATS,       <<"http://jabber.org/protocol/stats">>).
-define(NS_MUC,         <<"http://jabber.org/protocol/muc">>).
-define(NS_MUC_S,       "http://jabber.org/protocol/muc").
-define(NS_MUC_USER,    <<"http://jabber.org/protocol/muc#user">>).
-define(NS_MUC_ADMIN,   <<"http://jabber.org/protocol/muc#admin">>).
-define(NS_MUC_OWNER,   <<"http://jabber.org/protocol/muc#owner">>).
-define(NS_MUC_UNIQUE,  <<"http://jabber.org/protocol/muc#unique">>).
-define(NS_MUC_REQUEST,  <<"http://jabber.org/protocol/muc#request">>).
-define(NS_PING,        <<"urn:xmpp:ping">>).
-define(NS_PUBSUB,      <<"http://jabber.org/protocol/pubsub">>).
-define(NS_PUBSUB_EVENT,<<"http://jabber.org/protocol/pubsub#event">>).
-define(NS_PUBSUB_OWNER,<<"http://jabber.org/protocol/pubsub#owner">>).
-define(NS_PUBSUB_NMI,  <<"http://jabber.org/protocol/pubsub#node-meta-info">>).
-define(NS_PUBSUB_ERRORS, <<"http://jabber.org/protocol/pubsub#errors">>).
-define(NS_PUBSUB_NODE_CONFIG,<<"http://jabber.org/protocol/pubsub#node_config">>).
-define(NS_PUBSUB_SUB_OPTIONS,<<"http://jabber.org/protocol/pubsub#subscribe_options">>).
-define(NS_PUBSUB_SUB_AUTH,<<"http://jabber.org/protocol/pubsub#subscribe_authorization">>).
-define(NS_PUBSUB_GET_PENDING, "http://jabber.org/protocol/pubsub#get-pending").
-define(NS_COMMANDS,    <<"http://jabber.org/protocol/commands">>).
-define(NS_BYTESTREAMS, <<"http://jabber.org/protocol/bytestreams">>).
-define(NS_ADMIN,       <<"http://jabber.org/protocol/admin">>).
-define(NS_SERVERINFO,  <<"http://jabber.org/network/serverinfo">>).
-define(NS_MAM,         <<"urn:xmpp:mam:tmp">>).
-define(NS_MAM_03,      <<"urn:xmpp:mam:0">>). % MAM 0.3
-define(NS_MAM_04,      <<"urn:xmpp:mam:1">>). % MAM 0.4.1 or 0.5

-define(NS_RSM,         <<"http://jabber.org/protocol/rsm">>).
-define(NS_EJABBERD_CONFIG,<<"ejabberd:config">>).

-define(NS_STREAM,       <<"http://etherx.jabber.org/streams">>).

-define(NS_STANZAS,     <<"urn:ietf:params:xml:ns:xmpp-stanzas">>).
-define(NS_STREAMS,     <<"urn:ietf:params:xml:ns:xmpp-streams">>).

-define(NS_TLS,        <<"urn:ietf:params:xml:ns:xmpp-tls">>).
-define(NS_TLS_BIN,         <<"urn:ietf:params:xml:ns:xmpp-tls">>).
-define(NS_SASL,       <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_SASL_BIN,        <<"urn:ietf:params:xml:ns:xmpp-sasl">>).
-define(NS_SESSION,      <<"urn:ietf:params:xml:ns:xmpp-session">>).
-define(NS_BIND,         <<"urn:ietf:params:xml:ns:xmpp-bind">>).

-define(NS_FEATURE_IQAUTH, <<"http://jabber.org/features/iq-auth">>).
-define(NS_FEATURE_IQREGISTER, <<"http://jabber.org/features/iq-register">>).
-define(NS_FEATURE_COMPRESS,<<"http://jabber.org/features/compress">>).
-define(NS_FEATURE_MSGOFFLINE,<<"msgoffline">>).

-define(NS_COMPRESS,     <<"http://jabber.org/protocol/compress">>).
-define(NS_COMPRESS_BIN, <<"http://jabber.org/protocol/compress">>).

-define(NS_CAPS,         <<"http://jabber.org/protocol/caps">>).
-define(NS_SHIM,         <<"http://jabber.org/protocol/shim">>).
-define(NS_ADDRESS,      <<"http://jabber.org/protocol/address">>).

-define(NS_HTTPBIND,     <<"http://jabber.org/protocol/httpbind">>).

-define(NS_STREAM_MGNT_3, <<"urn:xmpp:sm:3">>).

-define(NS_CSI, <<"urn:xmpp:csi:0">>).

%% Erlang Solutions custom extension - token based authentication
-define(NS_ESL_TOKEN_AUTH, <<"erlang-solutions.com:xmpp:token-auth:0">>).

-define(ERR_BAD_REQUEST,
        jlib:stanza_error(<<"400">>,<<"modify">>,<<"bad-request">>)).
-define(ERR_CONFLICT,
        jlib:stanza_error(<<"409">>,<<"cancel">>,<<"conflict">>)).
-define(ERR_FEATURE_NOT_IMPLEMENTED,
        jlib:stanza_error(<<"501">>,<<"cancel">>,<<"feature-not-implemented">>)).
-define(ERR_FORBIDDEN,
        jlib:stanza_error(<<"403">>,<<"auth">>,  <<"forbidden">>)).
-define(ERR_GONE,
        jlib:stanza_error(<<"302">>,<<"modify">>,<<"gone">>)).
-define(ERR_INTERNAL_SERVER_ERROR,
        jlib:stanza_error(<<"500">>,<<"wait">>,  <<"internal-server-error">>)).
-define(ERR_ITEM_NOT_FOUND,
        jlib:stanza_error(<<"404">>,<<"cancel">>,<<"item-not-found">>)).
-define(ERR_JID_MALFORMED,
        jlib:stanza_error(<<"400">>,<<"modify">>,<<"jid-malformed">>)).
-define(ERR_NOT_ACCEPTABLE,
        jlib:stanza_error(<<"406">>,<<"modify">>,<<"not-acceptable">>)).
-define(ERR_NOT_ALLOWED,
        jlib:stanza_error(<<"405">>,<<"cancel">>,<<"not-allowed">>)).
-define(ERR_NOT_AUTHORIZED,
        jlib:stanza_error(<<"401">>,<<"auth">>,  <<"not-authorized">>)).
-define(ERR_PAYMENT_REQUIRED,
        jlib:stanza_error(<<"402">>,<<"auth">>,  <<"payment-required">>)).
-define(ERR_RECIPIENT_UNAVAILABLE,
        jlib:stanza_error(<<"404">>,<<"wait">>,  <<"recipient-unavailable">>)).
-define(ERR_REDIRECT,
        jlib:stanza_error(<<"302">>,<<"modify">>,<<"redirect">>)).
-define(ERR_REGISTRATION_REQUIRED,
        jlib:stanza_error(<<"407">>,<<"auth">>,  <<"registration-required">>)).
-define(ERR_REMOTE_SERVER_NOT_FOUND,
        jlib:stanza_error(<<"404">>,<<"cancel">>,<<"remote-server-not-found">>)).
-define(ERR_REMOTE_SERVER_TIMEOUT,
        jlib:stanza_error(<<"504">>,<<"wait">>,  <<"remote-server-timeout">>)).
-define(ERR_RESOURCE_CONSTRAINT,
        jlib:stanza_error(<<"500">>,<<"wait">>,  <<"resource-constraint">>)).
-define(ERR_SERVICE_UNAVAILABLE,
        jlib:stanza_error(<<"503">>,<<"cancel">>,<<"service-unavailable">>)).
-define(ERR_SERVICE_UNAVAILABLE_WAIT,
        jlib:stanza_error(<<"502">>,<<"wait">>,<<"service-unavailable">>)).
-define(ERR_SUBSCRIPTION_REQUIRED,
        jlib:stanza_error(<<"407">>,<<"auth">>,  <<"subscription-required">>)).
-define(ERR_UNEXPECTED_REQUEST,
        jlib:stanza_error(<<"400">>,<<"wait">>,  <<"unexpected-request">>)).
-define(ERR_UNEXPECTED_REQUEST_CANCEL,
  jlib:stanza_error(<<"401">>,<<"cancel">>,<<"unexpected-request">>)).
%-define(ERR_,
%       jlib:stanza_error(<<"">>,<<"">>,<<"">>)).


-define(ERRT_BAD_REQUEST(Lang, Text),
        jlib:stanza_errort(<<"400">>,<<"modify">>,<<"bad-request">>, Lang, Text)).
-define(ERRT_CONFLICT(Lang, Text),
        jlib:stanza_errort(<<"409">>,<<"cancel">>,<<"conflict">>, Lang, Text)).
-define(ERRT_FEATURE_NOT_IMPLEMENTED(Lang, Text),
        jlib:stanza_errort(<<"501">>,<<"cancel">>,<<"feature-not-implemented">>, Lang, Text)).
-define(ERRT_FORBIDDEN(Lang, Text),
        jlib:stanza_errort(<<"403">>,<<"auth">>,  <<"forbidden">>, Lang, Text)).
-define(ERRT_GONE(Lang, Text),
        jlib:stanza_errort(<<"302">>,<<"modify">>,<<"gone">>, Lang, Text)).
-define(ERRT_INTERNAL_SERVER_ERROR(Lang, Text),
        jlib:stanza_errort(<<"500">>,<<"wait">>,  <<"internal-server-error">>, Lang, Text)).
-define(ERRT_ITEM_NOT_FOUND(Lang, Text),
        jlib:stanza_errort(<<"404">>,<<"cancel">>,<<"item-not-found">>, Lang, Text)).
-define(ERRT_JID_MALFORMED(Lang, Text),
        jlib:stanza_errort(<<"400">>,<<"modify">>,<<"jid-malformed">>, Lang, Text)).
-define(ERRT_NOT_ACCEPTABLE(Lang, Text),
        jlib:stanza_errort(<<"406">>,<<"modify">>,<<"not-acceptable">>, Lang, Text)).
-define(ERRT_NOT_ALLOWED(Lang, Text),
        jlib:stanza_errort(<<"405">>,<<"cancel">>,<<"not-allowed">>, Lang, Text)).
-define(ERRT_NOT_AUTHORIZED(Lang, Text),
        jlib:stanza_errort(<<"401">>,<<"auth">>,  <<"not-authorized">>, Lang, Text)).
-define(ERRT_PAYMENT_REQUIRED(Lang, Text),
        jlib:stanza_errort(<<"402">>,<<"auth">>,  <<"payment-required">>, Lang, Text)).
-define(ERRT_RECIPIENT_UNAVAILABLE(Lang, Text),
        jlib:stanza_errort(<<"404">>,<<"wait">>,  <<"recipient-unavailable">>, Lang, Text)).
-define(ERRT_REDIRECT(Lang, Text),
        jlib:stanza_errort(<<"302">>,<<"modify">>,<<"redirect">>, Lang, Text)).
-define(ERRT_REGISTRATION_REQUIRED(Lang, Text),
        jlib:stanza_errort(<<"407">>,<<"auth">>,  <<"registration-required">>, Lang, Text)).
-define(ERRT_REMOTE_SERVER_NOT_FOUND(Lang, Text),
        jlib:stanza_errort(<<"404">>,<<"cancel">>,<<"remote-server-not-found">>, Lang, Text)).
-define(ERRT_REMOTE_SERVER_TIMEOUT(Lang, Text),
        jlib:stanza_errort(<<"504">>,<<"wait">>,  <<"remote-server-timeout">>, Lang, Text)).
-define(ERRT_RESOURCE_CONSTRAINT(Lang, Text),
        jlib:stanza_errort(<<"500">>,<<"wait">>,  <<"resource-constraint">>, Lang, Text)).
-define(ERRT_SERVICE_UNAVAILABLE(Lang, Text),
        jlib:stanza_errort(<<"503">>,<<"cancel">>,<<"service-unavailable">>, Lang, Text)).
-define(ERRT_SUBSCRIPTION_REQUIRED(Lang, Text),
        jlib:stanza_errort(<<"407">>,<<"auth">>,  <<"subscription-required">>, Lang, Text)).
-define(ERRT_UNEXPECTED_REQUEST(Lang, Text),
        jlib:stanza_errort(<<"400">>,<<"wait">>,  <<"unexpected-request">>, Lang, Text)).

% Auth stanza errors
-define(ERR_AUTH_NO_RESOURCE_PROVIDED(Lang),
        ?ERRT_NOT_ACCEPTABLE(Lang,<<"No resource provided">>)).
-define(ERR_AUTH_BAD_RESOURCE_FORMAT(Lang),
        ?ERRT_NOT_ACCEPTABLE(Lang,<<"Illegal resource format">>)).
-define(ERR_AUTH_RESOURCE_CONFLICT(Lang),
        ?ERRT_CONFLICT(Lang,<<"Resource conflict">>)).


-define(SERR_BAD_FORMAT,
        jlib:stream_error(<<"bad-format">>)).
-define(SERR_BAD_NAMESPACE_PREFIX,
        jlib:stream_error(<<"bad-namespace-prefix">>)).
-define(SERR_CONFLICT,
        jlib:stream_error(<<"conflict">>)).
-define(SERR_CONNECTION_TIMEOUT,
        jlib:stream_error(<<"connection-timeout">>)).
-define(SERR_HOST_GONE,
        jlib:stream_error(<<"host-gone">>)).
-define(SERR_HOST_UNKNOWN,
        jlib:stream_error(<<"host-unknown">>)).
-define(SERR_IMPROPER_ADDRESSING,
        jlib:stream_error(<<"improper-addressing">>)).
-define(SERR_INTERNAL_SERVER_ERROR,
        jlib:stream_error(<<"internal-server-error">>)).
-define(SERR_INVALID_FROM,
        jlib:stream_error(<<"invalid-from">>)).
-define(SERR_INVALID_ID,
        jlib:stream_error(<<"invalid-id">>)).
-define(SERR_INVALID_NAMESPACE,
        jlib:stream_error(<<"invalid-namespace">>)).
-define(SERR_INVALID_XML,
        jlib:stream_error(<<"invalid-xml">>)).
-define(SERR_NOT_AUTHORIZED,
        jlib:stream_error(<<"not-authorized">>)).
-define(SERR_POLICY_VIOLATION,
        jlib:stream_error(<<"policy-violation">>)).
-define(SERR_REMOTE_CONNECTION_FAILED,
        jlib:stream_error(<<"remote-connection-failed">>)).
-define(SERR_RESOURSE_CONSTRAINT,
        jlib:stream_error(<<"resource-constraint">>)).
-define(SERR_RESTRICTED_XML,
        jlib:stream_error(<<"restricted-xml">>)).
% TODO: include hostname or IP
-define(SERR_SEE_OTHER_HOST,
        jlib:stream_error(<<"see-other-host">>)).
-define(SERR_SYSTEM_SHUTDOWN,
        jlib:stream_error(<<"system-shutdown">>)).
-define(SERR_UNSUPPORTED_ENCODING,
        jlib:stream_error(<<"unsupported-encoding">>)).
-define(SERR_UNSUPPORTED_STANZA_TYPE,
        jlib:stream_error(<<"unsupported-stanza-type">>)).
-define(SERR_UNSUPPORTED_VERSION,
        jlib:stream_error(<<"unsupported-version">>)).
-define(SERR_XML_NOT_WELL_FORMED,
        jlib:stream_error(<<"xml-not-well-formed">>)).
%-define(SERR_,
%       jlib:stream_error(<<"">>)).

-define(SERRT_BAD_FORMAT(Lang, Text),
        jlib:stream_errort(<<"bad-format">>, Lang, Text)).
-define(SERRT_BAD_NAMESPACE_PREFIX(Lang, Text),
        jlib:stream_errort(<<"bad-namespace-prefix">>, Lang, Text)).
-define(SERRT_CONFLICT(Lang, Text),
        jlib:stream_errort(<<"conflict">>, Lang, Text)).
-define(SERRT_CONNECTION_TIMEOUT(Lang, Text),
        jlib:stream_errort(<<"connection-timeout">>, Lang, Text)).
-define(SERRT_HOST_GONE(Lang, Text),
        jlib:stream_errort(<<"host-gone">>, Lang, Text)).
-define(SERRT_HOST_UNKNOWN(Lang, Text),
        jlib:stream_errort(<<"host-unknown">>, Lang, Text)).
-define(SERRT_IMPROPER_ADDRESSING(Lang, Text),
        jlib:stream_errort(<<"improper-addressing">>, Lang, Text)).
-define(SERRT_INTERNAL_SERVER_ERROR(Lang, Text),
        jlib:stream_errort(<<"internal-server-error">>, Lang, Text)).
-define(SERRT_INVALID_FROM(Lang, Text),
        jlib:stream_errort(<<"invalid-from">>, Lang, Text)).
-define(SERRT_INVALID_ID(Lang, Text),
        jlib:stream_errort(<<"invalid-id">>, Lang, Text)).
-define(SERRT_INVALID_NAMESPACE(Lang, Text),
        jlib:stream_errort(<<"invalid-namespace">>, Lang, Text)).
-define(SERRT_INVALID_XML(Lang, Text),
        jlib:stream_errort(<<"invalid-xml">>, Lang, Text)).
-define(SERRT_NOT_AUTHORIZED(Lang, Text),
        jlib:stream_errort(<<"not-authorized">>, Lang, Text)).
-define(SERRT_POLICY_VIOLATION(Lang, Text),
        jlib:stream_errort(<<"policy-violation">>, Lang, Text)).
-define(SERRT_REMOTE_CONNECTION_FAILED(Lang, Text),
        jlib:stream_errort(<<"remote-connection-failed">>, Lang, Text)).
-define(SERRT_RESOURSE_CONSTRAINT(Lang, Text),
        jlib:stream_errort(<<"resource-constraint">>, Lang, Text)).
-define(SERRT_RESTRICTED_XML(Lang, Text),
        jlib:stream_errort(<<"restricted-xml">>, Lang, Text)).
% TODO: include hostname or IP
-define(SERRT_SEE_OTHER_HOST(Lang, Text),
        jlib:stream_errort(<<"see-other-host">>, Lang, Text)).
-define(SERRT_SYSTEM_SHUTDOWN(Lang, Text),
        jlib:stream_errort(<<"system-shutdown">>, Lang, Text)).
-define(SERRT_UNSUPPORTED_ENCODING(Lang, Text),
        jlib:stream_errort(<<"unsupported-encoding">>, Lang, Text)).
-define(SERRT_UNSUPPORTED_STANZA_TYPE(Lang, Text),
        jlib:stream_errort(<<"unsupported-stanza-type">>, Lang, Text)).
-define(SERRT_UNSUPPORTED_VERSION(Lang, Text),
        jlib:stream_errort(<<"unsupported-version">>, Lang, Text)).
-define(SERRT_XML_NOT_WELL_FORMED(Lang, Text),
        jlib:stream_errort(<<"xml-not-well-formed">>, Lang, Text)).
%-define(SERRT_(Lang, Text),
%       jlib:stream_errort(<<"">>, Lang, Text)).


-record(jid, {user = <<>>      :: ejabberd:user(),
              server = <<>>    :: ejabberd:server(),
              resource = <<>>  :: ejabberd:resource(),
              luser = <<>>     :: ejabberd:luser(),
              lserver = <<>>   :: ejabberd:lserver(),
              lresource = <<>> :: ejabberd:lresource()
             }).

-record(iq, {id = <<>>    :: binary(),
             type         :: atom(),
             xmlns = <<>> :: binary(),
             lang = <<>>  :: ejabberd:lang(),
             sub_el       :: [jlib:xmlel()] | jlib:xmlel()
            }).

-record(rsm_in, {max         :: non_neg_integer() | undefined | error,
                 direction   :: before | aft | undefined,
                %% id is empty, if cdata does not exist
                 id          :: binary() | integer() | undefined,
                 index       :: non_neg_integer() | undefined | error
                }).

-record(mam_borders, {after_id  :: non_neg_integer() | undefined,
                      before_id :: non_neg_integer() | undefined,
                      from_id   :: non_neg_integer() | undefined,
                      to_id     :: non_neg_integer() | undefined
                     }).

-record(rsm_out, {count :: pos_integer(),
                  index :: pos_integer(),
                  first :: binary(),
                  last :: binary()
                 }).

-type iq() :: #iq{}.
-type jid() :: #jid{}.

-endif.
