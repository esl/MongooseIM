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
-include("mongoose_ns.hrl").

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

-type iq() :: #iq{}.
-type jid() :: #jid{}.
-type ljid() :: {ejabberd:luser(), ejabberd:lserver(), ejabberd:lresource()}.

-type xmlel() :: #xmlel{}.

-endif.
