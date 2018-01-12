%%%-------------------------------------------------------------------
%%% @author Alex
%%% @doc
%%% The functions in this module were previously defined as macros in jlib.hrl
%%% @end
%%% Created : 10. Jan 2018
%%%-------------------------------------------------------------------
-module(mongoose_xmpp_errors).
-author("Alex").


-compile([export_all]).
-include("mongoose_ns.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_request() ->
    jlib:stanza_error(<<"400">>,<<"modify">>,<<"bad-request">>).

conflict() ->
    jlib:stanza_error(<<"409">>,<<"cancel">>,<<"conflict">>).

feature_not_implemented() ->
    jlib:stanza_error(<<"501">>,<<"cancel">>,<<"feature-not-implemented">>).

forbidden() ->
    jlib:stanza_error(<<"403">>,<<"auth">>,  <<"forbidden">>).

gone() ->
    jlib:stanza_error(<<"302">>,<<"modify">>,<<"gone">>).

internal_server_error() ->
    jlib:stanza_error(<<"500">>,<<"wait">>,  <<"internal-server-error">>).

item_not_found() ->
    jlib:stanza_error(<<"404">>,<<"cancel">>,<<"item-not-found">>).

jid_malformed() ->
    jlib:stanza_error(<<"400">>,<<"modify">>,<<"jid-malformed">>).

not_acceptable() ->
    jlib:stanza_error(<<"406">>,<<"modify">>,<<"not-acceptable">>).

not_acceptable_cancel() ->
    jlib:stanza_error(<<"406">>,<<"cancel">>,<<"not-acceptable">>).

not_acceptable_blocked() ->
    jlib:stanza_error(<<"406">>,<<"cancel">>,<<"not-acceptable">>, <<"blocked">>, ?NS_BLOCKING_ERRORS).

not_allowed() ->
    jlib:stanza_error(<<"405">>,<<"cancel">>,<<"not-allowed">>).

not_authorized() ->
    jlib:stanza_error(<<"401">>,<<"auth">>,  <<"not-authorized">>).

payment_required() ->
    jlib:stanza_error(<<"402">>,<<"auth">>,  <<"payment-required">>).

recipient_unavailable() ->
    jlib:stanza_error(<<"404">>,<<"wait">>,  <<"recipient-unavailable">>).

redirect() ->
    jlib:stanza_error(<<"302">>,<<"modify">>,<<"redirect">>).

registration_required() ->
    jlib:stanza_error(<<"407">>,<<"auth">>,  <<"registration-required">>).

remote_server_not_found() ->
    jlib:stanza_error(<<"404">>,<<"cancel">>,<<"remote-server-not-found">>).

remote_server_timeout() ->
    jlib:stanza_error(<<"504">>,<<"wait">>,  <<"remote-server-timeout">>).

resource_constraint() ->
    jlib:stanza_error(<<"500">>,<<"wait">>,  <<"resource-constraint">>).

service_unavailable() ->
    jlib:stanza_error(<<"503">>,<<"cancel">>,<<"service-unavailable">>).

service_unavailable_wait() ->
    jlib:stanza_error(<<"502">>,<<"wait">>,<<"service-unavailable">>).

subscription_required() ->
    jlib:stanza_error(<<"407">>,<<"auth">>,  <<"subscription-required">>).

unexpected_request() ->
    jlib:stanza_error(<<"400">>,<<"wait">>,  <<"unexpected-request">>).

unexpected_request_cancel() ->
    jlib:stanza_error(<<"401">>,<<"cancel">>,<<"unexpected-request">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Text Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_request(Lang, Text) ->
    jlib:stanza_errort(<<"400">>,<<"modify">>,<<"bad-request">>, Lang, Text).

conflict(Lang, Text) ->
    jlib:stanza_errort(<<"409">>,<<"cancel">>,<<"conflict">>, Lang, Text).

feature_not_implemented(Lang, Text) ->
    jlib:stanza_errort(<<"501">>,<<"cancel">>,<<"feature-not-implemented">>, Lang, Text).

forbidden(Lang, Text) ->
    jlib:stanza_errort(<<"403">>,<<"auth">>,  <<"forbidden">>, Lang, Text).

gone(Lang, Text) ->
    jlib:stanza_errort(<<"302">>,<<"modify">>,<<"gone">>, Lang, Text).

internal_server_error(Lang, Text) ->
    jlib:stanza_errort(<<"500">>,<<"wait">>,  <<"internal-server-error">>, Lang, Text).

item_not_found(Lang, Text) ->
    jlib:stanza_errort(<<"404">>,<<"cancel">>,<<"item-not-found">>, Lang, Text).

jid_malformed(Lang, Text) ->
    jlib:stanza_errort(<<"400">>,<<"modify">>,<<"jid-malformed">>, Lang, Text).

not_acceptable(Lang, Text) ->
    jlib:stanza_errort(<<"406">>,<<"modify">>,<<"not-acceptable">>, Lang, Text).

not_allowed(Lang, Text) ->
    jlib:stanza_errort(<<"405">>,<<"cancel">>,<<"not-allowed">>, Lang, Text).

not_authorized(Lang, Text) ->
    jlib:stanza_errort(<<"401">>,<<"auth">>,  <<"not-authorized">>, Lang, Text).

payment_required(Lang, Text) ->
    jlib:stanza_errort(<<"402">>,<<"auth">>,  <<"payment-required">>, Lang, Text).

recipient_unavailable(Lang, Text) ->
    jlib:stanza_errort(<<"404">>,<<"wait">>,  <<"recipient-unavailable">>, Lang, Text).

redirect(Lang, Text) ->
    jlib:stanza_errort(<<"302">>,<<"modify">>,<<"redirect">>, Lang, Text).

registration_required(Lang, Text) ->
    jlib:stanza_errort(<<"407">>,<<"auth">>,  <<"registration-required">>, Lang, Text).

remote_server_not_found(Lang, Text) ->
    jlib:stanza_errort(<<"404">>,<<"cancel">>,<<"remote-server-not-found">>, Lang, Text).

remote_server_timeout(Lang, Text) ->
    jlib:stanza_errort(<<"504">>,<<"wait">>,  <<"remote-server-timeout">>, Lang, Text).

resource_constraint(Lang, Text) ->
    jlib:stanza_errort(<<"500">>,<<"wait">>,  <<"resource-constraint">>, Lang, Text).

service_unavailable(Lang, Text) ->
    jlib:stanza_errort(<<"503">>,<<"cancel">>,<<"service-unavailable">>, Lang, Text).

subscription_required(Lang, Text) ->
    jlib:stanza_errort(<<"407">>,<<"auth">>,  <<"subscription-required">>, Lang, Text).

unexpected_request(Lang, Text) ->
    jlib:stanza_errort(<<"400">>,<<"wait">>,  <<"unexpected-request">>, Lang, Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auth Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auth_no_resource_provided(Lang) ->
    not_acceptable(Lang,<<"No resource provided">>).

auth_bad_resource_format(Lang) ->
    not_acceptable(Lang,<<"Illegal resource format">>).

auth_resource_conflict(Lang) ->
    conflict(Lang, <<"Resource conflict">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stream Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_format() ->
    jlib:stream_error(<<"bad-format">>).

bad_namespace_prefix() ->
    jlib:stream_error(<<"bad-namespace-prefix">>).

stream_conflict() ->
    jlib:stream_error(<<"conflict">>).

connection_timeout() ->
    jlib:stream_error(<<"connection-timeout">>).

host_gone() ->
    jlib:stream_error(<<"host-gone">>).

host_unknown() ->
    jlib:stream_error(<<"host-unknown">>).

improper_addressing() ->
    jlib:stream_error(<<"improper-addressing">>).

stream_internal_server_error() ->
    jlib:stream_error(<<"internal-server-error">>).

invalid_from() ->
    jlib:stream_error(<<"invalid-from">>).

invalid_id() ->
    jlib:stream_error(<<"invalid-id">>).

invalid_namespace() ->
    jlib:stream_error(<<"invalid-namespace">>).

invalid_xml() ->
    jlib:stream_error(<<"invalid-xml">>).

stream_not_authorized() ->
    jlib:stream_error(<<"not-authorized">>).

policy_violation() ->
    jlib:stream_error(<<"policy-violation">>).

remote_connection_failed() ->
    jlib:stream_error(<<"remote-connection-failed">>).

stream_resource_constraint() ->
    jlib:stream_error(<<"resource-constraint">>).

restricted_xml() ->
    jlib:stream_error(<<"restricted-xml">>).

% TODO: include hostname or IP
see_other_host() ->
    jlib:stream_error(<<"see-other-host">>).

system_shutdown() ->
    jlib:stream_error(<<"system-shutdown">>).

unsupported_encoding() ->
    jlib:stream_error(<<"unsupported-encoding">>).

unsupported_stanza_type() ->
    jlib:stream_error(<<"unsupported-stanza-type">>).

unsupported_version() ->
    jlib:stream_error(<<"unsupported-version">>).

xml_not_well_formed() ->
    jlib:stream_error(<<"xml-not-well-formed">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Text Stream Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_format(Lang, Text) ->
    jlib:stream_errort(<<"bad-format">>, Lang, Text).

bad_namespace_prefix(Lang, Text) ->
    jlib:stream_errort(<<"bad-namespace-prefix">>, Lang, Text).

stream_conflict(Lang, Text) ->
    jlib:stream_errort(<<"conflict">>, Lang, Text).

connection_timeout(Lang, Text) ->
    jlib:stream_errort(<<"connection-timeout">>, Lang, Text).

host_gone(Lang, Text) ->
    jlib:stream_errort(<<"host-gone">>, Lang, Text).

host_unknown(Lang, Text) ->
    jlib:stream_errort(<<"host-unknown">>, Lang, Text).

improper_addressing(Lang, Text) ->
    jlib:stream_errort(<<"improper-addressing">>, Lang, Text).

stream_internal_server_error(Lang, Text) ->
    jlib:stream_errort(<<"internal-server-error">>, Lang, Text).

invalid_from(Lang, Text) ->
    jlib:stream_errort(<<"invalid-from">>, Lang, Text).

invalid_id(Lang, Text) ->
    jlib:stream_errort(<<"invalid-id">>, Lang, Text).

invalid_namespace(Lang, Text) ->
    jlib:stream_errort(<<"invalid-namespace">>, Lang, Text).

invalid_xml(Lang, Text) ->
    jlib:stream_errort(<<"invalid-xml">>, Lang, Text).

stream_not_authorized(Lang, Text) ->
    jlib:stream_errort(<<"not-authorized">>, Lang, Text).

policy_violation(Lang, Text) ->
    jlib:stream_errort(<<"policy-violation">>, Lang, Text).

remote_connection_failed(Lang, Text) ->
    jlib:stream_errort(<<"remote-connection-failed">>, Lang, Text).

stream_resource_constraint(Lang, Text) ->
    jlib:stream_errort(<<"resource-constraint">>, Lang, Text).

restricted_xml(Lang, Text) ->
    jlib:stream_errort(<<"restricted-xml">>, Lang, Text).

% TODO: include hostname or IP
see_other_host(Lang, Text) ->
    jlib:stream_errort(<<"see-other-host">>, Lang, Text).

system_shutdown(Lang, Text) ->
    jlib:stream_errort(<<"system-shutdown">>, Lang, Text).

unsupported_encoding(Lang, Text) ->
    jlib:stream_errort(<<"unsupported-encoding">>, Lang, Text).

unsupported_stanza_type(Lang, Text) ->
    jlib:stream_errort(<<"unsupported-stanza-type">>, Lang, Text).

unsupported_version(Lang, Text) ->
    jlib:stream_errort(<<"unsupported-version">>, Lang, Text).

xml_not_well_formed(Lang, Text) ->
    jlib:stream_errort(<<"xml-not-well-formed">>, Lang, Text).

xml_not_well_formed_bin() ->
    exml:to_binary(xml_not_well_formed()).

bad_format_bin() ->
    exml:to_binary(bad_format()).

