-module(mongoose_xmpp_errors).

-compile([export_all, nowarn_export_all]).
-include("mongoose_ns.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type stanza_error() ::
    bad_request | conflict | feature_not_implemented | forbidden | gone
    | internal_server_error | item_not_found | jid_malformed | not_acceptable
    | not_acceptable_cancel | not_acceptable_blocked | not_allowed | not_authorized
    | payment_required | recipient_unavailable | redirect | registration_required
    | remote_server_not_found | remote_server_timeout | resource_constraint
    | service_unavailable | service_unavailable_wait | subscription_required
    | unexpected_request | unexpected_request_cancel.

-type stream_error() ::
    bad_format | bad_namespace_prefix | connection_timeout | encryption_required
    | host_gone | host_unknown | improper_addressing | invalid_from | invalid_id
    | invalid_namespace | invalid_xml | malformed_request | policy_violation
    | remote_connection_failed | restricted_xml | see_other_host | stream_conflict
    | stream_internal_server_error | stream_not_authorized | stream_resource_constraint
    | system_shutdown | undefined_condition | unsupported_encoding | unsupported_stanza_type
    | unsupported_version | xml_not_well_formed.

-export_type([stanza_error/0, stream_error/0]).

bad_request() ->
    jlib:stanza_error(<<"400">>, <<"modify">>, <<"bad-request">>).

conflict() ->
    jlib:stanza_error(<<"409">>, <<"cancel">>, <<"conflict">>).

feature_not_implemented() ->
    jlib:stanza_error(<<"501">>, <<"cancel">>, <<"feature-not-implemented">>).

forbidden() ->
    jlib:stanza_error(<<"403">>, <<"auth">>, <<"forbidden">>).

gone() ->
    jlib:stanza_error(<<"302">>, <<"modify">>, <<"gone">>).

internal_server_error() ->
    jlib:stanza_error(<<"500">>, <<"wait">>, <<"internal-server-error">>).

item_not_found() ->
    jlib:stanza_error(<<"404">>, <<"cancel">>, <<"item-not-found">>).

jid_malformed() ->
    jlib:stanza_error(<<"400">>, <<"modify">>, <<"jid-malformed">>).

not_acceptable() ->
    jlib:stanza_error(<<"406">>, <<"modify">>, <<"not-acceptable">>).

not_acceptable_cancel() ->
    jlib:stanza_error(<<"406">>, <<"cancel">>, <<"not-acceptable">>).

not_acceptable_blocked() ->
    jlib:stanza_error(<<"406">>, <<"cancel">>, <<"not-acceptable">>, <<"blocked">>, ?NS_BLOCKING_ERRORS).

not_allowed() ->
    jlib:stanza_error(<<"405">>, <<"cancel">>, <<"not-allowed">>).

not_authorized() ->
    jlib:stanza_error(<<"401">>, <<"auth">>, <<"not-authorized">>).

payment_required() ->
    jlib:stanza_error(<<"402">>, <<"auth">>, <<"payment-required">>).

recipient_unavailable() ->
    jlib:stanza_error(<<"404">>, <<"wait">>, <<"recipient-unavailable">>).

redirect() ->
    jlib:stanza_error(<<"302">>, <<"modify">>, <<"redirect">>).

registration_required() ->
    jlib:stanza_error(<<"407">>, <<"auth">>, <<"registration-required">>).

remote_server_not_found() ->
    jlib:stanza_error(<<"404">>, <<"cancel">>, <<"remote-server-not-found">>).

remote_server_timeout() ->
    jlib:stanza_error(<<"504">>, <<"wait">>, <<"remote-server-timeout">>).

resource_constraint() ->
    jlib:stanza_error(<<"500">>, <<"wait">>, <<"resource-constraint">>).

service_unavailable() ->
    jlib:stanza_error(<<"503">>, <<"cancel">>, <<"service-unavailable">>).

service_unavailable_wait() ->
    jlib:stanza_error(<<"502">>, <<"wait">>, <<"service-unavailable">>).

subscription_required() ->
    jlib:stanza_error(<<"407">>, <<"auth">>, <<"subscription-required">>).

unexpected_request() ->
    jlib:stanza_error(<<"400">>, <<"wait">>, <<"unexpected-request">>).

unexpected_request_cancel() ->
    jlib:stanza_error(<<"401">>, <<"cancel">>, <<"unexpected-request">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Text Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_request(Text) ->
    jlib:stanza_errort(<<"400">>, <<"modify">>, <<"bad-request">>, Text).

conflict(Text) ->
    jlib:stanza_errort(<<"409">>, <<"cancel">>, <<"conflict">>, Text).

feature_not_implemented(Text) ->
    jlib:stanza_errort(<<"501">>, <<"cancel">>, <<"feature-not-implemented">>, Text).

forbidden(Text) ->
    jlib:stanza_errort(<<"403">>, <<"auth">>, <<"forbidden">>, Text).

gone(Text) ->
    jlib:stanza_errort(<<"302">>, <<"modify">>, <<"gone">>, Text).

internal_server_error(Text) ->
    jlib:stanza_errort(<<"500">>, <<"wait">>, <<"internal-server-error">>, Text).

item_not_found(Text) ->
    jlib:stanza_errort(<<"404">>, <<"cancel">>, <<"item-not-found">>, Text).

jid_malformed(Text) ->
    jlib:stanza_errort(<<"400">>, <<"modify">>, <<"jid-malformed">>, Text).

not_acceptable(Text) ->
    jlib:stanza_errort(<<"406">>, <<"modify">>, <<"not-acceptable">>, Text).

not_allowed(Text) ->
    jlib:stanza_errort(<<"405">>, <<"cancel">>, <<"not-allowed">>, Text).

not_authorized(Text) ->
    jlib:stanza_errort(<<"401">>, <<"auth">>, <<"not-authorized">>, Text).

payment_required(Text) ->
    jlib:stanza_errort(<<"402">>, <<"auth">>, <<"payment-required">>, Text).

recipient_unavailable(Text) ->
    jlib:stanza_errort(<<"404">>, <<"wait">>, <<"recipient-unavailable">>, Text).

redirect(Text) ->
    jlib:stanza_errort(<<"302">>, <<"modify">>, <<"redirect">>, Text).

registration_required(Text) ->
    jlib:stanza_errort(<<"407">>, <<"auth">>, <<"registration-required">>, Text).

remote_server_not_found(Text) ->
    jlib:stanza_errort(<<"404">>, <<"cancel">>, <<"remote-server-not-found">>, Text).

remote_server_timeout(Text) ->
    jlib:stanza_errort(<<"504">>, <<"wait">>, <<"remote-server-timeout">>, Text).

resource_constraint(Text) ->
    jlib:stanza_errort(<<"500">>, <<"wait">>, <<"resource-constraint">>, Text).

service_unavailable(Text) ->
    jlib:stanza_errort(<<"503">>, <<"cancel">>, <<"service-unavailable">>, Text).

subscription_required(Text) ->
    jlib:stanza_errort(<<"407">>, <<"auth">>, <<"subscription-required">>, Text).

unexpected_request(Text) ->
    jlib:stanza_errort(<<"400">>, <<"wait">>, <<"unexpected-request">>, Text).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auth Stanza Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auth_no_resource_provided() ->
    not_acceptable(<<"No resource provided">>).

auth_bad_resource_format() ->
    not_acceptable(<<"Illegal resource format">>).

auth_resource_conflict() ->
    conflict(<<"Resource conflict">>).

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

encryption_required() ->
    jlib:stream_error(<<"encryption-required">>).

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

malformed_request() ->
    jlib:stream_error(<<"malformed-request">>).

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

undefined_condition() ->
    jlib:stream_error(<<"undefined-condition">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Text Stream Errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bad_format(Text) ->
    jlib:stream_errort(<<"bad-format">>, <<"en">>, Text).

bad_namespace_prefix(Text) ->
    jlib:stream_errort(<<"bad-namespace-prefix">>, <<"en">>, Text).

stream_conflict(Text) ->
    jlib:stream_errort(<<"conflict">>, <<"en">>, Text).

connection_timeout(Text) ->
    jlib:stream_errort(<<"connection-timeout">>, <<"en">>, Text).

encryption_required(Text) ->
    jlib:stream_errort(<<"encryption-required">>, <<"en">>, Text).

host_gone(Text) ->
    jlib:stream_errort(<<"host-gone">>, <<"en">>, Text).

host_unknown(Text) ->
    jlib:stream_errort(<<"host-unknown">>, <<"en">>, Text).

improper_addressing(Text) ->
    jlib:stream_errort(<<"improper-addressing">>, <<"en">>, Text).

stream_internal_server_error(Text) ->
    jlib:stream_errort(<<"internal-server-error">>, <<"en">>, Text).

invalid_from(Text) ->
    jlib:stream_errort(<<"invalid-from">>, <<"en">>, Text).

invalid_id(Text) ->
    jlib:stream_errort(<<"invalid-id">>, <<"en">>, Text).

invalid_namespace(Text) ->
    jlib:stream_errort(<<"invalid-namespace">>, <<"en">>, Text).

invalid_xml(Text) ->
    jlib:stream_errort(<<"invalid-xml">>, <<"en">>, Text).

malformed_request(Text) ->
    jlib:stream_errort(<<"malformed-request">>, <<"en">>, Text).

stream_not_authorized(Text) ->
    jlib:stream_errort(<<"not-authorized">>, <<"en">>, Text).

policy_violation(Text) ->
    jlib:stream_errort(<<"policy-violation">>, <<"en">>, Text).

remote_connection_failed(Text) ->
    jlib:stream_errort(<<"remote-connection-failed">>, <<"en">>, Text).

stream_resource_constraint(Text) ->
    jlib:stream_errort(<<"resource-constraint">>, <<"en">>, Text).

restricted_xml(Text) ->
    jlib:stream_errort(<<"restricted-xml">>, <<"en">>, Text).

% TODO: include hostname or IP
see_other_host(Text) ->
    jlib:stream_errort(<<"see-other-host">>, <<"en">>, Text).

system_shutdown(Text) ->
    jlib:stream_errort(<<"system-shutdown">>, <<"en">>, Text).

unsupported_encoding(Text) ->
    jlib:stream_errort(<<"unsupported-encoding">>, <<"en">>, Text).

unsupported_stanza_type(Text) ->
    jlib:stream_errort(<<"unsupported-stanza-type">>, <<"en">>, Text).

unsupported_version(Text) ->
    jlib:stream_errort(<<"unsupported-version">>, <<"en">>, Text).

xml_not_well_formed(Text) ->
    jlib:stream_errort(<<"xml-not-well-formed">>, <<"en">>, Text).

undefined_condition(Text) ->
    jlib:stream_errort(<<"undefined-condition">>, <<"en">>, Text).
