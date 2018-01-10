%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% The functions in this module were previously defined as macros in jlib.hrl
%%% @end
%%% Created : 10. Jan 2018 09:58
%%%-------------------------------------------------------------------
-module(mongoose_xmpp_errors).
-author("alex").


-compile([export_all]).
-include("mongoose_ns.hrl").
%%%%%%%%%%%%%%%%%
%% Stanza Errors
%%%%%%%%%%%%%%%%%

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



