%% These error macros will change the xmpp errors(xml) to binaries
%% All errors are defined in mongoose_xmpp_errors.erl
%% Previously they were defined in up to three different modules

-define(INVALID_NS_ERR_TO_BIN,
    exml:to_binary(mongoose_xmpp_errors:invalid_namespace())).

-define(HOST_UNKNOWN_ERR_TO_BIN,
    exml:to_binary(mongoose_xmpp_errors:host_unknown())).

-define(INVALID_XML_ERR_TO_BIN,
    exml:to_binary(mongoose_xmpp_errors:xml_not_well_formed())).

-define(INVALID_FROM_ERR_TO_BIN,
    exml:to_binary(mongoose_xmpp_errors:invalid_from())).

-define(CONFLICT_ERR_TO_BIN,
    exml:to_binary(mongoose_xmpp_errors:stream_conflict())).

-define(SOCKET_DEFAULT_RESULT, {error, badarg}).

%% End of stream closing element
-define(STREAM_TRAILER, <<"</stream:stream>">>).
