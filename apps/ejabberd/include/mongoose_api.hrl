%% backend state for HTTP API
-record(backend_state, {allowed_methods, bindings, parameters, command_category, entity = admin, opts = []}).
%% Error messages
-define(ARGS_LEN_ERROR, "Bad parameters length.").
-define(ARGS_SPEC_ERROR, "Bad name of the parameter.").
-define(BODY_MALFORMED, "The request body is malformed.").