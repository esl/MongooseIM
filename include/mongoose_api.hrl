%% backend state for HTTP API
-record(http_api_state, {allowed_methods,
                         bindings,
                         parameters,
                         command_category,
                         command_subcategory,
                         entity = admin,
                         auth = any,
                         opts = []}).
%% Error messages
-define(ARGS_LEN_ERROR, <<"Bad parameters length.">>).
-define(ARGS_SPEC_ERROR, <<"Bad name of the parameter.">>).
-define(BODY_MALFORMED, <<"The request body is malformed.">>).
-define(COMMANDS_ENGINE, mongoose_commands).

%% Types
-type method() ::binary().
-type arg_name() :: atom().
-type arg_value() :: binary() | string() | integer() | float().
-type arg_spec_list() :: list(mongoose_commands:argspec()).
-type optarg_spec_list() :: list(mongoose_commands:optargspec()).
-type args_applied() :: list({arg_name(), arg_value()}).
-type arg_values() :: list(arg_value()).
-type http_api_state() :: #http_api_state{}.
