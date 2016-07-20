-include("mongoose_commands.hrl").
%% backend state for HTTP API
-record(backend_state, {allowed_methods, bindings, parameters, command_category, entity = admin, opts = []}).
%% Error messages
-define(ARGS_LEN_ERROR, "Bad parameters length.").
-define(ARGS_SPEC_ERROR, "Bad name of the parameter.").
-define(BODY_MALFORMED, "The request body is malformed.").
-define(COMMANDS_ENGINE, mongoose_commands).

%% Types
-type method() ::binary().
-type arg_name() :: atom().
-type arg_value() :: any().
-type arg_spec_list() :: list(argspec()).
-type args_applied() :: list({arg_name(), arg_value()}).
-type arg_values() :: list(arg_value()).
-type mongoose_command() :: #mongoose_command{}.
