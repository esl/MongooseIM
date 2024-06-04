-module(mongoose_prometheus_handler).

-behaviour(mongoose_http_handler).

%% mongoose_http_handler callbacks
-export([routes/1]).

-spec routes(mongoose_http_handler:options()) -> mongoose_http_handler:routes().
routes(#{path := BasePath}) ->
    [{[BasePath, "/[:registry]"], prometheus_cowboy2_handler, #{}}].
