-module(mongoose_domain_utils).
-export([halt_node/1]).

halt_node(ExitText) ->
    timer:sleep(1000),
    halt(string:substr(ExitText, 1, 199)).
