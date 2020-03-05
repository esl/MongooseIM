-module(mongoose_system_metrics_file).

-include("mongoose.hrl").

-export([location/0]).
-export([save/1, read/0]).

-spec location() -> string().
location() ->
    LogDir = ejabberd_loglevel:dir(),
    LogDir ++ "/system_metrics_report.json".

-spec save([mongoose_system_metrics_collector:report_struct()]) -> ok.
save(Reports) ->
    JSON = jiffy:encode(Reports, [pretty]),
    file:write_file(location(), JSON),
    ok.

-spec read() -> [mongoose_system_metrics_collector:report_struct()].
read() ->
    case file:read_file(location()) of
        {ok, JSON} -> jiffy:decode(JSON, [return_maps]);
        _ -> []
    end.