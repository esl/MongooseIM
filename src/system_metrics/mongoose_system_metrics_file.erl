-module(mongoose_system_metrics_file).

-include("mongoose.hrl").

-export([location/0]).
-export([save/1]).

-spec location() -> string().
location() ->
    LogDir = ejabberd_loglevel:dir(),
    LogDir ++ "/system_metrics_report.json".

-spec save([mongoose_system_metrics_collector:report_struct()]) -> ok.
save(Reports) ->
    JSON = jiffy:encode(Reports, [pretty]),
    file:write_file(location(), JSON),
    ok.
