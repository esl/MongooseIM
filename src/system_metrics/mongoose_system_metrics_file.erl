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
        {ok, JSON} ->
            Report = jiffy:decode(JSON, [return_maps]),
            [#{report_name => b2a(Name), key => b2a(Key), value => b2a(Value)} ||
                #{<<"report_name">> := Name,
                <<"key">> := Key,
                <<"value">> := Value}  <- Report];
        _ -> []
    end.

-spec b2a(integer() | binary()) -> atom() | integer().
b2a(Int) when is_integer(Int) ->
    Int;
b2a(Bin) ->
    binary_to_atom(Bin, utf8).
