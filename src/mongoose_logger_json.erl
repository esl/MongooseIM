%%%-------------------------------------------------------------------
%%% @copyright 2020, Erlang Solutions Ltd.
%%% @doc
%%% A module formatting logger reports into JSON objects.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_logger_json).

%% API
-export([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(Map = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, UsrConfig) ->
    format(Map#{msg := {report,
                        #{unstructured_log =>
                          unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           UsrConfig);
format(E = #{msg := {string, String}}, FConfig) ->
    format(E#{msg := {report,
        #{unstructured_log =>
        unicode:characters_to_binary(String)}}}, FConfig);
format(E = #{msg := {report, Report}}, _FConfig) when is_map(Report) ->
    Formatted = format_item(E),
    unicode:characters_to_binary([jiffy:encode(Formatted), "\n"], utf8);
format(Map = #{msg := {Format, Terms}}, UsrConfig) ->
    format(Map#{msg := {report,
                        #{unstructured_log =>
                          unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           UsrConfig).

format_item({time, Timestamp}) ->
    TimeString = calendar:system_time_to_rfc3339(Timestamp, [{unit, microsecond}]),
    #{<<"time">> => unicode:characters_to_binary(TimeString)};
format_item(Item) when is_map(Item) ->
    ML = [format_item({Key, Val}) || {Key, Val} <- maps:to_list(Item)],
    lists:foldl(fun(Map1, Acc) -> maps:merge(Map1, Acc) end, #{}, ML);
format_item({K, V}) ->
    #{format_item(K) => format_item(V)};
format_item(Item) when is_list(Item) ->
    case io_lib:printable_unicode_list(Item) of
        true ->
            % We want to print strings as strings
            unicode:characters_to_binary(Item, utf8);
        false ->
            % And pass lists of objects as lists of objects
             [format_item(I) || I <- Item]
    end ;
format_item(Item) ->
    all_to_binary(Item).

all_to_binary(S) when is_binary(S) ->
    S;
all_to_binary(Something) ->
    Chars = io_lib:format("~0p", [Something]),
    unicode:characters_to_binary(Chars, utf8).
