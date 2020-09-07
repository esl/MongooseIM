%%%-------------------------------------------------------------------
%%% @copyright 2020, Erlang Solutions Ltd.
%%% @doc
%%% A module formatting logger reports into JSON objects.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_json_formatter).

%% API
-export([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(E = #{msg := {report, #{label := {error_logger, _}, format := Format, args := Terms}}}, FConfig) ->
    format(E#{msg := {report,
                      #{unstructured_log =>
                        unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           FConfig);
format(E = #{msg := {string, String}}, FConfig) ->
    format(E#{msg := {report,
        #{unstructured_log =>
          unicode:characters_to_binary(io_lib:format(String, []))}}}, FConfig);
format(E = #{msg := {report, Report}}, FConfig) when is_map(Report) ->
    Formatted = format_item(E, maps:merge(default_config(), FConfig)),
    unicode:characters_to_binary([jiffy:encode(Formatted), "\n"], utf8);
format(Map = #{msg := {Format, Terms}}, FConfig) ->
    format(Map#{msg := {report,
                        #{unstructured_log =>
                          unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           FConfig).

format_item({time, Timestamp}, _FConfig) ->
    TimeString = calendar:system_time_to_rfc3339(Timestamp, [{unit, microsecond}]),
    #{<<"time">> => unicode:characters_to_binary(TimeString)};
format_item(Item, FConfig) when is_map(Item) ->
    ML = [format_item({Key, Val}, FConfig) || {Key, Val} <- maps:to_list(Item)],
    lists:foldl(fun(Map1, Acc) -> maps:merge(Map1, Acc) end, #{}, ML);
format_item({K, V}, FConfig) ->
    % Keys need to be strings in JSON
    % We can get a nested structure as a key K here, it needs to be stringified
    #{all_to_binary(K, FConfig) => format_item(V, FConfig)};
format_item(Item, FConfig) when is_list(Item) ->
    case io_lib:printable_unicode_list(Item) of
        true ->
            % We want to print strings as strings
            B = unicode:characters_to_binary(Item, utf8),
            shorten_binary(B, FConfig);
        false ->
            % And pass lists of objects as lists of objects
            [format_item(I, FConfig) || I <- Item]
    end ;
format_item(Item, FConfig) ->
    all_to_binary(Item, FConfig).

all_to_binary(S, FConfig) when is_binary(S) ->
    shorten_binary(S, FConfig);
all_to_binary(Something, FConfig) ->
    Chars = format_str(Something, FConfig),
    unicode:characters_to_binary(Chars, utf8).

shorten_binary(S, FConfig) ->
    case format_chars_limit(FConfig) of
        [] -> S;
        [{chars_limit, L}] -> binary_part(S, 0, min(size(S), L))
    end.

format_chars_limit(Config) ->
    format_chars_limit_to_opts(maps:get(format_chars_limit, Config)).

format_chars_limit_to_opts(unlimited) -> [];
format_chars_limit_to_opts(CharsLimit) -> [{chars_limit, CharsLimit}].

format_str(S, #{format_depth := unlimited, format_chars_limit := L}) ->
    io_lib:format("~0tp", [S], format_chars_limit_to_opts(L));
format_str(S, #{format_depth := D, format_chars_limit := L}) ->
    io_lib:format("~0tP", [S, D], format_chars_limit_to_opts(L)).

default_config() ->
    #{format_chars_limit => unlimited,
      format_depth => unlimited
    }.
