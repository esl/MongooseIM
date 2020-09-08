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
    NewMap = process_metadata(E),
    NewReport = maps:merge(Report, NewMap),
    NewConfig = maps:merge(default_config(), config_correct_depth(FConfig)),
    Formatted = format_item(NewReport, NewConfig),
    unicode:characters_to_binary([jiffy:encode(Formatted), "\n"], utf8);
format(Map = #{msg := {Format, Terms}}, FConfig) ->
    format(Map#{msg := {report,
                        #{unstructured_log =>
                          unicode:characters_to_binary(io_lib:format(Format, Terms))}}},
           FConfig).

format_item(_Item, _FConfig = #{depth := 0}) ->
    <<"...">>;
format_item(Item, FConfig) when is_map(Item) ->
    ML = [format_item({Key, Val}, FConfig) || {Key, Val} <- maps:to_list(Item)],
    lists:foldl(fun(Map1, Acc) -> maps:merge(Map1, Acc) end, #{}, ML);
format_item({K, V}, FConfig = #{depth := Depth}) ->
    % Keys need to be strings in JSON
    % We can get a nested structure as a key K here, it needs to be stringified
    #{all_to_binary(K, FConfig) => format_item(V, FConfig#{depth := Depth - 1})};
format_item(Item, FConfig = #{depth := Depth}) when is_list(Item) ->
    case io_lib:printable_unicode_list(Item) of
        true ->
            % We want to print strings as strings
            format_str(Item, FConfig);
        false ->
            % And pass lists of objects as lists of objects
            [format_item(I, FConfig#{depth := Depth - 1}) || I <- Item]
    end;
format_item(Item, FConfig) ->
    all_to_binary(Item, FConfig).

format_time(T) ->
    TimeString = calendar:system_time_to_rfc3339(T, [{unit, microsecond}]),
    unicode:characters_to_binary(TimeString).

all_to_binary(S, FConfig) when is_binary(S) ->
    shorten_binary(S, FConfig);
all_to_binary(Something, FConfig) ->
    Chars = format_str(Something, FConfig),
    unicode:characters_to_binary(Chars, utf8).

shorten_binary(S, #{format_chars_limit := unlimited}) ->
    S;
shorten_binary(S, #{format_chars_limit := L}) ->
    binary_part(S, 0, min(size(S), L)).

format_chars_limit_to_opts(unlimited) -> [];
format_chars_limit_to_opts(CharsLimit) -> [{chars_limit, CharsLimit}].

format_str(S, FConfig) when is_list(S) ->
    case io_lib:printable_unicode_list(S) of
        true ->
            B = unicode:characters_to_binary(S, utf8),
            shorten_binary(B, FConfig);
        false ->
            do_format_str(S, FConfig)
    end;
format_str(S, FConfig) ->
    do_format_str(S, FConfig).

do_format_str(S, #{format_depth := unlimited, format_chars_limit := L}) ->
    io_lib:format("~0tp", [S], format_chars_limit_to_opts(L));
do_format_str(S, #{format_depth := D, format_chars_limit := L}) ->
    io_lib:format("~0tP", [S, D], format_chars_limit_to_opts(L)).

process_metadata(#{meta := #{time := T} = M, level := L}) ->
    DiscardKeys = [time, gl, report_cb],
    #{level => L,
      "when" => format_time(T),
      meta => maps:without(DiscardKeys, M)}.

config_correct_depth(C = #{depth := unlimited}) ->
    C#{depth := -1};
config_correct_depth(C) ->
    C.

default_config() ->
    #{format_chars_limit => unlimited,
      format_depth => unlimited,
      depth => -1
    }.
