-module(mongoose_flatlog_formatter).
-export([format/2]).

format(Map, UsrConfig = #{template_name := Name}) ->
    case application:get_env(kernel, Name) of
        {ok, Template} ->
            flatlog:format(Map, UsrConfig#{template => Template});
        _ ->
            %% Template not found, use default
            flatlog:format(Map, UsrConfig)
    end;
format(Map, UsrConfig) ->
    flatlog:format(Map, UsrConfig).
