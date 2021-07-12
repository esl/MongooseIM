-module(mongoose_flatlog_formatter).
-export([format/2]).

-ignore_xref([format/2]).

-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(Event, FConfig) ->
    try do_format(Event, FConfig)
    catch
        %% Errors during log formatting can lead to a death spiral of recursive error logging, so
        %% format the formatter error in a safe way and don't allow the exception to propagate.
        error:Reason:Stacktrace -> format_log_formatter_error(error, Reason, Stacktrace, Event, FConfig)
    end.


format_log_formatter_error(Class, Reason, Stacktrace, #{meta := Meta} = Event, FConfig) ->
    flatlog:format(
        #{
            level => error,
            msg => {report, #{
                class => Class, reason => Reason, stacktrace => Stacktrace,
                formatter_module => ?MODULE,
                original_event => unicode:characters_to_binary(io_lib:format("~0p", [Event]))
            }},
            meta => Meta#{what => log_format_failed}
        },
        FConfig#{template => template()}
    ).


do_format(Map, UsrConfig) ->
    Map2 = mongoose_log_filter:fill_metadata_filter(Map, fields()),
    flatlog:format(Map2, UsrConfig#{template => template()}).

fields() ->
    [what, text, user, from_jid, to_jid,
     class, reason, stacktrace].

template() ->
    [colored_start, "when=", time, " level=", level,
     {what, [" what=", what], ""},
     {reason, [" reason=", reason], ""},
     {pid, [" pid=", pid], ""}, " at=", mfa, ":", line, colored_end,
     {user, [" user=", user], ""},
     {from_jid, [" from_jid=", from_jid], ""},
     {to_jid, [" to_jid=", to_jid], ""},
     {stacktrace, [" stacktrace=", stacktrace], ""},
     {text, [" text=", text], ""}, " ",
     msg, "\n"].
