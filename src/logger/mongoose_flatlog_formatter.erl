-module(mongoose_flatlog_formatter).
-export([format/2]).

format(Map, UsrConfig) ->
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
