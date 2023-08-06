-ifndef(SAFELY).
-define(SAFELY, true).

-define(SAFELY(F),
        try F catch
            error:R:S -> {exception, #{class => error, reason => R, stacktrace => S}};
            throw:R -> {exception, #{class => throw, reason => R}};
            exit:R:S -> {exception, #{class => exit, reason => R, stacktrace => S}}
        end).

-endif.
