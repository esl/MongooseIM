-module(mongoose_lib_SUITE).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [pmap_works,
     sh_to_awk_converts_star,
     sh_to_awk_converts_question_mark,
     sh_to_awk_converts_character_class,
     sh_to_awk_converts_negated_character_class,
     sh_to_awk_escapes_special_chars,
     sh_to_awk_handles_empty_pattern,
     sh_to_awk_complex_patterns,
     sh_to_awk_matches_correctly].

init_per_suite(C) ->
    C.

end_per_suite(C) ->
    C.

pmap_works(_C) ->
    ?assertEqual([{ok, 1}, {ok, 2}, {ok, 3}],
                 mongoose_lib:pmap(fun(X) -> X end, [1, 2, 3])),
    ?assertMatch([{ok, 1}, {error, {oops, _}}, {ok, 3}],
                 mongoose_lib:pmap(fun(2) -> error(oops); (X) -> X end,
                                   [1, 2, 3])),
    ?assertMatch([_, {error, timeout}, _],
                 mongoose_lib:pmap(fun(2) -> timer:sleep(50000); (X) -> X end,
                                   [1, 2, 3], 10)).

sh_to_awk_converts_star(_C) ->
    ?assertEqual("^(.*)$", mongoose_lib:sh_to_awk("*")),
    ?assertEqual("^(foo.*)$", mongoose_lib:sh_to_awk("foo*")),
    ?assertEqual("^(.*bar)$", mongoose_lib:sh_to_awk("*bar")),
    ?assertEqual("^(foo.*bar)$", mongoose_lib:sh_to_awk("foo*bar")).

sh_to_awk_converts_question_mark(_C) ->
    ?assertEqual("^(.)$", mongoose_lib:sh_to_awk("?")),
    ?assertEqual("^(foo.)$", mongoose_lib:sh_to_awk("foo?")),
    ?assertEqual("^(...bar)$", mongoose_lib:sh_to_awk("???bar")).

sh_to_awk_converts_character_class(_C) ->
    ?assertEqual("^([])$", mongoose_lib:sh_to_awk("[]")),
    ?assertEqual("^([abc])$", mongoose_lib:sh_to_awk("[abc]")),
    ?assertEqual("^(foo[0-9])$", mongoose_lib:sh_to_awk("foo[0-9]")),
    ?assertEqual("^([a-z]bar)$", mongoose_lib:sh_to_awk("[a-z]bar")).

sh_to_awk_converts_negated_character_class(_C) ->
    ?assertEqual("^([^ab])$", mongoose_lib:sh_to_awk("[!ab]")),
    ?assertEqual("^([^^])$", mongoose_lib:sh_to_awk("[!^]")),
    ?assertEqual("^([0-6^])$", mongoose_lib:sh_to_awk("[^0-6]")).

sh_to_awk_escapes_special_chars(_C) ->
    ?assertEqual("^(a\\.b)$", mongoose_lib:sh_to_awk("a.b")),
    ?assertEqual("^(a\\+b)$", mongoose_lib:sh_to_awk("a+b")),
    ?assertEqual("^(a\\(b\\))$", mongoose_lib:sh_to_awk("a(b)")),
    ?assertEqual("^(a\\|b)$", mongoose_lib:sh_to_awk("a|b")),
    ?assertEqual("^(a\\^b)$", mongoose_lib:sh_to_awk("a^b")),
    ?assertEqual("^(a\\^b)$", mongoose_lib:sh_to_awk("a[^]b")),
    ?assertEqual("^(a\\$b)$", mongoose_lib:sh_to_awk("a$b")),
    ?assertEqual("^(a\\]b)$", mongoose_lib:sh_to_awk("a]b")),
    ?assertEqual("^(a\\\"b)$", mongoose_lib:sh_to_awk("a\"b")),
    ?assertEqual("^(a\\\\b)$", mongoose_lib:sh_to_awk("a\\b")).

sh_to_awk_handles_empty_pattern(_C) ->
    ?assertEqual("^()$", mongoose_lib:sh_to_awk("")).

sh_to_awk_complex_patterns(_C) ->
    ?assertEqual("^(file[0-9]\\..*)$", mongoose_lib:sh_to_awk("file[0-9].*")),
    ?assertEqual("^(test.-.*)$", mongoose_lib:sh_to_awk("test?-*")),
    ?assertEqual("^(data\\.[^\\.].*\\.txt)$", mongoose_lib:sh_to_awk("data.[!\\.]*.txt")).

sh_to_awk_matches_correctly(_C) ->
    Pattern = mongoose_lib:sh_to_awk("*.txt"),
    ?assertEqual(match, re:run("foo.txt", Pattern, [{capture, none}])),
    ?assertEqual(match, re:run(".txt", Pattern, [{capture, none}])),
    ?assertEqual(nomatch, re:run("foo.txta", Pattern, [{capture, none}])).
