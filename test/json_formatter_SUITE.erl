-module(json_formatter_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
    something_is_logged/1,
    something_is_formatted/1,
    acc_is_formatted/1,
    acc_is_preserved/1,
    chars_limited/1,
    format_depth_limited/1,
    json_depth_limited/1,
    large_event_dont_crash_formatter/1
]).

-import(logger_helper, [filter_out_non_matching/2, get_at_least_n_log_lines/3, get_log/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOGFILE, "log/mongooseim.log").
-define(HID, json_log).

all() ->
    [
        something_is_logged,
        something_is_formatted,
        acc_is_formatted,
        acc_is_preserved,
        chars_limited,
        format_depth_limited,
        json_depth_limited,
        large_event_dont_crash_formatter
    ].

init_per_testcase(acc_is_preserved, Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    ConfigFilters = #{filters =>
                        [{preserve_acc_filter, {fun mongoose_log_filter:preserve_acc_filter/2, no_state}},
                         {format_packet_filter, {fun mongoose_log_filter:format_packet_filter/2, no_state}},
                         {format_acc_filter, {fun mongoose_log_filter:format_acc_filter/2, no_state}},
                         {format_c2s_state_filter, {fun mongoose_log_filter:format_c2s_state_filter/2, no_state}},
                         {format_stacktrace_filter, {fun mongoose_log_filter:format_stacktrace_filter/2, no_state}}
                        ]},
    {ok, Backend} = mongoose_logger_running(ConfigFilters),
    [{logger_primary_config, LoggerConfig}, {logger_backend, Backend} | Config];
init_per_testcase(format_depth_limited, Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    DepthLimit = 2,
    ConfigFormatter = #{formatter => {mongoose_json_formatter, #{format_depth => DepthLimit}}},
    {ok, Backend} = mongoose_logger_running(ConfigFormatter),
    [{logger_primary_config, LoggerConfig}, {logger_backend, Backend}, {format_depth, DepthLimit}| Config];
init_per_testcase(chars_limited, Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    CharsLimit = 30,
    ConfigFormatter = #{formatter => {mongoose_json_formatter, #{format_chars_limit => CharsLimit}}},
    {ok, Backend} = mongoose_logger_running(ConfigFormatter),
    [{logger_primary_config, LoggerConfig}, {logger_backend, Backend}, {chars_limit, CharsLimit}| Config];
init_per_testcase(json_depth_limited, Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    Depth = 5,
    ConfigFormatter = #{formatter => {mongoose_json_formatter, #{depth => Depth}}},
    {ok, Backend} = mongoose_logger_running(ConfigFormatter),
    [{logger_primary_config, LoggerConfig}, {logger_backend, Backend}, {depth, Depth}| Config];
init_per_testcase(_TC, Config) ->
    LoggerConfig = logger:get_primary_config(),
    logger:set_primary_config(level, info),
    {ok, Backend} = mongoose_logger_running(),
    [{logger_primary_config, LoggerConfig}, {logger_backend, Backend} | Config].

end_per_testcase(_TC, Config) ->
    logger:remove_handler(?HID),
    logger:set_primary_config(?config(logger_primary_config, Config)),
    ok.

%%
%% Tests
%%
something_is_logged(Config) ->
    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),
    ?LOG_INFO(#{what => something_is_logged,
                text => "JSON formatter test. This is an example message.",
                code => 404, reason => test_execution}),
    ?assertNotEqual(timeout, get_at_least_n_log_lines(File, length(Before) + 1, timer:seconds(5))).

something_is_formatted(Config) ->
    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),

    ?LOG_INFO(#{in => config, user => #{name => <<"bbby">>, id => 12345}, details => {entropy, too_low},
                txt => <<"JSON formatter test. JSON-match-this-something">>}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, <<"JSON-match-this-something">>),
    Decoded = jiffy:decode(Line, [return_maps]),

    #{<<"level">> := <<"info">>,
      <<"when">> := DateTimeStrBin,
      <<"meta">> := #{<<"file">> := _File,
                      <<"line">> := _Line,
                      <<"mfa">> := <<"{json_formatter_SUITE,something_is_formatted,1}">>,
                      <<"pid">> := Pid},
      <<"details">> := <<"{entropy,too_low}">>,
      <<"in">> := <<"config">>,
      <<"txt">> := <<"JSON formatter test. JSON-match-this-something">>,
      <<"user">> := #{<<"id">> := <<"12345">>,
                      <<"name">> := <<"bbby">>}}
    = Decoded,

    ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(DateTimeStrBin)))),
    Pid = unicode:characters_to_binary(pid_to_list(self())).

acc_is_formatted(Config) ->
    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),

    Body = <<"JSON-match-this-acc">>,
    Acc = example_acc(Body),

    ?LOG_INFO(#{what => routing_result, acc => Acc, routing_modules => [mongoose_router_1, mongoose_router_2],
                routing_result => [{{inside, two_tuples}}, {inside, one_tuple}]}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, Body),

    Decoded = jiffy:decode(Line, [return_maps]),

    #{<<"level">> := <<"info">>,
      <<"when">> := DateTimeStrBin,
      <<"meta">> := #{<<"file">> := _File,
                      <<"line">> := _Line,
                      <<"mfa">> := <<"{json_formatter_SUITE,acc_is_formatted,1}">>,
                      <<"pid">> := Pid},
      % Because of the format_acc_filter/2 we don't get the full accumulator
      <<"acc_timestamp">> := DateTimeStrBin2,
      <<"from_jid">> := <<"usera@localhost">>,
      <<"origin_pid">> := Pid,
      % format_packet_filter/2 changes the packet
      <<"packet">> := <<"<message type='chat' id='1111'><body>JSON-match-this-acc</body></message>">>,
      <<"routing_modules">> := [<<"mongoose_router_1">>, <<"mongoose_router_2">>],
      <<"routing_result">> := [<<"{{inside,two_tuples}}">>,<<"{inside,one_tuple}">>],
      <<"to_jid">> := <<"userb@localhost">>,
      <<"what">> := <<"routing_result">>} = Decoded,

    ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(DateTimeStrBin)))),
    ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(DateTimeStrBin2)))),
    Pid = unicode:characters_to_binary(pid_to_list(self())).

acc_is_preserved(Config) ->
    ok = logger:add_primary_filter(preserve_acc_filter, {fun mongoose_log_filter:preserve_acc_filter/2, no_state}),

    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),

    Body = <<"JSON-match-this-preserve-acc">>,
    Acc = example_acc(Body),

    ?LOG_INFO(#{what => routing_result, acc => Acc, routing_modules => [mongoose_router_1, mongoose_router_2],
                routing_result => [{{inside, two_tuples}}, {inside, one_tuple}]}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, Body),

    Decoded = jiffy:decode(Line, [return_maps]),

    #{<<"level">> := <<"info">>,
      <<"when">> := DateTimeStrBin,
      <<"meta">> := #{<<"file">> := _File,
                      <<"line">> := _Line,
                      <<"mfa">> := <<"{json_formatter_SUITE,acc_is_preserved,1}">>,
                      <<"pid">> := Pid},
      % Because of the preserve_acc_filter/2 we get the full accumulator as acc_original
      <<"acc_original">> := A,
      <<"acc_timestamp">> := DateTimeStrBin2,
      <<"from_jid">> := <<"usera@localhost">>,
      <<"origin_pid">> := Pid,
      % format_packet_filter/2 changes the packet
      <<"packet">> := <<"<message type='chat' id='1111'><body>JSON-match-this-preserve-acc</body></message>">>,
      <<"routing_modules">> := [<<"mongoose_router_1">>, <<"mongoose_router_2">>],
      <<"routing_result">> := [<<"{{inside,two_tuples}}">>,<<"{inside,one_tuple}">>],
      <<"to_jid">> := <<"userb@localhost">>,
      <<"what">> := <<"routing_result">>} = Decoded,

    % This is not ideal but that's how the filter behaves
    A = iolist_to_binary(io_lib:format("~0p", [Acc])),

    ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(DateTimeStrBin)))),
    ?assert(is_integer(calendar:rfc3339_to_system_time(binary_to_list(DateTimeStrBin2)))),
    Pid = unicode:characters_to_binary(pid_to_list(self())).

chars_limited(Config) ->

    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),
    CharsLimit = ?config(chars_limit, Config),

    LongBinary = unicode:characters_to_binary([<<"a">> || _ <- lists:seq(1, CharsLimit*2)]),
    LongText = unicode:characters_to_list(["a" || _ <- lists:seq(1, CharsLimit*2)]),
    LongStruct = {["a" || _ <- lists:seq(1, CharsLimit*2)]},

    ?LOG_INFO(#{what => chars_are_limited,
                text => "JSON-match-this-chars-limited",
                long_binary => LongBinary,
                long_text => LongText,
                long_struct => LongStruct}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, <<"JSON-match-this-chars-limited">>),

    Decoded = jiffy:decode(Line, [return_maps]),

    ShortenedBinary = binary_part(LongBinary, 0, CharsLimit),
    ShortenedText = binary_part(unicode:characters_to_binary(LongText, utf8), 0, CharsLimit),
    ShortenedStruct = unicode:characters_to_binary(io_lib:format("~0p", [LongStruct], [{chars_limit, CharsLimit}])),

    ?assertMatch(#{<<"level">> := <<"info">>,
                   <<"when">> := _Time,
                   <<"meta">> := #{<<"file">> := _File,
                                   <<"line">> := _Line,
                                   <<"mfa">> := _MFA,
                                   <<"pid">> := _Pid},
                   <<"what">> := <<"chars_are_limited">>,
                   <<"text">> := <<"JSON-match-this-chars-limited">>,
                   <<"long_binary">> := ShortenedBinary,
                   <<"long_text">> := ShortenedText,
                   <<"long_struct">> := ShortenedStruct},
                 Decoded).

format_depth_limited(Config) ->

    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),

    FormatDepth = ?config(format_depth, Config),
    DeepStruct = deep_tuple(FormatDepth*2),

    ?LOG_INFO(#{what => format_depth_limited,
                text => "JSON-match-this-struct-depth-limited",
                deep_struct => DeepStruct}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, <<"JSON-match-this-struct-depth-limited">>),

    Decoded = jiffy:decode(Line, [return_maps]),

    ShortenedStruct = unicode:characters_to_binary(io_lib:format("~0P", [DeepStruct, FormatDepth])),

    ?assertMatch(#{<<"level">> := <<"info">>,
                   <<"when">> := _Time,
                   <<"meta">> := #{<<"file">> := _File,
                                   <<"line">> := _Line,
                                   <<"mfa">> := _MFA,
                                   <<"pid">> := _Pid},
                   <<"what">> := <<"format_depth_limited">>,
                   <<"text">> := <<"JSON-match-this-struct-depth-limited">>,
                   <<"deep_struct">> := ShortenedStruct},
                 Decoded).


json_depth_limited(Config) ->

    {_, File} = ?config(logger_backend, Config),
    Before = get_log(File),

    Depth = ?config(depth, Config),
    DeepList = deep_list(Depth*2),

    ?LOG_INFO(#{what => json_depth_limited,
                text => "JSON-match-this-json-depth-limited",
                deep_list => DeepList}),

    After = case get_at_least_n_log_lines(?LOGFILE,
                                          length(Before) + 1,
                                          timer:seconds(1)) of
                timeout -> ct:fail("timed out waiting for messages to reach the log file");
                Res -> Res
            end,

    [Line] = filter_out_non_matching(After -- Before, <<"JSON-match-this-json-depth-limited">>),

    Decoded = jiffy:decode(Line, [return_maps]),

    % 1 because it is at the first level
    ShortenedList = deep_list(Depth - 1, <<"...">>),

    ?assertMatch(#{<<"level">> := <<"info">>,
                   <<"when">> := _Time,
                   <<"meta">> := #{<<"file">> := _File,
                                   <<"line">> := _Line,
                                   <<"mfa">> := _MFA,
                                   <<"pid">> := _Pid},
                   <<"what">> := <<"json_depth_limited">>,
                   <<"text">> := <<"JSON-match-this-json-depth-limited">>,
                   <<"deep_list">> := ShortenedList},
                 Decoded).

large_event_dont_crash_formatter(_Config) ->

    ?LOG_INFO(#{what => large_log,
                large_log_msg => large_log_msg()}),

    DidFormatterCrash = is_in_file(?LOGFILE,
                                   "[0-9\\+\\-T:\\.]* info: FORMATTER CRASH: .*",
                                   timer:seconds(1)),

    ?assertMatch(DidFormatterCrash, false).

%%
%% Helpers
%%

example_acc(Body) ->
    Elem = {xmlel, <<"message">>,
            [{<<"type">>, <<"chat">>}, {<<"id">>, <<"1111">>}],
            [{xmlel, <<"body">>, [], [{xmlcdata, Body}]}]},
    #{lserver => <<"localhost">>,
      mongoose_acc => true,
      non_strippable => [],
      origin_location => #{file => "/Users/user/MongooseIM/src/ejabberd_router.erl",
                           line => 116,
                           mfa => {ejabberd_router,route,3}},
      origin_pid => self(),
      ref => make_ref(),
      stanza => #{element => Elem,
                  from_jid => {jid, <<"usera">>, <<"localhost">>, <<>>},
                  name => <<"message">>,
                  ref => make_ref(),
                  to_jid => {jid, <<"userb">>, <<"localhost">>, <<>>},
                  type => <<"chat">>},
      timestamp => 1598878576962100}.

mongoose_logger_running() ->
    mongoose_logger_running(#{}).
mongoose_logger_running(HandlerConfig) ->
    HandlerID = ?HID,
    HandlerModule = logger_std_h,
    DefaultConfig = #{level => info,
                      config => #{file => ?LOGFILE},
                      formatter => {mongoose_json_formatter, #{}},
                      filters => [
                          {format_packet_filter, {fun mongoose_log_filter:format_packet_filter/2, no_state}},
                          {format_acc_filter, {fun mongoose_log_filter:format_acc_filter/2, no_state}},
                          {format_c2s_state_filter, {fun mongoose_log_filter:format_c2s_state_filter/2, no_state}},
                          {format_stacktrace_filter, {fun mongoose_log_filter:format_stacktrace_filter/2, no_state}}
                      ]
    },
    Config = maps:merge(DefaultConfig, HandlerConfig),
    ok = logger:add_handler(HandlerID, HandlerModule, Config),
    FileBackend = {HandlerID, ?LOGFILE},
    {ok, FileBackend}.

deep_tuple(1) ->
    {"a"};
deep_tuple(N) ->
    {deep_tuple(N-1)}.

deep_list(N) ->
    deep_list(N, "a").
deep_list(1, Content) ->
    [Content];
deep_list(N, Content) ->
    [deep_list(N-1, Content)].

is_in_file(FileName, Pattern, Time) when Time > 0 ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            case re:run(Bin,Pattern,[{capture,none}]) of
                match ->
                    true;
                _ ->
                    timer:sleep(100),
                    is_in_file(FileName, Pattern, Time-100)
            end;
        Error ->
            erlang:error(Error)
    end;
is_in_file(_FileName, _Pattern, _Time) ->
    false.

large_log_msg() ->
    "These violent delights have violent ends " ++
    "And in their triumph die, like fire and powder, " ++
    "Which, as they kiss, consume. The sweetest honey" ++
    "Is loathsome in his own deliciousness " ++
    "And in the taste confounds the appetite. " ++
    "Therefore love moderately: long love doth so; " ++
    "Too swift arrives as tardy as too slow. " ++
    "Rom. In faith, I will. Let me peruse this face. " ++
    "Mercutio's kinsman, noble County Paris! " ++
    "What said my man when my betossed soul " ++
    "Did not attend him as we rode? I think " ++
    "He told me Paris should have married Juliet. " ++
    "Said he not so? or did I dream it so? " ++
    "Or am I mad, hearing him talk of Juliet " ++
    "To think it was so? O, give me thy hand, " ++
    "One writ with me in sour misfortune's book! " ++
    "I'll bury thee in a triumphant grave. " ++
    "A grave? O, no, a lanthorn, slaught'red youth, " ++
    "For here lies Juliet, and her beauty makes " ++
    "This vault a feasting presence full of light. " ++
    "Death, lie thou there, by a dead man interr'd. " ++
    "                                     [Lays him in the tomb.]" ++
    "How oft when men are at the point of death " ++
    "Have they been merry! which their keepers call " ++
    "A lightning before death. O, how may I " ++
    "Call this a lightning? O my love! my wife! " ++
    "Death, that hath suck'd the honey of thy breath," ++
    "Hath had no power yet upon thy beauty. " ++
    "Thou art not conquer'd. Beauty's ensign yet " ++
    "Is crimson in thy lips and in thy cheeks, " ++
    "And death's pale flag is not advanced there. " ++
    "Tybalt, liest thou there in thy bloody sheet? " ++
    "O, what more favour can I do to thee " ++
    "Than with that hand that cut thy youth in twain " ++
    "To sunder his that was thine enemy? " ++
    "Forgive me, cousin.' Ah, dear Juliet, " ++
    "Why art thou yet so fair? Shall I believe " ++
    "That unsubstantial Death is amorous, " ++
    "And that the lean abhorred monster keeps " ++
    "Thee here in dark to be his paramour? " ++
    "For fear of that I still will stay with thee " ++
    "And never from this palace of dim night " ++
    "Depart again. Here, here will I remain " ++
    "With worms that are thy chambermaids. O, here " ++
    "Will I set up my everlasting rest " ++
    "And shake the yoke of inauspicious stars " ++
    "From this world-wearied flesh. Eyes, look your last!" ++
    "Arms, take your last embrace! and, lips, O you " ++
    "The doors of breath, seal with a righteous kiss " ++
    "A dateless bargain to engrossing death! " ++
    "Come, bitter conduct; come, unsavoury guide! " ++
    "Thou desperate pilot, now at once run on " ++
    "The dashing rocks thy seasick weary bark! " ++
    "Here's to my love! [Drinks.] O true apothecary! " ++
    "Thy drugs are quick. Thus with a kiss I die.".
