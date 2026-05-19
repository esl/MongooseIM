-module(mod_muc_restore_opts_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("mod_muc.hrl").

all() ->
    [rdbms_restore_ignores_unknown_opts,
     mnesia_restore_allows_unknown_opts].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(),
    ok.

rdbms_restore_ignores_unknown_opts(_Config) ->
    ExtOpts = jiffy:encode({[{<<"title">>, <<"room">>}, {<<"logging">>, true}]}),
    meck:new(mongoose_rdbms, [no_link]),
    meck:expect(mongoose_rdbms, execute_successfully,
                fun(_HostType, Query, _Params) ->
                        case Query of
                            muc_select_room -> {selected, [{1, ExtOpts}]};
                            muc_select_aff -> {selected, []}
                        end
                end),
    meck:expect(mongoose_rdbms, result_to_integer, fun(_) -> 1 end),
    {ok, Opts} = mod_muc_rdbms:restore_room(test, <<"muc.example">>, <<"room">>),
    ?assertEqual(<<"room">>, proplists:get_value(title, Opts)),
    ?assertEqual(false, lists:keymember(logging, 1, Opts)),
    ok.

mnesia_restore_allows_unknown_opts(_Config) ->
    Opts = [{title, <<"room">>}, {logging, true}],
    meck:new(mnesia, [no_link]),
    meck:expect(mnesia, dirty_read,
                fun(muc_room, {RoomName, MucHost}) ->
                        [#muc_room{name_host = {RoomName, MucHost}, opts = Opts}]
                end),
    {ok, RestoredOpts} = mod_muc_mnesia:restore_room(test, <<"muc.example">>, <<"room">>),
    ?assertEqual(Opts, RestoredOpts),
    ok.
