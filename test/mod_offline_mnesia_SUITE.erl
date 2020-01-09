-module(mod_offline_mnesia_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(OFFLINE_MSG_3_5_FIELDS, [us, timestamp, expire, from, to, packet]).

all() ->
    [mnesia_offline_table_can_be_upgraded_from_3_5_to_next].

init_per_suite(C) ->
    application:ensure_all_started(stringprep),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

mnesia_offline_table_can_be_upgraded_from_3_5_to_next(_C) ->
    %% GIVEN offline_msg table existing with fields as in MongooseIM 3.5.0
    {atomic, ok} = mnesia:create_table(offline_msg,
                                       [{disc_only_copies, [node()]},
                                        {type, bag},
                                        {attributes, ?OFFLINE_MSG_3_5_FIELDS}]),
    Key = {<<"user">>, <<"localhost">>},
    Record = {offline_msg, Key, os:timestamp(), undefined,
              <<"user@localhost">>, <<"user2@localhost">>, <<"packet">>},
    %% AND there is a record in the table
    mnesia:dirty_write(Record),
    [Before] = mnesia:dirty_read(offline_msg, Key),
    %% WHEN new mod_offline_mnesia starts
    ok = mod_offline_mnesia:init(<<"localhost">>, []),
    %% THEN the field is added to table schema
    ?assert(lists:member(permanent_fields, mnesia:table_info(offline_msg, attributes))),
    %% AND empty the value of the new field is an empty list
    [After] = mnesia:dirty_read(offline_msg, Key),
    ?assertEqual(erlang:append_element(Before, []), After).


