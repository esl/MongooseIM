-module(auth_internal_SUITE).

-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("scram.hrl").

all() ->
    [passwords_as_records_are_still_supported,
     passwords_in_plain_can_be_converted_to_scram].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    AuthOpts = #{methods => [internal],
                 internal => #{},
                 password => #{format => scram, scram_iterations => 10}},
    mongoose_config:set_opts(#{{auth, host_type()} => AuthOpts}),
    ejabberd_auth_internal:start(host_type()),
    C.

end_per_suite(_C) ->
    ejabberd_auth_internal:stop(host_type()),
    mongoose_config:erase_opts(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_testcase(_TC, Config) ->
    mongoose_domain_core:start_link([{domain(), host_type()}], []),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

passwords_as_records_are_still_supported(_C) ->
    %% given in mnesia there is a user with password in old scram format
    {U, S, P} = gen_user(),
    OldScramFormat = old_password_to_scram(P, 340),
    UserRecord = {passwd, {U, S}, OldScramFormat},
    mnesia:dirty_write(UserRecord),
    %% when we read the password via ejabberd_auth_internal:get_password/3
    NewScramMap = ejabberd_auth_internal:get_password(host_type(), U, S),
    %% then new map with sha key is returned
    ?assertMatch(#{iteration_count := _,
                   sha := #{salt       := _,
                            server_key := _,
                            stored_key := _}}, NewScramMap),
    %% even though in db there is old record stored
    OldScram = mnesia:dirty_read({passwd, {U, S}}),
    ?assertMatch([{passwd, _, {scram, _, _, _, _}}], OldScram).

passwords_in_plain_can_be_converted_to_scram(_C) ->
    %% Given there in mnesia there are users
    %% with plain text password
    {U, S, P} = gen_user(),
    UserRecord = {passwd, {U, S}, P},
    mnesia:dirty_write(UserRecord),
    %% and password in the old record format
    {U2, S2, P2} = gen_user(),
    OldScramFormat = old_password_to_scram(P2, 340),
    UserRecord2 = {passwd, {U2, S2}, OldScramFormat},
    mnesia:dirty_write(UserRecord2),
    %% when the migration function is run
    ejabberd_auth_internal:scram_passwords(),
    AfterMigrationPlain = mnesia:dirty_read({passwd, {U, S}}),
    %% then plain text passwords are converted to new map with sha and sha256
    ?assertMatch([{passwd, _,
                   #{iteration_count := _,
                     sha    := #{salt       := _,
                                 server_key := _,
                                 stored_key := _},
                     sha224 := #{salt       := _,
                                 server_key := _,
                                 stored_key := _},
                     sha256 := #{salt       := _,
                                 server_key := _,
                                 stored_key := _},
                     sha384 := #{salt       := _,
                                 server_key := _,
                                 stored_key := _},
                     sha512 := #{salt       := _,
                                 server_key := _,
                                 stored_key := _}}}], AfterMigrationPlain),
    %% and the old scram format remains the same
    AfterMigrationScram = mnesia:dirty_read({passwd, {U2, S2}}),
    ?assertMatch([{passwd, _,
                 #{iteration_count := _,
                   sha := #{salt       := _,
                            server_key := _,
                            stored_key := _}}}], AfterMigrationScram).

gen_user() ->
    {base64:encode(crypto:strong_rand_bytes(5)),
     domain(),
     base64:encode(crypto:strong_rand_bytes(6))}.

old_password_to_scram(Password, IterationCount) ->
    Salt = crypto:strong_rand_bytes(16),
    SaltedPassword = fast_scram:salted_password(sha, jid:resourceprep(Password), Salt, IterationCount),
    ClientKey = fast_scram:client_key(sha, SaltedPassword),
    StoredKey = fast_scram:stored_key(sha, ClientKey),
    ServerKey = fast_scram:server_key(sha, SaltedPassword),
    #scram{storedkey = base64:encode(StoredKey),
           serverkey = base64:encode(ServerKey),
           salt = base64:encode(Salt),
           iterationcount = IterationCount}.

domain() ->
    <<"server">>.

host_type() ->
    <<"test host type">>.
