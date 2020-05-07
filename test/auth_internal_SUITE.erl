-module(auth_internal_SUITE).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("scram.hrl").

all() ->
    [passwords_as_records_are_still_supported,
     passwords_in_plain_can_be_converted_to_scram].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ejabberd_auth_internal:start(<<"server">>),
    C.

end_per_suite(_C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]).

passwords_as_records_are_still_supported(_C) ->
    %% given in mnesia there is a user with password in old scram format
    {U, S, P} = gen_user(),
    OldScramFormat = old_password_to_scram(P, 340),
    UserRecord = {passwd, {U, S}, OldScramFormat},
    mnesia:dirty_write(UserRecord),
    %% when we read the password via the `ejabberd_auth_internal:get_password/2
    NewScramMap = ejabberd_auth_internal:get_password(U, S),
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
    meck:new(ejabberd_auth),
    meck:expect(ejabberd_auth, get_opt, fun(_, scram_iterations, D) -> D end),
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
                            stored_key := _}}}], AfterMigrationScram),
    meck:unload().

gen_user() ->
    {base64:encode(crypto:strong_rand_bytes(5)),
     <<"server">>,
     base64:encode(crypto:strong_rand_bytes(6))}.

old_password_to_scram(Password, IterationCount) ->
    Salt = crypto:strong_rand_bytes(16),
    SaltedPassword = mongoose_scram:salted_password(sha, Password, Salt, IterationCount),
    ClientKey = mongoose_scram:client_key(sha, SaltedPassword),
    StoredKey = mongoose_scram:stored_key(sha, ClientKey),
    ServerKey = mongoose_scram:server_key(sha, SaltedPassword),
    #scram{storedkey = base64:encode(StoredKey),
           serverkey = base64:encode(ServerKey),
           salt = base64:encode(Salt),
           iterationcount = IterationCount}.
