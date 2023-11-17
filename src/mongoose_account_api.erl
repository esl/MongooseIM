%% @doc Provide an interface for frontends (like graphql or ctl) to manage accounts.
-module(mongoose_account_api).

-export([list_users/1,
         count_users/1,
         register_user/3,
         register_generated_user/2,
         unregister_user/1,
         unregister_user/2,
         ban_account/2,
         change_password/2,
         change_password/3,
         check_account/1,
         check_password/2,
         check_password_hash/3,
         import_users/1]).

-type register_result() :: {ok | exists | invalid_jid | cannot_register |
                            limit_per_domain_exceeded, iolist()}.

-type unregister_result() :: {ok | not_allowed | invalid_jid | user_does_not_exist, string()}.

-type change_password_result() :: {ok | empty_password | not_allowed | invalid_jid |
                                   user_does_not_exist, string()}.

-type check_password_result() :: {ok | incorrect | user_does_not_exist, string()}.

-type check_password_hash_result() :: {ok | incorrect | wrong_user | wrong_method, string()}.

-type check_account_result() :: {ok | user_does_not_exist, string()}.

-type list_user_result() :: {ok, [jid:literal_jid()]} | {domain_not_found, string()}.

-type count_user_result() :: {ok, non_neg_integer()} | {domain_not_found, string()}.

-export_type([register_result/0,
              unregister_result/0,
              change_password_result/0,
              check_password_result/0,
              check_password_hash_result/0,
              check_account_result/0,
              list_user_result/0]).

%% API

-spec list_users(jid:server()) -> list_user_result().
list_users(Domain) ->
    PrepDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_domain_host_type(PrepDomain) of
        {ok, _} ->
            Users = ejabberd_auth:get_vh_registered_users(PrepDomain),
            SUsers = lists:sort(Users),
            {ok, [jid:to_binary(US) || US <- SUsers]};
        {error, not_found} ->
            {domain_not_found, "Domain does not exist"}
    end.

-spec count_users(jid:server()) -> count_user_result().
count_users(Domain) ->
    PrepDomain = jid:nameprep(Domain),
    case mongoose_domain_api:get_domain_host_type(PrepDomain) of
        {ok, _} ->
            UserCount = ejabberd_auth:get_vh_registered_users_number(PrepDomain),
            {ok, UserCount};
        {error, not_found} ->
            {domain_not_found, "Domain does not exist"}
    end.

-spec register_generated_user(jid:server(), binary()) -> {register_result(), jid:literal_jid()}.
register_generated_user(Host, Password) ->
    Username = generate_username(),
    JID = jid:to_binary({Username, Host}),
    {register_user(Username, Host, Password), JID}.

-spec register_user(jid:user(), jid:server(), binary()) -> register_result().
register_user(User, Host, Password) ->
    JID = jid:make_bare(User, Host),
    case ejabberd_auth:try_register(JID, Password) of
        {error, exists} ->
            String =
                io_lib:format("User ~s already registered at node ~p",
                              [jid:to_binary(JID), node()]),
            {exists, String};
        {error, invalid_jid} ->
            String = io_lib:format("Invalid JID ~s@~s", [User, Host]),
            {invalid_jid, String};
        {error, limit_per_domain_exceeded} ->
            String = io_lib:format("User limit has been exceeded for domain ~s", [Host]),
            {limit_per_domain_exceeded, String};
        {error, Reason} ->
            String =
                io_lib:format("Can't register user ~s at node ~p: ~p",
                              [jid:to_binary(JID), node(), Reason]),
            {cannot_register, String};
        _ ->
            {ok, io_lib:format("User ~s successfully registered", [jid:to_binary(JID)])}
    end.

-spec unregister_user(jid:user(), jid:server()) -> unregister_result().
unregister_user(User, Host) ->
    JID = jid:make_bare(User, Host),
    unregister_user(JID).

-spec unregister_user(jid:jid()) -> unregister_result().
unregister_user(JID) ->
    case ejabberd_auth:remove_user(JID) of
        ok ->
            {ok, io_lib:format("User ~s successfully unregistered", [jid:to_binary(JID)])};
        error ->
            {invalid_jid, "Invalid JID"};
        {error, _} ->
            {not_allowed, "User does not exist or you are not authorized properly"}
    end.

-spec change_password(jid:user(), jid:server(), binary()) -> change_password_result().
change_password(User, Host, Password) ->
    JID = jid:make_bare(User, Host),
    change_password(JID, Password).

-spec change_password(jid:jid(), binary()) -> change_password_result().
change_password(JID, Password) ->
    Result = ejabberd_auth:set_password(JID, Password),
    format_change_password(Result).

-spec check_account(jid:jid()) -> check_account_result().
check_account(JID) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, io_lib:format("User ~s exists", [jid:to_binary(JID)])};
        false ->
            {user_does_not_exist, io_lib:format("User ~s does not exist", [jid:to_binary(JID)])}
    end.

-spec check_password(jid:jid(), binary()) -> check_password_result().
check_password(JID, Password) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            case ejabberd_auth:check_password(JID, Password) of
                true ->
                    {ok, io_lib:format("Password '~s' for user ~s is correct",
                                       [Password, jid:to_binary(JID)])};
                false ->
                    {incorrect, io_lib:format("Password '~s' for user ~s is incorrect",
                                              [Password, jid:to_binary(JID)])}
            end;
        false ->
            {user_does_not_exist,
            io_lib:format("Password ~s for user ~s is incorrect because this user does not"
                          " exist", [Password, jid:to_binary(JID)])}
    end.

-spec check_password_hash(jid:jid(), string(), string()) -> check_password_hash_result().
check_password_hash(JID, PasswordHash, HashMethod) ->
    AccountPass = ejabberd_auth:get_password_s(JID),
    AccountPassHash = case HashMethod of
        "md5" -> get_md5(AccountPass);
        "sha" -> get_sha(AccountPass);
        _ -> undefined
    end,
    case {AccountPass, AccountPassHash} of
        {<<>>, _} ->
            {wrong_user, "User does not exist or using SCRAM password"};
        {_, undefined} ->
            Msg = io_lib:format("Given hash method `~s` is not supported. Try `md5` or `sha`",
                                [HashMethod]),
            {wrong_method, Msg};
        {_, PasswordHash} ->
            {ok, "Password hash is correct"};
        _->
            {incorrect, "Password hash is incorrect"}
    end.

-spec import_users(file:filename()) -> {ok, #{binary() => [{ok, jid:jid() | binary()}]}}
                                     | {file_not_found, binary()}.
import_users(Filename) ->
    case mongoose_import_users:run(Filename) of
        {ok, Summary} ->
            {ok, maps:fold(
                fun(Reason, List, Map) ->
                    List2 = [{ok, El} || El <- List],
                    maps:put(from_reason(Reason), List2, Map)
                end,
                #{<<"status">> => <<"Completed">>},
                Summary)};
        {error, file_not_found} ->
            {file_not_found, <<"File not found">>}
    end.

-spec from_reason(mongoose_import_users:reason()) -> binary().
from_reason(ok) -> <<"created">>;
from_reason(exists) -> <<"existing">>;
from_reason(not_allowed) -> <<"notAllowed">>;
from_reason(invalid_jid) -> <<"invalidJID">>;
from_reason(null_password) -> <<"emptyPassword">>;
from_reason(bad_csv) -> <<"invalidRecord">>.

-spec ban_account(jid:jid(), binary()) -> change_password_result().
ban_account(JID, Reason) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            mongoose_session_api:kick_sessions(JID, Reason),
            case set_random_password(JID, Reason) of
                ok ->
                    {ok, io_lib:format("User ~s successfully banned with reason: ~s",
                                    [jid:to_binary(JID), Reason])};
                ErrResult ->
                    format_change_password(ErrResult)
            end;
        false ->
            {user_does_not_exist, io_lib:format("User ~s does not exist", [jid:to_binary(JID)])}
    end.

%% Internal

format_change_password(ok) ->
    {ok, "Password changed"};
format_change_password({error, empty_password}) ->
    {empty_password, "Empty password"};
format_change_password({error, not_allowed}) ->
    {not_allowed, "User does not exist or you are not authorized properly"};
format_change_password({error, invalid_jid}) ->
    {invalid_jid, "Invalid JID"}.

-spec set_random_password(JID, Reason) -> Result when
      JID :: jid:jid(),
      Reason :: binary(),
      Result :: ok | {error, any()}.
set_random_password(JID, Reason) ->
    NewPass = build_random_password(Reason),
    ejabberd_auth:set_password(JID, NewPass).

-spec build_random_password(Reason :: binary()) -> binary().
build_random_password(Reason) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    Date = iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                                          [Year, Month, Day, Hour, Minute, Second])),
    RandomString = mongoose_bin:gen_from_crypto(),
    <<"BANNED_ACCOUNT--", Date/binary, "--", RandomString/binary, "--", Reason/binary>>.

-spec generate_username() -> binary().
generate_username() ->
    mongoose_bin:join([mongoose_bin:gen_from_timestamp(),
                       mongoose_bin:gen_from_crypto()], $-).

-spec get_md5(binary()) -> string().
get_md5(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(md5, AccountPass))]).

-spec get_sha(binary()) -> string().
get_sha(AccountPass) ->
    lists:flatten([io_lib:format("~.16B", [X])
                   || X <- binary_to_list(crypto:hash(sha, AccountPass))]).
