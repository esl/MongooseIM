-module(accounts_helper).

-export([prepare_user_created_at/0, set_user_created_at/3]).

-import(distributed_helper, [mim/0, rpc/4]).

prepare_user_created_at() ->
    rpc(mim(), mongoose_rdbms, prepare, [accounts_helper_set_created_at,
                                         users,
                                         [server, username, created_at],
                                         <<"UPDATE users SET created_at = ? WHERE username = ? AND server = ?">>]).

set_user_created_at(Username, Server, Timestamp) ->
    {ok, HostType} = rpc(mim(), mongoose_domain_api, get_domain_host_type, [Server]),
    {updated, 1} = rpc(mim(), mongoose_rdbms, execute,
                       [HostType, accounts_helper_set_created_at, [Timestamp, Username, Server]]).
