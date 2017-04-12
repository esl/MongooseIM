-module(mod_global_distrib_cassandra).

-behaviour(mod_global_distrib).

-export([prepared_queries/0, put_session/2, get_session/1, delete_session/2]).

prepared_queries() ->
    [
        {put, "INSERT INTO sessions (jid, stamp, host) VALUES (?, ?, ?)"},
        {get, "SELECT * FROM sessions WHERE jid = ? LIMIT 1"},
        {delete, "DELETE FROM sessions WHERE jid = ? AND host = ?"}
    ].

put_session(Jid, Host) ->
    Stamp = p1_time_compat:system_time(milli_seconds),
    case mongoose_cassandra:cql_write(global, undefined, ?MODULE, put,
                                      [#{jid => Jid, stamp => Stamp, host => Host}]) of
        ok -> ok;
        _ -> error
    end.

get_session(Jid) ->
    case mongoose_cassandra:cql_read(global, undefined, ?MODULE, get, #{jid => Jid}) of
        {ok, [#{host := Host}]} -> {ok, Host};
        _ -> error
    end.

delete_session(Jid, Host) ->
    case mongoose_cassandra:cql_write(global, undefined, ?MODULE, delete,
                                      [#{jid => Jid, host => Host}]) of
        ok -> ok;
        _ -> error
    end.
