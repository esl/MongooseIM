-module(auth_helper).
-export([assert_event/2]).

-include_lib("jid/include/jid.hrl").
-import(domain_helper, [host_type/0]).

assert_event(EventName, BinJid)
    when EventName =:= auth_unregister_user; EventName =:= auth_register_user ->
    #jid{luser = LUser, lserver = LServer} = jid:from_binary(BinJid),
    F = fun(M) ->
            M =:= #{count => 1, user => LUser, server => LServer}
        end,
    instrument_helper:assert_one(EventName, #{host_type => host_type()}, F);
assert_event(EventName, BinJid)
    when EventName =:= auth_authorize ->
    #jid{lserver = LServer} = jid:from_binary(BinJid),
    F = fun(#{time := Time, count := 1, server := Server}) ->
           (Time > 0) and (Server =:= LServer)
        end,
    %% Note: this could match events from other tests because there is no user name
    instrument_helper:assert(EventName, #{host_type => host_type()}, F);
assert_event(EventName, BinJid) ->
    #jid{luser = LUser, lserver = LServer} = jid:from_binary(BinJid),
    F = fun(#{time := Time, count := 1, user := User, server := Server}) ->
           (Time > 0) and (User =:= LUser) and (Server =:= LServer)
        end,
    instrument_helper:assert_one(EventName, #{host_type => host_type()}, F).
