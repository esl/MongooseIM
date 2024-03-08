%%%----------------------------------------------------------------------
%%% File    : mod_roster_mnesia.erl
%%% Author  : Michał Piotrowski <michal.piotrowski@erlang-solutions.com>
%%% Purpose : mod_roster mnesia backend
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------
-module(mod_roster_mnesia).

-include("mod_roster.hrl").

-behaviour(mod_roster_backend).

%% API
-export([init/2,
         transaction/2,
         read_roster_version/3,
         write_roster_version/5,
         get_roster/3,
         get_roster_entry/6,
         get_subscription_lists/3,
         roster_subscribe_t/2,
         update_roster_t/2,
         del_roster_t/4,
         remove_user_t/3]).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_HostType, _Opts) ->
    mongoose_mnesia:create_table(roster,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, roster)}]),
    mongoose_mnesia:create_table(roster_version,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, roster_version)}]),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ok.

-spec transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()}.
transaction(_HostType, F) ->
    mnesia:transaction(F).

-spec read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    binary() | error.
read_roster_version(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
        [#roster_version{version = V}] -> V;
        [] -> error
    end.

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                               mod_roster:transaction_state(), mod_roster:version()) -> ok.
write_roster_version(_HostType, LUser, LServer, TransactionState, Ver) ->
    write(#roster_version{us = {LUser, LServer}, version = Ver}, TransactionState).

-spec get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_roster(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
        Items  when is_list(Items)-> Items;
        _ -> []
    end.

-spec get_roster_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact(),
                           mod_roster:transaction_state(), mod_roster:entry_format()) ->
    mod_roster:roster() | does_not_exist.
get_roster_entry(_HostType, LUser, LServer, LJID, TransactionState, _Format) ->
    LowerJID = jid:to_lower(LJID),
    case read({roster, {{LUser, LServer}, LowerJID}}, TransactionState) of
        [] ->
            does_not_exist;
        [I] ->
            I#roster{jid = LJID, xs = []}
    end.

-spec get_subscription_lists(mongoose_acc:t(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_subscription_lists(_, LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(roster, US, #roster.us) of
        Items when is_list(Items) -> Items;
        _ -> []
    end.

-spec roster_subscribe_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
roster_subscribe_t(_HostType, Item) ->
    mnesia:write(Item).

-spec update_roster_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
update_roster_t(_HostType, Item) ->
    mnesia:write(Item).

-spec del_roster_t(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact()) -> ok.
del_roster_t(_HostType, LUser, LServer, LJID) ->
    mnesia:delete({roster, {{LUser, LServer}, LJID}}).

-spec remove_user_t(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok.
remove_user_t(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    lists:foreach(fun (R) -> mnesia:delete_object(R) end,
                  mnesia:index_read(roster, US, #roster.us)),
    ok.

%% Helpers

write(Record, in_transaction) -> mnesia:write(Record);
write(Record, no_transaction) -> mnesia:dirty_write(Record).

read(TabKey, in_transaction) -> mnesia:read(TabKey);
read(TabKey, no_transaction) -> mnesia:dirty_read(TabKey).
