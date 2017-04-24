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
-include("jlib.hrl").
-behaviour(mod_roster).

%% API
-export([init/2,
         transaction/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_entry/3,
         get_roster_entry/4,
         get_roster_by_jid_t/3,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         get_roster_by_jid_with_groups_t/3,
         remove_user/2,
         update_roster_t/4,
         del_roster_t/3,
         read_subscription_and_groups/3]).

-export([raw_to_record/2]).

-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    mnesia:create_table(roster,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, roster)}]),
    mnesia:create_table(roster_version,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, roster_version)}]),
    mnesia:add_table_index(roster, us),
    mnesia:add_table_index(roster_version, us),
    ok.

-spec transaction(LServer :: ejabberd:lserver(), F :: fun()) ->
    {aborted, Reason :: any()} | {atomic, Result :: any()}.
transaction(_LServer, F) ->
    mnesia:transaction(F).

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver()) -> binary() | error.
read_roster_version(LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_read(roster_version, US) of
        [#roster_version{version = V}] -> V;
        [] -> error
    end.

-spec write_roster_version(LUser :: ejabberd:luser(),
                           LServer :: ejabberd:lserver(),
                           InTransaction :: boolean(),
                           Ver :: binary()) -> ok.
write_roster_version(LUser, LServer, true, Ver) ->
    mnesia:write(#roster_version{us = {LUser, LServer}, version = Ver});
write_roster_version(LUser, LServer, _, Ver) ->
    mnesia:dirty_write(#roster_version{us = {LUser, LServer}, version = Ver}).

get_roster(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(roster, US,
                                       #roster.us)
    of
        Items  when is_list(Items)-> Items;
        _ -> []
    end.

get_roster_entry(LUser, LServer, LJID) ->
    LowerJID = jid:to_lower(LJID),
    case mnesia:dirty_read({roster, {LUser, LServer, LowerJID}}) of
        [] ->
            does_not_exist;
        [I] ->
            I#roster{jid = LJID, name = <<"">>,
                xs = []}
    end.

get_roster_entry(LUser, LServer, LJID, full) ->
    get_roster_entry(LUser, LServer, LJID).

%%get_roster_entry_t(LUser, LServer, LJID) ->
%%    case mnesia:read({roster, {LUser, LServer, LJID}}) of
%%        [] ->
%%            does_not_exist;
%%        [I] ->
%%            I#roster{jid = LJID, name = <<"">>,
%%                xs = []}
%%    end.

%%get_roster_entry_t(LUser, LServer, LJID, full) ->
%%    get_roster_entry_t(LUser, LServer, LJID).

get_roster_by_jid_t(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
        [] ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        [I] ->
            I#roster{jid = LJID, name = <<"">>, groups = [],
                     xs = []}
    end.

get_subscription_lists(_, LUser, LServer) ->
    US = {LUser, LServer},
    case mnesia:dirty_index_read(roster, US, #roster.us) of
        Items when is_list(Items) -> Items;
        _ -> []
    end.

roster_subscribe_t(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    case mnesia:read({roster, {LUser, LServer, LJID}}) of
        [] ->
            #roster{usj = {LUser, LServer, LJID},
                    us = {LUser, LServer}, jid = LJID};
        [I] -> I
    end.

remove_user(LUser, LServer) ->
    US = {LUser, LServer},
    F = fun () ->
                lists:foreach(fun (R) -> mnesia:delete_object(R) end,
                              mnesia:index_read(roster, US, #roster.us))
        end,
    mnesia:transaction(F).

update_roster_t(_LUser, _LServer, _LJID, Item) ->
    mnesia:write(Item).

del_roster_t(LUser, LServer, LJID) ->
    mnesia:delete({roster, {LUser, LServer, LJID}}).


read_subscription_and_groups(LUser, LServer, LJID) ->
    case catch mnesia:dirty_read(roster,
                                 {LUser, LServer, LJID})
    of
        [#roster{subscription = Subscription,
                 groups = Groups}] ->
            {Subscription, Groups};
        _ -> error
    end.

raw_to_record(_, Item) -> Item.

