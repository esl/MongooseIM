%%%----------------------------------------------------------------------
%%% File    : mod_roster_riak.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : mod_roster Riak backend with quasi-transaction support
%%%           (see comment before transaction/1)
%%%
%%% MongooseIM, Copyright (C) 2015      Erlang Solutions Ltd.
%%%
%%%----------------------------------------------------------------------
-module(mod_roster_riak).

-include("mod_roster.hrl").
-include("jlib.hrl").

-behaviour(mod_roster).

%% API
-export([init/2,
         transaction/2,
         read_roster_version/2,
         write_roster_version/4,
         get_roster/2,
         get_roster_by_jid_t/3,
         get_subscription_lists/3,
         roster_subscribe_t/4,
         get_roster_by_jid_with_groups_t/3,
         remove_user/2,
         update_roster_t/4,
         del_roster_t/3,
         read_subscription_and_groups/3,
         raw_to_record/2]).

-define(ROSTER_BUCKET(LServer), {<<"rosters">>, LServer}).
-define(VER_BUCKET(LServer), {<<"roster_versions">>, LServer}).

-spec init(ejabberd:server(), list()) -> ok.
init(_Host, _Opts) ->
    ok. % Common Riak pool is used

%% WARNING: Riak does not support *real* transactions, so we are in fact applying
%% all accumulated changes with no rollback support so it is possible to end up with
%% inconsistent state e.g. if Riak connectivity goes down in the middle of application.
-spec transaction(LServer :: ejabberd:lserver(), F :: fun()) ->
    {aborted, Reason :: any()} | {atomic, Result :: any()}.
transaction(_LServer, F) ->
    put(riak_roster_t, []),
    put(riak_version_t, []),
    try F() of
        Result ->
            %% Applying
            apply_t_roster(),
            apply_t_version(),
            put(riak_roster_t, []),
            {atomic, Result}
    catch
        _:Reason -> {aborted, Reason}
    after
        lists:foreach(fun(Key) -> erase({riak_roster_t, Key}) end, erase(riak_roster_t)),
        erase(riak_version_t)
    end.

%% --------------------- Inside "transactions" --------------------------------

get_roster_by_jid_t(LUser, LServer, LJID) ->
    case riakc_map:find({jid:to_binary(LJID), register}, get_t_roster(LUser, LServer)) of
        {ok, ItemReg} ->
            UnpackedItem = unpack_item(ItemReg),
            UnpackedItem#roster{ jid = LJID, name = <<>>, groups = [], xs = [] };
        error ->
            #roster{usj = {LUser, LServer, LJID}, us = {LUser, LServer}, jid = LJID}
    end.

roster_subscribe_t(LUser, LServer, LJID, Item) ->
    set_t_roster(LUser, LServer, LJID, Item).

get_roster_by_jid_with_groups_t(LUser, LServer, LJID) ->
    case riakc_map:find({jid:to_binary(LJID), register}, get_t_roster(LUser, LServer)) of
        {ok, ItemReg} -> unpack_item(ItemReg);
        error -> #roster{usj = {LUser, LServer, LJID}, us = {LUser, LServer}, jid = LJID}
    end.

update_roster_t(LUser, LServer, LJID, Item) ->
    set_t_roster(LUser, LServer, LJID, Item).

del_roster_t(LUser, LServer, LJID) ->
    del_t_roster(LUser, LServer, LJID).

%% --------------------- Outside "transactions" -------------------------------

-spec read_roster_version(ejabberd:luser(), ejabberd:lserver()) -> binary() | error.
read_roster_version(LUser, LServer) ->
    case mongoose_riak:get(?VER_BUCKET(LServer), LUser) of
        {ok, VerObj} -> riakc_obj:get_value(VerObj);
        _ -> error
    end.

write_roster_version(LUser, LServer, false, Ver) ->
    VerObj = case mongoose_riak:get(?VER_BUCKET(LServer), LUser) of
                 {ok, VerObj1} -> riakc_obj:update_value(VerObj1, Ver);
                 _ -> riakc_obj:new(?VER_BUCKET(LServer), LUser, Ver)
             end,
    mongoose_riak:put(VerObj);
write_roster_version(LUser, LServer, true, Ver) ->
    Versions1 = get(riak_version_t),
    put(riak_version_t, lists:keystore({LUser, LServer}, 1, Versions1, {{LUser, LServer}, Ver})).

get_roster(LUser, LServer) ->
    case mongoose_riak:fetch_type(?ROSTER_BUCKET(LServer), LUser) of
        {ok, RosterMap} ->
            riakc_map:fold(
              fun({_, register}, ItemReg, Acc) ->
                      [unpack_item(ItemReg) | Acc]
              end, [], RosterMap);
        _ ->
            []
    end.

get_subscription_lists(_, LUser, LServer) ->
    get_roster(LUser, LServer).

remove_user(LUser, LServer) ->
    mongoose_riak:delete(?VER_BUCKET(LServer), LUser),
    mongoose_riak:delete(?ROSTER_BUCKET(LServer), LUser),
    {atomic, ok}.

read_subscription_and_groups(LUser, LServer, LJID) ->
    try
        {ok, RosterMap} = mongoose_riak:fetch_type(?ROSTER_BUCKET(LServer), LUser),
        ItemReg = riakc_map:fetch({jid:to_binary(LJID), register}, RosterMap),
        #roster{ subscription = Subscription, groups = Groups } = unpack_item(ItemReg),
        {Subscription, Groups}
    catch
        _:_ ->
            error
    end.

raw_to_record(_, Item) -> Item.

%% --------------------- Helpers --------------------------------

-spec unpack_item(ItemReg :: binary()) -> mod_roster:roster().
unpack_item(ItemReg) ->
    binary_to_term(ItemReg).

-spec get_t_roster(LUser :: ejabberd:luser(), LServer :: ejabberd:lserver()) ->
    riakc_map:crdt_map().
get_t_roster(LUser, LServer) ->
    case get({riak_roster_t, {LUser, LServer}}) of
        undefined ->
            case mongoose_riak:fetch_type(?ROSTER_BUCKET(LServer), LUser) of
                {ok, RosterMap} -> put({riak_roster_t, {LUser, LServer}}, RosterMap);
                _ -> put({riak_roster_t, {LUser, LServer}}, riakc_map:new())
            end,
            put(riak_roster_t, [{LUser, LServer} | get(riak_roster_t)]),
            get({riak_roster_t, {LUser, LServer}});
        RosterMap ->
            RosterMap
    end.

-spec set_t_roster(LUser :: ejabberd:luser(),
                   LServer :: ejabberd:lserver(),
                   LJID :: ejabberd:simple_jid(),
                   Item :: mod_roster:roster()) -> any().
set_t_roster(LUser, LServer, LJID, Item) ->
    RosterMap1 = get_t_roster(LUser, LServer),
    put({riak_roster_t, {LUser, LServer}},
        riakc_map:update({jid:to_binary(LJID), register},
                         fun(R) -> riakc_register:set(term_to_binary(Item), R) end, RosterMap1)).

-spec del_t_roster(LUser :: ejabberd:luser(),
                   LServer :: ejabberd:lserver(),
                   LJID :: ejabberd:simple_jid()) -> any().
del_t_roster(LUser, LServer, LJID) ->
    RosterMap1 = get_t_roster(LUser, LServer),
    RosterMap = case catch riakc_map:erase({jid:to_binary(LJID), register}, RosterMap1) of
                    context_required -> RosterMap1;
                    RosterMap2 -> RosterMap2
                end,
    put({riak_roster_t, {LUser, LServer}}, RosterMap).

-spec apply_t_roster() -> ok.
apply_t_roster() ->
    lists:foreach(
      fun({LUser, LServer} = LUS) ->
              RosterMap = erase({riak_roster_t, LUS}),
              case riakc_map:to_op(RosterMap) of
                  undefined -> ok;
                  ToOp -> catch mongoose_riak:update_type(?ROSTER_BUCKET(LServer), LUser, ToOp)
              end
      end, get(riak_roster_t)).

-spec apply_t_version() -> ok.
apply_t_version() ->
    lists:foreach(
      fun({{LUser, LServer}, NewVer}) ->
              catch write_roster_version(LUser, LServer, false, NewVer)
      end, get(riak_version_t)).

