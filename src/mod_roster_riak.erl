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
-include("mongoose.hrl").

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

-define(ROSTER_BUCKET(HostType, LServer),
        {get_opt(HostType, bucket_type, <<"rosters">>), LServer}).
-define(VER_BUCKET(HostType, LServer),
        {get_opt(HostType, version_bucket_type, <<"roster_versions">>), LServer}).

get_opt(HostType, Opt, Def) ->
    gen_mod:get_module_opt(HostType, mod_roster, Opt, Def).

%% --------------------- mod_roster backend API -------------------------------

-spec init(mongooseim:host_type(), list()) -> ok.
init(_HostType, _Opts) ->
    ok. % Common Riak pool is used

%% WARNING: Riak does not support *real* transactions, so we are in fact applying
%% all accumulated changes with no rollback support so it is possible to end up with
%% inconsistent state e.g. if Riak connectivity goes down in the middle of application.
-spec transaction(mongooseim:host_type(), fun(() -> any())) ->
    {aborted, any()} | {atomic, any()}.
transaction(HostType, F) ->
    put(riak_roster_t, []),
    put(riak_version_t, []),
    try F() of
        Result ->
            %% Applying
            apply_t_roster(HostType),
            apply_t_version(HostType),
            put(riak_roster_t, []),
            {atomic, Result}
    catch
        _:Reason -> {aborted, Reason}
    after
        lists:foreach(fun(Key) -> erase({riak_roster_t, Key}) end, erase(riak_roster_t)),
        erase(riak_version_t)
    end.

-spec read_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    binary() | error.
read_roster_version(HostType, LUser, LServer) ->
    case mongoose_riak:get(?VER_BUCKET(HostType, LServer), LUser) of
        {ok, VerObj} -> riakc_obj:get_value(VerObj);
        _ -> error
    end.

-spec write_roster_version(mongooseim:host_type(), jid:luser(), jid:lserver(),
                               mod_roster:transaction_state(), mod_roster:version()) -> ok.
write_roster_version(HostType, LUser, LServer, no_transaction, Ver) ->
    VerObj = case mongoose_riak:get(?VER_BUCKET(HostType, LServer), LUser) of
                 {ok, VerObj1} -> riakc_obj:update_value(VerObj1, Ver);
                 _ -> riakc_obj:new(?VER_BUCKET(HostType, LServer), LUser, Ver)
             end,
    mongoose_riak:put(VerObj);
write_roster_version(_HostType, LUser, LServer, in_transaction, Ver) ->
    Versions1 = get(riak_version_t),
    put(riak_version_t, lists:keystore({LUser, LServer}, 1, Versions1, {{LUser, LServer}, Ver})).

-spec get_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_roster(HostType, LUser, LServer) ->
    RosterMap = get_rostermap(HostType, LUser, LServer),
    riakc_map:fold(fun({_, register}, ItemReg, Acc) ->
                       [unpack_item(ItemReg) | Acc]
                   end,
                   [], RosterMap).

-spec get_roster_entry(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact(),
                           mod_roster:transaction_state(), mod_roster:entry_format()) ->
    mod_roster:roster() | does_not_exist.
get_roster_entry(HostType, LUser, LServer, LJID, TransactionState, _Format) ->
    RosterMap = case TransactionState of
                    in_transaction ->
                        get_t_roster(HostType, LUser, LServer);
                    no_transaction ->
                        get_rostermap(HostType, LUser, LServer)
                end,
    find_in_rostermap(LJID, RosterMap).

-spec get_subscription_lists(mongoose_acc:t(), jid:luser(), jid:lserver()) -> [mod_roster:roster()].
get_subscription_lists(Acc, LUser, LServer) ->
    HostType = mongoose_acc:host_type(Acc),
    get_roster(HostType, LUser, LServer).

roster_subscribe_t(HostType, Item = #roster{us = {LUser, LServer}, jid = LJID}) ->
    set_t_roster(HostType, LUser, LServer, LJID, Item).

-spec update_roster_t(mongooseim:host_type(), mod_roster:roster()) -> ok.
update_roster_t(HostType, Item = #roster{us = {LUser, LServer}, jid = LJID}) ->
    set_t_roster(HostType, LUser, LServer, LJID, Item).

-spec del_roster_t(mongooseim:host_type(), jid:luser(), jid:lserver(), mod_roster:contact()) -> ok.
del_roster_t(HostType, LUser, LServer, LJID) ->
    del_t_roster(HostType, LUser, LServer, LJID).

remove_user_t(HostType, LUser, LServer) ->
    mongoose_riak:delete(?VER_BUCKET(HostType, LServer), LUser),
    mongoose_riak:delete(?ROSTER_BUCKET(HostType, LServer), LUser),
    ok.

%% --------------------- Helpers --------------------------------

find_in_rostermap(LJID, RosterMap) ->
    case riakc_map:find({jid:to_binary(LJID), register}, RosterMap) of
        {ok, ItemReg} -> unpack_item(ItemReg);
        error -> does_not_exist
    end.

%% this is a transaction-less equivalent of get_t_roster
get_rostermap(HostType, LUser, LServer) ->
    case mongoose_riak:fetch_type(?ROSTER_BUCKET(HostType, LServer), LUser) of
        {ok, RMap} ->
            RMap;
        _ ->
            riakc_map:new()
    end.

-spec unpack_item(ItemReg :: binary()) -> mod_roster:roster().
unpack_item(ItemReg) ->
    binary_to_term(ItemReg).

-spec get_t_roster(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    riakc_map:crdt_map().
get_t_roster(HostType, LUser, LServer) ->
    case get({riak_roster_t, {LUser, LServer}}) of
        undefined ->
            case mongoose_riak:fetch_type(?ROSTER_BUCKET(HostType, LServer), LUser) of
                {ok, RosterMap} -> put({riak_roster_t, {LUser, LServer}}, RosterMap);
                _ -> put({riak_roster_t, {LUser, LServer}}, riakc_map:new())
            end,
            put(riak_roster_t, [{LUser, LServer} | get(riak_roster_t)]),
            get({riak_roster_t, {LUser, LServer}});
        RosterMap ->
            RosterMap
    end.

-spec set_t_roster(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   LJID :: jid:simple_jid(),
                   Item :: mod_roster:roster()) -> any().
set_t_roster(HostType, LUser, LServer, LJID, Item) ->
    RosterMap1 = get_t_roster(HostType, LUser, LServer),
    put({riak_roster_t, {LUser, LServer}},
        riakc_map:update({jid:to_binary(LJID), register},
                         fun(R) -> riakc_register:set(term_to_binary(Item), R) end, RosterMap1)).

-spec del_t_roster(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   LJID :: jid:simple_jid()) -> any().
del_t_roster(HostType, LUser, LServer, LJID) ->
    RosterMap1 = get_t_roster(HostType, LUser, LServer),
    RosterMap = case catch riakc_map:erase({jid:to_binary(LJID), register}, RosterMap1) of
                    context_required -> RosterMap1;
                    RosterMap2 -> RosterMap2
                end,
    put({riak_roster_t, {LUser, LServer}}, RosterMap).

-spec apply_t_roster(mongooseim:host_type()) -> ok.
apply_t_roster(HostType) ->
    lists:foreach(
      fun({LUser, LServer} = LUS) ->
              RosterMap = erase({riak_roster_t, LUS}),
              case riakc_map:to_op(RosterMap) of
                  undefined -> ok;
                  ToOp -> catch mongoose_riak:update_type(
                                  ?ROSTER_BUCKET(HostType, LServer), LUser, ToOp)
              end
      end, get(riak_roster_t)).

-spec apply_t_version(mongooseim:host_type()) -> ok.
apply_t_version(HostType) ->
    lists:foreach(
      fun({{LUser, LServer}, NewVer}) ->
              catch write_roster_version(HostType, LUser, LServer, no_transaction, NewVer)
      end, get(riak_version_t)).
