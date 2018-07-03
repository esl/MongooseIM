%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Mnesia.
%%%
%%% All preferencies of each user are stored inside a single row.
%%% All operations are dirty.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_mnesia_prefs).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/4,
         remove_archive/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_prefs, {host_user :: {jid:server(), jid:user()},
                    default_mode,
                    always_rules :: list(),
                    never_rules  :: list()
                }).
-type mam_prefs() :: #mam_prefs{}.
-type behaviour() :: 'always' | 'never' | 'roster'.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: jid:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    mnesia:create_table(mam_prefs,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_prefs)}]),
    mnesia:add_table_copy(mam_prefs, node(), disc_copies),
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            start_pm(Host, Opts);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            start_muc(Host, Opts);
        false ->
            ok
    end.


-spec stop(Host :: jid:server()) -> any().
stop(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, pm, false) of
        true ->
            stop_pm(Host);
        false ->
            ok
    end,
    case gen_mod:get_module_opt(Host, ?MODULE, muc, false) of
        true ->
            stop_muc(Host);
        false ->
            ok
    end.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam

-spec start_pm(jid:server(), list()) -> 'ok'.
start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_pm(jid:server()) -> 'ok'.
stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

-spec start_muc(jid:server(), list()) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_muc(jid:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec get_behaviour(Default :: behaviour(), Host :: jid:server(),
    ArcID :: mod_mam:archive_id(), LocJID :: jid:jid(),
    RemJID :: jid:jid()) -> any().
get_behaviour(DefaultBehaviour, _Host,
              _ArcID,
              LocJID=#jid{},
              RemJID=#jid{}) ->
    SU = su_key(LocJID),
    case mnesia:dirty_read(mam_prefs, SU) of
        [] -> DefaultBehaviour;
        [User] -> get_behaviour(User, LocJID, RemJID)
    end.


-spec get_behaviour(mam_prefs(), LocJID :: jid:jid(),
                    RemJID :: jid:jid()) -> behaviour().
get_behaviour(#mam_prefs{default_mode = always, never_rules=NeverJIDs}, LocJID, RemJID) ->
    IsNever = match_jid(LocJID, RemJID, NeverJIDs),
    case IsNever of
        true -> never;
        false -> always
    end;
get_behaviour(#mam_prefs{default_mode = never, always_rules=AlwaysJIDs}, LocJID, RemJID) ->
    IsAlways = match_jid(LocJID, RemJID, AlwaysJIDs),
    case IsAlways of
        true -> always;
        false -> never
    end;
get_behaviour(#mam_prefs{default_mode = roster,
        never_rules=NeverJIDs, always_rules=AlwaysJIDs}, LocJID, RemJID) ->
    IsNever = match_jid(LocJID, RemJID, NeverJIDs),
    case IsNever of
        true -> never;
        false ->
            IsAlways = match_jid(LocJID, RemJID, AlwaysJIDs),
            case IsAlways of
                true -> always;
                false -> roster
            end
    end.


-spec set_prefs(Result :: any(), Host :: jid:server(),
                ArcID :: mod_mam:archive_id(), ArcJID :: jid:jid(),
                DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [jid:literal_jid()],
                NeverJIDs :: [jid:literal_jid()]) -> any().
set_prefs(_Result, _Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    try
        set_prefs1(ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs)
    catch _Type:Error ->
        {error, Error}
    end.

set_prefs1(_ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    SU = su_key(ArcJID),
    NewARules = lists:usort(rules(ArcJID, AlwaysJIDs)),
    NewNRules = lists:usort(rules(ArcJID, NeverJIDs)),
    User = #mam_prefs{
        host_user = SU,
        default_mode = DefaultMode,
        always_rules = NewARules,
        never_rules = NewNRules
    },
    mnesia:sync_dirty(fun() ->
            mnesia:write(User)
        end),
    ok.


-spec get_prefs(mod_mam:preference(), _Host :: jid:server(),
                _ArcId :: mod_mam:archive_id(), ArcJID :: jid:jid()
                ) -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, _Host, _ArcID, ArcJID) ->
    SU = su_key(ArcJID),
    case mnesia:dirty_read(mam_prefs, SU) of
        [] ->
            {GlobalDefaultMode, [], []};
        [#mam_prefs{default_mode=DefaultMode,
                    always_rules=ARules, never_rules=NRules}] ->
            AlwaysJIDs = jids(ArcJID, ARules),
            NeverJIDs = jids(ArcJID, NRules),
            {DefaultMode, AlwaysJIDs, NeverJIDs}
    end.

%% #rh
remove_archive(Host, ArcID, ArcJID) ->
    remove_archive(ok, Host, ArcID, ArcJID).

remove_archive(Acc, _Host, _ArcID, ArcJID) ->
    SU = su_key(ArcJID),
    mnesia:sync_dirty(fun() ->
            mnesia:delete(mam_prefs, SU, write)
        end),
    Acc.

%% ----------------------------------------------------------------------
%% Helpers

-spec su_key(jid:jid()) -> jid:simple_bare_jid().
su_key(#jid{lserver=LocLServer, luser=LocLUser}) ->
    {LocLServer, LocLUser}.


-spec jids(jid:jid(),
    [jid:literal_jid() | jid:simple_bare_jid() | jid:simple_jid()]
    ) -> [jid:literal_jid()].
jids(ArcJID, Rules) ->
    [jid:to_binary(rule_to_jid(ArcJID, Rule)) || Rule <- Rules].


-spec rule_to_jid(jid:jid(),
    jid:luser() | jid:simple_bare_jid() | jid:simple_jid()
    ) -> jid:simple_jid().
rule_to_jid(#jid{lserver=LServer}, RemLUser) when is_binary(RemLUser) ->
    {RemLUser, LServer, <<>>};
rule_to_jid(_ArcJID, {RemLServer, RemLUser, RemLResource}) ->
    {RemLUser, RemLServer, RemLResource};
rule_to_jid(_ArcJID, {RemLServer, RemLUser}) ->
    {RemLUser, RemLServer, <<>>}.


-spec rules(jid:jid(), [jid:literal_jid()]) ->
    [jid:literal_jid() | jid:simple_bare_jid() | jid:simple_jid()].
rules(ArcJID, BinJIDs) ->
    [rule(ArcJID, jid:from_binary(BinJID)) || BinJID <- BinJIDs].


-spec rule(jid:jid(), jid:jid()) ->
    jid:literal_jid() | jid:simple_bare_jid() | jid:simple_jid().
rule(#jid{lserver=LServer}, #jid{lserver=LServer, luser=RemLUser, lresource = <<>>}) ->
    RemLUser;
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource = <<>>}) ->
    {RemLServer, RemLUser};
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource=RemLResource}) ->
    {RemLServer, RemLUser, RemLResource}.


-spec is_bare_jid(jid:jid()) -> boolean().
is_bare_jid(#jid{lresource = <<>>}) -> true;
is_bare_jid(_)                      -> false.


-spec match_jid(jid:jid(), jid:jid(), [any()]) -> boolean().
match_jid(ArcJID, JID, JIDs) ->
    case is_bare_jid(JID) of
    true ->
        ordsets:is_element(rule(ArcJID, JID), JIDs);
    false ->
        BareJID = jid:to_bare(JID),
        ordsets:is_element(rule(ArcJID, BareJID), JIDs)
            orelse
        ordsets:is_element(rule(ArcJID, JID), JIDs)
    end.
