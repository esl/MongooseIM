%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Mnesia.
%%%
%%% All preferencies of each user are stored inside a single row.
%%% All operations are dirty.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_mnesia_dirty_prefs).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-behaviour(ejabberd_gen_mam_prefs).
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/3]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_prefs, {arc_id :: mod_mam:archive_id(),
                    host_user :: ejabberd:user(),
                    default_mode,
                    always_rules :: list(),
                    never_rules  :: list()
                }).
-type mam_prefs() :: #mam_prefs{}.
-type behaviour() :: 'always' | 'never' | 'roster'.

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
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


-spec stop(Host :: ejabberd:server()) -> any().
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

-spec start_pm(ejabberd:server(), list()) -> 'ok'.
start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_pm(ejabberd:server()) -> 'ok'.
stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

-spec start_muc(ejabberd:server(), list()) -> 'ok'.
start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


-spec stop_muc(ejabberd:server()) -> 'ok'.
stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

-spec get_behaviour(Default :: behaviour(), Host :: ejabberd:server(),
    ArcID :: mod_mam:archive_id(), LocJID :: ejabberd:jid(),
    RemJID :: ejabberd:jid()) -> any().
get_behaviour(DefaultBehaviour, _Host,
              ArcID,
              LocJID=#jid{},
              RemJID=#jid{}) ->
    case mnesia:dirty_read(mam_prefs, ArcID) of
        [] -> DefaultBehaviour;
        [User] -> get_behaviour(User, LocJID, RemJID)
    end.


-spec get_behaviour(mam_prefs(), LocJID :: ejabberd:jid(),
                    RemJID :: ejabberd:jid()) -> behaviour().
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


-spec set_prefs(Result :: any(), Host :: ejabberd:server(),
                ArcID :: mod_mam:archive_id(), ArcJID :: ejabberd:jid(),
                DefaultMode :: mod_mam:archive_behaviour(),
                AlwaysJIDs :: [ejabberd:literal_jid()],
                NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(Result, _Host, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    SU = su_key(ArcJID),
    NewARules = lists:usort(rules(ArcJID, AlwaysJIDs)),
    NewNRules = lists:usort(rules(ArcJID, NeverJIDs)),
    User = #mam_prefs{
        arc_id = ArcID,
        host_user = SU,
        default_mode = DefaultMode,
        always_rules = NewARules,
        never_rules = NewNRules
    },
    mnesia:sync_dirty(fun() ->
            mnesia:write(User)
        end),
    Result.


-spec get_prefs(mod_mam:preference(), _Host :: ejabberd:server(),
                _ArcId :: mod_mam:archive_id(), ArcJID :: ejabberd:jid()
                ) -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, _Host, _ArcID, ArcJID) ->
    case mnesia:dirty_read(mam_prefs, su_key(ArcJID)) of
        [] ->
            {GlobalDefaultMode, [], []};
        [#mam_prefs{default_mode=DefaultMode,
                    always_rules=ARules, never_rules=NRules}] ->
            AlwaysJIDs = jids(ArcJID, ARules),
            NeverJIDs = jids(ArcJID, NRules),
            {DefaultMode, AlwaysJIDs, NeverJIDs}
    end.


-spec remove_archive(ejabberd:server(), mod_mam:archive_id(), ejabberd:jid()) -> any().
remove_archive(_Host, ArcID, _ArcJID) ->
    mnesia:sync_dirty(fun() ->
            mnesia:delete(mam_prefs, ArcID, write)
        end).

%% ----------------------------------------------------------------------
%% Helpers

-spec su_key(ejabberd:jid()) -> ejabberd:simple_bare_jid().
su_key(#jid{lserver=LocLServer, luser=LocLUser}) ->
    {LocLServer, LocLUser}.


-spec jids(ejabberd:jid(),
    [ejabberd:literal_jid() | ejabberd:simple_bare_jid() | ejabberd:simple_jid()]
    ) -> [ejabberd:literal_jid()].
jids(ArcJID, Rules) ->
    [jlib:jid_to_binary(rule_to_jid(ArcJID, Rule)) || Rule <- Rules].


-spec rule_to_jid(ejabberd:jid(),
    ejabberd:luser() | ejabberd:simple_bare_jid() | ejabberd:simple_jid()
    ) -> ejabberd:simple_jid().
rule_to_jid(#jid{lserver=LServer}, RemLUser) when is_binary(RemLUser) ->
    {RemLUser, LServer, <<>>};
rule_to_jid(_ArcJID, {RemLServer, RemLUser, RemLResource}) ->
    {RemLUser, RemLServer, RemLResource};
rule_to_jid(_ArcJID, {RemLServer, RemLUser}) ->
    {RemLUser, RemLServer, <<>>}.


-spec rules(ejabberd:jid(), [ejabberd:literal_jid()]) ->
    [ejabberd:literal_jid() | ejabberd:simple_bare_jid() | ejabberd:simple_jid()].
rules(ArcJID, BinJIDs) ->
    [rule(ArcJID, jlib:binary_to_jid(BinJID)) || BinJID <- BinJIDs].


-spec rule(ejabberd:jid(), ejabberd:jid()) ->
    ejabberd:literal_jid() | ejabberd:simple_bare_jid() | ejabberd:simple_jid().
rule(#jid{lserver=LServer}, #jid{lserver=LServer, luser=RemLUser, lresource = <<>>}) ->
    RemLUser;
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource = <<>>}) ->
    {RemLServer, RemLUser};
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource=RemLResource}) ->
    {RemLServer, RemLUser, RemLResource}.


-spec is_bare_jid(ejabberd:jid()) -> boolean().
is_bare_jid(#jid{lresource = <<>>}) -> true;
is_bare_jid(_)                      -> false.


-spec match_jid(ejabberd:jid(), ejabberd:jid(), [any()]) -> boolean().
match_jid(ArcJID, JID, JIDs) ->
    case is_bare_jid(JID) of
    true ->
        ordsets:is_element(rule(ArcJID, JID), JIDs);
    false ->
        BareJID = jlib:jid_remove_resource(JID),
        ordsets:is_element(rule(ArcJID, BareJID), JIDs)
            orelse
        ordsets:is_element(rule(ArcJID, JID), JIDs)
    end.
