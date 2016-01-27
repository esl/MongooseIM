%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Mnesia.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_mnesia_prefs).

%% ----------------------------------------------------------------------
%% Exports

%% gen_mod handlers
-export([start/2, stop/1]).

%% MAM hook handlers
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/3]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_prefs_rule, {key,
                        behaviour}).
-record(mam_prefs_user, {host_user,
                        default_mode,
                        always_rules :: [ejabberd:simple_jid() | ejabberd:simple_bare_jid()],
                        never_rules :: [ejabberd:simple_jid() | ejabberd:simple_bare_jid()]}).
-type ls() :: ejabberd:lserver().
-type lu() :: ejabberd:luser().
-type lr() :: ejabberd:lresource().

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    mnesia:create_table(mam_prefs_rule,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_prefs_rule)}]),
    mnesia:create_table(mam_prefs_user,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_prefs_user)}]),
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

-spec get_behaviour(DefaultBvr :: mod_mam:archive_behaviour(),
        _Host :: ejabberd:server(), ArcId :: mod_mam:archive_id(),
        LocJID :: ejabberd:jid(), RemJID :: ejabberd:jid())
            -> mod_mam:archive_behaviour().
get_behaviour(DefaultBehaviour, _Host, _ArcID,
              LocJID=#jid{},
              RemJID=#jid{}) ->
    %% Check full JID, bare JID and default for user
    case mnesia:dirty_read(mam_prefs_rule, key1(LocJID, RemJID)) of
        [] ->
            case mnesia:dirty_read(mam_prefs_rule, key2(LocJID, RemJID)) of
                [] ->
                    case mnesia:dirty_read(mam_prefs_rule, key3(LocJID)) of
                        [] -> DefaultBehaviour;
                        [#mam_prefs_rule{behaviour=B}] -> B
                    end;
                [#mam_prefs_rule{behaviour=B}] -> B
            end;
        [#mam_prefs_rule{behaviour=B}] -> B
    end.


-spec set_prefs(_, _Host :: ejabberd:server(), _ArcId :: mod_mam:archive_id(),
        ArcJID :: ejabberd:jid(), DefaultMode :: mod_mam:archive_behaviour(),
        AlwaysJIDs :: [ejabberd:literal_jid()],
        NeverJIDs :: [ejabberd:literal_jid()]) -> any().
set_prefs(Result, _Host, _ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
    NewARules = lists:usort(rules(AlwaysJIDs)),
    NewNRules = lists:usort(rules(NeverJIDs)),
    SU = su_key(ArcJID),
    {atomic, ok} = mnesia:transaction(fun() ->
        case mnesia:read(mam_prefs_user, SU, write) of
            [] ->
                mnesia:write(#mam_prefs_user{
                             host_user=SU, default_mode=DefaultMode,
                             always_rules=NewARules, never_rules=NewNRules}),
                set_default_rule(ArcJID, DefaultMode),
                update_rules(always, ArcJID, [], NewARules),
                update_rules(never, ArcJID, [], NewNRules),
                ok;
            [#mam_prefs_user{default_mode=DefaultMode,
                             always_rules=NewARules, never_rules=NewNRules}] ->
                ok; %% same
            [#mam_prefs_user{default_mode=OldDefaultMode,
                             always_rules=OldARules, never_rules=OldNRules}] ->
                update_default_rule(ArcJID, OldDefaultMode, DefaultMode),
                mnesia:write(#mam_prefs_user{
                             host_user=SU, default_mode=DefaultMode,
                             always_rules=NewARules, never_rules=NewNRules}),
                update_rules(always, ArcJID, OldARules, NewARules),
                update_rules(never, ArcJID, OldNRules, NewNRules),
                ok
        end
    end),
    Result.


-spec get_prefs(GlobalDeflt :: mod_mam:preference(), _Host :: ejabberd:server(),
        _ArcID :: mod_mam:archive_id(), ArcJID :: ejabberd:jid()
        ) -> mod_mam:preference().
get_prefs({GlobalDefaultMode, _, _}, _Host, _ArcID, ArcJID) ->
    case mnesia:dirty_read(mam_prefs_user, su_key(ArcJID)) of
        [] ->
            {GlobalDefaultMode, [], []};
        [#mam_prefs_user{default_mode=DefaultMode,
                         always_rules=ARules, never_rules=NRules}] ->
            AlwaysJIDs = jids(ARules),
            NeverJIDs = jids(NRules),
            {DefaultMode, AlwaysJIDs, NeverJIDs}
    end.


-spec remove_archive(_Host :: ejabberd:server(), _ArcID :: mod_mam:archive_id(),
                     ArcJID :: ejabberd:jid()) -> 'ok'.
remove_archive(_Host, _ArcID, ArcJID) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        case mnesia:read(mam_prefs_user, su_key(ArcJID)) of
            [] -> ok; %% already deleted
            [#mam_prefs_user{always_rules=ARules, never_rules=NRules}] ->
                Rules = [key3(ArcJID)] ++ ARules ++ NRules,
                Keys = [key(ArcJID, Rule) || Rule <- Rules],
                [mnesia:delete(mam_prefs_rule, Key, write) || Key <- Keys],
                ok
        end
    end),
    ok.

%% ----------------------------------------------------------------------
%% Helpers

%% @doc Key for full remote jid
-spec key1(ejabberd:jid(), ejabberd:jid()) -> {ls(), lu(), lu(), ls(), lr()}.
key1(#jid{lserver=LocLServer, luser=LocLUser},
     #jid{lserver=RemLServer, luser=RemLUser, lresource=RemLResource}) ->
    {LocLServer, LocLUser, RemLServer, RemLUser, RemLResource}.


%% @doc Key for bare remote jid
-spec key2(ejabberd:jid(), ejabberd:jid()) -> {ls(), lu(), lu(), ls()}.
key2(#jid{lserver=LocLServer, luser=LocLUser},
     #jid{lserver=RemLServer, luser=RemLUser}) ->
    {LocLServer, LocLUser, RemLServer, RemLUser}.


%% @doc Key for default params
-spec key3(ejabberd:jid()) -> {ls(), lu()}.
key3(#jid{lserver=LocLServer, luser=LocLUser}) ->
    {LocLServer, LocLUser}.


-spec su_key(ejabberd:jid()) -> {ls(), lu()}.
su_key(#jid{lserver=LocLServer, luser=LocLUser}) ->
    {LocLServer, LocLUser}.


%% @doc Expand short rule.
-spec key(ejabberd:jid(), {ls(), lu()} | {ls(), lu(), lr()})
            -> {ls(), lu(), ls(), lu()} | {ls(), lu(), ls(), lu(), lr()}.
key(#jid{lserver=LocLServer, luser=LocLUser}, {RemLServer, RemLUser}) ->
    {LocLServer, LocLUser, RemLServer, RemLUser}; %% key2
key(#jid{lserver=LocLServer, luser=LocLUser}, {RemLServer, RemLUser, RemLResource}) ->
    {LocLServer, LocLUser, RemLServer, RemLUser, RemLResource}. %% key1


-spec jids([{ls(),lu()} | {ls(),lu(),lr()}]) -> [ejabberd:literal_jid()].
jids(Rules) ->
    [jid:to_binary(rule_to_jid(Rule)) || Rule <- Rules].


-spec rule_to_jid({ls(), lu()} | {ls(), lu(), lr()}) -> {lu(), ls(), lr()}.
rule_to_jid({RemLServer, RemLUser, RemLResource}) ->
    {RemLUser, RemLServer, RemLResource};
rule_to_jid({RemLServer, RemLUser}) ->
    {RemLUser, RemLServer, <<>>}.


-spec rules([ejabberd:literal_jid()]) -> [{ls(), lu()} | {ls(), lu(), lr()}].
rules(BinJIDs) ->
    [rule(jid:from_binary(BinJID)) || BinJID <- BinJIDs].


-spec rule(ejabberd:jid()) -> {ls(), lu()} | {ls(), lu(), lr()}.
rule(#jid{lserver=RemLServer, luser=RemLUser, lresource = <<>>}) ->
    {RemLServer, RemLUser};
rule(#jid{lserver=RemLServer, luser=RemLUser, lresource=RemLResource}) ->
    {RemLServer, RemLUser, RemLResource}.


-spec update_rules(Mode :: 'always' | 'never', ArcJID :: ejabberd:jid(),
        OldNRules :: [{ls(), lu()} | {ls(), lu(), lr()}],
        NewNRules :: [{ls(), lu()} | {ls(), lu(), lr()}]) -> 'ok'.
update_rules(Mode, ArcJID, OldNRules, NewNRules) ->
    DelRules = ordsets:subtract(OldNRules, NewNRules),
    InsRules = ordsets:subtract(NewNRules, OldNRules),
    DelKeys = [key(ArcJID, Rule) || Rule <- DelRules],
    InsKeys = [key(ArcJID, Rule) || Rule <- InsRules],
    [mnesia:delete(mam_prefs_rule, Key, write) || Key <- DelKeys],
    [mnesia:write(#mam_prefs_rule{key=Key, behaviour=Mode}) || Key <- InsKeys],
    ok.

set_default_rule(ArcJID, DefaultMode) ->
    mnesia:write(#mam_prefs_rule{key=key3(ArcJID), behaviour=DefaultMode}).

update_default_rule(ArcJID, DefaultMode, DefaultMode) ->
    ok; % same default mode
update_default_rule(ArcJID, _OldDefaultMode, DefaultMode) ->
    set_default_rule(ArcJID, DefaultMode).
