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
-export([get_behaviour/5,
         get_prefs/4,
         set_prefs/7,
         remove_archive/3]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_prefs, {arc_id, host_user, default_mode, always_rules, never_rules}).

%% ----------------------------------------------------------------------
%% gen_mod callbacks
%% Starting and stopping functions for users' archives

start(Host, Opts) ->
    mnesia:create_table(mam_prefs,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_prefs)}]),
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

start_pm(Host, _Opts) ->
    ejabberd_hooks:add(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.

stop_pm(Host) ->
    ejabberd_hooks:delete(mam_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Add hooks for mod_mam_muc_muc

start_muc(Host, _Opts) ->
    ejabberd_hooks:add(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:add(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:add(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:add(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.

stop_muc(Host) ->
    ejabberd_hooks:delete(mam_muc_get_behaviour, Host, ?MODULE, get_behaviour, 50),
    ejabberd_hooks:delete(mam_muc_get_prefs, Host, ?MODULE, get_prefs, 50),
    ejabberd_hooks:delete(mam_muc_set_prefs, Host, ?MODULE, set_prefs, 50),
    ejabberd_hooks:delete(mam_muc_remove_archive, Host, ?MODULE, remove_archive, 50),
    ok.


%% ----------------------------------------------------------------------
%% Internal functions and callbacks

get_behaviour(DefaultBehaviour, _Host,
              ArcID,
              LocJID=#jid{},
              RemJID=#jid{}) ->
    case mnesia:dirty_read(mam_prefs, ArcID) of
        [] -> DefaultBehaviour;
        [User] -> get_behaviour(User, LocJID, RemJID)
    end.

get_behaviour(#mam_prefs{default_mode = always, never_rules=NeverJIDs}, LocJID, RemJID) ->
    IsNewer = match_jid(LocJID, RemJID, NeverJIDs),
    case IsNewer of
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
    IsNewer = match_jid(LocJID, RemJID, NeverJIDs),
    case IsNewer of
        true -> never;
        false ->
            IsAlways = match_jid(LocJID, RemJID, AlwaysJIDs),
            case IsAlways of
                true -> always;
                false -> roster
            end
    end.

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

remove_archive(_Host, ArcID, _ArcJID) ->
    mnesia:sync_dirty(fun() ->
            mnesia:delete(mam_prefs, ArcID, write)
        end).

%% ----------------------------------------------------------------------
%% Helpers

su_key(#jid{lserver=LocLServer, luser=LocLUser}) ->
    {LocLServer, LocLUser}.

jids(ArcJID, Rules) ->
    [jlib:jid_to_binary(rule_to_jid(ArcJID, Rule)) || Rule <- Rules].

rule_to_jid(#jid{lserver=LServer}, RemLUser) when is_binary(RemLUser) ->
    {RemLUser, LServer, <<>>};
rule_to_jid(_ArcJID, {RemLServer, RemLUser, RemLResource}) ->
    {RemLUser, RemLServer, RemLResource};
rule_to_jid(_ArcJID, {RemLServer, RemLUser}) ->
    {RemLUser, RemLServer, <<>>}.

rules(ArcJID, BinJIDs) ->
    [rule(ArcJID, jlib:binary_to_jid(BinJID)) || BinJID <- BinJIDs].

rule(#jid{lserver=LServer}, #jid{lserver=LServer, luser=RemLUser, lresource = <<>>}) ->
    RemLUser;
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource = <<>>}) ->
    {RemLServer, RemLUser};
rule(_ArcJID, #jid{lserver=RemLServer, luser=RemLUser, lresource=RemLResource}) ->
    {RemLServer, RemLUser, RemLResource}.

is_bare_jid(#jid{lresource = <<>>}) -> true;
is_bare_jid(_)                      -> false.

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
