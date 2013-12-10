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
-export([start/2,
         get_behaviour/6,
         get_prefs/5,
         set_prefs/7,
         remove_archive/4]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_prefs, {arc_id, host_user, default_mode, always_rules, never_rules}).

start(_Host, _Mod) ->
    mnesia:create_table(mam_prefs,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_prefs)}]),
    ok.

get_behaviour(_Host, _Mod,
              ArcID,
              LocJID=#jid{},
              RemJID=#jid{},
              DefaultBehaviour) ->
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

set_prefs(_Host, _Mod, ArcID, ArcJID, DefaultMode, AlwaysJIDs, NeverJIDs) ->
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
        end).

get_prefs(_Host, _Mod, _ArcID, ArcJID, GlobalDefaultMode) ->
    case mnesia:dirty_read(mam_prefs, su_key(ArcJID)) of
        [] -> 
            {GlobalDefaultMode, [], []};
        [#mam_prefs{default_mode=DefaultMode,
                    always_rules=ARules, never_rules=NRules}] ->
            AlwaysJIDs = jids(ArcJID, ARules),
            NeverJIDs = jids(ArcJID, NRules),
            {DefaultMode, AlwaysJIDs, NeverJIDs}
    end.

remove_archive(_Host, _Mod, ArcID, _ArcJID) ->
    mnesia:sync_dirty(fun() ->
            mnesia:delete(mam_prefs, ArcID)
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
