-module(roster_helper).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [host_type/0]).

-spec set_versioning(boolean(), boolean(), escalus_config:config()) -> escalus_config:config().
set_versioning(Versioning, VersionStore, Config) ->
    Opts = dynamic_modules:get_saved_config(host_type(), mod_roster, Config),
    dynamic_modules:ensure_modules(host_type(), [{mod_roster, Opts#{versioning => Versioning,
                                                                    store_current_id => VersionStore}}]),
    Config.

%% Instrumentation events

assert_roster_event(ClientOrJid, Event) ->
    ClientJid = jid:from_binary(escalus_utils:get_jid(ClientOrJid)),
    instrument_helper:assert_one(
      Event, #{host_type => host_type()},
      fun(#{count := 1, jid := Jid}) -> ClientJid =:= Jid end).

assert_subscription_event(FromClient, ToClient, CheckF) ->
    FromClientJid = jid:from_binary(escalus_utils:get_short_jid(FromClient)),
    ToClientJid = jid:from_binary(escalus_utils:get_short_jid(ToClient)),
    instrument_helper:assert_one(
      sm_presence_subscription, #{host_type => host_type()},
      fun(#{from_jid := FromJid, to_jid := ToJid} = M) ->
              FromClientJid =:= FromJid andalso ToClientJid =:= ToJid andalso CheckF(M)
      end).
