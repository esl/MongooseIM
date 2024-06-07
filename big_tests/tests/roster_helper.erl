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

%% Intrumentation events

assert_roster_event(Client, Event) ->
    ClientJid = jid:from_binary(escalus_utils:get_jid(Client)),
    instrument_helper:assert(
      Event, #{host_type => host_type()},
      fun(#{count := 1, jid := Jid}) -> jid:are_bare_equal(ClientJid, Jid) end).

assert_subscription_event(FromClient, ToClient, CheckF) ->
    FromClientJid = jid:from_binary(escalus_utils:get_short_jid(FromClient)),
    ToClientJid = jid:from_binary(escalus_utils:get_short_jid(ToClient)),
    instrument_helper:assert(
      sm_presence_subscription, #{},
      fun(#{from_jid := FromJid, to_jid := ToJid} = M) ->
              FromClientJid =:= FromJid andalso ToClientJid =:= ToJid andalso CheckF(M)
      end).
