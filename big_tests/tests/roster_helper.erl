-module(roster_helper).
-export([set_versioning/3, restore_versioning/1]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [host_type/0]).

-spec set_versioning(boolean(), boolean(), escalus_config:config()) -> escalus_config:config(). 
set_versioning(Versioning, VersionStore, Config) ->
    RosterVersioning = rpc(mim(), gen_mod, get_module_opt,
                           [host_type(), mod_roster, versioning, false]),
    RosterVersionOnDb = rpc(mim(), gen_mod, get_module_opt,
                            [host_type(), mod_roster, store_current_id, false]),
    rpc(mim(), gen_mod, set_module_opt, [host_type(), mod_roster, versioning, Versioning]),
    rpc(mim(), gen_mod, set_module_opt, [host_type(), mod_roster, store_current_id, VersionStore]),
    [{versioning, RosterVersioning},
     {store_current_id, RosterVersionOnDb} | Config].

-spec restore_versioning(escalus_config:config()) -> escalus_config:config().
restore_versioning(Config) ->
    RosterVersioning = proplists:get_value(versioning, Config),
    RosterVersionOnDb = proplists:get_value(store_current_id, Config),
    rpc(mim(), gen_mod, get_module_opt, [host_type(), mod_roster, versioning, RosterVersioning]),
    rpc(mim(), gen_mod, get_module_opt, [host_type(), mod_roster, store_current_id, RosterVersionOnDb]).
