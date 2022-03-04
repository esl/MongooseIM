-module(roster_helper).
-export([set_versioning/3]).

-import(distributed_helper, [mim/0, rpc/4]).
-import(domain_helper, [host_type/0]).

-spec set_versioning(boolean(), boolean(), escalus_config:config()) -> escalus_config:config().
set_versioning(Versioning, VersionStore, Config) ->
    Opts = dynamic_modules:get_saved_config(host_type(), mod_roster, Config),
    dynamic_modules:ensure_modules(host_type(), [{mod_roster, Opts#{versioning => Versioning,
                                                                    store_current_id => VersionStore}}]),
    Config.
