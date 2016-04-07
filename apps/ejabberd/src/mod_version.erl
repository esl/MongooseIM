-module(mod_version).
-behaviour(gen_mod).
-export([start/2, stop/1]).
-include("jlib.hrl").
-include("ejabberd.hrl").

start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_VERSION),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_VERSION, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_VERSION),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VERSION).
