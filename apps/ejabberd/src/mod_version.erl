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

process_local_iq(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};

process_local_iq(_From, _To, #iq{type = get} = IQ) ->
    {Name, Version} = mongoose_info(),
    System = os_info().

mongoose_info() ->
    [{Name, Version}] =
      [{N, V} || {mongooseim, N, V} <- application:which_applications()],
    {Name, Version}.

os_info() ->
    {Family, Name} = os:type(),
    {Major, Minor, Release} = os:version(),
    list_to_binary(
        atom_to_list(Family) ++ " " ++
        atom_to_list(Name) ++ " " ++
        integer_to_list(Major) ++ "." ++
        integer_to_list(Minor) ++ "." ++
        integer_to_list(Release)
    ).
