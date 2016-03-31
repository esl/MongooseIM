-module(mod_version).
-behaviour(gen_mod).
-export([start/2, stop/1, process_local_iq/3]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-xep([{xep, 92}, {version, "1.1"}]).
start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_VERSION),
    IQDisc = gen_mod:get_opt(iqdisc, Opts,
        one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?NS_VERSION, ?MODULE, process_local_iq, IQDisc).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_VERSION),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
        ?NS_VERSION).

process_local_iq(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};

process_local_iq(_From, _To, #iq{type = get} = IQ) ->
    {_, AppName} = application:get_application(),
    IQ#iq{type = result,
        sub_el =
        [#xmlel{name = <<"query">>,
            attrs = [{<<"xmlns">>, ?NS_VERSION}],
            children =
            [#xmlel{name = <<"name">>, attrs = [],
                children =
                #xmlcdata{content = iolist_to_binary(atom_to_list(AppName))}},
                #xmlel{name = <<"version">>, attrs = [],
                    children =
                    #xmlcdata{content = iolist_to_binary(application:get_env(AppName, vsn))}},
                #xmlel{name = <<"os">>, attrs = [],
                    children =
                    #xmlcdata{content = get_os()}}]}]}.

get_os() ->
    {OSfamily, OSname} = os:type(),
    {Major, Minor, Release} = os:version(),
    iolist_to_binary(atom_to_list(OSfamily) ++ " " ++
        atom_to_list(OSname) ++ " " ++
        integer_to_list(Major) ++ "." ++
        integer_to_list(Minor) ++ "." ++
        integer_to_list(Release)).
