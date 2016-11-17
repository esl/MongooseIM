-module(mod_version).
-behaviour(gen_mod).
-export([start/2, stop/1, process_iq/3]).
-include("jlib.hrl").
-include("ejabberd.hrl").
-xep([{xep, 92}, {version, "1.1"}]).

-spec start(ejabberd:server(), list()) -> any().
start(Host, Opts) ->
    mod_disco:register_feature(Host, ?NS_VERSION),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_VERSION, ?MODULE, process_iq, IQDisc).

-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_VERSION),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VERSION).

-spec process_iq(#jid{}, #jid{}, #iq{}) -> #iq{}.
process_iq(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};

process_iq(From, _To, #iq{type = get} = IQ) ->
    {Name, Version} = mongoose_info(),
    Host = From#jid.lserver,
    IQ#iq{type = result,
          sub_el =
          [#xmlel{name = <<"query">>,
                  attrs = [{<<"xmlns">>, ?NS_VERSION}],
                  children =
                  [#xmlel{name = <<"name">>, attrs = [],
                          children =[#xmlcdata{content = Name}]},
                   #xmlel{name = <<"version">>, attrs = [],
                          children =[#xmlcdata{content = Version}]}
                  ] ++ add_os_info(Host)}]}.

-spec add_os_info(binary()) -> [exml:element()] | [].
add_os_info(Host) ->
    case gen_mod:get_module_opt(Host, ?MODULE, os_info, false) of
        true ->
            [#xmlel{name = <<"os">>, attrs = [],
                    children = [#xmlcdata{content = os_info()}]}];
        _ ->
            []
    end.

-spec mongoose_info() -> {binary(), binary()}.
mongoose_info() ->
    {ok, EjdMimVsn} = application:get_key(ejabberd, vsn), %returns 2.1.8+mim-*
    Version = string:sub_string(EjdMimVsn, 11),
    {<<"MongooseIM">>, list_to_binary(Version)}.

-spec os_info() -> binary().
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

