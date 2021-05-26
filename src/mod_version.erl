-module(mod_version).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-export([start/2, stop/1, config_spec/0, add_local_features/5, process_iq/4]).

-xep([{xep, 92}, {version, "1.1"}]).

-spec start(jid:server(), list()) -> any().
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                                  ?NS_VERSION, ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(hooks(Host)).

-spec stop(jid:server()) -> any().
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VERSION).

hooks(Host) ->
    [{disco_local_features, Host, ?MODULE, add_local_features, 99}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"os_info">> => #option{type = boolean}
                }
      }.

-spec add_local_features(mongoose_disco:acc(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          mongoose_disco:acc().
add_local_features(Acc, _From, _To, <<>>, _Lang) ->
    mongoose_disco:add_features([?NS_VERSION], Acc);
add_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec process_iq(jid:jid(),jid:jid(), mongoose_acc:t(), jlib:iq()) -> {mongoose_acc:t(), jlib:iq()}.
process_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_iq(From, _To, Acc, #iq{type = get} = IQ) ->
    {Name, Version} = mongoose_info(),
    Host = From#jid.lserver,
    {Acc, IQ#iq{type = result,
          sub_el =
          [#xmlel{name = <<"query">>,
                  attrs = [{<<"xmlns">>, ?NS_VERSION}],
                  children =
                  [#xmlel{name = <<"name">>, attrs = [],
                          children =[#xmlcdata{content = Name}]},
                   #xmlel{name = <<"version">>, attrs = [],
                          children =[#xmlcdata{content = Version}]}
                  ] ++ add_os_info(Host)}]}}.

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
    {ok, Version} = application:get_key(mongooseim, vsn),
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
