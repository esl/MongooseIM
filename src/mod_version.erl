-module(mod_version).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-export([start/2]).
-export([stop/1]).
-export([supported_features/0]).
-export([config_spec/0]).
-export([process_iq/5]).

-ignore_xref([process_iq/5]).

-xep([{xep, 92}, {version, "1.1"}]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_VERSION, ejabberd_local,
                                             fun ?MODULE:process_iq/5, #{}, IQDisc).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_VERSION, ejabberd_local).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"os_info">> => #option{type = boolean}
                },
       defaults = #{<<"iqdisc">> => no_queue,
                    <<"os_info">> => false}
      }.

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), any()) ->
          {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_iq(Acc, _From, _To, #iq{type = get} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    {Name, Version} = mongoose_info(),
    {Acc, IQ#iq{type = result,
          sub_el =
          [#xmlel{name = <<"query">>,
                  attrs = [{<<"xmlns">>, ?NS_VERSION}],
                  children =
                  [#xmlel{name = <<"name">>, attrs = [],
                          children =[#xmlcdata{content = Name}]},
                   #xmlel{name = <<"version">>, attrs = [],
                          children =[#xmlcdata{content = Version}]}
                  ] ++ add_os_info(HostType)}]}}.

-spec add_os_info(mongooseim:host_type()) -> [exml:element()] | [].
add_os_info(HostType) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, os_info) of
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
