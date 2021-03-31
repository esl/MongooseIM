-module(mod_domain_isolation).
-behaviour(gen_mod).

-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).
-export([config_spec/0]).

%% hooks
-export([filter_local_packet/1]).

-type fpacket() :: {From :: jid:jid(),
                    To :: jid:jid(),
                    Acc :: mongoose_acc:t(),
                    Packet :: exml:element()}.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"extra_domains">> => extra_domains()}
      }.

extra_domains() ->
    #list{items = #option{type = string, validate = domain_template}}.

start(Host, Opts) ->
    ExtraDomains = proplists:get_value(extra_domains, Opts, []),
    SubHosts = [gen_mod:make_subhost(SubHost, Host) || SubHost <- ExtraDomains],
    AllHosts = [Host|SubHosts],
    [ejabberd_hooks:add(filter_local_packet, H, ?MODULE, filter_local_packet, 10)
     || H <- AllHosts],
    ok.

stop(Host) ->
    ExtraDomains = gen_mod:get_module_opt(Host, ?MODULE, extra_domains, []),
    SubHosts = [gen_mod:make_subhost(SubHost, Host) || SubHost <- ExtraDomains],
    AllHosts = [Host|SubHosts],
    [ejabberd_hooks:delete(filter_local_packet, H, ?MODULE, filter_local_packet, 10)
     || H <- AllHosts],
    ok.

-spec filter_local_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_local_packet(drop) ->
    drop;
filter_local_packet({From = #jid{lserver = FromServer},
                     To = #jid{lserver = ToServer}, Acc, Packet}) ->
    FromHost = domain_to_host(FromServer),
    ToHost = domain_to_host(ToServer),
    Pass = FromHost =:= ToHost,
    case Pass of
        true ->
            {From, To, Acc, Packet};
        false ->
            send_back_error(mongoose_xmpp_errors:service_unavailable(
                                <<"en">>, <<"Filtered by the domain isolation">>),
                            From, To, Acc),
            drop
     end.

%% muc.localhost becomes localhost.
%% localhost stays localhost.
domain_to_host(Domain) ->
    case mongoose_subhosts:get_host(Domain) of
        {ok, Host} -> Host;
        undefined -> Domain
    end.

send_back_error(Etype, From, To, Acc) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Etype),
    ejabberd_router:route(To, From, Acc1, Err).
