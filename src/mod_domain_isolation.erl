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
    #list{items = #option{type = string,
                          validate = subdomain_template,
                          process = fun mongoose_subdomain_utils:make_subdomain_pattern/1}}.

start(Host, Opts) ->
    ExtraDomains = proplists:get_value(extra_domains, Opts, []),
    SubHosts = [mongoose_subdomain_utils:get_fqdn(SubdomainPattern, Host)
                || SubdomainPattern <- ExtraDomains],
    AllHosts = [Host|SubHosts],
    [ejabberd_hooks:add(filter_local_packet, H, ?MODULE, filter_local_packet, 10)
     || H <- AllHosts],
    ok.

stop(Host) ->
    ExtraDomains = gen_mod:get_module_opt(Host, ?MODULE, extra_domains, []),
    SubHosts = [mongoose_subdomain_utils:get_fqdn(SubdomainPattern, Host)
                || SubdomainPattern <- ExtraDomains],
    AllHosts = [Host|SubHosts],
    [ejabberd_hooks:delete(filter_local_packet, H, ?MODULE, filter_local_packet, 10)
     || H <- AllHosts],
    ok.

-spec filter_local_packet(Value :: fpacket() | drop) -> fpacket() | drop.
filter_local_packet(drop) ->
    drop;
filter_local_packet({#jid{lserver = Server},
                     #jid{lserver = Server}, _Acc, _Packet} = Arg) ->
    Arg;
filter_local_packet({#jid{lserver = FromServer} = From,
                     #jid{lserver = ToServer} = To, Acc, Packet} = Arg) ->
    FromHost = domain_to_host(FromServer),
    ToHost = domain_to_host(ToServer),
    case FromHost =:= ToHost of
        true ->
            Arg;
        false ->
            %% Allow errors from this module to be passed
            case mongoose_acc:get(domain_isolation, ignore, false, Acc) of
                true ->
                    Arg;
                false ->
                    maybe_send_back_error(From, To, Acc, Arg),
                    drop
            end
     end.

%% muc.localhost becomes localhost.
%% localhost stays localhost.
domain_to_host(Domain) ->
    case mongoose_subhosts:get_host(Domain) of
        {ok, Host} -> Host;
        undefined -> Domain
    end.

maybe_send_back_error(From, To, Acc, Arg) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> -> %% Never reply to the errors
            Arg;
        _ ->
            Err = mongoose_xmpp_errors:service_unavailable(<<"en">>,
                    <<"Filtered by the domain isolation">>),
            Acc2 = mongoose_acc:set_permanent(domain_isolation, ignore, true, Acc),
            send_back_error(Err, From, To, Acc2),
            drop
   end.

send_back_error(Etype, From, To, Acc) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Etype),
    ejabberd_router:route(To, From, Acc1, Err).
