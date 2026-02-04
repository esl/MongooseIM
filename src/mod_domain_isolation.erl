-module(mod_domain_isolation).
-behaviour(gen_mod).

-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).
-export([hooks/1]).
-export([config_spec/0]).
-export([supported_features/0]).

%% hooks
-export([filter_local_packet/3]).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{}}.

start(_HostType, _Opts) ->
    ok.

stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 10}].

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

-spec filter_local_packet(FPacketAcc, Params, Extra) -> {ok, FPacketAcc} | {stop, drop} when
      FPacketAcc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
filter_local_packet(drop, _, _) ->
    {ok, drop};
filter_local_packet({#jid{lserver = Server}, #jid{lserver = Server}, _Acc, _Packet} = FPacketAcc, _, _) ->
    {ok, FPacketAcc};
filter_local_packet({#jid{lserver = FromServer} = From, #jid{lserver = ToServer} = To, Acc, _Packet} = FPacketAcc, _, _) ->
    case mongoose_acc:get(domain_isolation, ignore, false, Acc)
            orelse domain_to_host(FromServer) =:= domain_to_host(ToServer)
            orelse is_external_component(FromServer) of
        true ->
            {ok, FPacketAcc};
        false ->
            maybe_send_back_error(From, To, Acc, FPacketAcc),
            {stop, drop}
    end.

%% muc.localhost becomes localhost.
%% localhost stays localhost.
-spec domain_to_host(jid:lserver()) -> jid:lserver().
domain_to_host(Domain) ->
    case mongoose_domain_api:get_subdomain_info(Domain) of
        {ok, #{parent_domain := Parent}} when is_binary(Parent) -> Parent;
        _ -> Domain
    end.

-spec is_external_component(jid:lserver()) -> boolean().
is_external_component(Domain) ->
    mongoose_component:lookup_component(Domain) =/= [].

-spec maybe_send_back_error(From, To, Acc, FPacketAcc) -> FPacketAcc | drop when
    From :: jid:jid(),
    To :: jid:jid(),
    Acc :: mongoose_acc:t(),
    FPacketAcc :: mongoose_hooks:filter_packet_acc().
maybe_send_back_error(From, To, Acc, FPacketAcc) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> -> %% Never reply to the errors
            FPacketAcc;
        _ ->
            Err = mongoose_xmpp_errors:service_unavailable(<<"en">>,
                    <<"Filtered by the domain isolation">>),
            Acc2 = mongoose_acc:set_permanent(domain_isolation, ignore, true, Acc),
            send_back_error(Err, From, To, Acc2),
            drop
    end.

-spec send_back_error(Etype, From, To, Acc) -> Acc when
    Etype :: exml:element(),
    From :: jid:jid(),
    To :: jid:jid(),
    Acc :: mongoose_acc:t().
send_back_error(Etype, From, To, Acc) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Etype),
    ejabberd_router:route(To, From, Acc1, Err).
