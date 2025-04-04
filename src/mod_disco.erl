%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_disco).
-author('alexey@process-one.net').
-xep([{xep, 30}, {version, "2.5rc3"}]).
-xep([{xep, 157}, {version, "1.1.1"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0,
         instrumentation/1]).

%% iq handlers
-export([process_local_iq_items/5,
         process_local_iq_info/5,
         process_sm_iq_items/5,
         process_sm_iq_info/5]).

%% hook handlers
-export([disco_local_identity/3,
         disco_sm_identity/3,
         disco_local_items/3,
         disco_sm_items/3,
         disco_local_features/3,
         disco_info/3]).

-ignore_xref([disco_info/1, disco_local_identity/1, disco_local_items/1,
              disco_sm_identity/1, disco_sm_items/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-type return_hidden() :: mongoose_component:return_hidden().
-type server_info() :: #{name := binary(), urls := [binary()], modules => module()}.

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc}) ->
    [gen_iq_handler:add_iq_handler_for_domain(HostType, NS, Component, Handler, #{}, IQDisc) ||
        {Component, NS, Handler} <- iq_handlers()],
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, NS, Component) ||
        {Component, NS, _Handler} <- iq_handlers()],
    ok.

hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 100},
     {disco_local_items, HostType, fun ?MODULE:disco_local_items/3, #{}, 100},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 100},
     {disco_sm_items, HostType, fun ?MODULE:disco_sm_items/3, #{}, 100},
     {disco_sm_identity, HostType, fun ?MODULE:disco_sm_identity/3, #{}, 100},
     {disco_info, HostType, fun ?MODULE:disco_info/3, #{}, 100}].

iq_handlers() ->
    [{ejabberd_local, ?NS_DISCO_ITEMS, fun ?MODULE:process_local_iq_items/5},
     {ejabberd_local, ?NS_DISCO_INFO, fun ?MODULE:process_local_iq_info/5},
     {ejabberd_sm, ?NS_DISCO_ITEMS, fun ?MODULE:process_sm_iq_items/5},
     {ejabberd_sm, ?NS_DISCO_INFO, fun ?MODULE:process_sm_iq_info/5}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"extra_domains">> => #list{items = #option{type = binary,
                                                              validate = domain}},
                 <<"server_info">> => #list{items = server_info_spec()},
                 <<"users_can_see_hidden_services">> => #option{type = boolean},
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()
                },
       defaults = #{<<"extra_domains">> => [],
                    <<"server_info">> => [],
                    <<"users_can_see_hidden_services">> => true,
                    <<"iqdisc">> => one_queue}
      }.

server_info_spec() ->
    #section{
       items = #{<<"name">> => #option{type = binary,
                                       validate = non_empty},
                 <<"urls">> => #list{items = #option{type = binary,
                                                     validate = url}},
                 <<"modules">> => #list{items = #option{type = atom,
                                                        validate = module}}
                },
       required = [<<"name">>, <<"urls">>]
      }.

supported_features() -> [dynamic_domains].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_disco_roster_get, #{host_type => HostType}, #{metrics => #{count => spiral}}}].

%% IQ handlers

-spec process_local_iq_items(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_items(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_items(Acc, From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    Node = exml_query:attr(SubEl, <<"node">>, <<>>),
    case mongoose_disco:get_local_items(HostType, From, To, Node, Lang) of
        empty ->
            Error = mongoose_xmpp_errors:item_not_found(),
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
        {result, ItemsXML} ->
            {Acc, make_iq_result(IQ, ?NS_DISCO_ITEMS, Node, ItemsXML)}
    end.

-spec process_local_iq_info(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_info(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_info(Acc, From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    Node = exml_query:attr(SubEl, <<"node">>, <<>>),
    case mongoose_disco:get_local_features(HostType, From, To, Node, Lang) of
        empty ->
            Error = mongoose_xmpp_errors:item_not_found(),
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
        {result, FeaturesXML} ->
            IdentityXML = mongoose_disco:get_local_identity(HostType, From, To, Node, Lang),
            InfoXML = mongoose_disco:get_info(HostType, ?MODULE, Node, Lang),
            {Acc, make_iq_result(IQ, ?NS_DISCO_INFO, Node, IdentityXML ++ InfoXML ++ FeaturesXML)}
    end.

-spec process_sm_iq_items(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq_items(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_items(Acc, From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    case is_presence_subscribed(From, To) of
        true ->
            HostType = mongoose_acc:host_type(Acc),
            Node = exml_query:attr(SubEl, <<"node">>, <<>>),
            case mongoose_disco:get_sm_items(HostType, From, To, Node, Lang) of
                empty ->
                    Error = sm_error(From, To),
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
                {result, ItemsXML} ->
                    {Acc, make_iq_result(IQ, ?NS_DISCO_ITEMS, Node, ItemsXML)}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.

-spec process_sm_iq_info(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq_info(Acc, _From, _To, #iq{type = set, sub_el = SubEl} = IQ, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_info(Acc, From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ, _Extra) ->
    case is_presence_subscribed(From, To) of
        true ->
            HostType = mongoose_acc:host_type(Acc),
            Node = exml_query:attr(SubEl, <<"node">>, <<>>),
            case mongoose_disco:get_sm_features(HostType, From, To, Node, Lang) of
                empty ->
                    Error = sm_error(From, To),
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}};
                {result, FeaturesXML} ->
                    IdentityXML = mongoose_disco:get_sm_identity(HostType, From, To, Node, Lang),
                    {Acc, make_iq_result(IQ, ?NS_DISCO_INFO, Node, IdentityXML ++ FeaturesXML)}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.

%% Hook handlers

-spec disco_local_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:identity_acc(),
    Params :: map(),
    Extra :: map().
disco_local_identity(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_identities([#{category => <<"server">>,
                                          type => <<"im">>,
                                          name => <<"MongooseIM">>}], Acc)};
disco_local_identity(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:identity_acc(),
    Params :: map(),
    Extra :: map().
disco_sm_identity(Acc = #{to_jid := JID}, _, _) ->
    NewAcc = case ejabberd_auth:does_user_exist(JID) of
        true -> mongoose_disco:add_identities([#{category => <<"account">>,
                                                 type => <<"registered">>}], Acc);
        false -> Acc
    end,
    {ok, NewAcc}.

-spec disco_local_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: map().
disco_local_items(Acc = #{host_type := HostType, from_jid := From, to_jid := To, node := <<>>}, _, _) ->
    ReturnHidden = should_return_hidden(HostType, From),
    Subdomains = get_subdomains(To#jid.lserver),
    Components = get_external_components(ReturnHidden),
    ExtraDomains = get_extra_domains(HostType),
    Domains = Subdomains ++ Components ++ ExtraDomains,
    {ok, mongoose_disco:add_items([#{jid => Domain} || Domain <- Domains], Acc)};
disco_local_items(Acc, _, _) ->
    {ok, Acc}.

-spec disco_sm_items(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:item_acc(),
    Params :: map(),
    Extra :: map().
disco_sm_items(Acc = #{to_jid := To, node := <<>>}, _, _) ->
    Items = get_user_resources(To),
    {ok, mongoose_disco:add_items(Items, Acc)};
disco_sm_items(Acc, _, _) ->
    {ok, Acc}.

-spec disco_local_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:feature_acc(),
    Params :: map(),
    Extra :: map().
disco_local_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([<<"iq">>, <<"presence">>], Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

%% @doc Support for: XEP-0157 Contact Addresses for XMPP Services
-spec disco_info(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:info_acc(),
    Params :: map(),
    Extra :: map().
disco_info(Acc = #{host_type := HostType, module := Module, node := <<>>}, _, _) ->
    ServerInfoList = gen_mod:get_module_opt(HostType, ?MODULE, server_info),
    Fields = [server_info_to_field(ServerInfo) || ServerInfo <- ServerInfoList,
                                                  is_module_allowed(Module, ServerInfo)],
    {ok, mongoose_disco:add_info([#{xmlns => ?NS_SERVERINFO, fields => Fields}], Acc)};
disco_info(Acc, _, _) ->
    {ok, Acc}.

-spec get_extra_domains(mongooseim:host_type()) -> [jid:lserver()].
get_extra_domains(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, extra_domains).

%% Internal functions

-spec should_return_hidden(mongooseim:host_type(), From :: jid:jid()) -> return_hidden().
should_return_hidden(_HostType, #jid{ luser = <<>> } = _From) ->
    %% We respect "is hidden" flag only when a client performs the query
    all;
should_return_hidden(HostType, _From) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, users_can_see_hidden_services) of
        true -> all;
        false -> only_public
    end.

-spec get_subdomains(jid:lserver()) -> [jid:lserver()].
get_subdomains(LServer) ->
    [maps:get(subdomain, SubdomainInfo) ||
        SubdomainInfo <- mongoose_domain_api:get_all_subdomains_for_domain(LServer)].

-spec get_external_components(return_hidden()) -> [jid:lserver()].
get_external_components(ReturnHidden) ->
    mongoose_component:dirty_get_all_components(ReturnHidden).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_presence_subscribed(jid:jid(), jid:jid()) -> boolean().
is_presence_subscribed(#jid{luser = LFromUser, lserver = LFromServer} = FromJID,
                       #jid{luser = LToUser, lserver = LToServer} = _To) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LFromServer),
    A = mongoose_acc:new(#{ location => ?LOCATION,
                            host_type => HostType,
                            lserver => LFromServer,
                            element => undefined }),
    mongoose_instrument:execute(mod_disco_roster_get, #{host_type => HostType},
                                #{count => 1, jid => FromJID}),
    Roster = mongoose_hooks:roster_get(A, FromJID, false),
    lists:any(fun({roster, _, _, JID, _, S, _, _, _, _}) ->
                      {TUser, TServer} = jid:to_lus(JID),
                      LToUser == TUser andalso LToServer == TServer andalso S /= none
              end,
              Roster)
    orelse LFromUser == LToUser andalso LFromServer == LToServer.

sm_error(#jid{luser = LUser, lserver = LServer},
         #jid{luser = LUser, lserver = LServer}) ->
    mongoose_xmpp_errors:item_not_found();
sm_error(_From, _To) ->
    mongoose_xmpp_errors:not_allowed().

-spec get_user_resources(jid:jid()) -> [mongoose_disco:item()].
get_user_resources(JID = #jid{luser = LUser}) ->
    Rs = ejabberd_sm:get_user_resources(JID),
    lists:map(fun(R) ->
                      BJID = jid:to_binary(jid:replace_resource_noprep(JID, R)),
                      #{jid => BJID, name => LUser}
              end, lists:sort(Rs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_iq_result(jlib:iq(), binary(), binary(), [exml:element()]) -> jlib:iq().
make_iq_result(IQ, NameSpace, Node, ChildrenXML) ->
    Attrs = make_node_attrs(Node),
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = Attrs#{<<"xmlns">> => NameSpace},
                           children = ChildrenXML}]}.

-spec make_node_attrs(Node :: binary()) -> exml:attrs().
make_node_attrs(<<>>) -> #{};
make_node_attrs(Node) -> #{<<"node">> => Node}.

-spec server_info_to_field(server_info()) -> mongoose_disco:info_field().
server_info_to_field(#{name := Name, urls := URLs}) ->
    #{var => Name, values => URLs}.

-spec is_module_allowed(module(), server_info()) -> boolean().
is_module_allowed(Module, #{modules := Modules}) -> lists:member(Module, Modules);
is_module_allowed(_Module, #{}) -> true.
