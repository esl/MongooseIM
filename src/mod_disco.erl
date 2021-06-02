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
-xep([{xep, 30}, {version, "2.4"}]).
-xep([{xep, 157}, {version, "1.0"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2,
         stop/1,
         config_spec/0,
         process_server_info/1,
         process_local_iq_items/4,
         process_local_iq_info/4,
         get_local_identity/5,
         get_local_features/5,
         get_local_services/5,
         process_sm_iq_items/4,
         process_sm_iq_info/4,
         get_sm_identity/5,
         get_sm_items/5,
         get_info/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-type return_hidden() :: ejabberd_router:return_hidden().

-spec start(jid:server(), list()) -> 'ok'.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    [gen_iq_handler:add_iq_handler(Module, Host, NS, ?MODULE, Handler, IQDisc) ||
        {Module, NS, Handler} <- iq_handlers()],
    ejabberd_hooks:add(hooks(Host)).

-spec stop(jid:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    [gen_iq_handler:remove_iq_handler(Module, Host, NS) ||
        {Module, NS, _Handler} <- iq_handlers()],
    ok.

hooks(Host) ->
    [{disco_local_items, Host, ?MODULE, get_local_services, 100},
     {disco_local_features, Host, ?MODULE, get_local_features, 100},
     {disco_local_identity, Host, ?MODULE, get_local_identity, 100},
     {disco_sm_items, Host, ?MODULE, get_sm_items, 100},
     {disco_sm_identity, Host, ?MODULE, get_sm_identity, 100},
     {disco_info, Host, ?MODULE, get_info, 100}].

iq_handlers() ->
    [{ejabberd_local, ?NS_DISCO_ITEMS, process_local_iq_items},
     {ejabberd_local, ?NS_DISCO_INFO, process_local_iq_info},
     {ejabberd_sm, ?NS_DISCO_ITEMS, process_sm_iq_items},
     {ejabberd_sm, ?NS_DISCO_INFO, process_sm_iq_info}].

%% Configuration

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"extra_domains">> => #list{items = #option{type = binary,
                                                              validate = domain}},
                 <<"server_info">> => #list{items = server_info_spec()},
                 <<"users_can_see_hidden_services">> => #option{type = boolean},
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()
                }
      }.

server_info_spec() ->
    #section{
       items = #{<<"name">> => #option{type = string,
                                       validate = non_empty},
                 <<"urls">> => #list{items = #option{type = string,
                                                     validate = url}},
                 <<"modules">> => #list{items = #option{type = atom,
                                                        validate = module}}
                },
       required = [<<"name">>, <<"urls">>],
       process = fun ?MODULE:process_server_info/1
      }.

process_server_info(KVs) ->
    {[[{name, Name}], [{urls, URLs}]], _} = proplists:split(KVs, [name, urls]),
    Modules = proplists:get_value(modules, KVs, all),
    {Modules, Name, URLs}.

%% IQ handlers

-spec process_local_iq_items(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_items(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_items(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Host = To#jid.lserver,
    case mongoose_hooks:disco_local_items(Host, From, To, Node, Lang) of
        {result, Items} ->
            ANode = make_node_attr(Node),
            {Acc, IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                                   children = mongoose_disco:items_to_xml(Items)}]}};
        empty ->
            Error = mongoose_xmpp_errors:item_not_found(),
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
    end.

-spec process_local_iq_info(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_info(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_info(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Host = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Identity = mongoose_hooks:disco_local_identity(Host, From, To, Node, Lang),
    Info = mongoose_hooks:disco_info(Host, ?MODULE, Node, Lang),
    case mongoose_hooks:disco_local_features(Host, From, To, Node, Lang) of
        {result, Features} ->
            ANode = make_node_attr(Node),
            {Acc, IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                   children = mongoose_disco:identities_to_xml(Identity) ++
                                       Info ++
                                       mongoose_disco:features_to_xml(Features)}]}};
        empty ->
            Error = mongoose_xmpp_errors:item_not_found(),
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
    end.

-spec get_local_identity([mongoose_disco:identity()], jid:jid(), jid:jid(), binary(),
                         ejabberd:lang()) ->
          [mongoose_disco:identity()].
get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    [#{category => <<"server">>,
       type => <<"im">>,
       name => <<"MongooseIM">>}] ++ Acc;
get_local_identity(Acc, _From, _To, Node, _Lang) when is_binary(Node) ->
    Acc.

-spec get_local_features(mongoose_disco:feature_acc(), jid:jid(), jid:jid(), binary(),
                         ejabberd:lang()) ->
          mongoose_disco:feature_acc().
get_local_features(Acc, _From, _To, <<>>, _Lang) ->
    mongoose_disco:add_features([<<"iq">>, <<"presence">>, <<"presence-invisible">>], Acc);
get_local_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec get_local_services(mongoose_disco:item_acc(), jid:jid(), jid:jid(), binary(),
                         ejabberd:lang()) ->
          mongoose_disco:item_acc().
get_local_services(Acc, From, To, <<>>, _Lang) ->
    Host = To#jid.lserver,
    ReturnHidden = should_return_hidden(Host, From),
    Domains = get_vh_services(Host, ReturnHidden) ++ get_extra_domains(Host),
    mongoose_disco:add_items([#{jid => Domain} || Domain <- Domains], Acc);
get_local_services(Acc, _From, _To, _Node, _Lang) ->
    Acc.

get_extra_domains(Host) ->
    gen_mod:get_module_opt(Host, ?MODULE, extra_domains, []).

-spec should_return_hidden(Host :: jid:lserver(), From :: jid:jid()) -> return_hidden().
should_return_hidden(_Host, #jid{ luser = <<>> } = _From) ->
    %% We respect "is hidden" flag only when a client performs the query
    all;
should_return_hidden(Host, _From) ->
    case gen_mod:get_module_opt(Host, ?MODULE, users_can_see_hidden_services, true) of
        true -> all;
        false -> only_public
    end.

-type route() :: binary().
-spec get_vh_services(jid:server(), return_hidden()) -> [route()].
get_vh_services(Host, ReturnHidden) ->
    VHosts = lists:sort(fun(H1, H2) -> size(H1) >= size(H2) end, ?MYHOSTS),
    lists:filter(fun(Route) ->
                         check_if_host_is_the_shortest_suffix_for_route(Route, Host, VHosts)
                 end, ejabberd_router:dirty_get_all_routes(ReturnHidden)).

-spec check_if_host_is_the_shortest_suffix_for_route(
        Route :: route(), Host :: binary(), VHosts :: [binary()]) -> boolean().
check_if_host_is_the_shortest_suffix_for_route(Route, Host, VHosts) ->
    RouteS = binary_to_list(Route),
    case lists:dropwhile(
           fun(VH) ->
                   not lists:suffix("." ++ binary_to_list(VH), RouteS)
           end, VHosts) of
        [] ->
            false;
        [VH | _] ->
            VH == Host
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_sm_iq_items(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {string(), jlib:iq()}.
process_sm_iq_items(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_items(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    case is_presence_subscribed(From, To) of
        true ->
            Host = To#jid.lserver,
            Node = xml:get_tag_attr_s(<<"node">>, SubEl),
            case mongoose_hooks:disco_sm_items(Host, From, To, Node, Lang) of
                {result, Items} ->
                    ANode = make_node_attr(Node),
                    {Acc, IQ#iq{type = result,
                          sub_el = [#xmlel{name = <<"query">>,
                                           attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                                           children = mongoose_disco:items_to_xml(Items)}]}};
                empty ->
                    Error = sm_error(From, To),
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.

-spec get_sm_items(mongoose_disco:item_acc(), jid:jid(), jid:jid(), binary(), ejabberd:lang()) ->
          mongoose_disco:item_acc().
get_sm_items(Acc, _From, To, <<>>, _Lang) ->
    Items = get_user_resources(To),
    mongoose_disco:add_items(Items, Acc);
get_sm_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.

-spec is_presence_subscribed(jid:jid(), jid:jid()) -> boolean().
is_presence_subscribed(#jid{luser = LFromUser, lserver = LFromServer} = FromJID,
                       #jid{luser = LToUser, lserver = LToServer} = _To) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LFromServer),
    A = mongoose_acc:new(#{ location => ?LOCATION,
                            host_type => HostType,
                            lserver => LFromServer,
                            element => undefined }),
    A2 = mongoose_hooks:roster_get(A, FromJID),
    Roster = mongoose_acc:get(roster, items, [], A2),
    lists:any(fun({roster, _, _, JID, _, S, _, _, _, _}) ->
                      {TUser, TServer} = jid:to_lus(JID),
                      LToUser == TUser andalso LToServer == TServer andalso S /= none
              end,
              Roster)
    orelse LFromUser == LToUser andalso LFromServer == LToServer.


-spec process_sm_iq_info(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq_info(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_info(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    case is_presence_subscribed(From, To) of
        true ->
            Host = To#jid.lserver,
            Node = xml:get_tag_attr_s(<<"node">>, SubEl),
            Identity = mongoose_hooks:disco_sm_identity(Host, From, To, Node, Lang),
            case mongoose_hooks:disco_sm_features(Host, From, To, Node, Lang) of
                {result, Features} ->
                    ANode = make_node_attr(Node),
                    {Acc, IQ#iq{type = result,
                          sub_el = [#xmlel{name = <<"query">>,
                                           attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                           children = mongoose_disco:identities_to_xml(Identity) ++
                                           mongoose_disco:features_to_xml(Features)}]}};
                empty ->
                    Error = sm_error(From, To),
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.

sm_error(#jid{luser = LUser, lserver = LServer},
         #jid{luser = LUser, lserver = LServer}) ->
    mongoose_xmpp_errors:item_not_found();
sm_error(_From, _To) ->
    mongoose_xmpp_errors:not_allowed().

-spec get_sm_identity([mongoose_disco:identity()], jid:jid(), jid:jid(), binary(),
                         ejabberd:lang()) ->
          [mongoose_disco:identity()].
get_sm_identity(Acc, _From, JID = #jid{}, _Node, _Lang) ->
    case ejabberd_auth:does_user_exist(JID) of
        true -> [#{category => <<"account">>, type => <<"registered">>} | Acc];
        false -> Acc
    end.

-spec get_user_resources(jid:jid()) -> [mongoose_disco:item()].
get_user_resources(JID) ->
    #jid{user = User, server = Server} = JID,
    Rs = ejabberd_sm:get_user_resources(JID),
    lists:map(fun(R) ->
                      BJID = jid:to_binary({User, Server, R}),
                      #{jid => BJID, name => User}
              end, lists:sort(Rs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_node_attr(Node :: binary()) -> [{binary(), binary()}].
make_node_attr(<<>>) -> [];
make_node_attr(Node) -> [{<<"node">>, Node}].

%%% Support for: XEP-0157 Contact Addresses for XMPP Services

-spec get_info(Acc :: [exml:element()], jid:server(), module(), Node :: binary(),
        Lang :: ejabberd:lang()) -> [exml:element()].
get_info(Acc, Host, Mod, Node, _Lang) when Node == <<>> ->
    Module = case Mod of
                 undefined ->
                     ?MODULE;
                 _ ->
                     Mod
             end,
    ServerInfoFields = get_fields_xml(Host, Module),
    FormTypeField = #xmlel{name = <<"field">>,
                           attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
                           children = [#xmlel{name = <<"value">>,
                                              children = [#xmlcdata{content = ?NS_SERVERINFO}]}]},
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
            children = [FormTypeField | ServerInfoFields]} | Acc];
get_info(Acc, _, _, _Node, _) ->
    Acc.


-spec get_fields_xml(jid:server(), module()) -> [exml:element()].
get_fields_xml(Host, Module) ->
    Fields = gen_mod:get_module_opt(Host, ?MODULE, server_info, []),

    %% filter, and get only the ones allowed for this module
    FilteredFields = lists:filter(
                       fun({Modules, _, _}) ->
                               case Modules of
                                   all -> true;
                                   Modules -> lists:member(Module, Modules)
                               end
                       end,
                       Fields),

    fields_to_xml(FilteredFields).


-spec fields_to_xml([{Modules :: [module()], Var :: string(), Values :: [string()]}]) ->
    [exml:element()].
fields_to_xml(Fields) ->
    [ field_to_xml(Field) || Field <- Fields].


-spec field_to_xml({Modules :: [module()], Var :: string(), Values :: [string()]}) -> exml:element().
field_to_xml({_Module, Var, Values}) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, list_to_binary(Var)}],
           children = values_to_xml(Values)}.


-spec values_to_xml([binary()]) -> [exml:element()].
values_to_xml(Values) ->
    [ #xmlel{name = <<"value">>, children = [#xmlcdata{content = list_to_binary(Value)}]}
      || Value <- Values ].

